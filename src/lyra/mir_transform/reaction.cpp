#include "lyra/mir_transform/reaction.hpp"

#include <algorithm>
#include <array>
#include <cstdint>
#include <iterator>
#include <optional>
#include <span>
#include <unordered_map>
#include <utility>
#include <variant>
#include <vector>

#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/base/registry.hpp"
#include "lyra/mir/block_id.hpp"
#include "lyra/mir/callable.hpp"
#include "lyra/mir/callable_code.hpp"
#include "lyra/mir/callable_id.hpp"
#include "lyra/mir/class.hpp"
#include "lyra/mir/class_id.hpp"
#include "lyra/mir/code_source.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/expr_id.hpp"
#include "lyra/mir/local.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/mir/type.hpp"
#include "lyra/mir/type_builders.hpp"
#include "lyra/mir/type_id.hpp"
#include "lyra/mir_transform/block_rebuild.hpp"
#include "lyra/support/builtin_fn.hpp"

namespace lyra::mir_transform {

namespace {

// The order the loop's body does its two things in. Running its statements
// and then stopping, the setup is followed by a pass at once (a continuous
// assignment, LRM 10.3); stopping and then running them, the setup goes on to
// the first stop (an `always` whose event control opens its statement, LRM
// 9.4.2).
enum class LoopOrder : std::uint8_t { kRunThenStop, kStopThenRun };

// Where a body that fits the rule keeps what the split takes apart: the
// statements before its loop; the loop's block, and the path from it through
// blocks holding nothing but one block each to the block whose statements the
// loop runs; the order that block runs in, the statement stopping at its head,
// and the park it stops on; and the local holding the wait.
struct ReactionShape {
  std::vector<mir::StmtId> before_loop;
  mir::BlockId loop;
  std::vector<mir::BlockId> inner_blocks;
  mir::StmtId stop;
  LoopOrder order;
  mir::ExprId park;
  mir::LocalId wait_local;
};

auto Stops(const mir::ExprData& data) -> bool {
  return std::holds_alternative<mir::WaitExpr>(data) ||
         std::holds_alternative<mir::AwaitExpr>(data);
}

// Whether anything in `block`, at any depth, stops the execution running it,
// other than `allowed`, one of `block`'s own expressions.
auto StopsWithin(const mir::Block& block, std::optional<mir::ExprId> allowed)
    -> bool {
  for (const mir::ExprId id : block.exprs.Ids()) {
    if (allowed != id && Stops(block.exprs.Get(id).data)) {
      return true;
    }
  }
  return std::ranges::any_of(block.child_scopes, [](const mir::Block& child) {
    return StopsWithin(child, std::nullopt);
  });
}

// Whether one of `stmts`, statements of `block`, leaves the stretch they make
// up other than by reaching its end: a return, a break naming a statement the
// stretch does not hold, or a continue meant for a loop the stretch is the body
// of rather than for one nested inside it. `held` are the labels of the
// statements around `block` that the stretch holds.
auto LeavesEarly(
    const mir::Block& block, std::span<const mir::StmtId> stmts,
    const std::vector<mir::LabelId>& held, bool in_inner_loop) -> bool {
  const auto child = [&](mir::BlockId id, bool inner,
                         std::optional<mir::LabelId> label) {
    std::vector<mir::LabelId> within = held;
    if (label.has_value()) {
      within.push_back(*label);
    }
    const mir::Block& scope = block.child_scopes.Get(id);
    return LeavesEarly(scope, scope.root_stmts, within, inner);
  };
  return std::ranges::any_of(stmts, [&](mir::StmtId id) {
    return std::visit(
        Overloaded{
            [](const mir::EmptyStmt&) { return false; },
            [](const mir::LocalDeclStmt&) { return false; },
            [](const mir::ExprStmt&) { return false; },
            [](const mir::RaiseStmt&) { return false; },
            [&](const mir::BlockStmt& s) {
              return child(s.scope, in_inner_loop, s.break_label);
            },
            [&](const mir::TryStmt& s) {
              return child(s.body, in_inner_loop, std::nullopt) ||
                     child(s.handler, in_inner_loop, std::nullopt);
            },
            [&](const mir::FinallyStmt& s) {
              return child(s.body, in_inner_loop, std::nullopt) ||
                     child(s.cleanup, in_inner_loop, std::nullopt);
            },
            [&](const mir::IfStmt& s) {
              return child(s.then_scope, in_inner_loop, std::nullopt) ||
                     (s.else_scope.has_value() &&
                      child(*s.else_scope, in_inner_loop, std::nullopt));
            },
            [&](const mir::SwitchStmt& s) {
              return std::ranges::any_of(
                         s.cases,
                         [&](const mir::SwitchCase& selected) {
                           return child(
                               selected.body, in_inner_loop, std::nullopt);
                         }) ||
                     (s.default_body.has_value() &&
                      child(*s.default_body, in_inner_loop, std::nullopt));
            },
            [&](const mir::ForStmt& s) {
              return child(s.scope, true, s.break_label);
            },
            [&](const mir::WhileStmt& s) {
              return child(s.scope, true, s.break_label);
            },
            [&](const mir::DoWhileStmt& s) {
              return child(s.scope, true, s.break_label);
            },
            [&](const mir::BreakStmt& s) {
              return !std::ranges::contains(held, s.target);
            },
            [&](const mir::ContinueStmt&) { return !in_inner_loop; },
            [](const mir::ReturnStmt&) { return true; }},
        block.stmts.Get(id).data);
  });
}

void CollectLocals(
    const mir::Block& block, std::vector<mir::LocalId>& named,
    std::vector<mir::LocalId>& declared) {
  for (const mir::Expr& expr : block.exprs) {
    if (const std::optional<mir::LocalId> local =
            mir::ReferencedLocal(expr.data)) {
      named.push_back(*local);
    }
  }
  for (const mir::Stmt& stmt : block.stmts) {
    if (const auto* decl = std::get_if<mir::LocalDeclStmt>(&stmt.data)) {
      declared.push_back(decl->target);
    } else if (const auto* attempt = std::get_if<mir::TryStmt>(&stmt.data)) {
      declared.push_back(attempt->caught);
    } else if (const auto* loop = std::get_if<mir::ForStmt>(&stmt.data)) {
      for (const mir::ForInit& init : loop->init) {
        if (const auto* induction = std::get_if<mir::ForInitDecl>(&init)) {
          declared.push_back(induction->induction_var);
        }
      }
    }
  }
  for (const mir::Block& child : block.child_scopes) {
    CollectLocals(child, named, declared);
  }
}

// Whether `block`, at any depth, names a local the body declared outside it
// other than those `allowed`. A local declared inside it begins again on each
// run of it, so only one from outside can carry a value from one run to the
// next.
auto NamesOutsideLocals(
    const mir::Block& block, std::span<const mir::LocalId> allowed) -> bool {
  std::vector<mir::LocalId> named;
  std::vector<mir::LocalId> declared;
  CollectLocals(block, named, declared);
  const auto among = [](std::span<const mir::LocalId> locals,
                        mir::LocalId local) {
    return std::ranges::find(locals, local) != locals.end();
  };
  return std::ranges::any_of(named, [&](mir::LocalId local) {
    return !among(declared, local) && !among(allowed, local);
  });
}

// The block `block` holds and nothing else, where that is all it holds: a
// scope that adds nothing to the one it opens.
auto SoleNestedBlock(const mir::Block& block) -> std::optional<mir::BlockId> {
  if (!block.exprs.empty() || block.root_stmts.size() != 1) {
    return std::nullopt;
  }
  const auto* only = std::get_if<mir::BlockStmt>(
      &block.stmts.Get(block.root_stmts.front()).data);
  return only != nullptr ? std::optional{only->scope} : std::nullopt;
}

// What a process body that stops only at the head of its loop is made of, where
// `code` is one: entered on its object alone and completing with nothing;
// statements that never stop, then a loop that runs until left, then at most a
// bare return; the loop's body stopping at its last statement, or at its first
// where the statements before the loop never return, and nowhere else, and
// never leaving the loop; that stop parked on a wait the statements before the
// loop declared; and nothing else they declared named in the loop.
auto MatchReaction(
    const mir::CompilationUnit& unit, const mir::CallableDecl& decl)
    -> std::optional<ReactionShape> {
  const mir::CallableCode& code = decl.code;
  if (decl.foreign.has_value() || decl.virtual_dispatch.has_value() ||
      !code.body.has_value() ||
      code.result_type != unit.builtins.coroutine_void ||
      code.params.size() != 1 || !code.TakesReceiver()) {
    return std::nullopt;
  }
  const mir::Block& body = *code.body;

  std::span<const mir::StmtId> stmts = body.root_stmts;
  if (!stmts.empty()) {
    const auto* ret =
        std::get_if<mir::ReturnStmt>(&body.stmts.Get(stmts.back()).data);
    if (ret != nullptr && !ret->value.has_value()) {
      stmts = stmts.first(stmts.size() - 1);
    }
  }
  if (stmts.empty()) {
    return std::nullopt;
  }
  const auto* loop =
      std::get_if<mir::ForStmt>(&body.stmts.Get(stmts.back()).data);
  if (loop == nullptr || !loop->init.empty() || loop->condition.has_value() ||
      !loop->step.empty()) {
    return std::nullopt;
  }
  const std::span<const mir::StmtId> before_loop =
      stmts.first(stmts.size() - 1);

  std::vector<mir::BlockId> inner_blocks;
  const mir::Block* runs = &body.child_scopes.Get(loop->scope);
  while (const std::optional<mir::BlockId> inner = SoleNestedBlock(*runs)) {
    inner_blocks.push_back(*inner);
    runs = &runs->child_scopes.Get(*inner);
  }
  const mir::Block& pass = *runs;
  if (pass.root_stmts.empty()) {
    return std::nullopt;
  }
  // The wait `id` stops at, where it is a statement stopping and nothing else.
  const auto stopping = [&](mir::StmtId id) -> std::optional<mir::ExprId> {
    const auto* stmt = std::get_if<mir::ExprStmt>(&pass.stmts.Get(id).data);
    if (stmt == nullptr || !std::holds_alternative<mir::WaitExpr>(
                               pass.exprs.Get(stmt->expr).data)) {
      return std::nullopt;
    }
    return stmt->expr;
  };
  LoopOrder order = LoopOrder::kRunThenStop;
  mir::StmtId stop = pass.root_stmts.back();
  std::optional<mir::ExprId> wait = stopping(stop);
  if (!wait.has_value()) {
    order = LoopOrder::kStopThenRun;
    stop = pass.root_stmts.front();
    wait = stopping(stop);
  }
  if (!wait.has_value() || StopsWithin(pass, wait) ||
      LeavesEarly(pass, pass.root_stmts, {}, false)) {
    return std::nullopt;
  }
  // A setup going on to the first stop answers whether to stop there, which
  // leaves it no answer for the process having ended, so the statements before
  // the loop must not return.
  switch (order) {
    case LoopOrder::kRunThenStop:
      break;
    case LoopOrder::kStopThenRun:
      if (LeavesEarly(body, before_loop, {}, false)) {
        return std::nullopt;
      }
      break;
  }

  // Every expression of the body's own block belongs to a statement before the
  // loop, which has none, or to the bare return.
  if (std::ranges::any_of(
          body.exprs, [](const mir::Expr& expr) { return Stops(expr.data); })) {
    return std::nullopt;
  }
  for (const mir::BlockId child : body.child_scopes.Ids()) {
    if (child != loop->scope &&
        StopsWithin(body.child_scopes.Get(child), std::nullopt)) {
      return std::nullopt;
    }
  }

  std::optional<mir::LocalId> wait_local;
  for (const mir::StmtId id : before_loop) {
    const auto* decl =
        std::get_if<mir::LocalDeclStmt>(&body.stmts.Get(id).data);
    if (decl == nullptr ||
        code.locals.Get(decl->target).type != unit.builtins.wait) {
      continue;
    }
    if (wait_local.has_value()) {
      return std::nullopt;
    }
    wait_local = decl->target;
  }
  if (!wait_local.has_value() ||
      NamesOutsideLocals(pass, std::array{*code.receiver, *wait_local})) {
    return std::nullopt;
  }

  return ReactionShape{
      .before_loop = {before_loop.begin(), before_loop.end()},
      .loop = loop->scope,
      .inner_blocks = std::move(inner_blocks),
      .stop = stop,
      .order = order,
      .park = std::get<mir::WaitExpr>(pass.exprs.Get(*wait).data).park,
      .wait_local = *wait_local};
}

// What a split process adds beside its own slot, which its pass keeps: the
// slot of its setup, and the runtime's entry that runs the two, which says what
// the setup answers.
struct SplitParts {
  mir::CallableId setup;
  support::BuiltinFn entry;
};

using SplitProcesses = std::unordered_map<mir::CallableTarget, SplitParts>;

auto EntryFor(LoopOrder order) -> support::BuiltinFn {
  switch (order) {
    case LoopOrder::kRunThenStop:
      return support::BuiltinFn::kEnterReactionRunningFirst;
    case LoopOrder::kStopThenRun:
      return support::BuiltinFn::kEnterReactionStoppingFirst;
  }
  throw InternalError("mir_transform: unknown loop order");
}

// The machine function type both parts have: the object and the wait, each
// erased, in; whether the process goes on, out.
auto PartType(const mir::CompilationUnit& unit) -> mir::TypeId {
  const mir::TypeId erased = mir::ErasedPointer(unit.types);
  return unit.types.Intern(
      mir::Type{mir::MachineFunctionType{
          .params = {erased, erased}, .result = unit.builtins.machine_bool}});
}

// A part's signature and locals, without its body: the process's locals but
// for the wait, which the part reaches through a pointer, and the object and
// the wait it is handed erased.
struct PartCode {
  mir::CallableCode code;
  mir::LocalId object;
  mir::LocalId wait;
};

auto PartCodeOf(
    const mir::CompilationUnit& unit, const mir::CallableCode& process,
    mir::LocalId wait_local) -> PartCode {
  mir::CallableCode code;
  for (const mir::LocalId id : process.locals.Ids()) {
    code.locals.Add(
        id == wait_local ? mir::LocalDecl{.type = unit.builtins.wait_ptr}
                         : process.locals.Get(id));
  }
  const mir::LocalId object = code.AddLocal(mir::ErasedPointer(unit.types));
  const mir::LocalId wait = code.AddLocal(mir::ErasedPointer(unit.types));
  code.params = {object, wait};
  code.result_type = unit.builtins.machine_bool;
  code.named_locals = process.named_locals;
  return PartCode{.code = std::move(code), .object = object, .wait = wait};
}

// A part is reached by its position alone: no foreign code calls it and no
// class overrides it.
auto PartDecl(mir::CallableCode code) -> mir::CallableDecl {
  return mir::CallableDecl{
      .code = std::move(code),
      .foreign = std::nullopt,
      .virtual_dispatch = std::nullopt};
}

// `self = (Object*)object; wait_local = (Wait*)wait;`, the two statements
// opening each part.
auto RestoreErasedParameters(
    const mir::CompilationUnit& unit, const mir::CallableCode& process,
    mir::LocalId wait_local, const PartCode& part, BlockRebuild& rebuild)
    -> std::vector<mir::StmtId> {
  const mir::TypeId erased = mir::ErasedPointer(unit.types);
  const auto restored = [&](mir::LocalId from, mir::LocalId to,
                            mir::TypeId type) {
    const mir::ExprId read = rebuild.Add(
        mir::MakeCopyExpr(
            rebuild.Add(mir::MakeLocalRefExpr(from, erased)), erased));
    const mir::ExprId cast = rebuild.Add(
        mir::Expr{.data = mir::CastExpr{.operand = read}, .type = type});
    return rebuild.Add(mir::LocalDeclStmt{.target = to, .init = cast});
  };
  const mir::LocalId self = *process.receiver;
  return {
      restored(part.object, self, process.locals.Get(self).type),
      restored(part.wait, wait_local, unit.builtins.wait_ptr)};
}

// `*wait_local`: the wait, reached through the pointer the part was handed.
auto WaitThroughPointer(
    const mir::CompilationUnit& unit, BlockRebuild& rebuild,
    mir::LocalId wait_local) -> mir::Expr {
  const mir::ExprId pointer = rebuild.Add(
      mir::MakeCopyExpr(
          rebuild.Add(
              mir::MakeLocalRefExpr(wait_local, unit.builtins.wait_ptr)),
          unit.builtins.wait_ptr));
  return mir::MakeDerefExpr(pointer, unit.builtins.wait);
}

// Every name of `wait_local` becomes `*wait_local`.
auto ReachWaitThroughPointer(
    const mir::CompilationUnit& unit, mir::LocalId wait_local) {
  return [&unit, wait_local](
             BlockRebuild& rebuild,
             mir::ExprId id) -> std::optional<mir::Expr> {
    if (mir::ReferencedLocal(rebuild.From().exprs.Get(id).data) != wait_local) {
      return std::nullopt;
    }
    return WaitThroughPointer(unit, rebuild, wait_local);
  };
}

auto MachineBool(const mir::CompilationUnit& unit, bool value) -> mir::Expr {
  return mir::Expr{
      .data = mir::MachineBoolLiteral{.value = value},
      .type = unit.builtins.machine_bool};
}

// The block whose statements the loop of `body` runs.
auto LoopBody(const mir::Block& body, const ReactionShape& shape)
    -> const mir::Block& {
  const mir::Block* runs = &body.child_scopes.Get(shape.loop);
  for (const mir::BlockId inner : shape.inner_blocks) {
    runs = &runs->child_scopes.Get(inner);
  }
  return *runs;
}

// The block the loop runs, running `stmts` of it, the stop among them
// answering whether its wait has still to end instead of stopping there.
auto LoopBodyReturningAtStop(
    const mir::CompilationUnit& unit, const mir::Block& loop,
    const ReactionShape& shape, std::span<const mir::StmtId> stmts)
    -> mir::Block {
  const RebuildRules rules{
      .expr = ReachWaitThroughPointer(unit, shape.wait_local)};
  BlockRebuild rebuild(loop, rules);
  std::vector<mir::StmtId> ran;
  ran.reserve(stmts.size());
  for (const mir::StmtId id : stmts) {
    ran.push_back(
        id == shape.stop
            ? rebuild.Add(mir::ReturnStmt{.value = rebuild.Expr(shape.park)})
            : rebuild.Stmt(id));
  }
  return std::move(rebuild).Finish(std::move(ran));
}

// The statements before the loop: building the wait writes it where the part
// was handed. Where the loop runs and then stops, a return answers that no
// pass runs and reaching the loop that one does; where it stops and then runs,
// the setup goes on to that stop and answers as a pass does.
auto SetupOf(
    const mir::CompilationUnit& unit, const mir::CallableCode& process,
    const ReactionShape& shape) -> mir::CallableCode {
  PartCode part = PartCodeOf(unit, process, shape.wait_local);
  const mir::Block& body = process.Body();
  const RebuildRules rules{
      .expr = ReachWaitThroughPointer(unit, shape.wait_local),
      .stmt = [&](BlockRebuild& rebuild,
                  const mir::Stmt& stmt) -> std::optional<mir::StmtData> {
        if (const auto* ret = std::get_if<mir::ReturnStmt>(&stmt.data);
            ret != nullptr && !ret->value.has_value()) {
          return mir::ReturnStmt{
              .value = rebuild.Add(MachineBool(unit, false))};
        }
        const auto* decl = std::get_if<mir::LocalDeclStmt>(&stmt.data);
        if (decl == nullptr || decl->target != shape.wait_local) {
          return std::nullopt;
        }
        const mir::ExprId place =
            rebuild.Add(WaitThroughPointer(unit, rebuild, shape.wait_local));
        return mir::ExprStmt{
            .expr = rebuild.Add(
                mir::MakeAssignExpr(
                    unit.builtins, place, rebuild.Expr(decl->init)))};
      }};

  BlockRebuild rebuild(body, rules);
  std::vector<mir::StmtId> stmts =
      RestoreErasedParameters(unit, process, shape.wait_local, part, rebuild);
  std::ranges::copy(
      rebuild.Stmts(shape.before_loop), std::back_inserter(stmts));
  switch (shape.order) {
    case LoopOrder::kRunThenStop:
      stmts.push_back(rebuild.Add(
          mir::ReturnStmt{.value = rebuild.Add(MachineBool(unit, true))}));
      break;
    case LoopOrder::kStopThenRun: {
      const std::array stop{shape.stop};
      stmts.push_back(rebuild.Add(
          mir::BlockStmt{
              .scope = rebuild.Add(LoopBodyReturningAtStop(
                  unit, LoopBody(body, shape), shape, stop))}));
      break;
    }
  }
  part.code.body = std::move(rebuild).Finish(std::move(stmts));
  return std::move(part.code);
}

// The loop's statements run once and then its stop, answering whether the wait
// it parks on has still to end. Where the loop stops and then runs, the stop
// comes last here, since the setup made the first one.
auto PassOf(
    const mir::CompilationUnit& unit, const mir::CallableCode& process,
    const ReactionShape& shape) -> mir::CallableCode {
  PartCode part = PartCodeOf(unit, process, shape.wait_local);
  const mir::Block& loop = LoopBody(process.Body(), shape);
  std::vector<mir::StmtId> once = loop.root_stmts;
  switch (shape.order) {
    case LoopOrder::kRunThenStop:
      break;
    case LoopOrder::kStopThenRun:
      std::ranges::rotate(once, once.begin() + 1);
      break;
  }

  const mir::Block empty;
  const RebuildRules rules;
  BlockRebuild rebuild(empty, rules);
  std::vector<mir::StmtId> stmts =
      RestoreErasedParameters(unit, process, shape.wait_local, part, rebuild);
  stmts.push_back(rebuild.Add(
      mir::BlockStmt{
          .scope =
              rebuild.Add(LoopBodyReturningAtStop(unit, loop, shape, once))}));
  part.code.body = std::move(rebuild).Finish(std::move(stmts));
  return std::move(part.code);
}

// The split process `data` starts, where it is a call starting one.
auto SplitProcessStarted(const mir::ExprData& data, const SplitProcesses& split)
    -> const mir::CallableTarget* {
  const auto* call = std::get_if<mir::CallExpr>(&data);
  const auto* direct =
      call != nullptr ? std::get_if<mir::Direct>(&call->callee) : nullptr;
  const auto* target = direct != nullptr
                           ? std::get_if<mir::CallableTarget>(&direct->target)
                           : nullptr;
  return target != nullptr && split.contains(*target) ? target : nullptr;
}

// Whether `block`, at any depth, starts a process that was split.
auto StartsSplitProcess(const mir::Block& block, const SplitProcesses& split)
    -> bool {
  return std::ranges::any_of(
             block.exprs,
             [&](const mir::Expr& expr) {
               return SplitProcessStarted(expr.data, split) != nullptr;
             }) ||
         std::ranges::any_of(block.child_scopes, [&](const mir::Block& child) {
           return StartsSplitProcess(child, split);
         });
}

// `body(*object)` becomes `enter_reaction_running_first((void*)&*object, setup,
// pass)`: the same execution, run by the runtime's one coroutine for every
// split process.
auto RewriteSplitStarts(
    const mir::CompilationUnit& unit, const mir::CallableCode& code,
    std::span<const mir::ExprId> outside_roots, const SplitProcesses& split)
    -> mir::SourcedCode {
  const mir::TypeId part_type = PartType(unit);
  const RebuildRules rules{
      .expr = [&](BlockRebuild& rebuild,
                  mir::ExprId id) -> std::optional<mir::Expr> {
        const mir::Expr& expr = rebuild.From().exprs.Get(id);
        const mir::CallableTarget* target =
            SplitProcessStarted(expr.data, split);
        if (target == nullptr) {
          return std::nullopt;
        }
        const auto& call = std::get<mir::CallExpr>(expr.data);
        const auto& direct = std::get<mir::Direct>(call.callee);
        if (!direct.receiver.has_value() || !call.arguments.empty()) {
          throw InternalError(
              "mir_transform: a process body is started other than on its "
              "object alone -- please report this as a bug");
        }
        const mir::TypeId object_type =
            rebuild.From().exprs.Get(*direct.receiver).type;
        const mir::ExprId address = rebuild.Add(
            mir::MakeAddressOfExpr(
                rebuild.Expr(*direct.receiver),
                unit.types.Intern(
                    mir::Type{mir::PointerType{
                        .pointee = object_type,
                        .ownership = mir::PointerOwnership::kBorrowed}})));
        const mir::ExprId erased = rebuild.Add(
            mir::Expr{
                .data = mir::CastExpr{.operand = address},
                .type = mir::ErasedPointer(unit.types)});
        const auto code = [&](mir::CallableId slot) {
          return rebuild.Add(
              mir::Expr{
                  .data =
                      mir::ReferenceExpr{
                          .target =
                              mir::FunctionRef{
                                  .body =
                                      mir::CallableTarget{
                                          .owner = target->owner,
                                          .slot = slot}}},
                  .type = part_type});
        };
        const SplitParts& parts = split.at(*target);
        return mir::MakeCallExpr(
            mir::Direct{.target = parts.entry},
            {erased, code(parts.setup), code(target->slot)}, expr.type);
      }};
  return RebuildCode(code, outside_roots, rules);
}

using ReactionShapes = std::unordered_map<mir::CallableTarget, ReactionShape>;

}  // namespace

auto SplitReactions(mir::CompilationUnit unit) -> mir::CompilationUnit {
  ReactionShapes shapes;
  std::unordered_map<mir::CallableTarget, mir::CallableCode> setups;
  for (const mir::ClassId owner : unit.classes.Ids()) {
    const mir::Class& cls = unit.GetClass(owner);
    for (const mir::CallableId slot : cls.callables.Ids()) {
      const mir::CallableDecl& process = cls.callables.Get(slot);
      if (std::optional<ReactionShape> shape = MatchReaction(unit, process)) {
        const mir::CallableTarget target{.owner = owner, .slot = slot};
        setups.emplace(target, SetupOf(unit, process.code, *shape));
        shapes.emplace(target, *std::move(shape));
      }
    }
  }
  if (shapes.empty()) {
    return unit;
  }

  // Each setup is added to its class before any code naming one is rebuilt,
  // so every code the rewrite below reads is defined.
  SplitProcesses split;
  unit.classes =
      std::move(unit.classes)
          .TransformValues([&](mir::ClassId owner, mir::Class cls) {
            for (const mir::CallableId slot : cls.callables.Ids()) {
              const mir::CallableTarget target{.owner = owner, .slot = slot};
              if (const auto shape = shapes.find(target);
                  shape != shapes.end()) {
                split.emplace(
                    target, SplitParts{
                                .setup = cls.callables.Add(
                                    PartDecl(std::move(setups.at(target)))),
                                .entry = EntryFor(shape->second.order)});
              }
            }
            return cls;
          });

  // Each split process becomes its pass, and every other code starting a split
  // process starts it through the runtime.
  return mir::RewriteEachCode(
      std::move(unit),
      [&](const mir::CompilationUnit& unit, const mir::CodeSource& at,
          const mir::CallableCode& code,
          std::span<const mir::ExprId> outside_roots)
          -> std::optional<mir::SourcedCode> {
        if (const auto* method = std::get_if<mir::CallableTarget>(&at)) {
          if (const auto shape = shapes.find(*method); shape != shapes.end()) {
            return mir::SourcedCode{
                .code = PassOf(unit, code, shape->second), .outside_roots = {}};
          }
        }
        if (!code.body.has_value() || !StartsSplitProcess(*code.body, split)) {
          return std::nullopt;
        }
        return RewriteSplitStarts(unit, code, outside_roots, split);
      });
}

}  // namespace lyra::mir_transform
