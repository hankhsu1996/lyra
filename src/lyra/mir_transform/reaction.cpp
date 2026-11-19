#include "lyra/mir_transform/reaction.hpp"

#include <algorithm>
#include <array>
#include <cstddef>
#include <cstdint>
#include <iterator>
#include <optional>
#include <span>
#include <unordered_map>
#include <utility>
#include <variant>
#include <vector>

#include "lyra/abi/records.hpp"
#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/base/registry.hpp"
#include "lyra/mir/block_id.hpp"
#include "lyra/mir/callable.hpp"
#include "lyra/mir/callable_code.hpp"
#include "lyra/mir/callable_id.hpp"
#include "lyra/mir/class_id.hpp"
#include "lyra/mir/code_source.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/expr_builders.hpp"
#include "lyra/mir/expr_id.hpp"
#include "lyra/mir/library_record_builder.hpp"
#include "lyra/mir/linkage.hpp"
#include "lyra/mir/local.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/mir/type.hpp"
#include "lyra/mir/type_builders.hpp"
#include "lyra/mir/type_id.hpp"
#include "lyra/mir_transform/block_rebuild.hpp"
#include "lyra/support/builtin_fn.hpp"
#include "lyra/support/linkage.hpp"
#include "lyra/support/start_kind.hpp"

namespace lyra::mir_transform {

namespace {

// The order the loop of a process that stops only at its head does its two
// things in: running its statements and then stopping, or stopping and then
// running them.
enum class LoopOrder : std::uint8_t { kRunThenStop, kStopThenRun };

// How the run calls the parts a loop of `order` is split into.
auto FormOf(LoopOrder order) -> support::ReactionForm {
  switch (order) {
    case LoopOrder::kRunThenStop:
      return support::ReactionForm::kRunThenStop;
    case LoopOrder::kStopThenRun:
      return support::ReactionForm::kStopThenRun;
  }
  throw InternalError("mir_transform: a loop of no order");
}

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

// Whether `decl`, a callable of a class, is a body entered on its object alone
// and completing with nothing, as a process's is.
auto IsEnteredOnItsObjectAlone(
    const mir::CompilationUnit& unit, const mir::CallableDecl& decl) -> bool {
  const mir::CallableCode& code = decl.code;
  return !mir::VirtualDispatchOf(decl).has_value() && code.body.has_value() &&
         code.result_type == unit.builtins.coroutine_void &&
         code.params.size() == 1 && code.TakesReceiver();
}

// What a process body that stops only at the head of its loop is made of, where
// `code` is one: entered on its object alone and completing with nothing;
// statements that never stop, then a loop that runs until left, then at most a
// bare return; the loop's body stopping at its last statement, or at its first
// where the statements before the loop never return, and nowhere else, and
// never leaving the loop; that stop parked on a wait the statements before the
// loop declared; and nothing else they declared named in the loop. `decl` is a
// body entered on its object alone.
auto MatchReaction(
    const mir::CompilationUnit& unit, const mir::CallableDecl& decl)
    -> std::optional<ReactionShape> {
  const mir::CallableCode& code = decl.code;
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

// Each rewritten process, by its callable, and the unit's record of how it was
// compiled.
using RewrittenProcesses = std::unordered_map<mir::CallableId, mir::ConstantId>;

// The machine function type both parts have: the object and the wait, each
// erased, in; whether the process goes on, out.
auto PartType(const mir::CompilationUnit& unit) -> mir::TypeId {
  const mir::TypeId erased = mir::ErasedPointer(unit.types);
  return unit.types.Intern(
      mir::Type{mir::MachineFunctionType{
          .params = {erased, erased}, .result = unit.builtins.machine_bool}});
}

// A part's signature and locals, without its body: the process's locals but
// for the wait where it has one, which the part reaches through a pointer, and
// the object and the wait it is handed erased.
struct PartCode {
  mir::CallableCode code;
  mir::LocalId object;
  mir::LocalId wait;
};

auto PartCodeOf(
    const mir::CompilationUnit& unit, const mir::CallableCode& process,
    std::optional<mir::LocalId> wait_local) -> PartCode {
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
  code.span = process.span;
  return PartCode{.code = std::move(code), .object = object, .wait = wait};
}

// `to = (T)from;`, a parameter handed erased restored to its type.
auto RestoreErased(
    const mir::CompilationUnit& unit, BlockRebuild& rebuild, mir::LocalId from,
    mir::LocalId to, mir::TypeId type) -> mir::StmtId {
  const mir::TypeId erased = mir::ErasedPointer(unit.types);
  const mir::ExprId read = rebuild.Add(
      mir::MakeCopyExpr(
          rebuild.Add(mir::MakeLocalRefExpr(from, erased)), erased));
  const mir::ExprId cast = rebuild.Add(
      mir::Expr{.data = mir::CastExpr{.operand = read}, .type = type});
  return rebuild.Add(mir::LocalDeclStmt{.target = to, .init = cast});
}

// `self = (Object*)object; wait_local = (Wait*)wait;`, the two statements
// opening each part.
auto RestoreErasedParameters(
    const mir::CompilationUnit& unit, const mir::CallableCode& process,
    mir::LocalId wait_local, const PartCode& part, BlockRebuild& rebuild)
    -> std::vector<mir::StmtId> {
  const mir::LocalId self = *process.receiver;
  return {
      RestoreErased(
          unit, rebuild, part.object, self, process.locals.Get(self).type),
      RestoreErased(
          unit, rebuild, part.wait, wait_local, unit.builtins.wait_ptr)};
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

// A bare return in a setup, where the body returns before its loop, answers
// that no pass follows.
auto AnswerAtBareReturn(
    const mir::CompilationUnit& unit, BlockRebuild& rebuild,
    const mir::Stmt& stmt) -> std::optional<mir::StmtData> {
  const auto* ret = std::get_if<mir::ReturnStmt>(&stmt.data);
  if (ret == nullptr || ret->value.has_value()) {
    return std::nullopt;
  }
  return mir::ReturnStmt{.value = rebuild.Add(MachineBool(unit, false))};
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
    const mir::CompilationUnit& unit, const mir::CallableCode& process,
    const mir::Block& loop, const ReactionShape& shape,
    std::span<const mir::StmtId> stmts) -> mir::Block {
  const RebuildRules rules{
      .expr = ReachWaitThroughPointer(unit, shape.wait_local)};
  BlockRebuild rebuild(loop, rules, process.span);
  std::vector<mir::StmtId> ran;
  ran.reserve(stmts.size());
  for (const mir::StmtId id : stmts) {
    ran.push_back(
        id == shape.stop
            ? rebuild.AddInPlaceOf(
                  id, mir::ReturnStmt{.value = rebuild.Expr(shape.park)})
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
        if (std::optional<mir::StmtData> answered =
                AnswerAtBareReturn(unit, rebuild, stmt)) {
          return answered;
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

  BlockRebuild rebuild(body, rules, process.span);
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
                  unit, process, LoopBody(body, shape), shape, stop))}));
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
  BlockRebuild rebuild(empty, rules, process.span);
  std::vector<mir::StmtId> stmts =
      RestoreErasedParameters(unit, process, shape.wait_local, part, rebuild);
  stmts.push_back(rebuild.Add(
      mir::BlockStmt{
          .scope = rebuild.Add(
              LoopBodyReturningAtStop(unit, process, loop, shape, once))}));
  part.code.body = std::move(rebuild).Finish(std::move(stmts));
  return std::move(part.code);
}

// A body that never stops as its reaction's setup: handed its object and a wait
// it never builds, both erased, it runs its statements once, and its answer
// is not read.
auto OnlySetupOf(
    const mir::CompilationUnit& unit, const mir::CallableCode& process)
    -> mir::CallableCode {
  PartCode part = PartCodeOf(unit, process, std::nullopt);
  const mir::Block& body = process.Body();
  const RebuildRules rules{
      .stmt = [&](BlockRebuild& rebuild, const mir::Stmt& stmt) {
        return AnswerAtBareReturn(unit, rebuild, stmt);
      }};
  BlockRebuild rebuild(body, rules, process.span);
  const mir::LocalId self = *process.receiver;
  std::vector<mir::StmtId> stmts{RestoreErased(
      unit, rebuild, part.object, self, process.locals.Get(self).type)};
  std::ranges::copy(rebuild.Stmts(body.root_stmts), std::back_inserter(stmts));
  stmts.push_back(rebuild.Add(
      mir::ReturnStmt{.value = rebuild.Add(MachineBool(unit, false))}));
  part.code.body = std::move(rebuild).Finish(std::move(stmts));
  return std::move(part.code);
}

// A registration of a process that was rewritten: the call, how its entry
// hands the run a body, the started call that body is, and what that call
// names.
struct RewrittenRegistration {
  const mir::CallExpr* registration;
  support::HandsTheRunABody hands;
  const mir::CallExpr* started;
  const mir::Direct* started_direct;
  mir::CallableId process;
};

// The registration `data` is, where it registers a process that was rewritten.
auto RewrittenRegistrationOf(
    const mir::Block& block, const mir::ExprData& data,
    const RewrittenProcesses& rewritten)
    -> std::optional<RewrittenRegistration> {
  const auto* call = std::get_if<mir::CallExpr>(&data);
  const auto* direct =
      call != nullptr ? std::get_if<mir::Direct>(&call->callee) : nullptr;
  const auto* entry = direct != nullptr
                          ? std::get_if<support::BuiltinFn>(&direct->target)
                          : nullptr;
  const std::optional<support::HandsTheRunABody> hands =
      entry != nullptr ? support::RuntimeEntryOf(*entry).hands_the_run_a_body
                       : std::nullopt;
  if (!hands.has_value()) {
    return std::nullopt;
  }
  if (hands->body_at + 1 != call->arguments.size()) {
    throw InternalError(
        "mir_transform: a registration's body is not its last operand -- "
        "please report this as a bug");
  }
  const auto* started = std::get_if<mir::CallExpr>(
      &block.exprs.Get(call->arguments.at(hands->body_at)).data);
  const auto* started_direct =
      started != nullptr ? std::get_if<mir::Direct>(&started->callee) : nullptr;
  const auto* target =
      started_direct != nullptr
          ? std::get_if<mir::CallableTarget>(&started_direct->target)
          : nullptr;
  if (target == nullptr || !rewritten.contains(target->callable)) {
    return std::nullopt;
  }
  if (!started_direct->receiver.has_value() || !started->arguments.empty()) {
    throw InternalError(
        "mir_transform: a process body is started other than on its object "
        "alone -- please report this as a bug");
  }
  return RewrittenRegistration{
      .registration = call,
      .hands = *hands,
      .started = started,
      .started_direct = started_direct,
      .process = target->callable};
}

// Whether `block`, at any depth, registers a process that was rewritten.
auto RegistersRewrittenProcess(
    const mir::Block& block, const RewrittenProcesses& rewritten) -> bool {
  return std::ranges::any_of(
             block.exprs,
             [&](const mir::Expr& expr) {
               return RewrittenRegistrationOf(block, expr.data, rewritten)
                   .has_value();
             }) ||
         std::ranges::any_of(block.child_scopes, [&](const mir::Block& child) {
           return RegistersRewrittenProcess(child, rewritten);
         });
}

// The unit's record of how a process was compiled as a reaction: its setup,
// its pass where it has one, and its form.
auto InternReactionDefinition(
    const mir::CompilationUnit& unit, mir::CallableId setup,
    std::optional<mir::CallableId> pass, support::ReactionForm form)
    -> mir::ConstantId {
  using Record = abi::ReactionDefinition;
  const auto code = [&](mir::CallableId callable) {
    return mir::ConstantValue{mir::PointerValue{
        .to = mir::FunctionRef{.callable = callable}, .type = PartType(unit)}};
  };
  mir::LibraryRecordBuilder<Record> record(unit);
  record.Set(&Record::setup, code(setup));
  record.Set(
      &Record::pass,
      pass.has_value() ? code(*pass) : mir::ConstantValue{mir::NullValue{}});
  record.Set(
      &Record::form, mir::ConstantValue{mir::MachineIntValue{
                         .value = static_cast<std::int64_t>(form)}});
  return std::move(record).Intern();
}

// `register_coroutine(scope, instance, runs_as, &definition, body(*object))`
// becomes `register_reaction(scope, instance, runs_as, &definition, &reaction,
// (void*)&*object)`: the same process, handed to the run as the parts its body
// was split into.
auto RewriteRegistrations(
    const mir::CompilationUnit& unit, const mir::CallableCode& code,
    const RewrittenProcesses& rewritten) -> mir::CallableCode {
  const RebuildRules rules{
      .expr = [&](BlockRebuild& rebuild,
                  mir::ExprId id) -> std::optional<mir::Expr> {
        const mir::Block& from = rebuild.From();
        const mir::Expr& expr = from.exprs.Get(id);
        const std::optional<RewrittenRegistration> registration =
            RewrittenRegistrationOf(from, expr.data, rewritten);
        if (!registration.has_value()) {
          return std::nullopt;
        }
        const mir::ExprId receiver = *registration->started_direct->receiver;
        const mir::TypeId object_type = from.exprs.Get(receiver).type;
        const mir::ExprId object = rebuild.Add(
            mir::Expr{
                .data =
                    mir::CastExpr{
                        .operand = rebuild.Add(
                            mir::MakeAddressOfExpr(
                                rebuild.Expr(receiver),
                                unit.types.Intern(
                                    mir::Type{mir::PointerType{
                                        .pointee = object_type,
                                        .ownership = mir::PointerOwnership::
                                            kBorrowed}})))},
                .type = mir::ErasedPointer(unit.types)});
        const mir::ExprId reaction_address = mir::BuildConstantAddress(
            unit, rewritten.at(registration->process), rebuild);
        std::vector<mir::ExprId> operands;
        for (std::size_t i = 0; i < registration->hands.body_at; ++i) {
          operands.push_back(
              rebuild.Expr(registration->registration->arguments.at(i)));
        }
        operands.push_back(reaction_address);
        operands.push_back(object);
        return mir::MakeCallExpr(
            mir::Direct{.target = registration->hands.as_a_reaction},
            std::move(operands), expr.type);
      }};
  return RebuildCode(code, rules);
}

using ReactionShapes = std::unordered_map<mir::CallableId, ReactionShape>;

}  // namespace

auto SplitReactions(mir::CompilationUnit unit) -> mir::CompilationUnit {
  ReactionShapes shapes;
  std::unordered_map<mir::CallableId, mir::CallableCode> setups;
  RewrittenProcesses rewritten;
  for (const mir::ClassId owner : unit.classes.Ids()) {
    // A body changes its signature here, which only every caller's unit can
    // keep up with, so a class another unit can name keeps its bodies as they
    // are, as LLVM rewrites only a function of local linkage.
    switch (mir::LinkageOf(unit, owner)) {
      case support::Linkage::kInternal:
        break;
      case support::Linkage::kExternal:
      case support::Linkage::kShared:
        continue;
    }
    for (const mir::CallableId callable : unit.CallablesOf(owner).callables) {
      const mir::CallableDecl& process = unit.callables.Get(callable);
      if (!IsEnteredOnItsObjectAlone(unit, process)) {
        continue;
      }
      if (std::optional<ReactionShape> shape = MatchReaction(unit, process)) {
        setups.emplace(callable, SetupOf(unit, process.code, *shape));
        shapes.emplace(callable, *std::move(shape));
      } else if (!StopsWithin(*process.code.body, std::nullopt)) {
        rewritten.emplace(
            callable, InternReactionDefinition(
                          unit, callable, std::nullopt,
                          support::ReactionForm::kRunsOnce));
      }
    }
  }
  if (rewritten.empty() && shapes.empty()) {
    return unit;
  }

  // Each setup is added to its class before any code naming one is rebuilt,
  // so every code the rewrite below reads is defined.
  for (const mir::ClassId owner : unit.classes.Ids()) {
    const std::vector<mir::CallableId> declared =
        unit.CallablesOf(owner).callables;
    for (const mir::CallableId callable : declared) {
      if (const auto shape = shapes.find(callable); shape != shapes.end()) {
        const mir::CallableId setup = unit.AddClassCallable(
            owner, std::nullopt, std::move(setups.at(callable)));
        rewritten.emplace(
            callable, InternReactionDefinition(
                          unit, setup, callable, FormOf(shape->second.order)));
      }
    }
  }

  // Each process split around its loop becomes its pass, one that never stops
  // its own setup, and every code registering either hands it to the run as a
  // reaction.
  return mir::RewriteEachCode(
      std::move(unit),
      [&](const mir::CompilationUnit& unit, const mir::CodeSource& at,
          const mir::CallableCode& code) -> std::optional<mir::CallableCode> {
        if (const auto* method = std::get_if<mir::CallableTarget>(&at)) {
          if (const auto shape = shapes.find(method->callable);
              shape != shapes.end()) {
            return PassOf(unit, code, shape->second);
          }
          if (rewritten.contains(method->callable)) {
            return OnlySetupOf(unit, code);
          }
        }
        if (!code.body.has_value() ||
            !RegistersRewrittenProcess(*code.body, rewritten)) {
          return std::nullopt;
        }
        return RewriteRegistrations(unit, code, rewritten);
      });
}

}  // namespace lyra::mir_transform
