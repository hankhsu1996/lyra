#include "lyra/mir_transform/hold_large_values_in_temporaries.hpp"

#include <algorithm>
#include <cstddef>
#include <cstdint>
#include <map>
#include <optional>
#include <span>
#include <utility>
#include <variant>
#include <vector>

#include "lyra/base/arena.hpp"
#include "lyra/base/internal_error.hpp"
#include "lyra/mir/callable_code.hpp"
#include "lyra/mir/code_source.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/frame_size.hpp"
#include "lyra/mir/local.hpp"
#include "lyra/mir/operand_use.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/mir/storage_part.hpp"
#include "lyra/mir/type.hpp"
#include "lyra/mir/verify.hpp"
#include "lyra/mir_transform/block_rebuild.hpp"

namespace lyra::mir_transform {

namespace {

// How the rewrite takes an operand: how its node takes it, and what the node
// then does with it.
enum class TakenAs : std::uint8_t {
  // Read as a value, which one made of a large type is made in a temporary
  // for.
  kValue,
  // Bound where it lies, which such a one is likewise made in a temporary
  // for.
  kInPlace,
  // Built into storage the node or its callee owns -- what a new-expression
  // allocates, a capture, an argument of a body this program compiles, which
  // takes a large one in a box built where it is handed -- so a large one is
  // made there unless a later operand's step has to follow it, and a small one
  // is read.
  kHandedOver,
  // Formed where its node is, with no temporary of its own: a place a read or
  // a move takes, or the pointer a dereference opens, which is that place's
  // own operand.
  kAsItIs,
  // The call a suspension waits on, which stands under the suspension as it
  // is, since a suspension stands at the top of its statement and none falls
  // among a block expression's steps, so each of its arguments is an
  // evaluation of its own, before the call.
  kSuspendedOn,
  // An arm of a choice, which begins an evaluation of its own.
  kApart,
};

// How the rewrite takes `operand` of `node`, which `node` takes as `use`.
auto Taking(
    const mir::CompilationUnit& unit, const mir::Expr& node,
    mir::ExprId operand, mir::OperandUse use) -> TakenAs {
  if (std::holds_alternative<mir::AwaitExpr>(node.data) ||
      std::holds_alternative<mir::WaitExpr>(node.data)) {
    return TakenAs::kSuspendedOn;
  }
  if (const auto* chosen = std::get_if<mir::ConditionalExpr>(&node.data)) {
    return operand == chosen->condition ? TakenAs::kValue : TakenAs::kApart;
  }
  if (std::holds_alternative<mir::NewExpr>(node.data) ||
      std::holds_alternative<mir::ClosureExpr>(node.data)) {
    return TakenAs::kHandedOver;
  }
  if (const auto* call = std::get_if<mir::CallExpr>(&node.data)) {
    if (mir::EntersACompiledBody(unit, *call, node.type) &&
        std::ranges::find(call->arguments, operand) != call->arguments.end()) {
      return TakenAs::kHandedOver;
    }
  }
  switch (use) {
    case mir::OperandUse::kValue:
      return TakenAs::kValue;
    case mir::OperandUse::kStorage:
    case mir::OperandUse::kObject:
      return TakenAs::kInPlace;
    case mir::OperandUse::kPointer:
    case mir::OperandUse::kRead:
      return TakenAs::kAsItIs;
  }
  throw InternalError("mir_transform: an operand is taken in no known way");
}

// Each operand of `node`, in the order `node` evaluates them, with how the
// rewrite takes it.
auto OperandsOf(const mir::CompilationUnit& unit, const mir::Expr& node)
    -> std::vector<std::pair<mir::ExprId, TakenAs>> {
  std::vector<std::pair<mir::ExprId, TakenAs>> operands;
  mir::ForEachOperandAsTaken(
      unit.types, node, [&](mir::ExprId operand, mir::OperandUse use) {
        operands.emplace_back(operand, Taking(unit, node, operand, use));
      });
  return operands;
}

// Whether any node of `block`, or of a block nested in it, is of a type too
// large for a frame.
auto HoldsALargeValue(const mir::CompilationUnit& unit, const mir::Block& block)
    -> bool {
  return std::ranges::any_of(
             block.exprs,
             [&](const mir::Expr& expr) {
               return mir::IsTooLargeForAFrame(unit, expr.type);
             }) ||
         std::ranges::any_of(block.child_scopes, [&](const mir::Block& child) {
           return HoldsALargeValue(unit, child);
         });
}

// The rewrite of one body. It holds what it learns of the body once, and has a
// step for each kind of operand and statement, each handed the block it
// builds.
class BodyTemporaries {
 public:
  BodyTemporaries(
      const mir::CompilationUnit& unit, const mir::CallableCode& code)
      : unit_(&unit),
        code_(&code),
        completes_(unit.types.Get(code.result_type).Is<mir::CoroutineType>()),
        locals_(code.locals),
        rules_{
            .expr =
                [this](BlockRebuild& rebuild, mir::ExprId id) {
                  return Node(rebuild, rebuild.From().exprs.Get(id));
                },
            .stmt =
                [this](BlockRebuild& rebuild, const mir::Stmt& stmt) {
                  return Statement(rebuild, stmt);
                },
            .evaluation =
                [this](BlockRebuild& rebuild, mir::ExprId old) {
                  return Evaluated(
                      rebuild, old, TakenAs::kAsItIs,
                      [old](BlockRebuild& in) { return in.Expr(old); });
                }} {
  }

  BodyTemporaries(const BodyTemporaries&) = delete;
  BodyTemporaries(BodyTemporaries&&) = delete;
  auto operator=(const BodyTemporaries&) -> BodyTemporaries& = delete;
  auto operator=(BodyTemporaries&&) -> BodyTemporaries& = delete;
  ~BodyTemporaries() = default;

  // The code rewritten, each outside root -- what a constructor enters its
  // base's constructor with -- an evaluation of its own, and the temporaries
  // its evaluations declared among its locals.
  auto Rewritten(std::span<const mir::ExprId> outside_roots)
      -> mir::SourcedCode {
    mir::SourcedCode rewritten = RebuildCode(*code_, outside_roots, rules_);
    rewritten.code.locals = std::move(locals_);
    return rewritten;
  }

 private:
  // An evaluation being built: the block its steps and what they serve are
  // built in, and the steps taken so far, in the order they run.
  struct Evaluation {
    const BlockRebuild* in;
    std::vector<mir::StmtId> steps;
  };

  // Whether `operand`, a node of `block`, makes a value of a large type rather
  // than naming one that lies in storage.
  [[nodiscard]] auto MakesALargeValue(
      const mir::Block& block, mir::ExprId operand) const -> bool {
    return mir::IsTooLargeForAFrame(*unit_, block.exprs.Get(operand).type) &&
           !mir::ValueLiesInStorage(*unit_, block, operand);
  }

  // Whether taking `operand`, a node of `block`, as `taken` takes a step of
  // the evaluation it belongs to: it is made in a temporary, or one of its own
  // operands is.
  [[nodiscard]] auto TakesAStep(
      const mir::Block& block, TakenAs taken, mir::ExprId operand) -> bool {
    switch (taken) {
      case TakenAs::kValue:
      case TakenAs::kInPlace:
        if (MakesALargeValue(block, operand)) {
          return true;
        }
        break;
      case TakenAs::kHandedOver:
      case TakenAs::kAsItIs:
        break;
      case TakenAs::kSuspendedOn:
      case TakenAs::kApart:
        return false;
    }
    return OperandsTakeAStep(block, operand);
  }

  // Whether evaluating the node `id` of `block` takes a step through any of
  // its operands.
  [[nodiscard]] auto OperandsTakeAStep(const mir::Block& block, mir::ExprId id)
      -> bool {
    const std::pair<const mir::Block*, mir::ExprId> key{&block, id};
    if (const auto known = operands_take_a_step_.find(key);
        known != operands_take_a_step_.end()) {
      return known->second;
    }
    bool takes = false;
    for (const auto& [operand, taken] :
         OperandsOf(*unit_, block.exprs.Get(id))) {
      takes = takes || TakesAStep(block, taken, operand);
    }
    operands_take_a_step_.emplace(key, takes);
    return takes;
  }

  // A block of its own over the block `rebuild` builds from, in which `build`
  // makes what an evaluation serves: its steps run first, in the order they
  // were taken, then whatever `build` answers it needs run after them. Its
  // temporaries end with the block, as a C++ temporary ends with its
  // full-expression.
  template <typename Build>
  auto InEvaluation(BlockRebuild& rebuild, Build build) -> mir::BlockId {
    BlockRebuild in(rebuild.From(), rules_);
    evaluations_.push_back(Evaluation{.in = &in, .steps = {}});
    const std::optional<mir::StmtId> last = build(in);
    std::vector<mir::StmtId> stmts = std::move(evaluations_.back().steps);
    evaluations_.pop_back();
    if (last.has_value()) {
      stmts.push_back(*last);
    }
    return rebuild.Add(std::move(in).Finish(std::move(stmts)));
  }

  // The node standing for `old`, a node of the block `rebuild` builds from,
  // which an evaluation of its own begins at and takes as `taken`, built by
  // `build`: as it stands where the evaluation takes no step, and otherwise a
  // block expression running the steps, then yielding it where the
  // evaluation's value is made.
  template <typename Build>
  auto Evaluated(
      BlockRebuild& rebuild, mir::ExprId old, TakenAs taken, Build build)
      -> mir::ExprId {
    if (!TakesAStep(rebuild.From(), taken, old)) {
      return build(rebuild);
    }
    mir::ExprId value{};
    mir::TypeId type{};
    const mir::BlockId scope = InEvaluation(
        rebuild, [&](BlockRebuild& in) -> std::optional<mir::StmtId> {
          value = build(in);
          type = in.To().exprs.Get(value).type;
          return std::nullopt;
        });
    return rebuild.Add(
        mir::Expr{
            .data = mir::BlockExpr{.scope = scope, .value = value},
            .type = type});
  }

  // The return of an execution answering `old`, a node of the block `rebuild`
  // builds from. Its answer is read as a value and stored into its
  // completion, so it has no place a block expression could yield it into;
  // where the evaluation takes steps they run in a scope around the return,
  // which then reads the value where it lies.
  auto CompletedWith(BlockRebuild& rebuild, mir::ExprId old) -> mir::StmtData {
    if (!TakesAStep(rebuild.From(), TakenAs::kValue, old)) {
      return mir::ReturnStmt{.value = Value(rebuild, old)};
    }
    return mir::BlockStmt{
        .scope = InEvaluation(
            rebuild, [&](BlockRebuild& in) -> std::optional<mir::StmtId> {
              return in.Add(mir::ReturnStmt{.value = Value(in, old)});
            })};
  }

  // The place of a temporary of `type` built from `value`, a node of the block
  // `rebuild` builds, and declared by a step of the evaluation that block
  // holds.
  auto Temporary(BlockRebuild& rebuild, mir::TypeId type, mir::ExprId value)
      -> mir::ExprId {
    if (evaluations_.empty() || evaluations_.back().in != &rebuild) {
      throw InternalError(
          "mir_transform: a temporary was made outside the evaluation it "
          "belongs to");
    }
    const mir::LocalId id = locals_.Add(mir::LocalDecl{.type = type});
    evaluations_.back().steps.push_back(
        rebuild.Add(mir::LocalDeclStmt{.target = id, .init = value}));
    return rebuild.Add(mir::MakeLocalRefExpr(id, type));
  }

  // `operand`, a node of the block `rebuild` builds from, carried over where
  // it is held: a large value it makes is made in a temporary, and what is
  // carried is the temporary's place.
  auto Held(BlockRebuild& rebuild, mir::ExprId operand) -> mir::ExprId {
    const mir::ExprId carried = rebuild.Expr(operand);
    if (!MakesALargeValue(rebuild.From(), operand)) {
      return carried;
    }
    return Temporary(rebuild, rebuild.From().exprs.Get(operand).type, carried);
  }

  // `operand` carried over as a value read: one made in a temporary is read
  // out of it where it lies.
  auto Value(BlockRebuild& rebuild, mir::ExprId operand) -> mir::ExprId {
    const mir::ExprId held = Held(rebuild, operand);
    if (!MakesALargeValue(rebuild.From(), operand)) {
      return held;
    }
    return rebuild.Add(
        mir::MakeCopyExpr(held, rebuild.From().exprs.Get(operand).type));
  }

  // Whether `operand`, read as a value, has to be taken into a temporary of
  // its own before a later operand's step runs, so the steps keep the order
  // its node evaluates its operands in: it computes something, and no
  // temporary holds it already.
  [[nodiscard]] auto TakenFirst(
      BlockRebuild& rebuild, mir::ExprId operand) const -> bool {
    return !MakesALargeValue(rebuild.From(), operand) &&
           !mir::ValueLiesInStorage(*unit_, rebuild.From(), operand) &&
           !mir::EvaluatesNothing(rebuild.From(), operand);
  }

  // `operand` carried over as `taken` takes it, `before_a_step` where a later
  // operand of the same node takes a step.
  auto Operand(
      BlockRebuild& rebuild, TakenAs taken, mir::ExprId operand,
      bool before_a_step) -> mir::ExprId {
    const mir::TypeId type = rebuild.From().exprs.Get(operand).type;
    switch (taken) {
      case TakenAs::kHandedOver:
        if (!before_a_step && mir::IsTooLargeForAFrame(*unit_, type)) {
          return rebuild.Expr(operand);
        }
        [[fallthrough]];
      case TakenAs::kValue:
        if (before_a_step && TakenFirst(rebuild, operand)) {
          return rebuild.Add(
              mir::MakeCopyExpr(
                  Temporary(rebuild, type, rebuild.Expr(operand)), type));
        }
        return Value(rebuild, operand);
      // What is bound where it lies is formed where it is used -- a place
      // keeps its own operands there, and the object a call is entered on may
      // be a reference to part of a value, which exists only where it is
      // taken -- so only a large value made for it is held in a temporary.
      case TakenAs::kInPlace:
        return Held(rebuild, operand);
      case TakenAs::kAsItIs:
        return rebuild.Expr(operand);
      case TakenAs::kSuspendedOn:
        return SuspendedOn(rebuild, operand);
      case TakenAs::kApart:
        return rebuild.Evaluation(operand);
    }
    throw InternalError("mir_transform: an operand is taken in no known way");
  }

  // `expr`, a node of the block `rebuild` builds from, with each operand
  // carried over by `take`, which is told the operand's position among them.
  // A node computing nothing may stand in two positions, and is carried once.
  auto WithOperands(
      BlockRebuild& rebuild, const mir::Expr& expr, const auto& take)
      -> mir::Expr {
    const std::vector<std::pair<mir::ExprId, TakenAs>> operands =
        OperandsOf(*unit_, expr);
    std::map<mir::ExprId, mir::ExprId> carried;
    for (std::size_t at = 0; at < operands.size(); ++at) {
      const auto& [operand, taken] = operands[at];
      if (!carried.contains(operand)) {
        carried.emplace(operand, take(at, operand, taken));
      }
    }
    return mir::Expr{
        .data = rebuild.WithOperandsTaken(
            expr.data,
            [&](mir::ExprId operand) { return carried.at(operand); }),
        .type = expr.type};
  }

  // `call`, the call a suspension waits on, carried over with each argument
  // an evaluation of its own, which ends before the call is made. What a
  // suspension waits on that is no call evaluates nothing to take a step for.
  auto SuspendedOn(BlockRebuild& rebuild, mir::ExprId call) -> mir::ExprId {
    const mir::Expr& expr = rebuild.From().exprs.Get(call);
    if (!std::holds_alternative<mir::CallExpr>(expr.data)) {
      return rebuild.Expr(call);
    }
    return rebuild.Add(WithOperands(
        rebuild, expr, [&](std::size_t, mir::ExprId operand, TakenAs taken) {
          switch (taken) {
            case TakenAs::kValue:
            case TakenAs::kHandedOver:
              return Evaluated(rebuild, operand, taken, [&](BlockRebuild& in) {
                return Operand(in, taken, operand, false);
              });
            case TakenAs::kInPlace:
            case TakenAs::kAsItIs:
            case TakenAs::kSuspendedOn:
            case TakenAs::kApart:
              return Operand(rebuild, taken, operand, false);
          }
          throw InternalError(
              "mir_transform: an operand is taken in no known way");
        }));
  }

  // A node carried over with each operand taken as the rewrite takes it. A
  // conditional and a block expression are left to the rebuild, which begins
  // an evaluation at each arm and at the block's value: the arm a conditional
  // chooses is made where the conditional's value is, as clang evaluates each
  // arm of an aggregate conditional into its destination, and only once it is
  // chosen.
  auto Node(BlockRebuild& rebuild, const mir::Expr& expr)
      -> std::optional<mir::Expr> {
    if (std::holds_alternative<mir::BlockExpr>(expr.data) ||
        std::holds_alternative<mir::ConditionalExpr>(expr.data)) {
      return std::nullopt;
    }
    // One past the last operand taking a step: every operand before it is
    // taken before that step.
    std::size_t steps_until = 0;
    const std::vector<std::pair<mir::ExprId, TakenAs>> operands =
        OperandsOf(*unit_, expr);
    for (std::size_t at = 0; at < operands.size(); ++at) {
      if (TakesAStep(rebuild.From(), operands[at].second, operands[at].first)) {
        steps_until = at + 1;
      }
    }
    return WithOperands(
        rebuild, expr, [&](std::size_t at, mir::ExprId operand, TakenAs taken) {
          return Operand(rebuild, taken, operand, at + 1 < steps_until);
        });
  }

  // An execution's return, whose answer is read into its completion. Every
  // other statement's expressions begin evaluations the rebuild asks for.
  auto Statement(BlockRebuild& rebuild, const mir::Stmt& stmt)
      -> std::optional<mir::StmtData> {
    const auto* returned = std::get_if<mir::ReturnStmt>(&stmt.data);
    if (!completes_ || returned == nullptr || !returned->value.has_value()) {
      return std::nullopt;
    }
    return CompletedWith(rebuild, *returned->value);
  }

  const mir::CompilationUnit* unit_;
  const mir::CallableCode* code_;
  // Whether the body is an execution, which answers through its completion.
  bool completes_;
  // The body's locals, then the temporaries its evaluations declare.
  base::Arena<mir::LocalDecl, mir::LocalId> locals_;
  RebuildRules rules_;
  // The evaluations being built, innermost last.
  std::vector<Evaluation> evaluations_;
  // Whether a node's operands take a step, asked once per node of a block.
  std::map<std::pair<const mir::Block*, mir::ExprId>, bool>
      operands_take_a_step_;
};

}  // namespace

auto HoldLargeValuesInTemporaries(mir::CompilationUnit unit)
    -> mir::CompilationUnit {
  return mir::RewriteEachCode(
      std::move(unit),
      [](const mir::CompilationUnit& unit, const mir::CodeSource& at,
         const mir::CallableCode& code,
         std::span<const mir::ExprId> outside_roots)
          -> std::optional<mir::SourcedCode> {
        // A foreign body is entered by the convention its declaration states,
        // which nothing here changes.
        if (mir::IsEnteredForeign(unit, at) || !code.body.has_value() ||
            !HoldsALargeValue(unit, *code.body)) {
          return std::nullopt;
        }
        BodyTemporaries temporaries(unit, code);
        return temporaries.Rewritten(outside_roots);
      });
}

}  // namespace lyra::mir_transform
