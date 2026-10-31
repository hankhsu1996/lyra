#include "lyra/mir_transform/evaluation_temporaries.hpp"

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
#include "lyra/mir/evaluation_order.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/frame_size.hpp"
#include "lyra/mir/local.hpp"
#include "lyra/mir/operand_use.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/mir/storage_part.hpp"
#include "lyra/mir/type.hpp"
#include "lyra/mir/type_builders.hpp"
#include "lyra/mir/verify.hpp"
#include "lyra/mir_transform/block_rebuild.hpp"

namespace lyra::mir_transform {

namespace {

// How the rewrite takes an operand: how its node takes it, and what the node
// then does with it.
enum class TakenAs : std::uint8_t {
  // Read as a value, which one made of a type the rewrite holds in a temporary
  // is made in one for.
  kValue,
  // Bound where it lies for the node's own use -- what a write lands in or an
  // address-of takes, the object a field or a call is reached on -- which such
  // a one is likewise made in a temporary for.
  kInPlace,
  // Built into storage the node or its callee owns -- what a new-expression
  // allocates, from the heap or from its placement, a capture, an argument of
  // a body this program compiles, which takes a large one in a box built where
  // it is handed -- so a large one is made there unless a later operand's step
  // has to follow it, and a small one is read.
  kHandedOver,
  // Formed where its node is, with no temporary of its own: a place a read or
  // a move takes, or the pointer a dereference opens, which is that place's
  // own operand.
  kAsItIs,
  // The call a suspension waits on, which stands under the suspension as it
  // is, since a suspension stands at the top of its statement and none falls
  // among a block expression's steps.
  kSuspendedOn,
  // An arm of a choice, which begins an evaluation of its own.
  kApart,
  // What a borrow refers to. The reference may be carried past the step that
  // forms it -- into a temporary a step declares, which a later step or the
  // evaluation's value then reads -- so what it refers to has to last as long
  // as the evaluation, as rustc makes a borrowed value a local of its
  // statement and clang keeps it to the end of the full-expression.
  kReferredTo,
};

// Every operand a target could run in the other order to another answer is
// taken ahead of what follows it, whatever its size.
struct ForEvaluationOrder {
  // What the order of the body's evaluations asks of its nodes.
  mir::EvaluationOrder* order;
};

// Every value a frame keeps apart that an expression makes and another node
// reads is given one, which is then held through a box; the plan names each
// temporary among the locals, apart or not.
struct ForValuesKeptApart {
  FramePlan* plan;
};

// Which operands a rewrite gives a temporary of its own, beside the ones every
// rewrite gives to keep the order its steps take.
using TemporaryFor = std::variant<ForEvaluationOrder, ForValuesKeptApart>;

// Where a value made here that a node is entered on is made: where the node
// uses it, or materialized first in a temporary of the evaluation, as clang
// materializes a prvalue a reference binds to, which a node held in a
// temporary itself asks for so that a view it answers into the value lasts as
// long as the node's own temporary does.
enum class InPlaceOperand : std::uint8_t { kFormedWhereUsed, kMaterialized };

// Where a temporary's value is kept: in the frame, or apart from it, in
// storage of its own a box holds.
enum class KeptBy : std::uint8_t { kTheFrame, kStorageOfItsOwn };

// How the rewrite takes `operand` of `node`, which `node` takes as `use`.
auto Taking(
    const mir::CompilationUnit& unit, const mir::Expr& node,
    mir::ExprId operand, mir::OperandUse use) -> TakenAs {
  switch (mir::WhereOperandIsMade(node.data, operand)) {
    case mir::OperandMadeIn::kTheRuntime:
      return TakenAs::kSuspendedOn;
    case mir::OperandMadeIn::kTheOperationsValue:
      return TakenAs::kApart;
    case mir::OperandMadeIn::kTheOperationsAllocation:
      return TakenAs::kHandedOver;
    case mir::OperandMadeIn::kWhereTaken:
      break;
  }
  if (std::holds_alternative<mir::BorrowExpr>(node.data)) {
    return TakenAs::kReferredTo;
  }
  if (std::holds_alternative<mir::ClosureExpr>(node.data)) {
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

// Whether a temporary can stand in for a value of movability `movability`.
auto IsMovable(mir::Movability movability) -> bool {
  switch (movability) {
    case mir::Movability::kCopyable:
    case mir::Movability::kMovable:
      return true;
    case mir::Movability::kImmovable:
      return false;
  }
  throw InternalError("mir_transform: unknown movability");
}

// Whether a value of movability `movability` is read out of its temporary as
// a copy.
auto IsCopyable(mir::Movability movability) -> bool {
  switch (movability) {
    case mir::Movability::kCopyable:
      return true;
    case mir::Movability::kMovable:
    case mir::Movability::kImmovable:
      return false;
  }
  throw InternalError("mir_transform: unknown movability");
}

// Whether the operand `operand` of a node of `block`, taken as `taken`, is
// taken whole into a temporary where it has to run before a later operand: a
// value made here that a copy holds entire, which an operation entered on it
// is entered on where the temporary holds it, and a value a borrow refers to
// that the evaluation makes. Any other is formed where its node is, and what
// of it has to run first is taken out of it instead.
auto CanBeTakenWhole(
    const mir::CompilationUnit& unit, const mir::Block& block,
    mir::ExprId operand, TakenAs taken) -> bool {
  const mir::Expr& expr = block.exprs.Get(operand);
  const mir::Type& type = unit.types.Get(expr.type);
  switch (taken) {
    case TakenAs::kValue:
    case TakenAs::kHandedOver:
    case TakenAs::kInPlace:
      return !mir::IsPlace(expr.data) && IsMovable(type.ValueMovability());
    // A pointer a dereference opens is read out of the temporary, never moved
    // out of it, since the dereference reaches what it points to in place.
    case TakenAs::kAsItIs:
      return !mir::IsPlace(expr.data) && IsCopyable(type.ValueMovability());
    // What a borrow refers to that the evaluation makes is made in a temporary
    // the borrow then refers to, as rustc makes a borrowed value a local.
    case TakenAs::kReferredTo:
      return !mir::WholeLiesInStorage(unit, block, operand);
    case TakenAs::kSuspendedOn:
    case TakenAs::kApart:
      return false;
  }
  throw InternalError("mir_transform: an operand is taken in no known way");
}

// What the order of a node's evaluation asks of each of `operands`, the
// operands of the node `node` of `block` in the order it evaluates them, under
// a rewrite giving `given`, with `after` running after the node: whether it
// needs a temporary, which one a target could run in the other order to
// another answer does -- C++ leaves an operator's operands unordered and a
// call's arguments in an order of its own ([intro.execution], [expr.call]) --
// as rustc's builder takes each operand into a temporary in the order it
// runs, so the order the node states is the one every target runs.
auto OperandOrders(
    const mir::Block& block, const mir::Expr& node,
    const std::vector<std::pair<mir::ExprId, TakenAs>>& operands,
    const mir::OperandEffects& after, const TemporaryFor& given)
    -> std::vector<mir::OperandOrder> {
  const auto* ordering = std::get_if<ForEvaluationOrder>(&given);
  if (ordering == nullptr) {
    return std::vector<mir::OperandOrder>(operands.size());
  }
  std::vector<mir::ExprId> ids;
  ids.reserve(operands.size());
  for (const auto& entry : operands) {
    ids.push_back(entry.first);
  }
  return ordering->order->OperandsNeedingTemporaries(
      block, node.data, ids, after);
}

// Whether a rewrite giving `given` gives an operand of `node`, a node of
// `block`, a temporary.
auto NeedsATemporary(
    const mir::CompilationUnit& unit, const mir::Block& block,
    const mir::Expr& node, const TemporaryFor& given) -> bool {
  return std::ranges::any_of(
      OperandOrders(block, node, OperandsOf(unit, node), {}, given),
      &mir::OperandOrder::needs_temporary);
}

// Whether any node of `block`, or of a block nested in it, gives an operand a
// temporary.
auto HasOrderToMakeExplicit(
    const mir::CompilationUnit& unit, const mir::Block& block,
    mir::EvaluationOrder& order) -> bool {
  return std::ranges::any_of(
             block.exprs,
             [&](const mir::Expr& expr) {
               return NeedsATemporary(
                   unit, block, expr, ForEvaluationOrder{.order = &order});
             }) ||
         std::ranges::any_of(block.child_scopes, [&](const mir::Block& child) {
           return HasOrderToMakeExplicit(unit, child, order);
         });
}

// Whether any node of `block`, or of a block nested in it, is of a type too
// large for a frame.
auto MakesALargeValue(const mir::CompilationUnit& unit, const mir::Block& block)
    -> bool {
  return std::ranges::any_of(
             block.exprs,
             [&](const mir::Expr& expr) {
               return mir::IsTooLargeForAFrame(unit, expr.type);
             }) ||
         std::ranges::any_of(block.child_scopes, [&](const mir::Block& child) {
           return MakesALargeValue(unit, child);
         });
}

// The call a suspension standing at the top of `stmt`, a statement of `block`,
// waits on, where one stands there.
auto CallWaitedOnAtTop(const mir::Block& block, const mir::Stmt& stmt)
    -> std::optional<mir::ExprId> {
  std::optional<mir::ExprId> top;
  if (const auto* evaluated = std::get_if<mir::ExprStmt>(&stmt.data)) {
    top = evaluated->expr;
  } else if (
      const auto* declared = std::get_if<mir::LocalDeclStmt>(&stmt.data)) {
    top = declared->init;
  } else if (const auto* returned = std::get_if<mir::ReturnStmt>(&stmt.data)) {
    top = returned->value;
  }
  if (!top.has_value()) {
    return std::nullopt;
  }
  const mir::ExprData& data = block.exprs.Get(*top).data;
  std::optional<mir::ExprId> waited_on;
  if (const auto* await = std::get_if<mir::AwaitExpr>(&data)) {
    waited_on = await->execution;
  } else if (const auto* wait = std::get_if<mir::WaitExpr>(&data)) {
    waited_on = wait->park;
  }
  if (!waited_on.has_value() || !std::holds_alternative<mir::CallExpr>(
                                    block.exprs.Get(*waited_on).data)) {
    return std::nullopt;
  }
  return waited_on;
}

// The rewrite of one body. It holds what it learns of the body once, and has a
// step for each kind of operand and statement, each handed the block it
// builds.
class BodyTemporaries {
 public:
  BodyTemporaries(
      const mir::CompilationUnit& unit, const mir::CallableCode& code,
      TemporaryFor given)
      : unit_(&unit),
        code_(&code),
        given_(given),
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
                },
            .discarded =
                [this](BlockRebuild& rebuild, mir::ExprId old) {
                  return Discarded(rebuild, old);
                }} {
  }

  BodyTemporaries(const BodyTemporaries&) = delete;
  BodyTemporaries(BodyTemporaries&&) = delete;
  auto operator=(const BodyTemporaries&) -> BodyTemporaries& = delete;
  auto operator=(BodyTemporaries&&) -> BodyTemporaries& = delete;
  ~BodyTemporaries() = default;

  // The code rewritten, with the temporaries its evaluations declared among its
  // locals.
  auto Rewritten() -> mir::CallableCode {
    mir::CallableCode rewritten = RebuildCode(*code_, rules_);
    rewritten.locals = std::move(locals_);
    return rewritten;
  }

 private:
  // An evaluation being built: the block its steps and what they serve are
  // built in, and the steps taken so far, in the order they run.
  struct Evaluation {
    const BlockRebuild* in;
    std::vector<mir::StmtId> steps;
  };

  // Whether the frame keeps apart the value `operand`, a node of `block`,
  // stands for: one too large for a frame wherever it stands, and one the
  // plan finds no room for.
  [[nodiscard]] auto IsKeptApart(
      const mir::Block& block, mir::ExprId operand) const -> bool {
    const auto* apart = std::get_if<ForValuesKeptApart>(&given_);
    return apart != nullptr &&
           (mir::IsTooLargeForAFrame(*unit_, block.exprs.Get(operand).type) ||
            apart->plan->values_apart.contains(std::pair{&block, operand}));
  }

  // Whether `operand`, a node of `block`, makes a value this rewrite holds in
  // a temporary rather than naming one that lies in storage.
  [[nodiscard]] auto MakesAValueItHolds(
      const mir::Block& block, mir::ExprId operand) const -> bool {
    return IsKeptApart(block, operand) &&
           !mir::ValueLiesInStorage(*unit_, block, operand);
  }

  // Whether `operand`, a node of `block` a borrow refers to, is part of a value
  // made here rather than of storage that is already there.
  [[nodiscard]] auto PartOfAValueMadeHere(
      const mir::Block& block, mir::ExprId operand) const -> bool {
    return !mir::WholeLiesInStorage(*unit_, block, operand);
  }

  // Whether taking `operand`, a node of `block`, as `taken` takes a step of
  // the evaluation it belongs to: it is made in a temporary, or one of its own
  // operands is.
  [[nodiscard]] auto TakesAStep(
      const mir::Block& block, TakenAs taken, mir::ExprId operand) -> bool {
    switch (taken) {
      case TakenAs::kValue:
      case TakenAs::kInPlace:
        if (MakesAValueItHolds(block, operand)) {
          return true;
        }
        break;
      case TakenAs::kReferredTo:
        if (PartOfAValueMadeHere(block, operand)) {
          return true;
        }
        break;
      // One too large for a frame is made where it is handed over, and any
      // other the rewrite holds is held as a value read is.
      case TakenAs::kHandedOver:
        if (MakesAValueItHolds(block, operand) &&
            !mir::IsTooLargeForAFrame(*unit_, block.exprs.Get(operand).type)) {
          return true;
        }
        break;
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
    const mir::Expr& node = block.exprs.Get(id);
    const std::vector<std::pair<mir::ExprId, TakenAs>> operands =
        OperandsOf(*unit_, node);
    bool takes = NeedsATemporary(*unit_, block, node, given_);
    for (const auto& [operand, taken] : operands) {
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
    BlockRebuild in = rebuild.Within(rebuild.From());
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
  // holds; the frame keeps it apart where it holds a value `kept` says the
  // frame keeps apart.
  auto Temporary(
      BlockRebuild& rebuild, mir::TypeId type, mir::ExprId value,
      KeptBy kept = KeptBy::kTheFrame) -> mir::ExprId {
    if (!IsEvaluating(rebuild)) {
      throw InternalError(
          "mir_transform: a temporary was made outside the evaluation it "
          "belongs to");
    }
    const mir::LocalId id = locals_.Add(mir::LocalDecl{.type = type});
    if (auto* apart = std::get_if<ForValuesKeptApart>(&given_)) {
      switch (kept) {
        case KeptBy::kTheFrame:
          apart->plan->locals_apart.push_back(false);
          break;
        case KeptBy::kStorageOfItsOwn:
          apart->plan->locals_apart.push_back(true);
          break;
      }
    }
    evaluations_.back().steps.push_back(
        rebuild.Add(mir::LocalDeclStmt{.target = id, .init = value}));
    return rebuild.Add(mir::MakeLocalRefExpr(id, type));
  }

  // Where the frame keeps the value `operand`, a node of `block`, stands for.
  [[nodiscard]] auto KeptAt(const mir::Block& block, mir::ExprId operand) const
      -> KeptBy {
    return IsKeptApart(block, operand) ? KeptBy::kStorageOfItsOwn
                                       : KeptBy::kTheFrame;
  }

  // `operand`, a node of the block `rebuild` builds from, carried over where
  // it is held: a value it makes that the rewrite holds in a temporary is made
  // in one, and what is carried is the temporary's place.
  auto Held(BlockRebuild& rebuild, mir::ExprId operand) -> mir::ExprId {
    const mir::ExprId carried = rebuild.Expr(operand);
    if (!MakesAValueItHolds(rebuild.From(), operand)) {
      return carried;
    }
    return Temporary(
        rebuild, rebuild.From().exprs.Get(operand).type, carried,
        KeptBy::kStorageOfItsOwn);
  }

  // `operand`, a node of the block `rebuild` builds from that a borrow refers
  // to, carried over where it lasts as long as the evaluation: a value made
  // here is made in a temporary of the evaluation, and a place inside one is
  // held as the value read from it, since nothing else refers into a value the
  // evaluation makes.
  auto Referred(BlockRebuild& rebuild, mir::ExprId operand) -> mir::ExprId {
    const mir::ExprId carried = rebuild.Expr(operand);
    if (!PartOfAValueMadeHere(rebuild.From(), operand)) {
      return carried;
    }
    const mir::Expr& referent = rebuild.From().exprs.Get(operand);
    return Temporary(
        rebuild, referent.type,
        mir::IsPlace(referent.data)
            ? rebuild.Add(mir::MakeCopyExpr(carried, referent.type))
            : carried,
        KeptAt(rebuild.From(), operand));
  }

  // `operand` carried over as a value read: one made in a temporary is read
  // out of it where it lies.
  auto Value(BlockRebuild& rebuild, mir::ExprId operand) -> mir::ExprId {
    const mir::ExprId held = Held(rebuild, operand);
    if (!MakesAValueItHolds(rebuild.From(), operand)) {
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
    return !MakesAValueItHolds(rebuild.From(), operand) &&
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
        if (!before_a_step &&
            std::holds_alternative<ForValuesKeptApart>(given_) &&
            mir::IsTooLargeForAFrame(*unit_, type)) {
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
      // taken -- so only a value made for it that the rewrite holds is held
      // in a temporary here.
      case TakenAs::kInPlace:
        return Held(rebuild, operand);
      case TakenAs::kReferredTo:
        return Referred(rebuild, operand);
      case TakenAs::kAsItIs:
        return rebuild.Expr(operand);
      case TakenAs::kSuspendedOn:
        return SuspendedOn(rebuild, operand);
      case TakenAs::kApart:
        return rebuild.Evaluation(operand);
    }
    throw InternalError("mir_transform: an operand is taken in no known way");
  }

  // What `read`, a node of the block `rebuild` builds from, makes, held in a
  // temporary the evaluation in progress declares and read out of it, as a
  // copy where a copy holds the whole of it and moved out of it otherwise. A
  // value made here that `read` is entered on is held first, in its turn among
  // `read`'s operands, so a view `read` answers into it -- the C string of a
  // string -- lasts as long as the evaluation's temporaries do, as a C++
  // temporary lasts to the end of its full-expression.
  auto KeptInTemporary(BlockRebuild& rebuild, mir::ExprId read) -> mir::ExprId {
    const mir::TypeId type = rebuild.From().exprs.Get(read).type;
    const mir::Expr& node = rebuild.From().exprs.Get(read);
    std::optional<mir::Expr> made =
        Node(rebuild, node, {}, InPlaceOperand::kMaterialized);
    const mir::ExprId kept = Temporary(
        rebuild, type,
        made.has_value() ? rebuild.Add(*std::move(made)) : rebuild.Expr(read));
    if (IsCopyable(unit_->types.Get(type).ValueMovability())) {
      return rebuild.Add(mir::MakeCopyExpr(kept, type));
    }
    return rebuild.Add(
        mir::Expr{.data = mir::MoveExpr{.operand = kept}, .type = type});
  }

  // `operand`, a node of the block `rebuild` builds from taken as `taken`,
  // hoisted ahead of `after`, what runs after it, as LLVM's LICM hoists an
  // instruction ahead of a loop. One taken whole is held in a temporary the
  // evaluation in progress declares -- a value read out of it, or, for a value
  // an operation is entered on, the temporary itself. Any other is formed
  // where its node is, as a closure is made where its captures' references
  // were handed out and a write in progress stands for storage, so its own
  // operands are taken ahead of what runs after it instead. What is left of it
  // still stands beside what runs after it, so whether it is in the form the
  // order needs is what the verification of the unit asks. A conditional and
  // a block expression are left to the rebuild, which begins an evaluation at
  // each arm and at the block's value.
  auto Hoisted(
      BlockRebuild& rebuild, mir::ExprId operand, TakenAs taken,
      const mir::OperandEffects& after) -> mir::ExprId {
    const mir::Expr& node = rebuild.From().exprs.Get(operand);
    if (!CanBeTakenWhole(*unit_, rebuild.From(), operand, taken)) {
      std::optional<mir::Expr> formed = Node(rebuild, node, after);
      return formed.has_value() ? rebuild.Add(*std::move(formed))
                                : rebuild.Expr(operand);
    }
    switch (taken) {
      case TakenAs::kInPlace:
        return Temporary(rebuild, node.type, rebuild.Expr(operand));
      case TakenAs::kReferredTo:
        return Referred(rebuild, operand);
      case TakenAs::kValue:
      case TakenAs::kHandedOver:
      case TakenAs::kAsItIs:
        return KeptInTemporary(rebuild, operand);
      // Neither is ever taken whole.
      case TakenAs::kSuspendedOn:
      case TakenAs::kApart:
        break;
    }
    throw InternalError("mir_transform: an operand is taken in no known way");
  }

  // Whether an evaluation is being built over the block `rebuild` builds, so a
  // temporary can be declared by one of its steps.
  [[nodiscard]] auto IsEvaluating(const BlockRebuild& rebuild) const -> bool {
    return !evaluations_.empty() && evaluations_.back().in == &rebuild;
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

  // `call`, the call a suspension waits on, carried over. A suspension stands
  // at the top of its statement and none falls among a block expression's
  // steps, so where an argument has to be taken before a later one runs, the
  // statement is put in an evaluation of its own and the call's operands are
  // taken as any node's are; otherwise each argument is an evaluation of its
  // own, which ends before the call is made. What a suspension waits on that is
  // no call evaluates nothing to take a step for.
  auto SuspendedOn(BlockRebuild& rebuild, mir::ExprId call) -> mir::ExprId {
    const mir::Expr& expr = rebuild.From().exprs.Get(call);
    if (!std::holds_alternative<mir::CallExpr>(expr.data)) {
      return rebuild.Expr(call);
    }
    if (IsEvaluating(rebuild)) {
      return rebuild.Add(*Node(rebuild, expr));
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
            case TakenAs::kReferredTo:
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
  // chosen. `after` is what runs after the node, where the node is what is
  // left of an operand that is not taken whole.
  auto Node(
      BlockRebuild& rebuild, const mir::Expr& expr,
      const mir::OperandEffects& after = {},
      InPlaceOperand in_place = InPlaceOperand::kFormedWhereUsed)
      -> std::optional<mir::Expr> {
    if (std::holds_alternative<mir::BlockExpr>(expr.data) ||
        std::holds_alternative<mir::ConditionalExpr>(expr.data)) {
      return std::nullopt;
    }
    // One past the last operand taking a step: every operand before it is
    // taken before that step. An operand a target could run in the other
    // order to another answer is taken ahead, which is a step of its own, and
    // so is a value made here that the node is entered on where it is held
    // first.
    std::size_t steps_until = 0;
    const std::vector<std::pair<mir::ExprId, TakenAs>> operands =
        OperandsOf(*unit_, expr);
    const std::vector<mir::OperandOrder> orders =
        OperandOrders(rebuild.From(), expr, operands, after, given_);
    // Whether the operand at `at` is a value made here that the node is
    // entered on, materialized in a temporary of its own in its turn.
    const auto materialized = [&](std::size_t at) {
      switch (in_place) {
        case InPlaceOperand::kFormedWhereUsed:
          return false;
        case InPlaceOperand::kMaterialized:
          break;
      }
      const auto& [operand, taken] = operands[at];
      switch (taken) {
        case TakenAs::kInPlace:
          return CanBeTakenWhole(*unit_, rebuild.From(), operand, taken);
        case TakenAs::kValue:
        case TakenAs::kHandedOver:
        case TakenAs::kAsItIs:
        case TakenAs::kReferredTo:
        case TakenAs::kSuspendedOn:
        case TakenAs::kApart:
          return false;
      }
      throw InternalError("mir_transform: an operand is taken in no known way");
    };
    for (std::size_t at = 0; at < operands.size(); ++at) {
      if (orders[at].needs_temporary || materialized(at) ||
          TakesAStep(rebuild.From(), operands[at].second, operands[at].first)) {
        steps_until = at + 1;
      }
    }
    return WithOperands(
        rebuild, expr, [&](std::size_t at, mir::ExprId operand, TakenAs taken) {
          if (orders[at].needs_temporary) {
            return Hoisted(rebuild, operand, taken, orders[at].after);
          }
          if (materialized(at)) {
            return Temporary(
                rebuild, rebuild.From().exprs.Get(operand).type,
                rebuild.Expr(operand));
          }
          return Operand(rebuild, taken, operand, at + 1 < steps_until);
        });
  }

  // `old`, a node of the block `rebuild` builds from whose value its statement
  // discards, carried over: a value the frame keeps apart is made in a box the
  // statement discards with it, and so is one whose evaluation takes steps to
  // hold its operands apart, since the block running them would otherwise
  // yield it into the frame.
  auto Discarded(BlockRebuild& rebuild, mir::ExprId old) -> mir::ExprId {
    const mir::Block& from = rebuild.From();
    const mir::TypeId type = from.exprs.Get(old).type;
    const bool boxed = MakesAValueItHolds(from, old) ||
                       (std::holds_alternative<ForValuesKeptApart>(given_) &&
                        mir::FrameBytesOf(*unit_, type) > 0 &&
                        TakesAStep(from, TakenAs::kAsItIs, old));
    const mir::ExprId evaluated = rebuild.Evaluation(old);
    if (!boxed) {
      return evaluated;
    }
    return rebuild.Add(
        mir::MakeNewExpr(evaluated, mir::BoxOf(unit_->types, type)));
  }

  // A statement a suspension stands at the top of, whose call takes an
  // operand into a temporary before a later one runs, in an evaluation of its
  // own: the temporaries are declared by its steps and the statement runs
  // after them. An execution's return, whose answer is read into its
  // completion. Every other statement's expressions begin evaluations the
  // rebuild asks for.
  auto Statement(BlockRebuild& rebuild, const mir::Stmt& stmt)
      -> std::optional<mir::StmtData> {
    if (const std::optional<mir::ExprId> call =
            CallWaitedOnAtTop(rebuild.From(), stmt);
        call.has_value() &&
        NeedsATemporary(
            *unit_, rebuild.From(), rebuild.From().exprs.Get(*call), given_)) {
      // A declaration names its local for the rest of its block, so a block
      // around it would end the name with it: its steps run just before it,
      // in its own block, and its temporaries last as long as that block.
      if (std::holds_alternative<mir::LocalDeclStmt>(stmt.data)) {
        evaluations_.push_back(Evaluation{.in = &rebuild, .steps = {}});
        mir::StmtData declared = rebuild.WithOperandsCarried(stmt.data);
        for (const mir::StmtId step : evaluations_.back().steps) {
          rebuild.Precede(step);
        }
        evaluations_.pop_back();
        return declared;
      }
      return mir::BlockStmt{
          .scope = InEvaluation(
              rebuild, [&](BlockRebuild& in) -> std::optional<mir::StmtId> {
                return in.Add(in.WithOperandsCarried(stmt.data));
              })};
    }
    const auto* returned = std::get_if<mir::ReturnStmt>(&stmt.data);
    if (!completes_ || returned == nullptr || !returned->value.has_value()) {
      return std::nullopt;
    }
    return CompletedWith(rebuild, *returned->value);
  }

  const mir::CompilationUnit* unit_;
  const mir::CallableCode* code_;
  // Which operands the rewrite gives a temporary.
  TemporaryFor given_;
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

auto OrderEvaluation(mir::CompilationUnit unit) -> mir::CompilationUnit {
  return mir::RewriteEachCode(
      std::move(unit),
      [](const mir::CompilationUnit& unit, const mir::CodeSource& at,
         const mir::CallableCode& code) -> std::optional<mir::CallableCode> {
        // A foreign body is entered by the convention its declaration states,
        // which nothing here changes.
        if (mir::IsEnteredForeign(unit, at) || !code.body.has_value()) {
          return std::nullopt;
        }
        mir::EvaluationOrder order(unit.types, *code.body);
        if (!HasOrderToMakeExplicit(unit, *code.body, order)) {
          return std::nullopt;
        }
        BodyTemporaries temporaries(
            unit, code, ForEvaluationOrder{.order = &order});
        return temporaries.Rewritten();
      });
}

auto GiveTemporariesToValuesKeptApart(
    const mir::CompilationUnit& unit, const mir::CallableCode& code,
    FramePlan& plan) -> std::optional<mir::CallableCode> {
  if (!code.body.has_value() ||
      (plan.values_apart.empty() && !MakesALargeValue(unit, *code.body))) {
    return std::nullopt;
  }
  BodyTemporaries temporaries(unit, code, ForValuesKeptApart{.plan = &plan});
  return temporaries.Rewritten();
}

}  // namespace lyra::mir_transform
