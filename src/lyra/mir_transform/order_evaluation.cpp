#include "lyra/mir_transform/order_evaluation.hpp"

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
#include "lyra/base/overloaded.hpp"
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
  // Bound where it lies for the node's own use -- what a write lands in or an
  // address-of takes, the object a field or a call is reached on -- which such
  // a one is likewise made in a temporary for.
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
  // What a borrow refers to. The reference may be carried past the step that
  // forms it -- into a temporary a step declares, which a later step or the
  // evaluation's value then reads -- so what it refers to has to last as long
  // as the evaluation, as rustc makes a borrowed value a local of its
  // statement and clang keeps it to the end of the full-expression.
  kReferredTo,
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
  if (std::holds_alternative<mir::BorrowExpr>(node.data)) {
    return TakenAs::kReferredTo;
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

// Whether evaluating a node of a block may change or end storage a value was
// read from, asked once per node: a call, a write, a construction, a
// suspension or a move may, and a run of steps is taken to. Anything else only
// reads, so it may only through its operands.
class StorageChanges {
 public:
  auto May(const mir::Block& block, mir::ExprId id) -> bool {
    const std::pair<const mir::Block*, mir::ExprId> key{&block, id};
    if (const auto known = may_.find(key); known != may_.end()) {
      return known->second;
    }
    const bool may = Asked(block, id);
    may_.emplace(key, may);
    return may;
  }

 private:
  auto Asked(const mir::Block& block, mir::ExprId id) -> bool {
    const mir::ExprData& data = block.exprs.Get(id).data;
    const auto through_operands = [&] {
      bool may = false;
      mir::ForEachOperand(data, [&](mir::ExprId operand, mir::OperandUse) {
        may = may || May(block, operand);
      });
      return may;
    };
    return std::visit(
        Overloaded{
            [](const mir::CallExpr&) { return true; },
            [](const mir::AssignExpr&) { return true; },
            [](const mir::NewExpr&) { return true; },
            [](const mir::AwaitExpr&) { return true; },
            [](const mir::WaitExpr&) { return true; },
            [](const mir::MoveExpr&) { return true; },
            [](const mir::BlockExpr&) { return true; },
            [&](const mir::UnaryExpr&) { return through_operands(); },
            [&](const mir::BinaryExpr&) { return through_operands(); },
            [&](const mir::CastExpr&) { return through_operands(); },
            [&](const mir::ConditionalExpr&) { return through_operands(); },
            [&](const mir::CompositeExpr&) { return through_operands(); },
            [&](const mir::UnionExpr&) { return through_operands(); },
            [&](const mir::ClosureExpr&) { return through_operands(); },
            [&](const mir::VectorGetExpr&) { return through_operands(); },
            [&](const mir::DerefExpr&) { return through_operands(); },
            [&](const mir::AddressOfExpr&) { return through_operands(); },
            [&](const mir::BorrowExpr&) { return through_operands(); },
            [&](const mir::CopyExpr&) { return through_operands(); },
            [&](const mir::DiscriminantExpr&) { return through_operands(); },
            [&](const mir::FieldAccessExpr&) { return through_operands(); },
            [](const mir::StringLiteral&) { return false; },
            [](const mir::NullLiteral&) { return false; },
            [](const mir::MachineBoolLiteral&) { return false; },
            [](const mir::MachineIntLiteral&) { return false; },
            [](const mir::MachineFloatLiteral&) { return false; },
            [](const mir::ReferenceExpr&) { return false; },
        },
        data);
  }

  std::map<std::pair<const mir::Block*, mir::ExprId>, bool> may_;
};

// The nodes from the place `place` of `block` down to the read of the handle
// whose object it lies in, the nearest one: through field steps and references
// a call answers into its receiver, to the first dereference of a handle read
// where it lies. Empty where no such handle is reached.
auto HandleReachedThrough(
    const mir::CompilationUnit& unit, const mir::Block& block,
    mir::ExprId place) -> std::vector<mir::ExprId> {
  std::vector<mir::ExprId> chain;
  mir::ExprId at = place;
  while (true) {
    chain.push_back(at);
    const mir::ExprData& data = block.exprs.Get(at).data;
    if (const auto* field = std::get_if<mir::FieldAccessExpr>(&data)) {
      at = field->receiver;
      continue;
    }
    const auto* deref = std::get_if<mir::DerefExpr>(&data);
    if (deref == nullptr) {
      return {};
    }
    const mir::Expr& pointer = block.exprs.Get(deref->pointer);
    if (std::holds_alternative<mir::CopyExpr>(pointer.data) &&
        unit.types.Get(pointer.type).Is<mir::ManagedRefType>()) {
      chain.push_back(deref->pointer);
      return chain;
    }
    const auto* into = std::get_if<mir::CallExpr>(&pointer.data);
    if (into == nullptr || !mir::AnswersReferenceIntoReceiver(*into)) {
      return {};
    }
    chain.push_back(deref->pointer);
    at = *mir::CalleeReceiver(into->callee);
  }
}

// Whether `operand`, a node of `block` taken as `taken`, is read out of
// storage a later operand's evaluation could change or end under it, and so
// is taken into a temporary of its own before that operand runs, as rustc's
// `as_operand` takes a place into a temporary "so that we capture the value of
// `x` at this time": a value the runtime holds rather than plain bits -- a
// handle, a string, a container, a structure -- read where it lies, and the
// handle the object an operation is entered on lies in. A value of plain bits
// is read whole where it stands, and nothing a later step does can end it.
auto IsReadBeforeChange(
    const mir::CompilationUnit& unit, const mir::Block& block,
    mir::ExprId operand, TakenAs taken) -> bool {
  const mir::Expr& expr = block.exprs.Get(operand);
  switch (taken) {
    case TakenAs::kValue:
    case TakenAs::kHandedOver:
      return unit.types.Get(expr.type).IsRuntimeStoredValue() &&
             std::holds_alternative<mir::CopyExpr>(expr.data) &&
             mir::ValueLiesInStorage(unit, block, operand);
    case TakenAs::kInPlace:
      return !HandleReachedThrough(unit, block, operand).empty();
    case TakenAs::kReferredTo:
    case TakenAs::kAsItIs:
    case TakenAs::kSuspendedOn:
    case TakenAs::kApart:
      return false;
  }
  throw InternalError("mir_transform: an operand is taken in no known way");
}

// Which of `operands`, the operands of one node of `block` in the order it
// evaluates them, are read before a later one may change what they read.
auto ReadsBeforeChange(
    const mir::CompilationUnit& unit, const mir::Block& block,
    const std::vector<std::pair<mir::ExprId, TakenAs>>& operands,
    StorageChanges& changes) -> std::vector<bool> {
  std::vector<bool> read_before(operands.size(), false);
  bool changed_after = false;
  for (std::size_t at = operands.size(); at-- > 0;) {
    const auto& [operand, taken] = operands[at];
    read_before[at] =
        changed_after && IsReadBeforeChange(unit, block, operand, taken);
    changed_after = changed_after || changes.May(block, operand);
  }
  return read_before;
}

auto AnyOf(const std::vector<bool>& flags) -> bool {
  return std::ranges::any_of(flags, [](bool flag) { return flag; });
}

// Whether any node of `block`, or of a block nested in it, is of a type too
// large for a frame, or reads an operand a later one may change.
auto NeedsATemporary(
    const mir::CompilationUnit& unit, const mir::Block& block,
    StorageChanges& changes) -> bool {
  return std::ranges::any_of(
             block.exprs,
             [&](const mir::Expr& expr) {
               return mir::IsTooLargeForAFrame(unit, expr.type) ||
                      AnyOf(ReadsBeforeChange(
                          unit, block, OperandsOf(unit, expr), changes));
             }) ||
         std::ranges::any_of(block.child_scopes, [&](const mir::Block& child) {
           return NeedsATemporary(unit, child, changes);
         });
}

// The rewrite of one body. It holds what it learns of the body once, and has a
// step for each kind of operand and statement, each handed the block it
// builds.
class BodyTemporaries {
 public:
  // `changes` is what was already asked of the body's nodes.
  BodyTemporaries(
      const mir::CompilationUnit& unit, const mir::CallableCode& code,
      StorageChanges changes)
      : unit_(&unit),
        code_(&code),
        changes_(std::move(changes)),
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

  // The code rewritten, and the temporaries its evaluations declared among its
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

  // Whether `operand`, a node of `block`, makes a value of a large type rather
  // than naming one that lies in storage.
  [[nodiscard]] auto MakesALargeValue(
      const mir::Block& block, mir::ExprId operand) const -> bool {
    return mir::IsTooLargeForAFrame(*unit_, block.exprs.Get(operand).type) &&
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
        if (MakesALargeValue(block, operand)) {
          return true;
        }
        break;
      case TakenAs::kReferredTo:
        if (PartOfAValueMadeHere(block, operand)) {
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
    const std::vector<std::pair<mir::ExprId, TakenAs>> operands =
        OperandsOf(*unit_, block.exprs.Get(id));
    bool takes = AnyOf(ReadsBeforeChange(*unit_, block, operands, changes_));
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
            : carried);
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
      // taken -- so only a large value made for it is held in a temporary
      // here; the handle its object is reached through, where a later operand
      // may change it, is taken before this is asked.
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

  // `operand`, read out of storage a later operand may change, kept by `keep`
  // -- answered a node of `rebuild` holding what a read of the block rebuilt
  // from read -- before that operand runs: the value read, or the handle the
  // object it lies in is reached through, which then reaches it.
  auto TakenBeforeChange(
      BlockRebuild& rebuild, mir::ExprId operand, const auto& keep)
      -> mir::ExprId {
    const std::vector<mir::ExprId> chain =
        HandleReachedThrough(*unit_, rebuild.From(), operand);
    if (chain.empty()) {
      return keep(rebuild, operand);
    }
    return ReachedThrough(rebuild, chain, keep(rebuild, chain.back()));
  }

  // The place `chain` begins at, rebuilt with the handle read it ends at
  // replaced by `handle`, a node of `rebuild`.
  auto ReachedThrough(
      BlockRebuild& rebuild, std::span<const mir::ExprId> chain,
      mir::ExprId handle) -> mir::ExprId {
    if (chain.size() == 1) {
      return handle;
    }
    const mir::Expr& node = rebuild.From().exprs.Get(chain.front());
    return rebuild.Add(
        mir::Expr{
            .data = rebuild.WithOperandsTaken(
                node.data,
                [&](mir::ExprId operand) {
                  return operand == chain[1]
                             ? ReachedThrough(rebuild, chain.subspan(1), handle)
                             : rebuild.Expr(operand);
                }),
            .type = node.type});
  }

  // What `read`, a node of the block `rebuild` builds from, reads, held in a
  // temporary the evaluation in progress declares.
  auto KeptInTemporary(BlockRebuild& rebuild, mir::ExprId read) -> mir::ExprId {
    const mir::TypeId type = rebuild.From().exprs.Get(read).type;
    return rebuild.Add(
        mir::MakeCopyExpr(Temporary(rebuild, type, rebuild.Expr(read)), type));
  }

  // The same, in an evaluation of its own whose value is what it read: a
  // suspension stands at the top of its statement, so nothing evaluated
  // before it can declare a temporary its operand reads.
  auto KeptInEvaluation(BlockRebuild& rebuild, mir::ExprId read)
      -> mir::ExprId {
    mir::ExprId value{};
    const mir::BlockId scope = InEvaluation(
        rebuild, [&](BlockRebuild& in) -> std::optional<mir::StmtId> {
          value = KeptInTemporary(in, read);
          return std::nullopt;
        });
    return rebuild.Add(
        mir::Expr{
            .data = mir::BlockExpr{.scope = scope, .value = value},
            .type = rebuild.From().exprs.Get(read).type});
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
    const std::vector<bool> read_before = ReadsBeforeChange(
        *unit_, rebuild.From(), OperandsOf(*unit_, expr), changes_);
    return rebuild.Add(WithOperands(
        rebuild, expr, [&](std::size_t at, mir::ExprId operand, TakenAs taken) {
          if (read_before[at]) {
            return TakenBeforeChange(
                rebuild, operand, [this](BlockRebuild& in, mir::ExprId read) {
                  return KeptInEvaluation(in, read);
                });
          }
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
  // chosen.
  auto Node(BlockRebuild& rebuild, const mir::Expr& expr)
      -> std::optional<mir::Expr> {
    if (std::holds_alternative<mir::BlockExpr>(expr.data) ||
        std::holds_alternative<mir::ConditionalExpr>(expr.data)) {
      return std::nullopt;
    }
    // One past the last operand taking a step: every operand before it is
    // taken before that step. An operand read before a later one may change
    // what it read is taken into a temporary, which is a step of its own.
    std::size_t steps_until = 0;
    const std::vector<std::pair<mir::ExprId, TakenAs>> operands =
        OperandsOf(*unit_, expr);
    const std::vector<bool> read_before =
        ReadsBeforeChange(*unit_, rebuild.From(), operands, changes_);
    for (std::size_t at = 0; at < operands.size(); ++at) {
      if (read_before[at] ||
          TakesAStep(rebuild.From(), operands[at].second, operands[at].first)) {
        steps_until = at + 1;
      }
    }
    return WithOperands(
        rebuild, expr, [&](std::size_t at, mir::ExprId operand, TakenAs taken) {
          if (read_before[at]) {
            return TakenBeforeChange(
                rebuild, operand, [this](BlockRebuild& in, mir::ExprId read) {
                  return KeptInTemporary(in, read);
                });
          }
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
  // Whether a node may change storage, asked once per node of a block.
  StorageChanges changes_;
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
        StorageChanges changes;
        if (!NeedsATemporary(unit, *code.body, changes)) {
          return std::nullopt;
        }
        BodyTemporaries temporaries(unit, code, std::move(changes));
        return temporaries.Rewritten();
      });
}

}  // namespace lyra::mir_transform
