#include "lyra/mir/evaluation_order.hpp"

#include <algorithm>
#include <cstddef>
#include <optional>
#include <set>
#include <span>
#include <utility>
#include <variant>
#include <vector>

#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/expr_id.hpp"
#include "lyra/mir/local.hpp"
#include "lyra/mir/operand_use.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/mir/storage_part.hpp"
#include "lyra/mir/type.hpp"

namespace lyra::mir {

namespace {

// The local of the frame a place of `block` lies in, where it lies in one: a
// local, a component of one reached by field steps, or what a box a local holds
// owns, which nothing reaches but through that local -- the base local of the
// place, as rustc names a place by `Place::local` and its projections. A place
// reached through any other pointer lies wherever the pointer points.
auto BaseLocal(const TypePool& types, const Block& block, ExprId place)
    -> std::optional<LocalId> {
  const ExprData& data = block.exprs.Get(place).data;
  if (const auto* field = std::get_if<FieldAccessExpr>(&data)) {
    return BaseLocal(types, block, field->receiver);
  }
  if (const auto* deref = std::get_if<DerefExpr>(&data)) {
    const Expr& pointer = block.exprs.Get(deref->pointer);
    const auto* box = types.Get(pointer.type).As<PointerType>();
    if (box == nullptr) {
      return std::nullopt;
    }
    switch (box->ownership) {
      case PointerOwnership::kUnique:
        break;
      case PointerOwnership::kShared:
      case PointerOwnership::kBorrowed:
        return std::nullopt;
    }
    // The box opened where it lies, or read out of where it lies.
    const auto* read = std::get_if<CopyExpr>(&pointer.data);
    return BaseLocal(
        types, block, read != nullptr ? read->place : deref->pointer);
  }
  return ReferencedLocal(data);
}

// Adds to `borrowed` every local of `block`, and of the blocks nested in it,
// whose address something takes, as rustc's `borrowed_locals` gathers them: a
// borrow, an address-of, a call binding a reference to the storage it is
// handed, and a call answering a reference through which what it is entered on
// can be written. What the call answers is part of the entry's signature, which
// every target reads to make the call, never a statement of what it does.
void AddBorrowedLocals(
    const TypePool& types, const Block& block, std::set<LocalId>& borrowed) {
  const auto taken = [&](ExprId place) {
    if (const std::optional<LocalId> local = BaseLocal(types, block, place)) {
      borrowed.insert(*local);
    }
  };
  for (const Expr& expr : block.exprs) {
    if (const auto* borrow = std::get_if<BorrowExpr>(&expr.data)) {
      taken(borrow->operand);
    } else if (const auto* address = std::get_if<AddressOfExpr>(&expr.data)) {
      taken(address->operand);
    } else if (const auto* call = std::get_if<CallExpr>(&expr.data)) {
      if (BindsReference(types, *call, expr.type)) {
        ForEachOperand(
            expr.data, [&](ExprId operand, OperandUse) { taken(operand); });
      } else if (const std::optional<ExprId> receiver =
                     CalleeReceiver(call->callee);
                 receiver.has_value() &&
                 AnswersMutableReferenceIntoReceiver(*call)) {
        taken(*receiver);
      }
    }
  }
  for (const Block& child : block.child_scopes) {
    AddBorrowedLocals(types, child, borrowed);
  }
}

// Whether `call`, answering a value of `answer`, is trivial: it only makes a
// value out of its operands. A construction is, whether the value is the
// program's or the library's, whose constructors register nothing, and so is a
// call answering an execution, which allocates its frame and binds its
// arguments while its body runs from its first statement only once it is
// driven, on both backends. Every other call runs code that may read and
// change anything it reaches.
auto IsTrivialCall(const TypePool& types, const CallExpr& call, TypeId answer)
    -> bool {
  return std::holds_alternative<Construct>(call.callee) ||
         types.Get(answer).Is<CoroutineType>();
}

// `from` taken together with `into`.
void Add(OperandEffects& into, const OperandEffects& from) {
  into.side_effects = into.side_effects || from.side_effects;
  into.reads_other_storage =
      into.reads_other_storage || from.reads_other_storage;
  into.locals_read.insert(from.locals_read.begin(), from.locals_read.end());
  into.locals_mutated.insert(
      from.locals_mutated.begin(), from.locals_mutated.end());
}

auto Intersect(const std::set<LocalId>& one, const std::set<LocalId>& other)
    -> bool {
  return std::ranges::any_of(
      one, [&](LocalId local) { return other.contains(local); });
}

}  // namespace

EvaluationOrder::EvaluationOrder(const TypePool& types, const Block& body)
    : types_(&types) {
  AddBorrowedLocals(types, body, borrowed_);
}

auto EvaluationOrder::EffectsOf(const Block& block, ExprId id)
    -> const OperandEffects& {
  const std::pair<const Block*, ExprId> key{&block, id};
  if (const auto known = effects_.find(key); known != effects_.end()) {
    return known->second;
  }
  OperandEffects effects;
  // A read of the place `place` of `in`: of a local whose address nothing
  // takes, which only an evaluation naming it changes, or of other storage.
  const auto read = [&](const Block& in, ExprId place) {
    const std::optional<LocalId> local = BaseLocal(*types_, in, place);
    if (local.has_value() && !borrowed_.contains(*local)) {
      effects.locals_read.insert(*local);
    } else {
      effects.reads_other_storage = true;
    }
  };
  const auto mutated = [&](const Block& in, ExprId place) {
    if (const std::optional<LocalId> local = BaseLocal(*types_, in, place)) {
      effects.locals_mutated.insert(*local);
    }
  };
  // What the node `node` of `in` does itself, apart from its operands.
  const auto own = [&](const Block& in, ExprId node) {
    const Expr& expr = in.exprs.Get(node);
    std::visit(
        Overloaded{
            [&](const CallExpr& call) {
              if (!IsTrivialCall(*types_, call, expr.type)) {
                effects.side_effects = true;
                effects.reads_other_storage = true;
              }
              ForEachOperandAsTaken(
                  *types_, expr, [&](ExprId operand, OperandUse use) {
                    switch (use) {
                      case OperandUse::kStorage:
                      case OperandUse::kObject:
                        mutated(in, operand);
                        break;
                      case OperandUse::kValue:
                      case OperandUse::kRead:
                      case OperandUse::kPointer:
                        break;
                    }
                  });
            },
            [&](const AssignExpr& write) {
              effects.side_effects = true;
              effects.reads_other_storage = true;
              mutated(in, write.target);
            },
            [&](const AwaitExpr&) {
              effects.side_effects = true;
              effects.reads_other_storage = true;
            },
            [&](const WaitExpr&) {
              effects.side_effects = true;
              effects.reads_other_storage = true;
            },
            [&](const CopyExpr& e) { read(in, e.place); },
            // A move is a last use, so nothing after it names what it moved.
            [&](const MoveExpr& e) {
              read(in, e.operand);
              mutated(in, e.operand);
            },
            [&](const BorrowExpr& e) { mutated(in, e.operand); },
            [&](const AddressOfExpr& e) { mutated(in, e.operand); },
            // What a choice or a run of steps evaluates to is made by what it
            // runs, which no operand of the node it is stands for.
            [&](const ConditionalExpr&) { effects.reads_other_storage = true; },
            [&](const BlockExpr&) { effects.reads_other_storage = true; },
            // Allocating changes nothing a program reads, since an object ends
            // when its last share is let go of rather than when storage is
            // next asked for.
            [](const NewExpr&) {},
            [](const UnaryExpr&) {},
            [](const BinaryExpr&) {},
            [](const CastExpr&) {},
            [](const CompositeExpr&) {},
            [](const UnionExpr&) {},
            [](const ClosureExpr&) {},
            [](const VectorGetExpr&) {},
            [](const DerefExpr&) {},
            [](const DiscriminantExpr&) {},
            [](const FieldAccessExpr&) {},
            [](const StringLiteral&) {},
            [](const NullLiteral&) {},
            [](const MachineBoolLiteral&) {},
            [](const MachineIntLiteral&) {},
            [](const MachineFloatLiteral&) {},
            [](const ReferenceExpr&) {},
        },
        expr.data);
  };
  // What every node of `steps`, and of the blocks nested in it, does itself.
  const auto all_own = [&](const auto& self, const Block& steps) -> void {
    for (const ExprId node : steps.exprs.Ids()) {
      own(steps, node);
    }
    for (const Block& child : steps.child_scopes) {
      self(self, child);
    }
  };
  own(block, id);
  const ExprData& data = block.exprs.Get(id).data;
  ForEachOperand(data, [&](ExprId operand, OperandUse) {
    Add(effects, EffectsOf(block, operand));
  });
  if (const auto* steps = std::get_if<BlockExpr>(&data)) {
    all_own(all_own, block.child_scopes.Get(steps->scope));
  }
  return effects_.emplace(key, std::move(effects)).first->second;
}

auto EvaluationOrder::OperandsNeedingTemporaries(
    const Block& block, const ExprData& node, std::span<const ExprId> operands,
    const OperandEffects& after) -> std::vector<OperandOrder> {
  std::vector<OperandOrder> orders(operands.size());
  // What runs after the operand at hand, gathered from the last operand back.
  OperandEffects later = after;
  for (std::size_t at = operands.size(); at-- > 0;) {
    const ExprId operand = operands[at];
    if (!IsUnsequenced(node, operand)) {
      continue;
    }
    const OperandEffects& effects = EffectsOf(block, operand);
    // Either way round: what runs after it could change what it evaluates to,
    // or what it does could change what a later one evaluates to.
    const bool changed_by_later =
        (later.side_effects && effects.reads_other_storage) ||
        Intersect(effects.locals_read, later.locals_mutated);
    const bool changes_later =
        effects.side_effects &&
        (later.reads_other_storage ||
         Intersect(later.locals_read, effects.locals_mutated));
    orders[at] = OperandOrder{
        .needs_temporary = changed_by_later || changes_later, .after = later};
    Add(later, effects);
  }
  return orders;
}

auto IsUnsequenced(const ExprData& node, ExprId operand) -> bool {
  switch (WhereOperandIsMade(node, operand)) {
    case OperandMadeIn::kTheOperationsValue:
      return false;
    case OperandMadeIn::kWhereTaken:
    case OperandMadeIn::kTheOperationsAllocation:
    case OperandMadeIn::kTheRuntime:
      return true;
  }
  throw InternalError("mir: an operand is made in no known place");
}

}  // namespace lyra::mir
