#include "lyra/mir/storage_part.hpp"

#include <algorithm>
#include <optional>
#include <variant>

#include "lyra/base/internal_error.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/expr_id.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/mir/type.hpp"
#include "lyra/support/builtin_fn.hpp"

namespace lyra::mir {

auto SelectionOf(const CallExpr& call)
    -> std::optional<support::PartSelection> {
  const std::optional<support::BuiltinFn> fn = DirectBuiltinFn(call);
  if (!fn.has_value()) {
    return std::nullopt;
  }
  return support::RuntimeEntryOf(*fn).selects;
}

auto SelectedFrom(const Block& block, ExprId target) -> std::optional<ExprId> {
  const auto* part = std::get_if<CallExpr>(&block.exprs.Get(target).data);
  if (part == nullptr || !SelectionOf(*part).has_value()) {
    return std::nullopt;
  }
  return CalleeReceiver(part->callee);
}

auto AnswersReferenceIntoReceiver(const CallExpr& call) -> bool {
  const std::optional<support::BuiltinFn> fn = DirectBuiltinFn(call);
  if (!fn.has_value()) {
    return false;
  }
  switch (support::RuntimeEntryOf(*fn).answer) {
    case support::EntryAnswer::kReadOnlyReferenceIntoTheReceiver:
    case support::EntryAnswer::kMutableReferenceIntoTheReceiver:
      return true;
    case support::EntryAnswer::kNewValue:
    case support::EntryAnswer::kReferenceToContents:
    case support::EntryAnswer::kReferenceToSampledContents:
    case support::EntryAnswer::kExecution:
      return false;
  }
  throw InternalError("mir: unknown entry answer");
}

auto AnswersMutableReferenceIntoReceiver(const CallExpr& call) -> bool {
  const std::optional<support::BuiltinFn> fn = DirectBuiltinFn(call);
  if (!fn.has_value()) {
    return false;
  }
  switch (support::RuntimeEntryOf(*fn).answer) {
    case support::EntryAnswer::kMutableReferenceIntoTheReceiver:
      return true;
    case support::EntryAnswer::kReadOnlyReferenceIntoTheReceiver:
    case support::EntryAnswer::kNewValue:
    case support::EntryAnswer::kReferenceToContents:
    case support::EntryAnswer::kReferenceToSampledContents:
    case support::EntryAnswer::kExecution:
      return false;
  }
  throw InternalError("mir: unknown entry answer");
}

auto BindsReference(const TypePool& types, const CallExpr& call, TypeId type)
    -> bool {
  return std::holds_alternative<Construct>(call.callee) &&
         !call.arguments.empty() && types.Get(type).Is<VariableRefType>();
}

namespace {

auto Wider(PlaceKind a, PlaceKind b) -> PlaceKind {
  return std::max(a, b);
}

// The kind of a part of a value of `aggregate`: a union's member is overlaid,
// a tagged union's has no address of its own, and any other part is ordinary
// (LRM 7.3).
auto KindOfAPart(const CompilationUnit& unit, TypeId aggregate) -> PlaceKind {
  const std::optional<RecordKind> kind = RecordKindOf(unit, aggregate);
  if (!kind.has_value()) {
    return PlaceKind::kOrdinary;
  }
  switch (*kind) {
    case RecordKind::kStruct:
      return PlaceKind::kOrdinary;
    case RecordKind::kUnion:
      return PlaceKind::kOverlaid;
    case RecordKind::kTaggedUnion:
      return PlaceKind::kNoAddress;
  }
  throw InternalError("mir: unknown record kind");
}

// The kind of the part a call answering a place designated within a write
// designates: the kind of the part designated before it, widened by the kind
// of a part of what that designates, as a field step widens its receiver's.
// The whole a write was opened on is ordinary.
auto KindDesignated(
    const CompilationUnit& unit, const Block& block, ExprId designation)
    -> PlaceKind {
  const std::optional<ExprId> within = SelectedFrom(block, designation);
  if (!within.has_value()) {
    return PlaceKind::kOrdinary;
  }
  const auto* designated =
      unit.types.Get(block.exprs.Get(*within).type).As<DesignationType>();
  if (designated == nullptr) {
    throw InternalError(
        "mir: a step within a write is taken on no designation -- please "
        "report this as a bug");
  }
  return Wider(
      KindDesignated(unit, block, *within),
      KindOfAPart(unit, designated->value));
}

// What `deref` opens part of: the receiver of the call it dereferences, where
// that call answers a reference into its receiver, and nothing otherwise.
auto WholeAnsweredInto(const Block& block, const DerefExpr& deref)
    -> std::optional<ExprId> {
  const auto* call =
      std::get_if<CallExpr>(&block.exprs.Get(deref.pointer).data);
  if (call == nullptr || !AnswersReferenceIntoReceiver(*call)) {
    return std::nullopt;
  }
  const std::optional<ExprId> whole = CalleeReceiver(call->callee);
  if (!whole.has_value()) {
    throw InternalError(
        "mir: a reference into a receiver is taken on one, and this call "
        "names none -- please report this as a bug");
  }
  return whole;
}

// The node whose value `id` is part of; any other node is its own.
auto CompleteObjectOf(const Block& block, ExprId id) -> ExprId {
  const ExprData& data = block.exprs.Get(id).data;
  if (const auto* field = std::get_if<FieldAccessExpr>(&data)) {
    return CompleteObjectOf(block, field->receiver);
  }
  if (const auto* deref = std::get_if<DerefExpr>(&data)) {
    if (const std::optional<ExprId> whole = WholeAnsweredInto(block, *deref)) {
      return CompleteObjectOf(block, *whole);
    }
  }
  return id;
}

}  // namespace

auto KindOfPlace(const CompilationUnit& unit, const Block& block, ExprId place)
    -> PlaceKind {
  const ExprData& data = block.exprs.Get(place).data;
  if (const auto* field = std::get_if<FieldAccessExpr>(&data)) {
    const PlaceKind receiver = KindOfPlace(unit, block, field->receiver);
    return Wider(
        receiver, KindOfAPart(unit, block.exprs.Get(field->receiver).type));
  }
  if (const auto* deref = std::get_if<DerefExpr>(&data)) {
    const mir::Type& pointer =
        unit.types.Get(block.exprs.Get(deref->pointer).type);
    if (pointer.Is<DesignationType>()) {
      return KindDesignated(unit, block, deref->pointer);
    }
    const std::optional<ExprId> whole = WholeAnsweredInto(block, *deref);
    return whole.has_value() ? KindOfPlace(unit, block, *whole)
                             : PlaceKind::kOrdinary;
  }
  return PlaceKind::kOrdinary;
}

auto ReadIfOverlaid(const CompilationUnit& unit, Block& block, ExprId object)
    -> ExprId {
  if (!IsPlace(block.exprs.Get(object).data) ||
      KindOfPlace(unit, block, object) != PlaceKind::kOverlaid) {
    return object;
  }
  return block.Read(object);
}

auto Borrow(const CompilationUnit& unit, Block& block, ExprId operand)
    -> ExprId {
  ExprId object = operand;
  if (const auto* read = std::get_if<CopyExpr>(&block.exprs.Get(operand).data);
      read != nullptr &&
      KindOfPlace(unit, block, read->place) != PlaceKind::kOverlaid) {
    object = read->place;
  }
  object = ReadIfOverlaid(unit, block, object);
  return block.exprs.Add(
      Expr{
          .data = BorrowExpr{.operand = object},
          .type = unit.types.Intern(
              Type{ReferenceType{
                  .referent = block.exprs.Get(object).type,
                  .mutability = Mutability::kReadOnly}})});
}

auto ValueLiesInStorage(
    const CompilationUnit& unit, const Block& block, ExprId id) -> bool {
  const ExprData& data = block.exprs.Get(id).data;
  if (const auto* read = std::get_if<CopyExpr>(&data)) {
    return KindOfPlace(unit, block, read->place) != PlaceKind::kOverlaid;
  }
  return IsPlace(data) || std::holds_alternative<AwaitExpr>(data);
}

auto WholeLiesInStorage(
    const CompilationUnit& unit, const Block& block, ExprId id) -> bool {
  return ValueLiesInStorage(unit, block, CompleteObjectOf(block, id));
}

}  // namespace lyra::mir
