#include "lyra/mir/storage_part.hpp"

#include <algorithm>
#include <optional>
#include <variant>

#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
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

namespace {

auto Wider(PlaceKind a, PlaceKind b) -> PlaceKind {
  return std::max(a, b);
}

}  // namespace

auto KindOfPlace(const CompilationUnit& unit, const Block& block, ExprId place)
    -> PlaceKind {
  const ExprData& data = block.exprs.Get(place).data;
  if (const auto* field = std::get_if<FieldAccessExpr>(&data)) {
    const PlaceKind receiver = KindOfPlace(unit, block, field->receiver);
    const Type& aggregate =
        unit.types.Get(block.exprs.Get(field->receiver).type);
    if (aggregate.Is<UnionType>()) {
      return PlaceKind::kOverlaid;
    }
    if (aggregate.Is<TaggedUnionType>()) {
      return Wider(receiver, PlaceKind::kNoAddress);
    }
    return receiver;
  }
  if (const auto* deref = std::get_if<DerefExpr>(&data)) {
    const auto* call =
        std::get_if<CallExpr>(&block.exprs.Get(deref->pointer).data);
    if (call == nullptr || !AnswersReferenceIntoReceiver(*call)) {
      return PlaceKind::kOrdinary;
    }
    const std::optional<ExprId> whole = CalleeReceiver(call->callee);
    if (!whole.has_value()) {
      throw InternalError(
          "mir: a reference into a receiver is taken on one, and this call "
          "names none -- please report this as a bug");
    }
    return KindOfPlace(unit, block, *whole);
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

auto ValueLiesInStorage(
    const CompilationUnit& unit, const Block& block, ExprId id) -> bool {
  const ExprData& data = block.exprs.Get(id).data;
  if (const auto* read = std::get_if<CopyExpr>(&data)) {
    return KindOfPlace(unit, block, read->place) != PlaceKind::kOverlaid;
  }
  return IsPlace(data) || std::holds_alternative<AwaitExpr>(data);
}

}  // namespace lyra::mir
