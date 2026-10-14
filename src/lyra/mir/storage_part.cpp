#include "lyra/mir/storage_part.hpp"

#include <optional>
#include <variant>

#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
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

auto StorageSelection(const TypePool& types, const Block& block, ExprId id)
    -> std::optional<support::PartSelection> {
  const auto* call = std::get_if<CallExpr>(&block.exprs.Get(id).data);
  if (call == nullptr) {
    return std::nullopt;
  }
  const std::optional<support::PartSelection> selects = SelectionOf(*call);
  const std::optional<ExprId> receiver = CalleeReceiver(call->callee);
  if (!selects.has_value() || !receiver.has_value() ||
      !types.Get(block.exprs.Get(*receiver).type).PartsAreStorage()) {
    return std::nullopt;
  }
  return selects;
}

auto StoragePartAccess(const TypePool& types, const Block& block, ExprId id)
    -> const CallExpr* {
  const std::optional<support::PartSelection> selects =
      StorageSelection(types, block, id);
  if (!selects.has_value()) {
    return nullptr;
  }
  switch (*selects) {
    case support::PartSelection::kElement:
    case support::PartSelection::kComponent:
      return &std::get<CallExpr>(block.exprs.Get(id).data);
    // A slice is several elements rather than one storage, and bits lie
    // inside a value.
    case support::PartSelection::kSlice:
    case support::PartSelection::kBits:
      return nullptr;
  }
  throw InternalError("mir: unknown part selection");
}

auto WrapperRead(const Expr& expr) -> std::optional<ExprId> {
  const auto* call = std::get_if<CallExpr>(&expr.data);
  if (call == nullptr || DirectBuiltinFn(*call) != support::BuiltinFn::kLoad) {
    return std::nullopt;
  }
  return CalleeReceiver(call->callee);
}

auto ReadsWhereItLies(const TypePool& types, const Block& block, ExprId id)
    -> bool {
  return StoragePartAccess(types, block, id) != nullptr ||
         WrapperRead(block.exprs.Get(id)).has_value();
}

auto SelectedFrom(const Block& block, ExprId target) -> std::optional<ExprId> {
  const auto* part = std::get_if<CallExpr>(&block.exprs.Get(target).data);
  if (part == nullptr || !SelectionOf(*part).has_value()) {
    return std::nullopt;
  }
  return CalleeReceiver(part->callee);
}

auto IsPlaceExpression(const TypePool& types, const Block& block, ExprId id)
    -> bool {
  const auto place = [](const auto&) { return true; };
  const auto made = [](const auto&) { return false; };
  return std::visit(
      Overloaded{
          [&](const ReferenceExpr& e) { return place(e); },
          [&](const DerefExpr& e) { return place(e); },
          [&](const FieldAccessExpr& e) { return place(e); },
          [&](const AwaitExpr& e) { return place(e); },
          [&](const CallExpr&) { return ReadsWhereItLies(types, block, id); },
          [&](const StringLiteral& e) { return made(e); },
          [&](const NullLiteral& e) { return made(e); },
          [&](const MachineBoolLiteral& e) { return made(e); },
          [&](const MachineIntLiteral& e) { return made(e); },
          [&](const MachineFloatLiteral& e) { return made(e); },
          [&](const UnaryExpr& e) { return made(e); },
          [&](const BinaryExpr& e) { return made(e); },
          [&](const CastExpr& e) { return made(e); },
          [&](const DynamicCastExpr& e) { return made(e); },
          [&](const ConditionalExpr& e) { return made(e); },
          [&](const BlockExpr& e) { return made(e); },
          [&](const AssignExpr& e) { return made(e); },
          [&](const NewExpr& e) { return made(e); },
          [&](const AddressOfExpr& e) { return made(e); },
          [&](const BorrowExpr& e) { return made(e); },
          [&](const MoveExpr& e) { return made(e); },
          [&](const ClosureExpr& e) { return made(e); },
          [&](const CompositeExpr& e) { return made(e); },
          [&](const WaitExpr& e) { return made(e); },
          [&](const VectorGetExpr& e) { return made(e); }},
      block.exprs.Get(id).data);
}

}  // namespace lyra::mir
