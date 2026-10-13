#include "lyra/mir/storage_part.hpp"

#include <optional>
#include <variant>

#include "lyra/base/internal_error.hpp"
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

}  // namespace lyra::mir
