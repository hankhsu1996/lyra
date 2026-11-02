#include "lyra/mir/object_operand.hpp"

#include <optional>
#include <variant>

#include "lyra/mir/expr.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/mir/type.hpp"
#include "lyra/mir/type_id.hpp"
#include "lyra/support/builtin_fn.hpp"

namespace lyra::mir {

namespace {

// What `call`, answering a value of `result`, acts on, where it acts on an
// object. That operand is whatever reaches the object -- a class handle, the
// running method's own object, or a write in progress into it.
auto ObjectOperandOf(const TypePool& types, const CallExpr& call, TypeId result)
    -> std::optional<ObjectOperand> {
  std::optional<support::ObjectView> view;
  if (const std::optional<support::BuiltinFn> entry = DirectBuiltinFn(call)) {
    view = support::RuntimeEntryOf(*entry).acts_on_object_as;
  } else if (
      std::holds_alternative<Construct>(call.callee) &&
      types.Get(result).Is<ObjectWriteType>()) {
    view = support::TheClassReached{};
  }
  if (!view.has_value()) {
    return std::nullopt;
  }
  return ObjectOperand{.operand = call.arguments.front(), .view = *view};
}

}  // namespace

auto HandleReached(const TypePool& types, const Block& block, const Expr& expr)
    -> std::optional<ObjectOperand> {
  std::optional<ObjectOperand> through;
  if (const auto* deref = std::get_if<DerefExpr>(&expr.data)) {
    through = ObjectOperand{
        .operand = deref->pointer, .view = support::TheClassReached{}};
  } else if (const auto* call = std::get_if<CallExpr>(&expr.data)) {
    through = ObjectOperandOf(types, *call, expr.type);
  }
  if (!through.has_value() ||
      !types.Get(block.exprs.Get(through->operand).type).Is<ManagedRefType>()) {
    return std::nullopt;
  }
  return through;
}

}  // namespace lyra::mir
