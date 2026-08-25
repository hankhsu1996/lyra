#include "lyra/lowering/hir_to_mir/call_operands.hpp"

#include <cstddef>
#include <format>
#include <optional>
#include <vector>

#include "lyra/base/internal_error.hpp"
#include "lyra/hir/expr.hpp"
#include "lyra/hir/expr_id.hpp"

namespace lyra::lowering::hir_to_mir {

namespace {

auto Collect(const hir::CallExpr& call, std::size_t count)
    -> std::vector<hir::ExprId> {
  std::vector<hir::ExprId> operands;
  operands.reserve(count);
  for (std::size_t i = 0; i < count; ++i) {
    const std::optional<hir::ExprId>& argument = call.arguments[i];
    if (!argument.has_value()) {
      throw InternalError(
          std::format(
              "call operands: argument {} was elided, but the lowering names "
              "it part of the subroutine's shape",
              i));
    }
    operands.push_back(*argument);
  }
  return operands;
}

}  // namespace

auto RequiredOperands(const hir::CallExpr& call) -> std::vector<hir::ExprId> {
  return Collect(call, call.arguments.size());
}

auto RequiredOperands(const hir::CallExpr& call, std::size_t count)
    -> std::vector<hir::ExprId> {
  if (call.arguments.size() != count) {
    throw InternalError(
        std::format(
            "call operands: the lowering names {} operand(s), the front end "
            "admitted a call with {}",
            count, call.arguments.size()));
  }
  return Collect(call, count);
}

auto RequiredLeadingOperands(const hir::CallExpr& call, std::size_t count)
    -> std::vector<hir::ExprId> {
  if (call.arguments.size() < count) {
    throw InternalError(
        std::format(
            "call operands: the lowering names {} leading operand(s), the "
            "front end admitted a call with {}",
            count, call.arguments.size()));
  }
  return Collect(call, count);
}

auto OptionalOperand(const hir::CallExpr& call, std::size_t index)
    -> std::optional<hir::ExprId> {
  if (index >= call.arguments.size()) {
    return std::nullopt;
  }
  return call.arguments[index];
}

}  // namespace lyra::lowering::hir_to_mir
