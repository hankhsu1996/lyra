#include "lyra/lowering/hir_to_mir/expression.hpp"

#include <variant>

#include "lyra/common/internal_error.hpp"
#include "lyra/hir/expression.hpp"
#include "lyra/lowering/hir_to_mir/builder.hpp"
#include "lyra/mir/rvalue.hpp"

namespace lyra::lowering::hir_to_mir {

// Lowering Policy: Temp Materialization
//
// Leaf expressions (constants, name refs) produce Operands directly.
// Non-leaf expressions (unary, binary, calls) always materialize their
// result into a temp via EmitTemp, then return Use(temp).
//
// This avoids value identity issues (MIR has no SSA) and keeps the lowering
// uniform. Future expression kinds (casts, selects, bit-slices) should
// follow the same pattern.

namespace {

auto LowerConstant(const hir::ConstantExpressionData& data, MirBuilder& builder)
    -> mir::Operand {
  const Constant& constant =
      (*builder.GetContext().constant_arena)[data.constant];
  return mir::Operand::Const(constant);
}

auto LowerNameRef(const hir::NameRefExpressionData& data, MirBuilder& builder)
    -> mir::Operand {
  mir::PlaceId place_id = builder.GetContext().LookupPlace(data.symbol);
  return mir::Operand::Use(place_id);
}

auto LowerUnary(
    const hir::UnaryExpressionData& data, const hir::Expression& expr,
    MirBuilder& builder) -> mir::Operand {
  mir::Operand operand = LowerExpression(data.operand, builder);

  mir::Rvalue rvalue{
      .kind = mir::RvalueKind::kUnary,
      .op = static_cast<int>(data.op),
      .operands = {operand},
  };

  mir::PlaceId temp_id = builder.EmitTemp(expr.type, std::move(rvalue));
  return mir::Operand::Use(temp_id);
}

auto LowerBinary(
    const hir::BinaryExpressionData& data, const hir::Expression& expr,
    MirBuilder& builder) -> mir::Operand {
  mir::Operand lhs = LowerExpression(data.lhs, builder);
  mir::Operand rhs = LowerExpression(data.rhs, builder);

  mir::Rvalue rvalue{
      .kind = mir::RvalueKind::kBinary,
      .op = static_cast<int>(data.op),
      .operands = {lhs, rhs},
  };

  mir::PlaceId temp_id = builder.EmitTemp(expr.type, std::move(rvalue));
  return mir::Operand::Use(temp_id);
}

auto LowerSystemCall(
    const hir::SystemCallExpressionData& /*data*/,
    const hir::Expression& /*expr*/, MirBuilder& /*builder*/) -> mir::Operand {
  // Effect system calls ($display, etc.) are handled in statement.cpp
  // as Effect instructions. If we get here, it's because a system call
  // was used in an expression context where a value is expected.
  // Currently all supported system calls are effects, so this is an error.
  throw common::InternalError(
      "LowerSystemCall",
      "system call used in value context (only effect calls supported)");
}

}  // namespace

auto LowerExpression(hir::ExpressionId expr_id, MirBuilder& builder)
    -> mir::Operand {
  const hir::Expression& expr = (*builder.GetContext().hir_arena)[expr_id];

  return std::visit(
      [&](const auto& data) -> mir::Operand {
        using T = std::decay_t<decltype(data)>;
        if constexpr (std::is_same_v<T, hir::ConstantExpressionData>) {
          return LowerConstant(data, builder);
        } else if constexpr (std::is_same_v<T, hir::NameRefExpressionData>) {
          return LowerNameRef(data, builder);
        } else if constexpr (std::is_same_v<T, hir::UnaryExpressionData>) {
          return LowerUnary(data, expr, builder);
        } else if constexpr (std::is_same_v<T, hir::BinaryExpressionData>) {
          return LowerBinary(data, expr, builder);
        } else if constexpr (std::is_same_v<T, hir::SystemCallExpressionData>) {
          return LowerSystemCall(data, expr, builder);
        } else {
          throw common::InternalError(
              "LowerExpression", "unhandled expression kind");
        }
      },
      expr.data);
}

}  // namespace lyra::lowering::hir_to_mir
