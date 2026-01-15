#include "lyra/lowering/hir_to_mir/expression.hpp"

#include <variant>

#include "lyra/common/internal_error.hpp"
#include "lyra/hir/expression.hpp"
#include "lyra/lowering/hir_to_mir/builder.hpp"
#include "lyra/mir/rvalue.hpp"

namespace lyra::lowering::hir_to_mir {

namespace {

auto LowerConstant(const hir::ConstantExpressionData& data, MirBuilder& builder)
    -> mir::Operand {
  const Constant& constant = builder.GetContext().constant_arena[data.constant];
  return mir::Operand::Const(constant);
}

auto LowerSymbolRef(
    const hir::SymbolRefExpressionData& data, MirBuilder& builder)
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

auto LowerDisplayCall(
    const hir::DisplaySystemCallData& data, const hir::Expression& expr,
    MirBuilder& builder) -> mir::Operand {
  std::vector<mir::Operand> operands;
  operands.reserve(data.args.size());
  for (hir::ExpressionId arg_id : data.args) {
    operands.push_back(LowerExpression(arg_id, builder));
  }

  mir::Rvalue rvalue{
      .kind = mir::RvalueKind::kCall,
      .op = static_cast<int>(data.radix),
      .operands = std::move(operands),
  };

  mir::PlaceId temp_id = builder.EmitTemp(expr.type, std::move(rvalue));
  return mir::Operand::Use(temp_id);
}

auto LowerSystemCall(
    const hir::SystemCallExpressionData& data, const hir::Expression& expr,
    MirBuilder& builder) -> mir::Operand {
  return std::visit(
      [&](const auto& call_data) -> mir::Operand {
        using T = std::decay_t<decltype(call_data)>;
        if constexpr (std::is_same_v<T, hir::DisplaySystemCallData>) {
          return LowerDisplayCall(call_data, expr, builder);
        } else {
          throw common::InternalError(
              "LowerSystemCall", "unhandled system call kind");
        }
      },
      data);
}

}  // namespace

auto LowerExpression(hir::ExpressionId expr_id, MirBuilder& builder)
    -> mir::Operand {
  const hir::Expression& expr = builder.GetContext().hir_arena[expr_id];

  return std::visit(
      [&](const auto& data) -> mir::Operand {
        using T = std::decay_t<decltype(data)>;
        if constexpr (std::is_same_v<T, hir::ConstantExpressionData>) {
          return LowerConstant(data, builder);
        } else if constexpr (std::is_same_v<T, hir::SymbolRefExpressionData>) {
          return LowerSymbolRef(data, builder);
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
