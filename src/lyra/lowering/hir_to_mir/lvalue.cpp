#include "lyra/lowering/hir_to_mir/lvalue.hpp"

#include <type_traits>
#include <variant>

#include "lyra/common/internal_error.hpp"
#include "lyra/common/type.hpp"
#include "lyra/hir/expression.hpp"
#include "lyra/hir/fwd.hpp"
#include "lyra/lowering/hir_to_mir/builder.hpp"
#include "lyra/lowering/hir_to_mir/context.hpp"
#include "lyra/lowering/hir_to_mir/expression.hpp"
#include "lyra/mir/handle.hpp"
#include "lyra/mir/place.hpp"
#include "lyra/mir/place_type.hpp"

namespace lyra::lowering::hir_to_mir {

auto LowerLvalue(hir::ExpressionId expr_id, MirBuilder& builder)
    -> mir::PlaceId {
  Context& ctx = builder.GetContext();
  const hir::Expression& expr = (*ctx.hir_arena)[expr_id];

  return std::visit(
      [&](const auto& data) -> mir::PlaceId {
        using T = std::decay_t<decltype(data)>;
        if constexpr (std::is_same_v<T, hir::NameRefExpressionData>) {
          return ctx.LookupPlace(data.symbol);
        } else if constexpr (std::is_same_v<
                                 T, hir::ElementAccessExpressionData>) {
          mir::PlaceId base_place = LowerLvalue(data.base, builder);
          mir::Operand index_operand = LowerExpression(data.index, builder);

          const mir::Place& base = (*ctx.mir_arena)[base_place];
          TypeId base_type_id = mir::TypeOfPlace(*ctx.type_arena, base);
          const Type& base_type = (*ctx.type_arena)[base_type_id];
          if (base_type.Kind() != TypeKind::kUnpackedArray) {
            throw common::InternalError(
                "LowerLvalue", "ElementAccess base is not an unpacked array");
          }
          if (index_operand.kind != mir::Operand::Kind::kConst &&
              index_operand.kind != mir::Operand::Kind::kUse) {
            throw common::InternalError(
                "LowerLvalue",
                "ElementAccess index operand must be Const or Use");
          }

          mir::Projection proj{
              .kind = mir::Projection::Kind::kIndex,
              .operand = index_operand,
          };
          return ctx.mir_arena->DerivePlace(base_place, std::move(proj));
        } else {
          throw common::InternalError(
              "LowerLvalue", "unsupported lvalue expression");
        }
      },
      expr.data);
}

}  // namespace lyra::lowering::hir_to_mir
