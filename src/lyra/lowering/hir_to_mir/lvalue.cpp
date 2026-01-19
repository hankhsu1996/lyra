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

namespace {

auto LowerNameRefLvalue(const hir::NameRefExpressionData& data, Context& ctx)
    -> mir::PlaceId {
  return ctx.LookupPlace(data.symbol);
}

auto LowerElementAccessLvalue(
    const hir::ElementAccessExpressionData& data, MirBuilder& builder)
    -> mir::PlaceId {
  Context& ctx = builder.GetContext();

  mir::PlaceId base_place = LowerLvalue(data.base, builder);
  mir::Operand index_operand = LowerExpression(data.index, builder);

  const mir::Place& base = (*ctx.mir_arena)[base_place];
  TypeId base_type_id = mir::TypeOfPlace(*ctx.type_arena, base);
  const Type& base_type = (*ctx.type_arena)[base_type_id];
  if (base_type.Kind() != TypeKind::kUnpackedArray &&
      base_type.Kind() != TypeKind::kDynamicArray &&
      base_type.Kind() != TypeKind::kQueue) {
    throw common::InternalError(
        "LowerElementAccessLvalue", "base is not an array or queue");
  }
  if (index_operand.kind != mir::Operand::Kind::kConst &&
      index_operand.kind != mir::Operand::Kind::kUse) {
    throw common::InternalError(
        "LowerElementAccessLvalue", "index operand must be Const or Use");
  }

  mir::Projection proj{
      .info = mir::IndexProjection{.index = index_operand},
  };
  return ctx.mir_arena->DerivePlace(base_place, std::move(proj));
}

auto LowerMemberAccessLvalue(
    const hir::MemberAccessExpressionData& data, MirBuilder& builder)
    -> mir::PlaceId {
  Context& ctx = builder.GetContext();

  mir::PlaceId base_place = LowerLvalue(data.base, builder);

  const mir::Place& base = (*ctx.mir_arena)[base_place];
  TypeId base_type_id = mir::TypeOfPlace(*ctx.type_arena, base);
  const Type& base_type = (*ctx.type_arena)[base_type_id];
  if (base_type.Kind() != TypeKind::kUnpackedStruct) {
    throw common::InternalError(
        "LowerMemberAccessLvalue", "base is not an unpacked struct");
  }

  mir::Projection proj{
      .info = mir::FieldProjection{.field_index = data.field_index},
  };
  return ctx.mir_arena->DerivePlace(base_place, std::move(proj));
}

auto LowerPackedElementSelectLvalue(
    const hir::PackedElementSelectExpressionData& data,
    const hir::Expression& expr, MirBuilder& builder) -> mir::PlaceId {
  Context& ctx = builder.GetContext();

  mir::PlaceId base_place = LowerLvalue(data.base, builder);
  mir::Operand index_operand = LowerExpression(data.index, builder);

  mir::Projection proj{
      .info =
          mir::BitSliceProjection{
              .index = index_operand,
              .element_width = data.element_width,
              .lower_bound = data.array_lower_bound,
              .upper_bound = data.array_upper_bound,
              .is_descending = data.is_descending,
              .element_type = expr.type,
          },
  };
  return ctx.mir_arena->DerivePlace(base_place, std::move(proj));
}

}  // namespace

auto LowerLvalue(hir::ExpressionId expr_id, MirBuilder& builder)
    -> mir::PlaceId {
  Context& ctx = builder.GetContext();
  const hir::Expression& expr = (*ctx.hir_arena)[expr_id];

  return std::visit(
      [&](const auto& data) -> mir::PlaceId {
        using T = std::decay_t<decltype(data)>;
        if constexpr (std::is_same_v<T, hir::NameRefExpressionData>) {
          return LowerNameRefLvalue(data, ctx);
        } else if constexpr (std::is_same_v<
                                 T, hir::ElementAccessExpressionData>) {
          return LowerElementAccessLvalue(data, builder);
        } else if constexpr (std::is_same_v<
                                 T, hir::MemberAccessExpressionData>) {
          return LowerMemberAccessLvalue(data, builder);
        } else if constexpr (std::is_same_v<
                                 T, hir::PackedElementSelectExpressionData>) {
          return LowerPackedElementSelectLvalue(data, expr, builder);
        } else {
          throw common::InternalError(
              "LowerLvalue", "unsupported lvalue expression");
        }
      },
      expr.data);
}

}  // namespace lyra::lowering::hir_to_mir
