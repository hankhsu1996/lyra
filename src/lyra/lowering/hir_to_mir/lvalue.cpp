#include "lyra/lowering/hir_to_mir/lvalue.hpp"

#include <type_traits>
#include <variant>

#include "lyra/common/internal_error.hpp"
#include "lyra/hir/expression.hpp"
#include "lyra/hir/fwd.hpp"
#include "lyra/lowering/hir_to_mir/builder.hpp"
#include "lyra/lowering/hir_to_mir/context.hpp"
#include "lyra/mir/handle.hpp"

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
        } else {
          throw common::InternalError(
              "LowerLvalue", "unsupported lvalue expression");
        }
      },
      expr.data);
}

}  // namespace lyra::lowering::hir_to_mir
