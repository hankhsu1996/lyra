#include "render_expr.hpp"

#include <format>
#include <string>
#include <variant>

#include "lyra/mir/expr.hpp"
#include "lyra/support/overloaded.hpp"
#include "render_context.hpp"

namespace lyra::projection::cpp {

auto RenderExpr(const RenderContext& ctx, const mir::Expr& expr)
    -> std::string {
  return std::visit(
      support::Overloaded{
          [](const mir::IntegerLiteral& e) -> std::string {
            return std::format("{}", e.value);
          },
          [&](const mir::MemberRef& e) -> std::string {
            return ctx.Unit().GetMember(e.target).name;
          },
      },
      expr.data);
}

}  // namespace lyra::projection::cpp
