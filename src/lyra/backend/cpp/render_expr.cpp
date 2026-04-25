#include "render_expr.hpp"

#include <format>
#include <string>
#include <string_view>
#include <variant>

#include "lyra/mir/binary_op.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/support/internal_error.hpp"
#include "lyra/support/overloaded.hpp"
#include "render_context.hpp"

namespace lyra::backend::cpp {

namespace {

auto BinaryOpToken(mir::BinaryOp op) -> std::string_view {
  switch (op) {
    case mir::BinaryOp::kAdd:
      return " + ";
  }
  throw support::InternalError("RenderExpr: unsupported MIR BinaryOp");
}

}  // namespace

auto RenderLvalue(const RenderContext& ctx, const mir::Lvalue& target)
    -> std::string {
  return std::visit(
      support::Overloaded{
          [&](const mir::MemberVarRef& m) -> std::string {
            return ctx.Class().GetMemberVar(m.target).name;
          },
          [&](const mir::LocalVarRef& l) -> std::string {
            return ctx.Body().local_vars.at(l.target.value).name;
          },
      },
      target);
}

auto RenderExpr(const RenderContext& ctx, const mir::Expr& expr)
    -> std::string {
  return std::visit(
      support::Overloaded{
          [](const mir::IntegerLiteral& e) -> std::string {
            return std::format("{}", e.value);
          },
          [&](const mir::MemberVarRef& e) -> std::string {
            return ctx.Class().GetMemberVar(e.target).name;
          },
          [&](const mir::LocalVarRef& e) -> std::string {
            return ctx.Body().local_vars.at(e.target.value).name;
          },
          [&](const mir::BinaryExpr& e) -> std::string {
            const auto& lhs = ctx.Body().exprs.at(e.lhs.value);
            const auto& rhs = ctx.Body().exprs.at(e.rhs.value);
            return "(" + RenderExpr(ctx, lhs) +
                   std::string{BinaryOpToken(e.op)} + RenderExpr(ctx, rhs) +
                   ")";
          },
          [&](const mir::AssignExpr& e) -> std::string {
            const auto& rhs = ctx.Body().exprs.at(e.value.value);
            return "(" + RenderLvalue(ctx, e.target) + " = " +
                   RenderExpr(ctx, rhs) + ")";
          },
      },
      expr.data);
}

}  // namespace lyra::backend::cpp
