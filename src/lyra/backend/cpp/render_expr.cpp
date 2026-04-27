#include "render_expr.hpp"

#include <format>
#include <string>
#include <string_view>
#include <variant>

#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/mir/binary_op.hpp"
#include "lyra/mir/expr.hpp"
#include "render_context.hpp"
#include "string_literal.hpp"

namespace lyra::backend::cpp {

namespace {

auto BinaryOpToken(mir::BinaryOp op) -> std::string_view {
  switch (op) {
    case mir::BinaryOp::kAdd:
      return " + ";
  }
  throw InternalError("RenderExpr: unsupported MIR BinaryOp");
}

}  // namespace

auto RenderLvalue(const RenderContext& ctx, const mir::Lvalue& target)
    -> std::string {
  return std::visit(
      Overloaded{
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
      Overloaded{
          [](const mir::IntegerLiteral& e) -> std::string {
            return std::format("{}", e.value);
          },
          [](const mir::StringLiteral& e) -> std::string {
            return RenderStdStringLiteral(e.value);
          },
          [&](const mir::MemberVarRef& e) -> std::string {
            return ctx.Class().GetMemberVar(e.target).name;
          },
          [&](const mir::LocalVarRef& e) -> std::string {
            return ctx.Body().local_vars.at(e.target.value).name;
          },
          [&](const mir::BinaryExpr& e) -> std::string {
            return "(" + RenderExpr(ctx, ctx.Expr(e.lhs)) +
                   std::string{BinaryOpToken(e.op)} +
                   RenderExpr(ctx, ctx.Expr(e.rhs)) + ")";
          },
          [&](const mir::AssignExpr& e) -> std::string {
            return "(" + RenderLvalue(ctx, e.target) + " = " +
                   RenderExpr(ctx, ctx.Expr(e.value)) + ")";
          },
          [](const mir::CallExpr&) -> std::string {
            throw InternalError(
                "RenderExpr: mir::CallExpr lowering to C++ is not implemented");
          },
      },
      expr.data);
}

}  // namespace lyra::backend::cpp
