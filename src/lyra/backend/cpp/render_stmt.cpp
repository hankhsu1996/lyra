#include "render_stmt.hpp"

#include <cstddef>
#include <string>
#include <variant>

#include "formatting.hpp"
#include "lyra/mir/class_decl.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/support/overloaded.hpp"
#include "render_context.hpp"
#include "render_expr.hpp"
#include "render_type.hpp"

namespace lyra::backend::cpp {

auto RenderStmt(
    const RenderContext& ctx, const mir::Stmt& stmt, std::size_t indent)
    -> std::string {
  std::string out;
  if (stmt.label.has_value()) {
    out += Indent(indent) + *stmt.label + ":\n";
  }
  out += std::visit(
      support::Overloaded{
          [&](const mir::LocalVarDeclStmt& s) -> std::string {
            const auto& lv = ctx.Body().local_vars.at(s.local_var.value);
            return Indent(indent) +
                   RenderTypeAsCpp(ctx.Class().GetType(lv.type)) + " " +
                   lv.name + "{};\n";
          },
          [&](const mir::ExprStmt& s) -> std::string {
            const auto& expr = ctx.Body().exprs.at(s.expr.value);
            return Indent(indent) + RenderExpr(ctx, expr) + ";\n";
          },
          [&](const mir::BlockStmt& s) -> std::string {
            std::string result = Indent(indent) + "{\n";
            for (const auto child_id : s.statements) {
              result += RenderStmt(
                  ctx, ctx.Body().stmts.at(child_id.value), indent + 1);
            }
            result += Indent(indent) + "}\n";
            return result;
          },
          [&](const mir::IfStmt& s) -> std::string {
            const auto& cond_expr = ctx.Body().exprs.at(s.condition.value);
            const auto& then_body = stmt.child_bodies.at(s.then_body.value);
            std::string result;
            result +=
                Indent(indent) + "if (" + RenderExpr(ctx, cond_expr) + ") {\n";
            result += RenderBody(ctx.Class(), then_body, indent + 1);
            result += Indent(indent) + "}";
            if (s.else_body.has_value()) {
              const auto& else_body = stmt.child_bodies.at(s.else_body->value);
              result += " else {\n";
              result += RenderBody(ctx.Class(), else_body, indent + 1);
              result += Indent(indent) + "}";
            }
            result += "\n";
            return result;
          },
          [&](const mir::SwitchStmt& s) -> std::string {
            const auto& cond_expr = ctx.Body().exprs.at(s.condition.value);
            std::string result;
            result += Indent(indent) + "switch (" + RenderExpr(ctx, cond_expr) +
                      ") {\n";
            for (const auto& c : s.cases) {
              for (std::size_t i = 0; i < c.labels.size(); ++i) {
                const auto& label_expr = ctx.Body().exprs.at(c.labels[i].value);
                const bool is_last = (i + 1 == c.labels.size());
                result += Indent(indent + 1) + "case " +
                          RenderExpr(ctx, label_expr) +
                          (is_last ? ": {\n" : ":\n");
              }
              const auto& case_body = stmt.child_bodies.at(c.body.value);
              result += RenderBody(ctx.Class(), case_body, indent + 2);
              result += Indent(indent + 2) + "break;\n";
              result += Indent(indent + 1) + "}\n";
            }
            if (s.default_body.has_value()) {
              const auto& default_body =
                  stmt.child_bodies.at(s.default_body->value);
              result += Indent(indent + 1) + "default: {\n";
              result += RenderBody(ctx.Class(), default_body, indent + 2);
              result += Indent(indent + 2) + "break;\n";
              result += Indent(indent + 1) + "}\n";
            }
            result += Indent(indent) + "}\n";
            return result;
          },
      },
      stmt.data);
  return out;
}

auto RenderBody(
    const mir::ClassDecl& class_decl, const mir::Body& body, std::size_t indent)
    -> std::string {
  const RenderContext ctx{class_decl, body};
  std::string out;
  for (const auto& sid : body.root_stmts) {
    out += RenderStmt(ctx, body.stmts.at(sid.value), indent);
  }
  return out;
}

}  // namespace lyra::backend::cpp
