#include "render_stmt.hpp"

#include <cstddef>
#include <string>
#include <variant>

#include "formatting.hpp"
#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/mir/class_decl.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/mir/type.hpp"
#include "render_context.hpp"
#include "render_expr.hpp"
#include "render_type.hpp"

namespace lyra::backend::cpp {

namespace {

auto RenderForInit(const RenderContext& ctx, const mir::ForInit& init)
    -> std::string {
  return std::visit(
      Overloaded{
          [&](const mir::ForInitDecl& d) -> std::string {
            const auto& lv = ctx.Body()
                                 .local_scopes.at(d.local.scope.value)
                                 .locals.at(d.local.local.value);
            std::string out =
                RenderTypeAsCpp(ctx.Unit(), ctx.Class(), lv.type) + " " +
                lv.name;
            if (d.init.has_value()) {
              const auto& v = ctx.Body().exprs.at(d.init->value);
              out += " = " + RenderExpr(ctx, v);
            }
            return out;
          },
          [&](const mir::ForInitExpr& e) -> std::string {
            const auto& expr = ctx.Body().exprs.at(e.expr.value);
            return RenderExpr(ctx, expr);
          },
      },
      init);
}

}  // namespace

auto RenderStmt(
    const RenderContext& ctx, const mir::Stmt& stmt, std::size_t indent)
    -> std::string {
  std::string out;
  if (stmt.label.has_value()) {
    out += Indent(indent) + *stmt.label + ":\n";
  }
  out += std::visit(
      Overloaded{
          [&](const mir::LocalVarDeclStmt& s) -> std::string {
            const auto& lv = ctx.Body()
                                 .local_scopes.at(s.target.scope.value)
                                 .locals.at(s.target.local.value);
            return Indent(indent) +
                   RenderTypeAsCpp(ctx.Unit(), ctx.Class(), lv.type) + " " +
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
            result +=
                RenderBody(ctx.Unit(), ctx.Class(), then_body, indent + 1);
            result += Indent(indent) + "}";
            if (s.else_body.has_value()) {
              const auto& else_body = stmt.child_bodies.at(s.else_body->value);
              result += " else {\n";
              result +=
                  RenderBody(ctx.Unit(), ctx.Class(), else_body, indent + 1);
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
              result +=
                  RenderBody(ctx.Unit(), ctx.Class(), case_body, indent + 2);
              result += Indent(indent + 2) + "break;\n";
              result += Indent(indent + 1) + "}\n";
            }
            if (s.default_body.has_value()) {
              const auto& default_body =
                  stmt.child_bodies.at(s.default_body->value);
              result += Indent(indent + 1) + "default: {\n";
              result +=
                  RenderBody(ctx.Unit(), ctx.Class(), default_body, indent + 2);
              result += Indent(indent + 2) + "break;\n";
              result += Indent(indent + 1) + "}\n";
            }
            result += Indent(indent) + "}\n";
            return result;
          },
          [&](const mir::ConstructOwnedObjectStmt& s) -> std::string {
            const auto& member = ctx.Class().GetMemberVar(s.target);
            const auto& target_class = ctx.Class().GetClass(s.class_id);
            const std::string make =
                "std::make_unique<" + target_class.Name() + ">()";
            if (mir::IsVectorOfOwningObjectType(ctx.Unit(), member.type)) {
              return Indent(indent) + member.name + ".push_back(" + make +
                     ");\n";
            }
            if (mir::IsOwningObjectType(ctx.Unit(), member.type)) {
              return Indent(indent) + member.name + " = " + make + ";\n";
            }
            throw InternalError(
                "ConstructOwnedObjectStmt target is not an owning object "
                "member");
          },
          [&](const mir::ForStmt& s) -> std::string {
            std::string init;
            for (std::size_t i = 0; i < s.init.size(); ++i) {
              if (i != 0) {
                init += ", ";
              }
              init += RenderForInit(ctx, s.init[i]);
            }
            std::string cond;
            if (s.condition.has_value()) {
              const auto& cond_expr = ctx.Body().exprs.at(s.condition->value);
              cond = RenderExpr(ctx, cond_expr);
            }
            std::string step;
            for (std::size_t i = 0; i < s.step.size(); ++i) {
              if (i != 0) {
                step += ", ";
              }
              const auto& step_expr = ctx.Body().exprs.at(s.step[i].value);
              step += RenderExpr(ctx, step_expr);
            }
            const auto& body = stmt.child_bodies.at(s.body.value);
            std::string result = Indent(indent) + "for (" + init + "; " + cond +
                                 "; " + step + ") {\n";
            result += RenderBody(ctx.Unit(), ctx.Class(), body, indent + 1);
            result += Indent(indent) + "}\n";
            return result;
          },
      },
      stmt.data);
  return out;
}

auto RenderBody(
    const mir::CompilationUnit& unit, const mir::ClassDecl& class_decl,
    const mir::Body& body, std::size_t indent) -> std::string {
  const RenderContext ctx{unit, class_decl, body};
  std::string out;
  for (const auto& sid : body.root_stmts) {
    out += RenderStmt(ctx, body.stmts.at(sid.value), indent);
  }
  return out;
}

}  // namespace lyra::backend::cpp
