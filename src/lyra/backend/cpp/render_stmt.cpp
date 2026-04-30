#include "lyra/backend/cpp/render_stmt.hpp"

#include <cstddef>
#include <string>
#include <utility>
#include <variant>

#include "lyra/backend/cpp/formatting.hpp"
#include "lyra/backend/cpp/render_context.hpp"
#include "lyra/backend/cpp/render_expr.hpp"
#include "lyra/backend/cpp/render_type.hpp"
#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/diag/diagnostic.hpp"
#include "lyra/mir/class_decl.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/mir/type.hpp"

namespace lyra::backend::cpp {

namespace {

auto RenderForInit(const RenderContext& ctx, const mir::ForInit& init)
    -> diag::Result<std::string> {
  return std::visit(
      Overloaded{
          [&](const mir::ForInitDecl& d) -> diag::Result<std::string> {
            const auto& lv = ctx.BodyAtHops(d.local.body_hops)
                                 .locals.at(d.local.local.value);
            std::string out =
                RenderTypeAsCpp(ctx.Unit(), ctx.Class(), lv.type) + " " +
                lv.name;
            if (d.init.has_value()) {
              const auto& v = ctx.Body().exprs.at(d.init->value);
              auto rendered_or = RenderExpr(ctx, v);
              if (!rendered_or) {
                return std::unexpected(std::move(rendered_or.error()));
              }
              out += " = " + *rendered_or;
            }
            return out;
          },
          [&](const mir::ForInitExpr& e) -> diag::Result<std::string> {
            const auto& expr = ctx.Body().exprs.at(e.expr.value);
            return RenderExpr(ctx, expr);
          },
      },
      init);
}

}  // namespace

auto RenderStmt(
    const RenderContext& ctx, const mir::Stmt& stmt, std::size_t indent)
    -> diag::Result<std::string> {
  std::string out;
  if (stmt.label.has_value()) {
    out += Indent(indent) + *stmt.label + ":\n";
  }
  auto body_or = std::visit(
      Overloaded{
          [&](const mir::EmptyStmt&) -> diag::Result<std::string> {
            return Indent(indent) + ";\n";
          },
          [&](const mir::TimedStmt& s) -> diag::Result<std::string> {
            std::string out;
            std::visit(
                Overloaded{
                    [&](const mir::DelayControl& d) {
                      out += Indent(indent) + "co_await lyra::runtime::Delay(" +
                             std::to_string(d.duration) + ");\n";
                    },
                },
                s.timing);
            auto inner_or =
                RenderStmt(ctx, ctx.Body().stmts.at(s.body.value), indent);
            if (!inner_or) return std::unexpected(std::move(inner_or.error()));
            out += *inner_or;
            return out;
          },
          [&](const mir::LocalVarDeclStmt& s) -> diag::Result<std::string> {
            const auto& lv = ctx.BodyAtHops(s.target.body_hops)
                                 .locals.at(s.target.local.value);
            return Indent(indent) +
                   RenderTypeAsCpp(ctx.Unit(), ctx.Class(), lv.type) + " " +
                   lv.name + "{};\n";
          },
          [&](const mir::ExprStmt& s) -> diag::Result<std::string> {
            const auto& expr = ctx.Body().exprs.at(s.expr.value);
            auto rendered_or = RenderExpr(ctx, expr);
            if (!rendered_or) {
              return std::unexpected(std::move(rendered_or.error()));
            }
            return Indent(indent) + *rendered_or + ";\n";
          },
          [&](const mir::BlockStmt& s) -> diag::Result<std::string> {
            const auto& child = stmt.child_bodies.at(s.body.value);
            std::string result = Indent(indent) + "{\n";
            auto child_or =
                RenderBody(ctx.Unit(), ctx.Class(), child, indent + 1, &ctx);
            if (!child_or) return std::unexpected(std::move(child_or.error()));
            result += *child_or;
            result += Indent(indent) + "}\n";
            return result;
          },
          [&](const mir::IfStmt& s) -> diag::Result<std::string> {
            const auto& cond_expr = ctx.Body().exprs.at(s.condition.value);
            const auto& then_body = stmt.child_bodies.at(s.then_body.value);
            auto cond_or = RenderExpr(ctx, cond_expr);
            if (!cond_or) return std::unexpected(std::move(cond_or.error()));
            auto then_or = RenderBody(
                ctx.Unit(), ctx.Class(), then_body, indent + 1, &ctx);
            if (!then_or) return std::unexpected(std::move(then_or.error()));
            std::string result;
            result += Indent(indent) + "if (" + *cond_or + ") {\n";
            result += *then_or;
            result += Indent(indent) + "}";
            if (s.else_body.has_value()) {
              const auto& else_body = stmt.child_bodies.at(s.else_body->value);
              auto else_or = RenderBody(
                  ctx.Unit(), ctx.Class(), else_body, indent + 1, &ctx);
              if (!else_or) return std::unexpected(std::move(else_or.error()));
              result += " else {\n";
              result += *else_or;
              result += Indent(indent) + "}";
            }
            result += "\n";
            return result;
          },
          [&](const mir::SwitchStmt& s) -> diag::Result<std::string> {
            const auto& cond_expr = ctx.Body().exprs.at(s.condition.value);
            auto cond_or = RenderExpr(ctx, cond_expr);
            if (!cond_or) return std::unexpected(std::move(cond_or.error()));
            std::string result;
            result += Indent(indent) + "switch (" + *cond_or + ") {\n";
            for (const auto& c : s.cases) {
              for (std::size_t i = 0; i < c.labels.size(); ++i) {
                const auto& label_expr = ctx.Body().exprs.at(c.labels[i].value);
                auto label_or = RenderExpr(ctx, label_expr);
                if (!label_or) {
                  return std::unexpected(std::move(label_or.error()));
                }
                const bool is_last = (i + 1 == c.labels.size());
                result += Indent(indent + 1) + "case " + *label_or +
                          (is_last ? ": {\n" : ":\n");
              }
              const auto& case_body = stmt.child_bodies.at(c.body.value);
              auto case_or = RenderBody(
                  ctx.Unit(), ctx.Class(), case_body, indent + 2, &ctx);
              if (!case_or) return std::unexpected(std::move(case_or.error()));
              result += *case_or;
              result += Indent(indent + 2) + "break;\n";
              result += Indent(indent + 1) + "}\n";
            }
            if (s.default_body.has_value()) {
              const auto& default_body =
                  stmt.child_bodies.at(s.default_body->value);
              auto default_or = RenderBody(
                  ctx.Unit(), ctx.Class(), default_body, indent + 2, &ctx);
              if (!default_or) {
                return std::unexpected(std::move(default_or.error()));
              }
              result += Indent(indent + 1) + "default: {\n";
              result += *default_or;
              result += Indent(indent + 2) + "break;\n";
              result += Indent(indent + 1) + "}\n";
            }
            result += Indent(indent) + "}\n";
            return result;
          },
          [&](const mir::ConstructOwnedObjectStmt& s)
              -> diag::Result<std::string> {
            const auto& member = ctx.Class().GetMemberVar(s.target);
            const auto& target_class = ctx.Class().GetClass(s.class_id);
            const std::string make =
                "std::make_unique<" + target_class.name + ">()";
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
          [&](const mir::ForStmt& s) -> diag::Result<std::string> {
            std::string init;
            for (std::size_t i = 0; i < s.init.size(); ++i) {
              if (i != 0) {
                init += ", ";
              }
              auto init_or = RenderForInit(ctx, s.init[i]);
              if (!init_or) {
                return std::unexpected(std::move(init_or.error()));
              }
              init += *init_or;
            }
            std::string cond;
            if (s.condition.has_value()) {
              const auto& cond_expr = ctx.Body().exprs.at(s.condition->value);
              auto cond_or = RenderExprAsNative(ctx, cond_expr);
              if (!cond_or) {
                return std::unexpected(std::move(cond_or.error()));
              }
              cond = *std::move(cond_or);
            }
            std::string step;
            for (std::size_t i = 0; i < s.step.size(); ++i) {
              if (i != 0) {
                step += ", ";
              }
              const auto& step_expr = ctx.Body().exprs.at(s.step[i].value);
              auto step_or = RenderExpr(ctx, step_expr);
              if (!step_or) {
                return std::unexpected(std::move(step_or.error()));
              }
              step += *step_or;
            }
            const auto& body = stmt.child_bodies.at(s.body.value);
            auto body_or =
                RenderBody(ctx.Unit(), ctx.Class(), body, indent + 1, &ctx);
            if (!body_or) return std::unexpected(std::move(body_or.error()));
            std::string result = Indent(indent) + "for (" + init + "; " + cond +
                                 "; " + step + ") {\n";
            result += *body_or;
            result += Indent(indent) + "}\n";
            return result;
          },
          [&](const mir::WhileStmt&) -> diag::Result<std::string> {
            throw InternalError(
                "RenderStmt: WhileStmt is not yet supported by C++ emit");
          },
          [&](const mir::AwaitStmt&) -> diag::Result<std::string> {
            throw InternalError(
                "RenderStmt: AwaitStmt is not yet supported by C++ emit");
          },
      },
      stmt.data);
  if (!body_or) return std::unexpected(std::move(body_or.error()));
  out += *body_or;
  return out;
}

auto RenderBody(
    const mir::CompilationUnit& unit, const mir::ClassDecl& class_decl,
    const mir::Body& body, std::size_t indent, const RenderContext* parent)
    -> diag::Result<std::string> {
  std::string out;
  if (parent == nullptr) {
    const RenderContext ctx{unit, class_decl, body};
    for (const auto& sid : body.root_stmts) {
      auto rendered_or = RenderStmt(ctx, body.stmts.at(sid.value), indent);
      if (!rendered_or) return std::unexpected(std::move(rendered_or.error()));
      out += *rendered_or;
    }
  } else {
    const RenderContext ctx{unit, class_decl, body, *parent};
    for (const auto& sid : body.root_stmts) {
      auto rendered_or = RenderStmt(ctx, body.stmts.at(sid.value), indent);
      if (!rendered_or) return std::unexpected(std::move(rendered_or.error()));
      out += *rendered_or;
    }
  }
  return out;
}

}  // namespace lyra::backend::cpp
