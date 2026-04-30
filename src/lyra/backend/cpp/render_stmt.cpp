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
#include "lyra/mir/stmt.hpp"
#include "lyra/mir/structural_scope.hpp"
#include "lyra/mir/type.hpp"

namespace lyra::backend::cpp {

namespace {

auto RenderForInit(const RenderContext& ctx, const mir::ForInit& init)
    -> diag::Result<std::string> {
  return std::visit(
      Overloaded{
          [&](const mir::ForInitDecl& d) -> diag::Result<std::string> {
            const auto& lv = ctx.ProceduralScopeAtHops(d.induction_var.hops)
                                 .vars.at(d.induction_var.var.value);
            auto type_or =
                RenderTypeAsCpp(ctx.Unit(), ctx.StructuralScope(), lv.type);
            if (!type_or) return std::unexpected(std::move(type_or.error()));
            const auto& ty = ctx.Unit().GetType(lv.type);
            const bool is_packed_explicit =
                ty.IsPackedArray() &&
                ty.AsPackedArray().form == mir::PackedArrayForm::kExplicit;
            if (is_packed_explicit && d.init.has_value()) {
              return diag::Unsupported(
                  diag::DiagCode::kCppEmitPackedRuntimeNotSupported,
                  "packed for-init with initializer is not yet supported in "
                  "cpp emit",
                  diag::UnsupportedCategory::kFeature);
            }
            std::string out = *type_or + " " + lv.name;
            if (is_packed_explicit) {
              out += "{" + std::to_string(ty.AsPackedArray().BitWidth()) + "}";
            } else if (d.init.has_value()) {
              const auto& v = ctx.ProceduralScope().exprs.at(d.init->value);
              auto rendered_or = RenderExpr(ctx, v);
              if (!rendered_or) {
                return std::unexpected(std::move(rendered_or.error()));
              }
              out += " = " + *rendered_or;
            }
            return out;
          },
          [&](const mir::ForInitExpr& e) -> diag::Result<std::string> {
            const auto& expr = ctx.ProceduralScope().exprs.at(e.expr.value);
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
  auto rendered_or = std::visit(
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
            auto inner_or = RenderStmt(
                ctx, ctx.ProceduralScope().stmts.at(s.stmt.value), indent);
            if (!inner_or) return std::unexpected(std::move(inner_or.error()));
            out += *inner_or;
            return out;
          },
          [&](const mir::ProceduralVarDeclStmt& s)
              -> diag::Result<std::string> {
            const auto& lv = ctx.ProceduralScopeAtHops(s.target.hops)
                                 .vars.at(s.target.var.value);
            auto type_or =
                RenderTypeAsCpp(ctx.Unit(), ctx.StructuralScope(), lv.type);
            if (!type_or) return std::unexpected(std::move(type_or.error()));
            const auto& ty = ctx.Unit().GetType(lv.type);
            std::string init = "{}";
            if (ty.IsPackedArray() &&
                ty.AsPackedArray().form == mir::PackedArrayForm::kExplicit) {
              init = "{" + std::to_string(ty.AsPackedArray().BitWidth()) + "}";
            }
            return Indent(indent) + *type_or + " " + lv.name + init + ";\n";
          },
          [&](const mir::ExprStmt& s) -> diag::Result<std::string> {
            const auto& expr = ctx.ProceduralScope().exprs.at(s.expr.value);
            auto rendered_or = RenderExpr(ctx, expr);
            if (!rendered_or) {
              return std::unexpected(std::move(rendered_or.error()));
            }
            return Indent(indent) + *rendered_or + ";\n";
          },
          [&](const mir::BlockStmt& s) -> diag::Result<std::string> {
            const auto& child = stmt.child_procedural_scopes.at(s.scope.value);
            std::string result = Indent(indent) + "{\n";
            auto child_or = RenderNestedProceduralScope(ctx, child, indent + 1);
            if (!child_or) return std::unexpected(std::move(child_or.error()));
            result += *child_or;
            result += Indent(indent) + "}\n";
            return result;
          },
          [&](const mir::IfStmt& s) -> diag::Result<std::string> {
            const auto& cond_expr =
                ctx.ProceduralScope().exprs.at(s.condition.value);
            const auto& then_scope =
                stmt.child_procedural_scopes.at(s.then_scope.value);
            auto cond_or = RenderExpr(ctx, cond_expr);
            if (!cond_or) return std::unexpected(std::move(cond_or.error()));
            auto then_or =
                RenderNestedProceduralScope(ctx, then_scope, indent + 1);
            if (!then_or) return std::unexpected(std::move(then_or.error()));
            std::string result;
            result += Indent(indent) + "if (" + *cond_or + ") {\n";
            result += *then_or;
            result += Indent(indent) + "}";
            if (s.else_scope.has_value()) {
              const auto& else_scope =
                  stmt.child_procedural_scopes.at(s.else_scope->value);
              auto else_or =
                  RenderNestedProceduralScope(ctx, else_scope, indent + 1);
              if (!else_or) return std::unexpected(std::move(else_or.error()));
              result += " else {\n";
              result += *else_or;
              result += Indent(indent) + "}";
            }
            result += "\n";
            return result;
          },
          [&](const mir::SwitchStmt& s) -> diag::Result<std::string> {
            const auto& cond_expr =
                ctx.ProceduralScope().exprs.at(s.condition.value);
            auto cond_or = RenderExpr(ctx, cond_expr);
            if (!cond_or) return std::unexpected(std::move(cond_or.error()));
            std::string result;
            result += Indent(indent) + "switch (" + *cond_or + ") {\n";
            for (const auto& c : s.cases) {
              for (std::size_t i = 0; i < c.labels.size(); ++i) {
                const auto& label_expr =
                    ctx.ProceduralScope().exprs.at(c.labels[i].value);
                auto label_or = RenderExpr(ctx, label_expr);
                if (!label_or) {
                  return std::unexpected(std::move(label_or.error()));
                }
                const bool is_last = (i + 1 == c.labels.size());
                result += Indent(indent + 1) + "case " + *label_or +
                          (is_last ? ": {\n" : ":\n");
              }
              const auto& case_scope =
                  stmt.child_procedural_scopes.at(c.scope.value);
              auto case_or =
                  RenderNestedProceduralScope(ctx, case_scope, indent + 2);
              if (!case_or) return std::unexpected(std::move(case_or.error()));
              result += *case_or;
              result += Indent(indent + 2) + "break;\n";
              result += Indent(indent + 1) + "}\n";
            }
            if (s.default_scope.has_value()) {
              const auto& default_scope =
                  stmt.child_procedural_scopes.at(s.default_scope->value);
              auto default_or =
                  RenderNestedProceduralScope(ctx, default_scope, indent + 2);
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
            const auto& var = ctx.StructuralScope().GetStructuralVar(s.target);
            const auto& target_scope =
                ctx.StructuralScope().GetChildStructuralScope(s.scope_id);
            std::string args_str;
            for (std::size_t i = 0; i < s.args.size(); ++i) {
              if (i != 0) args_str += ", ";
              auto arg_or = RenderExprAsNative(ctx, ctx.Expr(s.args[i]));
              if (!arg_or) return std::unexpected(std::move(arg_or.error()));
              args_str += *arg_or;
            }
            const std::string make =
                "std::make_unique<" + target_scope.name + ">(" + args_str + ")";
            if (mir::IsVectorOfOwningObjectType(ctx.Unit(), var.type)) {
              return Indent(indent) + var.name + ".push_back(" + make + ");\n";
            }
            if (mir::IsOwningObjectType(ctx.Unit(), var.type)) {
              return Indent(indent) + var.name + " = " + make + ";\n";
            }
            throw InternalError(
                "ConstructOwnedObjectStmt target is not an owning object var");
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
              const auto& cond_expr =
                  ctx.ProceduralScope().exprs.at(s.condition->value);
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
              const auto& step_expr =
                  ctx.ProceduralScope().exprs.at(s.step[i].value);
              auto step_or = RenderExpr(ctx, step_expr);
              if (!step_or) {
                return std::unexpected(std::move(step_or.error()));
              }
              step += *step_or;
            }
            const auto& scope = stmt.child_procedural_scopes.at(s.scope.value);
            auto rendered_for_or =
                RenderNestedProceduralScope(ctx, scope, indent + 1);
            if (!rendered_for_or) {
              return std::unexpected(std::move(rendered_for_or.error()));
            }
            std::string result = Indent(indent) + "for (" + init + "; " + cond +
                                 "; " + step + ") {\n";
            result += *rendered_for_or;
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
  if (!rendered_or) return std::unexpected(std::move(rendered_or.error()));
  out += *rendered_or;
  return out;
}

auto RenderProceduralScopeStatements(
    const RenderContext& ctx, std::size_t indent) -> diag::Result<std::string> {
  const auto& scope = ctx.ProceduralScope();
  std::string out;
  for (const auto& sid : scope.root_stmts) {
    auto rendered_or = RenderStmt(ctx, scope.stmts.at(sid.value), indent);
    if (!rendered_or) return std::unexpected(std::move(rendered_or.error()));
    out += *rendered_or;
  }
  return out;
}

auto RenderNestedProceduralScope(
    const RenderContext& parent, const mir::ProceduralScope& scope,
    std::size_t indent) -> diag::Result<std::string> {
  const RenderContext child = parent.WithProceduralScope(scope);
  return RenderProceduralScopeStatements(child, indent);
}

}  // namespace lyra::backend::cpp
