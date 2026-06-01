#include "lyra/backend/cpp/render_stmt.hpp"

#include <cstddef>
#include <string>
#include <string_view>
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
            const auto& init_expr =
                ctx.ProceduralScope().exprs.at(d.init.value);
            auto rendered_or = RenderExpr(ctx, init_expr);
            if (!rendered_or) {
              return std::unexpected(std::move(rendered_or.error()));
            }
            return *type_or + " " + lv.name + " = " + *rendered_or;
          },
          [&](const mir::ForInitExpr& e) -> diag::Result<std::string> {
            const auto& expr = ctx.ProceduralScope().exprs.at(e.expr.value);
            return RenderExpr(ctx, expr);
          },
      },
      init);
}

auto RenderEventEdgeAsRuntime(mir::EventEdge edge) -> std::string_view {
  switch (edge) {
    case mir::EventEdge::kAnyChange:
      return "lyra::runtime::Edge::kAnyChange";
    case mir::EventEdge::kPosedge:
      return "lyra::runtime::Edge::kPosedge";
    case mir::EventEdge::kNegedge:
      return "lyra::runtime::Edge::kNegedge";
    case mir::EventEdge::kBothEdges:
      return "lyra::runtime::Edge::kBothEdges";
  }
  throw InternalError("RenderEventEdgeAsRuntime: unknown EventEdge value");
}

auto RenderProceduralVarDeclStmt(
    const RenderContext& ctx, const mir::ProceduralVarDeclStmt& s,
    std::size_t indent) -> diag::Result<std::string> {
  const auto& lv =
      ctx.ProceduralScopeAtHops(s.target.hops).vars.at(s.target.var.value);
  auto type_or = RenderTypeAsCpp(ctx.Unit(), ctx.StructuralScope(), lv.type);
  if (!type_or) return std::unexpected(std::move(type_or.error()));
  const auto& init_expr = ctx.ProceduralScope().exprs.at(s.init.value);
  auto init_or = RenderExpr(ctx, init_expr);
  if (!init_or) return std::unexpected(std::move(init_or.error()));
  return Indent(indent) + *type_or + " " + lv.name + " = " + *init_or + ";\n";
}

auto RenderExprStmt(
    const RenderContext& ctx, const mir::ExprStmt& s, std::size_t indent)
    -> diag::Result<std::string> {
  const auto& expr = ctx.ProceduralScope().exprs.at(s.expr.value);
  auto rendered_or = RenderExpr(ctx, expr);
  if (!rendered_or) return std::unexpected(std::move(rendered_or.error()));
  return Indent(indent) + *rendered_or + ";\n";
}

auto RenderBlockStmtNode(
    const RenderContext& ctx, const mir::Stmt& stmt, const mir::BlockStmt& s,
    std::size_t indent) -> diag::Result<std::string> {
  const auto& child = stmt.child_procedural_scopes.at(s.scope.value);
  std::string result = Indent(indent) + "{\n";
  auto child_or = RenderNestedProceduralScope(ctx, child, indent + 1);
  if (!child_or) return std::unexpected(std::move(child_or.error()));
  result += *child_or;
  result += Indent(indent) + "}\n";
  return result;
}

auto RenderIfStmtNode(
    const RenderContext& ctx, const mir::Stmt& stmt, const mir::IfStmt& s,
    std::size_t indent) -> diag::Result<std::string> {
  const auto& cond_expr = ctx.ProceduralScope().exprs.at(s.condition.value);
  const auto& then_scope = stmt.child_procedural_scopes.at(s.then_scope.value);
  auto cond_or = RenderConditionAsBool(ctx, cond_expr);
  if (!cond_or) return std::unexpected(std::move(cond_or.error()));
  auto then_or = RenderNestedProceduralScope(ctx, then_scope, indent + 1);
  if (!then_or) return std::unexpected(std::move(then_or.error()));
  std::string result;
  result += Indent(indent) + "if (" + *cond_or + ") {\n";
  result += *then_or;
  result += Indent(indent) + "}";
  if (s.else_scope.has_value()) {
    const auto& else_scope =
        stmt.child_procedural_scopes.at(s.else_scope->value);
    auto else_or = RenderNestedProceduralScope(ctx, else_scope, indent + 1);
    if (!else_or) return std::unexpected(std::move(else_or.error()));
    result += " else {\n";
    result += *else_or;
    result += Indent(indent) + "}";
  }
  result += "\n";
  return result;
}

auto RenderConstructOwnedObjectStmt(
    const RenderContext& ctx, const mir::ConstructOwnedObjectStmt& s,
    std::size_t indent) -> diag::Result<std::string> {
  const auto& var = ctx.StructuralScope().GetStructuralVar(s.target);
  const auto& target_scope =
      ctx.StructuralScope().GetChildStructuralScope(s.scope_id);
  std::string args_str;
  for (std::size_t i = 0; i < s.args.size(); ++i) {
    if (i != 0) args_str += ", ";
    auto arg_or = RenderExpr(ctx, ctx.Expr(s.args[i]));
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
}

auto RenderForStmtNode(
    const RenderContext& ctx, const mir::Stmt& stmt, const mir::ForStmt& s,
    std::size_t indent) -> diag::Result<std::string> {
  std::string init;
  for (std::size_t i = 0; i < s.init.size(); ++i) {
    if (i != 0) init += ", ";
    auto init_or = RenderForInit(ctx, s.init[i]);
    if (!init_or) return std::unexpected(std::move(init_or.error()));
    init += *init_or;
  }
  std::string cond;
  if (s.condition.has_value()) {
    const auto& cond_expr = ctx.ProceduralScope().exprs.at(s.condition->value);
    auto cond_or = RenderConditionAsBool(ctx, cond_expr);
    if (!cond_or) return std::unexpected(std::move(cond_or.error()));
    cond = *std::move(cond_or);
  }
  std::string step;
  for (std::size_t i = 0; i < s.step.size(); ++i) {
    if (i != 0) step += ", ";
    const auto& step_expr = ctx.ProceduralScope().exprs.at(s.step[i].value);
    auto step_or = RenderExpr(ctx, step_expr);
    if (!step_or) return std::unexpected(std::move(step_or.error()));
    step += *step_or;
  }
  const auto& scope = stmt.child_procedural_scopes.at(s.scope.value);
  auto body_or = RenderNestedProceduralScope(ctx, scope, indent + 1);
  if (!body_or) return std::unexpected(std::move(body_or.error()));
  std::string result =
      Indent(indent) + "for (" + init + "; " + cond + "; " + step + ") {\n";
  result += *body_or;
  result += Indent(indent) + "}\n";
  return result;
}

auto RenderWhileStmtNode(
    const RenderContext& ctx, const mir::Stmt& stmt, const mir::WhileStmt& s,
    std::size_t indent) -> diag::Result<std::string> {
  const auto& cond_expr = ctx.ProceduralScope().exprs.at(s.condition.value);
  auto cond_or = RenderConditionAsBool(ctx, cond_expr);
  if (!cond_or) return std::unexpected(std::move(cond_or.error()));
  const auto& scope = stmt.child_procedural_scopes.at(s.scope.value);
  auto body_or = RenderNestedProceduralScope(ctx, scope, indent + 1);
  if (!body_or) return std::unexpected(std::move(body_or.error()));
  std::string result =
      Indent(indent) + "while (" + *std::move(cond_or) + ") {\n";
  result += *body_or;
  result += Indent(indent) + "}\n";
  return result;
}

auto RenderDoWhileStmtNode(
    const RenderContext& ctx, const mir::Stmt& stmt, const mir::DoWhileStmt& s,
    std::size_t indent) -> diag::Result<std::string> {
  const auto& cond_expr = ctx.ProceduralScope().exprs.at(s.condition.value);
  auto cond_or = RenderConditionAsBool(ctx, cond_expr);
  if (!cond_or) return std::unexpected(std::move(cond_or.error()));
  const auto& scope = stmt.child_procedural_scopes.at(s.scope.value);
  auto body_or = RenderNestedProceduralScope(ctx, scope, indent + 1);
  if (!body_or) return std::unexpected(std::move(body_or.error()));
  std::string result = Indent(indent) + "do {\n";
  result += *body_or;
  result += Indent(indent) + "} while (" + *std::move(cond_or) + ");\n";
  return result;
}

auto RenderSensitivityWaitStmt(
    const RenderContext& ctx, const mir::SensitivityWaitStmt& s,
    std::size_t indent) -> diag::Result<std::string> {
  std::string result = Indent(indent) + "co_await lyra::runtime::WaitAny({";
  for (std::size_t i = 0; i < s.reads.size(); ++i) {
    const auto& read = s.reads[i];
    auto name_or = RenderStructuralVarName(ctx, read.ref);
    if (!name_or) return std::unexpected(std::move(name_or.error()));
    if (i != 0) result += ", ";
    // bit_range follows slang's `(lo_bit, hi_bit)` inclusive convention; the
    // runtime Trigger takes `(lsb_offset, width)`.
    const auto lsb = read.bit_range.first;
    const auto width = read.bit_range.second - read.bit_range.first + 1;
    result += "{&" + *name_or + ", " +
              std::string{RenderEventEdgeAsRuntime(read.edge_kind)} + ", " +
              std::to_string(lsb) + "ULL, " + std::to_string(width) + "ULL}";
  }
  result += "});\n";
  return result;
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
          [&](const mir::DelayStmt& s) -> diag::Result<std::string> {
            return Indent(indent) +
                   "co_await lyra::runtime::Delay(*services_, " +
                   std::to_string(s.duration) + ");\n";
          },
          [&](const mir::ProceduralVarDeclStmt& s)
              -> diag::Result<std::string> {
            return RenderProceduralVarDeclStmt(ctx, s, indent);
          },
          [&](const mir::ExprStmt& s) -> diag::Result<std::string> {
            return RenderExprStmt(ctx, s, indent);
          },
          [&](const mir::BlockStmt& s) -> diag::Result<std::string> {
            return RenderBlockStmtNode(ctx, stmt, s, indent);
          },
          [&](const mir::IfStmt& s) -> diag::Result<std::string> {
            return RenderIfStmtNode(ctx, stmt, s, indent);
          },
          [&](const mir::ConstructOwnedObjectStmt& s)
              -> diag::Result<std::string> {
            return RenderConstructOwnedObjectStmt(ctx, s, indent);
          },
          [&](const mir::ForStmt& s) -> diag::Result<std::string> {
            return RenderForStmtNode(ctx, stmt, s, indent);
          },
          [&](const mir::WhileStmt& s) -> diag::Result<std::string> {
            return RenderWhileStmtNode(ctx, stmt, s, indent);
          },
          [&](const mir::DoWhileStmt& s) -> diag::Result<std::string> {
            return RenderDoWhileStmtNode(ctx, stmt, s, indent);
          },
          [&](const mir::BreakStmt&) -> diag::Result<std::string> {
            return Indent(indent) + "break;\n";
          },
          [&](const mir::ContinueStmt&) -> diag::Result<std::string> {
            return Indent(indent) + "continue;\n";
          },
          [&](const mir::ReturnStmt& s) -> diag::Result<std::string> {
            if (!s.value.has_value()) {
              return Indent(indent) + "return;\n";
            }
            auto value_or = RenderExpr(ctx, ctx.Expr(*s.value));
            if (!value_or) return std::unexpected(std::move(value_or.error()));
            return Indent(indent) + "return " + *value_or + ";\n";
          },
          [&](const mir::SensitivityWaitStmt& s) -> diag::Result<std::string> {
            return RenderSensitivityWaitStmt(ctx, s, indent);
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
