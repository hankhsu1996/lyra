#include "render_stmt.hpp"

#include <cstddef>
#include <string>
#include <variant>

#include "formatting.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/support/overloaded.hpp"
#include "render_context.hpp"
#include "render_expr.hpp"

namespace lyra::projection::cpp {

namespace {

auto RenderStmt(
    const RenderContext& ctx, const mir::Stmt& stmt, std::size_t indent)
    -> std::string;

auto RenderStmtData(
    const RenderContext& ctx, const mir::StmtData& data, std::size_t indent)
    -> std::string {
  return std::visit(
      support::Overloaded{
          [&](const mir::Assignment& a) -> std::string {
            const auto& target = ctx.Unit().GetMember(a.target);
            const auto& value_expr = ctx.Process().exprs.at(a.value.value);
            return Indent(indent) + target.name + " = " +
                   RenderExpr(ctx, value_expr) + ";\n";
          },
          [&](const mir::BlockStmt& b) -> std::string {
            std::string out;
            out += Indent(indent) + "{\n";
            for (const auto& sid : b.statements) {
              out += RenderStmt(
                  ctx, ctx.Process().stmts.at(sid.value), indent + 1);
            }
            out += Indent(indent) + "}\n";
            return out;
          },
      },
      data);
}

auto RenderStmt(
    const RenderContext& ctx, const mir::Stmt& stmt, std::size_t indent)
    -> std::string {
  std::string out;
  if (stmt.label.has_value()) {
    out += Indent(indent) + *stmt.label + ":\n";
  }
  out += RenderStmtData(ctx, stmt.data, indent);
  return out;
}

}  // namespace

auto RenderProcessBody(
    const RenderContext& ctx, mir::StmtId body_id, std::size_t indent)
    -> std::string {
  const auto& body_stmt = ctx.Process().stmts.at(body_id.value);

  // Top-level BlockStmt is unwrapped: the enclosing method supplies the outer
  // braces, so nested children render inline at the method-body indent.
  if (const auto* block = std::get_if<mir::BlockStmt>(&body_stmt.data)) {
    std::string out;
    if (body_stmt.label.has_value()) {
      out += Indent(indent) + *body_stmt.label + ":\n";
    }
    for (const auto& sid : block->statements) {
      out += RenderStmt(ctx, ctx.Process().stmts.at(sid.value), indent);
    }
    return out;
  }

  return RenderStmt(ctx, body_stmt, indent);
}

}  // namespace lyra::projection::cpp
