#include "lyra/backend/cpp/render_stmt.hpp"

#include <cstddef>
#include <format>
#include <string>
#include <string_view>
#include <variant>
#include <vector>

#include "lyra/backend/cpp/formatting.hpp"
#include "lyra/backend/cpp/render_expr.hpp"
#include "lyra/backend/cpp/render_type.hpp"
#include "lyra/backend/cpp/scope_view.hpp"
#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/mir/stmt.hpp"

namespace lyra::backend::cpp {

namespace {

auto RenderForInit(const ScopeView& view, const mir::ForInit& init)
    -> std::string {
  return std::visit(
      Overloaded{
          [&](const mir::ForInitDecl& d) -> std::string {
            // The induction variable's C++ type is whatever the initializer
            // yields (every integral value is a PackedArray), so `auto` is the
            // exact same type as spelling it out -- and reads as the idiomatic
            // loop counter.
            const auto& lv = view.Code().locals.Get(d.induction_var);
            const auto& init_expr = view.Block().exprs.Get(d.init);
            return std::format(
                "auto {} = {}", lv.name, RenderExpr(view, init_expr));
          },
          [&](const mir::ForInitExpr& e) -> std::string {
            const auto& expr = view.Block().exprs.Get(e.expr);
            return RenderExpr(view, expr);
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

auto RenderLocalDeclStmt(
    const ScopeView& view, const mir::LocalDeclStmt& s, std::size_t indent)
    -> std::string {
  const auto& lv = view.Code().locals.Get(s.target);
  const auto& init_expr = view.Block().exprs.Get(s.init);
  return std::format(
      "{}{} {} = {};\n", Indent(indent),
      RenderTypeAsCpp(view.Unit(), view.Class(), lv.type), lv.name,
      RenderExpr(view, init_expr));
}

auto RenderExprStmt(
    const ScopeView& view, const mir::ExprStmt& s, std::size_t indent)
    -> std::string {
  const auto& expr = view.Block().exprs.Get(s.expr);
  return std::format("{}{};\n", Indent(indent), RenderExpr(view, expr));
}

auto RenderBlockStmt(
    const ScopeView& view, const mir::BlockStmt& s, std::size_t indent)
    -> std::string {
  const auto& child = view.Block().child_scopes.Get(s.scope);
  std::string result = std::format("{}{{\n", Indent(indent));
  result += RenderNestedBlock(view, child, indent + 1);
  result += std::format("{}}}\n", Indent(indent));
  return result;
}

auto RenderIfStmt(
    const ScopeView& view, const mir::IfStmt& s, std::size_t indent)
    -> std::string {
  const auto& cond_expr = view.Block().exprs.Get(s.condition);
  const auto& then_scope = view.Block().child_scopes.Get(s.then_scope);
  std::string result;
  result += std::format(
      "{}if ({}) {{\n", Indent(indent), RenderConditionAsBool(view, cond_expr));
  result += RenderNestedBlock(view, then_scope, indent + 1);
  result += std::format("{}}}", Indent(indent));
  if (s.else_scope.has_value()) {
    const auto& else_scope = view.Block().child_scopes.Get(*s.else_scope);
    result += " else {\n";
    result += RenderNestedBlock(view, else_scope, indent + 1);
    result += std::format("{}}}", Indent(indent));
  }
  result += "\n";
  return result;
}

// C++ has no labeled break, so a `foreach` break that must leave every nested
// dimension lowers to a `goto` aimed at a label after the outermost loop. Both
// the landing label and the jump derive their name from the loop's LoopLabelId.
auto BreakLandingLabel(mir::LoopLabelId label) -> std::string {
  return std::format("__lyra_break_{}", label.value);
}

auto RenderForStmt(
    const ScopeView& view, const mir::ForStmt& s, std::size_t indent)
    -> std::string {
  std::string init;
  for (std::size_t i = 0; i < s.init.size(); ++i) {
    if (i != 0) init += ", ";
    init += RenderForInit(view, s.init[i]);
  }
  std::string cond;
  if (s.condition.has_value()) {
    const auto& cond_expr = view.Block().exprs.Get(*s.condition);
    cond = RenderConditionAsBool(view, cond_expr);
  }
  std::string step;
  for (std::size_t i = 0; i < s.step.size(); ++i) {
    if (i != 0) step += ", ";
    const auto& step_expr = view.Block().exprs.Get(s.step[i]);
    step += RenderExpr(view, step_expr);
  }
  const auto& block = view.Block().child_scopes.Get(s.scope);
  std::string result =
      std::format("{}for ({}; {}; {}) {{\n", Indent(indent), init, cond, step);
  result += RenderNestedBlock(view, block, indent + 1);
  result += std::format("{}}}\n", Indent(indent));
  if (s.break_label.has_value()) {
    result += std::format(
        "{}{}:;\n", Indent(indent), BreakLandingLabel(*s.break_label));
  }
  return result;
}

auto RenderWhileStmt(
    const ScopeView& view, const mir::WhileStmt& s, std::size_t indent)
    -> std::string {
  const auto& cond_expr = view.Block().exprs.Get(s.condition);
  const auto& block = view.Block().child_scopes.Get(s.scope);
  std::string result = std::format(
      "{}while ({}) {{\n", Indent(indent),
      RenderConditionAsBool(view, cond_expr));
  result += RenderNestedBlock(view, block, indent + 1);
  result += std::format("{}}}\n", Indent(indent));
  return result;
}

auto RenderDoWhileStmt(
    const ScopeView& view, const mir::DoWhileStmt& s, std::size_t indent)
    -> std::string {
  const auto& cond_expr = view.Block().exprs.Get(s.condition);
  const auto& block = view.Block().child_scopes.Get(s.scope);
  std::string result = std::format("{}do {{\n", Indent(indent));
  result += RenderNestedBlock(view, block, indent + 1);
  result += std::format(
      "{}}} while ({});\n", Indent(indent),
      RenderConditionAsBool(view, cond_expr));
  return result;
}

auto RenderSensitivityWaitStmt(
    const ScopeView& view, const mir::SensitivityWaitStmt& s,
    std::size_t indent) -> std::string {
  std::string result =
      std::format("{}co_await lyra::runtime::WaitAny({{", Indent(indent));
  for (std::size_t i = 0; i < s.reads.size(); ++i) {
    const auto& read = s.reads[i];
    if (i != 0) result += ", ";
    result += std::format(
        "{{{}, {}, {}ULL, {}ULL}}",
        RenderExpr(view, view.Expr(read.observable_ptr)),
        RenderEventEdgeAsRuntime(read.edge_kind), read.lsb_bit_offset,
        read.bit_width);
  }
  result += "});\n";
  return result;
}

}  // namespace

auto RenderStmt(
    const ScopeView& view, const mir::Stmt& stmt, std::size_t indent)
    -> std::string {
  std::string out;
  if (stmt.label.has_value()) {
    out += std::format("{}{}:\n", Indent(indent), *stmt.label);
  }
  out += std::visit(
      Overloaded{
          [&](const mir::EmptyStmt&) -> std::string {
            return std::format("{};\n", Indent(indent));
          },
          [&](const mir::LocalDeclStmt& s) -> std::string {
            return RenderLocalDeclStmt(view, s, indent);
          },
          [&](const mir::ExprStmt& s) -> std::string {
            return RenderExprStmt(view, s, indent);
          },
          [&](const mir::BlockStmt& s) -> std::string {
            return RenderBlockStmt(view, s, indent);
          },
          [&](const mir::IfStmt& s) -> std::string {
            return RenderIfStmt(view, s, indent);
          },
          [&](const mir::ForStmt& s) -> std::string {
            return RenderForStmt(view, s, indent);
          },
          [&](const mir::WhileStmt& s) -> std::string {
            return RenderWhileStmt(view, s, indent);
          },
          [&](const mir::DoWhileStmt& s) -> std::string {
            return RenderDoWhileStmt(view, s, indent);
          },
          [&](const mir::BreakStmt& s) -> std::string {
            if (s.target.has_value()) {
              return std::format(
                  "{}goto {};\n", Indent(indent), BreakLandingLabel(*s.target));
            }
            return std::format("{}break;\n", Indent(indent));
          },
          [&](const mir::ContinueStmt&) -> std::string {
            return std::format("{}continue;\n", Indent(indent));
          },
          [&](const mir::ReturnStmt& s) -> std::string {
            // `is_coroutine_return` (set at HIR-to-MIR from the enclosing
            // callable's coroutine-ness, mir/stmt.hpp) is a C++ render hint --
            // LIR / LLVM ignore it -- choosing `co_return` over `return`. A
            // value rides the result either way (LRM 13.3 / 13.4.1).
            const std::string keyword =
                s.is_coroutine_return ? "co_return" : "return";
            if (!s.value.has_value()) {
              return std::format("{}{};\n", Indent(indent), keyword);
            }
            return std::format(
                "{}{} {};\n", Indent(indent), keyword,
                RenderExpr(view, view.Expr(*s.value)));
          },
          [&](const mir::SensitivityWaitStmt& s) -> std::string {
            return RenderSensitivityWaitStmt(view, s, indent);
          },
      },
      stmt.data);
  return out;
}

auto RenderBlockStatements(const ScopeView& view, std::size_t indent)
    -> std::string {
  const auto& block = view.Block();
  // When a scope's whole content is a single begin/end block, the enclosing
  // braces (a function body, a loop or branch body, an outer block) already
  // scope it; render the block's body directly instead of emitting a redundant
  // `{ }`. Recursing collapses a chain of such blocks.
  if (block.root_stmts.size() == 1) {
    const auto& only = block.stmts.Get(block.root_stmts.front());
    if (const auto* block_stmt = std::get_if<mir::BlockStmt>(&only.data)) {
      const auto& inner = block.child_scopes.Get(block_stmt->scope);
      return RenderBlockStatements(view.WithBlock(inner), indent);
    }
  }
  std::string out;
  for (const auto& sid : block.root_stmts) {
    out += RenderStmt(view, block.stmts.Get(sid), indent);
  }
  return out;
}

auto RenderNestedBlock(
    const ScopeView& parent, const mir::Block& block, std::size_t indent)
    -> std::string {
  const ScopeView child = parent.WithBlock(block);
  return RenderBlockStatements(child, indent);
}

}  // namespace lyra::backend::cpp
