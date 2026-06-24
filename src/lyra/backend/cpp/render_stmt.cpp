#include "lyra/backend/cpp/render_stmt.hpp"

#include <cstddef>
#include <string>
#include <string_view>
#include <utility>
#include <variant>
#include <vector>

#include "lyra/backend/cpp/formatting.hpp"
#include "lyra/backend/cpp/render_expr.hpp"
#include "lyra/backend/cpp/render_type.hpp"
#include "lyra/backend/cpp/scope_view.hpp"
#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/diag/diagnostic.hpp"
#include "lyra/mir/class.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/mir/type.hpp"

namespace lyra::backend::cpp {

namespace {

auto RenderForInit(const ScopeView& view, const mir::ForInit& init)
    -> diag::Result<std::string> {
  return std::visit(
      Overloaded{
          [&](const mir::ForInitDecl& d) -> diag::Result<std::string> {
            // The induction variable's C++ type is whatever the initializer
            // yields (every integral value is a PackedArray), so `auto` is the
            // exact same type as spelling it out -- and reads as the idiomatic
            // loop counter.
            const auto& lv = view.BlockAtHops(d.induction_var.hops)
                                 .vars.Get(d.induction_var.var);
            const auto& init_expr = view.Block().exprs.Get(d.init);
            auto rendered_or = RenderExpr(view, init_expr);
            if (!rendered_or) {
              return std::unexpected(std::move(rendered_or.error()));
            }
            return "auto " + lv.name + " = " + *rendered_or;
          },
          [&](const mir::ForInitExpr& e) -> diag::Result<std::string> {
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
    -> diag::Result<std::string> {
  const auto& lv = view.BlockAtHops(s.target.hops).vars.Get(s.target.var);
  auto type_or = RenderTypeAsCpp(view.Unit(), view.Class(), lv.type);
  if (!type_or) return std::unexpected(std::move(type_or.error()));
  const auto& init_expr = view.Block().exprs.Get(s.init);
  auto init_or = RenderExpr(view, init_expr);
  if (!init_or) return std::unexpected(std::move(init_or.error()));
  return Indent(indent) + *type_or + " " + lv.name + " = " + *init_or + ";\n";
}

auto RenderExprStmt(
    const ScopeView& view, const mir::ExprStmt& s, std::size_t indent)
    -> diag::Result<std::string> {
  const auto& expr = view.Block().exprs.Get(s.expr);
  auto rendered_or = RenderExpr(view, expr);
  if (!rendered_or) return std::unexpected(std::move(rendered_or.error()));
  return Indent(indent) + *rendered_or + ";\n";
}

// The C++ realization of a suspension point (mir/stmt.hpp): `co_await` the
// awaitable operand. One uniform site for every suspending construct ($finish,
// task call, named-event await).
auto RenderAwaitStmt(
    const ScopeView& view, const mir::AwaitStmt& s, std::size_t indent)
    -> diag::Result<std::string> {
  const auto& expr = view.Block().exprs.Get(s.awaitable);
  auto rendered_or = RenderExpr(view, expr);
  if (!rendered_or) return std::unexpected(std::move(rendered_or.error()));
  return Indent(indent) + "co_await " + *rendered_or + ";\n";
}

auto RenderBlockStmtNode(
    const ScopeView& view, const mir::BlockStmt& s, std::size_t indent)
    -> diag::Result<std::string> {
  const auto& child = view.Block().child_scopes.Get(s.scope);
  std::string result = Indent(indent) + "{\n";
  auto child_or = RenderNestedBlock(view, child, indent + 1);
  if (!child_or) return std::unexpected(std::move(child_or.error()));
  result += *child_or;
  result += Indent(indent) + "}\n";
  return result;
}

auto RenderForkJoinModeLiteral(mir::JoinMode mode) -> std::string_view {
  switch (mode) {
    case mir::JoinMode::kAll:
      return "lyra::runtime::JoinMode::kAll";
    case mir::JoinMode::kAny:
      return "lyra::runtime::JoinMode::kAny";
    case mir::JoinMode::kNone:
      return "lyra::runtime::JoinMode::kNone";
  }
  return "lyra::runtime::JoinMode::kAll";
}

// LRM 9.3.2: a fork is a block, rendered as a C++ block at the fork
// site. Its block_item_declarations initialize first, in the parent, before any
// branch spawns -- so a branch's by-value argument of one of them is a
// snapshot. Each branch is a coroutine-typed closure; the shared closure
// renderer emits it as a stateless coroutine lambda invoked on its captures,
// yielding a `lyra::runtime::Coroutine` collected into `fork_branches`. The
// fork then waits per the join mode.
auto RenderForkStmtNode(
    const ScopeView& view, const mir::ForkStmt& s, std::size_t indent)
    -> diag::Result<std::string> {
  // The fork-branches vector lives in this fork's own `{...}` block scope
  // (opened just below), so a fixed name suffices -- a sibling fork in the
  // surrounding scope, or a nested fork in a branch lambda body, each has
  // its own `fork_branches` in its own C++ scope.
  const std::string vec = "fork_branches";
  const ScopeView fork_view =
      view.WithBlock(view.Block().child_scopes.Get(s.scope));
  std::string out = Indent(indent) + "{\n";
  auto locals_or = RenderBlockStatements(fork_view, indent + 1);
  if (!locals_or) return std::unexpected(std::move(locals_or.error()));
  out += *locals_or;
  out += Indent(indent + 1) + "std::vector<lyra::runtime::Coroutine> " + vec +
         ";\n";
  for (const auto branch : s.branches) {
    auto closure_or = RenderExpr(fork_view, fork_view.Expr(branch));
    if (!closure_or) return std::unexpected(std::move(closure_or.error()));
    out += Indent(indent + 1) + vec + ".push_back(" + *closure_or + ");\n";
  }
  out += Indent(indent + 1) + "co_await lyra::runtime::Fork(" +
         "self->Services()" + ", std::move(" + vec + "), " +
         std::string(RenderForkJoinModeLiteral(s.mode)) + ");\n";
  out += Indent(indent) + "}\n";
  return out;
}

auto RenderIfStmtNode(
    const ScopeView& view, const mir::IfStmt& s, std::size_t indent)
    -> diag::Result<std::string> {
  const auto& cond_expr = view.Block().exprs.Get(s.condition);
  const auto& then_scope = view.Block().child_scopes.Get(s.then_scope);
  auto cond_or = RenderConditionAsBool(view, cond_expr);
  if (!cond_or) return std::unexpected(std::move(cond_or.error()));
  auto then_or = RenderNestedBlock(view, then_scope, indent + 1);
  if (!then_or) return std::unexpected(std::move(then_or.error()));
  std::string result;
  result += Indent(indent) + "if (" + *cond_or + ") {\n";
  result += *then_or;
  result += Indent(indent) + "}";
  if (s.else_scope.has_value()) {
    const auto& else_scope = view.Block().child_scopes.Get(*s.else_scope);
    auto else_or = RenderNestedBlock(view, else_scope, indent + 1);
    if (!else_or) return std::unexpected(std::move(else_or.error()));
    result += " else {\n";
    result += *else_or;
    result += Indent(indent) + "}";
  }
  result += "\n";
  return result;
}

auto RenderConstructOwnedObjectStmt(
    const ScopeView& view, const mir::ConstructOwnedObjectStmt& s,
    std::size_t indent) -> diag::Result<std::string> {
  const auto& var = view.Class().members.Get(s.target);
  const auto& target_scope = view.Class().nested_classes.Get(s.scope_id);
  std::string trailing_args;
  for (const auto arg_id : s.args) {
    auto arg_or = RenderExpr(view, view.Expr(arg_id));
    if (!arg_or) return std::unexpected(std::move(arg_or.error()));
    trailing_args += ", " + *arg_or;
  }
  const auto child = mir::GetChildScope(view.Unit(), var.type);
  if (!child.has_value() ||
      !std::holds_alternative<mir::GenerateScopeChild>(*child)) {
    throw InternalError(
        "ConstructOwnedObjectStmt target is not an owned object var");
  }
  const std::string lhs = "self->" + var.name;
  // The runtime scope name is the source-level label (LRM 27.6) -- the name an
  // upward climb and a by-name child lookup match against -- not the emitted
  // class name.
  const std::string reg_name =
      var.source_name.empty() ? var.name : var.source_name;
  const std::string make = "std::make_unique<" + target_scope.name +
                           ">(self, \"" + reg_name + "\", self->Services()" +
                           trailing_args + ")";
  if (std::holds_alternative<mir::VectorType>(
          view.Unit().GetType(var.type).data)) {
    return Indent(indent) + lhs + ".push_back(" + make + ");\n" +
           Indent(indent) + "self->RegisterChild(\"" + reg_name +
           "\", std::array{" + lhs + ".size() - 1}, *" + lhs + ".back());\n";
  }
  return Indent(indent) + lhs + " = " + make + ";\n" + Indent(indent) +
         "self->RegisterChild(\"" + reg_name + "\", {}, *" + lhs + ");\n";
}

// Materializes an external-unit member by recursing on its type, mirroring the
// type walk in RenderTypeAsCpp: each vector layer emits a counted loop that
// grows the vector and recurses into the new element; the owning-pointer leaf
// assigns a freshly constructed child whose parent is `this` and whose name is
// the member's compile-time label. `dims` carries one element count per vector
// layer, so a scalar (no vector layer) renders just the leaf.
auto RenderExternalUnitFill(
    const ScopeView& view, const std::string& lvalue, mir::TypeId type,
    const std::string& unit_name, const std::string& label,
    const std::vector<std::uint32_t>& dims, std::size_t depth,
    std::size_t indent) -> std::string {
  if (const auto* vec =
          std::get_if<mir::VectorType>(&view.Unit().GetType(type).data)) {
    const std::string idx = "i" + std::to_string(depth);
    std::string out;
    out += Indent(indent) + "for (std::size_t " + idx + " = 0; " + idx + " < " +
           std::to_string(dims.at(depth)) + "; ++" + idx + ") {\n";
    out += Indent(indent + 1) + lvalue + ".emplace_back();\n";
    out += RenderExternalUnitFill(
        view, lvalue + "[" + idx + "]", vec->element, unit_name, label, dims,
        depth + 1, indent + 1);
    out += Indent(indent) + "}\n";
    return out;
  }
  std::string out = Indent(indent) + lvalue + " = std::make_unique<" +
                    unit_name + ">(self, \"" + label +
                    "\", self->Services());\n";
  std::string idx = "{}";
  if (depth > 0) {
    idx = "std::array{";
    for (std::size_t d = 0; d < depth; ++d) {
      if (d != 0) idx += ", ";
      idx += "i" + std::to_string(d);
    }
    idx += "}";
  }
  out += Indent(indent) + "self->RegisterChild(\"" + label + "\", " + idx +
         ", *" + lvalue + ");\n";
  return out;
}

auto RenderConstructExternalUnitStmt(
    const ScopeView& view, const mir::ConstructExternalUnitStmt& s,
    std::size_t indent) -> diag::Result<std::string> {
  const auto& var = view.Class().members.Get(s.target);
  return RenderExternalUnitFill(
      view, "self->" + var.name, var.type, s.unit_name, var.name, s.dims, 0,
      indent);
}

// C++ has no labeled break, so a `foreach` break that must leave every nested
// dimension lowers to a `goto` aimed at a label after the outermost loop. Both
// the landing label and the jump derive their name from the loop's LoopLabelId.
auto BreakLandingLabel(mir::LoopLabelId label) -> std::string {
  return "__lyra_break_" + std::to_string(label.value);
}

auto RenderForStmtNode(
    const ScopeView& view, const mir::ForStmt& s, std::size_t indent)
    -> diag::Result<std::string> {
  std::string init;
  for (std::size_t i = 0; i < s.init.size(); ++i) {
    if (i != 0) init += ", ";
    auto init_or = RenderForInit(view, s.init[i]);
    if (!init_or) return std::unexpected(std::move(init_or.error()));
    init += *init_or;
  }
  std::string cond;
  if (s.condition.has_value()) {
    const auto& cond_expr = view.Block().exprs.Get(*s.condition);
    auto cond_or = RenderConditionAsBool(view, cond_expr);
    if (!cond_or) return std::unexpected(std::move(cond_or.error()));
    cond = *std::move(cond_or);
  }
  std::string step;
  for (std::size_t i = 0; i < s.step.size(); ++i) {
    if (i != 0) step += ", ";
    const auto& step_expr = view.Block().exprs.Get(s.step[i]);
    auto step_or = RenderExpr(view, step_expr);
    if (!step_or) return std::unexpected(std::move(step_or.error()));
    step += *step_or;
  }
  const auto& block = view.Block().child_scopes.Get(s.scope);
  auto body_or = RenderNestedBlock(view, block, indent + 1);
  if (!body_or) return std::unexpected(std::move(body_or.error()));
  std::string result =
      Indent(indent) + "for (" + init + "; " + cond + "; " + step + ") {\n";
  result += *body_or;
  result += Indent(indent) + "}\n";
  if (s.break_label.has_value()) {
    result += Indent(indent) + BreakLandingLabel(*s.break_label) + ":;\n";
  }
  return result;
}

auto RenderWhileStmtNode(
    const ScopeView& view, const mir::WhileStmt& s, std::size_t indent)
    -> diag::Result<std::string> {
  const auto& cond_expr = view.Block().exprs.Get(s.condition);
  auto cond_or = RenderConditionAsBool(view, cond_expr);
  if (!cond_or) return std::unexpected(std::move(cond_or.error()));
  const auto& block = view.Block().child_scopes.Get(s.scope);
  auto body_or = RenderNestedBlock(view, block, indent + 1);
  if (!body_or) return std::unexpected(std::move(body_or.error()));
  std::string result =
      Indent(indent) + "while (" + *std::move(cond_or) + ") {\n";
  result += *body_or;
  result += Indent(indent) + "}\n";
  return result;
}

auto RenderDoWhileStmtNode(
    const ScopeView& view, const mir::DoWhileStmt& s, std::size_t indent)
    -> diag::Result<std::string> {
  const auto& cond_expr = view.Block().exprs.Get(s.condition);
  auto cond_or = RenderConditionAsBool(view, cond_expr);
  if (!cond_or) return std::unexpected(std::move(cond_or.error()));
  const auto& block = view.Block().child_scopes.Get(s.scope);
  auto body_or = RenderNestedBlock(view, block, indent + 1);
  if (!body_or) return std::unexpected(std::move(body_or.error()));
  std::string result = Indent(indent) + "do {\n";
  result += *body_or;
  result += Indent(indent) + "} while (" + *std::move(cond_or) + ");\n";
  return result;
}

auto RenderSensitivityWaitStmt(
    const ScopeView& view, const mir::SensitivityWaitStmt& s,
    std::size_t indent) -> diag::Result<std::string> {
  std::string result = Indent(indent) + "co_await lyra::runtime::WaitAny({";
  for (std::size_t i = 0; i < s.reads.size(); ++i) {
    const auto& read = s.reads[i];
    auto ptr_or = RenderExpr(view, view.Expr(read.observable_ptr));
    if (!ptr_or) return std::unexpected(std::move(ptr_or.error()));
    if (i != 0) result += ", ";
    result += "{" + *ptr_or + ", " +
              std::string{RenderEventEdgeAsRuntime(read.edge_kind)} + ", " +
              std::to_string(read.lsb_bit_offset) + "ULL, " +
              std::to_string(read.bit_width) + "ULL}";
  }
  result += "});\n";
  return result;
}

}  // namespace

auto RenderStmt(
    const ScopeView& view, const mir::Stmt& stmt, std::size_t indent)
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
          [&](const mir::LocalDeclStmt& s) -> diag::Result<std::string> {
            return RenderLocalDeclStmt(view, s, indent);
          },
          [&](const mir::ExprStmt& s) -> diag::Result<std::string> {
            return RenderExprStmt(view, s, indent);
          },
          [&](const mir::BlockStmt& s) -> diag::Result<std::string> {
            return RenderBlockStmtNode(view, s, indent);
          },
          [&](const mir::ForkStmt& s) -> diag::Result<std::string> {
            return RenderForkStmtNode(view, s, indent);
          },
          [&](const mir::IfStmt& s) -> diag::Result<std::string> {
            return RenderIfStmtNode(view, s, indent);
          },
          [&](const mir::ConstructOwnedObjectStmt& s)
              -> diag::Result<std::string> {
            return RenderConstructOwnedObjectStmt(view, s, indent);
          },
          [&](const mir::ConstructExternalUnitStmt& s)
              -> diag::Result<std::string> {
            return RenderConstructExternalUnitStmt(view, s, indent);
          },
          [&](const mir::ForStmt& s) -> diag::Result<std::string> {
            return RenderForStmtNode(view, s, indent);
          },
          [&](const mir::WhileStmt& s) -> diag::Result<std::string> {
            return RenderWhileStmtNode(view, s, indent);
          },
          [&](const mir::DoWhileStmt& s) -> diag::Result<std::string> {
            return RenderDoWhileStmtNode(view, s, indent);
          },
          [&](const mir::BreakStmt& s) -> diag::Result<std::string> {
            if (s.target.has_value()) {
              return Indent(indent) + "goto " + BreakLandingLabel(*s.target) +
                     ";\n";
            }
            return Indent(indent) + "break;\n";
          },
          [&](const mir::ContinueStmt&) -> diag::Result<std::string> {
            return Indent(indent) + "continue;\n";
          },
          [&](const mir::ReturnStmt& s) -> diag::Result<std::string> {
            // The `is_coroutine_return` attribute (set at HIR-to-MIR from the
            // enclosing callable's coroutine-ness, mir/stmt.hpp) is a C++
            // render hint -- LIR / LLVM ignore it. A task's `return` is
            // `co_return` and carries no value (LRM 13.3 / 13.4.1).
            if (s.is_coroutine_return) {
              return Indent(indent) + "co_return;\n";
            }
            if (!s.value.has_value()) {
              return Indent(indent) + "return;\n";
            }
            auto value_or = RenderExpr(view, view.Expr(*s.value));
            if (!value_or) return std::unexpected(std::move(value_or.error()));
            return Indent(indent) + "return " + *value_or + ";\n";
          },
          [&](const mir::AwaitStmt& s) -> diag::Result<std::string> {
            return RenderAwaitStmt(view, s, indent);
          },
          [&](const mir::SensitivityWaitStmt& s) -> diag::Result<std::string> {
            return RenderSensitivityWaitStmt(view, s, indent);
          },
      },
      stmt.data);
  if (!rendered_or) return std::unexpected(std::move(rendered_or.error()));
  out += *rendered_or;
  return out;
}

auto RenderBlockStatements(const ScopeView& view, std::size_t indent)
    -> diag::Result<std::string> {
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
    auto rendered_or = RenderStmt(view, block.stmts.Get(sid), indent);
    if (!rendered_or) return std::unexpected(std::move(rendered_or.error()));
    out += *rendered_or;
  }
  return out;
}

auto RenderNestedBlock(
    const ScopeView& parent, const mir::Block& block, std::size_t indent)
    -> diag::Result<std::string> {
  const ScopeView child = parent.WithBlock(block);
  return RenderBlockStatements(child, indent);
}

}  // namespace lyra::backend::cpp
