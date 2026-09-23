#include "lyra/backend/cpp/render_stmt.hpp"

#include <variant>

#include "lyra/backend/cpp/naming.hpp"
#include "lyra/backend/cpp/render_expr.hpp"
#include "lyra/backend/cpp/render_type.hpp"
#include "lyra/backend/cpp/scope_view.hpp"
#include "lyra/backend/cpp/target_text.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/mir/stmt.hpp"

namespace lyra::backend::cpp {

namespace {

void RenderForInit(
    const ScopeView& view, const mir::ForInit& init, TargetText& out) {
  std::visit(
      Overloaded{
          [&](const mir::ForInitDecl& d) {
            // The induction variable's C++ type is whatever the initializer
            // yields (every integral value is a PackedArray), so `auto` is the
            // exact same type as spelling it out -- and reads as the idiomatic
            // loop counter.
            Write(
                view, out, "auto ",
                CppLocalName(view.Code().named_locals, d.induction_var), " = ",
                d.init);
          },
          [&](const mir::ForInitExpr& e) { Write(view, out, e.expr); },
      },
      init);
}

void RenderLocalDeclStmt(
    const ScopeView& view, const mir::LocalDeclStmt& s, TargetText& out) {
  const auto& lv = view.Code().locals.Get(s.target);
  out.OpenLine();
  Write(
      view, out, lv.type, " ", CppLocalName(view.Code().named_locals, s.target),
      " = ", s.init, ";\n");
}

void RenderExprStmt(
    const ScopeView& view, const mir::ExprStmt& s, TargetText& out) {
  out.OpenLine();
  Write(view, out, s.expr, ";\n");
}

void RenderBlockStmt(
    const ScopeView& view, const mir::BlockStmt& s, TargetText& out) {
  const auto& child = view.Block().child_scopes.Get(s.scope);
  out.OpenLine();
  out += "{\n";
  out.Indent();
  RenderNestedBlock(view, child, out);
  out.Outdent();
  out.OpenLine();
  out += "}\n";
}

void RenderTryStmt(
    const ScopeView& view, const mir::TryStmt& s, TargetText& out) {
  const auto& body = view.Block().child_scopes.Get(s.body);
  const auto& caught = view.Code().locals.Get(s.caught);
  const auto& handler = view.Block().child_scopes.Get(s.handler);
  out.OpenLine();
  out += "try {\n";
  out.Indent();
  RenderNestedBlock(view, body, out);
  out.Outdent();
  out.OpenLine();
  Write(
      view, out, "} catch (", caught.type, "& ",
      CppLocalName(view.Code().named_locals, s.caught), ") {\n");
  out.Indent();
  RenderNestedBlock(view, handler, out);
  out.Outdent();
  out.OpenLine();
  out += "}\n";
}

void RenderRaiseStmt(
    const ScopeView& view, const mir::RaiseStmt& s, TargetText& out) {
  out.OpenLine();
  Write(view, out, "throw ", s.effect, ";\n");
}

// C++ states an extent's exit through a destructor rather than through a
// construct of its own, so the cleanup becomes a scope-exit object declared
// ahead of the body: it runs however control leaves, including a `return` or a
// `break` a trailing copy of the cleanup would miss.
void RenderFinallyStmt(
    const ScopeView& view, const mir::FinallyStmt& s, TargetText& out) {
  const auto& body = view.Block().child_scopes.Get(s.body);
  const auto& cleanup = view.Block().child_scopes.Get(s.cleanup);
  out.OpenLine();
  out += "{\n";
  out.Indent();
  out.OpenLine();
  Write(
      out, BodyCleanupExtentCppType(), " __lyra_finally_", s.cleanup.value,
      "([&]() {\n");
  out.Indent();
  RenderNestedBlock(view, cleanup, out);
  out.Outdent();
  out.OpenLine();
  out += "});\n";
  RenderNestedBlock(view, body, out);
  out.Outdent();
  out.OpenLine();
  out += "}\n";
}

void RenderIfStmt(
    const ScopeView& view, const mir::IfStmt& s, TargetText& out) {
  const auto& then_scope = view.Block().child_scopes.Get(s.then_scope);
  out.OpenLine();
  Write(view, out, "if (", s.condition, ") {\n");
  out.Indent();
  RenderNestedBlock(view, then_scope, out);
  out.Outdent();
  out.OpenLine();
  out += "}";
  if (s.else_scope.has_value()) {
    const auto& else_scope = view.Block().child_scopes.Get(*s.else_scope);
    out += " else {\n";
    out.Indent();
    RenderNestedBlock(view, else_scope, out);
    out.Outdent();
    out.OpenLine();
    out += "}";
  }
  out += "\n";
}

// C++ has no labeled break, so a `foreach` break that must leave every nested
// dimension lowers to a `goto` aimed at a label after the outermost loop. Both
// the landing label and the jump derive their name from the loop's label.
void WriteBreakLandingLabel(mir::LoopLabelId label, TargetText& out) {
  Write(out, "__lyra_break_", label.value);
}

void RenderForStmt(
    const ScopeView& view, const mir::ForStmt& s, TargetText& out) {
  const auto& block = view.Block().child_scopes.Get(s.scope);
  out.OpenLine();
  out += "for (";
  bool first_init = true;
  for (const mir::ForInit& one : s.init) {
    if (!first_init) out += ", ";
    RenderForInit(view, one, out);
    first_init = false;
  }
  out += "; ";
  if (s.condition.has_value()) {
    Write(view, out, *s.condition);
  }
  out += "; ";
  WriteCommaSeparated(view, out, s.step);
  out += ") {\n";
  out.Indent();
  RenderNestedBlock(view, block, out);
  out.Outdent();
  out.OpenLine();
  out += "}\n";
  if (s.break_label.has_value()) {
    out.OpenLine();
    WriteBreakLandingLabel(*s.break_label, out);
    out += ":;\n";
  }
}

void RenderWhileStmt(
    const ScopeView& view, const mir::WhileStmt& s, TargetText& out) {
  const auto& block = view.Block().child_scopes.Get(s.scope);
  out.OpenLine();
  Write(view, out, "while (", s.condition, ") {\n");
  out.Indent();
  RenderNestedBlock(view, block, out);
  out.Outdent();
  out.OpenLine();
  out += "}\n";
}

void RenderDoWhileStmt(
    const ScopeView& view, const mir::DoWhileStmt& s, TargetText& out) {
  const auto& block = view.Block().child_scopes.Get(s.scope);
  out.OpenLine();
  out += "do {\n";
  out.Indent();
  RenderNestedBlock(view, block, out);
  out.Outdent();
  out.OpenLine();
  Write(view, out, "} while (", s.condition, ");\n");
}

}  // namespace

void RenderStmt(const ScopeView& view, const mir::Stmt& stmt, TargetText& out) {
  if (stmt.label.has_value()) {
    out.OpenLine();
    Write(out, *stmt.label, ":\n");
  }
  std::visit(
      Overloaded{
          [&](const mir::EmptyStmt&) {
            out.OpenLine();
            out += ";\n";
          },
          [&](const mir::LocalDeclStmt& s) {
            RenderLocalDeclStmt(view, s, out);
          },
          [&](const mir::ExprStmt& s) { RenderExprStmt(view, s, out); },
          [&](const mir::BlockStmt& s) { RenderBlockStmt(view, s, out); },
          [&](const mir::TryStmt& s) { RenderTryStmt(view, s, out); },
          [&](const mir::RaiseStmt& s) { RenderRaiseStmt(view, s, out); },
          [&](const mir::FinallyStmt& s) { RenderFinallyStmt(view, s, out); },
          [&](const mir::IfStmt& s) { RenderIfStmt(view, s, out); },
          [&](const mir::ForStmt& s) { RenderForStmt(view, s, out); },
          [&](const mir::WhileStmt& s) { RenderWhileStmt(view, s, out); },
          [&](const mir::DoWhileStmt& s) { RenderDoWhileStmt(view, s, out); },
          [&](const mir::BreakStmt& s) {
            out.OpenLine();
            if (s.target.has_value()) {
              out += "goto ";
              WriteBreakLandingLabel(*s.target, out);
              out += ";\n";
              return;
            }
            out += "break;\n";
          },
          [&](const mir::ContinueStmt&) {
            out.OpenLine();
            out += "continue;\n";
          },
          [&](const mir::ReturnStmt& s) {
            // A coroutine completes through `co_return`, which the enclosing
            // callable's result type states: coroutine-ness is the call
            // protocol, read from the type rather than restated on the
            // statement. A value rides the result either way (LRM 13.3 /
            // 13.4.1).
            const std::string_view keyword =
                view.Unit()
                        .types.Get(view.Code().result_type)
                        .Is<mir::CoroutineType>()
                    ? "co_return"
                    : "return";
            out.OpenLine();
            out += keyword;
            if (!s.value.has_value()) {
              out += ";\n";
              return;
            }
            Write(view, out, " ", *s.value, ";\n");
          },
      },
      stmt.data);
}

void RenderBlockStatements(const ScopeView& view, TargetText& out) {
  const auto& block = view.Block();
  for (const auto& sid : block.root_stmts) {
    RenderStmt(view, block.stmts.Get(sid), out);
  }
}

void RenderNestedBlock(
    const ScopeView& parent, const mir::Block& block, TargetText& out) {
  const ScopeView child = parent.WithBlock(block);
  RenderBlockStatements(child, out);
}

}  // namespace lyra::backend::cpp
