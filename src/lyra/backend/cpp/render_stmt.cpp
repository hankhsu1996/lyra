#include "lyra/backend/cpp/render_stmt.hpp"

#include <string_view>
#include <variant>

#include "lyra/backend/cpp/naming.hpp"
#include "lyra/backend/cpp/render_call.hpp"
#include "lyra/backend/cpp/render_expr.hpp"
#include "lyra/backend/cpp/render_type.hpp"
#include "lyra/backend/cpp/scope_view.hpp"
#include "lyra/backend/cpp/target_text.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/support/builtin_fn.hpp"

namespace lyra::backend::cpp {

namespace {

void RenderStmtInPlace(
    const ScopeView& view, const mir::Stmt& stmt, TargetText& out);

// A block in braces, its statements one level in and its closing brace on a
// line of its own, with nothing after that brace: what follows it -- a newline,
// an `else`, a `catch`, a `while` -- is the enclosing statement's.
void WriteBracedBlock(
    const ScopeView& view, const mir::Block& block, TargetText& out) {
  out += "{\n";
  out.Indent();
  RenderNestedBlock(view, block, out);
  out.Outdent();
  out.OpenLine();
  out += "}";
}

void RenderForInit(
    const ScopeView& view, const mir::ForInit& init, TargetText& out) {
  std::visit(
      Overloaded{
          [&](const mir::ForInitDecl& d) {
            // `auto i = init`: the initializer already has the variable's
            // type, so `auto` is exact and reads like an ordinary loop
            // counter.
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
  Write(
      view, out, lv.type, " ", CppLocalName(view.Code().named_locals, s.target),
      " = ", s.init, ";\n");
}

void RenderExprStmt(
    const ScopeView& view, const mir::ExprStmt& s, TargetText& out) {
  Write(view, out, s.expr, ";\n");
}

void RenderBlockStmt(
    const ScopeView& view, const mir::BlockStmt& s, TargetText& out) {
  WriteBracedBlock(view, view.Block().child_scopes.Get(s.scope), out);
  out += "\n";
}

// The region catches whatever unwinds into it, and binds what it caught as the
// control effect it carries, which only the runtime can say: an effect is
// itself, a run-time error becomes the departure it is, and anything else is
// passed on from inside that question before the handler runs.
void RenderTryStmt(
    const ScopeView& view, const mir::TryStmt& s, TargetText& out) {
  const auto& caught = view.Code().locals.Get(s.caught);
  out += "try ";
  WriteBracedBlock(view, view.Block().child_scopes.Get(s.body), out);
  out += " catch (...) {\n";
  out.Indent();
  out.OpenLine();
  Write(
      view, out, caught.type, " ",
      CppLocalName(view.Code().named_locals, s.caught), " = ");
  RenderStructuralCall(
      view, support::BuiltinFn::kReceiveDeparture, caught.type, out);
  out += ";\n";
  RenderNestedBlock(view, view.Block().child_scopes.Get(s.handler), out);
  out.Outdent();
  out.OpenLine();
  out += "}\n";
}

void RenderRaiseStmt(
    const ScopeView& view, const mir::RaiseStmt& s, TargetText& out) {
  Write(view, out, "throw ", s.effect, ";\n");
}

// C++ has no `finally`, so the cleanup becomes an object declared ahead of the
// body whose destructor runs it. It runs however the body is left, a `return`
// or `break` included, which a copy of the cleanup after the body would miss.
void RenderFinallyStmt(
    const ScopeView& view, const mir::FinallyStmt& s, TargetText& out) {
  const auto& body = view.Block().child_scopes.Get(s.body);
  const auto& cleanup = view.Block().child_scopes.Get(s.cleanup);
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
  Write(view, out, "if (", s.condition, ") ");
  WriteBracedBlock(view, view.Block().child_scopes.Get(s.then_scope), out);
  if (!s.else_scope.has_value()) {
    out += "\n";
    return;
  }
  // An `else` holding one statement is written without braces, so an `else
  // if` chain stays flat. With braces, an if-else-if or the items of a case
  // would nest one level per link, and a long enough chain exceeds the nesting
  // a C++ compiler accepts.
  const auto& else_scope = view.Block().child_scopes.Get(*s.else_scope);
  if (else_scope.root_stmts.size() == 1) {
    out += " else ";
    RenderStmtInPlace(
        view.WithBlock(else_scope),
        else_scope.stmts.Get(else_scope.root_stmts.front()), out);
    return;
  }
  out += " else ";
  WriteBracedBlock(view, else_scope, out);
  out += "\n";
}

// C++ has no labeled break, so a `foreach` break that leaves every nested
// dimension is `goto __lyra_break_<n>;`, aimed at a label after the outermost
// loop. The label and the jump both take the name from here.
void WriteBreakLandingLabel(mir::LoopLabelId label, TargetText& out) {
  Write(out, "__lyra_break_", label.value);
}

void RenderForStmt(
    const ScopeView& view, const mir::ForStmt& s, TargetText& out) {
  out += "for (";
  WriteSeparated(out, s.init, ", ", [&](const mir::ForInit& one) {
    RenderForInit(view, one, out);
  });
  out += "; ";
  if (s.condition.has_value()) {
    Write(view, out, *s.condition);
  }
  out += "; ";
  WriteCommaSeparated(view, out, s.step);
  out += ") ";
  WriteBracedBlock(view, view.Block().child_scopes.Get(s.scope), out);
  out += "\n";
  if (s.break_label.has_value()) {
    out.OpenLine();
    WriteBreakLandingLabel(*s.break_label, out);
    out += ":;\n";
  }
}

void RenderWhileStmt(
    const ScopeView& view, const mir::WhileStmt& s, TargetText& out) {
  Write(view, out, "while (", s.condition, ") ");
  WriteBracedBlock(view, view.Block().child_scopes.Get(s.scope), out);
  out += "\n";
}

void RenderDoWhileStmt(
    const ScopeView& view, const mir::DoWhileStmt& s, TargetText& out) {
  out += "do ";
  WriteBracedBlock(view, view.Block().child_scopes.Get(s.scope), out);
  Write(view, out, " while (", s.condition, ");\n");
}

// A statement written from the current position without starting a line:
// after an `else`, or on a line the caller already started.
void RenderStmtInPlace(
    const ScopeView& view, const mir::Stmt& stmt, TargetText& out) {
  if (stmt.label.has_value()) {
    Write(out, *stmt.label, ":\n");
    out.OpenLine();
  }
  std::visit(
      Overloaded{
          [&](const mir::EmptyStmt&) { out += ";\n"; },
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
            if (s.target.has_value()) {
              out += "goto ";
              WriteBreakLandingLabel(*s.target, out);
              out += ";\n";
              return;
            }
            out += "break;\n";
          },
          [&](const mir::ContinueStmt&) { out += "continue;\n"; },
          [&](const mir::ReturnStmt& s) {
            // `co_return` in a coroutine, `return` otherwise; the function's
            // result type says which it is (LRM 13.3, 13.4.1).
            const std::string_view keyword =
                view.Unit()
                        .types.Get(view.Code().result_type)
                        .Is<mir::CoroutineType>()
                    ? "co_return"
                    : "return";
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

}  // namespace

void RenderStmt(const ScopeView& view, const mir::Stmt& stmt, TargetText& out) {
  out.OpenLine();
  RenderStmtInPlace(view, stmt, out);
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
