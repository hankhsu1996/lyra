#include "lyra/backend/cpp/render_stmt.hpp"

#include <cstdint>
#include <optional>
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

// C++ has no labeled break, and its `break` leaves a `switch` as well as a
// loop, so a break naming a statement is `goto __lyra_break_<n>;`, aimed at a
// label after that statement. The label and the jump both take the name from
// here.
void WriteBreakLandingLabel(mir::LabelId label, TargetText& out) {
  Write(out, "__lyra_break_", label.value);
}

// The label after a statement a break may name, on a line of its own.
void WriteBreakLanding(std::optional<mir::LabelId> label, TargetText& out) {
  if (!label.has_value()) {
    return;
  }
  out.OpenLine();
  WriteBreakLandingLabel(*label, out);
  out += ":;\n";
}

void RenderBlockStmt(
    const ScopeView& view, const mir::BlockStmt& s, TargetText& out) {
  WriteBracedBlock(view, view.Block().child_scopes.Get(s.scope), out);
  out += "\n";
  WriteBreakLanding(s.break_label, out);
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
  // An `else` holding one statement is written without braces, so an `if` the
  // source nested in the `else` of another opens no brace per link, where a
  // brace per link would soon exceed the brackets a C++ compiler accepts.
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

// Each case's statements are a compound statement of their own, so entering
// one jumps over no declaration of another ([stmt.dcl]), and end in `break`,
// since a case never runs on into the next.
void RenderSwitchStmt(
    const ScopeView& view, const mir::SwitchStmt& s, TargetText& out) {
  Write(view, out, "switch (", s.discriminant, ") {\n");
  out.Indent();
  for (const mir::SwitchCase& selected : s.cases) {
    out.OpenLine();
    for (const std::int64_t value : selected.values) {
      out += "case ";
      WriteInt64Constant(out, value);
      out += ": ";
    }
    WriteBracedBlock(view, view.Block().child_scopes.Get(selected.body), out);
    out += " break;\n";
  }
  if (s.default_body.has_value()) {
    out.OpenLine();
    out += "default: ";
    WriteBracedBlock(view, view.Block().child_scopes.Get(*s.default_body), out);
    out += " break;\n";
  }
  out.Outdent();
  out.OpenLine();
  out += "}\n";
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
  WriteBreakLanding(s.break_label, out);
}

void RenderWhileStmt(
    const ScopeView& view, const mir::WhileStmt& s, TargetText& out) {
  Write(view, out, "while (", s.condition, ") ");
  WriteBracedBlock(view, view.Block().child_scopes.Get(s.scope), out);
  out += "\n";
  WriteBreakLanding(s.break_label, out);
}

void RenderDoWhileStmt(
    const ScopeView& view, const mir::DoWhileStmt& s, TargetText& out) {
  out += "do ";
  WriteBracedBlock(view, view.Block().child_scopes.Get(s.scope), out);
  Write(view, out, " while (", s.condition, ");\n");
  WriteBreakLanding(s.break_label, out);
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
          [&](const mir::SwitchStmt& s) { RenderSwitchStmt(view, s, out); },
          [&](const mir::ForStmt& s) { RenderForStmt(view, s, out); },
          [&](const mir::WhileStmt& s) { RenderWhileStmt(view, s, out); },
          [&](const mir::DoWhileStmt& s) { RenderDoWhileStmt(view, s, out); },
          [&](const mir::BreakStmt& s) {
            out += "goto ";
            WriteBreakLandingLabel(s.target, out);
            out += ";\n";
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
