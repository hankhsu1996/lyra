#include "lyra/mir/dump.hpp"

#include <cstddef>
#include <format>
#include <string>
#include <string_view>
#include <utility>
#include <variant>

#include "lyra/mir/class_decl.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/member.hpp"
#include "lyra/mir/process.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/mir/type.hpp"
#include "lyra/support/internal_error.hpp"
#include "lyra/support/overloaded.hpp"

namespace lyra::mir {

namespace {

class MirDumper {
 public:
  auto Dump(const CompilationUnit& unit) -> std::string {
    for (const auto& c : unit.Classes()) {
      DumpClass(c);
    }
    return std::move(out_);
  }

 private:
  void Line(std::string_view text) {
    out_.append(static_cast<std::size_t>(indent_) * 2, ' ');
    out_.append(text);
    out_.push_back('\n');
  }
  void Indent() {
    ++indent_;
  }
  void Dedent() {
    if (indent_ <= 0) {
      throw support::InternalError("MirDumper: indent underflow");
    }
    --indent_;
  }

  static auto FormatType(const Type& t) -> std::string {
    return std::visit(
        support::Overloaded{
            [](const BuiltinIntType&) -> std::string {
              return "BuiltinIntType";
            },
            [](const BuiltinLogicType&) -> std::string {
              return "BuiltinLogicType";
            },
        },
        t.data);
  }

  static auto FormatExpr(const Body& body, ExprId id) -> std::string {
    const auto& e = body.exprs.at(id.value);
    return std::visit(
        support::Overloaded{
            [](const IntegerLiteral& lit) -> std::string {
              return std::format("IntegerLiteral({})", lit.value);
            },
            [](const MemberRef& r) -> std::string {
              return std::format("MemberRef(Member[{}])", r.target.value);
            },
        },
        e.data);
  }

  static auto FormatProcessKind(const Process& p) -> std::string {
    return std::visit(
        support::Overloaded{
            [](const Initial&) -> std::string { return "Initial"; },
        },
        p.data);
  }

  void DumpClass(const ClassDecl& c) {
    Line(std::format("Class \"{}\"", c.Name()));
    Indent();

    Line("Types:");
    Indent();
    for (std::size_t i = 0; i < c.Types().size(); ++i) {
      Line(std::format("[{}] {}", i, FormatType(c.Types()[i])));
    }
    Dedent();

    Line("Members:");
    Indent();
    for (std::size_t i = 0; i < c.Members().size(); ++i) {
      const auto& m = c.Members()[i];
      Line(std::format("[{}] \"{}\" : Type[{}]", i, m.name, m.type.value));
    }
    Dedent();

    Line("Constructor:");
    Indent();
    DumpBody(c.Constructor());
    Dedent();

    Line("Processes:");
    Indent();
    for (std::size_t i = 0; i < c.Processes().size(); ++i) {
      DumpProcess(c.Processes()[i], i);
    }
    Dedent();

    Dedent();
  }

  void DumpProcess(const Process& p, std::size_t index) {
    Line(std::format("Process[{}] {}", index, FormatProcessKind(p)));
    Indent();
    DumpBody(p.body);
    Dedent();
  }

  void DumpBody(const Body& body) {
    Line(std::format("Body (root_stmts={})", body.root_stmts.size()));
    Indent();
    if (body.root_stmts.empty()) {
      Line("(empty)");
    } else {
      for (const auto& sid : body.root_stmts) {
        DumpStmt(body, sid);
      }
    }
    Dedent();
  }

  void DumpStmt(const Body& enclosing, StmtId id) {
    const auto& stmt = enclosing.stmts.at(id.value);
    if (stmt.label.has_value()) {
      Line(std::format("label: \"{}\"", *stmt.label));
    }
    std::visit(
        support::Overloaded{
            [&](const Assignment& a) { DumpAssignment(a, enclosing, id); },
            [&](const IfStmt& s) { DumpIfStmt(stmt, s, enclosing, id); },
            [&](const SwitchStmt& s) {
              DumpSwitchStmt(stmt, s, enclosing, id);
            },
        },
        stmt.data);
  }

  void DumpAssignment(const Assignment& a, const Body& enclosing, StmtId id) {
    Line(
        std::format(
            "Stmt[{}] Assignment target=Member[{}] value=Expr[{}]", id.value,
            a.target.value, a.value.value));
    Indent();
    Line(
        std::format(
            "Expr[{}] {}", a.value.value, FormatExpr(enclosing, a.value)));
    Dedent();
  }

  void DumpIfStmt(
      const Stmt& parent, const IfStmt& s, const Body& enclosing, StmtId id) {
    Line(
        std::format(
            "Stmt[{}] IfStmt cond=Expr[{}] {}", id.value, s.condition.value,
            FormatExpr(enclosing, s.condition)));
    Indent();
    Line(std::format("then_body (BodyId={}):", s.then_body.value));
    Indent();
    DumpBody(parent.child_bodies.at(s.then_body.value));
    Dedent();
    if (s.else_body.has_value()) {
      Line(std::format("else_body (BodyId={}):", s.else_body->value));
      Indent();
      DumpBody(parent.child_bodies.at(s.else_body->value));
      Dedent();
    } else {
      Line("else_body: <none>");
    }
    Dedent();
  }

  void DumpSwitchStmt(
      const Stmt& parent, const SwitchStmt& s, const Body& enclosing,
      StmtId id) {
    Line(
        std::format(
            "Stmt[{}] SwitchStmt cond=Expr[{}] {}", id.value, s.condition.value,
            FormatExpr(enclosing, s.condition)));
    Indent();
    for (std::size_t ci = 0; ci < s.cases.size(); ++ci) {
      const auto& c = s.cases[ci];
      std::string labels;
      for (std::size_t li = 0; li < c.labels.size(); ++li) {
        if (li != 0) {
          labels += ", ";
        }
        labels += std::format(
            "Expr[{}] {}", c.labels[li].value,
            FormatExpr(enclosing, c.labels[li]));
      }
      Line(
          std::format(
              "case[{}] labels=[{}] body (BodyId={}):", ci, labels,
              c.body.value));
      Indent();
      DumpBody(parent.child_bodies.at(c.body.value));
      Dedent();
    }
    if (s.default_body.has_value()) {
      Line(std::format("default_body (BodyId={}):", s.default_body->value));
      Indent();
      DumpBody(parent.child_bodies.at(s.default_body->value));
      Dedent();
    } else {
      Line("default_body: <none>");
    }
    Dedent();
  }

  std::string out_;
  int indent_ = 0;
};

}  // namespace

auto DumpMir(const CompilationUnit& unit) -> std::string {
  MirDumper dumper;
  return dumper.Dump(unit);
}

}  // namespace lyra::mir
