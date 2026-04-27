#include "lyra/mir/dump.hpp"

#include <cstddef>
#include <cstdint>
#include <format>
#include <string>
#include <string_view>
#include <utility>
#include <variant>
#include <vector>

#include "lyra/mir/binary_op.hpp"
#include "lyra/mir/class_decl.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/local_var.hpp"
#include "lyra/mir/member_var.hpp"
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
    Line("CompilationUnit");
    Indent();
    Line("Types:");
    Indent();
    for (std::size_t i = 0; i < unit.Types().size(); ++i) {
      Line(std::format("[{}] {}", i, FormatType(unit.Types()[i])));
    }
    Dedent();
    Line("Classes:");
    Indent();
    for (const auto& c : unit.Classes()) {
      DumpClass(c);
    }
    Dedent();
    Dedent();
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

  static auto FormatBitAtom(BitAtom a) -> std::string_view {
    switch (a) {
      case BitAtom::kBit:
        return "bit";
      case BitAtom::kLogic:
        return "logic";
      case BitAtom::kReg:
        return "reg";
    }
    throw support::InternalError("MirDumper::FormatBitAtom: unknown BitAtom");
  }

  static auto FormatSignedness(Signedness s) -> std::string_view {
    return s == Signedness::kSigned ? "signed" : "unsigned";
  }

  static auto FormatPackedForm(PackedArrayForm f) -> std::string_view {
    switch (f) {
      case PackedArrayForm::kExplicit:
        return "explicit";
      case PackedArrayForm::kByte:
        return "byte";
      case PackedArrayForm::kShortInt:
        return "shortint";
      case PackedArrayForm::kInt:
        return "int";
      case PackedArrayForm::kLongInt:
        return "longint";
      case PackedArrayForm::kInteger:
        return "integer";
      case PackedArrayForm::kTime:
        return "time";
    }
    throw support::InternalError(
        "MirDumper::FormatPackedForm: unknown PackedArrayForm");
  }

  static auto FormatPackedDims(const std::vector<PackedRange>& dims)
      -> std::string {
    if (dims.empty()) {
      return "[]";
    }
    std::string out;
    for (const auto& d : dims) {
      out += std::format("[{}:{}]", d.left, d.right);
    }
    return out;
  }

  static auto FormatUnpackedDims(const std::vector<UnpackedRange>& dims)
      -> std::string {
    if (dims.empty()) {
      return "[]";
    }
    std::string out;
    for (const auto& d : dims) {
      out += std::format("[{}:{}]", d.left, d.right);
    }
    return out;
  }

  static auto FormatType(const Type& t) -> std::string {
    return std::visit(
        support::Overloaded{
            [](const PackedArrayType& p) -> std::string {
              return std::format(
                  "PackedArray(atom={}, signed={}, dims={}, form={})",
                  FormatBitAtom(p.atom), FormatSignedness(p.signedness),
                  FormatPackedDims(p.dims), FormatPackedForm(p.form));
            },
            [](const UnpackedArrayType& u) -> std::string {
              return std::format(
                  "UnpackedArray(elem=Type[{}], dims={})", u.element_type.value,
                  FormatUnpackedDims(u.dims));
            },
            [](const DynamicArrayType& d) -> std::string {
              return std::format(
                  "DynamicArray(elem=Type[{}])", d.element_type.value);
            },
            [](const QueueType& q) -> std::string {
              if (q.max_bound.has_value()) {
                return std::format(
                    "Queue(elem=Type[{}], max={})", q.element_type.value,
                    *q.max_bound);
              }
              return std::format("Queue(elem=Type[{}])", q.element_type.value);
            },
            [](const AssociativeArrayType& a) -> std::string {
              if (a.key_type.has_value()) {
                return std::format(
                    "AssociativeArray(elem=Type[{}], key=Type[{}])",
                    a.element_type.value, a.key_type->value);
              }
              return std::format(
                  "AssociativeArray(elem=Type[{}], key=wildcard)",
                  a.element_type.value);
            },
            [](const StringType&) -> std::string { return "StringType"; },
            [](const EventType&) -> std::string { return "EventType"; },
            [](const RealType&) -> std::string { return "RealType"; },
            [](const ShortRealType&) -> std::string { return "ShortRealType"; },
            [](const RealTimeType&) -> std::string { return "RealTimeType"; },
            [](const ChandleType&) -> std::string { return "ChandleType"; },
            [](const VoidType&) -> std::string { return "VoidType"; },
        },
        t.data);
  }

  static auto FormatBinaryOp(BinaryOp op) -> std::string {
    switch (op) {
      case BinaryOp::kAdd:
        return "Add";
    }
    throw support::InternalError("MirDumper: unknown BinaryOp");
  }

  static auto FormatLvalue(const Lvalue& l) -> std::string {
    return std::visit(
        support::Overloaded{
            [](const MemberVarRef& r) -> std::string {
              return std::format("MemberVar[{}]", r.target.value);
            },
            [](const LocalVarRef& r) -> std::string {
              return std::format("LocalVar[{}]", r.target.value);
            },
        },
        l);
  }

  static auto FormatExpr(const Body& body, ExprId id) -> std::string {
    const auto& e = body.exprs.at(id.value);
    return std::visit(
        support::Overloaded{
            [](const IntegerLiteral& lit) -> std::string {
              return std::format("IntegerLiteral({})", lit.value);
            },
            [](const MemberVarRef& r) -> std::string {
              return std::format("MemberVarRef(MemberVar[{}])", r.target.value);
            },
            [](const LocalVarRef& r) -> std::string {
              return std::format("LocalVarRef(LocalVar[{}])", r.target.value);
            },
            [](const BinaryExpr& b) -> std::string {
              return std::format(
                  "BinaryExpr op={} lhs=Expr[{}] rhs=Expr[{}] type=Type[{}]",
                  FormatBinaryOp(b.op), b.lhs.value, b.rhs.value, b.type.value);
            },
            [](const AssignExpr& a) -> std::string {
              return std::format(
                  "AssignExpr target={} value=Expr[{}] type=Type[{}]",
                  FormatLvalue(a.target), a.value.value, a.type.value);
            },
        },
        e.data);
  }

  static auto FormatMemberKind(const MemberKind& kind) -> std::string {
    return std::visit(
        support::Overloaded{
            [](const ValueMember& v) -> std::string {
              return std::format("Type[{}]", v.type.value);
            },
            [](const ChildClassMember& c) -> std::string {
              return std::format("Class[{}]", c.target.value);
            },
        },
        kind);
  }

  static auto FormatProcessKind(const Process& p) -> std::string {
    switch (p.kind) {
      case ProcessKind::kInitial:
        return "Initial";
    }
    throw support::InternalError("MirDumper: unknown ProcessKind");
  }

  void DumpClass(const ClassDecl& c) {
    Line(std::format("Class \"{}\"", c.Name()));
    Indent();

    Line("Classes:");
    Indent();
    for (std::size_t i = 0; i < c.Classes().size(); ++i) {
      Line(std::format("[{}]", i));
      Indent();
      DumpClass(c.Classes()[i]);
      Dedent();
    }
    Dedent();

    Line("MemberVars:");
    Indent();
    for (std::size_t i = 0; i < c.MemberVars().size(); ++i) {
      const auto& m = c.MemberVars()[i];
      Line(
          std::format("[{}] \"{}\" : {}", i, m.name, FormatMemberKind(m.kind)));
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
    if (!body.local_vars.empty()) {
      Line("Locals:");
      Indent();
      for (std::size_t i = 0; i < body.local_vars.size(); ++i) {
        const auto& lv = body.local_vars[i];
        Line(
            std::format(
                "LocalVar[{}] \"{}\" : Type[{}]", i, lv.name, lv.type.value));
      }
      Dedent();
    }
    if (!body.exprs.empty()) {
      Line("Exprs:");
      Indent();
      for (std::size_t i = 0; i < body.exprs.size(); ++i) {
        const ExprId id{static_cast<std::uint32_t>(i)};
        Line(std::format("Expr[{}] {}", i, FormatExpr(body, id)));
      }
      Dedent();
    }
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
            [&](const LocalVarDeclStmt& s) {
              Line(
                  std::format(
                      "Stmt[{}] LocalVarDeclStmt local=LocalVar[{}]", id.value,
                      s.local_var.value));
            },
            [&](const ExprStmt& s) { DumpExprStmt(s, enclosing, id); },
            [&](const BlockStmt& s) { DumpBlockStmt(s, enclosing, id); },
            [&](const IfStmt& s) { DumpIfStmt(stmt, s, enclosing, id); },
            [&](const SwitchStmt& s) {
              DumpSwitchStmt(stmt, s, enclosing, id);
            },
            [&](const ConstructMemberStmt& s) {
              Line(
                  std::format(
                      "Stmt[{}] ConstructMemberStmt target=MemberVar[{}] "
                      "class=Class[{}]",
                      id.value, s.target.value, s.class_id.value));
            },
        },
        stmt.data);
  }

  void DumpBlockStmt(const BlockStmt& s, const Body& enclosing, StmtId id) {
    Line(
        std::format(
            "Stmt[{}] BlockStmt (count={})", id.value, s.statements.size()));
    Indent();
    for (const auto child : s.statements) {
      DumpStmt(enclosing, child);
    }
    Dedent();
  }

  void DumpExprStmt(const ExprStmt& s, const Body& enclosing, StmtId id) {
    Line(
        std::format("Stmt[{}] ExprStmt expr=Expr[{}]", id.value, s.expr.value));
    Indent();
    Line(
        std::format(
            "Expr[{}] {}", s.expr.value, FormatExpr(enclosing, s.expr)));
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
