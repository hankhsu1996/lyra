#include "lyra/mir/dump.hpp"

#include <cstddef>
#include <cstdint>
#include <format>
#include <string>
#include <string_view>
#include <utility>
#include <variant>
#include <vector>

#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/mir/binary_op.hpp"
#include "lyra/mir/class_decl.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/local_var.hpp"
#include "lyra/mir/member_var.hpp"
#include "lyra/mir/process.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/mir/type.hpp"
#include "lyra/support/system_subroutine.hpp"

namespace lyra::mir {

namespace {

class MirDumper {
 public:
  auto Dump(const CompilationUnit& unit) -> std::string {
    Line("CompilationUnit");
    Indent();
    Line("Types:");
    Indent();
    for (std::size_t i = 0; i < unit.types.size(); ++i) {
      Line(std::format("[{}] {}", i, FormatType(unit.types[i])));
    }
    Dedent();
    Line("Classes:");
    Indent();
    for (const auto& c : unit.classes) {
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
      throw InternalError("MirDumper: indent underflow");
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
    throw InternalError("MirDumper::FormatBitAtom: unknown BitAtom");
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
    throw InternalError("MirDumper::FormatPackedForm: unknown PackedArrayForm");
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
        Overloaded{
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
            [](const ObjectType& o) -> std::string {
              return std::format("Object(class=Class[{}])", o.target.value);
            },
            [](const OwningPtrType& p) -> std::string {
              return std::format(
                  "OwningPtr(pointee=Type[{}])", p.pointee.value);
            },
            [](const VectorType& v) -> std::string {
              return std::format("Vector(elem=Type[{}])", v.element.value);
            },
        },
        t.data);
  }

  static auto FormatTimeScale(TimeScale s) -> std::string_view {
    switch (s) {
      case TimeScale::kFs:
        return "fs";
      case TimeScale::kPs:
        return "ps";
      case TimeScale::kNs:
        return "ns";
      case TimeScale::kUs:
        return "us";
      case TimeScale::kMs:
        return "ms";
      case TimeScale::kS:
        return "s";
    }
    throw InternalError("MirDumper::FormatTimeScale: unknown TimeScale");
  }

  static auto FormatTimingControl(const TimingControl& tc) -> std::string {
    return std::visit(
        Overloaded{
            [](const DelayControl& d) -> std::string {
              return std::format("DelayControl ticks={}", d.duration);
            },
        },
        tc);
  }

  static auto FormatBinaryOp(BinaryOp op) -> std::string {
    switch (op) {
      case BinaryOp::kAdd:
        return "Add";
      case BinaryOp::kLessThan:
        return "LessThan";
    }
    throw InternalError("MirDumper: unknown BinaryOp");
  }

  static auto FormatLvalue(const Lvalue& l) -> std::string {
    return std::visit(
        Overloaded{
            [](const MemberVarRef& r) -> std::string {
              return std::format("MemberVar[{}]", r.target.value);
            },
            [](const LocalVarRef& r) -> std::string {
              return std::format(
                  "LocalVar[scope={}, local={}]", r.scope.value, r.local.value);
            },
        },
        l);
  }

  static auto FormatPrintRadix(support::PrintRadix r) -> std::string_view {
    switch (r) {
      case support::PrintRadix::kDecimal:
        return "decimal";
      case support::PrintRadix::kBinary:
        return "binary";
      case support::PrintRadix::kOctal:
        return "octal";
      case support::PrintRadix::kHex:
        return "hex";
    }
    throw InternalError("MirDumper::FormatPrintRadix: unknown PrintRadix");
  }

  static auto FormatPrintSinkKind(support::PrintSinkKind s)
      -> std::string_view {
    switch (s) {
      case support::PrintSinkKind::kStdout:
        return "stdout";
      case support::PrintSinkKind::kFile:
        return "file";
    }
    throw InternalError(
        "MirDumper::FormatPrintSinkKind: unknown PrintSinkKind");
  }

  [[nodiscard]] auto FormatCallee(const Callee& callee) const -> std::string {
    const auto& target = current_class_->GetUserSubroutineTarget(callee);
    return std::format("UserSubroutine[{}] \"{}\"", callee.value, target.name);
  }

  [[nodiscard]] auto FormatExpr(const Body& body, ExprId id) const
      -> std::string {
    const auto& e = body.exprs.at(id.value);
    std::string body_str = std::visit(
        Overloaded{
            [](const IntegerLiteral& lit) -> std::string {
              return std::format("IntegerLiteral({})", lit.value);
            },
            [](const StringLiteral& lit) -> std::string {
              return std::format("StringLiteral(\"{}\")", lit.value);
            },
            [](const TimeLiteral& lit) -> std::string {
              return std::format(
                  "TimeLiteral(value={}, scale={})", lit.value,
                  FormatTimeScale(lit.scale));
            },
            [](const MemberVarRef& r) -> std::string {
              return std::format("MemberVarRef(MemberVar[{}])", r.target.value);
            },
            [](const LocalVarRef& r) -> std::string {
              return std::format(
                  "LocalVarRef(scope={}, local={})", r.scope.value,
                  r.local.value);
            },
            [](const BinaryExpr& b) -> std::string {
              return std::format(
                  "BinaryExpr op={} lhs=Expr[{}] rhs=Expr[{}]",
                  FormatBinaryOp(b.op), b.lhs.value, b.rhs.value);
            },
            [](const AssignExpr& a) -> std::string {
              return std::format(
                  "AssignExpr target={} value=Expr[{}]", FormatLvalue(a.target),
                  a.value.value);
            },
            [this](const CallExpr& c) -> std::string {
              std::string args;
              for (std::size_t i = 0; i < c.arguments.size(); ++i) {
                if (i != 0) {
                  args += ", ";
                }
                args += std::format("Expr[{}]", c.arguments[i].value);
              }
              return std::format(
                  "CallExpr callee={} args=[{}]", FormatCallee(c.callee), args);
            },
            [](const RuntimeCallExpr&) -> std::string {
              return "RuntimeCallExpr";
            },
        },
        e.data);
    return std::format("{} type=Type[{}]", body_str, e.type.value);
  }

  static auto FormatMemberType(TypeId type) -> std::string {
    return std::format("Type[{}]", type.value);
  }

  static auto FormatProcessKind(const Process& p) -> std::string {
    switch (p.kind) {
      case ProcessKind::kInitial:
        return "Initial";
    }
    throw InternalError("MirDumper: unknown ProcessKind");
  }

  void DumpClass(const ClassDecl& c) {
    const ClassDecl* saved = current_class_;
    current_class_ = &c;
    Line(std::format("Class \"{}\"", c.name));
    Indent();

    Line("Classes:");
    Indent();
    for (std::size_t i = 0; i < c.classes.size(); ++i) {
      Line(std::format("[{}]", i));
      Indent();
      DumpClass(c.classes[i]);
      Dedent();
    }
    Dedent();

    Line("MemberVars:");
    Indent();
    for (std::size_t i = 0; i < c.member_vars.size(); ++i) {
      const auto& m = c.member_vars[i];
      Line(
          std::format("[{}] \"{}\" : {}", i, m.name, FormatMemberType(m.type)));
    }
    Dedent();

    Line("UserSubroutineTargets:");
    Indent();
    for (std::size_t i = 0; i < c.user_subroutine_targets.size(); ++i) {
      const auto& t = c.user_subroutine_targets[i];
      Line(std::format("[{}] \"{}\"", i, t.name));
    }
    Dedent();

    Line("Constructor:");
    Indent();
    DumpBody(c.constructor);
    Dedent();

    Line("Processes:");
    Indent();
    for (std::size_t i = 0; i < c.processes.size(); ++i) {
      DumpProcess(c.processes[i], i);
    }
    Dedent();

    Dedent();
    current_class_ = saved;
  }

  void DumpProcess(const Process& p, std::size_t index) {
    Line(std::format("Process[{}] {}", index, FormatProcessKind(p)));
    Indent();
    DumpBody(p.body);
    Dedent();
  }

  void DumpBody(const Body& body) {
    Line(std::format("LocalScopes (root={}):", body.root_scope.value));
    Indent();
    for (std::size_t i = 0; i < body.local_scopes.size(); ++i) {
      const auto& s = body.local_scopes[i];
      const std::string parent_str =
          s.parent.has_value() ? std::format("{}", s.parent->value) : "<none>";
      Line(std::format("Scope[{}] parent={}", i, parent_str));
      Indent();
      for (std::size_t j = 0; j < s.locals.size(); ++j) {
        const auto& lv = s.locals[j];
        Line(
            std::format(
                "LocalVar[{}] \"{}\" : Type[{}]", j, lv.name, lv.type.value));
      }
      Dedent();
    }
    Dedent();
    if (!body.exprs.empty()) {
      Line("Exprs:");
      Indent();
      for (std::size_t i = 0; i < body.exprs.size(); ++i) {
        const ExprId id{static_cast<std::uint32_t>(i)};
        Line(std::format("Expr[{}] {}", i, FormatExpr(body, id)));
        const auto& expr = body.exprs[i];
        if (const auto* rc = std::get_if<RuntimeCallExpr>(&expr.data)) {
          Indent();
          DumpRuntimePrintCallItems(rc->print, body);
          Dedent();
        }
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
        Overloaded{
            [&](const EmptyStmt&) {
              Line(std::format("Stmt[{}] EmptyStmt", id.value));
            },
            [&](const LocalVarDeclStmt& s) {
              Line(
                  std::format(
                      "Stmt[{}] LocalVarDeclStmt target=LocalVar[scope={}, "
                      "local={}]",
                      id.value, s.target.scope.value, s.target.local.value));
            },
            [&](const ExprStmt& s) { DumpExprStmt(s, enclosing, id); },
            [&](const BlockStmt& s) { DumpBlockStmt(s, enclosing, id); },
            [&](const IfStmt& s) { DumpIfStmt(stmt, s, enclosing, id); },
            [&](const SwitchStmt& s) {
              DumpSwitchStmt(stmt, s, enclosing, id);
            },
            [&](const ConstructOwnedObjectStmt& s) {
              Line(
                  std::format(
                      "Stmt[{}] ConstructOwnedObjectStmt target=MemberVar[{}] "
                      "class=Class[{}]",
                      id.value, s.target.value, s.class_id.value));
            },
            [&](const ForStmt& s) { DumpForStmt(stmt, s, enclosing, id); },
            [&](const TimedStmt& t) { DumpTimedStmt(t, enclosing, id); },
        },
        stmt.data);
  }

  void DumpTimedStmt(const TimedStmt& t, const Body& enclosing, StmtId id) {
    Line(std::format("Stmt[{}] TimedStmt", id.value));
    Indent();
    Line(std::format("timing: {}", FormatTimingControl(t.timing)));
    Line("body:");
    Indent();
    DumpStmt(enclosing, t.body);
    Dedent();
    Dedent();
  }

  void DumpRuntimePrintCallItems(
      const RuntimePrintCall& call, const Body& enclosing) {
    Line(
        std::format(
            "RuntimePrintCall kind={} descriptor={} items={}",
            FormatMirPrintKind(call.kind),
            call.descriptor.has_value()
                ? std::format("Expr[{}]", call.descriptor->value)
                : std::string("stdout"),
            call.items.size()));
    Indent();
    for (std::size_t i = 0; i < call.items.size(); ++i) {
      DumpRuntimePrintItem(i, call.items[i], enclosing);
    }
    Dedent();
  }

  void DumpRuntimePrintItem(
      std::size_t i, const RuntimePrintItem& item, const Body& enclosing) {
    std::visit(
        Overloaded{
            [&](const RuntimePrintLiteral& lit) {
              Line(
                  std::format(
                      "Item[{}] Literal {}", i, FormatStringLiteral(lit.text)));
            },
            [&](const RuntimePrintValue& v) {
              Line(
                  std::format(
                      "Item[{}] Value value=Expr[{}] type=Type[{}] "
                      "spec=Format(kind={}, width={}, precision={}, "
                      "zero_pad={}, "
                      "left_align={}, timeunit_power={})",
                      i, v.value.value, v.type.value,
                      FormatMirFormatKind(v.spec.kind), v.spec.modifiers.width,
                      v.spec.modifiers.precision,
                      v.spec.modifiers.zero_pad ? "true" : "false",
                      v.spec.modifiers.left_align ? "true" : "false",
                      v.spec.timeunit_power));
              Indent();
              Line(
                  std::format(
                      "Expr[{}] {}", v.value.value,
                      FormatExpr(enclosing, v.value)));
              Dedent();
            },
        },
        item);
  }

  static auto FormatMirPrintKind(PrintKind k) -> std::string_view {
    switch (k) {
      case PrintKind::kDisplay:
        return "kDisplay";
      case PrintKind::kWrite:
        return "kWrite";
      case PrintKind::kFDisplay:
        return "kFDisplay";
      case PrintKind::kFWrite:
        return "kFWrite";
    }
    throw InternalError("FormatMirPrintKind: unknown PrintKind");
  }

  static auto FormatMirFormatKind(FormatKind k) -> std::string_view {
    switch (k) {
      case FormatKind::kDecimal:
        return "kDecimal";
      case FormatKind::kHex:
        return "kHex";
      case FormatKind::kBinary:
        return "kBinary";
      case FormatKind::kOctal:
        return "kOctal";
      case FormatKind::kString:
        return "kString";
    }
    throw InternalError("FormatMirFormatKind: unknown FormatKind");
  }

  static auto FormatStringLiteral(std::string_view s) -> std::string {
    std::string out;
    out.push_back('"');
    for (char c : s) {
      switch (c) {
        case '\n':
          out += "\\n";
          break;
        case '\t':
          out += "\\t";
          break;
        case '\\':
          out += "\\\\";
          break;
        case '"':
          out += "\\\"";
          break;
        default:
          out.push_back(c);
          break;
      }
    }
    out.push_back('"');
    return out;
  }

  void DumpForStmt(
      const Stmt& parent, const ForStmt& s, const Body& enclosing, StmtId id) {
    Line(
        std::format(
            "Stmt[{}] ForStmt scope=Scope[{}]", id.value, s.scope.value));
    Indent();
    Line("init:");
    Indent();
    for (std::size_t i = 0; i < s.init.size(); ++i) {
      std::visit(
          Overloaded{
              [&](const ForInitDecl& d) {
                std::string init_str;
                if (d.init.has_value()) {
                  init_str = std::format(
                      " = Expr[{}] {}", d.init->value,
                      FormatExpr(enclosing, *d.init));
                }
                Line(
                    std::format(
                        "[{}] decl LocalVar[scope={}, local={}]{}", i,
                        d.local.scope.value, d.local.local.value, init_str));
              },
              [&](const ForInitExpr& e) {
                Line(
                    std::format(
                        "[{}] expr Expr[{}] {}", i, e.expr.value,
                        FormatExpr(enclosing, e.expr)));
              },
          },
          s.init[i]);
    }
    Dedent();
    if (s.condition.has_value()) {
      Line(
          std::format(
              "condition: Expr[{}] {}", s.condition->value,
              FormatExpr(enclosing, *s.condition)));
    } else {
      Line("condition: <none>");
    }
    Line("step:");
    Indent();
    for (std::size_t i = 0; i < s.step.size(); ++i) {
      Line(
          std::format(
              "[{}] Expr[{}] {}", i, s.step[i].value,
              FormatExpr(enclosing, s.step[i])));
    }
    Dedent();
    Line(std::format("body (BodyId={}):", s.body.value));
    Indent();
    DumpBody(parent.child_bodies.at(s.body.value));
    Dedent();
    Dedent();
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
  const ClassDecl* current_class_ = nullptr;
};

}  // namespace

auto DumpMir(const CompilationUnit& unit) -> std::string {
  MirDumper dumper;
  return dumper.Dump(unit);
}

}  // namespace lyra::mir
