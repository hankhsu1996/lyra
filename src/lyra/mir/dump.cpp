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
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/procedural_var.hpp"
#include "lyra/mir/process.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/mir/structural_scope.hpp"
#include "lyra/mir/structural_var.hpp"
#include "lyra/mir/type.hpp"
#include "lyra/mir/unary_op.hpp"

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
    Line("StructuralScope:");
    Indent();
    DumpStructuralScope(unit.structural_scope);
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
              return std::format(
                  "Object(scope=ChildStructuralScope[{}])", o.target.value);
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

  static auto FormatUnaryOp(UnaryOp op) -> std::string {
    switch (op) {
      case UnaryOp::kPlus:
        return "Plus";
      case UnaryOp::kMinus:
        return "Minus";
      case UnaryOp::kBitwiseNot:
        return "BitwiseNot";
      case UnaryOp::kLogicalNot:
        return "LogicalNot";
      case UnaryOp::kReductionAnd:
        return "ReductionAnd";
      case UnaryOp::kReductionOr:
        return "ReductionOr";
      case UnaryOp::kReductionXor:
        return "ReductionXor";
      case UnaryOp::kReductionNand:
        return "ReductionNand";
      case UnaryOp::kReductionNor:
        return "ReductionNor";
      case UnaryOp::kReductionXnor:
        return "ReductionXnor";
    }
    throw InternalError("MirDumper: unknown UnaryOp");
  }

  static auto FormatBinaryOp(BinaryOp op) -> std::string {
    switch (op) {
      case BinaryOp::kAdd:
        return "Add";
      case BinaryOp::kSub:
        return "Sub";
      case BinaryOp::kMul:
        return "Mul";
      case BinaryOp::kDiv:
        return "Div";
      case BinaryOp::kMod:
        return "Mod";
      case BinaryOp::kPower:
        return "Power";
      case BinaryOp::kBitwiseAnd:
        return "BitwiseAnd";
      case BinaryOp::kBitwiseOr:
        return "BitwiseOr";
      case BinaryOp::kBitwiseXor:
        return "BitwiseXor";
      case BinaryOp::kBitwiseXnor:
        return "BitwiseXnor";
      case BinaryOp::kEquality:
        return "Equality";
      case BinaryOp::kInequality:
        return "Inequality";
      case BinaryOp::kCaseEquality:
        return "CaseEquality";
      case BinaryOp::kCaseInequality:
        return "CaseInequality";
      case BinaryOp::kWildcardEquality:
        return "WildcardEquality";
      case BinaryOp::kWildcardInequality:
        return "WildcardInequality";
      case BinaryOp::kGreaterEqual:
        return "GreaterEqual";
      case BinaryOp::kGreaterThan:
        return "GreaterThan";
      case BinaryOp::kLessEqual:
        return "LessEqual";
      case BinaryOp::kLessThan:
        return "LessThan";
      case BinaryOp::kLogicalAnd:
        return "LogicalAnd";
      case BinaryOp::kLogicalOr:
        return "LogicalOr";
      case BinaryOp::kLogicalImplication:
        return "LogicalImplication";
      case BinaryOp::kLogicalEquivalence:
        return "LogicalEquivalence";
      case BinaryOp::kShiftLeft:
        return "ShiftLeft";
      case BinaryOp::kLogicalShiftRight:
        return "LogicalShiftRight";
      case BinaryOp::kArithmeticShiftRight:
        return "ArithmeticShiftRight";
    }
    throw InternalError("MirDumper: unknown BinaryOp");
  }

  static auto FormatLvalue(const Lvalue& l) -> std::string {
    return std::visit(
        Overloaded{
            [](const StructuralVarRef& r) -> std::string {
              return std::format(
                  "StructuralVarRef[hops={}, var={}]", r.hops.value,
                  r.var.value);
            },
            [](const ProceduralVarRef& r) -> std::string {
              return std::format(
                  "ProceduralVarRef[hops={}, var={}]", r.hops.value,
                  r.var.value);
            },
        },
        l);
  }

  [[nodiscard]] auto FormatCallee(const Callee& callee) const -> std::string {
    return std::visit(
        Overloaded{
            [](const SystemSubroutineCallee& s) -> std::string {
              return std::format("SystemSubroutineCallee[id={}]", s.id.value);
            },
            [this](const StructuralSubroutineRef& r) -> std::string {
              const auto& owner = ResolveScopeAtHops(r.hops.value);
              const auto& target = owner.GetStructuralSubroutine(r.subroutine);
              return std::format(
                  "StructuralSubroutineRef[hops={}, subroutine={}] \"{}\"",
                  r.hops.value, r.subroutine.value, target.name);
            },
        },
        callee);
  }

  [[nodiscard]] auto ResolveScopeAtHops(std::uint32_t hops) const
      -> const StructuralScope& {
    if (hops >= scope_stack_.size()) {
      throw InternalError("MirDumper::ResolveScopeAtHops: hops out of range");
    }
    return *scope_stack_[scope_stack_.size() - 1 - hops];
  }

  static auto FormatIntegralConstant(const IntegralConstant& c) -> std::string {
    std::string out = std::format(
        "{}'{}", c.width, c.signedness == Signedness::kSigned ? 's' : 'u');
    if (c.state_kind == IntegralStateKind::kFourState) {
      out += "(four-state)";
    }
    out += "{value=";
    for (std::size_t i = 0; i < c.value_words.size(); ++i) {
      if (i != 0) out += ',';
      out += std::format("0x{:x}", c.value_words[i]);
    }
    if (c.state_kind == IntegralStateKind::kFourState) {
      out += ", state=";
      for (std::size_t i = 0; i < c.state_words.size(); ++i) {
        if (i != 0) out += ',';
        out += std::format("0x{:x}", c.state_words[i]);
      }
    }
    out += "}";
    return out;
  }

  static auto FormatConversionKind(ConversionKind k) -> std::string_view {
    switch (k) {
      case ConversionKind::kImplicit:
        return "implicit";
      case ConversionKind::kPropagated:
        return "propagated";
      case ConversionKind::kStreamingConcat:
        return "streaming-concat";
      case ConversionKind::kExplicit:
        return "explicit";
      case ConversionKind::kBitstreamCast:
        return "bitstream-cast";
    }
    throw InternalError("MirDumper::FormatConversionKind: unknown kind");
  }

  [[nodiscard]] auto FormatExpr(const ProceduralScope& scope, ExprId id) const
      -> std::string {
    const auto& e = scope.exprs.at(id.value);
    std::string formatted = std::visit(
        Overloaded{
            [](const IntegerLiteral& lit) -> std::string {
              return std::format(
                  "IntegerLiteral({})", FormatIntegralConstant(lit.value));
            },
            [](const StringLiteral& lit) -> std::string {
              return std::format("StringLiteral(\"{}\")", lit.value);
            },
            [](const TimeLiteral& lit) -> std::string {
              return std::format(
                  "TimeLiteral(value={}, scale={})", lit.value,
                  FormatTimeScale(lit.scale));
            },
            [](const StructuralVarRef& r) -> std::string {
              return std::format(
                  "StructuralVarRef[hops={}, var={}]", r.hops.value,
                  r.var.value);
            },
            [](const ProceduralVarRef& r) -> std::string {
              return std::format(
                  "ProceduralVarRef[hops={}, var={}]", r.hops.value,
                  r.var.value);
            },
            [](const UnaryExpr& u) -> std::string {
              return std::format(
                  "UnaryExpr op={} operand=Expr[{}]", FormatUnaryOp(u.op),
                  u.operand.value);
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
            [](const ConversionExpr& cv) -> std::string {
              return std::format(
                  "ConversionExpr kind={} operand=Expr[{}]",
                  FormatConversionKind(cv.kind), cv.operand.value);
            },
        },
        e.data);
    return std::format("{} type=Type[{}]", formatted, e.type.value);
  }

  static auto FormatVarType(TypeId type) -> std::string {
    return std::format("Type[{}]", type.value);
  }

  static auto FormatProcessKind(const Process& p) -> std::string {
    switch (p.kind) {
      case ProcessKind::kInitial:
        return "Initial";
      case ProcessKind::kFinal:
        return "Final";
      case ProcessKind::kAlways:
        return "Always";
      case ProcessKind::kAlwaysComb:
        return "AlwaysComb";
      case ProcessKind::kAlwaysLatch:
        return "AlwaysLatch";
      case ProcessKind::kAlwaysFf:
        return "AlwaysFf";
    }
    throw InternalError("MirDumper: unknown ProcessKind");
  }

  static auto FormatAwaitKind(AwaitKind k) -> std::string_view {
    switch (k) {
      case AwaitKind::kAlwaysBackedge:
        return "AlwaysBackedge";
    }
    throw InternalError("MirDumper: unknown AwaitKind");
  }

  void DumpStructuralScope(const StructuralScope& s) {
    scope_stack_.push_back(&s);
    Line(std::format("StructuralScope \"{}\"", s.name));
    Indent();

    Line("ChildStructuralScopes:");
    Indent();
    for (std::size_t i = 0; i < s.child_structural_scopes.size(); ++i) {
      Line(std::format("[{}]", i));
      Indent();
      DumpStructuralScope(s.child_structural_scopes[i]);
      Dedent();
    }
    Dedent();

    Line("Vars:");
    Indent();
    for (std::size_t i = 0; i < s.structural_vars.size(); ++i) {
      const auto& v = s.structural_vars[i];
      Line(std::format("[{}] \"{}\" : {}", i, v.name, FormatVarType(v.type)));
    }
    Dedent();

    Line("StructuralSubroutines:");
    Indent();
    for (std::size_t i = 0; i < s.structural_subroutines.size(); ++i) {
      const auto& d = s.structural_subroutines[i];
      Line(std::format("[{}] \"{}\"", i, d.name));
    }
    Dedent();

    Line("Constructor:");
    Indent();
    DumpProceduralScope(s.constructor_scope);
    Dedent();

    Line("Processes:");
    Indent();
    for (std::size_t i = 0; i < s.processes.size(); ++i) {
      DumpProcess(s.processes[i], i);
    }
    Dedent();

    Dedent();
    scope_stack_.pop_back();
  }

  void DumpProcess(const Process& p, std::size_t index) {
    Line(std::format("Process[{}] {}", index, FormatProcessKind(p)));
    Indent();
    DumpProceduralScope(p.root_procedural_scope);
    Dedent();
  }

  void DumpProceduralScope(const ProceduralScope& scope) {
    if (!scope.vars.empty()) {
      Line("Vars:");
      Indent();
      for (std::size_t i = 0; i < scope.vars.size(); ++i) {
        const auto& v = scope.vars[i];
        Line(
            std::format(
                "ProceduralVar[{}] \"{}\" : Type[{}]", i, v.name,
                v.type.value));
      }
      Dedent();
    }
    if (!scope.exprs.empty()) {
      Line("Exprs:");
      Indent();
      for (std::size_t i = 0; i < scope.exprs.size(); ++i) {
        const ExprId id{static_cast<std::uint32_t>(i)};
        Line(std::format("Expr[{}] {}", i, FormatExpr(scope, id)));
        const auto& expr = scope.exprs[i];
        if (const auto* rc = std::get_if<RuntimeCallExpr>(&expr.data)) {
          Indent();
          DumpRuntimePrintCallItems(rc->print, scope);
          Dedent();
        }
      }
      Dedent();
    }
    Line(
        std::format(
            "ProceduralScope (root_stmts={})", scope.root_stmts.size()));
    Indent();
    if (scope.root_stmts.empty()) {
      Line("(empty)");
    } else {
      for (const auto& sid : scope.root_stmts) {
        DumpStmt(scope, sid);
      }
    }
    Dedent();
  }

  void DumpStmt(const ProceduralScope& enclosing, StmtId id) {
    const auto& stmt = enclosing.stmts.at(id.value);
    if (stmt.label.has_value()) {
      Line(std::format("label: \"{}\"", *stmt.label));
    }
    std::visit(
        Overloaded{
            [&](const EmptyStmt&) {
              Line(std::format("Stmt[{}] EmptyStmt", id.value));
            },
            [&](const ProceduralVarDeclStmt& s) {
              Line(
                  std::format(
                      "Stmt[{}] ProceduralVarDeclStmt "
                      "target=ProceduralVarRef[hops={}, var={}]",
                      id.value, s.target.hops.value, s.target.var.value));
            },
            [&](const ExprStmt& s) { DumpExprStmt(s, enclosing, id); },
            [&](const BlockStmt& s) { DumpBlockStmt(stmt, s, id); },
            [&](const IfStmt& s) { DumpIfStmt(stmt, s, enclosing, id); },
            [&](const SwitchStmt& s) {
              DumpSwitchStmt(stmt, s, enclosing, id);
            },
            [&](const ConstructOwnedObjectStmt& s) {
              Line(
                  std::format(
                      "Stmt[{}] ConstructOwnedObjectStmt "
                      "target=StructuralVar[{}] "
                      "scope=StructuralScope[{}]",
                      id.value, s.target.value, s.scope_id.value));
            },
            [&](const ForStmt& s) { DumpForStmt(stmt, s, enclosing, id); },
            [&](const TimedStmt& t) { DumpTimedStmt(t, enclosing, id); },
            [&](const WhileStmt& s) { DumpWhileStmt(stmt, s, enclosing, id); },
            [&](const AwaitStmt& s) {
              Line(
                  std::format(
                      "Stmt[{}] AwaitStmt kind={}", id.value,
                      FormatAwaitKind(s.kind)));
            },
        },
        stmt.data);
  }

  void DumpWhileStmt(
      const Stmt& parent, const WhileStmt& s, const ProceduralScope& enclosing,
      StmtId id) {
    Line(std::format("Stmt[{}] WhileStmt", id.value));
    Indent();
    Line(
        std::format(
            "condition: Expr[{}] {}", s.condition.value,
            FormatExpr(enclosing, s.condition)));
    Line(std::format("scope (ProceduralScopeId={}):", s.scope.value));
    Indent();
    DumpProceduralScope(parent.child_procedural_scopes.at(s.scope.value));
    Dedent();
    Dedent();
  }

  void DumpTimedStmt(
      const TimedStmt& t, const ProceduralScope& enclosing, StmtId id) {
    Line(std::format("Stmt[{}] TimedStmt", id.value));
    Indent();
    Line(std::format("timing: {}", FormatTimingControl(t.timing)));
    Line("stmt:");
    Indent();
    DumpStmt(enclosing, t.stmt);
    Dedent();
    Dedent();
  }

  void DumpRuntimePrintCallItems(
      const RuntimePrintCall& call, const ProceduralScope& enclosing) {
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
      std::size_t i, const RuntimePrintItem& item,
      const ProceduralScope& enclosing) {
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
      const Stmt& parent, const ForStmt& s, const ProceduralScope& enclosing,
      StmtId id) {
    Line(std::format("Stmt[{}] ForStmt", id.value));
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
                        "[{}] decl ProceduralVarRef[hops={}, var={}]{}", i,
                        d.local.hops.value, d.local.var.value, init_str));
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
    Line(std::format("scope (ProceduralScopeId={}):", s.scope.value));
    Indent();
    DumpProceduralScope(parent.child_procedural_scopes.at(s.scope.value));
    Dedent();
    Dedent();
  }

  void DumpBlockStmt(const Stmt& parent, const BlockStmt& s, StmtId id) {
    Line(
        std::format(
            "Stmt[{}] BlockStmt scope=ProceduralScopeId{{{}}}", id.value,
            s.scope.value));
    Indent();
    DumpProceduralScope(parent.child_procedural_scopes.at(s.scope.value));
    Dedent();
  }

  void DumpExprStmt(
      const ExprStmt& s, const ProceduralScope& enclosing, StmtId id) {
    Line(
        std::format("Stmt[{}] ExprStmt expr=Expr[{}]", id.value, s.expr.value));
    Indent();
    Line(
        std::format(
            "Expr[{}] {}", s.expr.value, FormatExpr(enclosing, s.expr)));
    Dedent();
  }

  void DumpIfStmt(
      const Stmt& parent, const IfStmt& s, const ProceduralScope& enclosing,
      StmtId id) {
    Line(
        std::format(
            "Stmt[{}] IfStmt cond=Expr[{}] {}", id.value, s.condition.value,
            FormatExpr(enclosing, s.condition)));
    Indent();
    Line(std::format("then_scope (ProceduralScopeId={}):", s.then_scope.value));
    Indent();
    DumpProceduralScope(parent.child_procedural_scopes.at(s.then_scope.value));
    Dedent();
    if (s.else_scope.has_value()) {
      Line(
          std::format(
              "else_scope (ProceduralScopeId={}):", s.else_scope->value));
      Indent();
      DumpProceduralScope(
          parent.child_procedural_scopes.at(s.else_scope->value));
      Dedent();
    } else {
      Line("else_scope: <none>");
    }
    Dedent();
  }

  void DumpSwitchStmt(
      const Stmt& parent, const SwitchStmt& s, const ProceduralScope& enclosing,
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
              "case[{}] labels=[{}] scope (ProceduralScopeId={}):", ci, labels,
              c.scope.value));
      Indent();
      DumpProceduralScope(parent.child_procedural_scopes.at(c.scope.value));
      Dedent();
    }
    if (s.default_scope.has_value()) {
      Line(
          std::format(
              "default_scope (ProceduralScopeId={}):", s.default_scope->value));
      Indent();
      DumpProceduralScope(
          parent.child_procedural_scopes.at(s.default_scope->value));
      Dedent();
    } else {
      Line("default_scope: <none>");
    }
    Dedent();
  }

  std::string out_;
  int indent_ = 0;
  std::vector<const StructuralScope*> scope_stack_;
};

}  // namespace

auto DumpMir(const CompilationUnit& unit) -> std::string {
  MirDumper dumper;
  return dumper.Dump(unit);
}

}  // namespace lyra::mir
