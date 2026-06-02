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
#include "lyra/mir/closure.hpp"
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

  static auto FormatType(const Type& t) -> std::string {
    return std::visit(
        Overloaded{
            [](const PackedArrayType& p) -> std::string {
              return std::format(
                  "PackedArray(atom={}, signed={}, dims={}, form={})",
                  FormatBitAtom(p.atom), FormatSignedness(p.signedness),
                  FormatPackedDims(p.dims), FormatPackedForm(p.form));
            },
            [](const EnumType& e) -> std::string {
              std::string members;
              for (std::size_t i = 0; i < e.members.size(); ++i) {
                if (i > 0) members += ", ";
                members +=
                    std::format("{}={}", e.members[i].name, e.members[i].value);
              }
              return std::format(
                  "Enum(base=PackedArray(atom={}, signed={}, dims={}, "
                  "form={}), members=[{}])",
                  FormatBitAtom(e.base.atom),
                  FormatSignedness(e.base.signedness),
                  FormatPackedDims(e.base.dims), FormatPackedForm(e.base.form),
                  members);
            },
            [](const UnpackedArrayType& u) -> std::string {
              return std::format(
                  "UnpackedArray(elem=Type[{}], size={})", u.element_type.value,
                  u.size);
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
            [](const ExternalUnitObjectType& e) -> std::string {
              return std::format(
                  "ExternalUnitObject(unit=\"{}\")", e.unit_name);
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

  static auto FormatEventEdge(EventEdge edge) -> std::string_view {
    switch (edge) {
      case EventEdge::kAnyChange:
        return "any";
      case EventEdge::kPosedge:
        return "posedge";
      case EventEdge::kNegedge:
        return "negedge";
      case EventEdge::kBothEdges:
        return "edge";
    }
    throw InternalError("MirDumper::FormatEventEdge: unknown EventEdge");
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
      case BinaryOp::kCasezEquality:
        return "CasezEquality";
      case BinaryOp::kCasexEquality:
        return "CasexEquality";
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
            [](const BuiltinMethodCallee& b) -> std::string {
              return std::visit(
                  Overloaded{
                      [](const EnumMethodInfo& m) -> std::string {
                        return std::format(
                            "EnumMethodInfo[enum_type=Type[{}], kind={}]",
                            m.enum_type.value, static_cast<int>(m.kind));
                      },
                      [](const StringMethodInfo& m) -> std::string {
                        return std::format(
                            "StringMethodInfo[kind={}]",
                            static_cast<int>(m.kind));
                      },
                      [](const EventMethodInfo& m) -> std::string {
                        return std::format(
                            "EventMethodInfo[kind={}]",
                            static_cast<int>(m.kind));
                      },
                  },
                  b.method);
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

  [[nodiscard]] auto ResolveProceduralScopeAtHops(std::uint32_t hops) const
      -> const ProceduralScope& {
    if (hops >= procedural_scope_stack_.size()) {
      throw InternalError(
          "MirDumper::ResolveProceduralScopeAtHops: hops out of range");
    }
    return *procedural_scope_stack_[procedural_scope_stack_.size() - 1 - hops];
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
            [](const RealLiteral& lit) -> std::string {
              return std::format("RealLiteral({})", lit.value);
            },
            [this](const StructuralParamRef& r) -> std::string {
              const auto& owner = ResolveScopeAtHops(r.hops.value);
              const auto& param = owner.GetStructuralParam(r.param);
              return std::format(
                  "StructuralParamRef[hops={}, param={}] \"{}\"", r.hops.value,
                  r.param.value, param.name);
            },
            [this](const StructuralVarRef& r) -> std::string {
              const auto& owner = ResolveScopeAtHops(r.hops.value);
              const auto& var = owner.GetStructuralVar(r.var);
              return std::format(
                  "StructuralVarRef[hops={}, var={}] \"{}\"", r.hops.value,
                  r.var.value, var.name);
            },
            [this](const ProceduralVarRef& r) -> std::string {
              const auto& owner = ResolveProceduralScopeAtHops(r.hops.value);
              const auto& var = owner.vars.at(r.var.value);
              return std::format(
                  "ProceduralVarRef[hops={}, var={}] \"{}\"", r.hops.value,
                  r.var.value, var.name);
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
            [](const ConditionalExpr& c) -> std::string {
              return std::format(
                  "ConditionalExpr cond=Expr[{}] then=Expr[{}] else=Expr[{}]",
                  c.condition.value, c.then_value.value, c.else_value.value);
            },
            [](const AssignExpr& a) -> std::string {
              const std::string op_str =
                  a.compound_op.has_value()
                      ? std::format(" op={}", FormatBinaryOp(*a.compound_op))
                      : std::string{};
              return std::format(
                  "AssignExpr target=Expr[{}]{} value=Expr[{}]", a.target.value,
                  op_str, a.value.value);
            },
            [](const IncDecExpr& inc) -> std::string {
              const auto* op_str = [&] {
                switch (inc.op) {
                  case IncDecOp::kPreInc:
                    return "PreInc";
                  case IncDecOp::kPostInc:
                    return "PostInc";
                  case IncDecOp::kPreDec:
                    return "PreDec";
                  case IncDecOp::kPostDec:
                    return "PostDec";
                }
                return "?";
              }();
              return std::format(
                  "IncDecExpr op={} target=Expr[{}]", op_str, inc.target.value);
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
            [](const ClosureExpr& cl) -> std::string {
              return std::format("ClosureExpr captures={}", cl.captures.size());
            },
            [](const ElementSelectExpr& sel) -> std::string {
              return std::format(
                  "ElementSelectExpr base=Expr[{}] index=Expr[{}]",
                  sel.base_value.value, sel.index.value);
            },
            [](const RangeSelectExpr& sel) -> std::string {
              return std::format(
                  "RangeSelectExpr base=Expr[{}] offset=Expr[{}] count={}",
                  sel.base_value.value, sel.offset_expr.value, sel.count);
            },
            [](const ConcatExpr& c) -> std::string {
              std::string operands;
              for (std::size_t i = 0; i < c.operands.size(); ++i) {
                if (i != 0) {
                  operands += ", ";
                }
                operands += std::format("Expr[{}]", c.operands[i].value);
              }
              return std::format("ConcatExpr operands=[{}]", operands);
            },
            [](const ReplicationExpr& r) -> std::string {
              return std::format(
                  "ReplicationExpr count=Expr[{}] concat=Expr[{}]",
                  r.count.value, r.concat.value);
            },
            [](const ArrayLiteralExpr& a) -> std::string {
              std::string elements;
              for (std::size_t i = 0; i < a.elements.size(); ++i) {
                if (i != 0) {
                  elements += ", ";
                }
                elements += std::format("Expr[{}]", a.elements[i].value);
              }
              return std::format("ArrayLiteralExpr elements=[{}]", elements);
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
    }
    throw InternalError("MirDumper: unknown ProcessKind");
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

    if (!s.structural_params.empty()) {
      Line("StructuralParams:");
      Indent();
      for (std::size_t i = 0; i < s.structural_params.size(); ++i) {
        const auto& p = s.structural_params[i];
        Line(std::format("[{}] \"{}\" : {}", i, p.name, FormatVarType(p.type)));
      }
      Dedent();
    }

    Line("Vars:");
    Indent();
    for (std::size_t i = 0; i < s.structural_vars.size(); ++i) {
      const auto& v = s.structural_vars[i];
      Line(
          std::format(
              "[{}] \"{}\" : {} init=Expr[{}]", i, v.name,
              FormatVarType(v.type), v.initializer.value));
    }
    Dedent();

    Line("StructuralSubroutines:");
    Indent();
    for (std::size_t i = 0; i < s.structural_subroutines.size(); ++i) {
      DumpStructuralSubroutine(s.structural_subroutines[i], i);
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

  [[nodiscard]] static auto FormatParamDirection(ParamDirection dir)
      -> std::string_view {
    switch (dir) {
      case ParamDirection::kInput:
        return "input";
      case ParamDirection::kOutput:
        return "output";
      case ParamDirection::kInOut:
        return "inout";
      case ParamDirection::kRef:
        return "ref";
      case ParamDirection::kConstRef:
        return "const ref";
    }
    throw InternalError("FormatParamDirection: unknown mir::ParamDirection");
  }

  void DumpStructuralSubroutine(
      const StructuralSubroutineDecl& d, std::size_t index) {
    Line(
        std::format(
            "[{}] \"{}\" : Type[{}]", index, d.name, d.result_type.value));
    Indent();
    for (std::size_t i = 0; i < d.params.size(); ++i) {
      const auto& param = d.params[i];
      Line(
          std::format(
              "Param[{}] {} \"{}\" : Type[{}]", i,
              FormatParamDirection(param.direction), param.name,
              param.type.value));
    }
    for (std::size_t i = 0; i < d.static_locals.size(); ++i) {
      const auto& sl = d.static_locals[i];
      Line(
          std::format(
              "StaticLocal[{}] var=ProceduralVar[{}] init=Expr[{}]", i,
              sl.var.value, sl.init.value));
    }
    DumpProceduralScope(d.root_procedural_scope);
    Dedent();
  }

  void DumpProceduralScope(const ProceduralScope& scope) {
    procedural_scope_stack_.push_back(&scope);
    if (!scope.vars.empty()) {
      Line("Vars:");
      Indent();
      for (std::size_t i = 0; i < scope.vars.size(); ++i) {
        const auto& v = scope.vars[i];
        Line(
            std::format(
                "ProceduralVar[{}] \"{}\" : Type[{}]{}", i, v.name,
                v.type.value,
                v.lifetime == VariableLifetime::kStatic ? " static" : ""));
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
          std::visit(
              Overloaded{
                  [&](const RuntimePrintCall& pc) {
                    DumpRuntimePrintCallItems(pc, scope);
                  },
                  [&](const RuntimeDiagnosticCall& dc) {
                    DumpRuntimeDiagnosticCallItems(dc, scope);
                  },
                  [&](const RuntimeFinishCall& fc) {
                    Line(std::format("RuntimeFinishCall level={}", fc.level));
                  },
                  [&](const RuntimeSubmitObservedCall& sc) {
                    Line(
                        std::format(
                            "RuntimeSubmitObservedCall site={} "
                            "closure=Expr[{}]",
                            sc.site_id.value, sc.closure.value));
                  },
                  [&](const RuntimeSubmitNbaCall& nc) {
                    Line(
                        std::format(
                            "RuntimeSubmitNbaCall closure=Expr[{}]",
                            nc.closure.value));
                  },
                  [&](const RuntimeFileOpenCall& fo) {
                    Line(
                        std::format(
                            "RuntimeFileOpenCall name=Expr[{}] mode={}",
                            fo.name.value,
                            fo.mode.has_value()
                                ? std::format("Expr[{}]", fo.mode->value)
                                : std::string("mcd")));
                  },
                  [&](const RuntimeFileCloseCall& fc) {
                    Line(
                        std::format(
                            "RuntimeFileCloseCall descriptor=Expr[{}]",
                            fc.descriptor.value));
                  },
              },
              rc->call);
          Dedent();
        }
        if (const auto* cl = std::get_if<ClosureExpr>(&expr.data)) {
          Indent();
          DumpClosureExpr(*cl, scope);
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
    procedural_scope_stack_.pop_back();
  }

  void DumpProceduralVarDeclStmt(
      const ProceduralScope& enclosing, StmtId id,
      const ProceduralVarDeclStmt& s) {
    const auto& owner = ResolveProceduralScopeAtHops(s.target.hops.value);
    const auto& var = owner.vars.at(s.target.var.value);
    Line(
        std::format(
            "Stmt[{}] ProceduralVarDeclStmt "
            "target=ProceduralVarRef[hops={}, var={}] \"{}\"",
            id.value, s.target.hops.value, s.target.var.value, var.name));
    Indent();
    Line(
        std::format(
            "init: Expr[{}] {}", s.init.value, FormatExpr(enclosing, s.init)));
    Dedent();
  }

  void DumpConstructOwnedObjectStmt(
      StmtId id, const ConstructOwnedObjectStmt& s) {
    std::string args_str;
    for (std::size_t i = 0; i < s.args.size(); ++i) {
      if (i != 0) args_str += ", ";
      args_str += std::format("Expr[{}]", s.args[i].value);
    }
    Line(
        std::format(
            "Stmt[{}] ConstructOwnedObjectStmt target=StructuralVar[{}] "
            "scope=StructuralScope[{}] args=[{}]",
            id.value, s.target.value, s.scope_id.value, args_str));
  }

  void DumpConstructExternalUnitStmt(
      StmtId id, const ConstructExternalUnitStmt& s) {
    Line(
        std::format(
            "Stmt[{}] ConstructExternalUnitStmt target=StructuralVar[{}] "
            "unit=\"{}\"",
            id.value, s.target.value, s.unit_name));
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
              DumpProceduralVarDeclStmt(enclosing, id, s);
            },
            [&](const ExprStmt& s) { DumpExprStmt(s, enclosing, id); },
            [&](const BlockStmt& s) { DumpBlockStmt(stmt, s, id); },
            [&](const IfStmt& s) { DumpIfStmt(stmt, s, enclosing, id); },
            [&](const ConstructOwnedObjectStmt& s) {
              DumpConstructOwnedObjectStmt(id, s);
            },
            [&](const ConstructExternalUnitStmt& s) {
              DumpConstructExternalUnitStmt(id, s);
            },
            [&](const ForStmt& s) { DumpForStmt(stmt, s, enclosing, id); },
            [&](const DelayStmt& d) {
              Line(
                  std::format(
                      "Stmt[{}] DelayStmt ticks={}", id.value, d.duration));
            },
            [&](const WhileStmt& s) { DumpWhileStmt(stmt, s, enclosing, id); },
            [&](const DoWhileStmt& s) {
              DumpDoWhileStmt(stmt, s, enclosing, id);
            },
            [&](const BreakStmt&) {
              Line(std::format("Stmt[{}] BreakStmt", id.value));
            },
            [&](const ContinueStmt&) {
              Line(std::format("Stmt[{}] ContinueStmt", id.value));
            },
            [&](const ReturnStmt& s) {
              if (s.value.has_value()) {
                Line(
                    std::format(
                        "Stmt[{}] ReturnStmt value=Expr[{}]", id.value,
                        s.value->value));
                Indent();
                Line(
                    std::format(
                        "Expr[{}] {}", s.value->value,
                        FormatExpr(enclosing, *s.value)));
                Dedent();
              } else {
                Line(std::format("Stmt[{}] ReturnStmt", id.value));
              }
            },
            [&](const SensitivityWaitStmt& s) {
              Line(std::format("Stmt[{}] SensitivityWaitStmt", id.value));
              Indent();
              for (const auto& r : s.reads) {
                Line(
                    std::format(
                        "StructuralVarRef hops={} var=StructuralVar[{}] "
                        "bits=[{}:{}] edge={}",
                        r.ref.hops.value, r.ref.var.value, r.bit_range.first,
                        r.bit_range.second, FormatEventEdge(r.edge_kind)));
              }
              Dedent();
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

  void DumpDoWhileStmt(
      const Stmt& parent, const DoWhileStmt& s,
      const ProceduralScope& enclosing, StmtId id) {
    Line(std::format("Stmt[{}] DoWhileStmt", id.value));
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

  void DumpClosureExpr(
      const ClosureExpr& closure, const ProceduralScope& enclosing) {
    if (closure.captures.empty()) {
      Line("captures: (none)");
    } else {
      Line("captures:");
      Indent();
      for (std::size_t i = 0; i < closure.captures.size(); ++i) {
        DumpCapture(i, closure.captures[i], enclosing);
      }
      Dedent();
    }
    if (closure.body == nullptr) {
      Line("body: <null>");
    } else {
      Line("body:");
      Indent();
      DumpProceduralScope(*closure.body);
      Dedent();
    }
  }

  void DumpCapture(
      std::size_t index, const Capture& capture,
      const ProceduralScope& enclosing) {
    std::visit(
        Overloaded{
            [&](const ByValueCapture& bv) {
              Line(
                  std::format(
                      "[{}] ByValueCapture value=Expr[{}] binding="
                      "ProceduralVarId{{{}}}",
                      index, bv.value.value, bv.binding.value));
              Indent();
              Line(
                  std::format(
                      "Expr[{}] {}", bv.value.value,
                      FormatExpr(enclosing, bv.value)));
              Dedent();
            },
        },
        capture);
  }

  void DumpRuntimePrintCallItems(
      const RuntimePrintCall& call, const ProceduralScope& enclosing) {
    Line(
        std::format(
            "RuntimePrintCall kind={} descriptor={} items={}",
            DumpPrintKindLabel(call.kind),
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

  void DumpRuntimeDiagnosticCallItems(
      const RuntimeDiagnosticCall& call, const ProceduralScope& enclosing) {
    const std::string origin_text =
        call.origin.has_value() ? std::format(
                                      "{}:{}:{}", call.origin->file,
                                      call.origin->line, call.origin->col)
                                : std::string{"<none>"};
    Line(
        std::format(
            "RuntimeDiagnosticCall severity={} origin={} items={}",
            FormatMirDiagnosticSeverity(call.severity), origin_text,
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
                      DumpFormatKindLabel(v.spec.kind), v.spec.modifiers.width,
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

  static auto FormatMirDiagnosticSeverity(DiagnosticSeverity s)
      -> std::string_view {
    switch (s) {
      case DiagnosticSeverity::kInfo:
        return "kInfo";
      case DiagnosticSeverity::kWarning:
        return "kWarning";
      case DiagnosticSeverity::kError:
        return "kError";
    }
    throw InternalError("FormatMirDiagnosticSeverity: unknown severity");
  }

  static auto DumpPrintKindLabel(value::PrintKind k) -> std::string_view {
    switch (k) {
      case value::PrintKind::kDisplay:
        return "kDisplay";
      case value::PrintKind::kWrite:
        return "kWrite";
      case value::PrintKind::kFDisplay:
        return "kFDisplay";
      case value::PrintKind::kFWrite:
        return "kFWrite";
    }
    throw InternalError("DumpPrintKindLabel: unknown value::PrintKind");
  }

  static auto DumpFormatKindLabel(value::FormatKind k) -> std::string_view {
    switch (k) {
      case value::FormatKind::kDecimal:
        return "kDecimal";
      case value::FormatKind::kHex:
        return "kHex";
      case value::FormatKind::kBinary:
        return "kBinary";
      case value::FormatKind::kOctal:
        return "kOctal";
      case value::FormatKind::kString:
        return "kString";
      case value::FormatKind::kChar:
        return "kChar";
      case value::FormatKind::kRealDecimal:
        return "kRealDecimal";
      case value::FormatKind::kRealExponential:
        return "kRealExponential";
      case value::FormatKind::kRealGeneral:
        return "kRealGeneral";
      case value::FormatKind::kAssignmentPattern:
        return "kAssignmentPattern";
    }
    throw InternalError("DumpFormatKindLabel: unknown value::FormatKind");
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
                const std::string init_str = std::format(
                    " = Expr[{}] {}", d.init.value,
                    FormatExpr(enclosing, d.init));
                const auto& owner =
                    ResolveProceduralScopeAtHops(d.induction_var.hops.value);
                const auto& var = owner.vars.at(d.induction_var.var.value);
                Line(
                    std::format(
                        "[{}] decl ProceduralVarRef[hops={}, var={}] \"{}\"{}",
                        i, d.induction_var.hops.value,
                        d.induction_var.var.value, var.name, init_str));
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

  std::string out_;
  int indent_ = 0;
  std::vector<const StructuralScope*> scope_stack_;
  std::vector<const ProceduralScope*> procedural_scope_stack_;
};

}  // namespace

auto DumpMir(const CompilationUnit& unit) -> std::string {
  MirDumper dumper;
  return dumper.Dump(unit);
}

}  // namespace lyra::mir
