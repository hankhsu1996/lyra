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
#include "lyra/mir/class.hpp"
#include "lyra/mir/closure.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/local.hpp"
#include "lyra/mir/member.hpp"
#include "lyra/mir/process.hpp"
#include "lyra/mir/runtime_print.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/mir/type.hpp"
#include "lyra/mir/unary_op.hpp"
#include "lyra/support/builtin_fn.hpp"
#include "lyra/value/format.hpp"

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
    Line("Class:");
    Indent();
    DumpClass(unit.top_class);
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
              return std::format("Object(name=\"{}\")", o.name);
            },
            [](const ExternalUnitObjectType& e) -> std::string {
              return std::format(
                  "ExternalUnitObject(unit=\"{}\")", e.unit_name);
            },
            [](const ScopeType&) -> std::string { return "Scope"; },
            [](const ServicesType&) -> std::string { return "Services"; },
            [](const FilesType&) -> std::string { return "Files"; },
            [](const DiagnosticType&) -> std::string { return "Diagnostic"; },
            [](const RuntimeLibraryType& r) -> std::string {
              switch (r.kind) {
                case RuntimeLibraryKind::kPrintItem:
                  return "RuntimeLibrary(PrintItem)";
                case RuntimeLibraryKind::kPrintLiteralItem:
                  return "RuntimeLibrary(PrintLiteralItem)";
                case RuntimeLibraryKind::kPrintValueItem:
                  return "RuntimeLibrary(PrintValueItem)";
                case RuntimeLibraryKind::kFormatSpec:
                  return "RuntimeLibrary(FormatSpec)";
                case RuntimeLibraryKind::kChannelCancellation:
                  return "RuntimeLibrary(ChannelCancellation)";
                case RuntimeLibraryKind::kTimeFormat:
                  return "RuntimeLibrary(TimeFormat)";
              }
              throw InternalError("dump: unknown RuntimeLibraryKind");
            },
            [](const CoroutineType& c) -> std::string {
              return std::format(
                  "Coroutine(payload=Type[{}])", c.payload.value);
            },
            [](const RefType& r) -> std::string {
              return std::format(
                  "Ref({}pointee=Type[{}])", r.is_const ? "const, " : "",
                  r.pointee.value);
            },
            [](const PointerType& p) -> std::string {
              switch (p.ownership) {
                case PointerOwnership::kUnique:
                  return std::format(
                      "Pointer(unique, pointee=Type[{}])", p.pointee.value);
                case PointerOwnership::kShared:
                  return std::format(
                      "Pointer(shared, pointee=Type[{}])", p.pointee.value);
                case PointerOwnership::kBorrowed:
                  return std::format(
                      "Pointer(borrowed, pointee=Type[{}])", p.pointee.value);
              }
              throw InternalError("MirDumper: unknown PointerOwnership");
            },
            [](const VectorType& v) -> std::string {
              return std::format("Vector(elem=Type[{}])", v.element.value);
            },
            [](const TupleType& t) -> std::string {
              std::string elements;
              for (std::size_t i = 0; i < t.elements.size(); ++i) {
                if (i != 0) {
                  elements += ", ";
                }
                elements += std::format("Type[{}]", t.elements[i].value);
              }
              return std::format("Tuple(elems=[{}])", elements);
            },
            [](const ExternalRefType& e) -> std::string {
              std::string tail;
              for (const auto& hop : e.tail) {
                tail += "." + hop.name;
                for (const std::uint32_t index : hop.indices) {
                  tail += std::format("[{}]", index);
                }
              }
              const std::string_view match =
                  e.match == ExternalRefMatch::kDefName ? "DefName"
                                                        : "ScopeName";
              return std::format(
                  "ExternalRef(elem=Type[{}], ancestor={}, match={}, tail={}, "
                  "signal={})",
                  e.element.value, e.ancestor, match, tail, e.signal);
            },
            [](const ObservableType& o) -> std::string {
              return std::format("Observable(value=Type[{}])", o.value.value);
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
            [this](const MethodRef& r) -> std::string {
              const auto& owner = ResolveScopeAtHops(r.hops.value);
              const auto& target = owner.methods.Get(r.method);
              return std::format(
                  "MethodRef[hops={}, method={}] \"{}\"", r.hops.value,
                  r.method.value, target.name);
            },
            [](const ClosureRef& cr) -> std::string {
              return std::format(
                  "ClosureRef[closure=Expr[{}]]", cr.closure.value);
            },
            [](const BuiltinFnCallee& b) -> std::string {
              return std::format(
                  "BuiltinFnCallee[id=\"{}\"]", support::BuiltinFnName(b.id));
            },
            [](const BuiltinStaticCallee& b) -> std::string {
              return std::format(
                  "BuiltinStaticCallee[id=\"{}\", type_qual=Type[{}]]",
                  support::BuiltinFnName(b.id), b.type_qual.value);
            },
            [](const FreeFnCallee& f) -> std::string {
              return std::format(
                  "FreeFnCallee[id=\"{}\"]", support::BuiltinFnName(f.id));
            },
            [](const ConstructorCallee&) -> std::string {
              return "ConstructorCallee";
            },
        },
        callee);
  }

  [[nodiscard]] auto ResolveScopeAtHops(std::uint32_t hops) const
      -> const Class& {
    if (hops >= scope_stack_.size()) {
      throw InternalError("MirDumper::ResolveScopeAtHops: hops out of range");
    }
    return *scope_stack_[scope_stack_.size() - 1 - hops];
  }

  [[nodiscard]] auto ResolveBlockAtHops(std::uint32_t hops) const
      -> const Block& {
    if (hops >= block_stack_.size()) {
      throw InternalError("MirDumper::ResolveBlockAtHops: hops out of range");
    }
    return *block_stack_[block_stack_.size() - 1 - hops];
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

  [[nodiscard]] auto FormatExpr(const Block& scope, ExprId id) const
      -> std::string {
    const auto& e = scope.exprs.Get(id);
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
            [](const NullLiteral&) -> std::string { return "NullLiteral"; },
            [](const HostIntLiteral& lit) -> std::string {
              return std::format("HostIntLiteral({})", lit.value);
            },
            [](const AddressOfExpr& a) -> std::string {
              return std::format(
                  "AddressOfExpr operand=Expr[{}]", a.operand.value);
            },
            [](const CastExpr& c) -> std::string {
              return std::format("CastExpr operand=Expr[{}]", c.operand.value);
            },
            [this](const ParamRef& r) -> std::string {
              const auto& owner = ResolveScopeAtHops(r.hops.value);
              const auto& param = owner.params.Get(r.param);
              return std::format(
                  "ParamRef[hops={}, param={}] \"{}\"", r.hops.value,
                  r.param.value, param.name);
            },
            [this](const LocalRef& r) -> std::string {
              const auto& owner = ResolveBlockAtHops(r.hops.value);
              const auto& var = owner.vars.Get(r.var);
              return std::format(
                  "LocalRef[hops={}, var={}] \"{}\"", r.hops.value, r.var.value,
                  var.name);
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
            [](const BoolCastExpr& b) -> std::string {
              return std::format(
                  "BoolCastExpr operand=Expr[{}]", b.operand.value);
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
            [](const MemberAccessExpr& m) -> std::string {
              return std::format(
                  "MemberAccessExpr receiver=Expr[{}] hops={} "
                  "var=Member[{}]",
                  m.receiver.value, m.member.hops.value, m.member.var.value);
            },
            [](const DerefExpr& d) -> std::string {
              return std::format("DerefExpr pointer=Expr[{}]", d.pointer.value);
            },
            [](const ClosureExpr& cl) -> std::string {
              return std::format(
                  "ClosureExpr captures={} params={}", cl.captures.size(),
                  cl.params.size());
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
            [](const TupleExpr& t) -> std::string {
              std::string components;
              for (std::size_t i = 0; i < t.components.size(); ++i) {
                if (i != 0) {
                  components += ", ";
                }
                components += std::format("Expr[{}]", t.components[i].value);
              }
              return std::format("TupleExpr components=[{}]", components);
            },
            [](const AwaitExpr& a) -> std::string {
              return std::format(
                  "AwaitExpr awaitable=Expr[{}]", a.awaitable.value);
            },
            [](const TupleGetExpr& g) -> std::string {
              return std::format(
                  "TupleGetExpr tuple=Expr[{}] index={}", g.tuple.value,
                  g.index);
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

  void DumpClass(const Class& s) {
    scope_stack_.push_back(&s);
    Line(std::format("Class \"{}\"", s.name));
    Indent();

    Line("NestedClasses:");
    Indent();
    for (std::size_t i = 0; i < s.nested_classes.size(); ++i) {
      Line(std::format("[{}]", i));
      Indent();
      DumpClass(s.nested_classes.Get(ClassId{static_cast<std::uint32_t>(i)}));
      Dedent();
    }
    Dedent();

    if (!s.params.empty()) {
      Line("Params:");
      Indent();
      for (std::size_t i = 0; i < s.params.size(); ++i) {
        const auto& p = s.params.Get(ParamId{static_cast<std::uint32_t>(i)});
        Line(std::format("[{}] \"{}\" : {}", i, p.name, FormatVarType(p.type)));
      }
      Dedent();
    }

    Line("Members:");
    Indent();
    for (std::size_t i = 0; i < s.members.size(); ++i) {
      const auto& v = s.members.Get(MemberId{static_cast<std::uint32_t>(i)});
      const std::string as =
          v.source_name.empty() ? "" : std::format(" as \"{}\"", v.source_name);
      Line(
          std::format(
              "[{}] \"{}\"{} : {}", i, v.name, as, FormatVarType(v.type)));
    }
    Dedent();

    Line("Methods:");
    Indent();
    for (std::size_t i = 0; i < s.methods.size(); ++i) {
      DumpMethod(s.methods.Get(MethodId{static_cast<std::uint32_t>(i)}), i);
    }
    Dedent();

    Line("Constructor:");
    Indent();
    DumpBlock(s.constructor_block);
    Dedent();

    if (s.resolve.has_value()) {
      Line("Resolve:");
      Indent();
      DumpMethod(*s.resolve, 0);
      Dedent();
    }

    if (s.initialize.has_value()) {
      Line("Initialize:");
      Indent();
      DumpMethod(*s.initialize, 0);
      Dedent();
    }

    Line("Processes:");
    Indent();
    for (std::size_t i = 0; i < s.processes.size(); ++i) {
      DumpProcess(s.processes.Get(ProcessId{static_cast<std::uint32_t>(i)}), i);
    }
    Dedent();

    Dedent();
    scope_stack_.pop_back();
  }

  void DumpProcess(const Process& p, std::size_t index) {
    Line(std::format("Process[{}] {}", index, FormatProcessKind(p)));
    Indent();
    DumpMethod(p.code, index);
    Dedent();
  }

  [[nodiscard]] static auto FormatMethodForm(MethodForm form)
      -> std::string_view {
    switch (form) {
      case MethodForm::kStatic:
        return "static";
      case MethodForm::kVirtual:
        return "virtual";
    }
    throw InternalError("FormatMethodForm: unknown mir::MethodForm");
  }

  void DumpMethod(const MethodDecl& d, std::size_t index) {
    Line(
        std::format(
            "[{}] {} \"{}\" : Type[{}]", index, FormatMethodForm(d.form),
            d.name, d.result_type.value));
    Indent();
    for (std::size_t i = 0; i < d.params.size(); ++i) {
      const auto& param = d.params[i];
      Line(
          std::format(
              "Param[{}] \"{}\" : Type[{}]", i, param.name, param.type.value));
    }
    DumpBlock(d.root_block);
    Dedent();
  }

  void DumpBlock(const Block& scope) {
    block_stack_.push_back(&scope);
    if (!scope.vars.empty()) {
      Line("Locals:");
      Indent();
      for (std::size_t i = 0; i < scope.vars.size(); ++i) {
        const auto& v = scope.vars.Get(LocalId{static_cast<std::uint32_t>(i)});
        Line(
            std::format(
                "Local[{}] \"{}\" : Type[{}]", i, v.name, v.type.value));
      }
      Dedent();
    }
    if (!scope.exprs.empty()) {
      Line("Exprs:");
      Indent();
      for (std::size_t i = 0; i < scope.exprs.size(); ++i) {
        const ExprId id{static_cast<std::uint32_t>(i)};
        Line(std::format("Expr[{}] {}", i, FormatExpr(scope, id)));
        const auto& expr = scope.exprs.Get(id);
        if (const auto* cl = std::get_if<ClosureExpr>(&expr.data)) {
          Indent();
          DumpClosureExpr(*cl, scope);
          Dedent();
        }
      }
      Dedent();
    }
    Line(std::format("Block (root_stmts={})", scope.root_stmts.size()));
    Indent();
    if (scope.root_stmts.empty()) {
      Line("(empty)");
    } else {
      for (const auto& sid : scope.root_stmts) {
        DumpStmt(scope, sid);
      }
    }
    Dedent();
    block_stack_.pop_back();
  }

  void DumpLocalDeclStmt(
      const Block& enclosing, StmtId id, const LocalDeclStmt& s) {
    const auto& owner = ResolveBlockAtHops(s.target.hops.value);
    const auto& var = owner.vars.Get(s.target.var);
    Line(
        std::format(
            "Stmt[{}] LocalDeclStmt "
            "target=LocalRef[hops={}, var={}] \"{}\"",
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
            "Stmt[{}] ConstructOwnedObjectStmt target=Member[{}] "
            "scope=Class[{}] args=[{}]",
            id.value, s.target.value, s.scope_id.value, args_str));
  }

  void DumpConstructExternalUnitStmt(
      StmtId id, const ConstructExternalUnitStmt& s) {
    std::string dims;
    for (const auto dim : s.dims) {
      dims += std::format("[{}]", dim);
    }
    Line(
        std::format(
            "Stmt[{}] ConstructExternalUnitStmt target=Member[{}] "
            "unit=\"{}\"{}",
            id.value, s.target.value, s.unit_name, dims));
  }

  void DumpStmt(const Block& enclosing, StmtId id) {
    const auto& stmt = enclosing.stmts.Get(id);
    if (stmt.label.has_value()) {
      Line(std::format("label: \"{}\"", *stmt.label));
    }
    std::visit(
        Overloaded{
            [&](const EmptyStmt&) {
              Line(std::format("Stmt[{}] EmptyStmt", id.value));
            },
            [&](const LocalDeclStmt& s) {
              DumpLocalDeclStmt(enclosing, id, s);
            },
            [&](const ExprStmt& s) { DumpExprStmt(s, enclosing, id); },
            [&](const BlockStmt& s) { DumpBlockStmt(enclosing, s, id); },
            [&](const ForkStmt& s) { DumpForkStmt(enclosing, s, id); },
            [&](const IfStmt& s) { DumpIfStmt(enclosing, s, id); },
            [&](const ConstructOwnedObjectStmt& s) {
              DumpConstructOwnedObjectStmt(id, s);
            },
            [&](const ConstructExternalUnitStmt& s) {
              DumpConstructExternalUnitStmt(id, s);
            },
            [&](const ForStmt& s) { DumpForStmt(enclosing, s, id); },
            [&](const WhileStmt& s) { DumpWhileStmt(enclosing, s, id); },
            [&](const DoWhileStmt& s) { DumpDoWhileStmt(enclosing, s, id); },
            [&](const BreakStmt& s) {
              Line(
                  std::format(
                      "Stmt[{}] BreakStmt{}", id.value,
                      s.target.has_value()
                          ? std::format(" -> label {}", s.target->value)
                          : ""));
            },
            [&](const ContinueStmt&) {
              Line(std::format("Stmt[{}] ContinueStmt", id.value));
            },
            [&](const ReturnStmt& s) {
              const std::string flavor =
                  s.is_coroutine_return ? " coroutine" : "";
              if (s.value.has_value()) {
                Line(
                    std::format(
                        "Stmt[{}] ReturnStmt{} value=Expr[{}]", id.value,
                        flavor, s.value->value));
                Indent();
                Line(
                    std::format(
                        "Expr[{}] {}", s.value->value,
                        FormatExpr(enclosing, *s.value)));
                Dedent();
              } else {
                Line(std::format("Stmt[{}] ReturnStmt{}", id.value, flavor));
              }
            },
            [&](const SensitivityWaitStmt& s) {
              Line(std::format("Stmt[{}] SensitivityWaitStmt", id.value));
              Indent();
              for (const auto& r : s.reads) {
                Line(
                    std::format(
                        "observable=Expr[{}] {} lsb={} width={} edge={}",
                        r.observable_ptr.value,
                        FormatExpr(enclosing, r.observable_ptr),
                        r.lsb_bit_offset, r.bit_width,
                        FormatEventEdge(r.edge_kind)));
              }
              Dedent();
            },
        },
        stmt.data);
  }

  void DumpWhileStmt(const Block& enclosing, const WhileStmt& s, StmtId id) {
    Line(std::format("Stmt[{}] WhileStmt", id.value));
    Indent();
    Line(
        std::format(
            "condition: Expr[{}] {}", s.condition.value,
            FormatExpr(enclosing, s.condition)));
    Line(std::format("scope (BlockId={}):", s.scope.value));
    Indent();
    DumpBlock(enclosing.child_scopes.Get(s.scope));
    Dedent();
    Dedent();
  }

  void DumpDoWhileStmt(
      const Block& enclosing, const DoWhileStmt& s, StmtId id) {
    Line(std::format("Stmt[{}] DoWhileStmt", id.value));
    Indent();
    Line(
        std::format(
            "condition: Expr[{}] {}", s.condition.value,
            FormatExpr(enclosing, s.condition)));
    Line(std::format("scope (BlockId={}):", s.scope.value));
    Indent();
    DumpBlock(enclosing.child_scopes.Get(s.scope));
    Dedent();
    Dedent();
  }

  void DumpClosureExpr(const ClosureExpr& closure, const Block& enclosing) {
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
    if (closure.params.empty()) {
      Line("params: (none)");
    } else {
      Line("params:");
      Indent();
      for (std::size_t i = 0; i < closure.params.size(); ++i) {
        Line(
            std::format(
                "[{}] Parameter binding=LocalId{{{}}}", i,
                closure.params[i].binding.value));
      }
      Dedent();
    }
    if (closure.body == nullptr) {
      Line("body: <null>");
    } else {
      Line("body:");
      Indent();
      DumpBlock(*closure.body);
      Dedent();
    }
  }

  void DumpCapture(
      std::size_t index, const Capture& capture, const Block& enclosing) {
    Line(
        std::format(
            "[{}] Capture value=Expr[{}] binding=LocalId{{{}}}", index,
            capture.value.value, capture.binding.value));
    Indent();
    Line(
        std::format(
            "Expr[{}] {}", capture.value.value,
            FormatExpr(enclosing, capture.value)));
    Dedent();
  }

  void DumpRuntimePrintItem(
      std::size_t i, const RuntimePrintItem& item, const Block& enclosing) {
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
                      "left_align={})",
                      i, v.value.value, v.type.value,
                      DumpFormatKindLabel(v.spec.kind), v.spec.modifiers.width,
                      v.spec.modifiers.precision,
                      v.spec.modifiers.zero_pad ? "true" : "false",
                      v.spec.modifiers.left_align ? "true" : "false"));
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

  static auto ForkJoinModeLabel(JoinMode mode) -> std::string_view {
    switch (mode) {
      case JoinMode::kAll:
        return "join";
      case JoinMode::kAny:
        return "join_any";
      case JoinMode::kNone:
        return "join_none";
    }
    throw InternalError("ForkJoinModeLabel: unknown JoinMode");
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
      case value::FormatKind::kTime:
        return "kTime";
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

  void DumpForStmt(const Block& enclosing, const ForStmt& s, StmtId id) {
    Line(
        std::format(
            "Stmt[{}] ForStmt{}", id.value,
            s.break_label.has_value()
                ? std::format(" break_label={}", s.break_label->value)
                : ""));
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
                    ResolveBlockAtHops(d.induction_var.hops.value);
                const auto& var = owner.vars.Get(d.induction_var.var);
                Line(
                    std::format(
                        "[{}] decl LocalRef[hops={}, var={}] \"{}\"{}", i,
                        d.induction_var.hops.value, d.induction_var.var.value,
                        var.name, init_str));
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
    Line(std::format("scope (BlockId={}):", s.scope.value));
    Indent();
    DumpBlock(enclosing.child_scopes.Get(s.scope));
    Dedent();
    Dedent();
  }

  void DumpBlockStmt(const Block& enclosing, const BlockStmt& s, StmtId id) {
    Line(
        std::format(
            "Stmt[{}] BlockStmt scope=BlockId{{{}}}", id.value, s.scope.value));
    Indent();
    DumpBlock(enclosing.child_scopes.Get(s.scope));
    Dedent();
  }

  void DumpForkStmt(const Block& enclosing, const ForkStmt& s, StmtId id) {
    Line(
        std::format(
            "Stmt[{}] ForkStmt {} (branches={})", id.value,
            ForkJoinModeLabel(s.mode), s.branches.size()));
    Indent();
    for (const auto branch : s.branches) {
      Line(std::format("branch=Expr[{}]", branch.value));
    }
    Line(std::format("scope (BlockId={}):", s.scope.value));
    Indent();
    DumpBlock(enclosing.child_scopes.Get(s.scope));
    Dedent();
    Dedent();
  }

  void DumpExprStmt(const ExprStmt& s, const Block& enclosing, StmtId id) {
    Line(
        std::format("Stmt[{}] ExprStmt expr=Expr[{}]", id.value, s.expr.value));
    Indent();
    Line(
        std::format(
            "Expr[{}] {}", s.expr.value, FormatExpr(enclosing, s.expr)));
    Dedent();
  }

  void DumpIfStmt(const Block& enclosing, const IfStmt& s, StmtId id) {
    Line(
        std::format(
            "Stmt[{}] IfStmt cond=Expr[{}] {}", id.value, s.condition.value,
            FormatExpr(enclosing, s.condition)));
    Indent();
    Line(std::format("then_scope (BlockId={}):", s.then_scope.value));
    Indent();
    DumpBlock(enclosing.child_scopes.Get(s.then_scope));
    Dedent();
    if (s.else_scope.has_value()) {
      Line(std::format("else_scope (BlockId={}):", s.else_scope->value));
      Indent();
      DumpBlock(enclosing.child_scopes.Get(*s.else_scope));
      Dedent();
    } else {
      Line("else_scope: <none>");
    }
    Dedent();
  }

  std::string out_;
  int indent_ = 0;
  std::vector<const Class*> scope_stack_;
  std::vector<const Block*> block_stack_;
};

}  // namespace

auto DumpMir(const CompilationUnit& unit) -> std::string {
  MirDumper dumper;
  return dumper.Dump(unit);
}

}  // namespace lyra::mir
