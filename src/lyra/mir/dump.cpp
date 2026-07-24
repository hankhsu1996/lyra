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
#include "lyra/mir/callable_code.hpp"
#include "lyra/mir/class.hpp"
#include "lyra/mir/class_ref.hpp"
#include "lyra/mir/closure.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/field.hpp"
#include "lyra/mir/local.hpp"
#include "lyra/mir/runtime_print.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/mir/type.hpp"
#include "lyra/mir/unary_op.hpp"
#include "lyra/support/builtin_fn.hpp"
#include "lyra/support/dpi_abi.hpp"
#include "lyra/value/format.hpp"

namespace lyra::mir {

namespace {

class MirDumper {
 public:
  auto Dump(const CompilationUnit& unit) -> std::string {
    unit_ = &unit;
    Line("CompilationUnit");
    Indent();
    Line("Types:");
    Indent();
    for (std::size_t i = 0; i < unit.types.size(); ++i) {
      Line(
          std::format(
              "[{}] {}", i,
              FormatType(
                  unit.types.Get(TypeId{static_cast<std::uint32_t>(i)}))));
    }
    Dedent();
    Line("Class:");
    Indent();
    if (unit.root.has_value()) {
      DumpClass(*unit.root, unit.GetClass(*unit.root));
    }
    // A class reached by a handle is not a child of the runtime tree, so it is
    // not dumped under the root; a runtime tree node is reached by the root
    // walk's Contained recursion instead.
    for (std::size_t i = 0; i < unit.classes.size(); ++i) {
      const ClassId id{static_cast<std::uint32_t>(i)};
      if ((unit.root.has_value() && id == *unit.root) ||
          !unit.classes.IsDefined(id)) {
        continue;
      }
      const Class& cls = unit.GetClass(id);
      if (cls.is_scope_tree_node) {
        continue;
      }
      DumpClass(id, cls);
    }
    Dedent();
    // A rootless unit (a package) owns its callables directly in the namespace.
    if (!unit.callables.empty()) {
      Line("Callables:");
      Indent();
      for (std::size_t i = 0; i < unit.callables.size(); ++i) {
        DumpCallable(
            unit.callables.Get(CallableId{static_cast<std::uint32_t>(i)}), i);
      }
      Dedent();
    }
    if (unit.structs.size() > 0) {
      Line("Structs:");
      Indent();
      for (std::size_t i = 0; i < unit.structs.size(); ++i) {
        const StructId sid{static_cast<std::uint32_t>(i)};
        if (!unit.structs.IsDefined(sid)) {
          continue;
        }
        DumpStruct(sid, unit.GetStruct(sid));
      }
      Dedent();
    }
    if (unit.closures.size() > 0) {
      Line("Closures:");
      Indent();
      for (std::size_t i = 0; i < unit.closures.size(); ++i) {
        const ClosureId cid{static_cast<std::uint32_t>(i)};
        if (!unit.closures.IsDefined(cid)) {
          continue;
        }
        DumpClosure(cid, unit.GetClosure(cid));
      }
      Dedent();
    }
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

  [[nodiscard]] auto FormatClassRef(const ClassRef& ref) const -> std::string {
    return std::visit(
        Overloaded{
            [this](const IntraUnitClassRef& i) -> std::string {
              return std::format(
                  "IntraUnit[#{}] \"{}\"", i.class_id.value,
                  unit_->GetClass(i.class_id).name);
            },
            [](const ExternalClassRef& e) -> std::string {
              return std::format("External(\"{}\")", e.qualified_name);
            }},
        ref);
  }

  [[nodiscard]] auto FormatVirtualSlot(const VirtualSlot& s) const
      -> std::string {
    return std::visit(
        Overloaded{
            [this](const LocalVirtualSlot& l) -> std::string {
              const auto& owner = unit_->GetClass(l.owner_class);
              const auto& callable = owner.callables.Get(l.slot);
              return std::format(
                  "Class[{}].{}(Callable[{}])", l.owner_class.value,
                  callable.name, l.slot.value);
            },
            [](const ExternalVirtualSlot& e) -> std::string {
              return std::format(
                  "External({}::{}::{})", e.unit_name, e.class_name,
                  e.method_name);
            }},
        s);
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
                  "UnpackedArray(elem=Type[{}], dim=[{}:{}])",
                  u.element_type.value, u.dim.left, u.dim.right);
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
              return std::format(
                  "AssociativeArray(elem=Type[{}], key=Type[{}])",
                  a.element_type.value, a.key_type.value);
            },
            [](const WildcardIndexType&) -> std::string {
              return "WildcardIndexType";
            },
            [](const StringType&) -> std::string { return "StringType"; },
            [](const MachineCStringType&) -> std::string {
              return "MachineCStringType";
            },
            [](const MachineIntType& m) -> std::string {
              return std::format(
                  "MachineInt(width={}, signed={})", m.bit_width,
                  m.signedness == Signedness::kSigned ? "true" : "false");
            },
            [](const MachineFloatType& m) -> std::string {
              return std::format("MachineFloat(width={})", m.bit_width);
            },
            [](const EventType&) -> std::string { return "EventType"; },
            [](const RealType&) -> std::string { return "RealType"; },
            [](const ShortRealType&) -> std::string { return "ShortRealType"; },
            [](const RealTimeType&) -> std::string { return "RealTimeType"; },
            [](const ChandleType&) -> std::string { return "ChandleType"; },
            [](const VoidType&) -> std::string { return "VoidType"; },
            [](const ObjectType& o) -> std::string {
              return std::format("Object(#{})", o.class_id.value);
            },
            [](const ExternalUnitObjectType& e) -> std::string {
              return std::format(
                  "ExternalUnitObject(unit=\"{}\")", e.unit_name);
            },
            [](const ExternalClassType& e) -> std::string {
              return std::format("ExternalClass(\"{}\")", e.qualified_name);
            },
            [](const RuntimeEffectsType&) -> std::string {
              return "RuntimeEffects";
            },
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
                case RuntimeLibraryKind::kFormatArg:
                  return "RuntimeLibrary(FormatArg)";
                case RuntimeLibraryKind::kChannelCancellation:
                  return "RuntimeLibrary(ChannelCancellation)";
                case RuntimeLibraryKind::kTimeFormat:
                  return "RuntimeLibrary(TimeFormat)";
                case RuntimeLibraryKind::kHierarchySegment:
                  return "RuntimeLibrary(HierarchySegment)";
                case RuntimeLibraryKind::kTrigger:
                  return "RuntimeLibrary(Trigger)";
                case RuntimeLibraryKind::kScopeProgram:
                  return "RuntimeLibrary(ScopeProgram)";
                case RuntimeLibraryKind::kUnitDefinition:
                  return "RuntimeLibrary(UnitDefinition)";
                case RuntimeLibraryKind::kScopeMetadata:
                  return "RuntimeLibrary(ScopeMetadata)";
                case RuntimeLibraryKind::kAbiStringRef:
                  return "RuntimeLibrary(AbiStringRef)";
                case RuntimeLibraryKind::kScopeEntry:
                  return "RuntimeLibrary(ScopeEntry)";
                case RuntimeLibraryKind::kDpiBitBuffer:
                  return "RuntimeLibrary(DpiBitBuffer)";
                case RuntimeLibraryKind::kDpiLogicBuffer:
                  return "RuntimeLibrary(DpiLogicBuffer)";
                case RuntimeLibraryKind::kDpiBitChunk:
                  return "RuntimeLibrary(DpiBitChunk)";
                case RuntimeLibraryKind::kDpiLogicChunk:
                  return "RuntimeLibrary(DpiLogicChunk)";
                case RuntimeLibraryKind::kDpiScopeGuard:
                  return "RuntimeLibrary(DpiScopeGuard)";
              }
              throw InternalError("dump: unknown RuntimeLibraryKind");
            },
            [](const CoroutineType& c) -> std::string {
              return std::format(
                  "Coroutine(payload=Type[{}])", c.payload.value);
            },
            [](const StructType& s) -> std::string {
              return std::format("Struct[{}]", s.struct_id.value);
            },
            [](const ClosureType& c) -> std::string {
              return std::format("Closure[{}]", c.closure_id.value);
            },
            [](const RefType& r) -> std::string {
              return std::format(
                  "Ref({}pointee=Type[{}])",
                  r.mutability == Mutability::kReadOnly ? "readonly, " : "",
                  r.pointee.value);
            },
            [](const PointerType& p) -> std::string {
              const std::string_view ro =
                  p.mutability == Mutability::kReadOnly ? ", readonly" : "";
              switch (p.ownership) {
                case PointerOwnership::kUnique:
                  return std::format(
                      "Pointer(unique{}, pointee=Type[{}])", ro,
                      p.pointee.value);
                case PointerOwnership::kShared:
                  return std::format(
                      "Pointer(shared{}, pointee=Type[{}])", ro,
                      p.pointee.value);
                case PointerOwnership::kBorrowed:
                  return std::format(
                      "Pointer(borrowed{}, pointee=Type[{}])", ro,
                      p.pointee.value);
              }
              throw InternalError("MirDumper: unknown PointerOwnership");
            },
            [](const ManagedRefType& m) -> std::string {
              return std::format(
                  "ManagedRef(pointee=Type[{}])", m.pointee.value);
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
            [](const UnionType& u) -> std::string {
              std::string elements;
              for (std::size_t i = 0; i < u.elements.size(); ++i) {
                if (i != 0) {
                  elements += ", ";
                }
                elements += std::format("Type[{}]", u.elements[i].value);
              }
              return std::format("Union(elems=[{}])", elements);
            },
            [](const ObservableType& o) -> std::string {
              return std::format("Observable(value=Type[{}])", o.value.value);
            },
            [](const ResolvedType& r) -> std::string {
              return std::format("Resolved(value=Type[{}])", r.value.value);
            },
            [](const DriverType& d) -> std::string {
              return std::format("Driver(value=Type[{}])", d.value.value);
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

  [[nodiscard]] auto FormatVirtualDispatchRole(
      const VirtualDispatchRole& role) const -> std::string {
    return std::visit(
        Overloaded{
            [](const IntroducesVirtualSlot&) -> std::string {
              return "IntroducesSlot";
            },
            [this](const OverridesIntraUnitSlot& o) -> std::string {
              const auto& owner = unit_->GetClass(o.slot_owner);
              const auto& callable = owner.callables.Get(o.slot_id);
              return std::format(
                  "OverridesIntraUnitSlot[Class[{}].{}(Callable[{}])]",
                  o.slot_owner.value, callable.name, o.slot_id.value);
            },
            [](const OverridesExternalSlot& e) -> std::string {
              return std::format(
                  "OverridesExternalSlot[{}::{}::{}]", e.unit_name,
                  e.class_name, e.method_name);
            }},
        role);
  }

  [[nodiscard]] auto FormatDirectTarget(const DirectTarget& target) const
      -> std::string {
    return std::visit(
        Overloaded{
            [this](const CallableTarget& c) -> std::string {
              const auto& callable =
                  unit_->GetClass(c.owner).callables.Get(c.slot);
              return std::format(
                  R"(callable=Class[{}].{} "{}")", c.owner.value, c.slot.value,
                  callable.name);
            },
            [](const support::BuiltinFn& id) -> std::string {
              return std::format("builtin=\"{}\"", support::BuiltinFnName(id));
            },
            [](const ImportedRuntimeCallTarget& i) -> std::string {
              return std::format(
                  "imported_runtime=\"{}\"",
                  support::ImportedRuntimeMethodSymbol(i.method));
            },
            [](const ExternalUnitCallableTarget& e) -> std::string {
              return std::format(
                  R"(external_unit={}::{})", e.unit_name, e.callable_name);
            },
            [](const ExternalUnitClassMethodTarget& e) -> std::string {
              return std::format(
                  "external_class_method={}::{}::{}", e.unit_name, e.class_name,
                  e.method_name);
            }},
        target);
  }

  [[nodiscard]] static auto FormatQualification(
      const std::optional<ScopeQualifier>& q) -> std::string {
    if (!q.has_value()) return "";
    return std::visit(
        Overloaded{
            [](const TypeQualifier& tq) -> std::string {
              return std::format(", qualification=Type[{}]", tq.type.value);
            },
        },
        *q);
  }

  [[nodiscard]] auto FormatCallee(const Callee& callee) const -> std::string {
    return std::visit(
        Overloaded{
            [this](const Direct& d) -> std::string {
              return std::format(
                  "Direct[{}{}]", FormatDirectTarget(d.target),
                  FormatQualification(d.qualification));
            },
            [](const Indirect& i) -> std::string {
              return std::format("Indirect[closure=Expr[{}]]", i.closure.value);
            },
            [](const Construct&) -> std::string { return "Construct"; },
            [this](const Virtual& v) -> std::string {
              return std::format(
                  "Virtual[recv=Expr[{}] slot={}]", v.receiver.value,
                  FormatVirtualSlot(v.slot));
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
            [](const MachineIntLiteral& lit) -> std::string {
              return std::format("MachineIntLiteral({})", lit.value);
            },
            [](const AddressOfExpr& a) -> std::string {
              return std::format(
                  "AddressOfExpr operand=Expr[{}]", a.operand.value);
            },
            [](const MoveExpr& m) -> std::string {
              return std::format("MoveExpr operand=Expr[{}]", m.operand.value);
            },
            [](const PointerCastExpr& c) -> std::string {
              return std::format(
                  "PointerCastExpr operand=Expr[{}]", c.operand.value);
            },
            [](const IntCastExpr& c) -> std::string {
              return std::format(
                  "IntCastExpr operand=Expr[{}]", c.operand.value);
            },
            [this](const LocalRef& r) -> std::string {
              const auto& var = code_->locals.Get(r.var);
              return std::format(
                  "LocalRef[var={}] \"{}\"", r.var.value, var.name);
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
            [](const FieldAccessExpr& m) -> std::string {
              return std::format(
                  "FieldAccessExpr receiver=Expr[{}] field={}",
                  m.receiver.value,
                  std::visit(
                      Overloaded{
                          [](const FieldTarget& t) -> std::string {
                            return std::format(
                                "Class[{}]::Field[{}]", t.owner.value,
                                t.slot.value);
                          },
                          [](const FieldId& id) -> std::string {
                            return std::format("Field[{}]", id.value);
                          },
                          [](const ExternalFieldTarget& t) -> std::string {
                            return std::format(
                                "External[{}::{}::{}]", t.unit_name,
                                t.class_name, t.field_name);
                          }},
                      m.field));
            },
            [](const DerefExpr& d) -> std::string {
              return std::format("DerefExpr pointer=Expr[{}]", d.pointer.value);
            },
            [](const StructConstructExpr& sc) -> std::string {
              return std::format(
                  "StructConstructExpr struct=Struct[{}] field_inits={}",
                  sc.struct_id.value, sc.field_inits.size());
            },
            [](const FunctionRef& fr) -> std::string {
              return std::format(
                  "FunctionRef adapter=AbiAdapter[{}]", fr.adapter.value);
            },
            [](const StaticConstantRef& r) -> std::string {
              return std::format(
                  "StaticConstantRef constant=StaticConstant[{}]",
                  r.constant.value);
            },
            [](const StaticPropertyRef& r) -> std::string {
              return std::format(
                  "StaticPropertyRef owner=Class[{}] prop=StaticProperty[{}]",
                  r.owner.value, r.prop.value);
            },
            [](const ExternalUnitVariableRef& r) -> std::string {
              return std::format(
                  "ExternalUnitVariableRef unit={} variable={}", r.unit_name,
                  r.variable_name);
            },
            [](const ExternalStaticPropertyRef& r) -> std::string {
              return std::format(
                  "ExternalStaticPropertyRef external={}::{}::{}", r.unit_name,
                  r.class_name, r.property_name);
            },
            [](const ClosureExpr& cl) -> std::string {
              return std::format(
                  "ClosureExpr closure=Closure[{}] field_inits={}",
                  cl.closure.value, cl.field_inits.size());
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
            [](const UnionExpr& u) -> std::string {
              return std::format(
                  "UnionExpr index={} value=Expr[{}]", u.index, u.value.value);
            },
            [](const UnionGetExpr& g) -> std::string {
              return std::format(
                  "UnionGetExpr union=Expr[{}] index={}", g.union_value.value,
                  g.index);
            },
            [](const UnionGetRefExpr& g) -> std::string {
              return std::format(
                  "UnionGetRefExpr union=Expr[{}] index={}",
                  g.union_value.value, g.index);
            },
        },
        e.data);
    return std::format("{} type=Type[{}]", formatted, e.type.value);
  }

  static auto FormatVarType(TypeId type) -> std::string {
    return std::format("Type[{}]", type.value);
  }

  void DumpClass(ClassId id, const Class& s) {
    scope_stack_.push_back(&s);
    const std::string kind = s.is_interface_class ? "InterfaceClass" : "Class";
    Line(std::format("{} \"{}\" (#{})", kind, s.name, id.value));
    Indent();

    if (s.base.has_value()) {
      Line(std::format("Base: {}", FormatClassRef(*s.base)));
    }

    for (const auto& impl : s.implements) {
      Line(std::format("Implements: {}", FormatClassRef(impl)));
    }

    Line("Contained:");
    Indent();
    for (const ClassId child : s.contained) {
      Line(std::format("[#{}]", child.value));
      Indent();
      DumpClass(child, unit_->GetClass(child));
      Dedent();
    }
    Dedent();

    Line("Fields:");
    Indent();
    DumpFieldList(s.fields);
    Dedent();

    Line("Callables:");
    Indent();
    for (std::size_t i = 0; i < s.callables.size(); ++i) {
      DumpCallable(
          s.callables.Get(CallableId{static_cast<std::uint32_t>(i)}), i);
    }
    Dedent();

    if (!s.abi_adapters.empty()) {
      Line("AbiAdapters:");
      Indent();
      for (std::size_t i = 0; i < s.abi_adapters.size(); ++i) {
        DumpAbiAdapter(
            s.abi_adapters.Get(AbiAdapterId{static_cast<std::uint32_t>(i)}), i);
      }
      Dedent();
    }

    if (!s.foreign_export_wrappers.empty()) {
      Line("ForeignExportWrappers:");
      Indent();
      for (std::size_t i = 0; i < s.foreign_export_wrappers.size(); ++i) {
        DumpForeignExportWrapper(s.foreign_export_wrappers[i], i);
      }
      Dedent();
    }

    Line("Constructor:");
    Indent();
    if (s.constructor.base_init.has_value()) {
      Line("BaseInit:");
      Indent();
      for (std::size_t i = 0; i < s.constructor.base_init->args.size(); ++i) {
        Line(
            std::format(
                "[{}] arg=Expr[{}]", i,
                s.constructor.base_init->args[i].value));
      }
      Dedent();
    }
    if (!s.constructor.member_inits.empty()) {
      Line("MemberInits:");
      Indent();
      for (std::size_t i = 0; i < s.constructor.member_inits.size(); ++i) {
        const auto& mi = s.constructor.member_inits[i];
        Line(
            std::format(
                "[{}] field=Field[{}] value=Expr[{}]", i, mi.target.value,
                mi.value.value));
      }
      Dedent();
    }
    Line("Body:");
    Indent();
    DumpCallableBody(s.constructor.code);
    Dedent();
    Dedent();

    Dedent();
    scope_stack_.pop_back();
  }

  void DumpFieldList(const base::Arena<FieldDecl, FieldId>& fields) {
    for (std::size_t i = 0; i < fields.size(); ++i) {
      const auto& v = fields.Get(FieldId{static_cast<std::uint32_t>(i)});
      const std::string as =
          v.source_name.empty() ? "" : std::format(" as \"{}\"", v.source_name);
      Line(
          std::format(
              "[{}] \"{}\"{} : {}", i, v.name, as, FormatVarType(v.type)));
    }
  }

  void DumpCallable(const CallableDecl& d, std::size_t index) {
    std::visit(
        Overloaded{
            [&](const InternalCallable& in) {
              Line(
                  std::format(
                      "[{}] \"{}\" : Type[{}]", index, d.name,
                      in.code.result_type.value));
              Indent();
              Line(
                  std::format(
                      "Visibility: {}",
                      d.visibility == CallableVisibility::kPublic
                          ? "public"
                          : "internal"));
              if (d.virtual_dispatch.has_value()) {
                Line(
                    std::format(
                        "VirtualDispatch: {}",
                        FormatVirtualDispatchRole(*d.virtual_dispatch)));
              }
              for (std::size_t i = 0; i < in.code.params.size(); ++i) {
                const auto& param = in.code.locals.Get(in.code.params[i]);
                Line(
                    std::format(
                        "Param[{}] \"{}\" : Type[{}]", i, param.name,
                        param.type.value));
              }
              DumpCallableBody(in.code);
              Dedent();
            },
            [&](const ExternalCallable& ext) {
              Line(
                  std::format(
                      R"([{}] "{}" external c_name="{}"{}{} ret={} : {})",
                      index, d.name, ext.external.foreign_name,
                      ext.external.is_pure ? " pure" : "",
                      ext.is_task ? " task" : "",
                      support::DpiScalarAbiName(ext.ret_abi),
                      FormatVarType(ext.ret_sv_type)));
              Indent();
              for (std::size_t i = 0; i < ext.params.size(); ++i) {
                Line(
                    std::format(
                        "Param[{}] {} {} : {}", i,
                        support::DpiDirectionName(ext.params[i].direction),
                        support::DpiCarrierName(ext.params[i].carrier),
                        FormatVarType(ext.params[i].sv_type)));
              }
              Dedent();
            },
            [&](const PurePrototype& proto) {
              Line(
                  std::format(
                      "[{}] \"{}\" : Type[{}] prototype", index, d.name,
                      proto.code.result_type.value));
              Indent();
              Line(
                  std::format(
                      "Visibility: {}",
                      d.visibility == CallableVisibility::kPublic
                          ? "public"
                          : "internal"));
              if (d.virtual_dispatch.has_value()) {
                Line(
                    std::format(
                        "VirtualDispatch: {}",
                        FormatVirtualDispatchRole(*d.virtual_dispatch)));
              }
              for (std::size_t i = 0; i < proto.code.params.size(); ++i) {
                const auto& param = proto.code.locals.Get(proto.code.params[i]);
                Line(
                    std::format(
                        "Param[{}] \"{}\" : Type[{}]", i, param.name,
                        param.type.value));
              }
              Dedent();
            }},
        d.impl);
  }

  void DumpStruct(StructId id, const StructDecl& decl) {
    Line(std::format("Struct \"{}\" (#{})", decl.name, id.value));
    Indent();
    Line("Fields:");
    Indent();
    DumpFieldList(decl.fields);
    Dedent();
    Dedent();
  }

  void DumpClosure(ClosureId id, const ClosureDecl& decl) {
    Line(std::format("Closure (#{})", id.value));
    Indent();
    Line("Captures:");
    Indent();
    DumpFieldList(decl.fields);
    Dedent();
    if (!decl.field_order.empty()) {
      std::string order;
      for (std::size_t i = 0; i < decl.field_order.size(); ++i) {
        if (i != 0) order += ", ";
        order += std::format("Field[{}]", decl.field_order[i].value);
      }
      Line(std::format("FieldOrder: [{}]", order));
    }
    Line("Invoke:");
    Indent();
    DumpCallableBody(decl.invoke);
    Dedent();
    Dedent();
  }

  void DumpForeignExportWrapper(
      const ForeignExportWrapper& w, std::size_t index) {
    Line(
        std::format(
            R"([{}] c_name="{}" instance="{}" : Type[{}])", index,
            w.foreign_name, w.instance_name, w.code.result_type.value));
    Indent();
    const auto& self = w.code.locals.Get(w.self_local);
    Line(std::format(R"(Self "{}" : Type[{}])", self.name, self.type.value));
    for (std::size_t i = 0; i < w.code.params.size(); ++i) {
      const auto& param = w.code.locals.Get(w.code.params[i]);
      Line(
          std::format(
              "Param[{}] \"{}\" : Type[{}]", i, param.name, param.type.value));
    }
    DumpCallableBody(w.code);
    Dedent();
  }

  void DumpAbiAdapter(const AbiAdapter& a, std::size_t index) {
    Line(
        std::format(
            "[{}] \"{}\" : Type[{}]", index, a.name, a.code.result_type.value));
    Indent();
    for (std::size_t i = 0; i < a.code.params.size(); ++i) {
      const auto& param = a.code.locals.Get(a.code.params[i]);
      Line(
          std::format(
              "Param[{}] \"{}\" : Type[{}]", i, param.name, param.type.value));
    }
    DumpCallableBody(a.code);
    Dedent();
  }

  // A callable owns its binding arena: every activation local and parameter
  // lives in `locals` (a closure's `locals[0]` is its receiver, a borrow of the
  // closure, and a captured read is a field access over it). Dump the locals,
  // then the body block, with `code_` set so `LocalRef` reads resolve against
  // this callable's arena.
  void DumpCallableBody(const CallableCode& code) {
    const CallableCode* saved = code_;
    code_ = &code;
    if (!code.locals.empty()) {
      Line("Locals:");
      Indent();
      for (std::size_t i = 0; i < code.locals.size(); ++i) {
        const auto& v = code.locals.Get(LocalId{static_cast<std::uint32_t>(i)});
        Line(
            std::format(
                "Local[{}] \"{}\" : Type[{}]", i, v.name, v.type.value));
      }
      Dedent();
    }
    DumpBlock(code.body);
    code_ = saved;
  }

  void DumpBlock(const Block& scope) {
    if (!scope.exprs.empty()) {
      Line("Exprs:");
      Indent();
      for (std::size_t i = 0; i < scope.exprs.size(); ++i) {
        const ExprId id{static_cast<std::uint32_t>(i)};
        Line(std::format("Expr[{}] {}", i, FormatExpr(scope, id)));
        const auto& expr = scope.exprs.Get(id);
        if (const auto* sc = std::get_if<StructConstructExpr>(&expr.data)) {
          Indent();
          DumpStructConstructExpr(*sc);
          Dedent();
        }
        if (const auto* cl = std::get_if<ClosureExpr>(&expr.data)) {
          Indent();
          DumpClosureExpr(*cl);
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
  }

  void DumpLocalDeclStmt(
      const Block& enclosing, StmtId id, const LocalDeclStmt& s) {
    const auto& var = code_->locals.Get(s.target);
    Line(
        std::format(
            "Stmt[{}] LocalDeclStmt target=LocalRef[var={}] \"{}\"", id.value,
            s.target.value, var.name));
    Indent();
    Line(
        std::format(
            "init: Expr[{}] {}", s.init.value, FormatExpr(enclosing, s.init)));
    Dedent();
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
            [&](const IfStmt& s) { DumpIfStmt(enclosing, s, id); },
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

  void DumpFieldInits(const std::vector<FieldInit>& field_inits) {
    if (field_inits.empty()) {
      Line("field_inits: (none)");
      return;
    }
    // Each initializer's value expr lives in the enclosing block and is dumped
    // there with the other exprs; reference it by id.
    Line("field_inits:");
    Indent();
    for (const FieldInit& init : field_inits) {
      Line(
          std::format(
              "Field[{}] = Expr[{}]", init.target.value, init.value.value));
    }
    Dedent();
  }

  void DumpStructConstructExpr(const StructConstructExpr& construct) {
    Line(std::format("struct: Struct[{}]", construct.struct_id.value));
    DumpFieldInits(construct.field_inits);
  }

  void DumpClosureExpr(const ClosureExpr& construct) {
    Line(std::format("closure: Closure[{}]", construct.closure.value));
    DumpFieldInits(construct.field_inits);
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
                const auto& var = code_->locals.Get(d.induction_var);
                Line(
                    std::format(
                        "[{}] decl LocalRef[var={}] \"{}\"{}", i,
                        d.induction_var.value, var.name, init_str));
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
  const CompilationUnit* unit_ = nullptr;
  const CallableCode* code_ = nullptr;
};

}  // namespace

auto DumpMir(const CompilationUnit& unit) -> std::string {
  MirDumper dumper;
  return dumper.Dump(unit);
}

}  // namespace lyra::mir
