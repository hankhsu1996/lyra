#include "lyra/mir/dump.hpp"

#include <cstddef>
#include <cstdint>
#include <format>
#include <optional>
#include <set>
#include <span>
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
#include "lyra/mir/class_constant_id.hpp"
#include "lyra/mir/class_ref.hpp"
#include "lyra/mir/closure.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/field.hpp"
#include "lyra/mir/integral_constant.hpp"
#include "lyra/mir/integral_constant_id.hpp"
#include "lyra/mir/local.hpp"
#include "lyra/mir/runtime_print.hpp"
#include "lyra/mir/static_property_id.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/mir/struct_decl.hpp"
#include "lyra/mir/type.hpp"
#include "lyra/mir/type_descriptor_id.hpp"
#include "lyra/mir/unary_op.hpp"
#include "lyra/support/builtin_fn.hpp"
#include "lyra/support/runtime_class.hpp"
#include "lyra/support/value_operation.hpp"
#include "lyra/value/format.hpp"

namespace lyra::mir {

namespace {

// What a dump calls one body: the name its owner answers it by, or what it is
// where nothing names it. A body the compiler synthesized has no name to print,
// and printing its position is what a reader can match against the arena.
auto CallableLabel(std::span<const NamedCallable> named, CallableId body)
    -> std::string {
  const std::optional<std::string_view> name = NameOf(named, body);
  return name.has_value() ? std::string{*name}
                          : std::format("<synthesized {}>", body.value);
}

// One plane of a constant's bits, most significant word first, so the text
// reads the way the value is written in source.
auto FormatWords(std::span<const std::uint64_t> words) -> std::string {
  std::string text;
  for (std::size_t i = words.size(); i > 0; --i) {
    text += std::format("{}{:016x}", text.empty() ? "" : "_", words[i - 1U]);
  }
  return text.empty() ? "0" : text;
}

auto FormatBits(const IntegralConstant& value) -> std::string {
  std::string text = std::format("'h{}", FormatWords(value.value_words));
  if (!value.state_words.empty()) {
    text += std::format(" x/z'h{}", FormatWords(value.state_words));
  }
  return text;
}

auto FormatIntegralConstant(const IntegralConstantDecl& decl) -> std::string {
  return std::format("Type[{}] {}", decl.type.value, FormatBits(decl.value));
}

auto FormatExprList(std::span<const ExprId> ids) -> std::string {
  std::string text;
  for (const ExprId id : ids) {
    if (!text.empty()) {
      text += ", ";
    }
    text += std::format("Expr[{}]", id.value);
  }
  return text;
}

auto FormatClass(const DeclaredClassRef& cls) -> std::string {
  return std::visit(
      Overloaded{
          [](const IntraUnitClassRef& intra) {
            return std::format("Class[{}]", intra.class_id.value);
          },
          [](const CrossUnitClassRef& cross) {
            return std::format(
                "Class[{}::{}]", cross.unit_name, cross.class_name);
          }},
      cls);
}

auto FormatField(const ClassFieldTarget& t) -> std::string {
  return std::format("{}::Field[{}]", FormatClass(t.owner), t.slot.value);
}

auto FormatField(const ClosureFieldTarget& t) -> std::string {
  return std::format("Closure[{}]::Field[{}]", t.owner.value, t.slot.value);
}

auto FormatCallPart(const CallPart& part) -> std::string {
  return std::visit(
      Overloaded{
          [](base::ComponentIndex position) {
            return std::format("{}", position.value);
          },
          [](const ClassFieldTarget& t) { return FormatField(t); }},
      part);
}

class MirDumper {
 public:
  auto Dump(const CompilationUnit& unit) -> std::string {
    unit_ = &unit;
    // Named, because a dump of a design carries several units and every id
    // below indexes the pool of the one it sits in. Two units number their
    // types from zero, so an id read without knowing which unit it belongs to
    // resolves against the wrong pool and answers anyway.
    Line(std::format("CompilationUnit \"{}\"", unit.name));
    Indent();
    Line("Types:");
    Indent();
    for (const TypeId id : unit.types.Ids()) {
      Line(std::format("[{}] {}", id.value, FormatType(unit.types.Get(id))));
    }
    Dedent();
    // The values the unit was written with. A reference to one names its
    // position, so the bits are printed here once rather than at every use.
    Line("Constants:");
    Indent();
    for (const IntegralConstantId id : unit.integral_constants.Ids()) {
      Line(
          std::format(
              "[{}] {}", id.value,
              FormatIntegralConstant(unit.integral_constants.Get(id))));
    }
    Dedent();
    Line("Class:");
    Indent();
    if (const RootedTree* tree = RootedTreeOf(unit)) {
      DumpClass(tree->root, unit.GetClass(tree->root));
    }
    // The walk above descended the root's contained classes, so what is left
    // is every class no containment edge reaches -- one a handle reaches
    // instead. Which those are is what the walk just found out, so it is asked
    // rather than restated.
    for (const ClassId id : unit.classes.Ids()) {
      if (dumped_.contains(id.value)) {
        continue;
      }
      DumpClass(id, unit.GetClass(id));
    }
    Dedent();
    // A unit owns callables directly in its namespace: a package's own
    // subroutines, the program-global symbol of every DPI-C export it defines,
    // and, on a unit whose instances are a tree, the body that makes its
    // object.
    Line("Callables:");
    Indent();
    for (const CallableId id : unit.callables.Ids()) {
      DumpCallable(unit.named_callables, id, unit.callables.Get(id), id.value);
    }
    Dedent();
    // The foreign names this unit declares on a scope: a prototype no callable
    // of the unit carries, and the symbol the unit writes for it, which belongs
    // to no unit and so sits outside the pool above.
    if (!unit.foreign_scope_entries.empty()) {
      Line("ForeignScopeEntries:");
      Indent();
      for (std::size_t i = 0; i < unit.foreign_scope_entries.size(); ++i) {
        DumpForeignScopeEntry(unit.foreign_scope_entries[i], i);
      }
      Dedent();
    }
    // A description is an expression tree with no statements, so what is
    // dumped under each entry is its expressions and which of them is the
    // description.
    Line("TypeDescriptions:");
    Indent();
    for (const TypeDescriptorId id : unit.type_descriptors.Ids()) {
      const ValueBuild& described = unit.builds.descriptors.Get(id);
      Line(std::format("[{}] = Expr[{}]", id.value, described.value.value));
      Indent();
      DumpBlock(described.body);
      Dedent();
    }
    Dedent();
    if (unit.structs.size() > 0) {
      Line("Structs:");
      Indent();
      for (const StructId sid : unit.structs.Ids()) {
        DumpStruct(sid, unit.GetStruct(sid));
      }
      Dedent();
    }
    if (!unit.external_structs.empty()) {
      Line("ExternalStructs:");
      Indent();
      for (const ExternalStruct& external : unit.external_structs) {
        DumpExternalStruct(external);
      }
      Dedent();
    }
    if (unit.closures.size() > 0) {
      Line("Closures:");
      Indent();
      for (const ClosureId cid : unit.closures.Ids()) {
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

  static auto FormatStateKind(IntegralStateKind s) -> std::string_view {
    switch (s) {
      case IntegralStateKind::kTwoState:
        return "2";
      case IntegralStateKind::kFourState:
        return "4";
    }
    throw InternalError(
        "MirDumper::FormatStateKind: unknown IntegralStateKind");
  }

  static auto FormatSignedness(Signedness s) -> std::string_view {
    return s == Signedness::kSigned ? "signed" : "unsigned";
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

  static auto FormatTypeList(const std::vector<TypeId>& types) -> std::string {
    std::string out;
    for (const TypeId type : types) {
      if (!out.empty()) out += ", ";
      out += std::format("Type[{}]", type.value);
    }
    return out;
  }

  [[nodiscard]] auto FormatClassRef(const ClassRef& ref) const -> std::string {
    return std::visit(
        Overloaded{
            [this](const IntraUnitClassRef& i) -> std::string {
              return std::format(
                  "IntraUnit[#{}]{}", i.class_id.value,
                  FormatName(unit_->GetClass(i.class_id).name));
            },
            [](const CrossUnitClassRef& e) -> std::string {
              return std::format(
                  "CrossUnit(\"{}::{}\")", e.unit_name, e.class_name);
            },
            [](const ObjectTreeRootRef&) -> std::string {
              return "ObjectTreeRoot";
            },
            [](const ManagedObjectRootRef&) -> std::string {
              return "ManagedObjectRoot";
            }},
        ref);
  }

  [[nodiscard]] auto FormatClassRef(const DeclaredClassRef& ref) const
      -> std::string {
    return FormatClassRef(AsClassRef(ref));
  }

  [[nodiscard]] auto FormatVirtualSlot(const VirtualSlot& s) const
      -> std::string {
    return std::visit(
        Overloaded{
            [this](const LocalVirtualSlot& l) -> std::string {
              const auto& owner = unit_->GetClass(l.owner_class);
              return std::format(
                  "Class[{}].{}(Callable[{}])", l.owner_class.value,
                  CallableLabel(owner.named_callables, l.slot), l.slot.value);
            },
            [](const ExternalVirtualSlot& e) -> std::string {
              return std::format(
                  "External({}::{}#{})", e.unit_name, e.class_name,
                  e.ordinal.value);
            }},
        s);
  }

  static auto FormatType(const Type& t) -> std::string {
    return t.Visit(
        Overloaded{
            [](const PackedArrayType& p) -> std::string {
              return std::format(
                  "PackedArray(state={}, signed={}, dims={})",
                  FormatStateKind(p.state_kind), FormatSignedness(p.signedness),
                  FormatPackedDims(p.dims));
            },
            [](const EnumType& e) -> std::string {
              std::string members;
              for (std::size_t i = 0; i < e.members.size(); ++i) {
                if (i > 0) members += ", ";
                members += std::format(
                    "{}={}", e.members[i].name, FormatBits(e.members[i].value));
              }
              return std::format(
                  "Enum(base=PackedArray(state={}, signed={}, dims={}), "
                  "members=[{}])",
                  FormatStateKind(e.base.state_kind),
                  FormatSignedness(e.base.signedness),
                  FormatPackedDims(e.base.dims), members);
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
            [](const MachineBoolType&) -> std::string {
              return "MachineBoolType";
            },
            [](const MachineIntType& m) -> std::string {
              return std::format(
                  "MachineInt(width={}, signed={})", BitsOf(m.width),
                  m.signedness == Signedness::kSigned ? "true" : "false");
            },
            [](const MachineFloatType& m) -> std::string {
              return std::format("MachineFloat(width={})", BitsOf(m.width));
            },
            [](const MachineArrayType& m) -> std::string {
              return std::format(
                  "MachineArray(elem=Type[{}], size={})", m.element.value,
                  m.size);
            },
            [](const MachineFunctionType& m) -> std::string {
              std::string params;
              for (std::size_t i = 0; i < m.params.size(); ++i) {
                if (i > 0) params += ", ";
                params += std::format("Type[{}]", m.params[i].value);
              }
              return std::format(
                  "MachineFunction(({}) -> Type[{}])", params, m.result.value);
            },
            [](const EventType&) -> std::string { return "EventType"; },
            [](const RealType&) -> std::string { return "RealType"; },
            [](const ShortRealType&) -> std::string { return "ShortRealType"; },
            [](const ChandleType&) -> std::string { return "ChandleType"; },
            [](const VoidType&) -> std::string { return "VoidType"; },
            [](const ObjectType& o) -> std::string {
              return std::format("Object({})", FormatClass(o.of));
            },
            [](const RuntimeClassType& e) -> std::string {
              return std::format(
                  "RuntimeClass({})", support::RuntimeClassName(e.which));
            },
            [](const RuntimeEffectsType&) -> std::string {
              return "RuntimeEffects";
            },
            [](const FilesType&) -> std::string { return "Files"; },
            [](const DiagnosticType&) -> std::string { return "Diagnostic"; },
            [](const RuntimeLibraryType& r) -> std::string {
              switch (r.kind) {
                case RuntimeLibraryKind::kPackedType:
                  return "RuntimeLibrary(PackedType)";
                case RuntimeLibraryKind::kPackedRange:
                  return "RuntimeLibrary(PackedRange)";
                case RuntimeLibraryKind::kUnpackedRange:
                  return "RuntimeLibrary(UnpackedRange)";
                case RuntimeLibraryKind::kEnumeration:
                  return "RuntimeLibrary(Enumeration)";
                case RuntimeLibraryKind::kPrintItem:
                  return "RuntimeLibrary(PrintItem)";
                case RuntimeLibraryKind::kPrintLiteralItem:
                  return "RuntimeLibrary(PrintLiteralItem)";
                case RuntimeLibraryKind::kPrintValueItem:
                  return "RuntimeLibrary(PrintValueItem)";
                case RuntimeLibraryKind::kCancellationTarget:
                  return "RuntimeLibrary(CancellationTarget)";
                case RuntimeLibraryKind::kControlEffect:
                  return "RuntimeLibrary(ControlEffect)";
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
                case RuntimeLibraryKind::kObservation:
                  return "RuntimeLibrary(Observation)";
                case RuntimeLibraryKind::kReadReport:
                  return "RuntimeLibrary(ReadReport)";
                case RuntimeLibraryKind::kObjectDefinition:
                  return "RuntimeLibrary(ObjectDefinition)";
                case RuntimeLibraryKind::kDpiBitBuffer:
                  return "RuntimeLibrary(DpiBitBuffer)";
                case RuntimeLibraryKind::kDpiLogicBuffer:
                  return "RuntimeLibrary(DpiLogicBuffer)";
                case RuntimeLibraryKind::kDpiOpenArray:
                  return "RuntimeLibrary(DpiOpenArray)";
                case RuntimeLibraryKind::kDpiOpenArrayHandle:
                  return "RuntimeLibrary(DpiOpenArrayHandle)";
                case RuntimeLibraryKind::kDpiBitChunk:
                  return "RuntimeLibrary(DpiBitChunk)";
                case RuntimeLibraryKind::kDpiLogicChunk:
                  return "RuntimeLibrary(DpiLogicChunk)";
                case RuntimeLibraryKind::kScopeInfo:
                  return "RuntimeLibrary(ScopeInfo)";
                case RuntimeLibraryKind::kScopeCallable:
                  return "RuntimeLibrary(ScopeCallable)";
              }
              throw InternalError("dump: unknown RuntimeLibraryKind");
            },
            [](const CoroutineType& c) -> std::string {
              return std::format(
                  "Coroutine(payload=Type[{}])", c.payload.value);
            },
            [](const StructType& s) -> std::string {
              return std::visit(
                  Overloaded{
                      [](StructId id) {
                        return std::format("Struct[{}]", id.value);
                      },
                      [](const TypeDeclarationRef& ref) {
                        return std::format(
                            "Struct({}::{})", ref.unit_name, ref.name);
                      }},
                  s.declaration);
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
              return std::format(
                  "Tuple(elems=[{}])", FormatTypeList(t.elements));
            },
            [](const UnionType& u) -> std::string {
              return std::format(
                  "Union(members=[{}])", FormatTypeList(u.members));
            },
            [](const EmptyType&) -> std::string {
              return std::string{"Empty"};
            },
            [](const TaggedUnionType& u) -> std::string {
              return std::format(
                  "TaggedUnion(members=[{}])", FormatTypeList(u.members));
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
            [](const OpenWriteType& w) -> std::string {
              return std::format("OpenWrite(value=Type[{}])", w.value.value);
            },
            [](const DesignationType& d) -> std::string {
              return std::format("Designation(value=Type[{}])", d.value.value);
            },
            [](const ObjectWriteType& w) -> std::string {
              return std::format(
                  "ObjectWrite(object=Type[{}])", w.object.value);
            },
            [](const SampledHistoryType& h) -> std::string {
              return std::format(
                  "SampledHistory(value=Type[{}])", h.value.value);
            },
            [](const EvaluationAttemptsType&) -> std::string {
              return "EvaluationAttempts";
            },
        });
  }

  static auto FormatUnaryOp(UnaryOp op) -> std::string {
    switch (op) {
      case UnaryOp::kMinus:
        return "Minus";
      case UnaryOp::kBitwiseNot:
        return "BitwiseNot";
      case UnaryOp::kLogicalNot:
        return "LogicalNot";
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
      case BinaryOp::kBitwiseAnd:
        return "BitwiseAnd";
      case BinaryOp::kBitwiseOr:
        return "BitwiseOr";
      case BinaryOp::kBitwiseXor:
        return "BitwiseXor";
      case BinaryOp::kEquality:
        return "Equality";
      case BinaryOp::kInequality:
        return "Inequality";
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
              return std::format(
                  "OverridesIntraUnitSlot[Class[{}].{}(Callable[{}])]",
                  o.slot_owner.value,
                  CallableLabel(owner.named_callables, o.slot_id),
                  o.slot_id.value);
            },
            [](const OverridesExternalSlot& e) -> std::string {
              return std::format(
                  "OverridesExternalSlot[{}::{}#{}]", e.unit_name, e.class_name,
                  e.ordinal.value);
            },
            [](const OverridesLibraryVirtual& l) -> std::string {
              return std::format(
                  "OverridesLibraryVirtual[{}]",
                  support::LibraryVirtualName(l.function));
            }},
        role);
  }

  [[nodiscard]] static auto FormatMintedEntry(MintedEntry entry)
      -> std::string_view {
    switch (entry) {
      case MintedEntry::kInstallStorage:
        return "install_storage";
      case MintedEntry::kInitializeStorage:
        return "initialize_storage";
      case MintedEntry::kMakeObject:
        return "make_object";
    }
    throw InternalError("mir dump: unknown minted entry");
  }

  [[nodiscard]] auto FormatDirectTarget(const DirectTarget& target) const
      -> std::string {
    return std::visit(
        Overloaded{
            [this](const CallableTarget& c) -> std::string {
              return std::format(
                  R"(callable=Class[{}].{} "{}")", c.owner.value, c.slot.value,
                  CallableLabel(
                      unit_->GetClass(c.owner).named_callables, c.slot));
            },
            [this](const UnitCallableTarget& c) -> std::string {
              return std::format(
                  R"(callable=Unit.{} "{}")", c.slot.value,
                  CallableLabel(unit_->named_callables, c.slot));
            },
            [](const support::BuiltinFn& id) -> std::string {
              return std::format(
                  "builtin=\"{}\"", support::RuntimeEntryOf(id).name);
            },
            [](const ExternalUnitCallableTarget& e) -> std::string {
              return std::format(
                  R"(external_unit={}::{})", e.unit_name, e.callable_name);
            },
            [](const ExternalUnitClassMethodTarget& e) -> std::string {
              return std::format(
                  "external_class_method={}::{}::{}", e.unit_name, e.class_name,
                  e.method_name);
            },
            [](const StructMethodTarget& s) -> std::string {
              return std::format(
                  R"(struct_method={}::{} "{}")", s.declaration.unit_name,
                  s.declaration.name, support::ValueOperationName(s.answers));
            },
            [](const ExternalUnitMintedEntryTarget& e) -> std::string {
              return std::format(
                  "external_unit_minted_entry={}::{}", e.unit_name,
                  FormatMintedEntry(e.entry));
            },
            [](const ForeignSymbolTarget& f) -> std::string {
              return std::format("foreign_symbol=\"{}\"", f.linkage_name);
            }},
        target);
  }

  [[nodiscard]] auto FormatCallee(const Callee& callee) const -> std::string {
    return std::visit(
        Overloaded{
            [this](const Direct& d) -> std::string {
              const std::string receiver =
                  d.receiver.has_value()
                      ? std::format(" recv=Expr[{}]", d.receiver->value)
                      : std::string{};
              const std::string part =
                  d.part.has_value()
                      ? std::format(" at={}", FormatCallPart(*d.part))
                      : std::string{};
              return std::format(
                  "Direct[{}{}{}]", FormatDirectTarget(d.target), receiver,
                  part);
            },
            [](const Indirect& i) -> std::string {
              return std::format("Indirect[code=Expr[{}]]", i.code.value);
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

  [[nodiscard]] auto FormatReferenceTarget(const ReferenceTarget& target) const
      -> std::string {
    return std::visit(
        Overloaded{
            [this](const LocalRef& r) -> std::string {
              return std::format(
                  "LocalRef[var={}]{}", r.var.value,
                  FormatName(NameOf(code_->named_locals, r.var)));
            },
            [this](const DefinitionRef& r) -> std::string {
              return std::format("DefinitionRef of={}", FormatClassRef(r.of));
            },
            [](const TypeDescriptorRef& r) -> std::string {
              return std::format(
                  "TypeDescriptorRef Descriptor[{}]", r.descriptor.value);
            },
            [](const IntegralConstantRef& r) -> std::string {
              return std::format(
                  "IntegralConstantRef Const[{}]", r.constant.value);
            },
            [](const StaticPropertyRef& r) -> std::string {
              return std::format(
                  "StaticPropertyRef owner=Class[{}] prop=StaticProperty[{}]",
                  r.owner.value, r.prop.value);
            },
            [](const StaticVariableRef& r) -> std::string {
              return std::format(
                  "StaticVariableRef variable=StaticVariable[{}]",
                  r.variable.value);
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
            [](const ClassConstantRef& r) -> std::string {
              return std::format(
                  "ClassConstantRef owner=Class[{}] constant=Constant[{}]",
                  r.owner.value, r.constant.value);
            },
            [](const FunctionRef& r) -> std::string {
              return std::format(
                  "FunctionRef owner=Class[{}] body=Callable[{}]",
                  r.body.owner.value, r.body.slot.value);
            }},
        target);
  }

  [[nodiscard]] auto ResolveScopeAtHops(std::uint32_t hops) const
      -> const Class& {
    if (hops >= scope_stack_.size()) {
      throw InternalError("MirDumper::ResolveScopeAtHops: hops out of range");
    }
    return *scope_stack_[scope_stack_.size() - 1 - hops];
  }

  [[nodiscard]] auto FormatExpr(const Block& scope, ExprId id) const
      -> std::string {
    const auto& e = scope.exprs.Get(id);
    std::string formatted = std::visit(
        Overloaded{
            [](const StringLiteral& lit) -> std::string {
              return std::format("StringLiteral(\"{}\")", lit.value);
            },
            [](const MachineFloatLiteral& lit) -> std::string {
              return std::format("MachineFloatLiteral({})", lit.value);
            },
            [](const NullLiteral&) -> std::string { return "NullLiteral"; },
            [](const MachineBoolLiteral& lit) -> std::string {
              return std::format(
                  "MachineBoolLiteral({})", lit.value ? "true" : "false");
            },
            [](const MachineIntLiteral& lit) -> std::string {
              return std::format("MachineIntLiteral({})", lit.value);
            },
            [](const CastExpr& c) -> std::string {
              return std::format("CastExpr operand=Expr[{}]", c.operand.value);
            },
            [](const DynamicCastExpr& c) -> std::string {
              return std::format(
                  "DynamicCastExpr operand=Expr[{}]", c.operand.value);
            },
            [](const AddressOfExpr& a) -> std::string {
              return std::format(
                  "AddressOfExpr operand=Expr[{}]", a.operand.value);
            },
            [](const MoveExpr& m) -> std::string {
              return std::format("MoveExpr operand=Expr[{}]", m.operand.value);
            },
            [this](const ReferenceExpr& r) -> std::string {
              return std::format(
                  "ReferenceExpr[{}]", FormatReferenceTarget(r.target));
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
            [](const BlockExpr& b) -> std::string {
              return std::format(
                  "BlockExpr scope=BlockId{{{}}} value=Expr[{}]", b.scope.value,
                  b.value.value);
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
            [this](const CallExpr& c) -> std::string {
              return std::format(
                  "CallExpr callee={} args=[{}]", FormatCallee(c.callee),
                  FormatExprList(c.arguments));
            },
            [](const FieldAccessExpr& m) -> std::string {
              return std::format(
                  "FieldAccessExpr receiver=Expr[{}] field={}",
                  m.receiver.value,
                  std::visit(
                      [](const auto& t) { return FormatField(t); }, m.field));
            },
            [](const DerefExpr& d) -> std::string {
              return std::format("DerefExpr pointer=Expr[{}]", d.pointer.value);
            },
            [](const ClosureExpr& cl) -> std::string {
              return std::format(
                  "ClosureExpr closure=Closure[{}] field_inits={}",
                  cl.closure.value, cl.field_inits.size());
            },
            [](const CompositeExpr& c) -> std::string {
              return std::format(
                  "CompositeExpr parts=[{}]", FormatExprList(c.parts));
            },
            [](const AwaitExpr& a) -> std::string {
              return std::format(
                  "AwaitExpr execution=Expr[{}]", a.execution.value);
            },
            [](const WaitExpr& w) -> std::string {
              return std::format(
                  "WaitExpr registration=Expr[{}]", w.registration.value);
            },
            [](const VectorGetExpr& g) -> std::string {
              return std::format(
                  "VectorGetExpr vector=Expr[{}] index=Expr[{}]",
                  g.vector.value, g.index.value);
            },
        },
        e.data);
    return std::format("{} type=Type[{}]", formatted, e.type.value);
  }

  static auto FormatVarType(TypeId type) -> std::string {
    return std::format("Type[{}]", type.value);
  }

  // The identifier a declaration answers to, quoted, or nothing at all where
  // the source declared no such thing. Printing the absence as an empty string
  // rather than a placeholder is the point: what a reader learns from this dump
  // is which declarations the design wrote.
  static auto FormatName(std::optional<std::string_view> name) -> std::string {
    return name.has_value() ? std::format(" \"{}\"", *name) : std::string{};
  }

  static auto FormatName(const std::optional<std::string>& name)
      -> std::string {
    return name.has_value() ? std::format(" \"{}\"", *name) : std::string{};
  }

  // A constant is an expression tree with no statements, so what is dumped
  // under each is its expressions and which of them is the constant.
  void DumpValueBuild(std::string_view what, const ValueBuild& build) {
    Line(std::format("{} = Expr[{}]", what, build.value.value));
    Indent();
    DumpBlock(build.body);
    Dedent();
  }

  void DumpClass(ClassId id, const Class& s) {
    dumped_.insert(id.value);
    scope_stack_.push_back(&s);
    const std::string kind = s.is_interface_class ? "InterfaceClass" : "Class";
    Line(std::format("{}{} (#{})", kind, FormatName(s.name), id.value));
    Indent();

    for (const std::string& alias : s.aliases) {
      Line(std::format("Alias: {}", alias));
    }
    if (s.base.has_value()) {
      Line(std::format("Base: {}", FormatClassRef(*s.base)));
    }

    for (const auto& impl : s.implements) {
      Line(std::format("Implements: {}", FormatClassRef(impl)));
    }
    for (const ConformingBehavior& answered : s.conforming) {
      Line(
          std::format(
              "Conforms: {} <- {}",
              FormatVirtualSlot(answered.interface_behavior),
              answered.answered_by.has_value()
                  ? FormatVirtualSlot(*answered.answered_by)
                  : std::string{"nothing"}));
    }
    for (const ClassConstantId constant : s.constants.Ids()) {
      DumpValueBuild(
          std::format("Constant[{}]", constant.value),
          s.constants.Get(constant).initializer);
    }
    DumpValueBuild("ObjectDefinition", s.object_definition_initializer);

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
    DumpFieldList(s.fields, s.named_fields);
    Dedent();

    Line("StaticProperties:");
    Indent();
    for (const StaticPropertyId id : s.static_properties.Ids()) {
      const StaticPropertyDecl& p = s.static_properties.Get(id);
      Line(
          std::format(
              "[{}]{} : {}", id.value,
              FormatName(NameOf(s.named_static_properties, id)),
              FormatVarType(p.type)));
    }
    Dedent();

    Line("Callables:");
    Indent();
    for (const CallableId id : s.callables.Ids()) {
      DumpCallable(s.named_callables, id, s.callables.Get(id), id.value);
    }
    Dedent();

    if (s.constructor.has_value()) {
      const ConstructorDecl& ctor = *s.constructor;
      Line("Constructor:");
      Indent();
      if (!ctor.base_args.empty()) {
        Line("BaseArgs:");
        Indent();
        for (std::size_t i = 0; i < ctor.base_args.size(); ++i) {
          Line(std::format("[{}] Expr[{}]", i, ctor.base_args[i].value));
        }
        Dedent();
      }
      Line("Body:");
      Indent();
      DumpCallableBody(ctor.code);
      Dedent();
      Dedent();
    }

    Dedent();
    scope_stack_.pop_back();
  }

  void DumpFieldList(
      const base::Arena<FieldDecl, FieldId>& fields,
      std::span<const NamedField> named) {
    for (const FieldId id : fields.Ids()) {
      Line(
          std::format(
              "[{}]{} : {}", id.value, FormatName(NameOf(named, id)),
              FormatVarType(fields.Get(id).type)));
    }
  }

  void DumpForeignLinkage(const ForeignLinkage& linkage) {
    Line(std::format(R"(ForeignLinkage: c_name="{}")", linkage.foreign_name));
  }

  void DumpCallable(
      std::span<const NamedCallable> named, CallableId id,
      const CallableDecl& d, std::size_t index) {
    Line(
        std::format(
            R"([{}] "{}"{} : Type[{}])", index, CallableLabel(named, id),
            d.code.body.has_value() ? "" : " declaration",
            d.code.result_type.value));
    Indent();
    if (d.virtual_dispatch.has_value()) {
      Line(
          std::format(
              "VirtualDispatch: {}",
              FormatVirtualDispatchRole(*d.virtual_dispatch)));
    }
    if (d.foreign.has_value()) {
      DumpForeignLinkage(*d.foreign);
    }
    DumpParams(d.code);
    if (d.code.body.has_value()) {
      DumpCallableBody(d.code);
    }
    Dedent();
  }

  void DumpStruct(StructId id, const StructDecl& decl) {
    Line(
        std::format(
            R"(Struct (#{}) "{}" elems=[{}])", id.value, decl.name,
            FormatTypeList(decl.elements)));
    Indent();
    for (const StructMethod& method : decl.methods) {
      Line(
          std::format(
              R"(Method "{}" : Type[{}])",
              support::ValueOperationName(method.answers),
              method.code.result_type.value));
      Indent();
      DumpParams(method.code);
      DumpCallableBody(method.code);
      Dedent();
    }
    Dedent();
  }

  void DumpExternalStruct(const ExternalStruct& external) {
    Line(
        std::format(
            "Struct({}::{}) elems=[{}]", external.declaration.unit_name,
            external.declaration.name, FormatTypeList(external.elements)));
  }

  void DumpParams(const CallableCode& code) {
    for (std::size_t i = 0; i < code.params.size(); ++i) {
      const LocalId param = code.params[i];
      Line(
          std::format(
              "Param[{}]{} : Type[{}]", i,
              FormatName(NameOf(code.named_locals, param)),
              code.locals.Get(param).type.value));
    }
  }

  void DumpClosure(ClosureId id, const ClosureDecl& decl) {
    Line(std::format("Closure (#{})", id.value));
    Indent();
    Line("Captures:");
    Indent();
    DumpFieldList(decl.fields, {});
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

  void DumpForeignScopeEntry(const ForeignScopeEntry& e, std::size_t index) {
    Line(std::format("[{}] : Type[{}]", index, e.signature.value));
    Indent();
    DumpForeignLinkage(e.linkage);
    DumpParams(e.definition);
    DumpCallableBody(e.definition);
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
      for (const LocalId id : code.locals.Ids()) {
        Line(
            std::format(
                "Local[{}]{} : Type[{}]", id.value,
                FormatName(NameOf(code.named_locals, id)),
                code.locals.Get(id).type.value));
      }
      Dedent();
    }
    DumpBlock(code.Body());
    code_ = saved;
  }

  void DumpBlock(const Block& scope) {
    if (!scope.exprs.empty()) {
      Line("Exprs:");
      Indent();
      for (const ExprId id : scope.exprs.Ids()) {
        Line(std::format("Expr[{}] {}", id.value, FormatExpr(scope, id)));
        const auto& expr = scope.exprs.Get(id);
        if (const auto* cl = std::get_if<ClosureExpr>(&expr.data)) {
          Indent();
          DumpClosureExpr(*cl);
          Dedent();
        }
        if (const auto* be = std::get_if<BlockExpr>(&expr.data)) {
          Indent();
          DumpBlock(scope.child_scopes.Get(be->scope));
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
    Line(
        std::format(
            "Stmt[{}] LocalDeclStmt target=LocalRef[var={}]{}", id.value,
            s.target.value, FormatName(NameOf(code_->named_locals, s.target))));
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
            [&](const TryStmt& s) { DumpTryStmt(enclosing, s, id); },
            [&](const RaiseStmt& s) { DumpRaiseStmt(enclosing, s, id); },
            [&](const FinallyStmt& s) { DumpFinallyStmt(enclosing, s, id); },
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
                Line(
                    std::format(
                        "[{}] decl LocalRef[var={}]{}{}", i,
                        d.induction_var.value,
                        FormatName(
                            NameOf(code_->named_locals, d.induction_var)),
                        init_str));
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

  void DumpTryStmt(const Block& enclosing, const TryStmt& s, StmtId id) {
    Line(
        std::format(
            "Stmt[{}] TryStmt body=BlockId{{{}}} caught=Local[{}] "
            "handler=BlockId{{{}}}",
            id.value, s.body.value, s.caught.value, s.handler.value));
    Indent();
    DumpBlock(enclosing.child_scopes.Get(s.body));
    DumpBlock(enclosing.child_scopes.Get(s.handler));
    Dedent();
  }

  void DumpFinallyStmt(
      const Block& enclosing, const FinallyStmt& s, StmtId id) {
    Line(
        std::format(
            "Stmt[{}] FinallyStmt body=BlockId{{{}}} cleanup=BlockId{{{}}}",
            id.value, s.body.value, s.cleanup.value));
    Indent();
    DumpBlock(enclosing.child_scopes.Get(s.body));
    DumpBlock(enclosing.child_scopes.Get(s.cleanup));
    Dedent();
  }

  void DumpRaiseStmt(const Block& enclosing, const RaiseStmt& s, StmtId id) {
    Line(
        std::format(
            "Stmt[{}] RaiseStmt effect=Expr[{}]", id.value, s.effect.value));
    Indent();
    Line(
        std::format(
            "Expr[{}] {}", s.effect.value, FormatExpr(enclosing, s.effect)));
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
  // The classes the walk has already printed, so a class reached by
  // containment is not printed a second time at the top level.
  std::set<std::size_t> dumped_;
  const CompilationUnit* unit_ = nullptr;
  const CallableCode* code_ = nullptr;
};

}  // namespace

auto DumpMir(const CompilationUnit& unit) -> std::string {
  MirDumper dumper;
  return dumper.Dump(unit);
}

}  // namespace lyra::mir
