#include "lyra/backend/cpp/render_type.hpp"

#include <expected>
#include <span>
#include <string_view>
#include <utility>
#include <variant>

#include "lyra/backend/cpp/naming.hpp"
#include "lyra/backend/cpp/target_text.hpp"
#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/mir/class_id.hpp"
#include "lyra/mir/class_ref.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/type.hpp"
#include "lyra/mir/type_builders.hpp"
#include "lyra/support/def_path.hpp"
#include "lyra/support/runtime_class.hpp"

namespace lyra::backend::cpp {

auto BodyCleanupExtentCppType() -> std::string_view {
  return "lyra::runtime::ScopeExit";
}

auto SuspensionCppType() -> std::string_view {
  return "lyra::runtime::Suspension";
}

namespace {

auto RuntimeClassCppType(support::RuntimeClass klass) -> std::string_view {
  switch (klass) {
    case support::RuntimeClass::kScope:
      return "lyra::runtime::Scope";
    case support::RuntimeClass::kObject:
      return "lyra::runtime::GcObject";
    case support::RuntimeClass::kProcess:
      return "lyra::runtime::RuntimeProcess";
  }
  throw InternalError("backend::cpp: unknown runtime class");
}

void WriteTypeList(
    TargetText& out, const mir::CompilationUnit& unit,
    std::span<const mir::TypeId> types) {
  WriteSeparated(out, types, ", ", [&](mir::TypeId type) {
    Write(out, CppType(unit, type));
  });
}

auto RuntimeLibraryCppType(mir::RuntimeLibraryKind kind) -> std::string_view {
  switch (kind) {
    case mir::RuntimeLibraryKind::kPackedType:
      return "lyra::value::PackedType";
    case mir::RuntimeLibraryKind::kPackedRange:
      return "lyra::value::PackedRange";
    case mir::RuntimeLibraryKind::kUnpackedRange:
      return "lyra::value::UnpackedRange";
    case mir::RuntimeLibraryKind::kEnumeration:
      return "lyra::value::Enumeration";
    case mir::RuntimeLibraryKind::kPrintItem:
      return "lyra::value::PrintItem";
    case mir::RuntimeLibraryKind::kPrintLiteralItem:
      return "lyra::value::PrintLiteralItem";
    case mir::RuntimeLibraryKind::kPrintValueItem:
      return "lyra::value::PrintValueItem";
    case mir::RuntimeLibraryKind::kCancellationTarget:
      return "lyra::runtime::CancellationTarget";
    case mir::RuntimeLibraryKind::kControlEffect:
      return "lyra::runtime::ControlEffect";
    case mir::RuntimeLibraryKind::kFormatSpec:
      return "lyra::value::FormatSpec";
    case mir::RuntimeLibraryKind::kFormatArg:
      return "lyra::value::FormatArg";
    case mir::RuntimeLibraryKind::kChannelCancellation:
      return "lyra::runtime::ChannelCancellation";
    case mir::RuntimeLibraryKind::kTimeFormat:
      return "lyra::value::TimeFormat";
    case mir::RuntimeLibraryKind::kHierarchySegment:
      return "lyra::runtime::HierarchySegment";
    case mir::RuntimeLibraryKind::kTrigger:
      return "lyra::runtime::Trigger";
    case mir::RuntimeLibraryKind::kObservation:
      return "lyra::runtime::Observation";
    case mir::RuntimeLibraryKind::kReadReport:
      return "lyra::runtime::ReadReport";
    case mir::RuntimeLibraryKind::kWait:
      return "lyra::runtime::Wait";
    case mir::RuntimeLibraryKind::kObjectDefinition:
      return "lyra::runtime::ObjectDefinition";
    case mir::RuntimeLibraryKind::kScopeInfo:
      return "lyra::runtime::ScopeInfo";
    case mir::RuntimeLibraryKind::kScopeCallable:
      return "lyra::runtime::ScopeCallable";
    case mir::RuntimeLibraryKind::kDpiBitBuffer:
      return "lyra::value::DpiBitBuffer";
    case mir::RuntimeLibraryKind::kDpiLogicBuffer:
      return "lyra::value::DpiLogicBuffer";
    case mir::RuntimeLibraryKind::kDpiBitChunk:
      return "svBitVecVal";
    case mir::RuntimeLibraryKind::kDpiLogicChunk:
      return "svLogicVecVal";
    case mir::RuntimeLibraryKind::kDpiOpenArray:
      return "lyra::value::DpiOpenArray";
    case mir::RuntimeLibraryKind::kDpiOpenArrayHandle:
      return "const svOpenArrayHandle";
  }
  throw InternalError("backend::cpp: unknown RuntimeLibraryKind");
}

auto MachineIntCppType(const mir::MachineIntType& m) -> std::string_view {
  const bool is_signed = m.signedness == mir::Signedness::kSigned;
  switch (m.width) {
    case mir::MachineIntWidth::k8:
      return is_signed ? "std::int8_t" : "std::uint8_t";
    case mir::MachineIntWidth::k16:
      return is_signed ? "std::int16_t" : "std::uint16_t";
    case mir::MachineIntWidth::k32:
      return is_signed ? "std::int32_t" : "std::uint32_t";
    case mir::MachineIntWidth::k64:
      return is_signed ? "std::int64_t" : "std::uint64_t";
  }
  throw InternalError("backend::cpp: unknown MachineIntWidth");
}

auto MachineFloatCppType(const mir::MachineFloatType& m) -> std::string_view {
  switch (m.width) {
    case mir::MachineFloatWidth::k32:
      return "float";
    case mir::MachineFloatWidth::k64:
      return "double";
  }
  throw InternalError("backend::cpp: unknown MachineFloatWidth");
}

}  // namespace

void WriteOne(TargetText& out, const CppType& spelling) {
  const mir::CompilationUnit& unit = spelling.Unit();
  const mir::TypeId type_id = spelling.Type();
  const auto type = [&unit](mir::TypeId t) { return CppType(unit, t); };
  unit.types.Get(type_id).Visit(
      Overloaded{
          [&](const mir::PackedArrayType&) {
            out += "lyra::value::PackedArray";
          },
          // An enum holds a packed array; its member names do not change how
          // the value is stored.
          [&](const mir::EnumType&) { out += "lyra::value::PackedArray"; },
          [&](const mir::StringType&) { out += "lyra::value::String"; },
          [&](const mir::MachineCStringType&) { out += "const char*"; },
          [&](const mir::MachineBoolType&) { out += "bool"; },
          [&](const mir::MachineIntType& m) { out += MachineIntCppType(m); },
          [&](const mir::MachineFloatType& m) {
            out += MachineFloatCppType(m);
          },
          [&](const mir::MachineArrayType& m) {
            Write(out, "std::array<", type(m.element), ", ", m.size, ">");
          },
          [&](const mir::MachineFunctionType& m) {
            // `R (*)(Args)` is only valid where a type stands alone, since a
            // declared function pointer's name goes inside the parentheses. A
            // restored prototype is only ever used that way; the erased
            // function type does get declared, so it uses the runtime's alias.
            if (mir::IsErasedFunction(unit.types, type_id)) {
              out += "lyra::runtime::ErasedEntry";
              return;
            }
            Write(out, type(m.result), " (*)(");
            WriteTypeList(out, unit, m.params);
            out += ")";
          },
          [&](const mir::ChandleType&) { out += "lyra::value::Chandle"; },
          [&](const mir::EventType&) { out += "lyra::runtime::NamedEvent"; },
          [&](const mir::RealType&) { out += "lyra::value::Real"; },
          [&](const mir::ShortRealType&) { out += "lyra::value::ShortReal"; },
          [&](const mir::UnpackedArrayType& ua) {
            Write(
                out, "lyra::value::UnpackedArray<", type(ua.element_type), ">");
          },
          [&](const mir::DynamicArrayType& da) {
            Write(
                out, "lyra::value::DynamicArray<", type(da.element_type), ">");
          },
          [&](const mir::QueueType& q) {
            Write(out, "lyra::value::Queue<", type(q.element_type), ">");
          },
          [&](const mir::AssociativeArrayType& a) {
            Write(
                out, "lyra::value::AssociativeArray<", type(a.key_type), ", ",
                type(a.element_type), ">");
          },
          [&](const mir::WildcardIndexType&) {
            out += "lyra::value::WildcardKey";
          },
          [&](const mir::ObjectType& o) {
            Write(out, CppClassRef(unit, o.of));
          },
          [&](const mir::StructType& s) {
            std::visit(
                Overloaded{
                    [&](mir::StructId id) {
                      Write(
                          out,
                          CppStructRef(unit.name, unit.GetStruct(id).path));
                    },
                    [&](const mir::TypeDeclarationRef& ref) {
                      Write(out, CppStructRef(ref.unit_name, ref.path));
                    }},
                s.declaration);
          },
          [&](const mir::RuntimeClassType& e) {
            out += RuntimeClassCppType(e.which);
          },
          [&](const mir::RuntimeEffectsType&) {
            out += "lyra::runtime::RuntimeEffects&";
          },
          [&](const mir::FilesType&) { out += "lyra::runtime::FileTable&"; },
          [&](const mir::DiagnosticType&) {
            out += "lyra::runtime::DiagnosticDispatcher&";
          },
          [&](const mir::RuntimeLibraryType& r) {
            out += RuntimeLibraryCppType(r.kind);
          },
          [&](const mir::CoroutineType& c) {
            Write(out, "lyra::runtime::Coroutine<", type(c.payload), ">");
          },
          [&](const mir::RefType& r) {
            if (r.mutability == mir::Mutability::kReadOnly) {
              out += "const ";
            }
            Write(out, "lyra::runtime::Ref<", type(r.pointee), ">");
          },
          [&](const mir::VoidType&) { out += "void"; },
          [&](const mir::PointerType& p) {
            switch (p.ownership) {
              case mir::PointerOwnership::kUnique:
                Write(out, "std::unique_ptr<", type(p.pointee), ">");
                return;
              case mir::PointerOwnership::kShared:
                Write(out, "std::shared_ptr<", type(p.pointee), ">");
                return;
              case mir::PointerOwnership::kBorrowed:
                // A borrowed pointer is a plain pointer to the pointee's
                // type, a `Var<T>` included; a read-only one is `const T*`.
                if (p.mutability == mir::Mutability::kReadOnly) {
                  out += "const ";
                }
                Write(out, type(p.pointee), "*");
                return;
            }
            throw InternalError("backend::cpp: unknown PointerOwnership");
          },
          // Every object reference is one type, whatever class it is seen as.
          // The class is written where the reference is used, because two
          // units may hold one cell as different classes, and a type per
          // class would give that cell two types.
          [&](const mir::ManagedRefType&) { out += "lyra::value::ObjectRef"; },
          [&](const mir::VectorType& v) {
            Write(out, "std::vector<", type(v.element), ">");
          },
          // A tuple is its components, so it is the library's product of them.
          [&](const mir::TupleType& t) {
            Write(out, CppTupleComponents{.unit = &unit, .of = t.elements});
          },
          [&](const mir::UnionType& u) {
            out += "lyra::value::Union<";
            WriteTypeList(out, unit, u.members);
            out += ">";
          },
          [&](const mir::EmptyType&) { out += "lyra::value::Empty"; },
          [&](const mir::TaggedUnionType& u) {
            out += "lyra::value::TaggedUnion<";
            WriteTypeList(out, unit, u.members);
            out += ">";
          },
          [&](const mir::ObservableType& o) {
            Write(out, "lyra::runtime::Var<", type(o.value), ">");
          },
          [&](const mir::ResolvedType& r) {
            Write(out, "lyra::runtime::ResolvedNet<", type(r.value), ">");
          },
          [&](const mir::DriverType& d) {
            Write(out, "lyra::runtime::Driver<", type(d.value), ">");
          },
          // A write, and a place designated within it, live and end within
          // the expression that opens the write, so nothing ever holds one
          // under a name.
          [](const mir::OpenWriteType&) {
            throw InternalError(
                "backend::cpp: a write in progress ends with the expression "
                "that opens it, so nothing names its type -- please report "
                "this as a bug");
          },
          [](const mir::DesignationType&) {
            throw InternalError(
                "backend::cpp: a place designated within a write ends with "
                "the expression that opens the write, so nothing names its "
                "type -- please report this as a bug");
          },
          // Named where it is opened, by constructing it on the object.
          [&](const mir::ObjectWriteType& w) {
            Write(out, "lyra::runtime::ObjectWrite<", type(w.object), ">");
          },
          [&](const mir::SampledHistoryType& h) {
            Write(out, "lyra::runtime::SampledHistory<", type(h.value), ">");
          },
          [&](const mir::EvaluationAttemptsType&) {
            out += "lyra::runtime::EvaluationAttempts";
          },
          [&](const mir::ClosureType& c) {
            Write(out, CppClosureName(c.closure_id));
          },
      });
}

auto DerefSpellingAsCpp(const mir::CompilationUnit& unit, mir::TypeId type_id)
    -> DerefSpelling {
  const auto reaches_nothing = []() -> DerefSpelling {
    throw InternalError(
        "backend::cpp: a value of this type reaches nothing, so there is "
        "nothing for a dereference to open -- please report this as a bug");
  };
  return unit.types.Get(type_id).Visit(
      Overloaded{
          [&](const mir::PointerType&) -> DerefSpelling {
            return DerefByOperator{};
          },
          // A `ref` is dereferenced like a pointer, which reaches the cell it
          // is bound to (LRM 23.3.3.2).
          [&](const mir::RefType&) -> DerefSpelling {
            return DerefByOperator{};
          },
          // A place designated within a write is dereferenced like a pointer,
          // which reaches it, where the write lands. The write itself names no
          // place.
          [&](const mir::DesignationType&) -> DerefSpelling {
            return DerefByOperator{};
          },
          [&](const mir::OpenWriteType&) { return reaches_nothing(); },
          // A write in progress into an object is dereferenced to the object,
          // as a guard is to what it guards.
          [&](const mir::ObjectWriteType&) -> DerefSpelling {
            return DerefByOperator{};
          },
          [&](const mir::ManagedRefType& m) -> DerefSpelling {
            return DerefThroughView{.pointee = m.pointee};
          },
          // Every other type is a value that reaches nothing, so there is
          // nothing to dereference.
          [&](const mir::PackedArrayType&) { return reaches_nothing(); },
          [&](const mir::EnumType&) { return reaches_nothing(); },
          [&](const mir::UnpackedArrayType&) { return reaches_nothing(); },
          [&](const mir::DynamicArrayType&) { return reaches_nothing(); },
          [&](const mir::QueueType&) { return reaches_nothing(); },
          [&](const mir::AssociativeArrayType&) { return reaches_nothing(); },
          [&](const mir::WildcardIndexType&) { return reaches_nothing(); },
          [&](const mir::StringType&) { return reaches_nothing(); },
          [&](const mir::MachineCStringType&) { return reaches_nothing(); },
          [&](const mir::MachineBoolType&) { return reaches_nothing(); },
          [&](const mir::MachineIntType&) { return reaches_nothing(); },
          [&](const mir::MachineFloatType&) { return reaches_nothing(); },
          [&](const mir::MachineArrayType&) { return reaches_nothing(); },
          [&](const mir::MachineFunctionType&) { return reaches_nothing(); },
          [&](const mir::EventType&) { return reaches_nothing(); },
          [&](const mir::RealType&) { return reaches_nothing(); },
          [&](const mir::ShortRealType&) { return reaches_nothing(); },
          [&](const mir::ChandleType&) { return reaches_nothing(); },
          [&](const mir::VoidType&) { return reaches_nothing(); },
          [&](const mir::EmptyType&) { return reaches_nothing(); },
          [&](const mir::ObjectType&) { return reaches_nothing(); },
          [&](const mir::RuntimeClassType&) { return reaches_nothing(); },
          [&](const mir::RuntimeEffectsType&) { return reaches_nothing(); },
          [&](const mir::FilesType&) { return reaches_nothing(); },
          [&](const mir::DiagnosticType&) { return reaches_nothing(); },
          [&](const mir::RuntimeLibraryType&) { return reaches_nothing(); },
          [&](const mir::CoroutineType&) { return reaches_nothing(); },
          [&](const mir::VectorType&) { return reaches_nothing(); },
          [&](const mir::TupleType&) { return reaches_nothing(); },
          [&](const mir::UnionType&) { return reaches_nothing(); },
          [&](const mir::TaggedUnionType&) { return reaches_nothing(); },
          [&](const mir::ObservableType&) { return reaches_nothing(); },
          [&](const mir::ResolvedType&) { return reaches_nothing(); },
          [&](const mir::DriverType&) { return reaches_nothing(); },
          [&](const mir::SampledHistoryType&) { return reaches_nothing(); },
          [&](const mir::EvaluationAttemptsType&) { return reaches_nothing(); },
          [&](const mir::StructType&) { return reaches_nothing(); },
          [&](const mir::ClosureType&) { return reaches_nothing(); },
      });
}

namespace {

auto IsMachineScalar(const mir::Type& type) -> bool {
  return type.Is<mir::MachineIntType>() || type.Is<mir::MachineBoolType>() ||
         type.Is<mir::MachineFloatType>();
}

// The values C++ cast notation converts among by itself: both sides one of
// these, and the same one.
auto CastNotationRelates(const mir::Type& from, const mir::Type& to) -> bool {
  return (from.IsIntegralPacked() && to.IsIntegralPacked()) ||
         (IsMachineScalar(from) && IsMachineScalar(to)) ||
         (from.Is<mir::PointerType>() && to.Is<mir::PointerType>()) ||
         (from.Is<mir::MachineFunctionType>() &&
          to.Is<mir::MachineFunctionType>());
}

// A value the runtime answers "is this nothing" for, which is what reducing
// one to a machine boolean asks (LRM 12.4).
auto HasTruthValue(const mir::Type& type) -> bool {
  return type.IsIntegralPacked() || type.IsRealFamily() ||
         IsMachineScalar(type) || type.Is<mir::ManagedRefType>() ||
         type.Is<mir::ChandleType>();
}

// A refusal naming what it refuses in the target's own spelling of the types
// involved, which is the vocabulary this backend has for them.
template <typename... Pieces>
auto Refusal(diag::DiagCode code, const Pieces&... pieces)
    -> std::unexpected<diag::Diagnostic> {
  TargetText message;
  Write(message, pieces...);
  return diag::Fail(code, std::move(message).Take());
}

}  // namespace

auto ConversionAsCpp(
    const mir::CompilationUnit& unit, mir::TypeId from, mir::TypeId to)
    -> diag::Result<Conversion> {
  const mir::Type& source = unit.types.Get(from);
  const mir::Type& destination = unit.types.Get(to);
  const auto* from_ref = source.As<mir::ManagedRefType>();
  const auto* to_ref = destination.As<mir::ManagedRefType>();
  if (from_ref != nullptr && to_ref != nullptr) {
    return ConvertedThroughView{
        .from = from_ref->pointee, .to = to_ref->pointee};
  }
  if (CastNotationRelates(source, destination) ||
      (destination.Is<mir::MachineBoolType>() && HasTruthValue(source))) {
    return ConvertedByCastNotation{.to = to};
  }
  return Refusal(
      diag::DiagCode::kUnsupportedConversionForm,
      "the C++ backend has no conversion from `", CppType(unit, from), "` to `",
      CppType(unit, to), "`");
}

auto NullSpellingAsCpp(const mir::CompilationUnit& unit, mir::TypeId type)
    -> diag::Result<NullSpelling> {
  const mir::Type& t = unit.types.Get(type);
  if (t.Is<mir::ManagedRefType>() || t.Is<mir::ChandleType>()) {
    return NullAsEmptyValue{.type = type};
  }
  if (t.Is<mir::PointerType>() || t.Is<mir::MachineFunctionType>()) {
    return NullAsNullAddress{};
  }
  return Refusal(
      diag::DiagCode::kUnsupportedExpressionForm,
      "the C++ backend has no value of `", CppType(unit, type),
      "` that names nothing");
}

void WriteNull(
    TargetText& out, const mir::CompilationUnit& unit,
    const NullSpelling& spelling) {
  std::visit(
      Overloaded{
          [&](const NullAsEmptyValue& empty) {
            Write(out, CppType(unit, empty.type), "{}");
          },
          [&](NullAsNullAddress) { out += "nullptr"; }},
      spelling);
}

void WriteOne(TargetText& out, const CppConstructorName& constructor) {
  const mir::CompilationUnit& unit = constructor.of.Unit();
  const mir::TypeId type_id = constructor.of.Type();
  // Most types are constructed by naming them, `T(args)`, with the name the
  // type mapping gives.
  const auto by_naming_itself = [&](const auto&) {
    Write(out, constructor.of);
  };
  unit.types.Get(type_id).Visit(
      Overloaded{
          // An owning pointer is made by the function that allocates and
          // constructs the pointee, `std::make_unique<T>(args)`. A borrowed
          // pointer points at storage that already exists and is never
          // constructed.
          [&](const mir::PointerType& p) {
            switch (p.ownership) {
              case mir::PointerOwnership::kUnique:
                Write(out, "std::make_unique<", CppType(unit, p.pointee), ">");
                return;
              case mir::PointerOwnership::kShared:
                Write(out, "std::make_shared<", CppType(unit, p.pointee), ">");
                return;
              case mir::PointerOwnership::kBorrowed:
                throw InternalError(
                    "backend::cpp: a borrowed pointer is bound to storage "
                    "that already exists, so none is constructed");
            }
            throw InternalError("backend::cpp: unknown PointerOwnership");
          },
          [&](const mir::ManagedRefType& m) {
            Write(
                out, "lyra::runtime::GcNew<",
                CppClassRef(unit, mir::ClassOfObject(unit.types, m.pointee)),
                ">");
          },
          // A sequence is built by the library function that takes its
          // elements, since the container type takes no element list.
          [&](const mir::VectorType& v) {
            Write(
                out, "lyra::runtime::MakeSequence<", CppType(unit, v.element),
                ">");
          },
          [&](const mir::PackedArrayType& t) { by_naming_itself(t); },
          [&](const mir::EnumType& t) { by_naming_itself(t); },
          [&](const mir::StringType& t) { by_naming_itself(t); },
          [&](const mir::MachineCStringType& t) { by_naming_itself(t); },
          [&](const mir::MachineBoolType& t) { by_naming_itself(t); },
          [&](const mir::MachineIntType& t) { by_naming_itself(t); },
          [&](const mir::MachineFloatType& t) { by_naming_itself(t); },
          [&](const mir::MachineArrayType& t) { by_naming_itself(t); },
          [&](const mir::MachineFunctionType& t) { by_naming_itself(t); },
          [&](const mir::ChandleType& t) { by_naming_itself(t); },
          [&](const mir::EventType& t) { by_naming_itself(t); },
          [&](const mir::RealType& t) { by_naming_itself(t); },
          [&](const mir::ShortRealType& t) { by_naming_itself(t); },
          [&](const mir::UnpackedArrayType& t) { by_naming_itself(t); },
          [&](const mir::DynamicArrayType& t) { by_naming_itself(t); },
          [&](const mir::QueueType& t) { by_naming_itself(t); },
          [&](const mir::AssociativeArrayType& t) { by_naming_itself(t); },
          [&](const mir::WildcardIndexType& t) { by_naming_itself(t); },
          [&](const mir::ObjectType& t) { by_naming_itself(t); },
          [&](const mir::StructType& t) { by_naming_itself(t); },
          [&](const mir::RuntimeClassType& t) { by_naming_itself(t); },
          [&](const mir::RuntimeEffectsType& t) { by_naming_itself(t); },
          [&](const mir::FilesType& t) { by_naming_itself(t); },
          [&](const mir::DiagnosticType& t) { by_naming_itself(t); },
          [&](const mir::RuntimeLibraryType& t) { by_naming_itself(t); },
          [&](const mir::CoroutineType& t) { by_naming_itself(t); },
          [&](const mir::RefType& t) { by_naming_itself(t); },
          [&](const mir::VoidType& t) { by_naming_itself(t); },
          [&](const mir::TupleType& t) { by_naming_itself(t); },
          [&](const mir::UnionType& t) { by_naming_itself(t); },
          [&](const mir::TaggedUnionType& t) { by_naming_itself(t); },
          [&](const mir::EmptyType& t) { by_naming_itself(t); },
          [&](const mir::ObservableType& t) { by_naming_itself(t); },
          [&](const mir::ResolvedType& t) { by_naming_itself(t); },
          [&](const mir::DriverType& t) { by_naming_itself(t); },
          [&](const mir::OpenWriteType& t) { by_naming_itself(t); },
          [&](const mir::DesignationType& t) { by_naming_itself(t); },
          [&](const mir::ObjectWriteType& t) { by_naming_itself(t); },
          [&](const mir::SampledHistoryType& t) { by_naming_itself(t); },
          [&](const mir::EvaluationAttemptsType& t) { by_naming_itself(t); },
          [&](const mir::ClosureType& t) { by_naming_itself(t); },
      });
}

void WriteOne(TargetText& out, const CppTupleComponents& components) {
  out += "lyra::value::Tuple<";
  WriteTypeList(out, *components.unit, components.of);
  out += ">";
}

void WriteOne(TargetText& out, const CppClassRef& ref) {
  const mir::CompilationUnit& unit = ref.Unit();
  std::visit(
      Overloaded{
          // A class no other unit names is declared in the unit's code file,
          // the one file that names it.
          [&](const mir::IntraUnitClassRef& i) {
            if (const std::optional<support::DefPath>& path =
                    unit.GetClass(i.class_id).path;
                path.has_value()) {
              out.Require(ClassDeclarationFileOf(unit.name, *path));
            }
            Write(out, CppClassPath(unit, i.class_id));
          },
          [&](const mir::CrossUnitClassRef& e) {
            out.Require(ClassDeclarationFileOf(e.unit_name, e.class_path));
            Write(out, CppExternalClassPath(e.unit_name, e.class_path));
          },
          // The tree's root is the library's scope class, spelled as a value
          // of that class's type is.
          [&](const mir::ObjectTreeRootRef&) {
            out += RuntimeClassCppType(support::RuntimeClass::kScope);
          },
          [&](const mir::ManagedObjectRootRef&) {
            out += RuntimeClassCppType(support::RuntimeClass::kObject);
          }},
      ref.Ref());
}

void WriteOne(TargetText& out, const CppBaseClass& base) {
  const mir::CompilationUnit& unit = base.of.Unit();
  std::visit(
      Overloaded{
          // A class no other unit names is defined in the unit's code file,
          // ahead of the classes there that derive from it.
          [&](const mir::IntraUnitClassRef& i) {
            if (const std::optional<support::DefPath>& path =
                    unit.GetClass(i.class_id).path;
                path.has_value()) {
              out.Require(ClassDefinitionFileOf(unit.name, *path));
            }
          },
          [&](const mir::CrossUnitClassRef& e) {
            out.Require(ClassDefinitionFileOf(e.unit_name, e.class_path));
          },
          [](const mir::ObjectTreeRootRef&) {},
          [](const mir::ManagedObjectRootRef&) {}},
      base.of.Ref());
  Write(out, base.of);
}

}  // namespace lyra::backend::cpp
