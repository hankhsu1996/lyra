#include "lyra/backend/cpp/render_type.hpp"

#include <span>
#include <string_view>
#include <variant>

#include "lyra/backend/cpp/naming.hpp"
#include "lyra/backend/cpp/target_text.hpp"
#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/mir/class_id.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/type.hpp"
#include "lyra/mir/type_builders.hpp"

namespace lyra::backend::cpp {

auto BodyCleanupExtentCppType() -> std::string_view {
  return "lyra::runtime::ScopeExit";
}

auto SuspensionCppType() -> std::string_view {
  return "lyra::runtime::Suspension";
}

auto ManagedObjectRootCppType() -> std::string_view {
  return "lyra::runtime::GcObject";
}

auto ObjectViewConversionCppName() -> std::string_view {
  return "lyra::runtime::ViewAs";
}

namespace {

// A type spelled out of others writes their spellings with the separator a
// template argument list puts between them. The empty list writes nothing.
void WriteTypeList(
    TargetText& out, const mir::CompilationUnit& unit,
    std::span<const mir::TypeId> types) {
  bool first = true;
  for (const mir::TypeId type : types) {
    if (!first) out += ", ";
    Write(out, CppType(unit, type));
    first = false;
  }
}

auto RuntimeLibraryCppType(mir::RuntimeLibraryKind kind) -> std::string_view {
  switch (kind) {
    case mir::RuntimeLibraryKind::kPackedType:
      return "lyra::value::PackedType";
    case mir::RuntimeLibraryKind::kPackedRange:
      return "lyra::value::PackedRange";
    case mir::RuntimeLibraryKind::kUnpackedRange:
      return "lyra::value::UnpackedRange";
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
    case mir::RuntimeLibraryKind::kScopeProgram:
      return "lyra::runtime::ScopeProgram";
    case mir::RuntimeLibraryKind::kScopeCallable:
      return "lyra::runtime::ScopeCallable";
    case mir::RuntimeLibraryKind::kScopeCallableTable:
      return "lyra::runtime::ScopeCallableTable";
    case mir::RuntimeLibraryKind::kScopeDefinition:
      return "lyra::runtime::ScopeDefinition";
    case mir::RuntimeLibraryKind::kScopeClass:
      return "lyra::runtime::ScopeClass";
    case mir::RuntimeLibraryKind::kScopeClassTable:
      return "lyra::runtime::ScopeClassTable";
    case mir::RuntimeLibraryKind::kObjectDefinition:
      return "lyra::runtime::ObjectDefinition";
    case mir::RuntimeLibraryKind::kPropertySlotTable:
      return "lyra::runtime::PropertySlotTable";
    case mir::RuntimeLibraryKind::kDispatchTakeover:
      return "lyra::runtime::DispatchTakeover";
    case mir::RuntimeLibraryKind::kTakeoverTable:
      return "lyra::runtime::TakeoverTable";
    case mir::RuntimeLibraryKind::kMethodDispatchTable:
      return "lyra::runtime::MethodDispatchTable";
    case mir::RuntimeLibraryKind::kResolvedProperty:
      return "lyra::runtime::ResolvedProperty";
    case mir::RuntimeLibraryKind::kResolvedPropertyTable:
      return "lyra::runtime::ResolvedPropertyTable";
    case mir::RuntimeLibraryKind::kResolvedBehavior:
      return "lyra::runtime::ResolvedBehavior";
    case mir::RuntimeLibraryKind::kResolvedBehaviorTable:
      return "lyra::runtime::ResolvedBehaviorTable";
    case mir::RuntimeLibraryKind::kDeclaredBody:
      return "lyra::runtime::DeclaredBody";
    case mir::RuntimeLibraryKind::kDeclaredBodyTable:
      return "lyra::runtime::DeclaredBodyTable";
    case mir::RuntimeLibraryKind::kScopeMetadata:
      return "lyra::runtime::ScopeMetadata";
    case mir::RuntimeLibraryKind::kAbiStringRef:
      return "lyra::runtime::AbiStringRef";
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
    case mir::RuntimeLibraryKind::kPropertyCoordinate:
      return "lyra::runtime::PropertyCoordinate";
    case mir::RuntimeLibraryKind::kBehaviorCoordinate:
      return "lyra::runtime::BehaviorCoordinate";
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
          // An enumeration and a packed aggregate are their base integral --
          // a `PackedArray`. What each declares beyond that is a set of names,
          // which is not part of how a value is held and so gives it no
          // representation of its own.
          [&](const mir::EnumType&) { out += "lyra::value::PackedArray"; },
          [&](const mir::PackedStructType&) {
            out += "lyra::value::PackedArray";
          },
          [&](const mir::PackedUnionType&) {
            out += "lyra::value::PackedArray";
          },
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
            // A C++ function pointer spells its name inside the declarator, so
            // the pointer form below reads correctly only as a type-id -- which
            // is all a restored prototype is ever used as. The erased form is
            // what a declaration names, so it takes the runtime's own alias.
            if (mir::IsErasedFunction(unit.types, type_id)) {
              out += "lyra::runtime::ErasedScopeCallable";
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
          [&](const mir::RealTimeType&) { out += "lyra::value::Real"; },
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
            Write(out, CppClassName(unit.GetClass(o.class_id), o.class_id));
          },
          [&](const mir::StructType& s) {
            Write(out, CppStructName(s.struct_id));
          },
          [&](const mir::ExternalUnitObjectType& e) {
            // The class a unit publishes its instances as is named on its
            // signature rather than derived from the unit's own name, so the
            // record of what it promised is what says which class this is.
            const mir::ExternalUnitObject& object =
                unit.external_unit_objects.Get(e.object);
            Write(
                out, CppUnitScope(object.unit_name),
                "::", ToCppName(object.class_name));
          },
          [&](const mir::CrossUnitClassType& e) {
            Write(
                out, CppUnitScope(e.unit_name), "::", ToCppName(e.class_name));
          },
          // An object this unit has no class to name has no spelling here
          // either, and nothing asks for one: a reference to such an object is
          // carried and compared without naming its class, and every operation
          // that would need the name is refused while the design elaborates.
          [](const mir::OpaqueObjectType&) {
            throw InternalError(
                "backend::cpp: an object with no class to name has no "
                "target-language spelling");
          },
          [&](const mir::RuntimeClassType& e) { out += e.symbol; },
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
                // A borrowed pointer refers to the pointee's storage cell --
                // a `Var<T>` if the pointee is an observable wrapper, the
                // bare type otherwise -- so the slot mirrors what it points
                // at by recursing. A read-only borrow grants no write
                // capability (`const T*`), the immutable-receiver case.
                if (p.mutability == mir::Mutability::kReadOnly) {
                  out += "const ";
                }
                Write(out, type(p.pointee), "*");
                return;
            }
            throw InternalError("backend::cpp: unknown PointerOwnership");
          },
          // One spelling for every static view. Which class a program point
          // assumes is a property of that point, so it is written where the
          // reference is used and not where storage for it is declared -- and
          // two units holding one cell under different views is ordinary, so a
          // spelling that followed the view would give that cell two types.
          [&](const mir::ManagedRefType&) { out += "lyra::value::ObjectRef"; },
          [&](const mir::VectorType& v) {
            Write(out, "std::vector<", type(v.element), ">");
          },
          [&](const mir::TupleType& t) {
            out += "lyra::value::Tuple<";
            WriteTypeList(out, unit, t.elements);
            out += ">";
          },
          // A declared structure realizes as the product its members make. The
          // names it declares for them are not part of how a value is held, so
          // they name nothing here.
          [&](const mir::UnpackedStructType& s) {
            out += "lyra::value::Tuple<";
            WriteTypeList(out, unit, mir::MemberTypes(s.members));
            out += ">";
          },
          [&](const mir::UnionType& u) {
            out += "lyra::value::Union<";
            WriteTypeList(out, unit, mir::MemberTypes(u.members));
            out += ">";
          },
          [&](const mir::EmptyType&) { out += "lyra::value::Empty"; },
          [&](const mir::TaggedUnionType& u) {
            out += "lyra::value::TaggedUnion<";
            WriteTypeList(out, unit, mir::MemberTypes(u.members));
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
          [&](const mir::SampledHistoryType& h) {
            Write(out, "lyra::runtime::SampledHistory<", type(h.value), ">");
          },
          [&](const mir::EvaluationAttemptsType&) {
            out += "lyra::runtime::EvaluationAttempts";
          },
          // A closure is emitted as a lambda, and C++ lets nothing name a
          // lambda's type -- so there is no spelling to answer with, and this
          // is not a spelling the target is missing. A closure value reaches
          // its uses directly, and the body reaches its captures as the
          // lambda's own bindings, so nothing asks.
          [](const mir::ClosureType&) {
            throw InternalError(
                "backend::cpp: a closure is emitted as a lambda, whose type "
                "C++ "
                "lets nothing name -- please report this as a bug");
          },
      });
}

auto PlaceAccessAsCpp(const mir::CompilationUnit& unit, mir::TypeId type_id)
    -> PlaceAccess {
  const auto opens_no_storage = []() -> PlaceAccess {
    throw InternalError(
        "PlaceAccessAsCpp: a place of this type stands for no storage, "
        "so there is nothing for an access to open -- please report this as a "
        "bug");
  };
  return unit.types.Get(type_id).Visit(
      Overloaded{
          [&](const mir::PointerType&) -> PlaceAccess {
            return OpenedByDereference{};
          },
          // Opened like a pointer and reached differently: what a reference
          // opens is the cell it was bound to, which is the target language's
          // own operator standing for the library's (LRM 23.3.3.2).
          [&](const mir::RefType&) -> PlaceAccess {
            return OpenedByDereference{};
          },
          [&](const mir::ManagedRefType& m) -> PlaceAccess {
            return OpenedThroughView{.pointee = m.pointee};
          },
          // Every other type is a value rather than something standing for
          // storage, so a place whose type is one of them was not built by
          // opening anything.
          [&](const mir::PackedArrayType&) { return opens_no_storage(); },
          [&](const mir::EnumType&) { return opens_no_storage(); },
          [&](const mir::PackedStructType&) { return opens_no_storage(); },
          [&](const mir::PackedUnionType&) { return opens_no_storage(); },
          [&](const mir::UnpackedArrayType&) { return opens_no_storage(); },
          [&](const mir::DynamicArrayType&) { return opens_no_storage(); },
          [&](const mir::QueueType&) { return opens_no_storage(); },
          [&](const mir::AssociativeArrayType&) { return opens_no_storage(); },
          [&](const mir::WildcardIndexType&) { return opens_no_storage(); },
          [&](const mir::StringType&) { return opens_no_storage(); },
          [&](const mir::MachineCStringType&) { return opens_no_storage(); },
          [&](const mir::MachineBoolType&) { return opens_no_storage(); },
          [&](const mir::MachineIntType&) { return opens_no_storage(); },
          [&](const mir::MachineFloatType&) { return opens_no_storage(); },
          [&](const mir::MachineArrayType&) { return opens_no_storage(); },
          [&](const mir::MachineFunctionType&) { return opens_no_storage(); },
          [&](const mir::EventType&) { return opens_no_storage(); },
          [&](const mir::RealType&) { return opens_no_storage(); },
          [&](const mir::ShortRealType&) { return opens_no_storage(); },
          [&](const mir::RealTimeType&) { return opens_no_storage(); },
          [&](const mir::ChandleType&) { return opens_no_storage(); },
          [&](const mir::VoidType&) { return opens_no_storage(); },
          [&](const mir::EmptyType&) { return opens_no_storage(); },
          [&](const mir::ObjectType&) { return opens_no_storage(); },
          [&](const mir::ExternalUnitObjectType&) {
            return opens_no_storage();
          },
          [&](const mir::CrossUnitClassType&) { return opens_no_storage(); },
          [&](const mir::OpaqueObjectType&) { return opens_no_storage(); },
          [&](const mir::RuntimeClassType&) { return opens_no_storage(); },
          [&](const mir::RuntimeEffectsType&) { return opens_no_storage(); },
          [&](const mir::FilesType&) { return opens_no_storage(); },
          [&](const mir::DiagnosticType&) { return opens_no_storage(); },
          [&](const mir::RuntimeLibraryType&) { return opens_no_storage(); },
          [&](const mir::CoroutineType&) { return opens_no_storage(); },
          [&](const mir::VectorType&) { return opens_no_storage(); },
          [&](const mir::TupleType&) { return opens_no_storage(); },
          [&](const mir::UnpackedStructType&) { return opens_no_storage(); },
          [&](const mir::UnionType&) { return opens_no_storage(); },
          [&](const mir::TaggedUnionType&) { return opens_no_storage(); },
          [&](const mir::ObservableType&) { return opens_no_storage(); },
          [&](const mir::ResolvedType&) { return opens_no_storage(); },
          [&](const mir::DriverType&) { return opens_no_storage(); },
          [&](const mir::SampledHistoryType&) { return opens_no_storage(); },
          [&](const mir::EvaluationAttemptsType&) {
            return opens_no_storage();
          },
          [&](const mir::StructType&) { return opens_no_storage(); },
          [&](const mir::ClosureType&) { return opens_no_storage(); },
      });
}

void WriteOne(TargetText& out, const CppConstructorName& constructor) {
  const mir::CompilationUnit& unit = constructor.of.Unit();
  const mir::TypeId type_id = constructor.of.Type();
  // A type that is built by naming itself, which is what C++ spells a
  // constructor with. The spelling comes from the type mapping rather than from
  // here, so a type this target has no name for -- a closure, emitted as a
  // lambda -- says so once, where it is named.
  const auto by_naming_itself = [&](const auto&) {
    Write(out, constructor.of);
  };
  unit.types.Get(type_id).Visit(
      Overloaded{
          // A wrapper that owns what it points at brings the pointee into
          // existence along with itself, so what names its construction is the
          // entry that allocates and constructs together rather than the
          // wrapper's own spelling. A borrowed pointer owns nothing and is
          // bound to storage that already exists, so nothing constructs one.
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
            Write(out, "lyra::runtime::GcNew<", CppType(unit, m.pointee), ">");
          },
          // The target type a sequence is kept in takes no element list of its
          // own, so what names its construction is the library entry that does.
          [&](const mir::VectorType& v) {
            Write(
                out, "lyra::runtime::MakeSequence<", CppType(unit, v.element),
                ">");
          },
          [&](const mir::PackedArrayType& t) { by_naming_itself(t); },
          [&](const mir::EnumType& t) { by_naming_itself(t); },
          [&](const mir::PackedStructType& t) { by_naming_itself(t); },
          [&](const mir::PackedUnionType& t) { by_naming_itself(t); },
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
          [&](const mir::RealTimeType& t) { by_naming_itself(t); },
          [&](const mir::UnpackedArrayType& t) { by_naming_itself(t); },
          [&](const mir::DynamicArrayType& t) { by_naming_itself(t); },
          [&](const mir::QueueType& t) { by_naming_itself(t); },
          [&](const mir::AssociativeArrayType& t) { by_naming_itself(t); },
          [&](const mir::WildcardIndexType& t) { by_naming_itself(t); },
          [&](const mir::ObjectType& t) { by_naming_itself(t); },
          [&](const mir::StructType& t) { by_naming_itself(t); },
          [&](const mir::ExternalUnitObjectType& t) { by_naming_itself(t); },
          [&](const mir::CrossUnitClassType& t) { by_naming_itself(t); },
          // Bringing an object into existence names its class, so a reference
          // with no class to name states no construction: the source could not
          // have written one.
          [](const mir::OpaqueObjectType&) {
            throw InternalError(
                "backend::cpp: an object with no class to name is never "
                "constructed");
          },
          [&](const mir::RuntimeClassType& t) { by_naming_itself(t); },
          [&](const mir::RuntimeEffectsType& t) { by_naming_itself(t); },
          [&](const mir::FilesType& t) { by_naming_itself(t); },
          [&](const mir::DiagnosticType& t) { by_naming_itself(t); },
          [&](const mir::RuntimeLibraryType& t) { by_naming_itself(t); },
          [&](const mir::CoroutineType& t) { by_naming_itself(t); },
          [&](const mir::RefType& t) { by_naming_itself(t); },
          [&](const mir::VoidType& t) { by_naming_itself(t); },
          [&](const mir::TupleType& t) { by_naming_itself(t); },
          [&](const mir::UnpackedStructType& t) { by_naming_itself(t); },
          [&](const mir::UnionType& t) { by_naming_itself(t); },
          [&](const mir::TaggedUnionType& t) { by_naming_itself(t); },
          [&](const mir::EmptyType& t) { by_naming_itself(t); },
          [&](const mir::ObservableType& t) { by_naming_itself(t); },
          [&](const mir::ResolvedType& t) { by_naming_itself(t); },
          [&](const mir::DriverType& t) { by_naming_itself(t); },
          [&](const mir::SampledHistoryType& t) { by_naming_itself(t); },
          [&](const mir::EvaluationAttemptsType& t) { by_naming_itself(t); },
          [&](const mir::ClosureType& t) { by_naming_itself(t); },
      });
}

void WriteOne(TargetText& out, const CppClassRef& ref) {
  const mir::CompilationUnit& unit = ref.Unit();
  std::visit(
      Overloaded{
          [&](const mir::IntraUnitClassRef& i) {
            Write(out, CppClassName(unit.GetClass(i.class_id), i.class_id));
          },
          [&](const mir::CrossUnitClassRef& e) {
            Write(
                out, CppUnitScope(e.unit_name), "::", ToCppName(e.class_name));
          },
          [&](const mir::RuntimeClassRef& e) { out += e.symbol; }},
      ref.Ref());
}

}  // namespace lyra::backend::cpp
