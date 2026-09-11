#include "lyra/backend/cpp/render_type.hpp"

#include <format>
#include <span>
#include <string>
#include <string_view>
#include <variant>
#include <vector>

#include "lyra/backend/cpp/formatting.hpp"
#include "lyra/backend/cpp/naming.hpp"
#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/mir/class.hpp"
#include "lyra/mir/class_id.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/type.hpp"
#include "lyra/mir/type_builders.hpp"

namespace lyra::backend::cpp {

auto BodyCleanupExtentCppType() -> std::string_view {
  return "lyra::runtime::ScopeExit";
}

auto ManagedObjectRootCppType() -> std::string_view {
  return "lyra::runtime::GcObject";
}

auto ObjectViewConversionCppName() -> std::string_view {
  return "lyra::runtime::ViewAs";
}

auto RenderEachTypeAsCpp(
    const mir::CompilationUnit& unit, std::span<const mir::TypeId> types)
    -> std::vector<std::string> {
  std::vector<std::string> rendered;
  rendered.reserve(types.size());
  for (const mir::TypeId type : types) {
    rendered.push_back(RenderTypeAsCpp(unit, type));
  }
  return rendered;
}

auto RenderTypeAsCpp(const mir::CompilationUnit& unit, mir::TypeId type_id)
    -> std::string {
  return unit.types.Get(type_id).Visit(
      Overloaded{
          [](const mir::PackedArrayType&) -> std::string {
            return std::string{"lyra::value::PackedArray"};
          },
          // An enumeration and a packed aggregate are their base integral --
          // a `PackedArray`. What each declares beyond that is a set of names,
          // which is not part of how a value is held and so gives it no
          // representation of its own.
          [](const mir::EnumType&) -> std::string {
            return std::string{"lyra::value::PackedArray"};
          },
          [](const mir::PackedStructType&) -> std::string {
            return std::string{"lyra::value::PackedArray"};
          },
          [](const mir::PackedUnionType&) -> std::string {
            return std::string{"lyra::value::PackedArray"};
          },
          [](const mir::StringType&) -> std::string {
            return std::string{"lyra::value::String"};
          },
          [](const mir::MachineCStringType&) -> std::string {
            return std::string{"const char*"};
          },
          [](const mir::MachineBoolType&) -> std::string {
            return std::string{"bool"};
          },
          [](const mir::MachineIntType& m) -> std::string {
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
            throw InternalError("RenderTypeAsCpp: unknown MachineIntWidth");
          },
          [](const mir::MachineFloatType& m) -> std::string {
            switch (m.width) {
              case mir::MachineFloatWidth::k32:
                return std::string{"float"};
              case mir::MachineFloatWidth::k64:
                return std::string{"double"};
            }
            throw InternalError("RenderTypeAsCpp: unknown MachineFloatWidth");
          },
          [&](const mir::MachineArrayType& m) -> std::string {
            return std::format(
                "std::array<{}, {}>", RenderTypeAsCpp(unit, m.element), m.size);
          },
          [&](const mir::MachineFunctionType& m) -> std::string {
            // A C++ function pointer spells its name inside the declarator, so
            // the pointer form below reads correctly only as a type-id -- which
            // is all a restored prototype is ever used as. The erased form is
            // what a declaration names, so it takes the runtime's own alias.
            if (mir::IsErasedFunction(unit.types, type_id)) {
              return std::string{"lyra::runtime::ErasedScopeCallable"};
            }
            return std::format(
                "{} (*)({})", RenderTypeAsCpp(unit, m.result),
                JoinCommaSeparated(RenderEachTypeAsCpp(unit, m.params)));
          },
          [](const mir::ChandleType&) -> std::string {
            return std::string{"lyra::value::Chandle"};
          },
          [](const mir::EventType&) -> std::string {
            return std::string{"lyra::runtime::NamedEvent"};
          },
          [](const mir::RealType&) -> std::string {
            return std::string{"lyra::value::Real"};
          },
          [](const mir::ShortRealType&) -> std::string {
            return std::string{"lyra::value::ShortReal"};
          },
          [](const mir::RealTimeType&) -> std::string {
            return std::string{"lyra::value::Real"};
          },
          [&](const mir::UnpackedArrayType& ua) -> std::string {
            return std::format(
                "lyra::value::UnpackedArray<{}>",
                RenderTypeAsCpp(unit, ua.element_type));
          },
          [&](const mir::DynamicArrayType& da) -> std::string {
            return std::format(
                "lyra::value::DynamicArray<{}>",
                RenderTypeAsCpp(unit, da.element_type));
          },
          [&](const mir::QueueType& q) -> std::string {
            return std::format(
                "lyra::value::Queue<{}>",
                RenderTypeAsCpp(unit, q.element_type));
          },
          [&](const mir::AssociativeArrayType& a) -> std::string {
            return std::format(
                "lyra::value::AssociativeArray<{}, {}>",
                RenderTypeAsCpp(unit, a.key_type),
                RenderTypeAsCpp(unit, a.element_type));
          },
          [](const mir::WildcardIndexType&) -> std::string {
            return "lyra::value::WildcardKey";
          },
          [&unit](const mir::ObjectType& o) -> std::string {
            return ToCppName(unit.GetClass(o.class_id).name);
          },
          [&unit](const mir::StructType& s) -> std::string {
            return ToCppName(unit.GetStruct(s.struct_id).name);
          },
          [&unit](const mir::ExternalUnitObjectType& e) -> std::string {
            // The class a unit publishes its instances as is named on its
            // signature rather than derived from the unit's own name, so the
            // record of what it promised is what says which class this is.
            const mir::ExternalUnitObject& object =
                unit.external_unit_objects.Get(e.object);
            return std::format(
                "{}::{}", UnitNamespaceOf(object.unit_name),
                ToCppName(object.class_name));
          },
          [](const mir::CrossUnitClassType& e) -> std::string {
            return std::format(
                "{}::{}", UnitNamespaceOf(e.unit_name),
                ToCppName(e.class_name));
          },
          // An object this unit has no class to name has no spelling here
          // either, and nothing asks for one: a reference to such an object is
          // carried and compared without naming its class, and every operation
          // that would need the name is refused while the design elaborates.
          [](const mir::OpaqueObjectType&) -> std::string {
            throw InternalError(
                "RenderTypeAsCpp: an object with no class to name has no "
                "target-language spelling");
          },
          [](const mir::RuntimeClassType& e) -> std::string {
            return e.symbol;
          },
          [](const mir::RuntimeEffectsType&) -> std::string {
            return std::string{"lyra::runtime::RuntimeEffects&"};
          },
          [](const mir::FilesType&) -> std::string {
            return std::string{"lyra::runtime::FileTable&"};
          },
          [](const mir::DiagnosticType&) -> std::string {
            return std::string{"lyra::runtime::DiagnosticDispatcher&"};
          },
          [](const mir::RuntimeLibraryType& r) -> std::string {
            switch (r.kind) {
              case mir::RuntimeLibraryKind::kPackedType:
                return std::string{"lyra::value::PackedType"};
              case mir::RuntimeLibraryKind::kPackedRange:
                return std::string{"lyra::value::PackedRange"};
              case mir::RuntimeLibraryKind::kPrintItem:
                return std::string{"lyra::value::PrintItem"};
              case mir::RuntimeLibraryKind::kPrintLiteralItem:
                return std::string{"lyra::value::PrintLiteralItem"};
              case mir::RuntimeLibraryKind::kPrintValueItem:
                return std::string{"lyra::value::PrintValueItem"};
              case mir::RuntimeLibraryKind::kCancellationTarget:
                return std::string{"lyra::runtime::CancellationTarget"};
              case mir::RuntimeLibraryKind::kControlEffect:
                return std::string{"lyra::runtime::ControlEffect"};
              case mir::RuntimeLibraryKind::kFormatSpec:
                return std::string{"lyra::value::FormatSpec"};
              case mir::RuntimeLibraryKind::kFormatArg:
                return std::string{"lyra::value::FormatArg"};
              case mir::RuntimeLibraryKind::kChannelCancellation:
                return std::string{"lyra::runtime::ChannelCancellation"};
              case mir::RuntimeLibraryKind::kTimeFormat:
                return std::string{"lyra::value::TimeFormat"};
              case mir::RuntimeLibraryKind::kHierarchySegment:
                return std::string{"lyra::runtime::HierarchySegment"};
              case mir::RuntimeLibraryKind::kTrigger:
                return std::string{"lyra::runtime::Trigger"};
              case mir::RuntimeLibraryKind::kObservation:
                return std::string{"lyra::runtime::Observation"};
              case mir::RuntimeLibraryKind::kScopeProgram:
                return std::string{"lyra::runtime::ScopeProgram"};
              case mir::RuntimeLibraryKind::kScopeCallable:
                return std::string{"lyra::runtime::ScopeCallable"};
              case mir::RuntimeLibraryKind::kScopeCallableTable:
                return std::string{"lyra::runtime::ScopeCallableTable"};
              case mir::RuntimeLibraryKind::kScopeDefinition:
                return std::string{"lyra::runtime::ScopeDefinition"};
              case mir::RuntimeLibraryKind::kScopeMetadata:
                return std::string{"lyra::runtime::ScopeMetadata"};
              case mir::RuntimeLibraryKind::kAbiStringRef:
                return std::string{"lyra::runtime::AbiStringRef"};
              case mir::RuntimeLibraryKind::kDpiBitBuffer:
                return std::string{"lyra::value::DpiBitBuffer"};
              case mir::RuntimeLibraryKind::kDpiLogicBuffer:
                return std::string{"lyra::value::DpiLogicBuffer"};
              case mir::RuntimeLibraryKind::kDpiBitChunk:
                return std::string{"svBitVecVal"};
              case mir::RuntimeLibraryKind::kDpiLogicChunk:
                return std::string{"svLogicVecVal"};
              case mir::RuntimeLibraryKind::kDpiOpenArray:
                return std::string{"lyra::value::DpiOpenArray"};
              case mir::RuntimeLibraryKind::kDpiOpenArrayHandle:
                return std::string{"const svOpenArrayHandle"};
              case mir::RuntimeLibraryKind::kDpiScopeGuard:
                return std::string{"lyra::runtime::DpiScopeGuard"};
              case mir::RuntimeLibraryKind::kForeignTaskAwaitable:
                return std::string{"lyra::runtime::ForeignTaskAwaitable"};
            }
            throw InternalError("RenderTypeAsCpp: unknown RuntimeLibraryKind");
          },
          [&](const mir::CoroutineType& c) -> std::string {
            return std::format(
                "lyra::runtime::Coroutine<{}>",
                RenderTypeAsCpp(unit, c.payload));
          },
          [&](const mir::RefType& r) -> std::string {
            std::string ref = std::format(
                "lyra::runtime::Ref<{}>", RenderTypeAsCpp(unit, r.pointee));
            return r.mutability == mir::Mutability::kReadOnly
                       ? std::format("const {}", ref)
                       : ref;
          },
          [](const mir::VoidType&) -> std::string {
            return std::string{"void"};
          },
          [&](const mir::PointerType& p) -> std::string {
            std::string inner = RenderTypeAsCpp(unit, p.pointee);
            switch (p.ownership) {
              case mir::PointerOwnership::kUnique:
                return std::format("std::unique_ptr<{}>", inner);
              case mir::PointerOwnership::kShared:
                return std::format("std::shared_ptr<{}>", inner);
              case mir::PointerOwnership::kBorrowed:
                // A borrowed pointer refers to the pointee's storage cell --
                // a `Var<T>` if the pointee is an observable wrapper, the
                // bare type otherwise -- so the slot mirrors what it points
                // at by recursing. A read-only borrow grants no write
                // capability (`const T*`), the immutable-receiver case.
                return std::format(
                    "{}{}*",
                    p.mutability == mir::Mutability::kReadOnly ? "const " : "",
                    inner);
            }
            throw InternalError("RenderTypeAsCpp: unknown PointerOwnership");
          },
          // One spelling for every static view. Which class a program point
          // assumes is a property of that point, so it is written where the
          // reference is used and not where storage for it is declared -- and
          // two units holding one cell under different views is ordinary, so a
          // spelling that followed the view would give that cell two types.
          [](const mir::ManagedRefType&) -> std::string {
            return std::string{"lyra::runtime::ObjectRef"};
          },
          [&](const mir::VectorType& v) -> std::string {
            return std::format(
                "std::vector<{}>", RenderTypeAsCpp(unit, v.element));
          },
          [&](const mir::TupleType& t) -> std::string {
            return std::format(
                "lyra::value::Tuple<{}>",
                JoinCommaSeparated(RenderEachTypeAsCpp(unit, t.elements)));
          },
          // A declared structure realizes as the product its members make. The
          // names it declares for them are not part of how a value is held, so
          // they name nothing here.
          [&](const mir::UnpackedStructType& s) -> std::string {
            return std::format(
                "lyra::value::Tuple<{}>",
                JoinCommaSeparated(
                    RenderEachTypeAsCpp(unit, mir::MemberTypes(s.members))));
          },
          [&](const mir::UnionType& u) -> std::string {
            return std::format(
                "lyra::value::Union<{}>",
                JoinCommaSeparated(
                    RenderEachTypeAsCpp(unit, mir::MemberTypes(u.members))));
          },
          [](const mir::EmptyType&) -> std::string {
            return std::string{"lyra::value::Empty"};
          },
          [&](const mir::TaggedUnionType& u) -> std::string {
            return std::format(
                "lyra::value::TaggedUnion<{}>",
                JoinCommaSeparated(
                    RenderEachTypeAsCpp(unit, mir::MemberTypes(u.members))));
          },
          [&](const mir::ObservableType& o) -> std::string {
            return std::format(
                "lyra::runtime::Var<{}>", RenderTypeAsCpp(unit, o.value));
          },
          [&](const mir::ResolvedType& r) -> std::string {
            return std::format(
                "lyra::runtime::ResolvedNet<{}>",
                RenderTypeAsCpp(unit, r.value));
          },
          [&](const mir::DriverType& d) -> std::string {
            return std::format(
                "lyra::runtime::Driver<{}>", RenderTypeAsCpp(unit, d.value));
          },
          [&](const mir::SampledHistoryType& h) -> std::string {
            return std::format(
                "lyra::runtime::SampledHistory<{}>",
                RenderTypeAsCpp(unit, h.value));
          },
          [](const mir::EvaluationAttemptsType&) -> std::string {
            return "lyra::runtime::EvaluationAttempts";
          },
          // A closure is emitted as a lambda, and C++ lets nothing name a
          // lambda's type -- so there is no spelling to answer with, and this
          // is not a spelling the target is missing. A closure value reaches
          // its uses directly, and the body reaches its captures as the
          // lambda's own bindings, so nothing asks.
          [](const mir::ClosureType&) -> std::string {
            throw InternalError(
                "RenderTypeAsCpp: a closure is emitted as a lambda, whose type "
                "C++ lets nothing name -- please report this as a bug");
          },
      });
}

auto RenderPlaceAccessAsCpp(
    const mir::CompilationUnit& unit, mir::TypeId type_id,
    std::string_view place) -> std::string {
  return unit.types.Get(type_id).Visit(
      Overloaded{
          [&](const mir::PointerType&) -> std::string {
            return std::format("(*{})", place);
          },
          // Spelled like a pointer's and reached differently: what a reference
          // opens is the cell it was bound to, which is the target language's
          // own operator standing for the library's (LRM 23.3.3.2).
          [&](const mir::RefType&) -> std::string {
            return std::format("(*{})", place);
          },
          [&](const mir::ManagedRefType& m) -> std::string {
            return std::format(
                "{}.Deref<{}>()", place, RenderTypeAsCpp(unit, m.pointee));
          },
          [](const auto&) -> std::string {
            throw InternalError(
                "RenderPlaceAccessAsCpp: this backend states no access for a "
                "place of this type");
          },
      });
}

auto RenderTypeConstructionAsCpp(
    const mir::CompilationUnit& unit, mir::TypeId type_id) -> std::string {
  // A type that is built by naming itself, which is what C++ spells a
  // constructor with. The spelling comes from the naming dispatch rather than
  // from here, so a type whose name this target has none for -- a closure,
  // emitted as a lambda -- says so once, where it is named.
  const auto by_naming_itself = [&](const auto&) -> std::string {
    return RenderTypeAsCpp(unit, type_id);
  };
  return unit.types.Get(type_id).Visit(
      Overloaded{
          // A wrapper that owns what it points at brings the pointee into
          // existence along with itself, so what names its construction is the
          // entry that allocates and constructs together rather than the
          // wrapper's own spelling. A borrowed pointer owns nothing and is
          // bound to storage that already exists, so nothing constructs one.
          [&](const mir::PointerType& p) -> std::string {
            const std::string inner = RenderTypeAsCpp(unit, p.pointee);
            switch (p.ownership) {
              case mir::PointerOwnership::kUnique:
                return std::format("std::make_unique<{}>", inner);
              case mir::PointerOwnership::kShared:
                return std::format("std::make_shared<{}>", inner);
              case mir::PointerOwnership::kBorrowed:
                throw InternalError(
                    "RenderTypeConstructionAsCpp: a borrowed pointer is bound "
                    "to storage that already exists, so none is constructed");
            }
            throw InternalError(
                "RenderTypeConstructionAsCpp: unknown PointerOwnership");
          },
          [&](const mir::ManagedRefType& m) -> std::string {
            return std::format(
                "lyra::runtime::GcNew<{}>", RenderTypeAsCpp(unit, m.pointee));
          },
          // The target type a sequence is kept in takes no element list of its
          // own, so what names its construction is the library entry that does.
          [&](const mir::VectorType& v) -> std::string {
            return std::format(
                "lyra::runtime::MakeSequence<{}>",
                RenderTypeAsCpp(unit, v.element));
          },
          [&](const mir::PackedArrayType& t) { return by_naming_itself(t); },
          [&](const mir::EnumType& t) { return by_naming_itself(t); },
          [&](const mir::PackedStructType& t) { return by_naming_itself(t); },
          [&](const mir::PackedUnionType& t) { return by_naming_itself(t); },
          [&](const mir::StringType& t) { return by_naming_itself(t); },
          [&](const mir::MachineCStringType& t) { return by_naming_itself(t); },
          [&](const mir::MachineBoolType& t) { return by_naming_itself(t); },
          [&](const mir::MachineIntType& t) { return by_naming_itself(t); },
          [&](const mir::MachineFloatType& t) { return by_naming_itself(t); },
          [&](const mir::MachineArrayType& t) { return by_naming_itself(t); },
          [&](const mir::MachineFunctionType& t) {
            return by_naming_itself(t);
          },
          [&](const mir::ChandleType& t) { return by_naming_itself(t); },
          [&](const mir::EventType& t) { return by_naming_itself(t); },
          [&](const mir::RealType& t) { return by_naming_itself(t); },
          [&](const mir::ShortRealType& t) { return by_naming_itself(t); },
          [&](const mir::RealTimeType& t) { return by_naming_itself(t); },
          [&](const mir::UnpackedArrayType& t) { return by_naming_itself(t); },
          [&](const mir::DynamicArrayType& t) { return by_naming_itself(t); },
          [&](const mir::QueueType& t) { return by_naming_itself(t); },
          [&](const mir::AssociativeArrayType& t) {
            return by_naming_itself(t);
          },
          [&](const mir::WildcardIndexType& t) { return by_naming_itself(t); },
          [&](const mir::ObjectType& t) { return by_naming_itself(t); },
          [&](const mir::StructType& t) { return by_naming_itself(t); },
          [&](const mir::ExternalUnitObjectType& t) {
            return by_naming_itself(t);
          },
          [&](const mir::CrossUnitClassType& t) { return by_naming_itself(t); },
          // Bringing an object into existence names its class, so a reference
          // with no class to name states no construction: the source could not
          // have written one.
          [](const mir::OpaqueObjectType&) -> std::string {
            throw InternalError(
                "RenderTypeConstructionAsCpp: an object with no class to name "
                "is never constructed");
          },
          [&](const mir::RuntimeClassType& t) { return by_naming_itself(t); },
          [&](const mir::RuntimeEffectsType& t) { return by_naming_itself(t); },
          [&](const mir::FilesType& t) { return by_naming_itself(t); },
          [&](const mir::DiagnosticType& t) { return by_naming_itself(t); },
          [&](const mir::RuntimeLibraryType& t) { return by_naming_itself(t); },
          [&](const mir::CoroutineType& t) { return by_naming_itself(t); },
          [&](const mir::RefType& t) { return by_naming_itself(t); },
          [&](const mir::VoidType& t) { return by_naming_itself(t); },
          [&](const mir::TupleType& t) { return by_naming_itself(t); },
          [&](const mir::UnpackedStructType& t) { return by_naming_itself(t); },
          [&](const mir::UnionType& t) { return by_naming_itself(t); },
          [&](const mir::TaggedUnionType& t) { return by_naming_itself(t); },
          [&](const mir::EmptyType& t) { return by_naming_itself(t); },
          [&](const mir::ObservableType& t) { return by_naming_itself(t); },
          [&](const mir::ResolvedType& t) { return by_naming_itself(t); },
          [&](const mir::DriverType& t) { return by_naming_itself(t); },
          [&](const mir::SampledHistoryType& t) { return by_naming_itself(t); },
          [&](const mir::EvaluationAttemptsType& t) {
            return by_naming_itself(t);
          },
          [&](const mir::ClosureType& t) { return by_naming_itself(t); },
      });
}

auto RenderClassRefAsCpp(
    const mir::CompilationUnit& unit, const mir::ClassRef& ref) -> std::string {
  return std::visit(
      Overloaded{
          [&unit](const mir::IntraUnitClassRef& i) -> std::string {
            return ToCppName(unit.GetClass(i.class_id).name);
          },
          [](const mir::CrossUnitClassRef& e) -> std::string {
            return std::format(
                "{}::{}", UnitNamespaceOf(e.unit_name),
                ToCppName(e.class_name));
          },
          [](const mir::RuntimeClassRef& e) -> std::string {
            return e.symbol;
          }},
      ref);
}

}  // namespace lyra::backend::cpp
