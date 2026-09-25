#include "lyra/lir/type.hpp"

#include <cstddef>
#include <cstdint>
#include <functional>
#include <optional>
#include <string>
#include <vector>

#include "lyra/base/hash.hpp"
#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/lir/type_id.hpp"

namespace lyra::lir {

namespace {

void Combine(std::size_t& seed, std::uint64_t value) {
  base::HashField(seed, value);
}

void Combine(std::size_t& seed, const std::string& value) {
  base::HashField(seed, value);
}

void Combine(std::size_t& seed, TypeId id) {
  Combine(seed, id.value);
}

void Combine(std::size_t& seed, ClassId id) {
  Combine(seed, id.value);
}

template <typename E>
  requires std::is_enum_v<E>
void Combine(std::size_t& seed, E value) {
  Combine(seed, static_cast<std::uint64_t>(value));
}

void Combine(std::size_t& seed, const std::vector<TypeId>& ids) {
  for (const TypeId id : ids) {
    Combine(seed, id);
  }
}

void Combine(std::size_t& seed, const PackedArrayType& packed) {
  Combine(seed, packed.state_kind);
  Combine(seed, packed.signedness);
  for (const PackedRange& dim : packed.dims) {
    Combine(seed, static_cast<std::uint64_t>(dim.left));
    Combine(seed, static_cast<std::uint64_t>(dim.right));
  }
}

void CombineMembers(
    std::size_t& seed, const std::vector<AggregateMember>& members) {
  for (const AggregateMember& member : members) {
    Combine(seed, member.name);
    Combine(seed, member.type);
  }
}

// Which runtime-library value this is. A reader that could not handle one needs
// to know which of them it met, and the kind is the whole of what separates
// them.
auto RuntimeLibraryKindName(RuntimeLibraryKind kind) -> const char* {
  switch (kind) {
    case RuntimeLibraryKind::kPackedType:
      return "packed type descriptor";
    case RuntimeLibraryKind::kPackedRange:
      return "packed range";
    case RuntimeLibraryKind::kUnpackedRange:
      return "unpacked range";
    case RuntimeLibraryKind::kEnumeration:
      return "enumeration";
    case RuntimeLibraryKind::kPrintItem:
      return "print item";
    case RuntimeLibraryKind::kPrintLiteralItem:
      return "print literal item";
    case RuntimeLibraryKind::kPrintValueItem:
      return "print value item";
    case RuntimeLibraryKind::kFormatSpec:
      return "format specification";
    case RuntimeLibraryKind::kFormatArg:
      return "format argument";
    case RuntimeLibraryKind::kChannelCancellation:
      return "channel cancellation";
    case RuntimeLibraryKind::kTimeFormat:
      return "time format";
    case RuntimeLibraryKind::kHierarchySegment:
      return "hierarchy segment";
    case RuntimeLibraryKind::kDpiBitBuffer:
      return "DPI canonical bit buffer";
    case RuntimeLibraryKind::kDpiLogicBuffer:
      return "DPI canonical logic buffer";
    case RuntimeLibraryKind::kDpiBitChunk:
      return "DPI bit chunk";
    case RuntimeLibraryKind::kDpiLogicChunk:
      return "DPI logic chunk";
    case RuntimeLibraryKind::kDpiOpenArray:
      return "DPI open array";
    case RuntimeLibraryKind::kDpiOpenArrayHandle:
      return "DPI open array handle";
    case RuntimeLibraryKind::kTrigger:
      return "trigger";
    case RuntimeLibraryKind::kObservation:
      return "observation";
    case RuntimeLibraryKind::kCancellationTarget:
      return "cancellation target";
    case RuntimeLibraryKind::kControlEffect:
      return "control effect";
    case RuntimeLibraryKind::kPropertyCoordinate:
      return "property coordinate";
    case RuntimeLibraryKind::kBehaviorCoordinate:
      return "behavior coordinate";
    case RuntimeLibraryKind::kObjectDefinition:
      return "object definition";
  }
  throw InternalError("lir::RuntimeLibraryKindName: unknown kind");
}

}  // namespace

auto BitsOf(MachineIntWidth width) -> std::uint32_t {
  switch (width) {
    case MachineIntWidth::k8:
      return 8;
    case MachineIntWidth::k16:
      return 16;
    case MachineIntWidth::k32:
      return 32;
    case MachineIntWidth::k64:
      return 64;
  }
  throw InternalError("lir: unknown MachineIntWidth");
}

auto Type::Hash::operator()(const Type& type) const -> std::size_t {
  std::size_t seed = std::hash<std::size_t>{}(type.data_.index());
  type.Visit(
      Overloaded{
          [&](const PackedStructType& t) {
            Combine(seed, t.base);
            CombineMembers(seed, t.members);
          },
          [&](const PackedUnionType& t) {
            Combine(seed, t.base);
            CombineMembers(seed, t.members);
          },
          [&](const PackedArrayType& t) { Combine(seed, t); },
          [&](const UnpackedArrayType& t) {
            Combine(seed, t.element_type);
            Combine(seed, t.size);
          },
          [&](const DynamicArrayType& t) { Combine(seed, t.element_type); },
          [&](const QueueType& t) {
            Combine(seed, t.element_type);
            Combine(seed, t.max_bound.value_or(0));
            Combine(seed, static_cast<std::uint64_t>(t.max_bound.has_value()));
          },
          [&](const AssociativeArrayType& t) {
            Combine(seed, t.element_type);
            Combine(seed, t.key_type);
          },
          [](const WildcardIndexType&) {},
          [](const StringType&) {},
          [](const MachineCStringType&) {},
          [](const MachineBoolType&) {},
          [&](const MachineIntType& t) {
            Combine(seed, t.width);
            Combine(seed, t.signedness);
          },
          [&](const MachineFloatType& t) { Combine(seed, t.width); },
          [&](const MachineArrayType& t) {
            Combine(seed, t.element);
            Combine(seed, t.size);
          },
          [&](const MachineFunctionType& t) {
            for (const TypeId param : t.params) {
              Combine(seed, param);
            }
            Combine(seed, t.result);
          },
          [](const EventType&) {},
          [](const RealType&) {},
          [](const ShortRealType&) {},
          [](const RealTimeType&) {},
          [](const ChandleType&) {},
          [](const VoidType&) {},
          [](const EmptyType&) {},
          [&](const ObjectType& t) { Combine(seed, t.class_id); },
          [&](const ExternalUnitObjectType& t) {
            Combine(seed, t.object.value);
          },
          [&](const CrossUnitClassType& t) {
            Combine(seed, t.unit_name);
            Combine(seed, t.class_name);
          },
          [](const OpaqueObjectType&) {},
          [&](const RuntimeClassType& t) { Combine(seed, t.symbol); },
          [&](const ClosureType& t) { Combine(seed, t.closure_id.value); },
          [&](const StructType& t) { Combine(seed, t.struct_id.value); },
          [](const RuntimeEffectsType&) {},
          [](const FilesType&) {},
          [](const DiagnosticType&) {},
          [&](const RuntimeLibraryType& t) { Combine(seed, t.kind); },
          [&](const CoroutineType& t) { Combine(seed, t.payload); },
          [&](const RefType& t) {
            Combine(seed, t.pointee);
            Combine(seed, t.mutability);
          },
          [&](const PointerType& t) {
            Combine(seed, t.pointee);
            Combine(seed, t.ownership);
            Combine(seed, t.mutability);
          },
          [&](const ManagedRefType& t) { Combine(seed, t.pointee); },
          [&](const VectorType& t) { Combine(seed, t.element); },
          [&](const TupleType& t) { Combine(seed, t.elements); },
          [&](const UnpackedStructType& t) { CombineMembers(seed, t.members); },
          [&](const UnionType& t) { CombineMembers(seed, t.members); },
          [&](const TaggedUnionType& t) { CombineMembers(seed, t.members); },
          [&](const ResolvedType& t) { Combine(seed, t.value); },
          [&](const DriverType& t) { Combine(seed, t.value); },
          [&](const ObservableType& t) { Combine(seed, t.value); },
          [&](const SampledHistoryType& t) { Combine(seed, t.value); },
          [](const EvaluationAttemptsType&) {}});
  return seed;
}

auto Type::KindName() const -> std::string_view {
  return Visit(
      Overloaded{
          [](const PackedArrayType&) { return "packed array"; },
          [](const PackedStructType&) { return "packed structure"; },
          [](const PackedUnionType&) { return "packed union"; },
          [](const UnpackedArrayType&) { return "unpacked array"; },
          [](const DynamicArrayType&) { return "dynamic array"; },
          [](const QueueType&) { return "queue"; },
          [](const AssociativeArrayType&) { return "associative array"; },
          [](const WildcardIndexType&) { return "wildcard index"; },
          [](const StringType&) { return "string"; },
          [](const MachineCStringType&) { return "machine C string"; },
          [](const MachineBoolType&) { return "machine boolean"; },
          [](const MachineIntType&) { return "machine integer"; },
          [](const MachineFloatType&) { return "machine float"; },
          [](const MachineArrayType&) { return "machine array"; },
          [](const MachineFunctionType&) { return "machine function"; },
          [](const EventType&) { return "named event"; },
          [](const RealType&) { return "real"; },
          [](const ShortRealType&) { return "shortreal"; },
          [](const RealTimeType&) { return "realtime"; },
          [](const ChandleType&) { return "chandle"; },
          [](const VoidType&) { return "void"; },
          [](const EmptyType&) { return "empty"; },
          [](const ObjectType&) { return "class object"; },
          [](const ExternalUnitObjectType&) { return "external unit object"; },
          [](const CrossUnitClassType&) { return "cross-unit class"; },
          [](const OpaqueObjectType&) { return "opaque object"; },
          [](const RuntimeClassType&) { return "runtime class"; },
          [](const ClosureType&) { return "closure"; },
          [](const StructType&) { return "struct"; },
          [](const RuntimeEffectsType&) { return "runtime services"; },
          [](const FilesType&) { return "file table"; },
          [](const DiagnosticType&) { return "diagnostic dispatcher"; },
          [](const RuntimeLibraryType& t) {
            return RuntimeLibraryKindName(t.kind);
          },
          [](const CoroutineType&) { return "coroutine"; },
          [](const RefType&) { return "reference"; },
          [](const PointerType&) { return "pointer"; },
          [](const ManagedRefType&) { return "managed reference"; },
          [](const VectorType&) { return "vector"; },
          [](const TupleType&) { return "product"; },
          [](const UnpackedStructType&) { return "unpacked structure"; },
          [](const UnionType&) { return "union"; },
          [](const TaggedUnionType&) { return "tagged union"; },
          [](const ResolvedType&) { return "net resolution node"; },
          [](const DriverType&) { return "net driver"; },
          [](const ObservableType&) { return "observable cell"; },
          [](const SampledHistoryType&) { return "sampled value history"; },
          [](const EvaluationAttemptsType&) {
            return "concurrent assertion attempts";
          }});
}

auto Type::Declaration() const -> std::optional<TypeDeclaration> {
  using Declared = std::optional<TypeDeclaration>;
  const auto names_none = []() -> Declared { return std::nullopt; };
  return Visit(
      Overloaded{
          // The five that name one.
          [](const ObjectType& t) -> Declared { return t; },
          [](const ExternalUnitObjectType& t) -> Declared { return t; },
          [](const CrossUnitClassType& t) -> Declared { return t; },
          [](const ClosureType& t) -> Declared { return t; },
          [](const StructType& t) -> Declared { return t; },

          // A value, however it is shaped and however its elements are held.
          // What a declaration gave it is a name for the type, which is not a
          // declaration anything is reached through.
          [&](const PackedArrayType&) { return names_none(); },
          [&](const PackedStructType&) { return names_none(); },
          [&](const PackedUnionType&) { return names_none(); },
          [&](const UnpackedArrayType&) { return names_none(); },
          [&](const DynamicArrayType&) { return names_none(); },
          [&](const QueueType&) { return names_none(); },
          [&](const AssociativeArrayType&) { return names_none(); },
          [&](const WildcardIndexType&) { return names_none(); },
          [&](const StringType&) { return names_none(); },
          [&](const RealType&) { return names_none(); },
          [&](const ShortRealType&) { return names_none(); },
          [&](const RealTimeType&) { return names_none(); },
          [&](const ChandleType&) { return names_none(); },
          [&](const TupleType&) { return names_none(); },
          [&](const UnpackedStructType&) { return names_none(); },
          [&](const UnionType&) { return names_none(); },
          [&](const TaggedUnionType&) { return names_none(); },
          [&](const EmptyType&) { return names_none(); },
          [&](const VoidType&) { return names_none(); },

          // The machine vocabulary, which crosses to a target on the target's
          // own terms and belongs to no declaration of the design.
          [&](const MachineCStringType&) { return names_none(); },
          [&](const MachineBoolType&) { return names_none(); },
          [&](const MachineIntType&) { return names_none(); },
          [&](const MachineFloatType&) { return names_none(); },
          [&](const MachineArrayType&) { return names_none(); },
          [&](const MachineFunctionType&) { return names_none(); },

          // An object this unit carries no declaration of, and a class the
          // runtime library defines: a symbol is the whole of the second's
          // identity and the first has none at all, so neither is reached
          // through a declaration this unit holds.
          [&](const OpaqueObjectType&) { return names_none(); },
          [&](const RuntimeClassType&) { return names_none(); },

          // Storage, a service, and an address. Each stands for something
          // else; what a declaration is behind is whatever it stands for.
          [&](const ObservableType&) { return names_none(); },
          [&](const ResolvedType&) { return names_none(); },
          [&](const DriverType&) { return names_none(); },
          [&](const SampledHistoryType&) { return names_none(); },
          [&](const EvaluationAttemptsType&) { return names_none(); },
          [&](const EventType&) { return names_none(); },
          [&](const RuntimeEffectsType&) { return names_none(); },
          [&](const FilesType&) { return names_none(); },
          [&](const DiagnosticType&) { return names_none(); },
          [&](const RuntimeLibraryType&) { return names_none(); },
          [&](const CoroutineType&) { return names_none(); },
          [&](const RefType&) { return names_none(); },
          [&](const PointerType&) { return names_none(); },
          [&](const ManagedRefType&) { return names_none(); },
          [&](const VectorType&) { return names_none(); }});
}

auto Type::Pointee() const -> std::optional<TypeId> {
  if (const auto* pointer = As<PointerType>()) {
    return pointer->pointee;
  }
  if (const auto* reference = As<RefType>()) {
    return reference->pointee;
  }
  if (const auto* managed = As<ManagedRefType>()) {
    return managed->pointee;
  }
  return std::nullopt;
}

auto Type::Address() const -> std::optional<AddressKind> {
  if (Is<MachineFunctionType>()) {
    return AddressKind::kCode;
  }
  if (Pointee().has_value()) {
    return AddressKind::kStorage;
  }
  return std::nullopt;
}

auto Type::DerefTarget() const -> std::optional<TypeId> {
  if (const auto* observable = As<ObservableType>()) {
    return observable->value;
  }
  if (const auto* resolved = As<ResolvedType>()) {
    return resolved->value;
  }
  if (const auto* driver = As<DriverType>()) {
    return driver->value;
  }
  return Pointee();
}

auto Type::IsAddressOnly() const -> bool {
  return Is<ObservableType>() || Is<ResolvedType>() || Is<ObjectType>() ||
         Is<ExternalUnitObjectType>() || Is<CrossUnitClassType>() ||
         Is<OpaqueObjectType>() || Is<RuntimeClassType>() || Is<EventType>() ||
         Is<SampledHistoryType>() || Is<EvaluationAttemptsType>();
}

auto Type::HeldObject() const -> std::optional<support::RuntimeObject> {
  using Held = std::optional<support::RuntimeObject>;
  using support::LibraryObject;
  using support::ValueDomain;
  return Visit(
      Overloaded{
          [](const PackedArrayType&) -> Held { return ValueDomain::kPacked; },
          // A packed aggregate is a packed value at runtime: one vector under a
          // set of names, and a name is not something a value carries.
          [](const PackedStructType&) -> Held { return ValueDomain::kPacked; },
          [](const PackedUnionType&) -> Held { return ValueDomain::kPacked; },
          // LRM 7.8.1 gives a wildcard-indexed array no index data type, so
          // this type names where an index goes rather than what one is made
          // of. What goes there is always integral, at whatever width the
          // expression carried, which is why the value states its width.
          [](const WildcardIndexType&) -> Held { return ValueDomain::kPacked; },
          [](const StringType&) -> Held { return ValueDomain::kString; },
          // `real` and `realtime` are one host-precision value (LRM 6.12.1);
          // `shortreal` is the single-precision one.
          [](const RealType&) -> Held { return ValueDomain::kReal; },
          [](const RealTimeType&) -> Held { return ValueDomain::kReal; },
          [](const ShortRealType&) -> Held { return ValueDomain::kShortReal; },
          // A chandle (LRM 6.14) is a value holding one host pointer.
          [](const ChandleType&) -> Held { return ValueDomain::kChandle; },
          // A declared structure and the anonymous product a lowering composes
          // realize as one product value; what the structure declares beyond
          // it is the name of each member, which no value carries.
          [](const TupleType&) -> Held { return ValueDomain::kTuple; },
          [](const UnpackedStructType&) -> Held { return ValueDomain::kTuple; },
          // An untagged union erases its tag and gives a cross-member read the
          // component default; a tagged union keeps the tag observable and
          // faults a mismatched access (LRM 7.3 / 7.3.2), so the two are
          // different runtime values.
          [](const UnionType&) -> Held { return ValueDomain::kUnion; },
          [](const TaggedUnionType&) -> Held {
            return ValueDomain::kTaggedUnion;
          },
          // A tagged union's `void` member (LRM 7.3.2) is a value carrying no
          // bits, a value of its own so a build's payload is uniform whatever
          // the member type.
          [](const EmptyType&) -> Held { return ValueDomain::kEmpty; },
          // A container's value is how its elements are held and nothing its
          // declaration says: an unpacked array's range (LRM 7.4.2), a queue's
          // bound (LRM 7.10), and an associative array's index type (LRM 7.8)
          // each reach an operation as an operand of their own, so one runtime
          // value per kind serves every declared shape.
          [](const DynamicArrayType&) -> Held {
            return ValueDomain::kDynArray;
          },
          [](const UnpackedArrayType&) -> Held {
            return ValueDomain::kUnpackedArray;
          },
          [](const QueueType&) -> Held { return ValueDomain::kQueue; },
          [](const AssociativeArrayType&) -> Held {
            return ValueDomain::kAssocArray;
          },
          // A class handle (LRM 8.3) refers to an object the simulator owns.
          // Which object it refers to is the whole value, so the value's
          // operations are the ones over a reference -- defaulting to null,
          // copying, and comparing identity.
          [](const ManagedRefType&) -> Held {
            return ValueDomain::kManagedRef;
          },

          // The storage a closure's captures live in is built for the call that
          // hands it on.
          [](const ClosureType&) -> Held { return LibraryObject::kClosure; },
          // A shared pointer is a hold on storage that outlives the scope
          // asking for it, and letting the hold go is what ends it; the other
          // two name storage somebody else ends.
          [](const PointerType& pointer) -> Held {
            switch (pointer.ownership) {
              case PointerOwnership::kShared:
                return LibraryObject::kPromotedScope;
              case PointerOwnership::kUnique:
              case PointerOwnership::kBorrowed:
                return std::nullopt;
            }
            throw InternalError("lir: unknown pointer ownership");
          },
          [](const RuntimeLibraryType& library) -> Held {
            switch (library.kind) {
              // Built for one use by the call that answers with one.
              case RuntimeLibraryKind::kPrintItem:
              case RuntimeLibraryKind::kPrintLiteralItem:
              case RuntimeLibraryKind::kPrintValueItem:
                return LibraryObject::kPrintItem;
              case RuntimeLibraryKind::kFormatSpec:
                return LibraryObject::kFormatSpec;
              case RuntimeLibraryKind::kFormatArg:
                return LibraryObject::kFormatArg;
              case RuntimeLibraryKind::kChannelCancellation:
                return LibraryObject::kChannelCancellation;
              case RuntimeLibraryKind::kHierarchySegment:
                return LibraryObject::kHierarchySegment;
              case RuntimeLibraryKind::kDpiBitBuffer:
                return LibraryObject::kDpiBitBuffer;
              case RuntimeLibraryKind::kDpiLogicBuffer:
                return LibraryObject::kDpiLogicBuffer;
              case RuntimeLibraryKind::kDpiOpenArray:
                return LibraryObject::kDpiOpenArray;
              case RuntimeLibraryKind::kTrigger:
                return LibraryObject::kTrigger;
              case RuntimeLibraryKind::kObservation:
                return LibraryObject::kObservation;
              // Kept by the run -- a description, a coordinate, a class's
              // record, the time format -- or reached inside something else --
              // a buffer's chunk, an image's handle, the effect a departure
              // carries, the target a disable names.
              case RuntimeLibraryKind::kPackedType:
              case RuntimeLibraryKind::kPackedRange:
              case RuntimeLibraryKind::kUnpackedRange:
              case RuntimeLibraryKind::kEnumeration:
              case RuntimeLibraryKind::kTimeFormat:
              case RuntimeLibraryKind::kDpiBitChunk:
              case RuntimeLibraryKind::kDpiLogicChunk:
              case RuntimeLibraryKind::kDpiOpenArrayHandle:
              case RuntimeLibraryKind::kCancellationTarget:
              case RuntimeLibraryKind::kControlEffect:
              case RuntimeLibraryKind::kPropertyCoordinate:
              case RuntimeLibraryKind::kBehaviorCoordinate:
              case RuntimeLibraryKind::kObjectDefinition:
                return std::nullopt;
            }
            throw InternalError("lir: unknown runtime library kind");
          },

          // Machine data is held as itself: a scalar, an array, a code address.
          [](const MachineCStringType&) -> Held { return std::nullopt; },
          [](const MachineBoolType&) -> Held { return std::nullopt; },
          [](const MachineIntType&) -> Held { return std::nullopt; },
          [](const MachineFloatType&) -> Held { return std::nullopt; },
          [](const MachineArrayType&) -> Held { return std::nullopt; },
          [](const MachineFunctionType&) -> Held { return std::nullopt; },
          // The absence of a type is not a value of one.
          [](const VoidType&) -> Held { return std::nullopt; },
          // An object, the several ways of naming one, and storage an owner
          // holds: each is reached where it lives rather than held, and what it
          // holds or answers with is a value of its own.
          [](const ObjectType&) -> Held { return std::nullopt; },
          [](const ExternalUnitObjectType&) -> Held { return std::nullopt; },
          [](const CrossUnitClassType&) -> Held { return std::nullopt; },
          [](const OpaqueObjectType&) -> Held { return std::nullopt; },
          [](const RuntimeClassType&) -> Held { return std::nullopt; },
          [](const StructType&) -> Held { return std::nullopt; },
          [](const EventType&) -> Held { return std::nullopt; },
          [](const ObservableType&) -> Held { return std::nullopt; },
          [](const ResolvedType&) -> Held { return std::nullopt; },
          [](const DriverType&) -> Held { return std::nullopt; },
          [](const SampledHistoryType&) -> Held { return std::nullopt; },
          [](const EvaluationAttemptsType&) -> Held { return std::nullopt; },
          [](const RuntimeEffectsType&) -> Held { return std::nullopt; },
          [](const FilesType&) -> Held { return std::nullopt; },
          [](const DiagnosticType&) -> Held { return std::nullopt; },
          [](const RefType&) -> Held { return std::nullopt; },
          [](const VectorType&) -> Held { return std::nullopt; },
          // A body in flight is taken by whoever drives it, the moment it is
          // made, so nothing is left for its maker to hold.
          [](const CoroutineType&) -> Held { return std::nullopt; }});
}

auto Type::IsOwnedValue() const -> bool {
  return HeldObject().has_value();
}

auto MemberTypes(const std::vector<AggregateMember>& members)
    -> std::vector<TypeId> {
  std::vector<TypeId> types;
  types.reserve(members.size());
  for (const AggregateMember& member : members) {
    types.push_back(member.type);
  }
  return types;
}

auto Type::IsIntegralPacked() const -> bool {
  return Is<PackedArrayType>() || Is<PackedStructType>() ||
         Is<PackedUnionType>();
}

auto Type::IsUnion() const -> bool {
  return Is<UnionType>() || Is<TaggedUnionType>();
}

auto Type::UnionMemberTypes() const -> std::vector<TypeId> {
  if (const auto* untagged = As<UnionType>()) {
    return MemberTypes(untagged->members);
  }
  if (const auto* tagged = As<TaggedUnionType>()) {
    return MemberTypes(tagged->members);
  }
  throw InternalError("lir: type is not a union");
}

auto Type::IsProduct() const -> bool {
  return Is<TupleType>() || Is<UnpackedStructType>();
}

auto Type::ProductComponentTypes() const -> std::vector<TypeId> {
  if (const auto* tuple = As<TupleType>()) {
    return tuple->elements;
  }
  if (const auto* structure = As<UnpackedStructType>()) {
    return MemberTypes(structure->members);
  }
  throw InternalError("lir: type is not a product");
}

auto Type::MachineIntegerSignedness() const -> std::optional<Signedness> {
  if (const auto* machine = As<MachineIntType>()) {
    return machine->signedness;
  }
  if (Is<MachineBoolType>()) {
    return Signedness::kUnsigned;
  }
  return std::nullopt;
}

auto Type::PackedShape() const -> const PackedArrayType& {
  if (const auto* packed = As<PackedArrayType>()) {
    return *packed;
  }
  if (const auto* packed_struct = As<PackedStructType>()) {
    return packed_struct->base;
  }
  if (const auto* packed_union = As<PackedUnionType>()) {
    return packed_union->base;
  }
  throw InternalError("lir: type has no packed shape; it is not integral");
}

}  // namespace lyra::lir
