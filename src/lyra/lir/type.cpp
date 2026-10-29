#include "lyra/lir/type.hpp"

#include <cstddef>
#include <cstdint>
#include <functional>
#include <optional>
#include <variant>
#include <vector>

#include "lyra/base/hash.hpp"
#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/lir/type_id.hpp"
#include "lyra/support/runtime_object.hpp"
#include "lyra/support/value_domain.hpp"

namespace lyra::lir {

namespace {

void Combine(std::size_t& seed, std::uint64_t value) {
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

void Combine(std::size_t& seed, const IntegralType& integral) {
  Combine(seed, integral.bit_width);
  Combine(seed, integral.signedness);
  Combine(seed, integral.state_kind);
}

// Which runtime-library value this is. A reader that could not handle one needs
// to know which of them it met, and the kind is the whole of what separates
// them.
auto RuntimeLibraryKindName(RuntimeLibraryKind kind) -> const char* {
  switch (kind) {
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
    case RuntimeLibraryKind::kBitVec:
      return "bit vector";
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
    case RuntimeLibraryKind::kWatchedPlace:
      return "watched place";
    case RuntimeLibraryKind::kTrigger:
      return "trigger";
    case RuntimeLibraryKind::kObservation:
      return "observation";
    case RuntimeLibraryKind::kReadReport:
      return "read report";
    case RuntimeLibraryKind::kWait:
      return "wait";
    case RuntimeLibraryKind::kLeasedReference:
      return "leased reference";
    case RuntimeLibraryKind::kObjectWrite:
      return "object write";
    case RuntimeLibraryKind::kCancellationTarget:
      return "cancellation target";
    case RuntimeLibraryKind::kTimer:
      return "timer";
    case RuntimeLibraryKind::kDeferredEffects:
      return "deferred effects";
    case RuntimeLibraryKind::kViolationReportKeeper:
      return "violation report keeper";
    case RuntimeLibraryKind::kControlEffect:
      return "control effect";
    case RuntimeLibraryKind::kObjectDefinition:
      return "object definition";
    case RuntimeLibraryKind::kScopeInfo:
      return "scope info";
    case RuntimeLibraryKind::kScopeCallable:
      return "scope callable";
    case RuntimeLibraryKind::kClassObjectInfo:
      return "class object info";
    case RuntimeLibraryKind::kInterfaceRow:
      return "interface row";
    case RuntimeLibraryKind::kConstructionTable:
      return "construction table";
    case RuntimeLibraryKind::kInstantiation:
      return "instantiation";
    case RuntimeLibraryKind::kGeneratePlaces:
      return "generate places";
    case RuntimeLibraryKind::kDumpEntry:
      return "dump entry";
    case RuntimeLibraryKind::kDumpBlock:
      return "dump block";
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
          [&](const IntegralType& t) { Combine(seed, t); },
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
          [](const AnyType&) {},
          [](const AnyReferenceType&) {},
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
          [](const RealType&) {},
          [](const ShortRealType&) {},
          [](const ChandleType&) {},
          [](const VoidType&) {},
          [](const EmptyType&) {},
          [&](const ObjectType& t) { Combine(seed, t.class_id); },
          [&](const RuntimeClassType& t) { Combine(seed, t.which); },
          [&](const ClosureType& t) { Combine(seed, t.closure_id.value); },
          [&](const ErasedCallableType& t) {
            for (const TypeId param : t.params) {
              Combine(seed, param);
            }
            Combine(seed, t.result);
          },
          [](const RuntimeEffectsType&) {},
          [](const FilesType&) {},
          [](const DiagnosticType&) {},
          [&](const RuntimeLibraryType& t) { Combine(seed, t.kind); },
          [&](const CoroutineType& t) { Combine(seed, t.payload); },
          [&](const VariableRefType& t) {
            Combine(seed, t.pointee);
            Combine(seed, t.mutability);
          },
          [&](const PointerType& t) {
            Combine(seed, t.pointee);
            Combine(seed, t.ownership);
            Combine(seed, t.mutability);
          },
          [&](const ManagedRefType& t) { Combine(seed, t.pointee); },
          [&](const WeakManagedRefType& t) { Combine(seed, t.pointee); },
          [&](const VectorType& t) { Combine(seed, t.element); },
          [&](const TupleType& t) { Combine(seed, t.elements); },
          [&](const RecordType& t) { Combine(seed, t.declaration.value); },
          [&](const ResolvedType& t) { Combine(seed, t.value); },
          [&](const DriverType& t) { Combine(seed, t.value); },
          [&](const ObservableType& t) { Combine(seed, t.value); },
          [&](const SampledHistoryType& t) { Combine(seed, t.value); },
          [](const EvaluationAttemptsType&) {},
          [](const LoopBlocksType&) {},
          [&](const OpenWriteType& t) { Combine(seed, t.value); },
          [&](const DesignationType& t) { Combine(seed, t.value); }});
  return seed;
}

auto Type::KindName() const -> std::string_view {
  return Visit(
      Overloaded{
          [](const IntegralType&) { return "integral"; },
          [](const UnpackedArrayType&) { return "unpacked array"; },
          [](const DynamicArrayType&) { return "dynamic array"; },
          [](const QueueType&) { return "queue"; },
          [](const AssociativeArrayType&) { return "associative array"; },
          [](const WildcardIndexType&) { return "wildcard index"; },
          [](const AnyType&) { return "value of any type"; },
          [](const AnyReferenceType&) {
            return "reference to a value of any type";
          },
          [](const StringType&) { return "string"; },
          [](const MachineCStringType&) { return "machine C string"; },
          [](const MachineBoolType&) { return "machine boolean"; },
          [](const MachineIntType&) { return "machine integer"; },
          [](const MachineFloatType&) { return "machine float"; },
          [](const MachineArrayType&) { return "machine array"; },
          [](const MachineFunctionType&) { return "machine function"; },
          [](const RealType&) { return "real"; },
          [](const ShortRealType&) { return "shortreal"; },
          [](const ChandleType&) { return "chandle"; },
          [](const VoidType&) { return "void"; },
          [](const EmptyType&) { return "empty"; },
          [](const ObjectType&) { return "class object"; },
          [](const RuntimeClassType&) { return "runtime class"; },
          [](const ClosureType&) { return "closure"; },
          [](const ErasedCallableType&) { return "erased callable"; },
          [](const RecordType&) { return "record"; },
          [](const RuntimeEffectsType&) { return "runtime services"; },
          [](const FilesType&) { return "file table"; },
          [](const DiagnosticType&) { return "diagnostic dispatcher"; },
          [](const RuntimeLibraryType& t) {
            return RuntimeLibraryKindName(t.kind);
          },
          [](const CoroutineType&) { return "coroutine"; },
          [](const VariableRefType&) { return "variable reference"; },
          [](const PointerType&) { return "pointer"; },
          [](const ManagedRefType&) { return "managed reference"; },
          [](const WeakManagedRefType&) { return "weak reference"; },
          [](const VectorType&) { return "vector"; },
          [](const TupleType&) { return "tuple"; },
          [](const ResolvedType&) { return "net resolution node"; },
          [](const DriverType&) { return "net driver"; },
          [](const ObservableType&) { return "observable cell"; },
          [](const SampledHistoryType&) { return "sampled value history"; },
          [](const EvaluationAttemptsType&) {
            return "concurrent assertion attempts";
          },
          [](const LoopBlocksType&) { return "loop generate blocks"; },
          [](const OpenWriteType&) { return "open write"; },
          [](const DesignationType&) { return "designation"; }});
}

auto Type::Declaration() const -> std::optional<TypeDeclaration> {
  using Declared = std::optional<TypeDeclaration>;
  const auto names_none = []() -> Declared { return std::nullopt; };
  return Visit(
      Overloaded{
          // Member-bearing storage, whose members are reached through its
          // declaration.
          [](const ObjectType& t) -> Declared { return t; },
          [](const ClosureType& t) -> Declared { return t; },

          // A record names a declaration too, but of a value, whose members
          // are reached by position as a tuple's components are.
          [&](const RecordType&) { return names_none(); },

          // A value, however it is shaped and however its elements are held.
          [&](const IntegralType&) { return names_none(); },
          [&](const UnpackedArrayType&) { return names_none(); },
          [&](const DynamicArrayType&) { return names_none(); },
          [&](const QueueType&) { return names_none(); },
          [&](const AssociativeArrayType&) { return names_none(); },
          [&](const WildcardIndexType&) { return names_none(); },
          [&](const AnyType&) { return names_none(); },
          [&](const AnyReferenceType&) { return names_none(); },
          [&](const StringType&) { return names_none(); },
          [&](const RealType&) { return names_none(); },
          [&](const ShortRealType&) { return names_none(); },
          [&](const ChandleType&) { return names_none(); },
          [&](const TupleType&) { return names_none(); },
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
          [&](const ErasedCallableType&) { return names_none(); },

          // A class the runtime library defines: a symbol is its whole
          // identity, so no declaration of this unit stands behind it.
          [&](const RuntimeClassType&) { return names_none(); },

          // Storage, a service, and an address. Each stands for something
          // else; what a declaration is behind is whatever it stands for.
          [&](const ObservableType&) { return names_none(); },
          [&](const ResolvedType&) { return names_none(); },
          [&](const DriverType&) { return names_none(); },
          [&](const SampledHistoryType&) { return names_none(); },
          [&](const EvaluationAttemptsType&) { return names_none(); },
          [&](const LoopBlocksType&) { return names_none(); },
          [&](const OpenWriteType&) { return names_none(); },
          [&](const DesignationType&) { return names_none(); },
          [&](const RuntimeEffectsType&) { return names_none(); },
          [&](const FilesType&) { return names_none(); },
          [&](const DiagnosticType&) { return names_none(); },
          [&](const RuntimeLibraryType&) { return names_none(); },
          [&](const CoroutineType&) { return names_none(); },
          [&](const VariableRefType&) { return names_none(); },
          [&](const PointerType&) { return names_none(); },
          [&](const ManagedRefType&) { return names_none(); },
          [&](const WeakManagedRefType&) { return names_none(); },
          [&](const VectorType&) { return names_none(); }});
}

auto Type::ContainerElementType() const -> std::optional<TypeId> {
  using Element = std::optional<TypeId>;
  return Visit(
      Overloaded{
          // The four a declaration names as holding a sequence of values,
          // however the sequence is sized and however it is indexed.
          [](const UnpackedArrayType& t) -> Element { return t.element_type; },
          [](const DynamicArrayType& t) -> Element { return t.element_type; },
          [](const QueueType& t) -> Element { return t.element_type; },
          [](const AssociativeArrayType& t) -> Element {
            return t.element_type;
          },

          // These hold a sequence of values too and are still not containers: a
          // lowering builds them to carry something, where a container is a
          // type a declaration named.
          [](const MachineArrayType&) -> Element { return std::nullopt; },
          [](const VectorType&) -> Element { return std::nullopt; },

          // One vector of bits: what looks like an element is bits of that
          // vector rather than a value held beside the others.
          [](const IntegralType&) -> Element { return std::nullopt; },

          // Parts side by side, or members overlaid, but never a sequence of
          // one type.
          [](const TupleType&) -> Element { return std::nullopt; },
          [](const RecordType&) -> Element { return std::nullopt; },

          // A single quantity or a single token.
          [](const WildcardIndexType&) -> Element { return std::nullopt; },
          [](const AnyType&) -> Element { return std::nullopt; },
          [](const AnyReferenceType&) -> Element { return std::nullopt; },
          [](const StringType&) -> Element { return std::nullopt; },
          [](const MachineCStringType&) -> Element { return std::nullopt; },
          [](const MachineBoolType&) -> Element { return std::nullopt; },
          [](const MachineIntType&) -> Element { return std::nullopt; },
          [](const MachineFloatType&) -> Element { return std::nullopt; },
          [](const RealType&) -> Element { return std::nullopt; },
          [](const ShortRealType&) -> Element { return std::nullopt; },
          [](const ChandleType&) -> Element { return std::nullopt; },
          [](const EmptyType&) -> Element { return std::nullopt; },
          [](const VoidType&) -> Element { return std::nullopt; },

          // Storage that keeps one value, and a write open on such storage;
          // the sequence, where there is one, belongs to the value kept.
          [](const ObservableType&) -> Element { return std::nullopt; },
          [](const ResolvedType&) -> Element { return std::nullopt; },
          [](const DriverType&) -> Element { return std::nullopt; },
          [](const SampledHistoryType&) -> Element { return std::nullopt; },
          [](const EvaluationAttemptsType&) -> Element { return std::nullopt; },
          [](const LoopBlocksType&) -> Element { return std::nullopt; },
          [](const OpenWriteType&) -> Element { return std::nullopt; },
          [](const DesignationType&) -> Element { return std::nullopt; },

          // These refer to a value living elsewhere rather than holding one,
          // so a container reached through one is reached by dereferencing it
          // first.
          [](const VariableRefType&) -> Element { return std::nullopt; },
          [](const PointerType&) -> Element { return std::nullopt; },
          [](const ManagedRefType&) -> Element { return std::nullopt; },
          [](const WeakManagedRefType&) -> Element { return std::nullopt; },
          [](const CoroutineType&) -> Element { return std::nullopt; },
          [](const MachineFunctionType&) -> Element { return std::nullopt; },

          // A nominal type names a declaration, and a declaration is not a run
          // of anything.
          [](const ObjectType&) -> Element { return std::nullopt; },
          [](const RuntimeClassType&) -> Element { return std::nullopt; },
          [](const ClosureType&) -> Element { return std::nullopt; },
          [](const ErasedCallableType&) -> Element { return std::nullopt; },

          // A handle to a runtime facility, and a type the library declares,
          // whose shape is the library's.
          [](const RuntimeEffectsType&) -> Element { return std::nullopt; },
          [](const FilesType&) -> Element { return std::nullopt; },
          [](const DiagnosticType&) -> Element { return std::nullopt; },
          [](const RuntimeLibraryType&) -> Element { return std::nullopt; }});
}

auto Type::Pointee() const -> std::optional<TypeId> {
  if (const auto* pointer = As<PointerType>()) {
    return pointer->pointee;
  }
  if (const auto* reference = As<VariableRefType>()) {
    return reference->pointee;
  }
  if (const auto* managed = As<ManagedRefType>()) {
    return managed->pointee;
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
  if (const auto* library = As<RuntimeLibraryType>()) {
    switch (library->kind) {
      // Storage an owner holds and reaches by its address: what a disable
      // names, what a delayed continuous assignment propagates through, and
      // the keepers a deferred closure is emplaced in.
      case RuntimeLibraryKind::kCancellationTarget:
      case RuntimeLibraryKind::kTimer:
      case RuntimeLibraryKind::kDeferredEffects:
      case RuntimeLibraryKind::kViolationReportKeeper:
        return true;
      case RuntimeLibraryKind::kBitVec:
      case RuntimeLibraryKind::kEnumeration:
      case RuntimeLibraryKind::kPrintItem:
      case RuntimeLibraryKind::kPrintLiteralItem:
      case RuntimeLibraryKind::kPrintValueItem:
      case RuntimeLibraryKind::kFormatSpec:
      case RuntimeLibraryKind::kFormatArg:
      case RuntimeLibraryKind::kChannelCancellation:
      case RuntimeLibraryKind::kTimeFormat:
      case RuntimeLibraryKind::kHierarchySegment:
      case RuntimeLibraryKind::kDpiBitBuffer:
      case RuntimeLibraryKind::kDpiLogicBuffer:
      case RuntimeLibraryKind::kDpiBitChunk:
      case RuntimeLibraryKind::kDpiLogicChunk:
      case RuntimeLibraryKind::kDpiOpenArray:
      case RuntimeLibraryKind::kDpiOpenArrayHandle:
      case RuntimeLibraryKind::kWatchedPlace:
      case RuntimeLibraryKind::kTrigger:
      case RuntimeLibraryKind::kObservation:
      case RuntimeLibraryKind::kReadReport:
      case RuntimeLibraryKind::kWait:
      case RuntimeLibraryKind::kLeasedReference:
      case RuntimeLibraryKind::kObjectWrite:
      case RuntimeLibraryKind::kControlEffect:
      case RuntimeLibraryKind::kObjectDefinition:
      case RuntimeLibraryKind::kScopeInfo:
      case RuntimeLibraryKind::kScopeCallable:
      case RuntimeLibraryKind::kClassObjectInfo:
      case RuntimeLibraryKind::kInterfaceRow:
      case RuntimeLibraryKind::kConstructionTable:
      case RuntimeLibraryKind::kInstantiation:
      case RuntimeLibraryKind::kGeneratePlaces:
      case RuntimeLibraryKind::kDumpEntry:
      case RuntimeLibraryKind::kDumpBlock:
        return false;
    }
    throw InternalError("lir: unknown runtime library kind");
  }
  return Is<ObservableType>() || Is<ResolvedType>() || Is<ObjectType>() ||
         Is<RuntimeClassType>() || Is<SampledHistoryType>() ||
         Is<EvaluationAttemptsType>() || Is<LoopBlocksType>();
}

auto Type::HeldAs() const -> std::optional<Holding> {
  using Held = std::optional<Holding>;
  using support::LibraryObject;
  using support::ValueDomain;
  return Visit(
      Overloaded{
          [](const IntegralType&) -> Held { return IntegralBits{}; },
          // LRM 7.8.1 gives a wildcard-indexed array no index data type, so
          // this type names where an index goes rather than what one is made
          // of. What goes there is always integral, at whatever width the
          // expression carried, which is why it is held with its type.
          [](const WildcardIndexType&) -> Held {
            return ValueDomain::kWildcardIndex;
          },
          [](const StringType&) -> Held { return ValueDomain::kString; },
          [](const AnyType&) -> Held { return ValueDomain::kAny; },
          [](const AnyReferenceType&) -> Held {
            return LibraryObject::kAnyReference;
          },
          [](const WeakManagedRefType&) -> Held {
            return LibraryObject::kWeakReference;
          },
          [](const RealType&) -> Held { return ValueDomain::kReal; },
          [](const ShortRealType&) -> Held { return ValueDomain::kShortReal; },
          // A chandle (LRM 6.14) is a value holding one host pointer.
          [](const ChandleType&) -> Held { return ValueDomain::kChandle; },
          // A value laid out by its type -- a tuple, a structure, a union of
          // either kind, a fixed-size unpacked array (LRM 7.4.2) -- is held as
          // the bytes that type lays out, whatever the type says lies where.
          [](const TupleType&) -> Held { return ValueDomain::kLaidOut; },
          [](const RecordType&) -> Held { return ValueDomain::kLaidOut; },
          [](const UnpackedArrayType&) -> Held {
            return ValueDomain::kLaidOut;
          },
          // A tagged union's `void` member (LRM 7.3.2) is a value carrying no
          // bits, a value of its own so a build's payload is uniform whatever
          // the member type.
          [](const EmptyType&) -> Held { return ValueDomain::kEmpty; },
          // A container whose size changes as the program runs holds its
          // elements in storage of its own, and nothing its declaration says:
          // a queue's bound (LRM 7.10) and an associative array's index type
          // (LRM 7.8) each reach an operation as an operand of their own, so
          // one runtime value per kind serves every declared shape.
          [](const DynamicArrayType&) -> Held {
            return ValueDomain::kDynArray;
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

          [](const ClosureType&) -> Held { return ClosureStorage{}; },
          // A shared pointer is a hold on storage that outlives the scope
          // asking for it, and letting the hold go is what ends it; a unique
          // one owns what it points at, and its end ends that and gives the
          // storage back; a borrowed one names storage somebody else ends.
          [](const PointerType& pointer) -> Held {
            switch (pointer.ownership) {
              case PointerOwnership::kShared:
                return LibraryObject::kSharedPointer;
              case PointerOwnership::kUnique:
                return OwnedStorageAddress{};
              case PointerOwnership::kBorrowed:
                return std::nullopt;
            }
            throw InternalError("lir: unknown pointer ownership");
          },
          [](const RuntimeLibraryType& library) -> Held {
            switch (library.kind) {
              // A value of the library's own, held as any value is.
              case RuntimeLibraryKind::kBitVec:
                return ValueDomain::kBitVec;
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
              // Held by the waiting frame across the waits it states into.
              case RuntimeLibraryKind::kReadReport:
                return LibraryObject::kReadReport;
              // Held by the body across every stop it makes at it.
              case RuntimeLibraryKind::kWait:
                return LibraryObject::kWait;
              // Held by the binding while the call runs; ending it lets go of
              // what it kept.
              case RuntimeLibraryKind::kLeasedReference:
                return LibraryObject::kLeasedReference;
              // Held by the writer for as long as the write lasts; ending it
              // is what tells the object it was written.
              case RuntimeLibraryKind::kObjectWrite:
                return LibraryObject::kObjectWrite;
              // Kept by the run -- an enumeration's members, a class's
              // record, the time format, a construction table -- or reached
              // inside something else -- a buffer's chunk, an image's handle,
              // the effect a departure carries, the target a disable names,
              // the timer a delayed continuous assignment propagates through.
              case RuntimeLibraryKind::kConstructionTable:
              case RuntimeLibraryKind::kInstantiation:
              case RuntimeLibraryKind::kGeneratePlaces:
              case RuntimeLibraryKind::kEnumeration:
              case RuntimeLibraryKind::kTimeFormat:
              case RuntimeLibraryKind::kDpiBitChunk:
              case RuntimeLibraryKind::kDpiLogicChunk:
              case RuntimeLibraryKind::kDpiOpenArrayHandle:
              case RuntimeLibraryKind::kWatchedPlace:
              case RuntimeLibraryKind::kCancellationTarget:
              case RuntimeLibraryKind::kTimer:
              case RuntimeLibraryKind::kDeferredEffects:
              case RuntimeLibraryKind::kViolationReportKeeper:
              case RuntimeLibraryKind::kControlEffect:
              case RuntimeLibraryKind::kObjectDefinition:
              case RuntimeLibraryKind::kScopeInfo:
              case RuntimeLibraryKind::kScopeCallable:
              case RuntimeLibraryKind::kClassObjectInfo:
              case RuntimeLibraryKind::kInterfaceRow:
              case RuntimeLibraryKind::kDumpEntry:
              case RuntimeLibraryKind::kDumpBlock:
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
          // Only ever behind a box or a borrow, never held as itself.
          [](const ErasedCallableType&) -> Held { return std::nullopt; },
          // The absence of a type is not a value of one.
          [](const VoidType&) -> Held { return std::nullopt; },
          // An object, the several ways of naming one, and storage an owner
          // holds: each is reached where it lives rather than held, and what it
          // holds or answers with is a value of its own.
          [](const ObjectType&) -> Held { return std::nullopt; },
          [](const RuntimeClassType&) -> Held { return std::nullopt; },
          [](const ObservableType&) -> Held { return std::nullopt; },
          [](const ResolvedType&) -> Held { return std::nullopt; },
          [](const DriverType&) -> Held { return std::nullopt; },
          [](const SampledHistoryType&) -> Held { return std::nullopt; },
          [](const EvaluationAttemptsType&) -> Held { return std::nullopt; },
          [](const LoopBlocksType&) -> Held { return std::nullopt; },
          [](const RuntimeEffectsType&) -> Held { return std::nullopt; },
          [](const FilesType&) -> Held { return std::nullopt; },
          [](const DiagnosticType&) -> Held { return std::nullopt; },
          [](const VectorType&) -> Held { return std::nullopt; },
          // A body in flight is taken by whoever drives it, the moment it is
          // made, so nothing is left for its maker to hold.
          [](const CoroutineType&) -> Held { return std::nullopt; },
          // Held by the writer for as long as the write lasts; ending it is
          // what reports the write. A part designated within it is built by
          // the step that reaches it and borrows the write.
          [](const OpenWriteType&) -> Held {
            return LibraryObject::kOpenWrite;
          },
          [](const DesignationType&) -> Held {
            return LibraryObject::kDesignation;
          },
          // Where a value lies and the variable, if any, it belongs to, so a
          // write through it is told to whoever waits on that variable. The
          // storage it names is somebody else's, so ending it ends nothing.
          [](const VariableRefType&) -> Held {
            return LibraryObject::kReference;
          }});
}

auto Type::IsOwnedValue() const -> bool {
  return HeldAs().has_value();
}

auto Type::IsMoveOnly() const -> bool {
  const std::optional<Holding> held = HeldAs();
  return held.has_value() && std::holds_alternative<OwnedStorageAddress>(*held);
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

}  // namespace lyra::lir
