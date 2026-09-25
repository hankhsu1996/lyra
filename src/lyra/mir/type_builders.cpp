#include "lyra/mir/type_builders.hpp"

#include <cstddef>
#include <cstdint>

#include "lyra/base/overloaded.hpp"
#include "lyra/mir/type.hpp"
#include "lyra/mir/type_id.hpp"

namespace lyra::mir {

auto PackedVectorOf(
    const TypePool& types, std::uint64_t width, IntegralStateKind state_kind)
    -> TypeId {
  return types.Intern(
      Type{PackedArrayType{
          .state_kind = state_kind,
          .signedness = Signedness::kUnsigned,
          .dims = {PackedRange{
              .left = static_cast<std::int64_t>(width) - 1, .right = 0}}}});
}

auto MachineArrayOf(const TypePool& types, TypeId element, std::size_t size)
    -> TypeId {
  return types.Intern(
      Type{MachineArrayType{
          .element = element, .size = static_cast<std::uint32_t>(size)}});
}

auto ErasedFunction(const TypePool& types) -> TypeId {
  return types.Intern(
      Type{MachineFunctionType{
          .params = {}, .result = types.Intern(Type{VoidType{}})}});
}

auto IsErasedFunction(const TypePool& types, TypeId id) -> bool {
  return id == ErasedFunction(types);
}

auto ErasedPointer(const TypePool& types) -> TypeId {
  return types.Intern(
      Type{PointerType{
          .pointee = types.Intern(Type{VoidType{}}),
          .ownership = PointerOwnership::kBorrowed}});
}

auto ObservableCellOf(const TypePool& types, TypeId value_type) -> TypeId {
  // One arm per MIR type and no catch-all: a type added later fails to compile
  // here until it is classified, rather than silently defaulting to
  // non-observable -- which would leave a new value type's writes firing no
  // subscribers, a bug no test would obviously catch.
  const auto wrap = [&] {
    return types.Intern(Type{ObservableType{.value = value_type}});
  };
  const auto bare = [&] { return value_type; };
  const Type& value = types.Get(value_type);
  return value.Visit(
      Overloaded{
          // A SystemVerilog value-storage data object (LRM 6.5): a variable of
          // one of these is an observable signal, so it is wrapped in the cell
          // that fires subscribers on change.
          [&](const PackedArrayType&) { return wrap(); },
          [&](const EnumType&) { return wrap(); },
          [&](const PackedStructType&) { return wrap(); },
          [&](const PackedUnionType&) { return wrap(); },
          [&](const UnpackedArrayType&) { return wrap(); },
          [&](const DynamicArrayType&) { return wrap(); },
          [&](const QueueType&) { return wrap(); },
          [&](const AssociativeArrayType&) { return wrap(); },
          [&](const StringType&) { return wrap(); },
          [&](const RealType&) { return wrap(); },
          [&](const ShortRealType&) { return wrap(); },
          [&](const RealTimeType&) { return wrap(); },
          [&](const TupleType&) { return wrap(); },
          [&](const UnpackedStructType&) { return wrap(); },
          [&](const UnionType&) { return wrap(); },
          [&](const TaggedUnionType&) { return wrap(); },
          [&](const EmptyType&) { return wrap(); },
          // A variable naming an object is value storage too: LRM 9.4.2 makes a
          // write to an object handle an event when what it names is not what
          // it named before, and the clause's own example puts that beside a
          // wait on a property of the object to say they are two waits.
          [&](const ManagedRefType&) { return wrap(); },
          // A chandle is value storage too, and is not wrapped for a reason of
          // its own: its value is the pointer it carries (LRM 6.14), so a null
          // one is a null pointer and nothing tells it apart from a watched
          // expression that settled no value at all. Such a wait waits for that
          // pointer to change, which needs the domain to carry its value the
          // way every other one does.
          [&](const ChandleType&) { return bare(); },
          // Not value storage, so its own declaration shape is its storage and
          // it is not wrapped: a handle onto storage of another lifetime
          // (pointer, borrowed reference, vector), an object (a class instance
          // or an instantiated child), a named event (LRM 15 -- it carries its
          // own subscribe mechanism), a runtime facade (effects, files,
          // diagnostics, a runtime-library type), a coroutine result, a machine
          // primitive (a plain boolean, integer, float, C string, array, or
          // code address), a compiler-generated promoted scope struct or
          // closure, an internal index, `void`, the observable, net-cell and
          // sampled-history wrappers themselves, which are already storage, and
          // a write open on one of them.
          [&](const WildcardIndexType&) { return bare(); },
          [&](const MachineCStringType&) { return bare(); },
          [&](const MachineBoolType&) { return bare(); },
          [&](const MachineIntType&) { return bare(); },
          [&](const MachineFloatType&) { return bare(); },
          [&](const MachineArrayType&) { return bare(); },
          [&](const MachineFunctionType&) { return bare(); },
          [&](const EventType&) { return bare(); },
          [&](const VoidType&) { return bare(); },
          [&](const ObjectType&) { return bare(); },
          [&](const ExternalUnitObjectType&) { return bare(); },
          [&](const CrossUnitClassType&) { return bare(); },
          [&](const OpaqueObjectType&) { return bare(); },
          [&](const RuntimeClassType&) { return bare(); },
          [&](const RuntimeEffectsType&) { return bare(); },
          [&](const FilesType&) { return bare(); },
          [&](const DiagnosticType&) { return bare(); },
          [&](const RuntimeLibraryType&) { return bare(); },
          [&](const CoroutineType&) { return bare(); },
          [&](const RefType&) { return bare(); },
          [&](const PointerType&) { return bare(); },
          [&](const VectorType&) { return bare(); },
          [&](const ObservableType&) { return bare(); },
          [&](const ResolvedType&) { return bare(); },
          [&](const DriverType&) { return bare(); },
          [&](const OpenWriteType&) { return bare(); },
          [&](const SampledHistoryType&) { return bare(); },
          [&](const EvaluationAttemptsType&) { return bare(); },
          [&](const StructType&) { return bare(); },
          [&](const ClosureType&) { return bare(); },
      });
}

}  // namespace lyra::mir
