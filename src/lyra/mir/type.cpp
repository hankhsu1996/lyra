#include "lyra/mir/type.hpp"

#include <algorithm>
#include <cstddef>
#include <cstdint>
#include <functional>
#include <optional>
#include <type_traits>
#include <utility>
#include <variant>
#include <vector>

#include "lyra/base/hash.hpp"
#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/mir/integral_constant.hpp"
#include "lyra/support/library_decl.hpp"
#include "lyra/support/movability.hpp"
#include "lyra/value/integral.hpp"
#include "lyra/value/integral_words.hpp"

namespace lyra::mir {

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
  throw InternalError("mir: unknown MachineIntWidth");
}

namespace {

using base::HashField;

void HashId(std::size_t& seed, TypeId id) {
  HashField(seed, id.value);
}

void HashIds(std::size_t& seed, const std::vector<TypeId>& ids) {
  for (const TypeId id : ids) {
    HashId(seed, id);
  }
}

template <typename E>
  requires std::is_enum_v<E>
void HashEnum(std::size_t& seed, E value) {
  HashField(seed, static_cast<std::uint64_t>(value));
}

}  // namespace

auto StateDomainOf(IntegralStateKind state) -> value::StateDomain {
  switch (state) {
    case IntegralStateKind::kTwoState:
      return value::StateDomain::kTwoState;
    case IntegralStateKind::kFourState:
      return value::StateDomain::kFourState;
  }
  throw InternalError("mir: unknown integral state kind");
}

auto BlankIntegralConstant(const IntegralType& integral) -> IntegralConstant {
  const std::size_t words = value::WordCountForBits(integral.bit_width);
  switch (integral.state_kind) {
    case IntegralStateKind::kTwoState:
      return IntegralConstant{
          .value_words = std::vector<std::uint64_t>(words, 0U),
          .state_words = {}};
    case IntegralStateKind::kFourState:
      return IntegralConstant{
          .value_words = std::vector<std::uint64_t>(words, 0U),
          .state_words = std::vector<std::uint64_t>(words, 0U)};
  }
  throw InternalError("BlankIntegralConstant: unknown IntegralStateKind");
}

void HashIntegral(std::size_t& seed, const IntegralType& integral) {
  HashField(seed, integral.bit_width);
  HashEnum(seed, integral.signedness);
  HashEnum(seed, integral.state_kind);
}

void HashEnumeration(std::size_t& seed, const EnumType& enumeration) {
  HashIntegral(seed, enumeration.base);
  for (const EnumMember& member : enumeration.members) {
    HashField(seed, member.name);
    HashIntegralConstant(seed, member.value);
  }
}

auto Type::Hash::operator()(const Type& type) const -> std::size_t {
  std::size_t seed = std::hash<std::size_t>{}(type.data_.index());
  type.Visit(
      Overloaded{
          [&](const IntegralType& t) { HashIntegral(seed, t); },
          [&](const EnumType& t) { HashEnumeration(seed, t); },
          [&](const UnpackedArrayType& t) {
            HashId(seed, t.element_type);
            HashField(seed, t.count);
          },
          [](const ErasedValueType&) {},
          [](const MachineCStringType&) {},
          [](const MachineBoolType&) {},
          [&](const MachineIntType& t) {
            HashEnum(seed, t.width);
            HashEnum(seed, t.signedness);
          },
          [&](const MachineFloatType& t) { HashEnum(seed, t.width); },
          [&](const MachineArrayType& t) {
            HashId(seed, t.element);
            HashField(seed, t.size);
          },
          [&](const MachineSpanType& t) { HashId(seed, t.element); },
          [&](const MachineFunctionType& t) {
            HashIds(seed, t.params);
            HashId(seed, t.result);
          },
          [&](const RealType& t) { HashEnum(seed, t.width); },
          [](const VoidType&) {},
          [&](const ObjectType& t) { HashField(seed, t.of.value); },
          [&](const LibraryType& t) {
            HashEnum(seed, t.decl);
            for (const LibraryType::Arg& arg : t.args) {
              std::visit(
                  Overloaded{
                      [&](TypeId type) { HashId(seed, type); },
                      [&](const support::QueueBound& bound) {
                        HashField(seed, bound.index());
                        if (const auto* most =
                                std::get_if<support::BoundedQueue>(&bound)) {
                          HashField(seed, most->count);
                        }
                      }},
                  arg);
            }
          },
          [&](const CoroutineType& t) { HashId(seed, t.payload); },
          [&](const VariableRefType& t) {
            HashId(seed, t.pointee);
            HashEnum(seed, t.mutability);
          },
          [&](const ReferenceWrapperType& t) {
            HashId(seed, t.pointee);
            HashEnum(seed, t.mutability);
          },
          [&](const PointerType& t) {
            HashId(seed, t.pointee);
            HashEnum(seed, t.ownership);
            HashEnum(seed, t.mutability);
          },
          [&](const ReferenceType& t) {
            HashId(seed, t.referent);
            HashEnum(seed, t.mutability);
          },
          [&](const ManagedRefType& t) { HashId(seed, t.pointee); },
          [&](const UnmanagedRefType& t) { HashId(seed, t.pointee); },
          [&](const VectorType& t) { HashId(seed, t.element); },
          [&](const TupleType& t) { HashIds(seed, t.elements); },
          [&](const ObservableType& t) { HashId(seed, t.value); },
          [&](const ResolvedType& t) { HashId(seed, t.value); },
          [&](const DriverType& t) { HashId(seed, t.value); },
          [&](const OpenWriteType& t) { HashId(seed, t.value); },
          [&](const DesignationType& t) { HashId(seed, t.value); },
          [&](const BitsReferenceType& t) { HashId(seed, t.bits); },
          [&](const ObjectWriteType& t) { HashId(seed, t.object); },
          [&](const RecordType& t) { HashField(seed, t.declaration.value); },
          [&](const ClosureType& t) { HashField(seed, t.closure_id.value); },
          [&](const ErasedCallableType& t) {
            HashEnum(seed, t.kind);
            HashIds(seed, t.params);
            HashId(seed, t.result);
          }});
  return seed;
}

auto Type::IsIntegral() const -> bool {
  return Is<IntegralType>() || Is<EnumType>();
}

auto Type::Integral() const -> const IntegralType& {
  if (const auto* integral = As<IntegralType>()) {
    return *integral;
  }
  if (const auto* enumeration = As<EnumType>()) {
    return enumeration->base;
  }
  throw InternalError("mir: type is not integral");
}

auto Type::IsDeclaredBy(support::LibraryDecl decl) const -> bool {
  const auto* library = As<LibraryType>();
  return library != nullptr && library->decl == decl;
}

auto Type::RuntimeClassOf() const -> std::optional<support::LibraryDecl> {
  const auto* library = As<LibraryType>();
  if (library == nullptr ||
      !support::DeclarationOf(library->decl).runtime_class.has_value()) {
    return std::nullopt;
  }
  return library->decl;
}

auto Type::IsCapabilityWrapper() const -> bool {
  return Is<ObservableType>() || IsVariableRef() || Is<ResolvedType>() ||
         Is<DriverType>();
}

auto Type::IsVariableRef() const -> bool {
  return Is<VariableRefType>() || Is<ReferenceWrapperType>();
}

auto Type::IsAddressOnly() const -> bool {
  // One arm per MIR type and no catch-all, so a type added later fails to
  // compile here until it says which it is; answering that it has a value to
  // read would refuse a read of it wrongly rather than refuse it by name.
  return Visit(
      Overloaded{
          // Storage the runtime keeps, which is consumed where it lies.
          [](const ObservableType&) { return true; },
          [](const ResolvedType&) { return true; },
          [](const ObjectType&) { return true; },

          // A value, or something whose value is what it holds.
          [](const IntegralType&) { return false; },
          [](const EnumType&) { return false; },
          [](const UnpackedArrayType&) { return false; },
          [](const ErasedValueType&) { return false; },
          [](const MachineCStringType&) { return false; },
          [](const MachineBoolType&) { return false; },
          [](const MachineIntType&) { return false; },
          [](const MachineFloatType&) { return false; },
          [](const MachineArrayType&) { return false; },
          [](const MachineSpanType&) { return false; },
          [](const MachineFunctionType&) { return false; },
          [](const RealType&) { return false; },
          [](const VoidType&) { return false; },
          // Storage the library keeps, or a value, as its declaration says.
          [](const LibraryType& t) { return support::IsAddressOnly(t.decl); },
          [](const CoroutineType&) { return false; },
          [](const VariableRefType&) { return false; },
          [](const ReferenceWrapperType&) { return false; },
          [](const PointerType&) { return false; },
          [](const ReferenceType&) { return false; },
          [](const ManagedRefType&) { return false; },
          [](const UnmanagedRefType&) { return false; },
          [](const VectorType&) { return false; },
          [](const TupleType&) { return false; },
          [](const DriverType&) { return false; },
          [](const OpenWriteType&) { return false; },
          [](const DesignationType&) { return false; },
          [](const BitsReferenceType&) { return false; },
          [](const ObjectWriteType&) { return false; },
          [](const RecordType&) { return false; },
          [](const ClosureType&) { return false; },
          [](const ErasedCallableType&) { return false; }});
}

auto Type::OwnsPointee() const -> bool {
  const auto* pointer = As<PointerType>();
  if (pointer == nullptr) {
    return false;
  }
  switch (pointer->ownership) {
    case PointerOwnership::kUnique:
    case PointerOwnership::kShared:
      return true;
    case PointerOwnership::kBorrowed:
      return false;
  }
  std::unreachable();
}

auto Type::WrappedValueType() const -> TypeId {
  if (const auto* observable = As<ObservableType>()) {
    return observable->value;
  }
  if (const auto* reference = As<VariableRefType>()) {
    return reference->pointee;
  }
  if (const auto* wrapper = As<ReferenceWrapperType>()) {
    return wrapper->pointee;
  }
  if (const auto* resolved = As<ResolvedType>()) {
    return resolved->value;
  }
  if (const auto* driver = As<DriverType>()) {
    return driver->value;
  }
  throw InternalError("mir: type is not a capability wrapper");
}

auto Type::IsRuntimeStoredValue() const -> bool {
  // One arm per MIR type and no catch-all, so a type added later fails to
  // compile here until it says which side it is on -- rather than silently
  // answering that the holder has the whole of it, which is a wrong answer and
  // not a refusal: the holder goes on holding a handle after the storage behind
  // it is gone.
  return Visit(
      Overloaded{
          // An integral value is the bits its type declares, an enumeration's
          // is its base's, and a real is the IEEE value of its width (LRM
          // 6.12), so whoever holds one holds it entire.
          [](const IntegralType&) { return false; },
          [](const EnumType&) { return false; },
          [](const RealType&) { return false; },

          // A value built of other values -- a fixed-size sequence, a product,
          // the members a union overlays.
          [](const UnpackedArrayType&) { return true; },
          [](const TupleType&) { return true; },
          [](const RecordType&) { return true; },
          [](const ErasedValueType&) { return false; },
          // A type the library declares says whether a value of it lies in
          // storage of its own its holder keeps a claim on.
          [](const LibraryType& t) {
            return support::IsRuntimeStoredValue(t.decl);
          },

          // A class handle is one of these and a reference owning nothing is
          // not, which LRM 8.4 Table 8-1 states directly: an unreferenced
          // object is collected where the handle is an object handle, and is
          // not where it is a chandle or a C pointer. A handle's value is
          // therefore which object it names together with a claim on that
          // object's life, which no address carries on its own; a reference
          // owning nothing is the address, so whoever holds the address holds
          // the whole of it (LRM 6.14).
          [](const ManagedRefType&) { return true; },
          [](const UnmanagedRefType&) { return false; },

          // A machine quantity, which whoever holds it holds entire.
          [](const MachineCStringType&) { return false; },
          [](const MachineBoolType&) { return false; },
          [](const MachineIntType&) { return false; },
          [](const MachineFloatType&) { return false; },
          [](const MachineArrayType&) { return false; },
          [](const MachineSpanType&) { return false; },
          [](const MachineFunctionType&) { return false; },

          // An address whose value is the address: what it names lives for its
          // own reasons and ends for them, so nothing about holding the address
          // keeps it or loses it.
          [](const VariableRefType&) { return false; },
          [](const ReferenceWrapperType&) { return false; },
          [](const PointerType&) { return false; },
          [](const ReferenceType&) { return false; },
          [](const VectorType&) { return false; },
          [](const DriverType&) { return false; },
          [](const CoroutineType&) { return false; },

          // An object is reached by its address, and so is a closure whose
          // type is not known.
          [](const ObjectType&) { return false; },
          [](const ErasedCallableType&) { return false; },
          // A closure never moves once made: it is made where the local
          // holding it lies, and so is never a variable whose storage is
          // filled by handing it a value made elsewhere.
          [](const ClosureType&) { return false; },

          // Storage, consumed where it lives rather than read out as a value,
          // so no holder ever has a copy of one to lose.
          [](const ObservableType&) { return false; },
          [](const ResolvedType&) { return false; },
          [](const OpenWriteType&) { return false; },
          [](const DesignationType&) { return false; },
          [](const BitsReferenceType&) { return false; },
          [](const ObjectWriteType&) { return false; },

          // `void` is the absence of a value, which nothing holds.
          [](const VoidType&) { return false; }});
}

auto Type::ValueMovability() const -> support::Movability {
  // One arm per MIR type and no catch-all, so a type added later fails to
  // compile here until it says how a temporary can stand in its place.
  return Visit(
      Overloaded{
          // A value of the program, whoever holds it holding the whole of it.
          [](const IntegralType&) { return support::Movability::kCopyable; },
          [](const EnumType&) { return support::Movability::kCopyable; },
          [](const UnpackedArrayType&) {
            return support::Movability::kCopyable;
          },
          [](const TupleType&) { return support::Movability::kCopyable; },
          [](const RecordType&) { return support::Movability::kCopyable; },
          [](const RealType&) { return support::Movability::kCopyable; },
          [](const ManagedRefType&) { return support::Movability::kCopyable; },
          [](const UnmanagedRefType&) {
            return support::Movability::kCopyable;
          },
          [](const MachineBoolType&) { return support::Movability::kCopyable; },
          [](const MachineIntType&) { return support::Movability::kCopyable; },
          [](const MachineFloatType&) {
            return support::Movability::kCopyable;
          },
          [](const MachineFunctionType&) {
            return support::Movability::kCopyable;
          },
          // Where entries kept elsewhere begin and how many there are, which
          // owns none of them.
          [](const MachineSpanType&) { return support::Movability::kCopyable; },
          // The address of a string's characters, which lasts as long as the
          // string does.
          [](const MachineCStringType&) {
            return support::Movability::kCopyable;
          },
          // A type the library declares states it.
          [](const LibraryType& t) {
            return support::DeclarationOf(t.decl).movability;
          },

          // A reference is the address of what it refers to, whether a
          // frame's or one the library reads and writes through, and so is a
          // pointer that owns nothing or shares what it points to. One that
          // owns is moved, since a copy would be a second owner.
          [](const ReferenceType&) { return support::Movability::kCopyable; },
          [](const VariableRefType&) { return support::Movability::kCopyable; },
          [](const PointerType& t) {
            switch (t.ownership) {
              case PointerOwnership::kShared:
              case PointerOwnership::kBorrowed:
                return support::Movability::kCopyable;
              case PointerOwnership::kUnique:
                return support::Movability::kMovable;
            }
            throw InternalError("mir: unknown pointer ownership");
          },

          // A value made only to be handed on, which nothing holds a second
          // copy of.
          [](const ErasedValueType&) { return support::Movability::kMovable; },
          [](const ErasedCallableType&) {
            return support::Movability::kMovable;
          },

          // A value no temporary stands in for: one whose identity is where it
          // was made -- a closure, whose captures' references were handed out
          // there, an object -- one standing for storage elsewhere, and a
          // write in progress or what designates part of one. A sequence of
          // machine values is built where it is kept.
          [](const MachineArrayType&) {
            return support::Movability::kImmovable;
          },
          [](const ReferenceWrapperType&) {
            return support::Movability::kImmovable;
          },
          [](const VectorType&) { return support::Movability::kImmovable; },
          [](const DriverType&) { return support::Movability::kImmovable; },
          [](const CoroutineType&) { return support::Movability::kImmovable; },
          [](const ObjectType&) { return support::Movability::kImmovable; },
          [](const ClosureType&) { return support::Movability::kImmovable; },
          [](const ObservableType&) { return support::Movability::kImmovable; },
          [](const ResolvedType&) { return support::Movability::kImmovable; },
          [](const OpenWriteType&) { return support::Movability::kImmovable; },
          [](const DesignationType&) {
            return support::Movability::kImmovable;
          },
          [](const BitsReferenceType&) {
            return support::Movability::kImmovable;
          },
          [](const ObjectWriteType&) {
            return support::Movability::kImmovable;
          },
          [](const VoidType&) { return support::Movability::kImmovable; }});
}

auto Type::BitsAreWrittenInPlace() const -> bool {
  return Is<IntegralType>();
}

auto Type::ContainerElementType() const -> std::optional<TypeId> {
  using Element = std::optional<TypeId>;
  return Visit(
      Overloaded{
          // A sequence of values whose count is the type's, and a container
          // the library declares, which says which of its arguments is its
          // element type.
          [](const UnpackedArrayType& t) -> Element { return t.element_type; },
          [](const LibraryType& t) -> Element {
            return t.FindTypeArg(support::LibraryParam::kElement);
          },

          // These hold a sequence of values too and are still not containers: a
          // lowering builds them to carry something, where a container is a
          // type a declaration named.
          [](const MachineArrayType&) -> Element { return std::nullopt; },
          [](const MachineSpanType&) -> Element { return std::nullopt; },
          [](const VectorType&) -> Element { return std::nullopt; },

          // One vector of bits, under a set of names or not: what looks like
          // an element is bits of that vector rather than a value held beside
          // the others.
          [](const IntegralType&) -> Element { return std::nullopt; },
          [](const EnumType&) -> Element { return std::nullopt; },

          // Parts side by side, or members overlaid, but never a sequence of
          // one type.
          [](const TupleType&) -> Element { return std::nullopt; },
          [](const RecordType&) -> Element { return std::nullopt; },

          // A single quantity or a single token.
          [](const ErasedValueType&) -> Element { return std::nullopt; },
          [](const MachineCStringType&) -> Element { return std::nullopt; },
          [](const MachineBoolType&) -> Element { return std::nullopt; },
          [](const MachineIntType&) -> Element { return std::nullopt; },
          [](const MachineFloatType&) -> Element { return std::nullopt; },
          [](const RealType&) -> Element { return std::nullopt; },
          [](const VoidType&) -> Element { return std::nullopt; },

          // A cell keeps one value, whatever it publishes about changes to it;
          // the sequence, where there is one, belongs to the value it keeps.
          [](const ObservableType&) -> Element { return std::nullopt; },
          [](const ResolvedType&) -> Element { return std::nullopt; },
          [](const DriverType&) -> Element { return std::nullopt; },
          [](const OpenWriteType&) -> Element { return std::nullopt; },
          [](const DesignationType&) -> Element { return std::nullopt; },
          [](const BitsReferenceType&) -> Element { return std::nullopt; },
          [](const ObjectWriteType&) -> Element { return std::nullopt; },

          // These refer to a value living elsewhere rather than holding one,
          // so a container reached through one is reached by dereferencing it
          // first.
          [](const VariableRefType&) -> Element { return std::nullopt; },
          [](const ReferenceWrapperType&) -> Element { return std::nullopt; },
          [](const PointerType&) -> Element { return std::nullopt; },
          [](const ReferenceType&) -> Element { return std::nullopt; },
          [](const ManagedRefType&) -> Element { return std::nullopt; },
          [](const UnmanagedRefType&) -> Element { return std::nullopt; },
          [](const CoroutineType&) -> Element { return std::nullopt; },
          [](const MachineFunctionType&) -> Element { return std::nullopt; },

          // An object and a closure are declarations, and a declaration is not
          // a sequence of anything.
          [](const ObjectType&) -> Element { return std::nullopt; },
          [](const ClosureType&) -> Element { return std::nullopt; },
          [](const ErasedCallableType&) -> Element { return std::nullopt; }});
}

auto Type::IsLibraryContainer() const -> bool {
  const auto* library = As<LibraryType>();
  return library != nullptr &&
         library->FindTypeArg(support::LibraryParam::kElement).has_value();
}

auto Type::IsLibrarySequence() const -> bool {
  return IsLibraryContainer() && !IndexType().has_value();
}

auto Type::IndexType() const -> std::optional<TypeId> {
  const auto* library = As<LibraryType>();
  if (library == nullptr) {
    return std::nullopt;
  }
  return library->FindTypeArg(support::LibraryParam::kIndex);
}

auto IsDataType(const TypePool& types, TypeId type) -> bool {
  // One arm per MIR type and no catch-all: a type added later fails to compile
  // here until it is classified, rather than silently counting as no data --
  // which would leave a variable of a new value type unobserved, its writes
  // firing no update event (LRM 9.4.2), a bug no test would obviously catch.
  return types.Get(type).Visit(
      Overloaded{
          // What a variable is declared to hold (LRM 6.2, 7).
          [](const IntegralType&) { return true; },
          [](const EnumType&) { return true; },
          [](const UnpackedArrayType&) { return true; },
          [](const RealType&) { return true; },
          [](const RecordType&) { return true; },
          [&](const TupleType& t) {
            return std::ranges::all_of(t.elements, [&](TypeId component) {
              return IsDataType(types, component);
            });
          },
          // A variable naming an object or holding a pointer is value storage
          // too: LRM 9.4.2 makes a write to an object handle or a chandle an
          // event when it is not equal to what was there, and the clause's own
          // example puts a wait on a handle beside a wait on a property of the
          // object to say they are two waits. A virtual interface is the same
          // (LRM 25.9).
          [](const ManagedRefType&) { return true; },
          [](const UnmanagedRefType&) { return true; },

          [](const ErasedValueType&) { return false; },
          // A machine primitive: a plain boolean, integer, float, C string,
          // array, view of an array or code address.
          [](const MachineCStringType&) { return false; },
          [](const MachineBoolType&) { return false; },
          [](const MachineIntType&) { return false; },
          [](const MachineFloatType&) { return false; },
          [](const MachineArrayType&) { return false; },
          [](const MachineSpanType&) { return false; },
          [](const MachineFunctionType&) { return false; },
          [](const VoidType&) { return false; },
          // An object: a class instance or an instantiated child.
          [](const ObjectType&) { return false; },
          // A type the library declares says whether a declaration names it.
          [](const LibraryType& t) {
            return support::DeclarationOf(t.decl).is_a_data_type;
          },
          [](const CoroutineType&) { return false; },
          // A handle onto storage of another lifetime.
          [](const VariableRefType&) { return false; },
          [](const ReferenceWrapperType&) { return false; },
          [](const PointerType&) { return false; },
          [](const ReferenceType&) { return false; },
          [](const VectorType&) { return false; },
          // The observable, net-cell and net-driver storage itself, and a write
          // open on one of them.
          [](const ObservableType&) { return false; },
          [](const ResolvedType&) { return false; },
          [](const DriverType&) { return false; },
          [](const OpenWriteType&) { return false; },
          [](const DesignationType&) { return false; },
          [](const BitsReferenceType&) { return false; },
          [](const ObjectWriteType&) { return false; },
          // Code: a closure, and one of no known type.
          [](const ClosureType&) { return false; },
          [](const ErasedCallableType&) { return false; }});
}

auto OperationsStatedPerUnit(const Type& type) -> std::optional<StatedPerUnit> {
  using Stated = std::optional<StatedPerUnit>;
  const auto by_a_declaration_or_none = [](const auto&) -> Stated {
    return std::nullopt;
  };
  return type.Visit(
      Overloaded{
          [](const UnpackedArrayType&) -> Stated {
            return StatedPerUnit::kFixedArray;
          },
          [](const TupleType&) -> Stated { return StatedPerUnit::kTuple; },
          [&](const IntegralType& t) { return by_a_declaration_or_none(t); },
          [&](const EnumType& t) { return by_a_declaration_or_none(t); },
          [&](const ErasedValueType& t) { return by_a_declaration_or_none(t); },
          [&](const MachineCStringType& t) {
            return by_a_declaration_or_none(t);
          },
          [&](const MachineBoolType& t) { return by_a_declaration_or_none(t); },
          [&](const MachineIntType& t) { return by_a_declaration_or_none(t); },
          [&](const MachineFloatType& t) {
            return by_a_declaration_or_none(t);
          },
          [&](const MachineArrayType& t) {
            return by_a_declaration_or_none(t);
          },
          [&](const MachineSpanType& t) { return by_a_declaration_or_none(t); },
          [&](const MachineFunctionType& t) {
            return by_a_declaration_or_none(t);
          },
          [&](const RealType& t) { return by_a_declaration_or_none(t); },
          [&](const VoidType& t) { return by_a_declaration_or_none(t); },
          [&](const ObjectType& t) { return by_a_declaration_or_none(t); },
          [&](const LibraryType& t) { return by_a_declaration_or_none(t); },
          [&](const CoroutineType& t) { return by_a_declaration_or_none(t); },
          [&](const VariableRefType& t) { return by_a_declaration_or_none(t); },
          [&](const ReferenceWrapperType& t) {
            return by_a_declaration_or_none(t);
          },
          [&](const PointerType& t) { return by_a_declaration_or_none(t); },
          [&](const ReferenceType& t) { return by_a_declaration_or_none(t); },
          [&](const ManagedRefType& t) { return by_a_declaration_or_none(t); },
          [&](const UnmanagedRefType& t) {
            return by_a_declaration_or_none(t);
          },
          [&](const VectorType& t) { return by_a_declaration_or_none(t); },
          [&](const ObservableType& t) { return by_a_declaration_or_none(t); },
          [&](const ResolvedType& t) { return by_a_declaration_or_none(t); },
          [&](const DriverType& t) { return by_a_declaration_or_none(t); },
          [&](const OpenWriteType& t) { return by_a_declaration_or_none(t); },
          [&](const DesignationType& t) { return by_a_declaration_or_none(t); },
          [&](const BitsReferenceType& t) {
            return by_a_declaration_or_none(t);
          },
          [&](const ObjectWriteType& t) { return by_a_declaration_or_none(t); },
          [&](const RecordType& t) { return by_a_declaration_or_none(t); },
          [&](const ClosureType& t) { return by_a_declaration_or_none(t); },
          [&](const ErasedCallableType& t) {
            return by_a_declaration_or_none(t);
          }});
}

}  // namespace lyra::mir
