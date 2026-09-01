#pragma once

#include <cstddef>
#include <cstdint>
#include <optional>
#include <string>
#include <variant>
#include <vector>

#include "lyra/base/internal_error.hpp"
#include "lyra/base/interner.hpp"
#include "lyra/lir/class_id.hpp"
#include "lyra/lir/closure_id.hpp"
#include "lyra/lir/external_unit_object_id.hpp"
#include "lyra/lir/type_id.hpp"

namespace lyra::lir {

// The LIR type graph. Each type is a LIR-owned identity translated from a
// generic-PL MIR type at MIR-to-LIR; it carries no reference back to MIR. LIR
// types continue MIR's type universe -- the ownership, wrapper, and object
// decisions MIR fixed are read here, not re-decided. They name the logical
// shape a value has during execution; physical size, alignment, and offsets are
// derived below LIR.
//
// Every type compares by its content, and that comparison is what the pool
// interns by: two that look the same are the same type, so comparing identities
// compares types. Every field a type carries is part of that content -- an
// object's class identity is what keeps two classes with identical members
// apart, and nothing here is carried for a reason other than what the type
// means.

// How many values a bit of an integral type can take (LRM 6.11.2). A source
// keyword chooses it -- `bit` two, `logic` four -- but the keyword does not
// reach this layer; what a consumer needs to know is the count.
enum class IntegralStateKind : std::uint8_t { kTwoState, kFourState };

enum class Signedness : std::uint8_t { kSigned, kUnsigned };

enum class PointerOwnership : std::uint8_t { kUnique, kShared, kBorrowed };

enum class Mutability : std::uint8_t { kMutable, kReadOnly };

enum class RuntimeLibraryKind : std::uint8_t {
  kPackedType,
  kPackedRange,
  kPrintItem,
  kPrintLiteralItem,
  kPrintValueItem,
  kFormatSpec,
  kFormatArg,
  kChannelCancellation,
  kTimeFormat,
  kHierarchySegment,
  kDpiBitBuffer,
  kDpiLogicBuffer,
  kDpiBitChunk,
  kDpiLogicChunk,
  kDpiOpenArray,
  kDpiOpenArrayHandle,
  kTrigger,
  kCancellationTarget,
  kControlEffect,
};

struct PackedRange {
  std::int64_t left;
  std::int64_t right;

  auto operator==(const PackedRange&) const -> bool = default;
};

struct PackedArrayType {
  IntegralStateKind state_kind;
  Signedness signedness;
  std::vector<PackedRange> dims;

  auto operator==(const PackedArrayType&) const -> bool = default;
};

struct EnumMember {
  std::string name;
  std::int64_t value;

  auto operator==(const EnumMember&) const -> bool = default;
};

struct EnumType {
  PackedArrayType base;
  std::vector<EnumMember> members;

  auto operator==(const EnumType&) const -> bool = default;
};

struct UnpackedArrayType {
  TypeId element_type;
  std::uint64_t size;

  auto operator==(const UnpackedArrayType&) const -> bool = default;
};

struct DynamicArrayType {
  TypeId element_type;

  auto operator==(const DynamicArrayType&) const -> bool = default;
};

struct QueueType {
  TypeId element_type;
  std::optional<std::uint64_t> max_bound;

  auto operator==(const QueueType&) const -> bool = default;
};

struct AssociativeArrayType {
  TypeId element_type;
  TypeId key_type;

  auto operator==(const AssociativeArrayType&) const -> bool = default;
};

struct WildcardIndexType {
  auto operator==(const WildcardIndexType&) const -> bool = default;
};

struct StringType {
  auto operator==(const StringType&) const -> bool = default;
};

// A borrowed, NUL-terminated machine string (C `const char*`): raw character
// storage the value does not own, distinct from the owning `StringType`.
struct MachineCStringType {
  auto operator==(const MachineCStringType&) const -> bool = default;
};

// A primitive machine boolean (C `_Bool`): the two-valued scalar a predicate
// yields, distinct from a one-bit integer and from the four-state
// `PackedArrayType`.
struct MachineBoolType {
  auto operator==(const MachineBoolType&) const -> bool = default;
};

struct MachineIntType {
  std::uint32_t bit_width;
  Signedness signedness;

  auto operator==(const MachineIntType&) const -> bool = default;
};

// A primitive machine float (C `float` / `double`), distinct from `RealType`,
// which is a simulation value reached through a value wrapper.
struct MachineFloatType {
  std::uint32_t bit_width;

  auto operator==(const MachineFloatType&) const -> bool = default;
};

// A fixed-size contiguous aggregate of plain machine data (C++ `std::array<T,
// N>`), distinct from `UnpackedArrayType`, which is a simulation value reached
// through a value wrapper. Its interior is not independently addressable; the
// storage is reached as a whole or through a pointer to its first element.
struct MachineArrayType {
  TypeId element;
  std::uint32_t size;

  auto operator==(const MachineArrayType&) const -> bool = default;
};

struct EventType {
  auto operator==(const EventType&) const -> bool = default;
};
struct RealType {
  auto operator==(const RealType&) const -> bool = default;
};
struct ShortRealType {
  auto operator==(const ShortRealType&) const -> bool = default;
};
struct RealTimeType {
  auto operator==(const RealTimeType&) const -> bool = default;
};
struct ChandleType {
  auto operator==(const ChandleType&) const -> bool = default;
};
struct VoidType {
  auto operator==(const VoidType&) const -> bool = default;
};

// A value type carrying no information: exactly one value, nothing to store.
// Distinct from `VoidType`, which marks the absence of a type. Reached through
// a tagged union's `void` member (LRM 7.3.2).
struct EmptyType {
  auto operator==(const EmptyType&) const -> bool = default;
};

struct ObjectType {
  ClassId class_id;

  auto operator==(const ObjectType&) const -> bool = default;
};

// An instance of the object another unit defines, named by this unit's record
// of what that unit published about it. Only the published prefix of its
// members is reachable through it.
struct ExternalUnitObjectType {
  ExternalUnitObjectId object;

  auto operator==(const ExternalUnitObjectType&) const -> bool = default;
};

// A class another compilation unit declares, named by that unit and the class's
// canonical name. A backend spells the pair in its own target language; nothing
// here composes one.
struct CrossUnitClassType {
  std::string unit_name;
  std::string class_name;

  auto operator==(const CrossUnitClassType&) const -> bool = default;
};

// A class the runtime library defines, named by the library symbol. It belongs
// to no compilation unit, so the symbol is the whole identity.
struct RuntimeClassType {
  std::string symbol;

  auto operator==(const RuntimeClassType&) const -> bool = default;
};

// A callable value: an instance of one closure declaration, holding that
// declaration's captures. A capture is reached by a member projection over the
// invoke's receiver, the way an object's member is reached over its own.
struct ClosureType {
  ClosureId closure_id;

  auto operator==(const ClosureType&) const -> bool = default;
};

struct RuntimeEffectsType {
  auto operator==(const RuntimeEffectsType&) const -> bool = default;
};
struct FilesType {
  auto operator==(const FilesType&) const -> bool = default;
};
struct DiagnosticType {
  auto operator==(const DiagnosticType&) const -> bool = default;
};

struct RuntimeLibraryType {
  RuntimeLibraryKind kind;

  auto operator==(const RuntimeLibraryType&) const -> bool = default;
};

struct CoroutineType {
  TypeId payload;

  auto operator==(const CoroutineType&) const -> bool = default;
};

struct RefType {
  TypeId pointee;
  Mutability mutability;

  auto operator==(const RefType&) const -> bool = default;
};

struct PointerType {
  TypeId pointee;
  PointerOwnership ownership;
  Mutability mutability;

  auto operator==(const PointerType&) const -> bool = default;
};

struct ManagedRefType {
  TypeId pointee;

  auto operator==(const ManagedRefType&) const -> bool = default;
};

struct VectorType {
  TypeId element;

  auto operator==(const VectorType&) const -> bool = default;
};

struct TupleType {
  std::vector<TypeId> elements;

  auto operator==(const TupleType&) const -> bool = default;
};

struct UnionType {
  std::vector<TypeId> elements;

  auto operator==(const UnionType&) const -> bool = default;
};

struct TaggedUnionType {
  std::vector<TypeId> elements;

  auto operator==(const TaggedUnionType&) const -> bool = default;
};

// How a net folds its drivers' contributions into its value (LRM 6.6), carried
// down from the net's declared net type.
enum class NetResolution : std::uint8_t { kTriState };

// The sealed endpoint of a cross-instance reference -- a resolution node
// wrapping the referenced value type.
struct ResolvedType {
  TypeId value;
  NetResolution resolution;

  auto operator==(const ResolvedType&) const -> bool = default;
};

// The drive capability for a net: a handle to one of a net's contributions,
// folding under the same resolution its net does.
struct DriverType {
  TypeId value;
  NetResolution resolution;

  auto operator==(const DriverType&) const -> bool = default;
};

struct ObservableType {
  TypeId value;

  auto operator==(const ObservableType&) const -> bool = default;
};

// A type one LIR compilation unit names, and the vocabulary for asking what it
// is. The alternatives are a closed set, consumed by visiting them: a visitor
// that names each one rather than defaulting is what makes an alternative added
// here fail to compile until every consumer says what the new one means.
//
// A question belongs to the type when its answer spans several alternatives or
// states a rule over them. Testing for one alternative, or reaching into it,
// stays at the call site; a question about how a later stage will treat a type
// belongs to that stage.
class Type {
 private:
  using Data = std::variant<
      PackedArrayType, EnumType, UnpackedArrayType, DynamicArrayType, QueueType,
      AssociativeArrayType, WildcardIndexType, StringType, MachineCStringType,
      MachineBoolType, MachineIntType, MachineFloatType, MachineArrayType,
      EventType, RealType, ShortRealType, RealTimeType, ChandleType, VoidType,
      EmptyType, ObjectType, ExternalUnitObjectType, CrossUnitClassType,
      RuntimeClassType, ClosureType, RuntimeEffectsType, FilesType,
      DiagnosticType, RuntimeLibraryType, CoroutineType, RefType, PointerType,
      ManagedRefType, VectorType, TupleType, UnionType, TaggedUnionType,
      ResolvedType, DriverType, ObservableType>;

 public:
  explicit Type(Data data) : data_(std::move(data)) {
  }

  // The name of this type's kind, for a consumer that must say which type it
  // met and could not handle. The kind is the whole answer: an element type or
  // a dimension would lengthen the name without changing what the reader does
  // next.
  [[nodiscard]] auto KindName() const -> std::string_view;

  // The type this one refers to; absent when it refers to nothing. This is the
  // narrow relation of indirection -- storage that lives elsewhere -- which is
  // what an address-of yields and a pointer cast retypes.
  [[nodiscard]] auto Pointee() const -> std::optional<TypeId>;

  // The type of the storage a dereference reaches. That is a reference's
  // referent, and also what a capability wrapper represents -- a wrapper is not
  // an indirection to storage elsewhere, it is storage whose contents are the
  // value. Absent when the type stands for no storage, which is what makes a
  // dereference of it invalid.
  [[nodiscard]] auto DerefTarget() const -> std::optional<TypeId>;

  // A type whose storage object has no first-class value in LIR: operations on
  // it consume its address. A storage cell (an observable variable, a net
  // resolution node) and every object-tree node (a class object, a scope, an
  // instance) are such a type -- there is nothing to read out of the storage,
  // write into it, or hand to a callee except where it lives.
  //
  // This is about the storage object, not about how a value is represented,
  // and not about a capability that reaches one. A packed value is a runtime
  // object reached through an opaque handle, and a net's driver is a handle
  // naming one of a resolution node's slots; either handle is an ordinary
  // first-class value its holder owns, so a place holding one is loaded and
  // stored like any other. The cell is what is address-only, never the value
  // or the capability reaching it.
  [[nodiscard]] auto IsAddressOnly() const -> bool;

  // True for any type whose value-level shape is a single packed vector: a
  // packed array, or an enumeration through its base. This is the precondition
  // of asking for a packed shape, so a consumer that does not already know it
  // asks here rather than listing the integral types itself.
  [[nodiscard]] auto IsIntegralPacked() const -> bool;

  // The packed shape an integral type's value is structured by: a packed array
  // is its own shape, an enumeration is represented by its base's. Every
  // consumer that must know how an integral value's bits are grouped asks
  // this, so the answer is given once rather than re-derived wherever it is
  // needed. A type that is not integral has no such shape and is a caller
  // error, never a width guess.
  [[nodiscard]] auto PackedShape() const -> const PackedArrayType&;

  template <typename T>
  [[nodiscard]] auto Is() const -> bool {
    return std::holds_alternative<T>(data_);
  }

  // Null where the type is a different alternative, for a caller whose next
  // step depends on which one it met.
  template <typename T>
  [[nodiscard]] auto As() const -> const T* {
    return std::get_if<T>(&data_);
  }

  // For a caller that has already established which alternative this is, so
  // meeting another one is that caller's own invariant broken.
  template <typename T>
  [[nodiscard]] auto Get() const -> const T& {
    const T* arm = std::get_if<T>(&data_);
    if (arm == nullptr) {
      throw InternalError(
          "lir::Type::Get: type is not the alternative asked for");
    }
    return *arm;
  }

  template <typename Visitor>
  auto Visit(Visitor&& visitor) const -> decltype(auto) {
    return std::visit(std::forward<Visitor>(visitor), data_);
  }

  auto operator==(const Type&) const -> bool = default;

  // How the pool spreads the types it holds. It reads the type's alternative
  // and the identities that alternative names rather than walking a member
  // list, because equality decides the answer and a hash only has to separate
  // the types one unit actually holds.
  struct Hash {
    auto operator()(const Type& type) const -> std::size_t;
  };

 private:
  Data data_;
};

// The types one compilation unit names at this layer. A LIR type is structural
// -- two that look the same are the same type -- so its identity is its
// content, and a `TypeId` comparison is a type comparison. Consumers rely on
// that, from a verifier checking a store against its place to a code generator
// keying a target type.
using TypePool = base::Interner<Type, TypeId, Type::Hash>;

}  // namespace lyra::lir
