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
#include "lyra/lir/record_id.hpp"
#include "lyra/lir/type_id.hpp"
#include "lyra/support/runtime_class.hpp"
#include "lyra/support/runtime_object.hpp"

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
  // A run of bits whose length is the value's.
  kBitVec,
  kEnumeration,
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
  kWatchedPlace,
  kTrigger,
  kObservation,
  kReadReport,
  kWait,
  kLeasedReference,
  kObjectWrite,
  kCancellationTarget,
  kTimer,
  kDeferredEffects,
  kViolationReportKeeper,
  kControlEffect,
  // The definition every object of one class carries. A body names it to ask
  // the class a question, and forwards its address without reading inside it,
  // so what reaches here is the kind and never the runtime struct's layout.
  kObjectDefinition,
  // The structures a definition is made of, which only a constant names.
  kScopeInfo,
  kScopeCallable,
  kClassObjectInfo,
  kInterfaceRow,
  // What a constructor is handed, what one of its entries is, and the entries
  // of one generate. A body hands a table's address to the library and never
  // reads inside it.
  kConstructionTable,
  kInstantiation,
  kGeneratePlaces,
  // What a class states of each thing a value dump may record and of each
  // named block, task or function around them, which only a constant names
  // and a body forwards the address of.
  kDumpEntry,
  kDumpBlock,
};

// An integral value's type: how many bits it has, whether they are read as
// signed, and how many values a bit can take (LRM 6.11).
struct IntegralType {
  std::uint64_t bit_width;
  Signedness signedness;
  IntegralStateKind state_kind;

  auto operator==(const IntegralType&) const -> bool = default;
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

// A value of any type, kept with that type and the identity it is tested by,
// which the library holds as an object of its own.
struct AnyType {
  auto operator==(const AnyType&) const -> bool = default;
};

// A reference to a value of a type known only where the program runs: where a
// value of its identity is received and what carries it on, held as an object
// of the library's own, as a reference to an unsized value is a pointer and
// its metadata in rustc's layout.
struct AnyReferenceType {
  auto operator==(const AnyReferenceType&) const -> bool = default;
};

// A borrowed, NUL-terminated machine string (C `const char*`): raw character
// storage the value does not own, distinct from the owning `StringType`.
struct MachineCStringType {
  auto operator==(const MachineCStringType&) const -> bool = default;
};

// A primitive machine boolean (C `_Bool`): the two-valued scalar a predicate
// yields, distinct from a one-bit integer and from a one-bit `IntegralType`.
struct MachineBoolType {
  auto operator==(const MachineBoolType&) const -> bool = default;
};

// The widths a machine scalar comes in. Machine data is what crosses to a
// target on the target's own terms -- a foreign call's by-value argument, a
// table the runtime reads as raw storage -- so the widths are the ones a C
// type system names outright.
enum class MachineIntWidth : std::uint8_t { k8, k16, k32, k64 };

enum class MachineFloatWidth : std::uint8_t { k32, k64 };

// The width as a number, for a target that parameterizes its integer type by a
// bit count rather than naming a type per width.
[[nodiscard]] auto BitsOf(MachineIntWidth width) -> std::uint32_t;

// A primitive machine integer (C `intN_t`): a fixed-width 2-state scalar,
// distinct from the simulation value an `IntegralType` is.
struct MachineIntType {
  MachineIntWidth width;
  Signedness signedness;

  auto operator==(const MachineIntType&) const -> bool = default;
};

// A primitive machine float (C `float` / `double`), distinct from `RealType`,
// which is a simulation value reached through a value wrapper.
struct MachineFloatType {
  MachineFloatWidth width;

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

// A pointer to code (C `R (*)(A, B)`): the machine family's callable axis,
// beside the machine scalars and the aggregate of them. Distinct from a
// function of this unit, which a call names outright -- this is the type an
// address has when code is a value, and the signature it carries is what a
// call made through it is compiled against.
struct MachineFunctionType {
  std::vector<TypeId> params;
  TypeId result;

  auto operator==(const MachineFunctionType&) const -> bool = default;
};

struct RealType {
  auto operator==(const RealType&) const -> bool = default;
};
struct ShortRealType {
  auto operator==(const ShortRealType&) const -> bool = default;
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

// A class the runtime library defines. It belongs to no compilation unit, so
// which class it is is the whole identity.
struct RuntimeClassType {
  support::RuntimeClass which;

  auto operator==(const RuntimeClassType&) const -> bool = default;
};

// A callable value: an instance of one closure declaration, holding that
// declaration's captures. A capture is reached by a member projection over the
// invoke's receiver, the way an object's member is reached over its own.
struct ClosureType {
  ClosureId closure_id;

  auto operator==(const ClosureType&) const -> bool = default;
};

// A closure whose type is not known where it is called, as the library holds
// one: always behind a box or a borrow, called through the definition its
// value opens with. Its signature is machine types alone.
struct ErasedCallableType {
  std::vector<TypeId> params;
  TypeId result;

  auto operator==(const ErasedCallableType&) const -> bool = default;
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

struct VariableRefType {
  TypeId pointee;
  Mutability mutability;

  auto operator==(const VariableRefType&) const -> bool = default;
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

// A weak reference to an object a managed reference names (LRM 8.30), which
// does not keep the object: the library object holding one.
struct WeakManagedRefType {
  TypeId pointee;

  auto operator==(const WeakManagedRefType&) const -> bool = default;
};

struct VectorType {
  TypeId element;

  auto operator==(const VectorType&) const -> bool = default;
};

// A heterogeneous fixed product that is its components and nothing more, each
// reached by its position: two with the same components are the same type.
struct TupleType {
  std::vector<TypeId> elements;

  auto operator==(const TupleType&) const -> bool = default;
};

// A structure, a union or a tagged union named by its declaration, as clang's
// record type names its record declaration: two with the same members are
// still two types (LRM 6.22.1). Which aggregate it is and its members' types
// are the declaration's, as are the operations on a whole value of one; a
// member is reached by its position.
struct RecordType {
  // Where the declaration sits in the unit's record registry, whichever unit
  // defines it.
  RecordId declaration;

  auto operator==(const RecordType&) const -> bool = default;
};

// The sealed endpoint of a cross-instance reference -- a resolution node
// wrapping the referenced value type. The fold it resolves under (LRM 6.6) is
// installed at construction, so it is the net's state rather than its type.
struct ResolvedType {
  TypeId value;

  auto operator==(const ResolvedType&) const -> bool = default;
};

// The drive capability for a net: a handle to one of a net's contributions.
struct DriverType {
  TypeId value;

  auto operator==(const DriverType&) const -> bool = default;
};

struct ObservableType {
  TypeId value;

  auto operator==(const ObservableType&) const -> bool = default;
};

// Storage holding what the ticks of one clocking event settled for one
// expression, which is what a read of an earlier tick answers from. It names
// the values it keeps; how many it keeps is fixed by filling the storage rather
// than by building it, so it is not part of the type.
struct SampledHistoryType {
  TypeId value;

  auto operator==(const SampledHistoryType&) const -> bool = default;
};

// Storage holding what one concurrent assertion has in flight. It names no
// value, because what it holds is machine words that mean something only to
// the transition reading them: nothing of the design is in there.
struct EvaluationAttemptsType {
  auto operator==(const EvaluationAttemptsType&) const -> bool = default;
};

// Storage holding the blocks one loop generate built, each under the value its
// index stood at. It names no value, because what it holds is the blocks
// themselves, which a name reaching one asks it for.
struct LoopBlocksType {
  auto operator==(const LoopBlocksType&) const -> bool = default;
};

// A write in progress into the storage a capability wrapper stands for, whose
// contents are values of `value` (LRM 11.5.1). It is an object the writer holds
// and owes the end of, and it names no place itself: the whole of those
// contents is designated within it. Ending it -- with the full-expression, or
// on any way out of it -- is the end of the write, when the wrapper learns once
// what the write did (LRM 4.3).
struct OpenWriteType {
  TypeId value;

  auto operator==(const OpenWriteType&) const -> bool = default;
};

// A place holding a value of `value`, designated within a write in progress --
// the whole of what the write was opened on, or a part of it: the write and
// where the place lies. It borrows the write, so ending it does nothing; a
// further step is taken on it, and landing it is where the write lands.
struct DesignationType {
  TypeId value;

  auto operator==(const DesignationType&) const -> bool = default;
};

// The declaration of member-bearing storage a type names: a class, whichever
// unit defines it, or a closure a lowering introduces that no source
// declaration stands behind. Each is a position in its own registry of this
// unit, so a consumer reaching what the declaration holds asks which of the two
// it met rather than asking every type there is.
using TypeDeclaration = std::variant<ObjectType, ClosureType>;

// What a value is held as: an integral value as the bits its type declares,
// whose layout this layer does not state; any other value of the design as an
// object of its value domain, which is never a domain of an integral layout
// here; what the library builds for its own use as that object; a pointer
// owning what it points at as the address of that storage, which its end gives
// back, as Rust's `Box` is held; and a closure as the storage its declaration
// lays out, which the body its declaration states for it ends, as a C++
// lambda's object is held.
struct IntegralBits {};
struct OwnedStorageAddress {};
struct ClosureStorage {};
using Holding = std::variant<
    IntegralBits, support::ValueDomain, support::LibraryObject,
    OwnedStorageAddress, ClosureStorage>;

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
      IntegralType, UnpackedArrayType, DynamicArrayType, QueueType,
      AssociativeArrayType, WildcardIndexType, StringType, AnyType,
      AnyReferenceType, MachineCStringType, MachineBoolType, MachineIntType,
      MachineFloatType, MachineArrayType, MachineFunctionType, RealType,
      ShortRealType, ChandleType, VoidType, EmptyType, ObjectType,
      RuntimeClassType, ClosureType, ErasedCallableType, RuntimeEffectsType,
      FilesType, DiagnosticType, RuntimeLibraryType, CoroutineType,
      VariableRefType, PointerType, ManagedRefType, WeakManagedRefType,
      VectorType, TupleType, RecordType, ResolvedType, DriverType,
      ObservableType, SampledHistoryType, EvaluationAttemptsType,
      LoopBlocksType, OpenWriteType, DesignationType>;

 public:
  explicit Type(Data data) : data_(std::move(data)) {
  }

  // The name of this type's kind, for a consumer that must say which type it
  // met and could not handle. The kind is the whole answer: an element type or
  // a dimension would lengthen the name without changing what the reader does
  // next.
  [[nodiscard]] auto KindName() const -> std::string_view;

  // The declaration of member-bearing storage this type names -- a class, a
  // closure, another unit's class -- absent for every other type.
  // What that declaration holds -- its members, the class it extends, the names
  // it was emitted under -- is reached through the unit it belongs to, so this
  // answers which declaration it is and not what is in it.
  [[nodiscard]] auto Declaration() const -> std::optional<TypeDeclaration>;

  // The type this one refers to; absent when it refers to nothing. This is the
  // narrow relation of indirection -- storage that lives elsewhere -- which is
  // what an address-of yields.
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
  // and not about a capability that reaches one. A net's driver is a handle
  // naming one of a resolution node's slots, an ordinary first-class value its
  // holder owns, so a place holding one is loaded and stored like any other.
  // The cell is what is address-only, never the value or the capability
  // reaching it.
  [[nodiscard]] auto IsAddressOnly() const -> bool;

  // What a value of this type is held as, and nothing for a type whose value
  // is held as itself or is reached where it lives. An integral value is its
  // bits at its type, and how those are laid out is decided below this layer.
  // Any other value of the design is its domain's object; an object a call
  // builds for one use -- what a print is assembled from, what a wait
  // registers, the storage a closure captures into, a counted hold on a value
  // -- is the library's own. A pointer owning what it points at is the address
  // of that storage. Each of those names a logical shape only, whose size is a
  // physical fact decided below this layer too.
  [[nodiscard]] auto HeldAs() const -> std::optional<Holding>;

  // True for a value its holder owns and has to end. One of these ends where
  // the lowering that made it says, the way a C++ temporary ends with its
  // full-expression and a local with its block.
  [[nodiscard]] auto IsOwnedValue() const -> bool;

  // True for an owned value that has no copy: a pointer owning what it points
  // at, which two holders would each give back. Wherever one is handed on --
  // to a slot, to a caller, to a callee, as another type -- its end goes with
  // it, as Rust moves a value that is not `Copy`.
  [[nodiscard]] auto IsMoveOnly() const -> bool;

  // The type of the elements this one holds a sequence of, or nothing where it
  // holds none: the unpacked array (LRM 7.4), the dynamic array (7.5), the
  // associative array (7.8) and the queue (7.10). Narrower than holding values:
  // a machine array and a vector hold a sequence of them and are still not
  // containers, being shapes a lowering builds rather than types a declaration
  // named.
  [[nodiscard]] auto ContainerElementType() const -> std::optional<TypeId>;

  // How this type's sign bit is read as a machine integer, and nothing for a
  // type that is not one. A machine integer's operators are the target's own
  // instructions over one scalar, and the only thing an instruction still has
  // to be told is the signedness. A machine boolean is one such integer, one
  // bit wide and never negative, so it answers rather than standing outside.
  [[nodiscard]] auto MachineIntegerSignedness() const
      -> std::optional<Signedness>;

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
