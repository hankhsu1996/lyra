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
#include "lyra/support/float_width.hpp"
#include "lyra/support/library_decl.hpp"
#include "lyra/support/library_type.hpp"
#include "lyra/support/runtime_object.hpp"
#include "lyra/value/scalar.hpp"

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

// An integral value's type: how many bits it has, whether they are read as
// signed, and how many values a bit can take (LRM 6.11).
struct IntegralType {
  std::uint64_t bit_width;
  Signedness signedness;
  IntegralStateKind state_kind;

  auto operator==(const IntegralType&) const -> bool = default;
};

// The states a bit of a type holds, as the value library names them.
[[nodiscard]] auto StateDomainOf(IntegralStateKind state) -> value::StateDomain;

struct UnpackedArrayType {
  TypeId element_type;
  std::uint64_t size;

  auto operator==(const UnpackedArrayType&) const -> bool = default;
};

// A reference to a value of a type known only where the program runs: where a
// value of its identity is received and what carries it on, held as an object
// of the library's own, as a reference to an unsized value is a pointer and
// its metadata in rustc's layout.
struct AnyReferenceType {
  auto operator==(const AnyReferenceType&) const -> bool = default;
};

// A borrowed, NUL-terminated machine string (C `const char*`): raw character
// storage the value does not own, distinct from the owning string the library
// declares.
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
// which is a value of the design a variable's cell holds and the library
// prints, though both are laid out as the same IEEE value.
struct MachineFloatType {
  support::FloatWidth width;

  auto operator==(const MachineFloatType&) const -> bool = default;
};

// A fixed-size contiguous aggregate of plain machine data (C++ `std::array<T,
// N>`), distinct from `UnpackedArrayType`, which is a value of the design. Its
// interior is not independently addressable; the storage is reached as a whole
// or through a pointer to its first element.
struct MachineArrayType {
  TypeId element;
  std::uint32_t size;

  auto operator==(const MachineArrayType&) const -> bool = default;
};

// A read-only view of a run of plain machine data something else owns: where
// the first entry lies and how many there are, the pair a C program holds a
// table as.
struct MachineSpanType {
  TypeId element;

  auto operator==(const MachineSpanType&) const -> bool = default;
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

// An SV floating-point value: the IEEE 754 value of its width.
struct RealType {
  support::FloatWidth width;

  auto operator==(const RealType&) const -> bool = default;
};
struct VoidType {
  auto operator==(const VoidType&) const -> bool = default;
};

struct ObjectType {
  ClassId class_id;

  auto operator==(const ObjectType&) const -> bool = default;
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

// A type the runtime library declares, named by its declaration and its
// arguments as MIR names it; what any consumer asks of one it reads off the
// declaration.
using LibraryType = support::LibraryType<TypeId>;

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

// A reference that owns nothing (a chandle, LRM 6.14; a virtual interface, LRM
// 25.9): one address, or null, held as a value of the design is.
struct UnmanagedRefType {
  TypeId pointee;

  auto operator==(const UnmanagedRefType&) const -> bool = default;
};

struct VectorType {
  TypeId element;

  auto operator==(const VectorType&) const -> bool = default;
};

// A heterogeneous fixed product that is its components and nothing more, each
// reached by its position: two with the same components are the same type. The
// tuple of no components is laid out as nothing at all, and is what a tagged
// union's `void` member holds (LRM 7.3.2).
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

// A write in progress into the properties of an object (LRM 8.4), which the
// writer holds for as long as it writes there and which tells the object it
// was written as it ends. It is one object whatever the object's class, since
// each access through it states its own member's class.
struct ObjectWriteType {
  auto operator==(const ObjectWriteType&) const -> bool = default;
};

// The declaration of member-bearing storage a type names: a class, whichever
// unit defines it, or a closure a lowering introduces that no source
// declaration stands behind. Each is a position in its own registry of this
// unit, so a consumer reaching what the declaration holds asks which of the two
// it met rather than asking every type there is.
using TypeDeclaration = std::variant<ObjectType, ClosureType>;

// What a value is held as: a scalar the IR lays out -- an integral value of
// its width and states, a real of its width, a reference owning nothing -- as
// its own bytes, which that scalar lays out, as rustc's layout calls a value
// one machine scalar; a value laid out by its type -- a tuple, a structure, a
// union of either kind, a fixed-size unpacked array (LRM 7.4.2) -- as the
// bytes that type places its parts in, stepped part by part by code compiled
// for the type; an object of the library's, built and ended by its entries --
// a class handle or a value of a type the library declares as an object of its
// value domain, an object a declaration states as its own as that object, and
// what the library builds for a type the IR owns as that object; a pointer
// owning what it points at as the address of that storage, which its end
// gives back, as Rust's `Box` is held; and a closure as the storage its
// declaration lays out, which the body its declaration states for it ends, as
// a C++ lambda's object is held.
struct LaidOutByType {};
struct OwnedStorageAddress {};
struct ClosureStorage {};
using Holding = std::variant<
    value::Scalar, LaidOutByType, support::RuntimeObject, OwnedStorageAddress,
    ClosureStorage>;

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
      IntegralType, UnpackedArrayType, AnyReferenceType, MachineCStringType,
      MachineBoolType, MachineIntType, MachineFloatType, MachineArrayType,
      MachineSpanType, MachineFunctionType, RealType, VoidType, ObjectType,
      ClosureType, ErasedCallableType, LibraryType, CoroutineType,
      VariableRefType, PointerType, ManagedRefType, UnmanagedRefType,
      VectorType, TupleType, RecordType, ResolvedType, DriverType,
      ObservableType, OpenWriteType, DesignationType, ObjectWriteType>;

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
  // is held as itself -- machine data, a borrowed pointer -- or is reached
  // where it lives. Each holding names a logical shape only, whose size is a
  // physical fact decided below this layer.
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

  // True for an owned value that lies in storage of its own, which an operand
  // of it names by where it lies and a step of it is taken at: every owned
  // value but a pointer owning what it points at, which is that address
  // itself, as a Rust `Box` is.
  [[nodiscard]] auto IsHeldInStorage() const -> bool;

  // The type of the elements this one holds a sequence of, or nothing where it
  // holds none: the unpacked array (LRM 7.4), the dynamic array (7.5), the
  // associative array (7.8) and the queue (7.10). Narrower than holding values:
  // a machine array and a vector hold a sequence of them and are still not
  // containers, being shapes a lowering builds rather than types a declaration
  // named.
  [[nodiscard]] auto ContainerElementType() const -> std::optional<TypeId>;

  // The type an associative array is indexed by (LRM 7.8), and nothing for a
  // type whose elements are reached by position.
  [[nodiscard]] auto IndexType() const -> std::optional<TypeId>;

  // How this type's sign bit is read as a machine integer, and nothing for a
  // type that is not one. A machine integer's operators are the target's own
  // instructions over one scalar, and the only thing an instruction still has
  // to be told is the signedness. A machine boolean is one such integer, one
  // bit wide and never negative, so it answers rather than standing outside.
  [[nodiscard]] auto MachineIntegerSignedness() const
      -> std::optional<Signedness>;

  // The declaration of the class of the runtime's a value of this type is an
  // object of, where it is one rather than a class some unit declares or no
  // object at all.
  [[nodiscard]] auto RuntimeClassOf() const
      -> std::optional<support::LibraryDecl>;

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
