#pragma once

#include <cstddef>
#include <cstdint>
#include <optional>
#include <string>
#include <variant>
#include <vector>

#include "lyra/base/internal_error.hpp"
#include "lyra/base/interner.hpp"
#include "lyra/mir/class_id.hpp"
#include "lyra/mir/closure_id.hpp"
#include "lyra/mir/integral_constant.hpp"
#include "lyra/mir/record_id.hpp"
#include "lyra/mir/type_id.hpp"
#include "lyra/support/callable_bounds.hpp"
#include "lyra/support/float_width.hpp"
#include "lyra/support/library_decl.hpp"
#include "lyra/support/library_type.hpp"
#include "lyra/support/movability.hpp"
#include "lyra/value/integral_fwd.hpp"

namespace lyra::mir {

// How many values a bit of an integral value can take (LRM 6.11.2). A source
// keyword chooses it -- `bit` two, `logic` four -- but the keyword does not
// reach this layer; what a consumer needs to know is the count.
enum class IntegralStateKind : std::uint8_t {
  kTwoState,
  kFourState,
};

enum class Signedness {
  kSigned,
  kUnsigned,
};

// An integral value's type is how many bits it has, whether they are read as
// signed, and how many values a bit can take (LRM 6.11). How a declaration
// divides those bits into dimensions or members is the source's account of
// them, and reaches this layer only as the bit positions a select names.
struct IntegralType {
  std::uint64_t bit_width;
  Signedness signedness;
  IntegralStateKind state_kind;

  auto operator==(const IntegralType&) const -> bool = default;
};

// The states a bit of a type holds, as the value library names them.
[[nodiscard]] auto StateDomainOf(IntegralStateKind state) -> value::StateDomain;

// The planes a value of `integral` is held in, every position clear: one word
// per 64 bits of its width, and an unknown plane only where a bit of the type
// can be x or z.
[[nodiscard]] auto BlankIntegralConstant(const IntegralType& integral)
    -> IntegralConstant;

// A member is its whole value at the enumeration's base type: over a 4-state
// base it may hold x or z bits, and the base may be wider than a machine word
// (LRM 6.19).
struct EnumMember {
  std::string name;
  IntegralConstant value;

  auto operator==(const EnumMember&) const -> bool = default;
};

struct EnumType {
  IntegralType base;
  std::vector<EnumMember> members;

  auto operator==(const EnumType&) const -> bool = default;
};

// Folds into `seed` what decides an integral type's meaning -- its width,
// signedness and state domain -- and, for an enumeration, that plus every
// member's name and whole value. Shared by every pool keyed on one of these,
// so equal keys cannot hash apart.
void HashIntegral(std::size_t& seed, const IntegralType& integral);
void HashEnumeration(std::size_t& seed, const EnumType& enumeration);

// A fixed-size unpacked array: an element type and how many elements it
// holds, numbered from the left (LRM 7.6), as LLVM's array type is an element
// type and a count. How the source declared the range is a coordinate system a
// select resolves where it is lowered, and no part of the value: two arrays of
// equivalent element types and one count are one type, whatever ranges they
// were declared with (LRM 6.22.2).
struct UnpackedArrayType {
  TypeId element_type;
  std::uint64_t count;

  [[nodiscard]] auto Size() const -> std::uint64_t {
    return count;
  }

  auto operator==(const UnpackedArrayType&) const -> bool = default;
};

// A value of a type known only where the program runs, which is reached only
// through a reference: Rust's `dyn Any` behind a `&mut`. A reference to one
// stands for a variable of a known type, made from a reference to it, and only
// a value of that type's identity is written through it, as the library's
// value of any type carries one (LRM 15.4.5, 13.5.2). It has no value of its
// own to read or hold, as an erased callable has none apart from a box or a
// borrow.
struct ErasedValueType {
  auto operator==(const ErasedValueType&) const -> bool = default;
};

// A borrowed, NUL-terminated machine string (the generic-language `*const
// c_char`, C `const char*`), distinct from the owning string the library
// declares, which is a simulation value. It names raw character storage the
// value does not own, so it copies nothing and carries no length.
struct MachineCStringType {
  auto operator==(const MachineCStringType&) const -> bool = default;
};

// A primitive machine boolean (the generic-language `bool`, C `_Bool`): the
// two-valued scalar a predicate reduction produces and a condition consumes.
// Distinct from a 1-bit SV `IntegralType`, a simulation value reached through
// a value wrapper, and not a width of `MachineIntType`, whose values are the
// integers and admit arithmetic a boolean's two do not.
struct MachineBoolType {
  auto operator==(const MachineBoolType&) const -> bool = default;
};

// The widths a machine scalar comes in. Machine data is what crosses to a
// target on the target's own terms -- a foreign call's by-value argument, a
// table the runtime reads as raw storage -- so the widths are the ones a C
// type system names outright.
enum class MachineIntWidth : std::uint8_t { k8, k16, k32, k64 };

// The width as a number. A target names the type it has for each width rather
// than spelling one from a count, so this is for a reader of the width and not
// for composing a type name.
[[nodiscard]] auto BitsOf(MachineIntWidth width) -> std::uint32_t;

// A primitive machine integer (the generic-language `iN` / `uN`, C `intN_t`):
// a fixed-width 2-state scalar, distinct from the SV `IntegralType`.
// It is plain machine data, not a simulation value, and lowers to a raw target
// integer rather than a value wrapper -- a scope's time precision power
// (LRM 3.14.2), a foreign call's by-value integer argument.
struct MachineIntType {
  MachineIntWidth width;
  Signedness signedness;

  auto operator==(const MachineIntType&) const -> bool = default;
};

// A primitive machine float (the generic-language `fN`, C `float` / `double`):
// plain machine data, what crosses a foreign boundary or a runtime entry as a
// raw target float. Distinct from the SV `RealType` for the reason a machine
// integer is distinct from an integral value: a real is a value of the design,
// carrying the language's own operators and conversions (LRM 11.4.3 `**`, the
// rounding of LRM 6.12.1) and held in a variable's cell, and machine data is
// neither.
struct MachineFloatType {
  support::FloatWidth width;

  auto operator==(const MachineFloatType&) const -> bool = default;
};

// A fixed-size contiguous aggregate of plain machine data (the generic-language
// `[T; N]`, C++ `std::array<T, N>`). It completes the machine family on the
// aggregate axis: the machine scalars above are plain data rather than
// simulation values, and this is the plain-data aggregate of them, distinct
// from `UnpackedArrayType`, which is a simulation value reached through a value
// wrapper. Its element is itself plain machine data -- a machine scalar, a
// pointer, or a runtime-library record -- so the entries of a table the runtime
// reads are stated here rather than being assembled by each backend.
struct MachineArrayType {
  TypeId element;
  std::uint32_t size;

  auto operator==(const MachineArrayType&) const -> bool = default;
};

// A read-only view of a run of plain machine data that something else owns
// (Rust `&[T]`, C++ `std::span<const T>`): which entries, and how many. It is
// made from a machine array, whose type says how many there are, so a view
// and the entries it counts cannot disagree. A table a runtime record names is
// one, and every target lays one out as the runtime lays out its own view.
struct MachineSpanType {
  TypeId element;

  auto operator==(const MachineSpanType&) const -> bool = default;
};

// A pointer to code (the generic-language `fn(A, B) -> R`, C++ `R (*)(A, B)`).
// It completes the machine family on the callable axis: the machine scalars are
// plain data, the machine array is the plain-data aggregate of them, the
// machine span a view of one, and this is the plain code address. Distinct from
// a callable declaration, which is a named body this unit owns and calls by
// name; this is the type an address has when code is a value -- held in a
// table, handed to a runtime that calls back through it.
struct MachineFunctionType {
  std::vector<TypeId> params;
  TypeId result;

  auto operator==(const MachineFunctionType&) const -> bool = default;
};

// An SV floating-point value (LRM 6.12), laid out as the IEEE 754 value of its
// width, whose arithmetic and comparisons are the IEEE ones.
struct RealType {
  support::FloatWidth width;

  auto operator==(const RealType&) const -> bool = default;
};

struct VoidType {
  auto operator==(const VoidType&) const -> bool = default;
};

// The type of an instance of a class some compilation unit declares -- a module
// instance, a named generate scope, an interface, or a SystemVerilog class --
// named by the class's identity in this unit's registry, one per class
// whichever unit defines it. Reaching a member is a separate act from naming
// the type.
struct ObjectType {
  ClassId of;

  auto operator==(const ObjectType&) const -> bool = default;
};

// A type the runtime library declares, named by its declaration and the
// arguments the language gives it. MIR makes, takes apart and lays out no
// value of one: a value is built and reached through the library's calls, and
// whatever MIR asks of one -- whether a temporary may stand in for it, how
// large a frame holding one is, whether it is storage reached where it lies --
// it reads off the declaration, which states it once for every layer.
using LibraryType = support::LibraryType<TypeId>;

// The call protocol of a coroutine callable: invoking it yields a coroutine the
// site must await or spawn, and awaiting it produces a value of `payload` --
// the completion payload (a task's output pack, or `Void` when the completion
// yields nothing). A fork branch (LRM 9.3.2) is a closure whose result type is
// this. What an execution completes with lands in storage of its own that its
// awaiter is handed, never in the frame that made it, so `payload` is never
// boxed, whatever its size. The scheduler holds a single coroutine-handle type
// regardless of `payload`.
struct CoroutineType {
  TypeId payload;

  auto operator==(const CoroutineType&) const -> bool = default;
};

// A structure, a union or a tagged union the source declared (LRM 7.2, 7.3), as
// clang's record type names its record declaration and rustc's ADT type its
// definition: two declarations with the same members are two types (LRM
// 6.22.1). The type is the declaration; which aggregate it is and its members'
// types are the declaration's. A structure's members each hold storage of their
// own; a union's overlay one storage, read through any member (LRM 7.3); a
// tagged union's do too, with the position of the member it holds observable
// as its tag (LRM 7.3.2, 11.9), and a `void` member carries the tuple of no
// components. A member is reached by position. A value of one is a value:
// reference semantics come from the wrapper around it, never from the type.
struct RecordType {
  // Where the declaration sits in the unit's record registry, whichever unit
  // defines it.
  RecordId declaration;

  auto operator==(const RecordType&) const -> bool = default;
};

// The type of a closure value: an anonymous concrete callable value, named by
// its unit-wide identity; the unit's closure registry resolves the id to the
// declaration (its capture fields and one invoke body). Distinct per closure
// site, so two closures of the same signature but different captures are
// different types. Callable directly: a call resolves its signature from the
// declaration's invoke. A separate category from `RecordType`: a closure has an
// invoke body, and a record is a value with none.
struct ClosureType {
  ClosureId closure_id;

  auto operator==(const ClosureType&) const -> bool = default;
};

// A closure whose type is not known where it is called: the generic-language
// `dyn` callable of a signature, `dyn FnMut(A, B) -> R`. What a library
// compiled before the program holds a closure as, behind a box it owns or a
// borrow, and calls without knowing its captures. Its parameters and its
// result are machine types, since the library knows no other. Its kind is the
// trait the erased closure is known to serve, as the principal trait of a
// rustc `dyn` type is part of that type: what the closure's body does to the
// closure, so a holder asking a kind reads whether it is served off the type.
// How long the holder keeps the closure is the holder's to state, as a Rust
// callee states it on its parameter. A closure is made one by a cast of the
// box or the borrow it is held through.
struct ErasedCallableType {
  support::ClosureKind kind;
  std::vector<TypeId> params;
  TypeId result;

  auto operator==(const ErasedCallableType&) const -> bool = default;
};

// The write capability a reference or borrow grants its holder (the
// generic-language `&T` vs `&mut T`, a method's `const` vs non-const receiver).
// Mutability lives on references and borrows, never as a qualifier on an
// ordinary value type; a place's or binding's mutability is a separate axis
// carried where that place or binding lives.
enum class Mutability : std::uint8_t {
  kMutable,
  kReadOnly,
};

// A reference value aliasing a storage cell of `pointee` type (LRM 13.5.2
// pass-by-reference; a fork branch / `$sscanf` / with-clause body sharing
// enclosing storage). It is read and written like an observable cell: a read
// asks it for the cell's contents and a write publishes through the cell, so it
// wakes what waits on the cell (LRM 4.3). A read-only `mutability` marks a
// `const ref` formal (LRM 13.5.2). A library wrapper, like ObservableType;
// constructing one from a cell is an explicit MIR operation, not a
// backend-synthesized wrap.
struct VariableRefType {
  TypeId pointee;
  Mutability mutability;

  auto operator==(const VariableRefType&) const -> bool = default;
};

// A reference held as a member and bound after the object holding it is built
// (C++ `std::reference_wrapper`), where a `VariableRefType` is bound once, as
// it is created. Reading, writing and waiting through one reach what it names
// as through a `VariableRefType`; only a binding makes one name other storage,
// never an assignment. A port is such a member: its connection binds it while
// the design's references are resolved (LRM 23.3.3), and a connection may name
// a port of an instance resolved later.
struct ReferenceWrapperType {
  TypeId pointee;
  Mutability mutability;

  auto operator==(const ReferenceWrapperType&) const -> bool = default;
};

enum class PointerOwnership {
  kUnique,
  kShared,
  kBorrowed,
};

// One level of indirection on two orthogonal axes: `ownership` is the lifetime
// discipline (`kUnique` / `kShared` own the pointee as `unique_ptr<T>` /
// `shared_ptr<T>`; `kBorrowed` owns nothing and only refers as `T*`), and
// `mutability` is the write capability the handle grants -- a read-only borrow
// renders `const T*`, the immutable-receiver (`&self`) case. The two axes are
// independent: ownership says who frees, mutability says who may write.
struct PointerType {
  TypeId pointee;
  PointerOwnership ownership;
  // A pointer reaches storage the holder may write unless the declaration that
  // built it says otherwise, so read-only is the case that has to be stated.
  Mutability mutability = Mutability::kMutable;

  auto operator==(const PointerType&) const -> bool = default;
};

// A reference: another name for an object that exists elsewhere, bound once
// and never null, as `&T` in Rust and `T&` in C++. Unlike a borrowed pointer it
// cannot be rebound or tested, so holding one says only that its referent is
// reached without being copied; `mutability` says whether it may be written
// through. A whole-value operation takes its operands this way, as Rust's
// `PartialEq::eq(&self, other: &Self)` does, so that a value is read where it
// lies rather than copied in to be asked about.
struct ReferenceType {
  TypeId referent;
  Mutability mutability;

  auto operator==(const ReferenceType&) const -> bool = default;
};

// A managed reference (LRM 8.3 class handle): a traced edge to a
// garbage-collected object on the managed heap. It is not a `PointerType` --
// its target's lifetime is governed by reachability, not by RAII ownership, so
// the tracing collector follows it as an edge. Null is a legal value, identity
// is comparable, copies are shallow, and the target is retained while
// reachable. The pointee is the reference's static view at the program point
// holding it -- what may be reached through it there -- and says nothing about
// how a target realizes the reference, which is why one cell reached under two
// views is ordinary rather than a conflict.
struct ManagedRefType {
  TypeId pointee;

  auto operator==(const ManagedRefType&) const -> bool = default;
};

// A reference that owns nothing: the address of an object that lives for its
// own reasons, or null, as C's `T*` and Rust's `*const T` are. It is a value of
// the simulation all the same -- a variable holds one in a cell, it defaults to
// null, it is printed and compared by identity -- which is what separates it
// from a `PointerType`, a quantity of the machine no variable is declared as.
// SystemVerilog has two: a `chandle` (LRM 6.14), whose pointee is `void`
// because the simulation never reaches through it, and a virtual interface
// (LRM 25.9), whose pointee is the object of the interface its type names and
// which an access reaches through, the instance living as long as the
// simulation does.
struct UnmanagedRefType {
  TypeId pointee;

  auto operator==(const UnmanagedRefType&) const -> bool = default;
};

// A homogeneous sequence: one element type, laid down as many times as the
// declaration standing for several objects covers (LRM 23.3.2). A member
// holding one receives it complete, so how many objects stand behind a
// declaration is settled before anything can name them. Unlike
// `MachineArrayType`, which is the plain data an element list is, this is a
// library type whose representation the target owns -- so one comes into
// existence through its own constructor, and the entries that compose it are
// the library's.
struct VectorType {
  TypeId element;

  auto operator==(const VectorType&) const -> bool = default;
};

// A heterogeneous fixed product identified by its components (the Rust / Python
// tuple, C++ `std::tuple`), where `VectorType` is the homogeneous one and a
// structure's `RecordType` the one identified by its declaration. A lowering
// composes one for itself -- the `(key, value)` pair an associative literal is
// a sequence of, a call's completion -- so two with the same components are one
// type.
//
// The tuple of no components has exactly one value, so it is the type of a
// value carrying no information, as Rust's `()` is. SystemVerilog reaches it
// through a tagged union's `void` member (LRM 7.3.2), the one position where
// `void` names a data type: a member is a value slot, which `VoidType`, the
// absence of a type, cannot fill.
struct TupleType {
  std::vector<TypeId> elements;

  auto operator==(const TupleType&) const -> bool = default;
};

// Observable storage wrapper around a value type. Declares that a member's
// storage is a module-scope cell whose write fires the LRM 9.4.2 update event,
// so subscribers wake on a change. The lowering wraps the storage of a variable
// (LRM 6.5), a class handle's and an unmanaged reference's included; a pointer,
// a child instance and a reference to storage elsewhere are not wrapped. The
// C++ backend renders the wrapper as `lyra::runtime::Var<T>` where T is the
// inner value type; the C++ template requires `T` to satisfy
// `lyra::value::LyraValue`, so a value type that forgot to implement the
// contract fails at template instantiation.
struct ObservableType {
  TypeId value;

  auto operator==(const ObservableType&) const -> bool = default;
};

// A net's resolved storage: an observable value produced by resolving the
// contributions of the net's drivers (LRM 6.5, 6.6). Readable and observable
// like an `ObservableType` cell, but never written directly -- a value reaches
// it through a driver, or through a procedural continuous assignment that
// overrides what the drivers resolve to (LRM 10.6.2). The fold those
// contributions resolve under (LRM 6.6) is installed at construction, so it is
// the net's state rather than its type.
struct ResolvedType {
  TypeId value;

  auto operator==(const ResolvedType&) const -> bool = default;
};

// The drive capability for a net: a handle to one of a `ResolvedType` net's
// contributions. A driver updates only its own contribution and never the
// resolved value, which is the net's alone to arrive at.
struct DriverType {
  TypeId value;

  auto operator==(const DriverType&) const -> bool = default;
};

// A write in progress into what a capability wrapper stands for (LRM 11.5.1),
// whose contents hold a value of type `value`. It is an object the writer
// holds, not an address, and it ends with the full-expression that opened it,
// which is when the wrapper learns once whether the write changed it (LRM 4.3).
// It names no place itself: the whole of those contents is designated within
// it, and the steps into parts start there.
struct OpenWriteType {
  TypeId value;

  auto operator==(const OpenWriteType&) const -> bool = default;
};

// A place designated within a write in progress, holding a value of type
// `value`: the whole of what the write was opened on, or a part of it that is
// storage of its own, reached by a step taken within the write. It borrows the
// write rather than holding it, so it ends with nothing to do. A step to an
// element reports what forming it did to the variable -- an element made by
// being written, or an index naming none (LRM 7.8.7, 7.10.1, 7.4.6) -- and
// dereferencing it is where the write lands, so that part's value before the
// write is what its value after is compared with.
struct DesignationType {
  TypeId value;

  auto operator==(const DesignationType&) const -> bool = default;
};

// A reference to some bits of an integral value lying in storage, of type
// `bits`, through which a write lands on them where they lie (LRM 11.5.1), as
// `std::bitset`'s `operator[]` answers a reference to one bit. Bits are no
// storage of their own, so it is no pointer: it names the storage they lie
// in, or a place designated within a write in progress, and where in it they
// start, and a write through it is its `set`. Nothing holds one past the
// full-expression that takes it.
struct BitsReferenceType {
  TypeId bits;

  auto operator==(const BitsReferenceType&) const -> bool = default;
};

// A write in progress into the properties of an object (LRM 8.4), opened on
// the object alone, which answers the object as `object`, the class its
// properties are reached through. It is an object the writer holds and it ends
// with the full-expression that opened it, which is when it tells the object it
// was written (LRM 9.4.2); the expression waiting on the object decides whether
// that was an event, so nothing from before the write is kept.
struct ObjectWriteType {
  TypeId object;

  auto operator==(const ObjectWriteType&) const -> bool = default;
};

// A type one MIR compilation unit names, and the vocabulary for asking what it
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
      IntegralType, EnumType, UnpackedArrayType, ErasedValueType,
      MachineCStringType, MachineBoolType, MachineIntType, MachineFloatType,
      MachineArrayType, MachineSpanType, MachineFunctionType, RealType,
      VoidType, ObjectType, LibraryType, CoroutineType, VariableRefType,
      ReferenceWrapperType, PointerType, ReferenceType, ManagedRefType,
      UnmanagedRefType, VectorType, TupleType, ObservableType, ResolvedType,
      DriverType, OpenWriteType, DesignationType, BitsReferenceType,
      ObjectWriteType, RecordType, ClosureType, ErasedCallableType>;

 public:
  explicit Type(Data data) : data_(std::move(data)) {
  }

  // True for any type whose value is a single vector of bits: an integral
  // type, or an enumeration through its base. A site that treats the type as
  // its integral representation asks this; one that must tell them apart
  // matches on the alternatives directly.
  [[nodiscard]] auto IsIntegral() const -> bool;

  // The integral type a value of this type is, an enumeration's being its
  // base. A type that is not integral has none and is a caller error, never a
  // width guess.
  [[nodiscard]] auto Integral() const -> const IntegralType&;

  // True for a type the library declares by `decl`, whatever its arguments.
  [[nodiscard]] auto IsDeclaredBy(support::LibraryDecl decl) const -> bool;

  // The declaration of the class of the runtime's a value of this type is an
  // object of, where it is one rather than a class some unit declares or no
  // object at all.
  [[nodiscard]] auto RuntimeClassOf() const
      -> std::optional<support::LibraryDecl>;

  // True for a wrapper that grants an access capability over a value it holds
  // rather than being that value: the observable cell, a procedural reference,
  // a net's resolved cell, a net driver's handle.
  [[nodiscard]] auto IsCapabilityWrapper() const -> bool;

  // True for a reference to storage elsewhere, whether bound as it is created
  // or held and bound later: what is read, written or waited on through either
  // is what it names.
  [[nodiscard]] auto IsVariableRef() const -> bool;

  // True for storage that has no value to read and is only ever reached
  // through where it lies: an observable cell, a net's resolved cell, an
  // object, an event, a history, an attempt record. A place of one stands as
  // itself wherever it is handed on, since there is nothing to read out of it.
  [[nodiscard]] auto IsAddressOnly() const -> bool;

  // True for a pointer that owns what it reaches. One is an object with an
  // owner's duties, so `*owner` reaches the pointee through the owner itself,
  // as C++ `*unique_ptr` enters `operator*` on it rather than reading it as
  // builtin `*p` reads a pointer.
  [[nodiscard]] auto OwnsPointee() const -> bool;

  // True for a value built of other values or represented by the library --
  // an array of any kind, a structure, a union, a string, a class handle -- as
  // opposed to an integral value, a real, a machine quantity or an address
  // whose value is the address, which whoever holds it holds as one quantity
  // entire.
  //
  // Narrower than being a domain the runtime realizes values of, which a
  // reference owning nothing also is: its value is the address it carries, and
  // the address is in the holder's own hands.
  [[nodiscard]] auto IsAggregateOrLibraryValue() const -> bool;

  // Whether a temporary can hold a value of this type and stand in its place,
  // and whether it is then read out as a copy or moved out, as rustc's builder
  // consumes a place by `Operand::Copy` where its type is `Copy` and by
  // `Operand::Move` otherwise.
  [[nodiscard]] auto ValueMovability() const -> support::Movability;

  // True for a value whose bits lie contiguously (LRM 7.4.1), so a write into
  // some of them lands on those bits where they lie, at the cost of those bits,
  // rather than writing the whole they are part of.
  [[nodiscard]] auto BitsAreWrittenInPlace() const -> bool;

  // The value a capability wrapper wraps; throws where there is none.
  [[nodiscard]] auto WrappedValueType() const -> TypeId;

  // The type of the elements this one holds a sequence of, or nothing where it
  // holds none: the unpacked array (LRM 7.4), the dynamic array (7.5), the
  // associative array (7.8) and the queue (7.10). Narrower than holding
  // values: a machine array and a vector hold a sequence of them and are still
  // not containers, being shapes a lowering builds rather than types a
  // declaration named.
  [[nodiscard]] auto ContainerElementType() const -> std::optional<TypeId>;

  // True for a container the library declares, whose declaration names its
  // element type among its arguments (LRM 7.5, 7.8, 7.10): every container but
  // the fixed-size unpacked array, which MIR lays out.
  [[nodiscard]] auto IsLibraryContainer() const -> bool;

  // True for such a container whose elements are reached by position, a
  // dynamic array or a queue (LRM 7.5, 7.10), as opposed to an associative
  // array, whose elements are reached by an index of a type of its own.
  [[nodiscard]] auto IsLibrarySequence() const -> bool;

  // The type an associative array is indexed by (LRM 7.8), and nothing for a
  // type whose elements are reached by position.
  [[nodiscard]] auto IndexType() const -> std::optional<TypeId>;

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
          "mir::Type::Get: type is not the alternative asked for");
    }
    return *arm;
  }

  template <typename Visitor>
  auto Visit(Visitor&& visitor) const -> decltype(auto) {
    return std::visit(std::forward<Visitor>(visitor), data_);
  }

  auto operator==(const Type&) const -> bool = default;

  // How a pool spreads the types it holds. It reads the type's alternative and
  // the identities that alternative names rather than walking a member list,
  // because equality decides the answer and a hash only has to separate the
  // types one unit actually holds.
  struct Hash {
    auto operator()(const Type& type) const -> std::size_t;
  };

 private:
  Data data_;
};

// The types one compilation unit names. Interning canonicalizes: within one
// unit a semantic type maps to one `TypeId`, so equal types share an id and a
// `TypeId` comparison is a semantic-type comparison. A class type reaches
// itself through its own declaration identity, which is what lets a recursive
// type graph be built out of complete requests alone.
using TypePool = base::Interner<Type, TypeId, Type::Hash>;

// Whether `type` is a data type of the program (LRM 6.2): what a variable
// declaration names, whose data object is value storage (LRM 6.5) and answers
// every operation a value of the program does -- its default, `==`, `===`,
// whether two are the same bits, whether it holds an unknown. A tuple is one
// exactly where every component is, as rustc's `tuple_impls!` gives a tuple
// `PartialEq` where every component has it, so the tuple of no components is
// one. Everything a lowering builds to carry such a value, reach it, or run
// the program around it is not.
[[nodiscard]] auto IsDataType(const TypePool& types, TypeId type) -> bool;

// The kinds of type each unit naming one states the operations of itself: a
// fixed-size unpacked array, which the language identifies by its element and
// its range rather than by a declaration (LRM 6.22.1 f), and a tuple, which is
// identified by its components, so no declaration states them once for every
// unit, as a structure's and a union's does.
enum class StatedPerUnit : std::uint8_t { kFixedArray, kTuple };

// Which of those kinds `type` is, and nothing for every other type.
[[nodiscard]] auto OperationsStatedPerUnit(const Type& type)
    -> std::optional<StatedPerUnit>;

}  // namespace lyra::mir
