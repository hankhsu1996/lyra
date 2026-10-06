#pragma once

#include <cstddef>
#include <cstdint>
#include <optional>
#include <string>
#include <variant>
#include <vector>

#include "lyra/base/internal_error.hpp"
#include "lyra/base/interner.hpp"
#include "lyra/mir/closure_id.hpp"
#include "lyra/mir/declared_class.hpp"
#include "lyra/mir/integral_constant.hpp"
#include "lyra/mir/struct_id.hpp"
#include "lyra/mir/type_declaration_ref.hpp"
#include "lyra/mir/type_id.hpp"
#include "lyra/support/runtime_class.hpp"

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

struct PackedRange {
  std::int64_t left;
  std::int64_t right;

  [[nodiscard]] auto ElementCount() const -> std::uint64_t;
  [[nodiscard]] auto IsAscending() const -> bool;
  [[nodiscard]] auto Contains(std::int64_t index) const -> bool;

  auto operator==(const PackedRange&) const -> bool = default;
};

struct PackedArrayType {
  IntegralStateKind state_kind;
  Signedness signedness;
  std::vector<PackedRange> dims;

  [[nodiscard]] auto BitWidth() const -> std::uint64_t;

  auto operator==(const PackedArrayType&) const -> bool = default;
};

// A member is its whole value at the enumeration's base type: over a 4-state
// base it may hold x or z bits, and the base may be wider than a machine word
// (LRM 6.19).
struct EnumMember {
  std::string name;
  IntegralConstant value;

  auto operator==(const EnumMember&) const -> bool = default;
};

struct EnumType {
  PackedArrayType base;
  std::vector<EnumMember> members;

  auto operator==(const EnumType&) const -> bool = default;
};

// Folds into `seed` what decides a packed array's meaning -- four-stateness,
// signedness, and the dimensions its width comes from -- and, for an
// enumeration, that plus every member's name and whole value. Shared by every
// pool keyed on one of these, so equal keys cannot hash apart.
void HashPackedShape(std::size_t& seed, const PackedArrayType& packed);
void HashEnumeration(std::size_t& seed, const EnumType& enumeration);

// One declared unpacked dimension, `[left:right]`. Element order runs
// left-to-right (LRM 7.6), so the leftmost element (index `left`) is storage
// ordinal 0. Distinct from `PackedRange`, whose ordinal counts from the right
// (least-significant) end.
struct UnpackedRange {
  std::int64_t left;
  std::int64_t right;

  [[nodiscard]] auto ElementCount() const -> std::uint64_t {
    // A declared range spans `|left - right| + 1` elements and is never empty.
    // The synthetic zero-based empty list `[0:-1]` is the one exception -- it
    // stands in for a zero-length compiler-internal array that a real SV range
    // cannot express. No real declared range is `[0:-1]`.
    if (left == 0 && right == -1) {
      return 0;
    }
    return static_cast<std::uint64_t>(
               left < right ? right - left : left - right) +
           1U;
  }
  [[nodiscard]] auto IsAscending() const -> bool {
    return left <= right;
  }

  // The storage ordinal a source index resolves to: the element the dimension
  // names first is ordinal zero however the range was declared.
  [[nodiscard]] auto LinearOffset(std::int64_t index) const -> std::uint64_t {
    return static_cast<std::uint64_t>(
        left <= right ? index - left : left - index);
  }

  // The declared range of a compiler-internal zero-based array of `count`
  // elements, `[0 : count - 1]`. An empty list is `[0:-1]`.
  [[nodiscard]] static auto ZeroBased(std::uint64_t count) -> UnpackedRange {
    return UnpackedRange{
        .left = 0, .right = static_cast<std::int64_t>(count) - 1};
  }

  auto operator==(const UnpackedRange&) const -> bool = default;
};

// An unpacked array is an element type plus its declared range. The range is
// the array's coordinate system: a source index names an element through it.
// The range lives on the type, not the runtime value -- the backend value is
// ordinal-only payload, and a select reads the range here to name the element
// by its ordinal.
struct UnpackedArrayType {
  TypeId element_type;
  UnpackedRange dim;

  [[nodiscard]] auto Size() const -> std::uint64_t {
    return dim.ElementCount();
  }

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

// LRM 7.8.1 wildcard index type (`[*]`): the key type of an associative array
// indexed by any integral value, identified by magnitude regardless of the
// index expression's width. A parameter-less value type whose C++ realization
// is `lyra::value::WildcardKey`; it is the key type that selects the
// magnitude-identified, width-independent ordering instead of the fixed-shape
// integral or lexicographic-string orderings.
struct WildcardIndexType {
  auto operator==(const WildcardIndexType&) const -> bool = default;
};

struct StringType {
  auto operator==(const StringType&) const -> bool = default;
};

// A borrowed, NUL-terminated machine string (the generic-language `*const
// c_char`, C `const char*`), distinct from the owning `StringType`, which is a
// simulation value. It names raw character storage the value does not own, so
// it copies nothing and carries no length.
struct MachineCStringType {
  auto operator==(const MachineCStringType&) const -> bool = default;
};

// A primitive machine boolean (the generic-language `bool`, C `_Bool`): the
// two-valued scalar a predicate reduction produces and a condition consumes.
// Distinct from the SV 1-bit `PackedArrayType`, a four-state simulation value
// reached through a value wrapper, and not a width of `MachineIntType`, whose
// values are the integers and admit arithmetic a boolean's two do not.
struct MachineBoolType {
  auto operator==(const MachineBoolType&) const -> bool = default;
};

// The widths a machine scalar comes in. Machine data is what crosses to a
// target on the target's own terms -- a foreign call's by-value argument, a
// table the runtime reads as raw storage -- so the widths are the ones a C
// type system names outright.
enum class MachineIntWidth : std::uint8_t { k8, k16, k32, k64 };

enum class MachineFloatWidth : std::uint8_t { k32, k64 };

// The width as a number. A target names the type it has for each width rather
// than spelling one from a count, so this is for a reader of the width and not
// for composing a type name.
[[nodiscard]] auto BitsOf(MachineIntWidth width) -> std::uint32_t;
[[nodiscard]] auto BitsOf(MachineFloatWidth width) -> std::uint32_t;

// A primitive machine integer (the generic-language `iN` / `uN`, C `intN_t`):
// a fixed-width 2-state scalar, distinct from the 4-state SV `PackedArrayType`.
// It is plain machine data, not a simulation value, and lowers to a raw target
// integer rather than a value wrapper -- a scope's time precision power
// (LRM 3.14.2), a foreign call's by-value integer argument.
struct MachineIntType {
  MachineIntWidth width;
  Signedness signedness;

  auto operator==(const MachineIntType&) const -> bool = default;
};

// A primitive machine float (the generic-language `fN`, C `float` / `double`):
// plain machine data, distinct from the SV `RealType`, which is a simulation
// value reached through a value wrapper. It lowers to a raw target float.
struct MachineFloatType {
  MachineFloatWidth width;

  auto operator==(const MachineFloatType&) const -> bool = default;
};

// A fixed-size contiguous aggregate of plain machine data (the generic-language
// `[T; N]`, C++ `std::array<T, N>`). It completes the machine family on the
// aggregate axis: the machine scalars above are plain data rather than
// simulation values, and this is the plain-data aggregate of them, distinct
// from `UnpackedArrayType`, which is a simulation value reached through a value
// wrapper. Its element is itself plain machine data -- a machine scalar, a
// pointer, or a runtime-library record -- so a table the runtime reads as raw
// storage is stated here rather than being assembled by each backend.
struct MachineArrayType {
  TypeId element;
  std::uint32_t size;

  auto operator==(const MachineArrayType&) const -> bool = default;
};

// A pointer to code (the generic-language `fn(A, B) -> R`, C++ `R (*)(A, B)`).
// It completes the machine family on the callable axis: the machine scalars are
// plain data, the machine array is the plain-data aggregate of them, and this
// is the plain code address. Distinct from a callable declaration, which is a
// named body this unit owns and calls by name; this is the type an address has
// when code is a value -- held in a table, handed to a runtime that calls back
// through it.
struct MachineFunctionType {
  std::vector<TypeId> params;
  TypeId result;

  auto operator==(const MachineFunctionType&) const -> bool = default;
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
struct ChandleType {
  auto operator==(const ChandleType&) const -> bool = default;
};

struct VoidType {
  auto operator==(const VoidType&) const -> bool = default;
};

// The type of an instance of a class some compilation unit declares -- a module
// instance, a named generate scope, an interface, or a SystemVerilog class --
// named by the class's identity, one per class whichever unit declares it. Of
// a class of this unit MIR knows the declaration; of another's, only what that
// unit published, and reaching a member is a separate act from naming the type.
struct ObjectType {
  DeclaredClassRef of;

  auto operator==(const ObjectType&) const -> bool = default;
};

// The type of an instance of a class the runtime library defines. MIR does not
// know its members either; unlike a class some unit declares it belongs to no
// compilation unit, so which class it is is the whole identity and no unit
// dependency follows from naming it.
struct RuntimeClassType {
  support::RuntimeClass which;

  auto operator==(const RuntimeClassType&) const -> bool = default;
};

// The runtime capability surface `lyra::runtime::RuntimeEffects`. Reached
// via ambient (`current_runtime()`) with no receiver; every runtime-effect
// call threads it as a plain argument.
struct RuntimeEffectsType {
  auto operator==(const RuntimeEffectsType&) const -> bool = default;
};

// The file-IO subsystem handle `lyra::runtime::FileTable`, reached from the
// runtime through its `Files` method. Opaque, never inspected by MIR.
struct FilesType {
  auto operator==(const FilesType&) const -> bool = default;
};

// The diagnostic subsystem handle `lyra::runtime::DiagnosticDispatcher`,
// reached from the runtime through its `Diagnostic` method. The receiver of
// severity-fixed emit methods (`EmitInfo` / `EmitWarning` / `EmitError`)
// carrying a pre-formatted text. Opaque, never inspected by MIR.
struct DiagnosticType {
  auto operator==(const DiagnosticType&) const -> bool = default;
};

// A pass-through value type from the runtime library that MIR never inspects:
// it is constructed and forwarded to a runtime-effect call, and MIR makes no
// decision on its contents. The branch selects which library type, and the
// backend maps the branch to the concrete library name. An object-model handle
// is a different thing -- receiver semantics reason about one -- while these
// are inert payloads.
enum class RuntimeLibraryKind : std::uint8_t {
  // The declared representation of an integral value: its dimension stack,
  // signedness, and state domain. A factory that builds a value of a type takes
  // one to say which type, so the shape reaches the runtime as an inert payload
  // like any other rather than as a value of the type being built, whose
  // contents would be constructed and then discarded.
  kPackedType,
  // One declared dimension of an integral type: a pair of bounds. It is a
  // payload of that type's descriptor rather than a value any expression has,
  // so it appears in a type position only.
  kPackedRange,
  // The declared range of an unpacked array: the coordinate system a select on
  // one resolves a source index against. It is that type's whole description,
  // where an integral type's is a stack of dimensions plus its signedness and
  // state domain -- one description each, of the shape each family needs.
  kUnpackedRange,
  // The members an enumeration declares, each its whole value at the base type
  // and its name, in declared order. It is that type's description for the
  // questions asked of a value against its members, where the base's own
  // description is what every other operation on the value takes.
  kEnumeration,
  kPrintItem,
  kPrintLiteralItem,
  kPrintValueItem,
  kFormatSpec,
  // LRM 21.3.3 runtime-format operand: `lyra::value::FormatArg`, a type-erased
  // value carrying no conversion of its own. A format string known only at
  // simulation time binds each operand to a conversion as it is parsed, so the
  // operand cannot be paired with a spec ahead of time the way a print item is.
  kFormatArg,
  // LRM 21.3.2 cancel-on-close token: `lyra::runtime::ChannelCancellation`,
  // acquired by `FileTable::CancellationFor(fd)` at `$fstrobe` submit time
  // and queried in the postponed-body guard.
  kChannelCancellation,
  // LRM 20.4.3 `$timeformat` display state: `lyra::value::TimeFormat`, read by
  // the value-layer format step for `%t` directives. Threaded into `Format` as
  // an explicit operand from the engine's current state.
  kTimeFormat,
  // LRM 9.4.2 / 9.4.2.2 / 9.4.3 one leaf of a value-change wait:
  // `lyra::runtime::Trigger`, pairing the observable cell with the bits of its
  // encoding the wait reads and what decides whether a change there is an
  // event. A wait registers a set of these.
  kTrigger,
  // LRM 9.4.2 what an event control watches while a procedure waits at it:
  // `lyra::runtime::Observation`, holding the expression whose value decides
  // the event and the value it had when the wait began. The leaves of one
  // event expression name one of these between them.
  kObservation,
  // LRM 9.4.2 what one evaluation a waiting process makes reached:
  // `lyra::runtime::ReadReport`, into which the evaluation states what it
  // reaches and every function it calls reports what a call of it reads. An
  // `always_comb` or `always_latch` collects its implicit list into one too,
  // once, told what is written as well (LRM 9.2.2.2.1).
  kReadReport,
  // LRM 23.3.3.5 / 27.6 elaborated hierarchy segment:
  // `lyra::runtime::HierarchySegment`, the per-scope structured identity each
  // child carries from construction (base name plus per-dimension indices).
  // The owner of a child threads this into the child's constructor as a
  // single packaged value.
  kHierarchySegment,
  // The canonical buffer a packed vector crosses the DPI-C boundary in (LRM
  // 35.5.6, Annex H.10.1.2): `lyra::value::DpiBitBuffer` holds `svBitVecVal`
  // chunks, `lyra::value::DpiLogicBuffer` holds `svLogicVecVal` chunks. The
  // buffer sizes itself from the SV value and hands the foreign side the
  // canonical chunk pointer.
  kDpiBitBuffer,
  kDpiLogicBuffer,
  // One canonical packed-vector chunk element (LRM 35.5.6, Annex H.10.1.2): the
  // backend-neutral runtime-ABI storage unit a packed value crosses the foreign
  // boundary as, used as a borrowed-pointer pointee to spell the by-pointer
  // carrier an export's C entry point receives. This is a plumbing type, never
  // an SV value type: it has no declared range, signedness, or four-state
  // expression semantics, and no `PackedArray` operation acts on it.
  // `kDpiBitChunk` is a 32-bit value-plane word; `kDpiLogicChunk` is a
  // two-plane `{ aval, bval }` record, each plane a 32-bit word. Each backend's
  // type mapping realizes it: the C++ backend as `svBitVecVal` /
  // `svLogicVecVal`, a lower backend as the layout-equivalent word / record.
  kDpiBitChunk,
  kDpiLogicChunk,
  // A DPI-C open array as the foreign side sees it (LRM 35.5.6.1, Annex H.12):
  // `lyra::value::DpiOpenArray`, the canonical image of a whole actual plus the
  // declared coordinate system of each dimension, built at the call site and
  // read back after it. `kDpiOpenArrayHandle` is the opaque handle the foreign
  // side receives in its place -- `svOpenArrayHandle`, the ABI spelling of a
  // reference to that image, and the only one of the two a prototype names.
  kDpiOpenArray,
  kDpiOpenArrayHandle,
  // LRM 9.6.2 `disable`, in the three parts it takes: the per-instance
  // `lyra::runtime::CancellationTarget` a scope is named through, carrying the
  // generation an execution captures on entry and `disable` advances; and
  // `lyra::runtime::ControlEffect`, the effect raised by leaving a disabled
  // scope, which the region naming that scope binds and consumes.
  kCancellationTarget,
  kControlEffect,
  // What every object of one class carries, so the object answers what its
  // class extends and what a class of the design hierarchy states of its
  // instances: `lyra::runtime::ObjectDefinition`. The class's unit emits it as
  // a constant, which every unit naming the class reaches by its symbol.
  kObjectDefinition,
  // The parts a definition is made of, each a structure of the library a unit
  // states as a constant: `lyra::runtime::ScopeInfo` is what a class of the
  // design hierarchy states of its instances, and `ScopeCallable` a DPI-C
  // export's C name with its body (LRM 35.5.3).
  kScopeInfo,
  kScopeCallable,
};

struct RuntimeLibraryType {
  RuntimeLibraryKind kind;

  auto operator==(const RuntimeLibraryType&) const -> bool = default;
};

// The call protocol of a coroutine callable: invoking it yields a coroutine the
// site must await or spawn, and awaiting it produces a value of `payload` --
// the completion payload (a task's output pack, or `Void` when the completion
// yields nothing). A fork branch (LRM 9.3.2) is a closure whose result type is
// this. `payload` is a MIR-level type; the C++ backend realizes every coroutine
// as one monomorphic `lyra::runtime::Coroutine` and transports the payload
// through a caller-owned completion slot, so the scheduler holds a single
// coroutine-handle type regardless of `payload`.
struct CoroutineType {
  TypeId payload;

  auto operator==(const CoroutineType&) const -> bool = default;
};

// Which declaration a struct type is: one this unit declares, reached by where
// it sits in the unit's struct registry, or one another unit declares, named
// the way that unit names it. Only a struct the source declared has a name, so
// only such a struct is reached from another unit.
using StructRef = std::variant<StructId, TypeDeclarationRef>;

// A heterogeneous fixed product identified by its declaration (the C++, Rust
// and Swift struct): two declarations with the same members are two types. Its
// members are reached by position, as a tuple's components are. The source
// declares one -- an SV unpacked structure (LRM 7.2, 6.22.1) -- and so does a
// lowering, for the variables a scope must keep past its end (LRM 6.21), held
// through a shared pointer the way a program in a language without a collector
// would hold them. A value of one is a value: reference semantics come from the
// wrapper around it, never from the type.
struct StructType {
  StructRef declaration;

  auto operator==(const StructType&) const -> bool = default;
};

// The type of a closure value: an anonymous concrete callable value, named by
// its unit-wide identity; the unit's closure registry resolves the id to the
// declaration (its capture fields and one invoke body). Distinct per closure
// site, so two closures of the same signature but different captures are
// different types. Callable directly: a call resolves its signature from the
// declaration's invoke. A separate category from `StructType`: a closure has an
// invoke body, and a struct is a value with none.
struct ClosureType {
  ClosureId closure_id;

  auto operator==(const ClosureType&) const -> bool = default;
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
// enclosing storage). The runtime wrapper `lyra::runtime::Ref<T>` routes reads
// through `Get` and writes through the cell's `Set` (its update-event path), so
// a value of this type is read and written like an observable cell. A read-only
// `mutability` marks a `const ref` formal (LRM 13.5.2). A library wrapper, like
// ObservableType; constructing one from a cell is an explicit MIR operation,
// not a backend-synthesized wrap.
struct RefType {
  TypeId pointee;
  Mutability mutability;

  auto operator==(const RefType&) const -> bool = default;
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
// tuple, C++ `std::tuple`), where `VectorType` is the homogeneous one and
// `StructType` the one identified by its declaration. A lowering composes one
// for itself -- the `(key, value)` pair an associative literal is a sequence
// of, a call's completion -- so two with the same components are one type.
struct TupleType {
  std::vector<TypeId> elements;

  auto operator==(const TupleType&) const -> bool = default;
};

// Overlapping member storage: one of an ordered list of member types is the
// value at a time (the C `union`, distinct from the product and from a tagged
// sum). The value-layer realization of an SV untagged unpacked union (LRM 7.3).
struct UnionType {
  std::vector<TypeId> members;

  auto operator==(const UnionType&) const -> bool = default;
};

// A value type carrying no information: it has exactly one value, so nothing
// distinguishes two of them. Distinct from `VoidType`, which marks the absence
// of a type -- a callable that yields nothing, a pointee left unspecified.
// SystemVerilog reaches this through a tagged union's `void` member (LRM
// 7.3.2), the one position where `void` names a data type; a value slot needs
// a type that has a value, which absence-of-a-type cannot supply.
struct EmptyType {
  auto operator==(const EmptyType&) const -> bool = default;
};

// Type-checked sum: one of an ordered list of component types is the value at
// a time, with the tag observably part of the value. The value-layer
// realization of an SV tagged unpacked union (LRM 7.3.2 / 11.9). Distinct from
// `UnionType`: an untagged union erases the tag and gives a cross-member read
// a deterministic fallback; here the tag is observable through the pattern-
// matching surface (LRM 12.6) and a member access whose type is inconsistent
// with the tag is a run-time error. A member's position is what the tag codes.
// A `void` member -- allowed only in tagged unions (LRM 7.3.2) -- carries
// `EmptyType`.
struct TaggedUnionType {
  std::vector<TypeId> members;

  auto operator==(const TaggedUnionType&) const -> bool = default;
};

// Observable storage wrapper around a value type. Declares that a member's
// storage is a module-scope cell whose write fires the LRM 9.4.2 update event,
// so subscribers wake on a change. HIR-to-MIR wraps a member declaration whose
// value type is a SystemVerilog data type (not a handle, child instance, or
// external ref) in this wrapper. The C++ backend renders the wrapper as
// `lyra::runtime::Var<T>` where T is the inner value type; the C++ template
// requires `T` to satisfy `lyra::value::LyraValue`, so a value type that forgot
// to implement the contract fails at template instantiation.
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

// What the ticks of one clocking event have settled for one expression (LRM
// 16.9.3). Declares that a member's storage keeps sampled values of the inner
// value type: filled where the design activates, appended to at each tick, and
// read by naming how far back the reader reaches.
//
// How deep it reaches is not part of the type. It is fixed by filling the
// storage rather than by constructing it, so two histories of one value type
// that reach back different distances are one type here and one realization
// below -- which is what keeps a distinction with no realization behind it out
// of the type pool.
struct SampledHistoryType {
  TypeId value;

  auto operator==(const SampledHistoryType&) const -> bool = default;
};

// What one concurrent assertion has in flight: an evaluation attempt per tick
// its clock has reached and not yet answered, each holding its own evaluations
// (LRM 16.14.1). One assertion has one of these, and it holds every attempt
// rather than one merged state, because each attempt carries its own result.
//
// How wide a position set is, and what the statements an outcome selects are,
// are not part of the type. They are fixed by filling the storage rather than
// by constructing it -- the same reason a history's depth is not -- so two
// assertions over properties of different sizes are one type here and one
// realization below.
struct EvaluationAttemptsType {
  auto operator==(const EvaluationAttemptsType&) const -> bool = default;
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
      PackedArrayType, EnumType, UnpackedArrayType, DynamicArrayType, QueueType,
      AssociativeArrayType, WildcardIndexType, StringType, MachineCStringType,
      MachineBoolType, MachineIntType, MachineFloatType, MachineArrayType,
      MachineFunctionType, EventType, RealType, ShortRealType, ChandleType,
      VoidType, ObjectType, RuntimeClassType, RuntimeEffectsType, FilesType,
      DiagnosticType, RuntimeLibraryType, CoroutineType, RefType, PointerType,
      ManagedRefType, VectorType, TupleType, UnionType, TaggedUnionType,
      EmptyType, ObservableType, ResolvedType, DriverType, OpenWriteType,
      DesignationType, ObjectWriteType, SampledHistoryType,
      EvaluationAttemptsType, StructType, ClosureType>;

 public:
  explicit Type(Data data) : data_(std::move(data)) {
  }

  // True for any type whose value-level shape is a single packed vector: a
  // packed array, or an enumeration through its base. A site that treats the
  // type as its integral representation asks this; one that must tell them
  // apart matches on the alternatives directly.
  [[nodiscard]] auto IsIntegralPacked() const -> bool;

  // The packed shape an integral type's value is structured by. A type that is
  // not integral has no such shape and is a caller error, never a width guess.
  [[nodiscard]] auto PackedShape() const -> const PackedArrayType&;

  // True for the two SV floating-point types (LRM 6.12), which share one set
  // of conversions -- the axis a cast or an operator decides on is the family,
  // not which of the two.
  [[nodiscard]] auto IsRealFamily() const -> bool;

  // True for a stable runtime facade the backend realizes as a live reference
  // rather than a movable value -- the runtime handle, the file table handle,
  // the diagnostic dispatcher handle. A lowering site deciding whether an
  // operand may be transferred consults this: an alias handle carries no
  // ownership to move.
  [[nodiscard]] auto IsAliasHandle() const -> bool;

  // True for a wrapper that grants an access capability over a value it holds
  // rather than being that value: the observable cell, a procedural reference,
  // a net's resolved cell, a net driver's handle.
  [[nodiscard]] auto IsCapabilityWrapper() const -> bool;

  // True for a type whose value lives in storage the runtime owns, so whatever
  // holds such a value holds a handle onto that storage rather than the value
  // itself. What follows is that the handle is not the whole of the value:
  // whoever ends the storage ends the value, and a holder that outlives it is
  // left naming nothing. A type answering false is one the holder has entire --
  // a machine quantity, or an address whose value is the address.
  //
  // Narrower than being a domain the runtime realizes values of, which a
  // chandle also is: its value is the pointer it carries, and the pointer is in
  // the holder's own hands.
  [[nodiscard]] auto IsRuntimeStoredValue() const -> bool;

  // True for a value whose parts are each storage of its own, so a write to one
  // lands in it where it lies. A part of any other value is a view, written by
  // writing the whole it is a view of.
  [[nodiscard]] auto PartsAreStorage() const -> bool;

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

}  // namespace lyra::mir
