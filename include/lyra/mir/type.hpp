#pragma once

#include <cstdint>
#include <optional>
#include <string>
#include <variant>
#include <vector>

#include "lyra/mir/class_id.hpp"
#include "lyra/mir/type_id.hpp"

namespace lyra::mir {

enum class TypeKind {
  kPackedArray,
  kEnum,
  kUnpackedArray,
  kDynamicArray,
  kQueue,
  kAssociativeArray,
  kWildcardIndex,
  kString,
  kEvent,
  kReal,
  kShortReal,
  kRealTime,
  kChandle,
  kVoid,
  kObject,
  kExternalUnitObject,
  kScope,
  kServices,
  kFiles,
  kDiagnostic,
  kRuntimeLibrary,
  kCoroutine,
  kReference,
  kPointer,
  kManagedRef,
  kVector,
  kTuple,
  kUnion,
  kExternalRef,
  kObservable,
};

enum class BitAtom {
  kBit,
  kLogic,
  kReg,
};

enum class Signedness {
  kSigned,
  kUnsigned,
};

enum class PackedArrayForm {
  kExplicit,
  kByte,
  kShortInt,
  kInt,
  kLongInt,
  kInteger,
  kTime,
};

struct PackedRange {
  std::int64_t left;
  std::int64_t right;

  [[nodiscard]] auto ElementCount() const -> std::uint64_t;
  [[nodiscard]] auto IsAscending() const -> bool;
  [[nodiscard]] auto Contains(std::int64_t index) const -> bool;
  [[nodiscard]] auto LinearOffset(std::int64_t index) const -> std::uint64_t;

  auto operator==(const PackedRange&) const -> bool = default;
};

struct PackedArrayType {
  BitAtom atom;
  Signedness signedness;
  std::vector<PackedRange> dims;
  PackedArrayForm form;

  [[nodiscard]] auto BitWidth() const -> std::uint64_t;
  [[nodiscard]] auto IsFourState() const -> bool;
};

struct EnumMember {
  std::string name;
  std::int64_t value;

  auto operator==(const EnumMember&) const -> bool = default;
};

struct EnumType {
  PackedArrayType base;
  std::vector<EnumMember> members;
};

// LRM 7.2.1 / 7.3.1 packed struct and packed union have no MIR-level
// distinct shape: HIR -> MIR translates `hir::PackedStructType` and
// `hir::PackedUnionType` to their "single vector" projection
// (`PackedArrayType`). Field accesses lower to constant-bounds RangeSelect
// against that vector. An unpacked struct is a value product and maps to the
// existing `TupleType`, not a distinct variant; an unpacked union (overlapping
// member storage) is a separate representation problem.

// MIR tracks unpacked arrays as plain C++ vectors: element type plus a
// non-zero element count. The SV declared range (`[left:right]`, descending
// or with a non-zero base) is resolved at the HIR-to-MIR boundary -- the
// index translation lives inside ElementSelectExpr.index, not on the type.
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

// The type of an instance of a class of this compilation unit -- a module
// instance, a named generate scope, or a SystemVerilog class. It names the
// class by its unit-wide identity; the unit's class registry resolves the id to
// the declaration. The name is not here -- it is a property of the registered
// declaration, read through the id.
struct ObjectType {
  ClassId class_id;

  auto operator==(const ObjectType&) const -> bool = default;
};

// The cross-unit twin of ObjectType: the target is another unit's name,
// resolved by name at link time, not a scope of this unit -- so its layout is
// not visible here.
struct ExternalUnitObjectType {
  std::string unit_name;

  auto operator==(const ExternalUnitObjectType&) const -> bool = default;
};

// The runtime object-tree base class `lyra::runtime::Scope`, type-erased. A
// by-name navigation handle (a `GetChild` result) is a
// `PointerType{ScopeType, kBorrowed}`; the concrete child class is unknown
// across the unit boundary, so only the runtime base is named.
struct ScopeType {
  auto operator==(const ScopeType&) const -> bool = default;
};

// The engine facade `lyra::runtime::RuntimeServices`. A callable body reaches
// it from `self` through the `Services` scope method; it is the engine handle
// every runtime-effect call threads as a plain argument.
struct ServicesType {
  auto operator==(const ServicesType&) const -> bool = default;
};

// The file-IO subsystem handle `lyra::runtime::FileTable`, reached from
// `RuntimeServices` through its `Files` method. A by-name navigation handle
// like ServicesType; the receiver of file-layer methods (`CancellationFor`,
// future `Open` / `Close` / ...). Opaque, never inspected by MIR.
struct FilesType {
  auto operator==(const FilesType&) const -> bool = default;
};

// The diagnostic subsystem handle `lyra::runtime::DiagnosticDispatcher`,
// reached from `RuntimeServices` through its `Diagnostic` method. The
// receiver of severity-fixed emit methods (`EmitInfo` / `EmitWarning` /
// `EmitError`) carrying a pre-formatted text. Opaque, never inspected by
// MIR.
struct DiagnosticType {
  auto operator==(const DiagnosticType&) const -> bool = default;
};

// A pass-through value type from the runtime library that MIR never inspects:
// it is constructed (via ConstructExpr) and forwarded to a runtime-effect call,
// and MIR makes no decision on its contents. The branch selects which library
// type, and the backend maps the branch to the concrete library name. Distinct
// from ServicesType / ScopeType, which are runtime object-model handles the
// receiver semantics reason about; these are inert payloads.
enum class RuntimeLibraryKind : std::uint8_t {
  kPrintItem,
  kPrintLiteralItem,
  kPrintValueItem,
  kFormatSpec,
  // LRM 21.3.2 cancel-on-close token: `lyra::runtime::ChannelCancellation`,
  // acquired by `FileTable::CancellationFor(fd)` at `$fstrobe` submit time
  // and queried in the postponed-body guard.
  kChannelCancellation,
  // LRM 20.4.3 `$timeformat` display state: `lyra::value::TimeFormat`, read by
  // the value-layer format step for `%t` directives. Threaded into `Format` as
  // an explicit operand from the engine's current state.
  kTimeFormat,
  // LRM 23.3.3.5 / 27.6 elaborated hierarchy segment:
  // `lyra::runtime::HierarchySegment`, the per-scope structured identity each
  // child carries from construction (base name plus per-dimension indices).
  // The owner of a child threads this into the child's constructor as a
  // single packaged value.
  kHierarchySegment,
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

// A reference value aliasing a storage cell of `pointee` type (LRM 13.5.2
// pass-by-reference; a fork branch / `$sscanf` / with-clause body sharing
// enclosing storage). The runtime wrapper `lyra::runtime::Ref<T>` routes reads
// through `Get` and writes through the cell's `Set` (its update-event path), so
// a value of this type is read and written like an observable cell. `is_const`
// marks a `const ref` formal (read-only). A library wrapper, like
// ObservableType; constructing one from a cell is an explicit MIR operation,
// not a backend-synthesized wrap.
struct RefType {
  TypeId pointee;
  bool is_const = false;

  auto operator==(const RefType&) const -> bool = default;
};

enum class PointerOwnership {
  kUnique,
  kShared,
  kBorrowed,
};

// One level of indirection, classified by its lifetime axis: `kUnique` and
// `kShared` own the pointee (`unique_ptr<T>` / `shared_ptr<T>`); `kBorrowed`
// owns nothing and only refers (`T*`).
struct PointerType {
  TypeId pointee;
  PointerOwnership ownership;

  auto operator==(const PointerType&) const -> bool = default;
};

// A managed reference (LRM 8.3 class handle): a traced edge to a
// garbage-collected object on the managed heap. It is not a `PointerType` --
// its target's lifetime is governed by reachability, not by RAII ownership, so
// the tracing collector follows it as an edge. Null is a legal value, identity
// is comparable, copies are shallow, and the target is retained while
// reachable. The C++ backend renders it as `lyra::runtime::GcRef<T>`.
struct ManagedRefType {
  TypeId pointee;

  auto operator==(const ManagedRefType&) const -> bool = default;
};

struct VectorType {
  TypeId element;

  auto operator==(const VectorType&) const -> bool = default;
};

// A heterogeneous fixed product: an ordered list of component types, each
// independent. MIR's only heterogeneous aggregate -- the generic-language
// product type (the Rust / Python tuple, C++ `std::tuple` / `std::pair`),
// where `VectorType` is the homogeneous one. Built by `TupleExpr`. It is what
// lets an associative literal be a vector of `(key, value)` pairs instead of
// two parallel lists, so no associative-specific construction node is needed.
struct TupleType {
  std::vector<TypeId> elements;

  auto operator==(const TupleType&) const -> bool = default;
};

// Overlapping member storage: one of an ordered list of component types is the
// value at a time (the generic-language C `union`, distinct from the product
// `TupleType` and from a tagged sum). The value-layer realization of an SV
// untagged unpacked union (LRM 7.3); the member names are dropped to positions
// at HIR-to-MIR, the index is the carrier. Built by `UnionExpr`, read by
// `UnionGetExpr`, written through `UnionGetRefExpr`. Carries component
// types only: a tagged union is a separate concept rejected at the HIR-to-MIR
// gate, so no flag distinguishes one here.
struct UnionType {
  std::vector<TypeId> elements;

  auto operator==(const UnionType&) const -> bool = default;
};

// Observable storage wrapper around a value type. Declares that a member's
// storage is a module-scope cell that exposes Set / Get / Mutate
// (LRM 9.4.2 update event) -- writes route through the engine so subscribers
// wake. HIR-to-MIR wraps a member declaration whose value type is a
// SystemVerilog data type (not a handle, child instance, or external ref) in
// this wrapper. The C++ backend renders the wrapper as `lyra::runtime::Var<T>`
// where T is the inner value type; the C++ template requires `T` to satisfy
// `lyra::value::LyraValue`, so a value type that forgot to implement the
// contract fails at template instantiation.
struct ObservableType {
  TypeId value;

  auto operator==(const ObservableType&) const -> bool = default;
};

// An upward cross-unit reference's storage type: a wrapper around `element`
// that the runtime relocates at the bind step. The wrapper exposes its
// per-reference state (the climb shape, the descent path, the leaf signal
// name) through method calls (`BindRoot`, `BindVisibleChild`); MIR carries
// no per-reference data in the type itself. The construction state is
// emitted in the constructor body as ordinary `CallExpr` against one of
// those methods, with `StringLiteral` / `ArrayLiteralExpr` primitives for
// the arguments. The type carries only the classification (this member is an
// upward extern reference of `element`).
struct ExternalRefType {
  TypeId element;

  auto operator==(const ExternalRefType&) const -> bool = default;
};

using TypeData = std::variant<
    PackedArrayType, EnumType, UnpackedArrayType, DynamicArrayType, QueueType,
    AssociativeArrayType, WildcardIndexType, StringType, EventType, RealType,
    ShortRealType, RealTimeType, ChandleType, VoidType, ObjectType,
    ExternalUnitObjectType, ScopeType, ServicesType, FilesType, DiagnosticType,
    RuntimeLibraryType, CoroutineType, RefType, PointerType, ManagedRefType,
    VectorType, TupleType, UnionType, ExternalRefType, ObservableType>;

struct Type {
  TypeData data;

  [[nodiscard]] auto Kind() const -> TypeKind;
  [[nodiscard]] auto IsPackedArray() const -> bool;
  [[nodiscard]] auto AsPackedArray() const -> const PackedArrayType&;
  [[nodiscard]] auto IsEnum() const -> bool;
  [[nodiscard]] auto AsEnum() const -> const EnumType&;
  // True for any type whose value-level shape is a single packed vector:
  // PackedArrayType or EnumType (base). Sites that treat the type as its
  // integral representation use this predicate; sites that need to
  // distinguish should match on the variant directly.
  [[nodiscard]] auto IsIntegralPacked() const -> bool;
  [[nodiscard]] auto AsIntegralPacked() const -> const PackedArrayType&;
};

class CompilationUnit;

// A child-scope member, classified by the leaf object type after stripping any
// vector layers: an intra-unit object is a generate scope (carrying its target
// scope's class name), an external-unit object is a module instance.
struct GenerateScopeChild {
  std::string name;
};
struct ModuleInstanceChild {};
using ChildScope = std::variant<GenerateScopeChild, ModuleInstanceChild>;

[[nodiscard]] auto GetChildScope(const CompilationUnit& unit, TypeId type)
    -> std::optional<ChildScope>;

// True for storage forms that expose the observable-cell surface
// (`Get` / `Set` / `Mutate`): the explicit `ObservableType` wrapper and the
// intrinsic `ExternalRefType` (upward hierarchical reference).
[[nodiscard]] auto IsObservableCellType(const Type& ty) -> bool;

// The inner value type of an observable storage wrapper -- the `value` of
// an `ObservableType`, the `element` of an `ExternalRefType`.
[[nodiscard]] auto ObservableInnerValueType(const Type& ty) -> TypeId;

}  // namespace lyra::mir
