#pragma once

#include <compare>
#include <cstddef>
#include <cstdint>
#include <functional>
#include <optional>
#include <string>
#include <string_view>
#include <variant>
#include <vector>

#include "lyra/base/arena.hpp"
#include "lyra/base/component_index.hpp"
#include "lyra/base/internal_error.hpp"
#include "lyra/lir/function_id.hpp"
#include "lyra/lir/integral_constant.hpp"
#include "lyra/lir/operator.hpp"
#include "lyra/lir/type_id.hpp"
#include "lyra/support/builtin_fn.hpp"

namespace lyra::lir {

// An SSA-style value: a function parameter or the result of one instruction.
// Values are numbered per function in the function's value arena.
struct ValueId {
  std::uint32_t value;

  auto operator<=>(const ValueId&) const -> std::strong_ordering = default;
};

struct BlockId {
  std::uint32_t value;

  auto operator<=>(const BlockId&) const -> std::strong_ordering = default;
};

// How a value is realized. A parameter arrives in the callable's signature; a
// temporary is a transient computed once and consumed. A place local is named
// storage on the frame. Which one a local is follows a canonical lowering rule,
// not the source notion of a variable: a local is a place exactly when the
// lowering needs an address for it -- its address is taken, it is assigned
// after its initialization, or it holds a control-flow join. LIR is not SSA, so
// a value produced on several paths is a place each path writes, not a merge of
// transients.
enum class LocalKind : std::uint8_t { kParam, kTemp, kPlace };

struct Local {
  std::string name;
  TypeId type;
  LocalKind kind;

  // Whether the local names storage that is written through and addressed, as
  // opposed to standing for the value it was bound to once.
  [[nodiscard]] constexpr auto NamesStorage() const -> bool {
    switch (kind) {
      case LocalKind::kPlace:
        return true;
      case LocalKind::kParam:
      case LocalKind::kTemp:
        return false;
    }
    throw InternalError("lir::Local::NamesStorage: unknown LocalKind");
  }
};

struct Use {
  ValueId value;
};

struct IntConst {
  IntegralConstant value;
  TypeId type;
};

struct StrConst {
  std::string value;
  TypeId type;
};

struct RealConst {
  double value;
  TypeId type;
};

// A null value, the sole literal of the pointer-like types: a chandle (LRM
// 6.14), a class handle (LRM 8.4), a pointer. It carries only its type -- there
// is no payload, since every such value's null is the host null pointer -- and
// the type names the domain the surrounding operator reads it in.
struct NullConst {
  TypeId type;
};

struct BoolConst {
  bool value;
  TypeId type;
};

// An integral type's runtime descriptor, named by the type it describes. Its
// contents are an instruction sequence and an operand is a leaf, so what the
// operand carries is which type is described, not how the description is built.
struct PackedTypeRef {
  TypeId integral;
  TypeId type;
};

// The code address of a function, as a value. A closure is built from a code
// reference plus its environment, so a function's address is an operand, not a
// call target.
struct FuncRef {
  FunctionId function;
};

// The address of storage the whole program shares, named by its linkage
// symbol: a variable of a package (LRM 26.2) or of a compilation-unit scope
// (LRM 3.12.1), which a referrer reaches by a fully resolved name. The data
// counterpart of a code reference -- no local slot and no receiver chain
// arrives at such storage, so a place over one opens at the symbol and
// dereferences it. `type` is a pointer to what the symbol names; how the name
// is resolved -- a link line, an execution session -- is below LIR.
struct StaticRef {
  std::string symbol;
  TypeId type;
};

// An instruction input: a prior value, an inline constant, or a reference to
// code or to shared storage. A constant or a reference is an operand rather
// than a value of its own because it has no dataflow origin to name -- it is
// materialized at the use site.
using Operand = std::variant<
    Use, IntConst, StrConst, RealConst, NullConst, BoolConst, PackedTypeRef,
    FuncRef, StaticRef>;

// A runtime-library entry. A static factory is named by its type namespace as
// well as its function -- `String::FromPackedArray` and `PackedArray::FromInt`
// are different entries of one `fn` -- so the qualifying type rides the target.
// It is absent for an entry that takes a receiver, whose type the receiver
// already names.
struct BuiltinTarget {
  support::BuiltinFn fn;
  std::optional<TypeId> qualifier;
};

// A function of this unit, named outright. The callee is static, so it is
// reached at compile time -- there is no code value in flight and no dispatch.
// A function a class declares as a method takes its receiver as the first
// argument like any other, so naming one is no different from naming a
// function no class declares.
struct FunctionTarget {
  FunctionId function;
};

// The type the call builds a value of, which is the whole identity: a type has
// one way to come into existence, so naming it names the entry. A wrapper that
// owns what it points at brings the pointee into existence along with itself,
// which is the same one way seen from the owner.
struct ConstructTarget {
  TypeId result;
};

// A function outside the program, called by its linkage name -- a DPI-C
// import's foreign symbol (LRM 35.4). The host resolves the name: a link line
// for an ahead-of-time image, the execution session for a JIT one. The
// signature is the call's own, since every operand and the result are already
// the ABI carriers the boundary marshaled to, so the target carries nothing but
// the name.
struct ForeignTarget {
  std::string symbol;
};

// An operation on a value in the activation frame -- the storage a value-typed
// local gets in a suspending body, where a value's handle cannot outlive the
// stretch that produced it. MIR-to-LIR introduces this as storage placement:
// `kAllocate` builds a cell in the running activation's frame, `kLoad` copies
// its value out, `kStore` overwrites it. A LIR-only target with no MIR twin --
// the activation frame is a below-MIR storage realization the C++ backend never
// sees -- so a backend realizes it the way it realizes any call: the value
// domain the op works in names the runtime entry.
struct ActivationFrameTarget {
  enum class Op : std::uint8_t { kAllocate, kLoad, kStore };
  Op op;
};

// An operation on the control effect that leaves a disabled target (LRM
// 9.6.2). `kHasInvalidatedTarget` and `kInvalidatedTarget` ask whether a target
// this execution is inside was disabled while it was away, and which one; both
// are asked where an execution regains control, and each is answered by
// comparing generations in the runtime rather than by reading a flag anyone
// set. The effect crosses as that target, since naming one is all an effect
// does. `kSettleCancelled` reports an effect that left the body with no region
// of it claiming the effect, which is this activation's outcome rather than a
// value it returns. A LIR-only target with no MIR twin: a target that can be
// unwound through reaches all of this by unwinding instead, so what a body
// carries is the region alone.
struct ControlEffectTarget {
  enum class Op : std::uint8_t {
    kHasInvalidatedTarget,
    kInvalidatedTarget,
    kSettleCancelled
  };
  Op op;
};

// The operation's stable spelling. This is an interface contract, not a display
// string: it names the operation in a dump, and it is the operation half of the
// runtime-library symbol a generated module calls, so changing it renames a
// linked symbol. Change it only to correct the operation's identity, never to
// improve how a dump reads.
auto ActivationFrameOpName(ActivationFrameTarget::Op op) -> std::string_view;
auto ControlEffectOpName(ControlEffectTarget::Op op) -> std::string_view;

// The target of a call: a runtime builtin, a function of this unit, a value
// constructor named by the call's result type, a foreign symbol the host
// resolves, an activation-frame value operation, or a control-effect operation.
using CallTarget = std::variant<
    BuiltinTarget, FunctionTarget, ConstructTarget, ForeignTarget,
    ActivationFrameTarget, ControlEffectTarget>;

struct CallInstr {
  CallTarget target;
  std::vector<Operand> args;
};

// Builds a product value from its components, in declaration order. Its result
// is the product itself; the components keep their own types.
struct ProductInstr {
  std::vector<Operand> components;
};

// Collects `elements` into contiguous storage and names it by a
// {pointer, length} span. The result is that storage, not a container built
// from it: a container the span feeds is a separate construction, so this
// instruction is the same operation whichever one consumes it.
struct ArrayInstr {
  std::vector<Operand> elements;
};

// Names a subvalue within an aggregate value. A `TupleElement` selects a
// product component by its declaration-order position, carrying no operands
// because the position is the whole coordinate. A `UnionMember` selects a
// union's member by the same kind of position but is a distinct selector: a
// union holds one member at a time, so an update makes the selected member the
// live one rather than replacing one of several that coexist.
struct TupleElement {
  base::ComponentIndex index;
};

struct UnionMember {
  base::ComponentIndex index;
};

// One coordinate into a homogeneous or keyed value, and one fixed-width range
// of a value. `operands` carries the source coordinates followed by whatever
// the value's family takes from its static type rather than from the value -- a
// declared range, a declared result shape. Which runtime entry realizes a step
// follows from the aggregate's type, below this layer; the selector says only
// which subvalue is named.
struct ContainerElement {
  std::vector<Operand> operands;
};

struct ContainerSlice {
  std::vector<Operand> operands;
};

using AggregateSelector =
    std::variant<TupleElement, UnionMember, ContainerElement, ContainerSlice>;

// Extracts a subvalue of an aggregate value, named by `selector`. The aggregate
// is a value, reached by value: the subvalue is copied out, not aliased. This
// is the read half of value-aggregate access, the peer of LLVM's
// `extractvalue`; it is distinct from a place projection, which reaches
// independently addressable storage. The result's type is the selected
// subvalue's type.
struct AggregateExtractInstr {
  Operand aggregate;
  AggregateSelector selector;
};

// Produces an aggregate value equal to `aggregate` with the subvalue at
// `selector` replaced by `replacement`. A pure value operation -- it never
// mutates the operand -- so a component write is a whole-value store of the
// result, and value semantics hold even when the operand is shared. The peer of
// LLVM's `insertvalue`; a value aggregate has no independently addressable
// interior, so a subvalue write is this functional update, not a store into a
// sub-place. The result's type is the operand's aggregate type.
struct AggregateUpdateInstr {
  Operand aggregate;
  AggregateSelector selector;
  Operand replacement;
};

// A logical member identity local to the declaration that holds it: the
// member's stable declaration-order slot, carried over from the MIR field it
// lowers from. It is meaningful only together with the base's own type --
// `member 0` of one declaration is unrelated to `member 0` of another. Never a
// physical index or byte offset.
struct MemberId {
  std::uint32_t value;

  auto operator<=>(const MemberId&) const -> std::strong_ordering = default;
};

// Reaches the storage a reference-like value refers to. A projection chain may
// only cross a pointer through this step: a member step never implicitly
// dereferences, so `self.counter` -- whose receiver arrives as a pointer -- is
// the chain `deref, member`, never `member` alone.
struct DerefProjection {};

// Selects a member of whatever the projection has reached so far -- an
// instance's own storage, or the captures a closure value holds.
struct MemberProjection {
  MemberId member;
};

// One step of a place's projection chain. The place vocabulary is dereference,
// member, index, slice, and downcast: each names storage reached from the
// storage the chain has arrived at, never a byte offset from it.
using Projection = std::variant<DerefProjection, MemberProjection>;

// Storage named by logical identity: a base plus a projection chain. The base
// is either a place local, whose storage the chain starts at, or a
// reference-like value, which the chain must open with a dereference. A place
// is what a load, store, or address-of names; the physical address it resolves
// to is derived below LIR, never encoded here. An empty chain names the base
// local itself.
struct Place {
  Operand base;
  std::vector<Projection> chain;
};

// Reads the value held at `place`. The result's type is the place's type -- the
// type the projection chain arrives at. A place whose storage is a runtime cell
// object names the cell; the value the cell holds is the place one dereference
// further, and reading that is what goes through the cell's own access.
struct LoadInstr {
  Place place;
};

// Writes `value` into `place`. The store yields no value; its instruction
// result is an unused void.
struct StoreInstr {
  Place place;
  Operand value;
};

// Names the address of `place`. The result is a borrowed pointer to the place's
// type. This is how storage itself -- a cell, an aggregate, a member the callee
// mutates -- is handed to a callee, as opposed to a copy of its contents.
struct AddrOfInstr {
  Place place;
};

// Applies an operator to values. The operator's semantics come from the operand
// type: the same `kAdd` is a machine add over a machine integer and an
// X-propagating library add over a four-state packed value.
struct BinaryInstr {
  BinaryOp op;
  Operand lhs;
  Operand rhs;
};

struct UnaryInstr {
  UnaryOp op;
  Operand operand;
};

// Reduces a value to a machine boolean, the type a conditional branch tests.
// This is the explicit form of the contextual conversion a C++ target performs
// implicitly in a boolean context.
struct BoolCastInstr {
  Operand operand;
};

// Reinterprets a reference-like value as a reference to the result type. It
// moves no bits; it names the destination type that an implicit conversion
// would otherwise leave for a consumer to infer.
struct PointerCastInstr {
  Operand operand;
};

// Names the result type for a value whose bits the destination structures
// identically -- crossing between an enumeration and its base is what reaches
// here. The peer of the pointer cast above on the value side: it moves no bits
// and computes nothing, and exists because the type a value is held to is part
// of what a program states.
struct ValueCastInstr {
  Operand operand;
};

// Converts a machine integer to the machine integer the result type names,
// truncating or extending it. Extension follows the *source* type's signedness,
// which is what decides whether the added high bits repeat the sign bit or are
// zero. This is a machine conversion, not a simulation-value one: a packed
// value's resize is a library call.
struct IntCastInstr {
  Operand operand;
};

using InstrData = std::variant<
    CallInstr, ProductInstr, ArrayInstr, AggregateExtractInstr,
    AggregateUpdateInstr, LoadInstr, StoreInstr, AddrOfInstr, BinaryInstr,
    UnaryInstr, BoolCastInstr, PointerCastInstr, ValueCastInstr, IntCastInstr>;

// One instruction: it defines `result` (whose type lives on the function's
// value arena) from `data`.
struct Instr {
  ValueId result;
  InstrData data;
};

// Returns from the callable; the value, when present, rides the result. Whether
// this is a coroutine completion or a plain return is the callable's result
// type, not a property of the terminator.
struct ReturnTerm {
  std::optional<Operand> value;
};

// Transfers to `target` unconditionally.
struct BranchTerm {
  BlockId target;
};

// Tests a machine boolean and transfers to one of two successors.
struct CondBranchTerm {
  Operand condition;
  BlockId if_true;
  BlockId if_false;
};

// Hands control back to the scheduler and resumes at `resume` when the
// activation is next run. It schedules nothing and names no wakeup source: the
// source is registered by the runtime calls that precede this terminator, so a
// delay, an event control, and a level wait differ only in those calls, never
// in the suspend.
struct SuspendTerm {
  BlockId resume;
};

// Ends a block control never reaches -- the join of a conditional whose arms
// all returned, or the tail of a value-returning body that always returns
// earlier. Reaching it is undefined, which is what lets a target drop the
// block.
struct UnreachableTerm {};

using TerminatorData = std::variant<
    ReturnTerm, BranchTerm, CondBranchTerm, SuspendTerm, UnreachableTerm>;

struct Terminator {
  TerminatorData data;
};

// A maximal straight-line run of instructions entered only at its first and
// left only at its last. The terminator is what makes it one: control leaves
// through it and nowhere else, so a block is never partly formed here -- a run
// of instructions with no decided exit is a thing under construction, which the
// pass building it holds in its own shape.
struct BasicBlock {
  std::vector<Instr> instrs;
  Terminator terminator;
};

// A callable lowered to a CFG. `name` is unique across the unit, so it is the
// symbol the function is emitted and linked under; a consumer never composes
// one from where the function is listed. `values` holds every value of the
// body -- parameters first, then the temporaries and place locals it minted;
// `params` names the parameter subset in signature order, with the receiver
// `self` at `params[0]`. The entry block is `blocks[0]`, and a `BlockId`
// indexes `blocks`. A body whose call protocol is the coroutine one carries
// that fact in its `result_type` (a `CoroutineType`): it may hold
// `SuspendTerm`s and its completion is a coroutine completion, which a backend
// realizes through the scheduling protocol rather than a single call.
struct Function {
  std::string name;
  base::Arena<Local, ValueId> values;
  std::vector<ValueId> params;
  TypeId result_type;
  std::vector<BasicBlock> blocks;
};

// The type of a value operand: the type of the value a use names, or of a
// constant. A code reference names a callable, not a value, so it has none.
auto OperandType(const Function& fn, const Operand& operand)
    -> std::optional<TypeId>;

}  // namespace lyra::lir

// A `ValueId` is a value identity, so it keys hashed containers directly rather
// than being unwrapped to its raw integer at the use site.
template <>
struct std::hash<lyra::lir::ValueId> {
  auto operator()(lyra::lir::ValueId id) const noexcept -> std::size_t {
    return std::hash<std::uint32_t>{}(id.value);
  }
};
