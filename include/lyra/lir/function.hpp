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
#include "lyra/base/pool_id.hpp"
#include "lyra/lir/enum_table_id.hpp"
#include "lyra/lir/function_id.hpp"
#include "lyra/lir/integral_constant.hpp"
#include "lyra/lir/integral_constant_id.hpp"
#include "lyra/lir/linkage.hpp"
#include "lyra/lir/operator.hpp"
#include "lyra/lir/type_id.hpp"
#include "lyra/support/builtin_fn.hpp"

namespace lyra::lir {

// A value of a body: a parameter, a transient an assignment defines, or a
// place local naming storage. Values are numbered per function in the
// function's value arena.
struct ValueId {
  std::uint32_t value = base::kUnassignedId;

  auto operator<=>(const ValueId&) const -> std::strong_ordering = default;
};

struct BlockId {
  std::uint32_t value = base::kUnassignedId;

  auto operator<=>(const BlockId&) const -> std::strong_ordering = default;
};

// How a value is realized. A parameter arrives in the callable's signature; a
// temporary is a transient computed once and consumed. A place local is named
// storage: the frame's, or, for the answer, wherever the backend's convention
// for answering puts it. Which one a local is follows a canonical lowering
// rule, not the source notion of a variable: a local is a place exactly when
// the lowering names it as storage -- a declared local, a value a part is
// reached in, a result that control-flow paths join at, or the answer. LIR is
// not SSA, so a value produced on several paths is a place each path writes,
// not a merge of transients.
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

// The one value of the void type, as rustc's MIR writes `()`: what an
// evaluation yielding nothing stands as where an operand is wanted.
struct VoidConst {
  TypeId type;
};

// How large a value of `of` is, or what its address is a multiple of, as a
// number of `type`: rustc's MIR names `size_of::<T>()` and `align_of::<T>()`
// the same way, with no number in it. Laying the type out is the layer
// below's, so that layer answers it.
struct LayoutConst {
  enum class Measure : std::uint8_t { kSize, kAlignment };
  TypeId of;
  Measure measure;
  TypeId type;
};

// An enumeration's member table, named by the entry holding it. Its contents
// are data the unit states and an operand is a leaf, so what the operand
// carries is which table it is.
struct EnumTableRef {
  EnumTableId table;
  TypeId type;
};

// A constant value of the unit, named by the entry holding it. Its contents are
// data the unit states and an operand is a leaf, so what the operand carries is
// which constant it is.
struct IntegralConstantRef {
  IntegralConstantId constant;
  TypeId type;
};

// The address of storage the whole program shares, named by its linkage
// symbol: a variable of a package (LRM 26.2) or of a compilation-unit scope
// (LRM 3.12.1), which a referrer reaches by a fully resolved name. No local
// slot and no receiver chain arrives at such storage, so a place over one opens
// at the symbol and dereferences it. `type` is a pointer to what the symbol
// names; how the name is resolved -- a link line, an execution session -- is
// below LIR.
struct StaticRef {
  std::string symbol;
  TypeId type;
};

// The address of the definition every value of `defined` carries, a constant
// the unit declaring that type emits. `type` is a pointer to the definition.
struct DefinitionRef {
  TypeId defined;
  TypeId type;
};

// The address of a function of this unit, read as a value: code handed to
// whatever calls back through it. `type` is the function's machine function
// type.
struct FunctionRef {
  FunctionId function;
  TypeId type;
};

// An instruction input: a prior value, an inline constant, or a reference to
// a constant, to shared storage or to code. A constant or a reference is an
// operand rather than a value of its own because it has no dataflow origin to
// name -- it is materialized at the use site.
using Operand = std::variant<
    Use, IntConst, StrConst, RealConst, NullConst, BoolConst, VoidConst,
    LayoutConst, EnumTableRef, IntegralConstantRef, StaticRef, DefinitionRef,
    FunctionRef>;

// A runtime-library entry. `position` names the part the entry acts on where
// the call itself fixes it, carried on the callee rather than among the
// arguments because the part named has a type of its own. A target whose calls
// take only values writes it as one more argument; one that resolves types
// writes it where it resolves them. `type_argument` is the type an entry
// generic over one is called at, which is part of which code is called: a
// target calling code compiled once for every type hands that code what it
// needs of the type.
struct BuiltinTarget {
  support::BuiltinFn fn;
  std::optional<base::ComponentIndex> position = std::nullopt;
  std::optional<TypeId> type_argument = std::nullopt;
};

// A function of this unit, named outright. The callee is static, so it is
// reached at compile time -- there is no code value in flight and no dispatch.
// A function a class declares as a method takes its receiver as the first
// argument like any other, so naming one is no different from naming a
// function no class declares.
struct FunctionTarget {
  FunctionId function;
};

// Which of the behaviors one class introduces this is, counted in the order
// that class introduces them. Peer of `MemberSlot`: each is an ordinal within
// one declaration, and where it lands in a whole value is a layout question
// answered from the lineage rather than carried here.
struct DispatchOrdinal {
  std::uint32_t value = base::kUnassignedId;

  auto operator<=>(const DispatchOrdinal&) const
      -> std::strong_ordering = default;
};

// One behavior a value answers, named the way a member is: the declaration that
// introduced it, and which of that declaration's introductions it is. Every
// class extending the introducer answers the same behavior under the same name,
// which is what lets a call name one without knowing what the value it is made
// on turns out to be -- and lets it name one a class of another compilation
// unit introduced, since naming the introducer is all a call does.
struct StatedDispatchRef {
  TypeId introduced_by;
  DispatchOrdinal ordinal;

  auto operator==(const StatedDispatchRef&) const -> bool = default;
};

// The body the value the call is made on answers one behavior with (LRM 8.20).
// The value is the call's first argument, as it is for a function named
// outright, and which body runs is decided by what that value is rather than by
// anything written here -- which is the whole of what makes the call dynamic. A
// behavior carries one signature in every class that answers it (LRM 8.20), so
// the call states the signature and every body reachable through it answers to
// that one.
struct DispatchTarget {
  StatedDispatchRef method;
};

// The same for a method an interface class declares itself (LRM 8.26.5),
// answered by the table the value's class gives that interface class, found
// among those its class implements.
struct InterfaceDispatchTarget {
  StatedDispatchRef requirement;
};

// A call through a code address the program computed -- the dual of naming a
// function outright. Which body runs is not known where the call is written, so
// the callee arrives as an operand, and the signature the call is made against
// is the machine function type that operand carries.
struct IndirectTarget {
  Operand callee;
};

// The one way the value's type comes into existence, which is the whole
// identity -- and that type is what the call answers with, so the target names
// nothing further. A shared pointer begins the storage it owns along with
// itself; the owner of storage a new-expression allocated takes over that
// storage, which already holds its value.
struct ConstructTarget {};

// Creates a value of the class `cls` whole in storage of its own, which the
// call answers: the part the runtime library defines, every member of every
// class of its lineage at the value its type starts with, and its class, once
// -- as the JVM's `new` gives an object all its fields and its class before
// any constructor runs. No code of the design runs, so nothing departs from
// it. `cls` is a class of this unit or one another unit declares, and the
// unit defining it is where its size is known.
struct CreationTarget {
  TypeId cls;
};

// A body of this program reached by the symbol it is emitted under rather than
// as a function of this unit: one another compilation unit emits, or one of
// this unit's own namespace that other units reach the same way. The host
// resolves the name: a link line for an ahead-of-time image, the execution
// session for a JIT one. The signature is the call's own, so the target carries
// nothing but the name -- which the caller composes from what it already knows
// the callee by, and which is therefore the whole of what the two sides have to
// agree on. It is the design's own code, so a departure can come out of it.
struct SymbolTarget {
  std::string symbol;
};

// A DPI-C import's foreign symbol (LRM 35.4): a body another language's
// compiler emits, resolved by name the way a symbol target is. What separates
// the two is the frame the call enters, which here is one no departure crosses
// (LRM 35.9). It is still unwound through where the run ends while an exported
// task beneath it waits, because the stack it stands on is released by
// unwinding, and it runs nothing of the other language's on the way.
struct ForeignTarget {
  std::string symbol;
};

// An operation on a value cell -- storage that holds a value, written through
// itself so a write lands at the representation the declaration gave it, and
// read by answering with the value where it lies. `kAllocate` asks for one the
// running activation owns, which is what a value the body holds across a
// suspension -- a call's completion -- needs to outlive it.
// A LIR-only target with no MIR twin -- where a value's storage sits is a
// below-MIR realization the C++ backend never sees -- so a backend realizes it
// the way it realizes any call: the value domain the op works in names the
// runtime entry.
struct ValueCellTarget {
  enum class Op : std::uint8_t { kAllocate, kLoad, kStore };
  Op op;
  TypeId value;
};

// An operation on what a write in progress designates -- the write, or a part
// designated within it. `kLand` is where the write lands: the part designated,
// whose value from before the write the write keeps where anything will ask
// whether the write changed the variable (LRM 4.3); it answers with where the
// part lies. `kAssignElementSlice` writes a run of the elements of the array
// designated within the write (LRM 7.4.6, 7.6), from where it starts and how
// many it takes, telling it what moved; `kAssignQueueSlice` does the same for
// a queue's slice, between the two positions the queue clamps (LRM 7.10.1).
// `value` is what the part or the container holds, and the domain it is
// realized in names the runtime entry.
// A LIR-only target with no MIR twin: MIR states the landing as a dereference
// and the slice write as the container's own slice write on the designation,
// and a place is named by an address here, so each is a call on the
// designation.
struct OpenWriteTarget {
  enum class Op : std::uint8_t {
    kLand,
    kAssignElementSlice,
    kAssignQueueSlice
  };
  Op op;
  TypeId value;
};

// One part of an integral value: where it starts within what it is a part of,
// counted from the least significant bit, and the type it is written at,
// which fixes how many bits it has. A start naming no position names no
// part (LRM 11.5.1).
struct BitsPart {
  Operand start;
  TypeId type;
};

// Writes bits of the integral value a write in progress designates (LRM
// 11.5.1), where they lie in that value, keeping what they held and telling
// the variable which of them moved (LRM 4.3); what it costs is the bits and
// never the value they lie in. The call's operands are the designation and the
// bits written. `value` is the type of the value designated, and `parts` names
// the bits as a store of bits at a place names them: outermost first, the
// first a part of the value, each one after it a part of the one before, and
// the last the part written, whose type is the type of the bits.
// A LIR-only target with no MIR twin: MIR states the write as `set` through
// the reference `slice_mut` answers on the designation.
struct DesignatedBitsTarget {
  TypeId value;
  std::vector<BitsPart> parts;
};

// The end of an owned value, and a second owned value equal to one the body
// only reads. A value the body made ends where the lowering that made it says
// -- at the end of the full-expression that made it, at the end of the block a
// declaration bound it in, and on every way out of either. A copy is what a
// body takes when a value it has to own, to store in a slot or hand back to its
// caller, is one it was lent.
//
// LIR-only targets with no MIR twin: a target whose own language ends a value
// at the end of its scope states none of this.
struct EndValueTarget {};

struct CopyValueTarget {};

// Storage of dynamic duration for one value: allocating it, which answers a
// borrowed pointer to storage holding no value yet, and giving back storage so
// allocated while it holds none, as C++'s allocation and deallocation functions
// do ([basic.stc.dynamic]). What is allocated is the pointee of the pointer
// answered or given back. A new-expression allocates, initializes what the
// storage is to hold, and only then makes the pointer owning it; one whose
// initialization departs gives the storage back instead.
//
// A LIR-only target with no MIR twin: MIR states the new-expression whole.
struct DynamicStorageTarget {
  enum class Op : std::uint8_t { kAllocate, kDeallocate };
  Op op;
};

// An operation on the control effect that leaves an execution (LRM 9.6.2, 9.7).
// The effect crosses as the target it names, since naming one is all an effect
// does, and a departure naming none is one no region may claim.
//
// `kTakeDepartureIfDue` is where a body regains control and leaves if one is
// owed: it is a call that can be unwound out of rather than a question, so what
// a body carries at such a point is the same whether or not anything encloses
// it. `kDeclineDeparture` is the way out of a region's handler where the region
// did not claim what it received: it raises the same departure again, toward
// whatever takes it outside. A LIR-only target with no MIR twin: what a body
// states is the region and the cleanup, never how a departure travels between
// them.
struct ControlEffectTarget {
  enum class Op : std::uint8_t { kTakeDepartureIfDue, kDeclineDeparture };
  Op op;
};

// The protocol by which a body entered as a coroutine is run and answers.
//
// `kEnter` makes an execution the engine drives out of a body's own frame,
// which holds everything the body reads: a receiver it borrows, or the box of a
// closure it took over and ends when it ends.
//
// `kAwait` hands one such execution the thread its awaiter is carrying and
// answers whether the awaiter must park at all, since an execution that
// consumes no time settles before it can be waited for. `kRelease` gives the
// thread back and ends the awaited execution. Neither names the execution once
// it is handed over: the thread carries one at a time, so the runtime knows
// which without being told.
//
// The value an execution completes with (LRM 13.4, 13.5.2) travels without any
// operation of its own: the awaiting body allocates the storage and hands it
// over as the call's last argument, so the completing body writes exactly where
// its awaiter will read.
//
// A LIR-only target with no MIR twin: a target that can enter the body where
// the call sits does so with an ordinary call, and every operation here exists
// only where the runtime runs the body on the program's behalf.
struct CoroutineTarget {
  enum class Op : std::uint8_t { kEnter, kAwait, kRelease };
  Op op;
};

// The operation's stable spelling. This is an interface contract, not a display
// string: it names the operation in a dump, and it is the operation half of the
// runtime-library symbol a generated module calls, so changing it renames a
// linked symbol. Change it only to correct the operation's identity, never to
// improve how a dump reads.
auto ValueCellOpName(ValueCellTarget::Op op) -> std::string_view;
auto OpenWriteOpName(OpenWriteTarget::Op op) -> std::string_view;
auto ControlEffectOpName(ControlEffectTarget::Op op) -> std::string_view;
auto DynamicStorageOpName(DynamicStorageTarget::Op op) -> std::string_view;
auto CoroutineOpName(CoroutineTarget::Op op) -> std::string_view;

// The target of a call: a runtime builtin, a function of this unit, a dispatch
// slot the receiving value's own class fills, a code address the program
// computed, a value constructor named by the call's result type, the creation
// of an object, a body of this program or a foreign symbol the host resolves, a
// value-cell operation, the end or copy of an owned value, storage of dynamic
// duration allocated or given back, a control-effect operation, or an
// operation of the coroutine protocol.
using CallTarget = std::variant<
    BuiltinTarget, FunctionTarget, DispatchTarget, InterfaceDispatchTarget,
    IndirectTarget, ConstructTarget, CreationTarget, SymbolTarget,
    ForeignTarget, ValueCellTarget, OpenWriteTarget, DesignatedBitsTarget,
    EndValueTarget, CopyValueTarget, DynamicStorageTarget, ControlEffectTarget,
    CoroutineTarget>;

// How a call to `target` ends, which is a property of the callee and never of
// what it happens to do. The design's own code can depart, wherever it stands
// and whatever artifact holds it, because a `disable` anywhere inside it
// reaches every execution it encloses (LRM 9.6.2). Runtime code can be unwound
// out of wherever it can raise, since a run-time error is a departure too; a
// runtime entry answers for itself, and one that only moves memory raises
// nothing. Foreign code no departure comes out of is unwound through all the
// same where the stack it waits on is released. A call that can be unwound out
// of names the landing it reaches, so it is a terminator.
auto CallEndingOf(const CallTarget& target) -> support::CallEnding;

struct CallInstr {
  CallTarget target;
  std::vector<Operand> args;
};

// Builds a product value -- a tuple or a structure -- from its components, in
// position order. Its result is the product itself; the components keep their
// own types.
struct ProductInstr {
  std::vector<Operand> components;
};

// Builds a callable value from its captures, in the order its declaration lists
// them. Its result is that value, whose type names the declaration. Each
// capture is taken the way its storage holds one -- a copy, or the pointer it
// was handed -- and none of the design's code runs, so the build only returns.
struct ClosureInstr {
  std::vector<Operand> captures;
};

// Collects `elements` into contiguous storage and names it by a
// {pointer, length} span. The result is that storage, not a container built
// from it: a container the span feeds is a separate construction, so this
// instruction is the same operation whichever one consumes it.
struct ArrayInstr {
  std::vector<Operand> elements;
};

// Builds a union holding member `member` as `value`, every other byte of its
// storage zero, and, for a tagged union, the tag naming that member (LRM 7.3,
// 7.3.2): the peer of a product's build for a value whose type overlays its
// members, as rustc's MIR builds a union or an enum's variant by an aggregate
// naming the field or the variant. Its result is the union.
struct UnionInstr {
  base::ComponentIndex member;
  Operand value;
};

// One element of a sequence of handles, at the position the program computes.
// The element is built out of the sequence, not aliased: a part that is
// storage of its own is a step of a place instead.
struct VectorGetInstr {
  Operand vector;
  Operand index;
};

// Where one declaration put one of its own members: the member's stable
// position in that declaration's own member list, carried over from the MIR
// field it lowers from. It is meaningless alone -- slot 0 of one declaration is
// unrelated to slot 0 of another -- which is why every use of one names the
// declaration beside it. Never a physical index or byte offset.
struct MemberSlot {
  std::uint32_t value = base::kUnassignedId;

  auto operator<=>(const MemberSlot&) const -> std::strong_ordering = default;
};

// Reaches the storage a reference-like value refers to. A projection chain may
// only cross a pointer through this step: a member step never implicitly
// dereferences, so `self.counter` -- whose receiver arrives as a pointer -- is
// the chain `deref, member`, never `member` alone.
struct DerefProjection {};

// Which member an access names: the declaration that declares it, and the slot
// that declaration gave it. Both halves are needed because a class extending
// another carries its base's members as well as its own and may declare one of
// the same name, so a slot alone does not say which storage is meant -- and
// which of the two an access means is fixed where the access is written, never
// by the object it runs on (LRM 8.14).
struct StatedMemberRef {
  TypeId declared_by;
  MemberSlot slot;

  auto operator==(const StatedMemberRef&) const -> bool = default;
};

// Selects a member of whatever the projection has reached so far -- an
// instance's own storage, or the captures a closure value holds. What the chain
// arrived at has to carry the member's declaration.
struct MemberProjection {
  StatedMemberRef member;
};

// Selects one element of whatever container the projection has reached, by the
// coordinate the program computed -- an ordinal, or the index an associative
// array is keyed by. Only a container that holds each element as storage of
// its own is stepped into this way (LRM 7.4, 7.8, 7.10); an element of a
// sequence of handles is taken out of it by value instead. Which element the
// step reaches depends on the access: a read of one that is not there reads
// the default, and a write to one allocates or discards it by the container's
// own rule, so the access, not the step, says which.
struct ElementProjection {
  std::vector<Operand> coordinates;
};

// Selects one component of whatever product or union the projection has
// reached, by its position. A product's component is storage of its own (LRM
// 7.2); a union's member lies at the start of the union's storage, and where
// the union is untagged its bits may be another member's, so a load through it
// reads the value its type holds out of them.
struct ComponentProjection {
  base::ComponentIndex index;
};

// One step of a place's projection chain: each names storage reached from the
// storage the chain has arrived at, never a byte offset from it.
using Projection = std::variant<
    DerefProjection, MemberProjection, ElementProjection, ComponentProjection>;

// Storage named by logical identity: a base plus a projection chain. The base
// is either a place local, whose storage the chain starts at, or a
// reference-like value, which the chain must open with a dereference. A place
// is what a load, store, or address-of names; the physical address it resolves
// to is derived below LIR, never encoded here. An empty chain names the base
// local itself.
//
// A load names the value the place holds, where it lies, and a store writes
// into the storage the place names: an element or a component takes the value
// into the object already there.
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

// Reads the value of the place's type that the bits at `place` stand for,
// where another member of an untagged union may have written them (LRM
// 7.3.2): a value of its own, with every position its type keeps clear made
// clear, as a bit-field's load is its bits made a value of the field's type.
// The result's type is the place's type.
struct OverlaidLoadInstr {
  Place place;
};

// The tag of the tagged union at `place`, as a machine integer, as rustc's MIR
// reads an enum's discriminant before it projects a variant.
struct DiscriminantInstr {
  Place place;
};

// Writes `value` into the value `place` holds, which goes on being that value.
struct StoreInstr {
  Place place;
  Operand value;
};

// `operand` put where the assignment puts it, as rustc's MIR writes
// `Rvalue::Use`: an owned value moved, any other copied. Put in storage holding
// no value yet -- a frame slot, or storage of dynamic duration just allocated
// -- it initializes that storage, and is ended where that storage's value is.
struct UseInstr {
  Operand operand;
};

// Writes `bits` over some bits of the integral value held at `place`, where
// they lie, leaving every other bit of that value as it stands (LRM 11.5.1);
// what it costs is the bits and never the value they lie in. `parts` names the
// bits, outermost first: the first is a part of the value, each one after it a
// part of the one before, and the last is the part written, whose type is the
// type of `bits`. A part reaches nothing outside the part it is taken in, and
// one that names no position is not written.
//
// The bits ride on the statement rather than on the place, because a place is
// storage a load, a store and an address-of can each name, and some bits of a
// value are none of those alone.
struct BitsStoreInstr {
  Place place;
  std::vector<BitsPart> parts;
  Operand bits;
};

// Where the bytes a frame keeps for the value `of` come into use and go out of
// it, which is apart from the value they hold being made or ended: a value
// handed on leaves the bytes it was made in behind, and a slot's bytes hold
// one value after another. A slot's bytes come into use where the lowering
// says; a made value's, with the instruction that makes it, which needs no
// statement. Either goes out of use where the lowering says, on every way out
// of the extent it was introduced in, unwinding included -- and where a call
// that would have made the value is unwound out of instead, in that call's
// landing. A value handed on takes its bytes out of use with it, which is what
// a return does with the value it returns. How long a frame's bytes are needed
// is all this states; none of them is a value too large for a frame, which MIR
// keeps in a box.
//
// A LIR-only statement with no MIR twin: a target whose own language places
// its values states none of this. It is the pair rustc's MIR states as
// `StorageLive` and `StorageDead`, and is read as that pair is: storage that
// comes into use while in use begins afresh, and storage that goes out of use
// twice has gone out of use.
struct StorageInstr {
  enum class Op : std::uint8_t { kLive, kDead };
  Op op;
  ValueId of;
};

auto StorageOpName(StorageInstr::Op op) -> std::string_view;

// Names the address of `place`. The result is a borrowed pointer to the place's
// type. This is how storage itself -- a cell, an aggregate, a member the callee
// mutates -- is handed to a callee, as opposed to a copy of its contents.
struct AddrOfInstr {
  Place place;
};

// Applies an operator to values. The operator's semantics come from the operand
// type: the same `kAdd` is a machine add over a machine integer and an add
// that propagates X over a four-state integral value.
struct BinaryInstr {
  BinaryOp op;
  Operand lhs;
  Operand rhs;
};

struct UnaryInstr {
  UnaryOp op;
  Operand operand;
};

// The operand read as the type this instruction's result has. Both types are
// already carried -- the operand's own is what the value comes from, the
// result's is what it goes to -- so nothing beside them says which cast this
// is, and a target that emits different machine code for different pairs reads
// the pair. MIR's verification admits only pairs every target writes in its own
// conversion syntax; two types sharing a machine representation convert by
// emitting nothing.
struct CastInstr {
  Operand operand;
};

// Takes what is unwinding out of the call whose landing this is, answering it
// as the machine pointer the unwinder carries; nothing is handled yet. A block
// beginning with this is a landing, which is the whole of what makes it one: it
// is reached only as some call's landing, and what arrives is not an operand
// anyone passed.
//
// `clause` says whether this frame takes the raise on the way out of that call
// -- a region of the frame encloses it, or the frame is an execution's top --
// or only runs what is owed and lets it go on, which is what decides where the
// platform's search for a taker stops. It is read off the same nesting that
// builds the way out, as clang reads the clauses of a landing pad off its scope
// stack.
struct LandingInstr {
  enum class Clause : std::uint8_t { kCatchAll, kCleanup };
  Clause clause;
};

// Which of a body's declared variables: the position the body's statement of
// its variables gave it. Meaningless outside that body.
struct VariablePosition {
  std::uint32_t value = base::kUnassignedId;

  auto operator<=>(const VariablePosition&) const
      -> std::strong_ordering = default;
};

// The storage a body's declared variables live in, reserved where the body
// begins and lasting the whole body, as clang makes a local's storage in the
// entry block. Its result is the storage, which the others name.
//
// A declaration is what gives a variable storage, so none of these reads
// anything about what the body does with one. What each piece of storage is
// follows from the type the body stated at the position, the same way a
// declaration's storage follows its type everywhere else. Where the storage
// sits is the target's to choose; a target whose own language gives a
// declaration storage is handed none of this.
struct VariableStorageInstr {};

// A variable exists from its declaration to the end of the block declaring it
// (LRM 6.21), as a C automatic variable does: it is begun each time its
// declaration is reached, which yields a borrowed pointer to its storage, and
// ended on every way out of that block. Between an end and the next begin the
// position holds nothing.
struct BeginVariableInstr {
  Operand variables;
  VariablePosition position;
};

struct EndVariableInstr {
  Operand variables;
  VariablePosition position;
};

// The extent in which a handler of this frame handles `exception`, what a
// landing took: begun where the handler starts, before it asks the runtime what
// arrived, and ended on every way out of the handler -- falling off its end,
// and being unwound out of it by a raise of its own or by what it asked about.
// Every begin is matched by one end, so the platform's record of what is being
// handled is left as the frame found it, as clang begins a catch and pushes
// the end of it as a cleanup of the handler.
struct BeginHandlingInstr {
  Operand exception;
};

struct EndHandlingInstr {};

// What computes a value, as rustc's MIR names the right-hand side of an
// assignment: every instruction that yields one, a call to a callee answering
// nothing included, whose value is the void one.
using Rvalue = std::variant<
    CallInstr, ProductInstr, UnionInstr, ClosureInstr, ArrayInstr,
    VectorGetInstr, DiscriminantInstr, LoadInstr, OverlaidLoadInstr,
    AddrOfInstr, BinaryInstr, UnaryInstr, CastInstr, LandingInstr,
    VariableStorageInstr, BeginVariableInstr, UseInstr>;

// A value computed once and consumed where it is used, with a pure dataflow
// origin and no storage of its own to name.
struct Transient {
  ValueId value;
};

// Where a computed value goes, as rustc's MIR states the place an assignment
// writes and a call's `destination`: a transient it defines, or storage
// holding nothing yet that it initializes and whose holder then owns it. Which
// one is always stated, never worked out from how the value is used.
using Destination = std::variant<Transient, Place>;

// A value computed and put where `destination` says.
struct AssignInstr {
  Destination destination;
  Rvalue value;
};

// One step of a block: an assignment, or a statement that yields nothing --
// a write into a value already there, a frame's bytes coming into or going out
// of use, a variable ending, or a handler's extent beginning or ending.
using InstrData = std::variant<
    AssignInstr, StoreInstr, BitsStoreInstr, StorageInstr, EndVariableInstr,
    BeginHandlingInstr, EndHandlingInstr>;

struct Instr {
  InstrData data;
};

// Returns from the callable, whose answer, where it has one, is already in its
// return place. Whether this is a coroutine completion or a plain return is the
// callable's result type, not a property of the terminator.
struct ReturnTerm {};

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

// One value a switch transfers on, and where it transfers.
struct SwitchTarget {
  std::int64_t value = 0;
  BlockId target;
};

// Transfers to the target of the value the machine integer `discriminant`
// holds, or to `otherwise` where no target names it, as rustc's `SwitchInt`
// and LLVM's `switch` do. No value appears twice.
struct SwitchTerm {
  Operand discriminant;
  std::vector<SwitchTarget> targets;
  BlockId otherwise;
};

// Hands control back to the scheduler and names both ways control can leave.
// It schedules nothing and names no wakeup source: the source is registered by
// the runtime calls that precede this terminator, so a delay, an event
// control, and a level wait differ only in those calls, never in the suspend.
//
// `resume` continues the body where it left off. `abandoned` is where control
// goes when whoever drives the execution ends it instead of running it again;
// that is a way out of every scope the body has open, so it runs what those
// scopes owe. No statement of the body runs there, because being ended is not
// something the body observes.
struct SuspendTerm {
  BlockId resume;
  BlockId abandoned;
};

// Ends a body that was left where it stood rather than run to its end. Control
// reaches it only from a suspension's abandonment edge, once what the open
// scopes owed has run. An execution ended this way settles no outcome, so it
// carries no value and is not a completion.
struct AbandonTerm {};

// Completes the execution this body is the top of with `departure`, the one its
// own handler received once what every scope owed on the way out had run: the
// other way an execution completes beside returning (LRM 9.6.2, 20.2), handed
// to the execution the way a C++ coroutine hands what escaped its body to its
// promise. It is an operand rather than whatever the unwinder brought, because
// a run-time error was received as the departure no region may claim, and that
// is the one the execution settles. Only a body entered as a coroutine is the
// top of an execution.
struct DepartTerm {
  Operand departure;
};

// Goes on unwinding out of the body with `exception`, what a landing took,
// once what every scope owed on the way out has run and nothing in this frame
// took it: the way a body is left by a raise it does not handle, as LLVM's
// `resume` and rustc's `UnwindResume` are.
struct UnwindResumeTerm {
  Operand exception;
};

// Ends a block control never reaches -- the join of a conditional whose arms
// all returned, or the tail of a value-returning body that always returns
// earlier. Reaching it is undefined, which is what lets a target drop the
// block.
struct UnreachableTerm {};

// A call whose callee can be unwound out of instead of returning (LRM 9.6.2,
// 9.7): the result continues at `returned`, and unwinding transfers to
// `landing`, where what is unwinding arrives rather than being passed as an
// operand -- LLVM's `invoke`, and a call naming its unwind action in rustc's
// MIR.
//
// It is a terminator and not an instruction because control leaves the block
// through it either way, which is what a terminator means here; both of this
// IR's peers say it the same way. Only a callee that can be unwound out of
// takes this form, which is every callee but one declared not to; a call to
// one that cannot stays an instruction. Where its value goes is stated as an
// assignment's is, as a call in rustc's MIR names its `destination`.
struct InvokeTerm {
  Destination destination;
  CallTarget target;
  std::vector<Operand> args;
  BlockId returned;
  BlockId landing;
};

using TerminatorData = std::variant<
    ReturnTerm, BranchTerm, CondBranchTerm, SwitchTerm, SuspendTerm,
    AbandonTerm, DepartTerm, UnwindResumeTerm, UnreachableTerm, InvokeTerm>;

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
  std::vector<TypeId> variables;
  TypeId result_type;
  // The storage the answer is put in, as rustc's return place: a place local
  // every way out initializes before it returns, whose value the caller then
  // owns. None where the callable answers nothing, and for a coroutine, which
  // completes into storage it is handed.
  std::optional<ValueId> return_place;
  std::vector<BasicBlock> blocks;
  Linkage linkage = Linkage::kExternal;
};

// The type of an operand: the type of the value a use names, of a constant, or
// of the reference itself, which is an address.
auto OperandType(const Function& fn, const Operand& operand) -> TypeId;

}  // namespace lyra::lir

// A `ValueId` is a value identity, so it keys hashed containers directly rather
// than being unwrapped to its raw integer at the use site.
template <>
struct std::hash<lyra::lir::ValueId> {
  auto operator()(lyra::lir::ValueId id) const noexcept -> std::size_t {
    return std::hash<std::uint32_t>{}(id.value);
  }
};
