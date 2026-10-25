#pragma once

#include <cstdint>
#include <optional>
#include <span>
#include <string>
#include <variant>
#include <vector>

#include "lyra/diag/diagnostic.hpp"
#include "lyra/lir/function.hpp"
#include "lyra/lir/type_id.hpp"
#include "lyra/lowering/mir_to_lir/unit_lowerer.hpp"
#include "lyra/mir/callable_code.hpp"
#include "lyra/mir/closure_decl.hpp"
#include "lyra/mir/declared_class.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/expr_id.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/support/builtin_fn.hpp"

namespace lyra::lowering::mir_to_lir {

// Lowers one MIR callable body -- a method, a class constructor, or a closure's
// invoke -- into a LIR function. Structured control flow becomes a CFG, and
// expression trees become instruction streams: each MIR expression node becomes
// one LIR assignment putting its value where its destination says -- a
// transient the parent reads as an operand, or storage it is made in. A
// source-level place -- a member, an automatic local
// -- becomes a LIR place, and the use decides whether it is read, written, or
// addressed.
class FunctionLowerer {
 public:
  FunctionLowerer(
      UnitLowerer& unit, const mir::CallableCode& code, std::string name);
  // Lowers a closure's invoke. Its signature leads with the receiver naming the
  // storage the captures live in, and the body reads each of them as a member
  // of it.
  FunctionLowerer(
      UnitLowerer& unit, const mir::ClosureDecl& closure, std::string name);
  auto Run() -> diag::Result<lir::Function>;

 private:
  // The branch targets of one statement a `break` leaves: a loop, which a
  // `continue` inside it also transfers to the next pass of, or a labeled
  // block, which it does not. A labeled statement is the target of a break
  // naming it from any depth. `scope_depth` is how many scopes were open where
  // the statement began, so a branch out of it runs the cleanups of exactly the
  // ones it leaves.
  struct BreakableScope {
    std::optional<mir::LabelId> label;
    std::optional<lir::BlockId> continue_target;
    lir::BlockId break_target{};
    std::size_t scope_depth{};
  };

  // What is owed on every way out of the extent that incurred it: a cleanup a
  // guarded body states, with the block whose child scopes hold it; the end of
  // an owned value the extent made; and the end of the owned value a frame slot
  // holds. A value whose end passed to a slot, or to the caller, leaves nothing
  // where its own end stood.
  struct CleanupScope {
    const mir::Block* owner = nullptr;
    mir::BlockId cleanup{};
  };
  struct ValueEnd {
    lir::ValueId value;
  };
  struct SlotEnd {
    lir::ValueId slot;
  };
  // A variable among the body's own, which ends with the scope declaring it
  // while its storage lasts the whole body.
  struct VariableEnd {
    lir::VariablePosition position;
  };
  struct EndPassedOn {};
  // Storage a new-expression allocated, while what it is to hold is still
  // being initialized there: a way out gives the storage back and ends
  // nothing, since nothing whole lies in it yet, as clang's new-expression
  // calls `operator delete` when a constructor throws.
  struct StorageRelease {
    lir::Operand storage;
  };

  // A region that takes what unwinds into it: the block that runs its handler,
  // and the storage the handler binds what it received to. The top of a body
  // entered as a coroutine is one too, around the whole body, as a C++
  // coroutine's body is a `try` whose handler hands what escaped to its
  // promise.
  struct RegionScope {
    lir::BlockId handler{};
    lir::ValueId caught{};
  };

  // Stands over a handler while it handles what it took, and ends the
  // handling on every way out of it, being unwound out of it included.
  struct HandlingScope {};

  // Stands over a cleanup's own code while it is lowered. A cleanup runs on
  // every way out of its body, a departure included, so a departure starting
  // inside one would have nowhere to go, and nothing under this scope may ask
  // for a landing.
  struct TerminateScope {};

  // A scope unwinding meets on its way out of the body, kept on one stack in
  // the order they nest, which is the order unwinding meets them.
  // `unwind_entry` is where unwinding continues once it reaches this scope --
  // what the scope owes, run once and passed outward, or the region's handler
  // -- built the first time something needs it and shared by every landing and
  // every scope nested inside after that.
  using ScopeKind = std::variant<
      CleanupScope, ValueEnd, SlotEnd, VariableEnd, EndPassedOn, StorageRelease,
      RegionScope, HandlingScope, TerminateScope>;
  struct UnwindScope {
    ScopeKind kind;
    std::optional<lir::BlockId> unwind_entry;
  };

  // Where a source local's storage is. A local whose type the runtime holds
  // values of gets storage of its own among the body's variables, and the
  // binding names it by its address there, taken where its declaration begins
  // it -- which is the one storage a reference can bind and the one that
  // outlives a suspension of the body. A local whose type is stable as it
  // stands -- an integral value, a pointer, a code reference, a machine scalar
  // -- is a slot of the body's own frame.
  //
  // Nothing about what the body does with the local is consulted. The two
  // follow from the declared type alone, so a local's storage is settled where
  // the declaration is read.
  struct PlaceBinding {
    lir::ValueId slot;
  };
  struct CellBinding {
    lir::Operand cell;
  };
  using LocalBinding = std::variant<PlaceBinding, CellBinding>;

  // What entering one class's constructor takes: the type the object it runs on
  // is opened as, and the callee. Both are answered from the class's identity
  // alone, so a class this unit compiles and one another unit declares differ
  // in where each answer is read and in nothing after.
  struct EnteredConstructor {
    lir::TypeId object_type;
    lir::CallTarget callee;
  };
  auto ConstructorOf(const mir::DeclaredClassRef& cls) -> EnteredConstructor;

  // Enters the constructor of `cls` on the object the storage `object` holds,
  // already created whole, with `arguments` after the object -- which is how a
  // value the program builds and an instance of the design hierarchy alike are
  // constructed.
  auto EnterConstructor(
      const mir::DeclaredClassRef& cls, const lir::Operand& object,
      std::vector<lir::Operand> arguments) -> diag::Result<void>;

  // A block is an extent: what its declarations bind ends where it does. Its
  // statements run in order.
  auto LowerBlockInto(const mir::Block& block) -> diag::Result<void>;
  auto LowerStatementsInto(const mir::Block& block) -> diag::Result<void>;
  // A block expression's extent ends once the value it yields is made, as
  // rustc builds a block's tail into its destination before popping its
  // scopes, so that value is carried out of the extent first: one only lent
  // is copied, and the end owed for one it made is owed by the extent around
  // it.
  auto LowerBlockExpr(const mir::Block& scope, mir::ExprId value)
      -> diag::Result<lir::Operand>;
  auto LowerBlockExprInto(
      const mir::Block& scope, mir::ExprId value,
      const lir::Destination& destination) -> diag::Result<void>;
  // Closes the extent above `depth` once a block expression's value `yielded`
  // is made. Where that extent owes the value's end, the extent around it owes
  // it from here.
  auto CloseExtentYielding(std::size_t depth, const lir::Operand& yielded)
      -> diag::Result<void>;
  auto LowerStmtInto(const mir::Block& block, const mir::Stmt& stmt)
      -> diag::Result<void>;
  // An expression a statement evaluates for itself, which is a full-expression:
  // every value made while evaluating it ends when it is done (C++
  // [class.temporary]; Rust's temporary scopes). What it yields is read by
  // nothing.
  auto LowerDiscardedInto(const mir::Block& block, mir::ExprId id)
      -> diag::Result<void>;
  // A declaration: its initializer is a full-expression, and the value it
  // settles is the local's -- copied into the variable the local begins, or
  // taken by the frame slot the local is; either ends with the enclosing
  // block.
  auto DeclareLocal(
      const mir::Block& block, mir::LocalId local, mir::ExprId init)
      -> diag::Result<void>;
  // Ends every value owed above `depth` that no way out has already ended, and
  // forgets them. A block that control left by a statement of its own has had
  // them ended on that way out, and a way out never returns here.
  auto CloseExtent(std::size_t depth) -> diag::Result<void>;
  // `value` handed on, the end this body owed for it going with it. Only a
  // value this body owns is handed on.
  auto HandOn(lir::Operand value) -> lir::Operand;
  // What the scope at `index` owed is owed no longer, from here on: its value
  // went to whoever holds it now, or the storage it was to give back holds
  // what it was allocated for.
  void PassEndAt(std::size_t index);
  // `value` as what an assignment puts where it goes, as rustc's
  // `consume_by_copy_or_move`: one this body made is moved, and one it was only
  // lent is copied, so what is put there is a value of its own.
  auto ConsumeByCopyOrMove(lir::Operand value) -> lir::Rvalue;
  // `value` put where `destination` says, consumed; one moved out leaves the
  // bytes it was made in behind.
  void AssignFrom(lir::Destination destination, lir::Operand value);
  // The scope owing the end of `value`, where this body made it and still owes
  // that end, and nothing otherwise.
  [[nodiscard]] auto OwedEndAt(const lir::Operand& value) const
      -> std::optional<std::size_t>;
  // Whether `value` is one this body was only lent and can copy: owned by its
  // type, with a copy, and with no end owed for it here.
  [[nodiscard]] auto IsLentCopyable(const lir::Operand& value) const -> bool;
  // States the end of an owned value, and of the one a frame slot holds.
  void EndValue(lir::Operand value);
  void EndSlotValue(lir::ValueId slot);
  // States that the bytes the frame keeps for `of` come into use, or go out
  // of it.
  void MarkStorage(lir::StorageInstr::Op op, lir::ValueId of);
  // What a scope owes where a way out runs it: a value's end and its bytes
  // going out of use, a variable's end, or storage given back.
  void EmitScopeDrop(const ValueEnd& end);
  void EmitScopeDrop(const SlotEnd& end);
  void EmitScopeDrop(const VariableEnd& end);
  void EmitScopeDrop(const StorageRelease& release);
  auto LowerIfInto(const mir::Block& block, const mir::IfStmt& stmt)
      -> diag::Result<void>;
  auto LowerSwitchInto(const mir::Block& block, const mir::SwitchStmt& stmt)
      -> diag::Result<void>;
  auto LowerBlockStmtInto(const mir::Block& block, const mir::BlockStmt& stmt)
      -> diag::Result<void>;
  auto LowerForInto(const mir::Block& block, const mir::ForStmt& stmt)
      -> diag::Result<void>;
  auto LowerWhileInto(const mir::Block& block, const mir::WhileStmt& stmt)
      -> diag::Result<void>;
  auto LowerDoWhileInto(const mir::Block& block, const mir::DoWhileStmt& stmt)
      -> diag::Result<void>;
  auto LowerBreakInto(const mir::BreakStmt& stmt) -> diag::Result<void>;
  auto LowerContinueInto() -> diag::Result<void>;
  auto LowerReturnInto(const mir::Block& block, const mir::ReturnStmt& stmt)
      -> diag::Result<void>;
  auto LowerTryInto(const mir::Block& block, const mir::TryStmt& stmt)
      -> diag::Result<void>;
  auto LowerFinallyInto(const mir::Block& block, const mir::FinallyStmt& stmt)
      -> diag::Result<void>;

  // Opens a scope a departure meets on its way out, innermost.
  void OpenScope(ScopeKind kind);
  // Runs the cleanup of every scope open between here and `depth`, innermost
  // first. A way out of a guarded body runs the cleanups it leaves and no
  // others, so the depth a loop recorded is what bounds it.
  auto RunCleanupsDownTo(std::size_t depth) -> diag::Result<void>;
  // Lowers a cleanup's own code, on whichever way out reached it, under a
  // scope that refuses any landing inside it.
  auto LowerCleanupInto(CleanupScope cleanup) -> diag::Result<void>;
  // Hands control back to the scheduler where `park` holds, the body going on
  // in one block either way: where it suspended and is resumed, and where it
  // did not have to. Being ended rather than run again is a way out of every
  // scope open here, so the second way out runs all of their cleanups and then
  // ends the body; the source spells none of it, which is why it is built from
  // what is owed rather than from a statement.
  auto SuspendIf(lir::Operand park) -> diag::Result<void>;
  // Carries `effect`, the departure a handler received, on outward: a region
  // that declined it is not where it was going, and what a further handler
  // tests is the target it names. Reached only from a handler, which is the
  // only place one is held.
  auto LeaveCarrying(lir::Operand effect) -> diag::Result<void>;
  // Where an execution regains control: asks the runtime whether it has been
  // told to stop -- a target it is inside was disabled while it was away, or
  // its own termination is owed -- and leaves carrying the effect that names
  // whichever it was. The comparison is the runtime's; what crosses is its
  // answer, because a simulated process cannot be made to run code partway
  // through a statement of the design.
  //
  // Which targets an execution is inside is the execution's own state, not the
  // body's: an activity spawned or enabled inside a target is enclosed by it
  // (LRM 9.6.2) while its body may state no region at all, so the question is
  // asked wherever control returns rather than only where a body nests one.
  auto TakeDepartureIfDue() -> diag::Result<void>;
  // Emits a call whose callee can be unwound out of, naming where unwinding
  // lands as well as where it returns. Every such call names one, including
  // where nothing here takes or owes anything: a landing is also how a frame
  // says unwinding is passing through it. What the call answers goes where
  // `destination` says.
  auto EmitInvoke(
      lir::CallTarget target, std::vector<lir::Operand> args,
      lir::Destination destination) -> diag::Result<void>;
  // The landing the call about to be made here unwinds to: it takes what is
  // unwinding into the frame's one slot for it and continues where the
  // innermost open scope takes it, saying whether this frame takes the raise on
  // that way out. Each call has a landing of its own, because what a target
  // owes on the way out of one call -- a temporary it made to pass an argument
  // -- is that call's alone; what the scopes owe is built once and shared.
  // `unmade` is the value the call would have made, where it makes one whose
  // bytes a frame keeps.
  auto LandingHere(std::optional<lir::ValueId> unmade)
      -> diag::Result<lir::BlockId>;
  // Where unwinding continues once it reaches the scope at `index` on the
  // stack, or the frame's own edge below every scope: the cleanup run and
  // passed on outward, the region's handler, or -- at the edge -- the
  // unwinding carried out of the frame. Each is built once, and every landing
  // and every scope nested inside reaches it.
  auto UnwindEntryAt(std::size_t index) -> diag::Result<lir::BlockId>;
  // The entry of the scope at `index`, built where what is owed outside it is
  // reached at `outward`.
  auto BuildUnwindEntry(std::size_t index, lir::BlockId outward)
      -> diag::Result<lir::BlockId>;
  auto FrameEdge() -> lir::BlockId;
  // Opens a region's handler in the block its scope named: the handling of
  // what the landing took begins, an extent that ends it on every way out is
  // opened, and the runtime is asked what arrived, into the region's `caught`.
  // The handler's own code follows, and closing the extent ends the handling.
  auto OpenHandler(const RegionScope& region) -> diag::Result<void>;
  // The handler of the region a coroutine's whole body stands in: what arrived
  // completes the execution, as a departure.
  auto EmitCoroutineHandler(const RegionScope& top) -> diag::Result<void>;
  // The frame storage a landing puts what is unwinding in and every handler
  // and every way out by unwinding reads it from. One serves the whole frame,
  // because it is read only on the way from a landing to where that way ends,
  // and nothing on it can be unwound out of, so no other landing writes it in
  // between -- as clang keeps one exception slot per function.
  auto ExceptionSlot() -> lir::ValueId;
  // What a landing takes: the pointer the platform's unwinder carries, to an
  // object no code here names the type of.
  auto UnwindingType() -> lir::TypeId;
  auto CurrentRuntime() -> lir::Operand;

  // Reads a value out of an expression, as rustc's `as_operand`. An expression
  // naming storage that has no value of its own -- a cell -- has no reading,
  // and is rejected here.
  auto LowerExpr(const mir::Block& block, mir::ExprId id)
      -> diag::Result<lir::Operand>;
  // What computes an expression's value, as rustc's `as_rvalue`: the one step
  // that makes it, or the value read or made elsewhere, consumed.
  struct TypedRvalue {
    lir::TypeId type;
    lir::Rvalue value;
  };
  auto LowerRvalue(const mir::Block& block, mir::ExprId id)
      -> diag::Result<TypedRvalue>;
  // The value `operand` makes, read as a value of `type`: the one value seen
  // another way, so one with no copy goes on to the new type with its end.
  auto LowerReadAsAnother(
      const mir::Block& block, mir::ExprId operand, mir::TypeId type)
      -> diag::Result<lir::Operand>;
  // An expression's value put where `destination` says, as rustc's
  // `expr_into_dest`: a call and the step that makes a value make it there.
  auto LowerExprInto(
      const mir::Block& block, mir::ExprId id,
      const lir::Destination& destination) -> diag::Result<void>;
  // The readings of `ids`, in order. An instruction taking a list of them
  // composes these operands; the walk over the list is not each caller's.
  auto LowerEachExpr(const mir::Block& block, std::span<const mir::ExprId> ids)
      -> diag::Result<std::vector<lir::Operand>>;
  // Passes an expression to a callee: a cell crosses as its address, every
  // other value as itself. This is the one use context that addresses a place
  // without an explicit address-of in the source IR.
  auto LowerArgument(const mir::Block& block, mir::ExprId id)
      -> diag::Result<lir::Operand>;
  // Where a place holding a value of `type` is, reached the way a write reaches
  // it: an element a write makes by being written is made here, where a read
  // would find none.
  auto AddressOf(lir::Place place, lir::TypeId type) -> lir::Operand;
  // Every operand a call carries, in the order the boundary takes them. The
  // object the call dispatches on leads, because that boundary takes what an
  // operation acts on as its first parameter, whichever way the library
  // declares the entry to a call site that writes it out.
  auto LowerCallOperands(const mir::Block& block, const mir::CallExpr& call)
      -> diag::Result<std::vector<lir::Operand>>;
  // What an access does to the place it names: reads it, writes it whole, or
  // writes into a part of it. Only the last asks anything of the storage: a
  // write into what a subscribable cell holds is made through a write opened
  // on the cell, which tells the cell once what the write did, while reading
  // it and replacing it whole are the cell's own operations.
  enum class Reach : std::uint8_t { kRead, kWhole, kInto };
  // What reaching a part asks of the value holding it: reading a part reads
  // the holder, and writing a part, whole or in part, writes into the holder.
  static auto HolderReach(Reach part) -> Reach;
  // What binding a reference asks of its referent: one read through reads it,
  // and one written through reaches the whole of it.
  static auto BorrowReach(mir::Mutability mutability) -> Reach;
  // Whether the access writes into what the storage holds, which is what a
  // cell reporting its writes has to be opened for.
  static auto WritesInto(Reach reach) -> bool;
  auto LowerPlace(const mir::Block& block, mir::ExprId id, Reach reach)
      -> diag::Result<lir::Place>;
  // The storage a reference names, or why the referent has none. Whether a
  // referent has storage is the target's own fact, so the place side owns that
  // answer and a use in value position reads it.
  auto ReferencePlace(
      const mir::ReferenceTarget& target, mir::TypeId type, Reach reach)
      -> diag::Result<lir::Place>;
  // The storage a program-wide symbol names. The symbol carries the cell's
  // address, so the place opens there and dereferences it -- the same shape a
  // member place has once its receiver is resolved.
  auto SymbolPlace(std::string symbol, mir::TypeId type) -> lir::Place;
  // The value a name of something that is no storage stands for: a member
  // table, a constant, a function's address.
  auto ReferenceValue(const mir::ReferenceTarget& target, mir::TypeId type)
      -> diag::Result<lir::Operand>;
  // The storage a capability wrapper stands for. A wrapper that is itself
  // storage continues the chain that reached it; one that refers to storage
  // elsewhere opens a new chain at what it refers to. Naming that storage is
  // what a write descending into it and a reference lent over it both ask for,
  // and asking it here once is what keeps the two from disagreeing about where
  // the contents live.
  auto WrapperContentsPlace(const mir::Block& block, mir::ExprId wrapper)
      -> diag::Result<lir::Place>;
  // The word a wrapper that refers elsewhere holds, as rustc's `as_operand`
  // makes an operand of either kind of expression. MIR names the wrapper as a
  // value it read, or as the place it lies in where a member is reached on it
  // or an owner is reached through, as C++ enters `operator*` on the object.
  auto WrapperOperand(const mir::Block& block, mir::ExprId wrapper)
      -> diag::Result<lir::Operand>;
  // Opens a write into what a wrapper stands for -- an object held for the rest
  // of the full-expression, whose end tells the wrapper what the write did --
  // and answers with the whole of what the wrapper holds, designated within
  // it. `handle` reaches the wrapper, and `value` is the type of what it holds.
  auto OpenWrite(lir::Operand handle, lir::TypeId value)
      -> diag::Result<lir::Operand>;
  // Where a write lands: the part `designation` names, whose value from before
  // the write the write keeps (LRM 4.3), named as the storage a store writes
  // into. `value` is what the part holds.
  auto Land(lir::Operand designation, lir::TypeId value)
      -> diag::Result<lir::Place>;
  // The address of storage holding a value of `value`.
  auto AddressType(lir::TypeId value) -> lir::TypeId;
  // The place the element `element` reaches lies in: the place holding its
  // receiver, then the step to the element, the receiver evaluated before the
  // coordinates. Reaching the element for any write is writing into that
  // place.
  auto ElementPlace(
      const mir::Block& block, const mir::CallExpr& element, Reach reach)
      -> diag::Result<lir::Place>;
  // The step a call reaching a part takes into the value it is taken on.
  auto PartStep(const mir::Block& block, const mir::CallExpr& part)
      -> diag::Result<lir::Projection>;
  // The place the value `id` evaluates to is reached in. An expression naming
  // storage names its place; any other value is held in a slot of this frame
  // for the rest of the full-expression, which is then its place.
  auto PlaceHolding(const mir::Block& block, mir::ExprId id, Reach reach)
      -> diag::Result<lir::Place>;
  // A slot of this frame holding `value` for the rest of the full-expression.
  auto HeldInSlot(lir::Operand value, lir::TypeId type) -> lir::ValueId;
  // The step `field` takes off the place of its receiver: a member an object
  // or a closure declares, or a component where its product's type lays it.
  auto FieldStep(const mir::FieldRef& field) -> lir::Projection;
  // The value `place` holds, read where it lies: the one way a place stands
  // as a value, under a copy and a move alike. A cell is address-only and holds
  // no value a reader can take out of it, so a place whose own type is one is
  // rejected: what such a reader wants is what the cell holds, which is a
  // dereference and names a different place.
  auto LowerRead(const mir::Block& block, mir::ExprId place)
      -> diag::Result<lir::Operand>;
  // Reduces an expression to the machine boolean a conditional branch tests.
  auto LowerCondition(const mir::Block& block, mir::ExprId id)
      -> diag::Result<lir::Operand>;

  // A call, what it answers put where `destination` says.
  auto LowerCallInto(
      const mir::Block& block, const mir::CallExpr& call, mir::TypeId type,
      const lir::Destination& destination) -> diag::Result<void>;
  // Resolves the callee and emits the call over arguments already lowered.
  // The result type is passed rather than read off the MIR expression, because
  // a caller partway through building a compound shape has the type in hand
  // and no expression to read it from.
  auto EmitCall(
      const mir::Block& block, const mir::CallExpr& call,
      std::vector<lir::Operand> args, lir::TypeId result_type)
      -> diag::Result<lir::Operand>;
  // Emits a call to a resolved callee, followed by what its ending leaves: the
  // statement after it for one that returns, a landing as well for one that
  // can be unwound out of, and nothing at all for one that only unwinds.
  auto EmitCallTo(
      lir::CallTarget target, std::vector<lir::Operand> args,
      lir::TypeId result_type) -> diag::Result<lir::Operand>;
  // The same, with what the call answers put where `destination` says, as a
  // call in rustc's MIR names its `destination`.
  auto CallInto(
      lir::CallTarget target, std::vector<lir::Operand> args,
      lir::Destination destination) -> diag::Result<void>;
  // Says what a call reaches. A callee named outright resolves to its identity
  // with nothing evaluated; one that is a code address the program computed
  // resolves by lowering that address, which is why this reaches the block.
  auto LowerCallTarget(const mir::Block& block, const mir::Callee& callee)
      -> diag::Result<lir::CallTarget>;
  // Awaits an execution the runtime drives: hands it this one's thread, parks
  // where it did not settle in the same instant, and reads back the value it
  // completed with. What ends this wait is a second body reaching its own end
  // (LRM 13.3), not a wakeup source this one armed, so nothing is registered.
  auto LowerCoroutineAwait(
      const mir::Block& block, const mir::AwaitExpr& await, mir::TypeId type)
      -> diag::Result<lir::Operand>;
  // Drives an execution to its end where it stands, rather than waiting for it:
  // the frame asking is a foreign one that cannot be parked, so the runtime
  // carries it until the body is done (LRM 35.8). The value the body completes
  // with arrives the way an awaited one does, in storage this frame supplies.
  auto LowerDriveToCompletion(
      const mir::Block& block, const mir::CallExpr& call, mir::TypeId type)
      -> diag::Result<lir::Operand>;
  // Builds a coroutine body's frame and makes an execution of it. `completion`
  // is where that execution writes the value it finishes with, and is absent
  // exactly when it finishes with no value.
  auto EnterCoroutine(
      const mir::Block& block, const mir::CallExpr& call, mir::TypeId type,
      std::optional<lir::Operand> completion) -> diag::Result<lir::Operand>;
  // Allocates the storage a new-expression owns, initializes it from the
  // initializer, and hands it to `owner`, the type that owns it from there
  // (LRM 8.3, 8.7).
  auto LowerNew(
      const mir::Block& block, const mir::NewExpr& allocation,
      mir::TypeId owner) -> diag::Result<lir::Operand>;
  // A new-expression of an object: its construction's arguments, then the
  // object created whole and held by `owner`, then its constructor entered on
  // it (LRM 8.7, 8.29).
  auto LowerObjectNew(
      const mir::Block& block, const mir::Expr& initializer, mir::TypeId owner)
      -> diag::Result<lir::Operand>;
  // A reference is the address of the storage its referent lives in, carrying
  // which of the two kinds that storage is. One built over a reference denotes
  // the storage at the end of the chain rather than binding afresh, so it
  // carries what it was handed (LRM 23.3.3.2).
  auto LowerReferenceBind(
      const mir::Block& block, const mir::CallExpr& call, mir::TypeId type)
      -> diag::Result<lir::Operand>;
  // A reference into the receiver of `call` -- an element read or written,
  // as rustc's `Index::index` and `IndexMut::index_mut` answer one -- is the
  // address of the part where it lies, taken at the reference's own
  // mutability, which is what asks the step for a read or for a write.
  auto LowerReferenceIntoReceiver(
      const mir::Block& block, const mir::CallExpr& call, mir::TypeId type)
      -> diag::Result<lir::Operand>;
  // A reference to what a capability wrapper holds: the address of the
  // storage the wrapper represents.
  auto LowerReferenceToContents(
      const mir::Block& block, const mir::CallExpr& call, mir::TypeId type)
      -> diag::Result<lir::Operand>;
  // A reference to a property of an object (LRM 13.5.2), the call naming the
  // property as its part: the runtime entry takes what holds the storage and
  // where the storage is, both of which are the one object reached once.
  auto LowerPropertyReference(
      const mir::Block& block, const mir::CallExpr& call, mir::TypeId type)
      -> diag::Result<lir::Operand>;
  // A write opened on the object its one argument reaches (LRM 8.4, 9.4.2).
  auto LowerObjectWriteOpening(
      const mir::Block& block, const mir::CallExpr& call, mir::TypeId type)
      -> diag::Result<lir::Operand>;
  // The address of the object `reaches` reaches -- `&*reaches` -- whether it
  // is a class handle, a pointer, or a write in progress into the object.
  auto ObjectAddress(const mir::Block& block, mir::ExprId reaches)
      -> diag::Result<lir::Operand>;
  // The place naming the storage a reference binds. It is the same storage a
  // write to the referent descends into, so what may be lent and what may be
  // written are one question; only a local the body gave storage of its own is
  // reached differently, by the handle the body already holds on it.
  auto LowerCellPlace(const mir::Block& block, mir::ExprId referent)
      -> diag::Result<lir::Place>;
  auto LowerAssign(const mir::Block& block, const mir::AssignExpr& assign)
      -> diag::Result<lir::Operand>;
  // A method that changes the object it is applied to. The entry changes the
  // object where it lies, and answers with whatever result of its own the
  // method states -- a queue pop yields the element it removed (LRM 7.10.2.4).
  auto LowerMutatingCall(
      const mir::Block& block, const mir::CallExpr& call, support::BuiltinFn fn,
      mir::TypeId type) -> diag::Result<lir::Operand>;
  // A store of `value` through the reference to bits `reference` (LRM
  // 11.5.1): the `slice_mut` steps it was taken by, followed back to what the
  // first was taken on, and the bits written there.
  auto LowerBitsStore(
      const mir::Block& block, mir::ExprId reference, mir::ExprId value)
      -> diag::Result<lir::Operand>;
  // The parts `steps` name, outermost first: where each starts and the type
  // it is written at. `steps` are the `slice_mut` calls a reference to bits
  // was taken by, innermost first.
  auto LowerBitsParts(
      const mir::Block& block, std::span<const mir::CallExpr* const> steps)
      -> diag::Result<std::vector<lir::BitsPart>>;
  auto LowerConditional(
      const mir::Block& block, const mir::ConditionalExpr& cond,
      mir::TypeId type) -> diag::Result<lir::Operand>;
  auto LowerConditionalInto(
      const mir::Block& block, const mir::ConditionalExpr& cond,
      const lir::Place& destination) -> diag::Result<void>;

  // `value` computed and put where `destination` says, as rustc's
  // `push_assign`; a transient holding an owned value owes its end here.
  void Assign(lir::Destination destination, lir::Rvalue value);
  // The transient an owned value made into `destination` owes its end in, or
  // nothing where the value goes into storage, whose holder owns it.
  [[nodiscard]] auto OwningTransient(
      const lir::Destination& destination, bool makes_value) const
      -> std::optional<lir::ValueId>;
  auto NewTransient(lir::TypeId type) -> lir::ValueId;
  // `value` computed into a new transient of `type`, as rustc's `as_temp`.
  auto Emit(lir::TypeId type, lir::Rvalue value) -> lir::Operand;
  // A step that yields nothing: a write, a frame's bytes coming into or going
  // out of use, a variable ending, or a handler's extent beginning or ending.
  void EmitStatement(lir::InstrData statement);
  // What a step yielding nothing stands as where a value is wanted.
  auto VoidValue() -> lir::Operand;
  auto NewPlaceLocal(lir::TypeId type) -> lir::ValueId;
  // A new frame slot of `type`, its bytes in use from here where it is to own
  // what it holds, not yet holding a value.
  auto NewLiveSlot(lir::TypeId type) -> lir::ValueId;
  // The frame slot a local the body gave no storage of its own becomes, in
  // use from here and not yet holding a value.
  auto OpenLocalSlot(mir::LocalId local, lir::TypeId type) -> lir::ValueId;
  auto Load(lir::Place place, lir::TypeId type) -> lir::Operand;
  auto Store(lir::Place place, lir::Operand value) -> lir::Operand;

  // Reading and writing a slot the activation owns: a load answers with the
  // value where it lies and a store overwrites it, the first store installing
  // the slot's representation. Each operation states the value the slot holds,
  // which is what names the entry realizing it: a store settles nothing and the
  // handle names storage rather than a value, so neither of those carries it.
  auto LoadActivationValue(lir::Operand handle, lir::TypeId value_type)
      -> lir::Operand;
  void StoreActivationValue(
      lir::Operand handle, lir::Operand value, lir::TypeId value_type);
  // A cell this body supplies for something it calls to complete into, built at
  // frame entry so a call in a loop writes into one place rather than leaving a
  // fresh one behind per iteration. The caller supplies it because the caller
  // outlives what it calls; a cell of the callee's own would be gone by the
  // time the caller read it.
  auto AllocateCompletionFor(lir::TypeId payload) -> lir::Operand;

  // Reserves the storage this body's variables live in. Runs once, before
  // anything the body does, because a declaration reached many times is one
  // storage, and only the variable in it begins afresh.
  void ReserveVariableStorage();
  // Begins the variable `local` in its piece of that storage, binds the local
  // to it, and owes its end where the scope declaring it is left -- before its
  // initial value is made, so a way out taken while making it ends it too.
  // Returns the variable's cell.
  auto BeginVariable(mir::LocalId local) -> lir::Operand;
  // Installs a cell's representation and initial contents, the one write it
  // takes before it will accept a store.
  void InitializeCell(lir::Operand cell, lir::Operand value);
  // The place an address opens to, and the place reached by opening that in
  // turn. What the first names is storage, reached through whatever protocol
  // that storage has rather than by loading it; the second is for an address
  // whose storage holds a value of its own, where reaching the value takes the
  // storage's step and then the value's.
  [[nodiscard]] static auto StorageAt(lir::Operand address) -> lir::Place;
  [[nodiscard]] static auto ValueAt(lir::Operand address) -> lir::Place;
  auto NewBlock() -> lir::BlockId;
  void SetCurrent(lir::BlockId id);
  void Terminate(lir::TerminatorData data);
  [[nodiscard]] auto Terminated() const -> bool;

  // Takes one parameter of the body's signature, a closure invoke's receiver
  // included: the argument it is handed. One handed over rather than lent is
  // the body's from here, and ends on every way out of the body.
  auto TakeParam(mir::LocalId param) -> lir::ValueId;
  // Binds a taken parameter's local to its argument, which needs the storage
  // the body's variables live in.
  void BindParam(mir::LocalId param, lir::ValueId argument);
  // A coroutine body stops before its first statement, so that whoever drives
  // it runs every stretch of it, the first included. Ended there, it ends what
  // it was handed and nothing else, since nothing else is open yet.
  auto EmitInitialSuspend() -> diag::Result<void>;

  UnitLowerer* unit_;
  const mir::CallableCode* code_;
  const mir::ClosureDecl* closure_;
  std::string name_;
  lir::Function fn_;
  // A block while it is being built, which is before its exit is decided. The
  // lowering reaches a block's instructions well before it knows how control
  // leaves it -- and for a block control never reaches, only the end of the
  // body settles it -- so what it accumulates is not yet a basic block and does
  // not claim to be one.
  struct OpenBlock {
    lir::BlockId id;
    std::vector<lir::Instr> instrs;
    std::optional<lir::Terminator> terminator;
  };

  std::vector<OpenBlock> blocks_;
  lir::BlockId current_{};
  std::vector<BreakableScope> breakable_scopes_;
  std::vector<UnwindScope> scopes_;
  // Where unwinding goes once it has passed every scope, built the first time
  // one is needed.
  std::optional<lir::BlockId> frame_edge_;
  std::optional<lir::ValueId> exception_slot_;
  // Which of the body's variables each local is, where its declared type gives
  // it one, and what each local has resolved to so far.
  std::vector<std::optional<std::uint32_t>> variable_slot_;
  std::vector<std::optional<LocalBinding>> locals_;
  // The storage this body's variables live in, reserved at entry for the
  // whole body. Absent where the body declares nothing that needs one.
  std::optional<lir::Operand> variables_;
  // Where this body writes the value it finishes with: storage its caller
  // allocated and handed over as the call's last argument, and the type of the
  // value that storage holds. Absent exactly when this body finishes with no
  // value.
  struct CompletionCell {
    lir::Operand cell;
    lir::TypeId type;
  };
  std::optional<CompletionCell> completion_cell_;
  // The block every stretch of the body runs after: the entry block, or for a
  // coroutine the one its first stop resumes into. What the body allocates
  // once for all its calls is allocated there, inside a stretch.
  lir::BlockId stretch_start_;
};

}  // namespace lyra::lowering::mir_to_lir
