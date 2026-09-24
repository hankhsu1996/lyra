#pragma once

#include <functional>
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
#include "lyra/mir/class.hpp"
#include "lyra/mir/closure_decl.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/expr_id.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/mir/value_build.hpp"
#include "lyra/support/builtin_fn.hpp"

namespace lyra::lowering::mir_to_lir {

// Lowers one MIR callable body -- a method, a class constructor, or a closure's
// invoke -- into a LIR function. Structured control flow becomes a CFG, and
// expression trees become instruction streams: each MIR expression node becomes
// one LIR instruction defining a temporary, and the parent reads the child
// temporaries as operands. A source-level place -- a member, an automatic local
// -- becomes a LIR place, and the use decides whether it is read, written, or
// addressed.
class FunctionLowerer {
 public:
  FunctionLowerer(
      UnitLowerer& unit, const mir::CallableCode& code, std::string name);
  // Lowers a class's constructor. The base is constructed before the body runs
  // (LRM 8.7), and what base that is belongs to the class rather than to the
  // body, so the class comes with the code it constructs from.
  FunctionLowerer(UnitLowerer& unit, const mir::Class& cls, std::string name);
  // Lowers a closure's invoke. Its signature leads with the receiver naming the
  // storage the captures live in, and the body reads each of them as a member
  // of it.
  FunctionLowerer(
      UnitLowerer& unit, const mir::ClosureDecl& closure, std::string name);
  auto Run() -> diag::Result<lir::Function>;

  // Lowers one value the unit holds -- a type's description, a constant the
  // source wrote. Building a value at this layer is an instruction sequence, so
  // what carries one is a nullary function whose whole content is that
  // expression and a return.
  static auto LowerValueBuild(
      UnitLowerer& unit, const mir::ValueBuild& build, std::string name)
      -> diag::Result<lir::Function>;

 private:
  FunctionLowerer(
      UnitLowerer& unit, const mir::ValueBuild& build, std::string name);
  auto RunValueBuild() -> diag::Result<lir::Function>;

  // The branch targets a `break` and a `continue` inside one loop transfer to.
  // A labeled loop is also the target of a labeled break from a nested loop.
  // `cleanup_depth` is how many cleanups were owed where the loop began, so a
  // branch out of it runs exactly the ones it leaves.
  struct LoopTargets {
    std::optional<mir::LoopLabelId> label;
    lir::BlockId continue_target{};
    lir::BlockId break_target{};
    std::size_t cleanup_depth{};
  };

  // A cleanup owed on every way out of the body it guards, and the block whose
  // child scopes hold it. It is lowered afresh at each way out, because a CFG
  // reaches an extent's end by as many edges as there are ways to leave it.
  struct PendingCleanup {
    const mir::Block* owner = nullptr;
    mir::BlockId cleanup{};
  };

  // Where a control effect leaving a region's body lands: the block that runs
  // the region's handler, the storage the effect is bound to for it to read,
  // and how many cleanups were owed where the region began.
  struct RegionTargets {
    lir::BlockId handler{};
    lir::ValueId caught{};
    std::size_t cleanup_depth{};
  };

  // Where a source local's storage is. A local whose type the runtime holds
  // values of gets storage of its own among the body's variables, and the
  // binding names it by the address the body opened over that storage -- which
  // is the one storage a reference can bind and the one that outlives the
  // stretch that wrote it. A local whose type is stable as it stands -- a
  // pointer, a code reference, a machine scalar -- is a slot of the body's own
  // frame.
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
  // How the constructor of the class `cls` names is entered, or nothing where
  // there is no body to enter -- a base the runtime library defines comes into
  // existence with the object and is entered by whatever builds it.
  auto ConstructorOf(const mir::ClassRef& cls)
      -> std::optional<EnteredConstructor>;

  // The type an object of the class `cls` names is opened as, which is how
  // every consumer below names a class: a member's declarer, a dispatch
  // position's introducer, and the record a class's objects carry.
  auto ObjectTypeOf(const mir::ClassRef& cls) -> diag::Result<lir::TypeId>;

  // Enters the base's constructor on this same object, ahead of the body (LRM
  // 8.7). The base's members sit ahead of this class's in one shared
  // numbering, so the base initializes its own through the receiver it is
  // handed.
  auto ConstructBase() -> diag::Result<void>;

  auto LowerBlockInto(const mir::Block& block) -> diag::Result<void>;
  auto LowerStmtInto(const mir::Block& block, const mir::Stmt& stmt)
      -> diag::Result<void>;
  auto LowerIfInto(const mir::Block& block, const mir::IfStmt& stmt)
      -> diag::Result<void>;
  auto LowerForInto(const mir::Block& block, const mir::ForStmt& stmt)
      -> diag::Result<void>;
  auto LowerWhileInto(const mir::Block& block, const mir::WhileStmt& stmt)
      -> diag::Result<void>;
  auto LowerDoWhileInto(const mir::Block& block, const mir::DoWhileStmt& stmt)
      -> diag::Result<void>;
  auto LowerBreakInto(const mir::BreakStmt& stmt) -> diag::Result<void>;
  auto LowerContinueInto() -> diag::Result<void>;
  auto LowerTryInto(const mir::Block& block, const mir::TryStmt& stmt)
      -> diag::Result<void>;
  auto LowerFinallyInto(const mir::Block& block, const mir::FinallyStmt& stmt)
      -> diag::Result<void>;

  // Runs every cleanup owed between here and `depth`, innermost first. A way
  // out of a guarded body runs the cleanups it leaves and no others, so the
  // depth a loop or a region recorded is what bounds it.
  auto RunCleanupsDownTo(std::size_t depth) -> diag::Result<void>;
  // Hands control back to the scheduler, leaving the body at `resume`. Being
  // ended rather than run again is a way out of every scope open here, so the
  // second way out runs all of their cleanups and then ends the body; the
  // source spells none of it, which is why it is built from what is owed
  // rather than from a statement.
  auto SuspendResumingAt(lir::BlockId resume) -> diag::Result<void>;
  // Hands the departure this landing is holding back, to carry on outward: a
  // region that declined it is not where it was going, and what a further
  // landing tests is the target the same departure named. Reached only from a
  // landing, which is the only place one is held.
  auto LeaveCarrying() -> diag::Result<void>;
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
  // Emits a call whose callee may leave by a departure, naming where it lands
  // as well as where it returns. Every such call names one, including where
  // nothing here claims or owes anything: a landing is also how a frame says
  // the departure is passing through it, and a frame that says nothing is one
  // the platform will not look at.
  auto EmitDepartingCall(
      lir::CallTarget target, std::vector<lir::Operand> args,
      lir::TypeId result_type) -> diag::Result<lir::Operand>;
  // Builds the landing this point would use: it receives the departure, runs
  // the cleanups owed between here and whatever claims it, and hands it to the
  // innermost region's own test -- or, where no region encloses, gives it back
  // to carry on outward.
  auto BuildLanding() -> diag::Result<lir::BlockId>;
  auto CurrentRuntime() -> lir::Operand;

  // Reads a value out of an expression. An expression naming storage that has
  // no value of its own -- a cell -- has no reading, and is rejected here.
  auto LowerExpr(const mir::Block& block, mir::ExprId id)
      -> diag::Result<lir::Operand>;
  // The readings of `ids`, in order. An instruction taking a list of them
  // composes these operands; the walk over the list is not each caller's.
  auto LowerEachExpr(const mir::Block& block, std::span<const mir::ExprId> ids)
      -> diag::Result<std::vector<lir::Operand>>;
  // Passes an expression to a callee: a cell crosses as its address, every
  // other value as itself. This is the one use context that addresses a place
  // without an explicit address-of in the source IR.
  auto LowerArgument(const mir::Block& block, mir::ExprId id)
      -> diag::Result<lir::Operand>;
  // Every operand a call carries, in the order the boundary takes them. The
  // object the call dispatches on leads, because that boundary takes what an
  // operation acts on as its first parameter, whichever way the library
  // declares the entry to a call site that writes it out.
  auto LowerCallOperands(const mir::Block& block, const mir::CallExpr& call)
      -> diag::Result<std::vector<lir::Operand>>;
  auto LowerPlace(const mir::Block& block, mir::ExprId id)
      -> diag::Result<lir::Place>;
  // The storage a reference names, or why the referent has none. Whether a
  // referent has storage is the target's own fact, so the place side owns that
  // answer and a use in value position reads it.
  auto ReferencePlace(const mir::ReferenceTarget& target, mir::TypeId type)
      -> diag::Result<lir::Place>;
  // The storage a program-wide symbol names. The symbol carries the cell's
  // address, so the place opens there and dereferences it -- the same shape a
  // member place has once its receiver is resolved.
  auto SymbolPlace(std::string symbol, mir::TypeId type) -> lir::Place;
  // The value naming a referent yields, whether that is the reference itself --
  // a descriptor, a function, a local bound to a value that never had storage
  // -- or what the storage it names holds.
  auto ReferenceValue(
      const mir::Block& block, mir::ExprId id,
      const mir::ReferenceTarget& target, mir::TypeId type)
      -> diag::Result<lir::Operand>;
  // The storage a capability wrapper stands for. A wrapper that is itself
  // storage continues the chain that reached it; one that refers to storage
  // elsewhere opens a new chain at what it refers to. Naming that storage is
  // what a write descending into it and a reference lent over it both ask for,
  // and asking it here once is what keeps the two from disagreeing about where
  // the contents live.
  auto WrapperContentsPlace(const mir::Block& block, mir::ExprId wrapper)
      -> diag::Result<lir::Place>;
  auto MemberRefOf(const mir::FieldRef& field) -> lir::StatedMemberRef;
  // Reads the value held where an expression names storage, whichever way it
  // names it. A cell is address-only and holds no value a reader can take out
  // of it, so an expression whose own type is one is rejected: what such a
  // reader wants is what the cell holds, which is a dereference and names a
  // different place.
  auto ReadPlace(const mir::Block& block, mir::ExprId id, lir::TypeId type)
      -> diag::Result<lir::Operand>;
  // Reduces an expression to the machine boolean a conditional branch tests.
  auto LowerCondition(const mir::Block& block, mir::ExprId id)
      -> diag::Result<lir::Operand>;

  auto LowerCall(
      const mir::Block& block, const mir::CallExpr& call, mir::TypeId type)
      -> diag::Result<lir::Operand>;
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
  // can depart, and nothing at all for one that only departs.
  auto EmitCallTo(
      lir::CallTarget target, std::vector<lir::Operand> args,
      lir::TypeId result_type) -> diag::Result<lir::Operand>;
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
  // Brings an object into existence and enters the construction the program
  // asked for on it (LRM 8.3, 8.7). The heap is the runtime's, so what it
  // answers is an object whose properties hold their storage's default; the
  // body that brings them to their initial values is this program's own and is
  // entered like any other.
  auto LowerObjectConstruction(
      const mir::Block& block, const mir::CallExpr& call, mir::TypeId type)
      -> diag::Result<lir::Operand>;
  // A reference is the address of the storage its referent lives in, carrying
  // which of the two kinds that storage is. One built over a reference denotes
  // the storage at the end of the chain rather than binding afresh, so it
  // carries what it was handed (LRM 23.3.3.2).
  auto LowerReferenceBind(
      const mir::Block& block, const mir::CallExpr& call, mir::TypeId type)
      -> diag::Result<lir::Operand>;
  // The place naming the storage a reference binds. It is the same storage a
  // write to the referent descends into, so what may be lent and what may be
  // written are one question; only a local the body gave storage of its own is
  // reached differently, by the handle the body already holds on it.
  auto LowerCellPlace(const mir::Block& block, mir::ExprId referent)
      -> diag::Result<lir::Place>;
  auto LowerAssign(const mir::Block& block, const mir::AssignExpr& assign)
      -> diag::Result<lir::Operand>;
  // The value a compound assignment stores: its operator applied to the old
  // value and the right-hand side (LRM 11.4.1). An assignment carries only the
  // operators a target applies to two values of one type, so this is the
  // ordinary binary instruction and nothing else.
  auto LowerCompoundOperator(
      mir::BinaryOp op, lir::Operand old_value, lir::Operand rhs,
      lir::TypeId type) -> lir::Operand;
  // A method that changes the object it is applied to. The generated side holds
  // a value as a handle a copy may alias, so the entry answers with the changed
  // object rather than changing one in place, and the answer is put back where
  // the object came from. Where the method also states a result of its own -- a
  // queue pop yields the element it removed (LRM 7.10.2.4) -- the entry
  // completes with both, the changed object first, and each is projected out of
  // that product.
  auto LowerMutatingCall(
      const mir::Block& block, const mir::CallExpr& call, support::BuiltinFn fn,
      mir::TypeId type) -> diag::Result<lir::Operand>;
  // Reading what a target holds, changing it, and putting the result back,
  // reaching the target exactly once. Which kind of storage the target names --
  // a part of a value aggregate, or storage the chain reaches -- is answered
  // here and nowhere else, so no site that changes what a target holds reaches
  // one twice, once to read and once to write. `change` is handed a way to read
  // the old value and the type it has, and answers with what to put back; one
  // that never reads emits no read at all, which is how a plain write reaches
  // this. What this yields is the write, whose type is void; a caller in
  // expression position states the value its own expression has, out of what it
  // kept while `change` ran.
  using ValueReader = std::function<lir::Operand()>;
  using ValueChange = std::function<diag::Result<lir::Operand>(
      const ValueReader&, lir::TypeId)>;
  auto UpdateTarget(
      const mir::Block& block, mir::ExprId target, const ValueChange& change)
      -> diag::Result<lir::Operand>;
  // Updating a target that reaches into a value aggregate: a read of the
  // owner's whole value, a rebuild of it with the part changed, and the change
  // put back through the owner. A value reaches the generated side as a handle
  // a copy may alias, so nothing here has an interior to write.
  auto LowerValuePartUpdate(
      const mir::Block& block, mir::ExprId target, const ValueChange& change)
      -> diag::Result<lir::Operand>;
  // Which subvalue one reaching call names, in the vocabulary this layer's
  // aggregate instructions take.
  auto LowerValuePartSelector(
      const mir::Block& block, const mir::CallExpr& call)
      -> diag::Result<lir::AggregateSelector>;
  auto LowerIncDec(const mir::Block& block, const mir::IncDecExpr& inc_dec)
      -> diag::Result<lir::Operand>;
  auto LowerConditional(
      const mir::Block& block, const mir::ConditionalExpr& cond,
      mir::TypeId type) -> diag::Result<lir::Operand>;

  auto Emit(lir::TypeId type, lir::InstrData data) -> lir::Operand;
  auto Append(lir::TypeId type, lir::InstrData data) -> lir::Operand;
  auto NewPlaceLocal(lir::TypeId type) -> lir::ValueId;
  void BindLocal(mir::LocalId local, lir::TypeId type, lir::Operand init);
  auto Load(lir::Place place, lir::TypeId type) -> lir::Operand;
  auto Store(lir::Place place, lir::Operand value) -> lir::Operand;

  // Reading and writing a slot the activation owns: a load copies the current
  // value out and a store overwrites it, the first store installing the slot's
  // representation. The handle is typed as the slot's value type -- both cross
  // the boundary as one opaque handle -- so each operation states the value the
  // slot holds, which is what names the entry realizing it: a store settles
  // nothing and a handle is opaque, so neither of those carries it.
  auto LoadActivationValue(lir::Operand handle, lir::TypeId value_type)
      -> lir::Operand;
  auto StoreActivationValue(
      lir::Operand handle, lir::Operand value, lir::TypeId value_type)
      -> lir::Operand;
  // A cell this body supplies for something it calls to complete into, built at
  // frame entry so a call in a loop writes into one place rather than leaving a
  // fresh one behind per iteration. The caller supplies it because the caller
  // outlives what it calls; a cell of the callee's own would be gone by the
  // time the caller read it.
  auto AllocateCompletionFor(lir::TypeId payload) -> lir::Operand;

  // Brings the storage this body's variables live in into existence and binds
  // each of them to the address of its own piece of it. Runs once, before
  // anything the body does, because a declaration reached many times is one
  // variable in one storage and only its contents begin afresh.
  void OpenVariables();
  // Ends that storage, and with it every variable in it. Owed on every way out
  // of the body, including the one no statement of it spells.
  void CloseVariables();
  // Installs a cell's representation and initial contents, the one write it
  // takes before it will accept a store.
  auto InitializeCell(lir::Operand cell, lir::Operand value) -> lir::Operand;
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

  // Binds a closure invoke's receiver: the one parameter its signature leads
  // with, naming the storage its captures live in.
  void BindCaptureReceiver(mir::LocalId receiver);

  UnitLowerer* unit_;
  const mir::CallableCode* code_;
  const mir::Class* constructed_class_;
  const mir::ClosureDecl* closure_;
  const mir::ValueBuild* build_;
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
  std::vector<LoopTargets> loops_;
  std::vector<PendingCleanup> cleanups_;
  std::vector<RegionTargets> regions_;
  // Which of the body's variables each local is, where its declared type gives
  // it one, and what each local has resolved to so far.
  std::vector<std::optional<std::uint32_t>> variable_slot_;
  std::vector<std::optional<LocalBinding>> locals_;
  // The storage this body's variables live in, opened at entry and ended on
  // every way out. Absent where the body declares nothing that needs one.
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
};

}  // namespace lyra::lowering::mir_to_lir
