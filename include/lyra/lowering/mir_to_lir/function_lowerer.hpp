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
#include "lyra/mir/packed_type_description.hpp"
#include "lyra/mir/stmt.hpp"
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

  // Lowers one type's description. A description is an expression, not a body:
  // building a value at this layer is an instruction sequence, so what carries
  // one is a nullary function whose whole content is that expression and a
  // return. It is a factory rather than a constructor and a `Run` because
  // nothing else can be done with a description lowerer, and a pairing a caller
  // could get wrong is not one worth offering.
  static auto LowerDescription(
      UnitLowerer& unit, const mir::PackedTypeDescription& description,
      std::string name) -> diag::Result<lir::Function>;

 private:
  FunctionLowerer(
      UnitLowerer& unit, const mir::PackedTypeDescription& description,
      std::string name);
  auto RunDescription() -> diag::Result<lir::Function>;

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

  // What a source local resolves to. A local is frame storage exactly when the
  // canonical lowering needs an address for it: when its address is taken, or
  // when it is assigned after its initialization. Otherwise it stays the value
  // it was bound to, with no storage. A value-typed local in a suspending body
  // is an activation value instead: its value crosses suspensions, so it lives
  // in a cell of the running execution's own store, reached through a handle
  // the cell operations read and write. A local whose storage is lent by
  // reference lives in a cell too, since that is the one storage a reference
  // can name, and the binding holds the reference the lowering built over it.
  struct PlaceBinding {
    lir::ValueId slot;
  };
  struct ValueBinding {
    lir::Operand value;
  };
  struct ActivationValueBinding {
    lir::Operand handle;
  };
  struct CellBinding {
    lir::Operand reference;
  };
  using LocalBinding = std::variant<
      PlaceBinding, ValueBinding, ActivationValueBinding, CellBinding>;

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
  auto LowerRaiseInto(const mir::Block& block, const mir::RaiseStmt& stmt)
      -> diag::Result<void>;

  // Runs every cleanup owed between here and `depth`, innermost first. A way
  // out of a guarded body runs the cleanups it leaves and no others, so the
  // depth a loop or a region recorded is what bounds it.
  auto RunCleanupsDownTo(std::size_t depth) -> diag::Result<void>;
  // Leaves through the innermost region of this frame carrying `effect`, or,
  // where no region encloses this point, settles the activation cancelled and
  // returns. Either way the cleanups the departure passes run first.
  auto LeaveCarrying(lir::Operand effect) -> diag::Result<void>;
  // Where an execution regains control: asks the runtime whether a target it
  // is inside was disabled while it was away, and leaves carrying that effect
  // when one was. The generation comparison is the runtime's; what crosses is
  // its answer, because a simulated process cannot be made to run code partway
  // through a statement of the design.
  //
  // Which targets an execution is inside is the execution's own state, not the
  // body's: an activity spawned or enabled inside a target is enclosed by it
  // (LRM 9.6.2) while its body may state no region at all, so the question is
  // asked wherever control returns rather than only where a body nests one.
  auto CheckDisabledTarget() -> diag::Result<void>;
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
  auto MemberRefOf(const mir::FieldRef& field) -> diag::Result<lir::MemberRef>;
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
  // The result type is stated rather than read off the MIR expression, because
  // an awaitable's is not what MIR gave it.
  auto EmitCall(
      const mir::Block& block, const mir::CallExpr& call,
      std::vector<lir::Operand> args, lir::TypeId result_type)
      -> diag::Result<lir::Operand>;
  // Says what a call reaches. A callee named outright resolves to its identity
  // with nothing evaluated; one that is a code address the program computed
  // resolves by lowering that address, which is why this reaches the block.
  auto LowerCallTarget(const mir::Block& block, const mir::Callee& callee)
      -> diag::Result<lir::CallTarget>;
  // Lowers the call an await is over, which arranges this execution's
  // resumption and answers whether it must park at all: a delay and a
  // value-change wait always must, a join whose condition is already met and a
  // `wait fork` whose children have all terminated must not (LRM 9.3.2,
  // 9.6.1). The answer is a machine predicate the suspend edge branches on --
  // it decides control and never reaches the design's own semantics, so it
  // carries no width and no unknown state. Only a target whose suspension is
  // an explicit edge has to ask it out loud, which is why the value is stated
  // here rather than upstream.
  auto LowerRegistration(const mir::Block& block, const mir::CallExpr& call)
      -> diag::Result<lir::Operand>;
  // Awaits an execution the runtime drives: hands it this one's thread, parks
  // where it did not settle in the same instant, and reads back the value it
  // completed with. What ends this wait is a second body reaching its own end
  // (LRM 13.3), not a wakeup source this one armed, so nothing is registered.
  auto LowerCoroutineAwait(
      const mir::Block& block, const mir::AwaitExpr& await, mir::TypeId type)
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
  // A reference is the address of the cell its referent lives in.
  auto LowerReferenceBind(
      const mir::Block& block, const mir::CallExpr& call, mir::TypeId type)
      -> diag::Result<lir::Operand>;
  // The place naming the cell a referent lives in -- the storage a reference
  // binds. A referent that is itself a cell is that place already; one that is
  // a value has a cell only where the lowering gave it one, which it does for a
  // local whose storage is lent.
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
  // a part of a value aggregate, an activation value the execution's own store
  // keeps across a suspension, a place -- is answered here and nowhere else, so
  // no site that changes what a target holds reaches one twice, once to read
  // and once to write. `change` is handed a way to read the old value and the
  // type it has, and answers with what to put back; one that never reads emits
  // no read at all, which is how a plain write reaches this. What this yields
  // is the write, whose type is void; a caller in expression position states
  // the value its own expression has, out of what it kept while `change` ran.
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
  auto NewPlaceLocal(lir::TypeId type) -> lir::ValueId;
  void BindLocal(mir::LocalId local, lir::TypeId type, lir::Operand init);
  auto Load(lir::Place place, lir::TypeId type) -> lir::Operand;
  auto Store(lir::Place place, lir::Operand value) -> lir::Operand;

  // The cell operations, emitted for a value-typed local in a suspending body.
  // Allocating builds the slot uninitialized, the first store installing its
  // representation; a load copies the current value out and a store overwrites
  // it. The handle is typed as the slot's value type -- both cross the boundary
  // as one opaque handle -- so each operation states the value the slot holds,
  // which is what names the entry realizing it: a store settles nothing and a
  // handle is opaque, so neither of those carries it.
  auto AllocateActivationValue(lir::TypeId value_type) -> lir::Operand;
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

  // The storage a local lent by reference lives in. `AllocateCell` builds the
  // cell and returns the reference to it; `InitializeCell` installs the cell's
  // representation and initial contents, the one write it takes before it will
  // accept a store.
  auto AllocateCell(lir::TypeId value_type) -> lir::Operand;
  auto InitializeCell(lir::Operand reference, lir::Operand value)
      -> lir::Operand;
  // The two places a reference names: opening it reaches the cell it binds, and
  // reaching through that cell names the value. A local whose storage is a cell
  // holds a reference to it, so it names both the same way.
  [[nodiscard]] static auto ReferencedCell(lir::Operand reference)
      -> lir::Place;
  [[nodiscard]] static auto ReferencedValue(lir::Operand reference)
      -> lir::Place;
  // The cell handle an assignable expression writes through, when it names an
  // activation value local directly; nothing otherwise (a place is written the
  // ordinary way).
  auto ActivationValueHandleForTarget(const mir::Block& block, mir::ExprId id)
      -> std::optional<lir::Operand>;

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
  const mir::PackedTypeDescription* description_;
  std::string name_;
  lir::Function fn_;
  // A block while it is being built, which is before its exit is decided. The
  // lowering reaches a block's instructions well before it knows how control
  // leaves it -- and for a block control never reaches, only the end of the
  // body settles it -- so what it accumulates is not yet a basic block and does
  // not claim to be one.
  struct OpenBlock {
    std::vector<lir::Instr> instrs;
    std::optional<lir::Terminator> terminator;
  };

  std::vector<OpenBlock> blocks_;
  lir::BlockId current_{};
  std::vector<LoopTargets> loops_;
  std::vector<PendingCleanup> cleanups_;
  std::vector<RegionTargets> regions_;
  // Where each local's value lives: a frame place the body writes through or
  // addresses, an activation value (a value-typed local in a suspending body),
  // or a lent cell (a local whose storage a reference binds). The last holds
  // what each local has resolved to so far.
  std::vector<bool> placed_;
  std::vector<bool> activation_value_local_;
  std::vector<bool> cell_local_;
  std::vector<std::optional<LocalBinding>> locals_;
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
