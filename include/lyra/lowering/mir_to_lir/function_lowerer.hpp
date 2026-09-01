#pragma once

#include <functional>
#include <optional>
#include <string>
#include <variant>
#include <vector>

#include "lyra/diag/diagnostic.hpp"
#include "lyra/lir/function.hpp"
#include "lyra/lir/type_id.hpp"
#include "lyra/lowering/mir_to_lir/unit_lowerer.hpp"
#include "lyra/mir/callable_code.hpp"
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
    const mir::Block* owner;
    mir::BlockId cleanup;
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
  // is an activation-frame value instead: its value crosses suspensions, so it
  // lives in the running activation's frame, reached through a handle read and
  // written by activation-frame calls. A local whose storage is lent by
  // reference lives in a cell, since that is the one storage a reference can
  // name, and the binding holds the reference the lowering built over it.
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
  // Passes an expression to a callee: a cell crosses as its address, every
  // other value as itself. This is the one use context that addresses a place
  // without an explicit address-of in the source IR.
  auto LowerArgument(const mir::Block& block, mir::ExprId id)
      -> diag::Result<lir::Operand>;
  auto LowerPlace(const mir::Block& block, mir::ExprId id)
      -> diag::Result<lir::Place>;
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
      const mir::CallExpr& call, std::vector<lir::Operand> args,
      lir::TypeId result_type) -> diag::Result<lir::Operand>;
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
  // Extracts the designated part's current value; called at most once, and only
  // by a leaf transform that needs the old value.
  using LeafReader = std::function<lir::Operand()>;
  // Produces the designated part's new value, given a reader for its current
  // one and the part's type.
  using LeafTransform =
      std::function<diag::Result<lir::Operand>(const LeafReader&, lir::TypeId)>;
  // The shared realization of every write through a designator: read the
  // owner's whole value, descend the path, transform the part, rebuild the
  // whole value outward, store it back. The owner and every coordinate evaluate
  // exactly once, and the store back through the owner is a single one -- for
  // an observable owner, one cell write whatever the path's depth.
  auto LowerProjectionUpdate(
      const mir::Block& block, mir::ExprId target,
      const LeafTransform& make_leaf) -> diag::Result<lir::Operand>;
  // A write through a designated part of an owner's value (`s.f = x`,
  // `arr[i] = x`, `a[i].f = x`). The owner's whole value is read, the path is
  // folded into a functional whole-value update -- a product component a static
  // value instruction, a container element a runtime-library call -- and the
  // rebuilt whole value is stored back through the owner, so value semantics
  // hold and, when the owner is an observable cell, the whole-cell update
  // fires once whatever the path's depth.
  auto LowerProjectionAssign(
      const mir::Block& block, const mir::AssignExpr& assign)
      -> diag::Result<lir::Operand>;
  // A receiver-mutating value-container method (`arr.delete()`). The container
  // value cannot be mutated in place through a shared handle, so the method is
  // a functional operation whose result is stored back through the receiver's
  // owner, the same whole-value read / update / write as an element write. That
  // updated container is the entry's one result, so a method that also states a
  // result of its own has no realization in this form.
  auto LowerMutatingCall(
      const mir::Block& block, const mir::CallExpr& call, support::BuiltinFn fn,
      mir::TypeId type) -> diag::Result<lir::Operand>;
  // Stores a whole value back through the place that owns it -- the root of a
  // designator's chain, or a mutating method's receiver -- so the update goes
  // through that place's own store rather than reaching past it. What it yields
  // is the write, whose type is void; a caller in expression position states
  // the value its own expression has.
  auto WriteWholeValue(
      const mir::Block& block, mir::ExprId id, lir::Operand value)
      -> diag::Result<lir::Operand>;
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

  // Activation-frame value operations, emitted for a value-typed local in a
  // suspending body. `AllocateActivationValue` builds the slot (uninitialized
  // -- the first `StoreActivationValue` installs its representation) and
  // returns its handle; `LoadActivationValue` copies the current value out;
  // `StoreActivationValue` overwrites it. The handle is typed as the slot's
  // value type -- both cross the boundary as one opaque handle -- so the value
  // domain an activation-frame call works in is read from that type.
  auto AllocateActivationValue(lir::TypeId value_type) -> lir::Operand;
  auto LoadActivationValue(lir::Operand handle, lir::TypeId value_type)
      -> lir::Operand;
  auto StoreActivationValue(lir::Operand handle, lir::Operand value)
      -> lir::Operand;

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
  // The activation-frame handle an assignable expression writes through, when
  // it names an activation-frame value local directly; nothing otherwise (a
  // place is written the ordinary way).
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
  // addresses, an activation-frame value (a value-typed local in a suspending
  // body), or a cell (a local whose storage is lent by reference). The last
  // holds what each local has resolved to so far.
  std::vector<bool> placed_;
  std::vector<bool> activation_value_local_;
  std::vector<bool> cell_local_;
  std::vector<std::optional<LocalBinding>> locals_;
};

}  // namespace lyra::lowering::mir_to_lir
