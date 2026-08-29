#pragma once

#include <cstdint>
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

 private:
  // The branch targets a `break` and a `continue` inside one loop transfer to.
  // A labeled loop is also the target of a labeled break from a nested loop.
  struct LoopTargets {
    std::optional<mir::LoopLabelId> label;
    lir::BlockId continue_target{};
    lir::BlockId break_target{};
  };

  // What a source local resolves to. A local is frame storage exactly when the
  // canonical lowering needs an address for it: when its address is taken, or
  // when it is assigned after its initialization. Otherwise it stays the value
  // it was bound to, with no storage. A value-typed local in a suspending body
  // is an activation-frame value instead: its value crosses suspensions, so it
  // lives in the running activation's frame, reached through a handle read and
  // written by activation-frame calls.
  struct PlaceBinding {
    lir::ValueId slot;
  };
  struct ValueBinding {
    lir::Operand value;
  };
  struct ActivationValueBinding {
    lir::Operand handle;
  };
  using LocalBinding =
      std::variant<PlaceBinding, ValueBinding, ActivationValueBinding>;

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
  // Reduces an expression to the machine boolean a conditional branch tests.
  auto LowerCondition(const mir::Block& block, mir::ExprId id)
      -> diag::Result<lir::Operand>;

  auto LowerCall(
      const mir::Block& block, const mir::CallExpr& call, mir::TypeId type)
      -> diag::Result<lir::Operand>;
  // A reference is the address of the storage it binds; binding one, and
  // reading or writing through one, are the address-of, load, and store over
  // the referent's own place.
  auto LowerReferenceBind(
      const mir::Block& block, const mir::CallExpr& call, mir::TypeId type)
      -> diag::Result<lir::Operand>;
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
  // The conditional operator over a three-valued predicate, which a two-way
  // branch cannot express: a predicate that is neither definitely true nor
  // definitely false evaluates both arms and combines them, so each arm is
  // evaluated under its own guard and the three outcomes meet at one result.
  auto LowerMergingConditional(
      const mir::Block& block, const mir::MergingConditionalExpr& cond,
      mir::TypeId type) -> diag::Result<lir::Operand>;
  // A join of packed runs, which reaches the machine as a chain of two-run
  // joins because no entry takes an operand list of arbitrary length.
  auto LowerConcat(
      const mir::Block& block, const mir::ConcatExpr& concat, mir::TypeId type)
      -> diag::Result<lir::Operand>;

  auto Emit(lir::TypeId type, lir::InstrData data) -> lir::Operand;
  auto NewPlaceLocal(lir::TypeId type) -> lir::ValueId;
  void BindLocal(mir::LocalId local, lir::TypeId type, lir::Operand init);
  auto Load(lir::Place place, lir::TypeId type) -> lir::Operand;
  auto Store(lir::Place place, lir::Operand value) -> lir::Operand;
  // A count an entry takes as a plain machine scalar rather than as a
  // simulation value.
  auto MachineCount(std::uint64_t count) -> lir::Operand;

  // Activation-frame value operations, emitted for a value-typed local in a
  // suspending body. `AllocateActivationValue` builds the cell (uninitialized
  // -- the first `StoreActivationValue` installs its representation) and
  // returns its handle; `LoadActivationValue` copies the current value out;
  // `StoreActivationValue` overwrites it. The handle is typed as the cell's
  // value type -- both cross the boundary as one opaque handle -- so the value
  // domain an activation-frame call works in is read from that type.
  auto AllocateActivationValue(lir::TypeId value_type) -> lir::Operand;
  auto LoadActivationValue(lir::Operand handle, lir::TypeId value_type)
      -> lir::Operand;
  auto StoreActivationValue(lir::Operand handle, lir::Operand value)
      -> lir::Operand;
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
  // Which locals the body writes through or addresses, which are
  // activation-frame values (a value-typed local in a suspending body), and
  // what each local has resolved to so far.
  std::vector<bool> placed_;
  std::vector<bool> activation_value_local_;
  std::vector<std::optional<LocalBinding>> locals_;
};

}  // namespace lyra::lowering::mir_to_lir
