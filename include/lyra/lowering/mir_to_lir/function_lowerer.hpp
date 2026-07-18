#pragma once

#include <optional>
#include <string>
#include <variant>
#include <vector>

#include "lyra/diag/diagnostic.hpp"
#include "lyra/lir/class_id.hpp"
#include "lyra/lir/function.hpp"
#include "lyra/lir/type_id.hpp"
#include "lyra/lowering/mir_to_lir/unit_lowerer.hpp"
#include "lyra/mir/callable_code.hpp"
#include "lyra/mir/expr_id.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/support/builtin_fn.hpp"

namespace lyra::lowering::mir_to_lir {

// Lowers one MIR callable body (a method's code or a class constructor) into a
// LIR function. Structured control flow becomes a CFG, and expression trees
// become instruction streams: each MIR expression node becomes one LIR
// instruction defining a temporary, and the parent reads the child temporaries
// as operands. A source-level place -- a member, an automatic local -- becomes
// a LIR place, and the use decides whether it is read, written, or addressed.
class FunctionLowerer {
 public:
  FunctionLowerer(
      UnitLowerer& unit, const mir::CallableCode& code, std::string name,
      lir::ClassId current_class);

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
  auto LowerAssign(const mir::Block& block, const mir::AssignExpr& assign)
      -> diag::Result<lir::Operand>;
  // A write to one component of a product value (`s.f = x`). The chain of
  // component projections is rebuilt bottom-up as pure functional-update values
  // over the product's current whole value, then the rebuilt whole value is
  // stored back through the product's storage owner -- so value semantics hold
  // and, when the owner is an observable cell, the whole-cell update fires.
  auto LowerComponentAssign(
      const mir::Block& block, const mir::AssignExpr& assign)
      -> diag::Result<lir::Operand>;
  // A write to one element of a value container (`arr[i] = x`). The container
  // is a value reached by an opaque handle, so the element write is a
  // functional whole-value update: read the whole container, produce a new one
  // with the element replaced, store it back through the container's owner --
  // the runtime-library counterpart of `LowerComponentAssign`'s static value
  // instructions, for a dynamic, runtime-indexed selector.
  auto LowerElementAssign(
      const mir::Block& block, const mir::AssignExpr& assign)
      -> diag::Result<lir::Operand>;
  // A receiver-mutating value-container method (`arr.delete()`). The container
  // value cannot be mutated in place through a shared handle, so the method is
  // a functional operation whose result is stored back through the receiver's
  // owner, the same whole-value read / update / write as an element write.
  auto LowerMutatingCall(
      const mir::Block& block, const mir::CallExpr& call, support::BuiltinFn fn)
      -> diag::Result<lir::Operand>;
  // Reads / writes the whole value of a product-write chain's root. The root is
  // an ordinary place, or the mutate proxy of an observable cell -- reached by
  // its whole-cell get / set so a component write never bypasses the cell's
  // update semantics.
  auto ReadWholeValue(const mir::Block& block, mir::ExprId id)
      -> diag::Result<lir::Operand>;
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
  void Store(lir::Place place, lir::Operand value);

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
  void StoreActivationValue(lir::Operand handle, lir::Operand value);
  // The activation-frame handle an assignable expression writes through, when
  // it names an activation-frame value local directly; nothing otherwise (a
  // place is written the ordinary way).
  auto ActivationValueHandleForTarget(const mir::Block& block, mir::ExprId id)
      -> std::optional<lir::Operand>;

  auto NewBlock() -> lir::BlockId;
  void SetCurrent(lir::BlockId id);
  void Terminate(lir::TerminatorData data);
  [[nodiscard]] auto Terminated() const -> bool;

  UnitLowerer* unit_;
  const mir::CallableCode* code_;
  std::string name_;
  lir::ClassId current_class_;
  lir::Function fn_;
  lir::BlockId current_{};
  std::vector<bool> terminated_;
  std::vector<LoopTargets> loops_;
  // Which locals the body writes through or addresses, which are
  // activation-frame values (a value-typed local in a suspending body), and
  // what each local has resolved to so far.
  std::vector<bool> placed_;
  std::vector<bool> activation_value_local_;
  std::vector<std::optional<LocalBinding>> locals_;
};

}  // namespace lyra::lowering::mir_to_lir
