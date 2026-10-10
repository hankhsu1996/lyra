#pragma once

#include <cstddef>
#include <cstdint>
#include <optional>
#include <span>
#include <string_view>
#include <unordered_map>
#include <vector>

#include <llvm/ADT/STLFunctionalExtras.h>
#include <llvm/IR/IRBuilder.h>

#include "lyra/backend/llvm/codegen_module.hpp"
#include "lyra/backend/llvm/codegen_tuple.hpp"
#include "lyra/backend/llvm/fn_abi.hpp"
#include "lyra/backend/llvm/runtime_entry.hpp"
#include "lyra/diag/diagnostic.hpp"
#include "lyra/lir/function.hpp"
#include "lyra/lir/type.hpp"
#include "lyra/lir/type_id.hpp"
#include "lyra/support/builtin_fn.hpp"
#include "lyra/support/integral_operation.hpp"
#include "lyra/support/runtime_object.hpp"
#include "lyra/support/value_domain.hpp"

namespace llvm {
class BasicBlock;
class Function;
class FunctionCallee;
class Instruction;
class Value;
}  // namespace llvm

namespace lyra::backend::llvm_backend {

class CodeGenModule;

// Per-function code generation: lowers one LIR callable body into its LLVM
// function. Each LIR value becomes one LLVM value; reads resolve through the
// per-function value map. Everything shared across the module's functions is
// reached through the owning module-level code generation and never held here,
// so a body carries no state that outlives it.
class CodeGenFunction {
 public:
  CodeGenFunction(CodeGenModule& module, lir::FunctionId id);

  auto Run() -> diag::Result<void>;

 private:
  // A call with its callee and its arguments settled, which is everything the
  // two forms of a call share: one continues in place, the other names where a
  // departure lands, and nothing about resolving the callee differs.
  struct ResolvedCall {
    llvm::FunctionCallee callee;
    std::vector<llvm::Value*> args;
  };

  // The LLVM argument each of the body's parameters reads, one for one.
  auto BindParameters() -> void;
  auto LowerInstr(const lir::Instr& instr) -> diag::Result<llvm::Value*>;

  // Storage in the body's own frame, allocated where the body opens: every
  // path reaching an instruction then names the same storage, and a loop
  // reuses it rather than growing the stack on each turn.
  auto FrameStorage(llvm::Type* type) -> llvm::Value*;
  // Frame storage of the size and alignment `layout` states.
  auto LaidOutStorage(support::ObjectLayout layout) -> llvm::Value*;
  // Frame storage for one runtime object, sized and aligned as the library
  // states that object.
  auto ObjectStorage(support::RuntimeObject object) -> llvm::Value*;
  // Where a value of `type` an instruction makes is built: storage for an
  // owned value, and nothing for any other value, which the instruction
  // answers as itself. Every owned value is built in storage its maker gives,
  // as clang builds a class-type result through `sret`, and is ended by the end
  // LIR states for it.
  auto StorageFor(lir::TypeId type) -> llvm::Value*;
  // `count` arguments that are each an address this target computed.
  [[nodiscard]] auto Addresses(std::size_t count) const -> std::vector<ArgAbi>;
  // Calls the library entry `symbol` on this target's own account, handing it
  // `args` as `arranged` says each of what they were built from crosses. The
  // entry answers the machine value `result`, which is what this answers.
  auto CallEntry(
      std::string_view symbol, llvm::Type* result, std::vector<ArgAbi> arranged,
      std::span<llvm::Value* const> args) -> llvm::Value*;
  // The same of an entry that builds its answer in `out`, handed to it last;
  // what the call answers is `out`.
  auto BuildInto(
      std::string_view symbol, std::vector<ArgAbi> arranged,
      std::vector<llvm::Value*> args, llvm::Value* out) -> llvm::Value*;
  // The runtime object an owned value of `type` is. For a product that is the
  // tuple domain the library holds one as, while its lifecycle here is the one
  // compiled for its type.
  [[nodiscard]] auto ObjectOf(lir::TypeId type) const -> support::RuntimeObject;
  // What carries out a lifecycle step on an owned value of `type`: the step
  // compiled for its tuple type, as clang emits a class's implicit special
  // members, or the entry the library defines over its object. An integral
  // value has neither, its bytes being the whole of it.
  auto OwnedCallee(lir::TypeId type, TupleLifecycle step)
      -> llvm::FunctionCallee;
  // The lifecycle of an owned value. `EndValue` ends the value `value` names,
  // where ending one has anything to do. `CopyValue` builds a second value
  // equal to it in the unbuilt storage `out`, and answers `out`.
  // `RelocateValue` moves it into the unbuilt storage `out` and ends what the
  // move left behind, so the value lives in `out` and nowhere else.
  // `AssignValue` writes it into the value already built at `out`, which goes
  // on being that value.
  void EndValue(lir::TypeId type, llvm::Value* value);
  auto CopyValue(lir::TypeId type, llvm::Value* value, llvm::Value* out)
      -> llvm::Value*;
  void RelocateValue(lir::TypeId type, llvm::Value* value, llvm::Value* out);
  void AssignValue(lir::TypeId type, llvm::Value* value, llvm::Value* out);
  // What each of the three is over an integral value, which owns nothing: its
  // bytes copied to `out`.
  void CopyIntegralBytes(
      lir::TypeId type, llvm::Value* value, llvm::Value* out);

  // A value an operation is handed, with the type it is a value of.
  struct OperandRef {
    llvm::Value* value = nullptr;
    lir::TypeId type;
  };
  // The two destinations of a call that can depart, as LLVM names those of an
  // invoke (`InvokeInst::getNormalDest`, `getUnwindDest`): the block the call
  // returns to, and the landing a departure out of it reaches.
  struct InvokeDest {
    llvm::BasicBlock* normal;
    llvm::BasicBlock* unwind;
  };
  // The call on `callee`, made the way the place it is emitted at asks: naming
  // where a departure lands where the call being lowered names one, and
  // continuing in place everywhere else. What it answers is the call.
  auto EmitCallOrInvoke(
      llvm::FunctionCallee callee, std::span<llvm::Value* const> args)
      -> llvm::Value*;

  // What emits the operations over integral values (LRM 11.4): an emitter of
  // its own built over this one, as clang keeps scalar expressions apart from
  // the function they are emitted into.
  class IntegralEmitter;
  // `op` applied to `operands`, answering at `answer_type`: the few
  // instructions it is where every integral value it is handed and answers
  // with fits one word per plane, and a call on the library's entry otherwise.
  // An integral answer is laid out in `out`, which is what this answers with;
  // a machine answer is the value itself. `out` may be an operand's own
  // storage, every operand being read before anything is written.
  auto LowerIntegral(
      support::IntegralOp op, std::span<const OperandRef> operands,
      lir::TypeId answer_type, llvm::Value* out) -> llvm::Value*;
  // Each operand a call states, with the type it is a value of.
  auto LowerOperandRefs(std::span<const lir::Operand> operands)
      -> diag::Result<std::vector<OperandRef>>;
  // A call on the entry `target` names, which is the operation `op` over
  // integral values, applied to the operands the call states.
  auto LowerIntegralCall(
      support::IntegralOp op, const lir::BuiltinTarget& target,
      const lir::CallInstr& call, lir::TypeId result_type, llvm::Value* out)
      -> diag::Result<llvm::Value*>;
  // Where component `index` of a tuple lives, given where the tuple does.
  auto ComponentAddress(
      lir::TypeId tuple, llvm::Value* value, std::size_t index) -> llvm::Value*;

  auto ResolveCall(
      const lir::CallInstr& call, lir::TypeId result_type, llvm::Value* out)
      -> diag::Result<ResolvedCall>;
  // What a call answers with: the answer the callee builds in `out` where the
  // call makes a value, the callee's own answer otherwise, and nothing for a
  // call that answers nothing. Whether a target is entered as a callee or
  // realized as instructions of this body is decided by the target's kind and
  // the types it states, never by the values in hand.
  auto LowerCall(
      const lir::CallInstr& call, lir::TypeId result_type, llvm::Value* out)
      -> diag::Result<llvm::Value*>;
  // Opens a landing: the pad the platform transfers to, and the target the
  // departure names, which is what the body's own test reads.
  auto LowerReceiveDeparture() -> diag::Result<llvm::Value*>;
  // What a call arranged as `abi` enters. A dispatch through an interface
  // class's part enters a body with the value's start instead of the part, as
  // C++ adjusts `this` for a virtual base, so the receiver in `args` is
  // rewritten there.
  auto ResolveCallee(
      const lir::CallInstr& call, lir::TypeId result_type, const FnAbi& abi,
      std::span<llvm::Value*> args) -> diag::Result<llvm::FunctionCallee>;
  // A {pointer, length} view over a scratch buffer of `element` this function
  // fills with `values`, for an entry that takes a sequence of them.
  auto SpanOver(std::span<llvm::Value* const> values, llvm::Type* element)
      -> llvm::Value*;
  auto LowerArray(const lir::ArrayInstr& array, lir::TypeId result_type)
      -> diag::Result<llvm::Value*>;
  auto LowerTuple(
      const lir::TupleInstr& tuple, lir::TypeId result_type, llvm::Value* out)
      -> diag::Result<llvm::Value*>;
  auto LowerAggregateExtract(
      const lir::AggregateExtractInstr& extract, lir::TypeId result_type,
      llvm::Value* out) -> diag::Result<llvm::Value*>;
  auto LowerAggregateUpdate(
      const lir::AggregateUpdateInstr& update, lir::TypeId result_type,
      llvm::Value* out) -> diag::Result<llvm::Value*>;
  // The position a selector names bits of an integral value by (LRM 11.5.1).
  // It names them by that one operand: how many bits is the width of the type
  // they are read at, or of the value written over them.
  auto IntegralPosition(const lir::AggregateSelector& selector)
      -> diag::Result<OperandRef>;
  auto LowerLoad(const lir::LoadInstr& load, lir::TypeId result_type)
      -> diag::Result<llvm::Value*>;
  auto LowerStore(const lir::StoreInstr& store) -> diag::Result<llvm::Value*>;
  auto LowerAddrOf(
      const lir::AddrOfInstr& addr, lir::TypeId result_type, llvm::Value* out)
      -> diag::Result<llvm::Value*>;
  auto LowerBinary(
      const lir::BinaryInstr& binary, lir::TypeId result_type, llvm::Value* out)
      -> diag::Result<llvm::Value*>;
  auto LowerMachineBinary(
      const lir::BinaryInstr& binary, lir::Signedness signedness)
      -> diag::Result<llvm::Value*>;
  auto LowerUnary(
      const lir::UnaryInstr& unary, lir::TypeId result_type, llvm::Value* out)
      -> diag::Result<llvm::Value*>;
  auto LowerMachineUnary(const lir::UnaryInstr& unary)
      -> diag::Result<llvm::Value*>;
  auto LowerCast(const lir::CastInstr& cast, lir::TypeId result_type)
      -> diag::Result<llvm::Value*>;
  // A handle read as a handle of another class, through that class's part of
  // the object: the part the declared types say is there, or the part a cast
  // finds, or none.
  auto LowerHandleCast(
      const lir::HandleCastInstr& cast, lir::TypeId result_type,
      llvm::Value* out) -> diag::Result<llvm::Value*>;
  auto LowerDynamicCast(
      const lir::DynamicCastInstr& cast, lir::TypeId result_type,
      llvm::Value* out) -> diag::Result<llvm::Value*>;
  // The part of class `to` of the object whose part of class `from` is at the
  // non-null `view`, where the declared types say it has one.
  auto ConvertView(llvm::Value* view, lir::TypeId from, lir::TypeId to)
      -> diag::Result<llvm::Value*>;
  // The class a handle or a pointer to an object is of, where it is of one of
  // the source's classes.
  [[nodiscard]] auto ClassBehind(lir::TypeId reference) const
      -> std::optional<lir::TypeId>;
  // `reach` applied to `view` where it is not null, and null where it is: a
  // handle referring to no object has no part to reach anything from.
  auto IfNotNull(
      llvm::Value* view,
      llvm::function_ref<diag::Result<llvm::Value*>(llvm::Value*)> reach)
      -> diag::Result<llvm::Value*>;
  // The part a handle reaches its object through, null where it refers to
  // none; and a handle referring to the same object through `view`, built in
  // `out`.
  auto HandleView(llvm::Value* handle) -> llvm::Value*;
  auto HandleWithView(llvm::Value* handle, llvm::Value* view, llvm::Value* out)
      -> llvm::Value*;

  // The body's variables, laid out as one record in its own frame the way a C++
  // compiler lays out locals: each variable's storage at its own offset, sized
  // and aligned as the library states that storage. Opening builds each in
  // place, a variable is its offset, and closing ends each, last first.
  auto Variables() -> diag::Result<const RecordLayout*>;
  auto LowerOpenVariables() -> diag::Result<llvm::Value*>;
  auto LowerVariableAddress(const lir::VariableAddressInstr& reached)
      -> diag::Result<llvm::Value*>;
  auto LowerCloseVariables(const lir::CloseVariablesInstr& closed)
      -> diag::Result<llvm::Value*>;
  auto LowerOperand(const lir::Operand& operand) -> diag::Result<llvm::Value*>;

  // Whether this body's call protocol is the coroutine one. Such a body is
  // emitted with LLVM coroutine intrinsics and split into a resumable form by
  // the coroutine passes; the state machine and the frame are theirs, not this
  // emitter's.
  [[nodiscard]] auto IsCoroutine() const -> bool;

  // Emits the coroutine ramp (identity, frame allocation, begin) into the entry
  // block and builds the shared final-suspend, cleanup, and end blocks a
  // coroutine body returns through. Runs before the body's blocks are filled.
  void OpenCoroutine();

  // Emits a suspension: save, `llvm.coro.suspend`, and the switch that resumes
  // at `resume`, returns to the caller, or -- where the execution is ended
  // rather than run again -- enters `abandoned`, which runs what the scopes
  // open here owe before the frame goes.
  void EmitCoroutineSuspend(
      llvm::BasicBlock* resume, llvm::BasicBlock* abandoned, bool is_final);

  // Which way an access goes through a place. A step into a container reaches
  // a different element for each: a read of one that is not there reads the
  // default and changes nothing, and a write to one allocates or discards it by
  // the container's own rule.
  enum class Access : std::uint8_t { kRead, kWrite };
  // The address a place names. The base contributes the storage the chain
  // starts from, either a place local's own frame slot or the referent of a
  // reference value, and each further step walks one projection.
  auto ResolvePlaceAddress(const lir::Place& place, Access access)
      -> diag::Result<llvm::Value*>;
  // Which of an element's two entries an access steps through: the one
  // answering with the element as it is, or the one answering with the storage
  // a write lands in.
  static auto EntryFor(Access access, const ElementEntries& entries)
      -> const StepEntry&;
  auto LowerIntConst(const lir::IntConst& constant)
      -> diag::Result<llvm::Value*>;
  auto LowerStrConst(const lir::StrConst& constant) -> llvm::Value*;
  auto LowerRealConst(const lir::RealConst& constant)
      -> diag::Result<llvm::Value*>;
  auto LowerNullConst(const lir::NullConst& constant) -> llvm::Value*;
  auto LowerTerminatorInto(const lir::Terminator& terminator)
      -> diag::Result<void>;

  // Where the value `storage` holds lies in it, or nothing where what `held`
  // states is storage that hands its contents out through its own access.
  auto HeldAt(llvm::Value* storage, support::DeclaredMemberStorage held)
      -> std::optional<llvm::Value*>;
  // Where the part a designation names lies.
  auto DesignatedPart(llvm::Value* designation) -> llvm::Value*;

  // Appends to `args` the arguments `operands` cross as, and to `arranged`
  // how each crosses. They are what selects a part of a value, so they follow
  // the value an entry acts on, and each is read the way `readings` says the
  // operand after that value is.
  auto SelectorArgs(
      const support::OperandReadings& readings,
      std::span<const lir::Operand> operands, std::vector<ArgAbi>& arranged,
      std::vector<llvm::Value*>& args) -> diag::Result<void>;

  // The type of an operand, and the value domain a library entry is chosen by.
  [[nodiscard]] auto OperandType(const lir::Operand& operand) const
      -> lir::TypeId;
  [[nodiscard]] auto DomainOf(lir::TypeId type) const
      -> diag::Result<support::ValueDomain>;
  // The domain of the value cell `place` names, where the storage it reaches is
  // one. Such storage is written and read through itself rather than off its
  // address, so an access to it is that storage's own operation. A place naming
  // anything else answers nothing.
  [[nodiscard]] auto PlaceValueCellDomain(
      const lir::Place& place, lir::TypeId value) const
      -> std::optional<support::ValueDomain>;
  // Place access: the capability wrapper a place names the storage of, which
  // wrapper it is, and the domain that representation picks its library entries
  // by; nothing when the place names ordinary addressable storage. It is the
  // one site that asks that of a place, so however deep the chain is an access
  // through a wrapper is classified once.
  struct WrapperPlace {
    support::ValueDomain domain{};
    WrapperKind kind{};
    lir::Place wrapper;
  };
  [[nodiscard]] auto WrapperPlaceOf(const lir::Place& place) const
      -> diag::Result<std::optional<WrapperPlace>>;
  // The contents of the wrapper at `wrapper`, as the storage a read of them
  // answers with.
  auto ContentsOf(
      support::ValueDomain domain, WrapperKind kind, llvm::Value* wrapper)
      -> llvm::Value*;
  // The handle of the value the value cell at `cell` holds, which is not the
  // cell's own address for every domain.
  auto ValueCellContents(support::ValueDomain domain, llvm::Value* cell)
      -> llvm::Value*;
  // Building a closure: the runtime makes the value, sized as its definition
  // states, and each operand is taken into the capture it initializes. A
  // closure's captures are laid out on this side, so it is this side that
  // fills them.
  auto LowerClosure(
      const lir::ClosureInstr& built, lir::TypeId result_type, llvm::Value* out)
      -> diag::Result<llvm::Value*>;

  // Whether a type is the sequence of handles a declaration standing for
  // several objects builds. It belongs to no value domain -- what it holds are
  // objects, not values -- so an operation over one is answered by the entry
  // that knows sequences rather than through the value model.
  [[nodiscard]] auto IsHandleSequence(lir::TypeId type) const -> bool;

  // The address of the storage a member step reaches. A class's storage extends
  // its base's, so the step names the class that declares the member beside the
  // slot that class gave it, and the member sits at the offset that class's
  // layout gave the slot.
  auto MemberStorage(llvm::Value* owner, const lir::StatedMemberRef& member)
      -> diag::Result<llvm::Value*>;

  [[nodiscard]] auto ReachedType(
      const lir::Place& place, std::ptrdiff_t index) const -> lir::TypeId;

  auto OpenedReferent(llvm::Value* reference, lir::TypeId type) -> llvm::Value*;

  CodeGenModule* module_;
  lir::FunctionId id_;
  const lir::Function* fn_;
  llvm::Function* value_;
  llvm::IRBuilder<> builder_;
  CallArranger arranger_;
  // Where the call being lowered goes on from, while that call is one that can
  // depart; nothing while what is lowered continues in place.
  std::optional<InvokeDest> invoke_dest_;
  std::unordered_map<lir::ValueId, llvm::Value*> values_;
  std::vector<llvm::BasicBlock*> blocks_;
  // Where frame storage is allocated: the end of the code the body opens with,
  // ahead of its first statement.
  llvm::Instruction* frame_storage_point_ = nullptr;
  // Where the body's variables sit, laid out once for the whole body.
  std::optional<RecordLayout> variables_;
  // A coroutine body's ramp state: the coroutine identity (which names the
  // frame to release) and its handle, plus the blocks every suspension and
  // return funnels through. The frame's layout and the resume state machine are
  // the coroutine passes' to synthesize, never this emitter's.
  llvm::Value* coro_id_ = nullptr;
  llvm::Value* coro_handle_ = nullptr;
  llvm::BasicBlock* coro_final_ = nullptr;
  llvm::BasicBlock* coro_cleanup_ = nullptr;
  llvm::BasicBlock* coro_end_ = nullptr;
};

}  // namespace lyra::backend::llvm_backend
