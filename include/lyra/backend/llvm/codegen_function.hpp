#pragma once

#include <cstddef>
#include <cstdint>
#include <optional>
#include <span>
#include <string_view>
#include <unordered_map>
#include <variant>
#include <vector>

#include <llvm/ADT/STLFunctionalExtras.h>
#include <llvm/IR/IRBuilder.h>

#include "lyra/backend/llvm/codegen_module.hpp"
#include "lyra/backend/llvm/codegen_value_type.hpp"
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
struct Planes;

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
    // The storage the callee builds its return value in, handed to it last,
    // or nothing where its convention returns the value directly.
    llvm::Value* return_slot;
  };

  // The LLVM argument each of the body's parameters reads, one for one.
  auto BindParameters() -> void;
  auto LowerInstrInto(const lir::Instr& instr) -> diag::Result<void>;
  // An assignment: its value computed and put where its destination says.
  auto LowerAssignInto(const lir::AssignInstr& assign) -> diag::Result<void>;
  // What this target makes of a LIR destination, named as rustc's codegen
  // names what it makes of a MIR place, operand or local (`PlaceRef`,
  // `OperandRef`, `LocalRef`): a transient whose value is not kept in storage,
  // still to be defined, as rustc's `LocalRef::PendingOperand`; or storage
  // lying at `address` that holds a value of `type`, as rustc's `PlaceRef`.
  struct PendingOperand {
    lir::ValueId value;
  };
  struct PlaceRef {
    llvm::Value* address = nullptr;
    lir::TypeId type;
  };
  using DestinationRef = std::variant<PendingOperand, PlaceRef>;
  auto ResolveDestination(const lir::Destination& destination)
      -> diag::Result<DestinationRef>;
  // The type of the value `destination` holds.
  [[nodiscard]] auto TypeOf(const DestinationRef& destination) const
      -> lir::TypeId;
  // The storage a value lying in storage is built in: the place `destination`
  // names, or storage of its own for a transient, made only by the step that
  // builds a value there, as clang's `EnsureDest` makes a temporary only when
  // handed no slot -- a transient that reads or moves in a value already made
  // is that value and takes none. Nothing for a value computed as itself.
  auto EnsureDestination(const DestinationRef& destination) -> llvm::Value*;
  // What an rvalue computes, built in the storage its destination gives where
  // it is built in storage, as rustc's `codegen_rvalue` and
  // `codegen_rvalue_operand` do for a place and for an operand.
  auto LowerRvalue(const lir::Rvalue& value, const DestinationRef& destination)
      -> diag::Result<llvm::Value*>;

  // Storage in the body's own frame, allocated where the body opens: every
  // path reaching an instruction then names the same storage, and a loop
  // reuses it rather than growing the stack on each turn.
  auto FrameStorage(llvm::Type* type) -> llvm::Value*;
  // Frame storage of the size and alignment `layout` states.
  auto LaidOutStorage(support::ObjectLayout layout) -> llvm::Value*;
  // Frame storage for one runtime object, sized and aligned as the library
  // states that object.
  auto ObjectStorage(support::RuntimeObject object) -> llvm::Value*;
  // Where a value of `type` an instruction makes is built: storage for a value
  // lying in storage and for a sequence of machine values, as clang builds a
  // class-type result through `sret`, and nothing for any other value, which
  // the instruction answers as itself. LIR states where each is ended.
  auto StorageFor(lir::TypeId type) -> llvm::Value*;
  // Whether a value of `type` is built in storage -- one lying in storage of
  // its own, or a sequence of machine values -- rather than computed as itself.
  [[nodiscard]] auto IsBuiltInStorage(lir::TypeId type) const -> bool;
  // Leaves what was computed where `destination` says, as rustc's
  // `store_return`: a pending transient becomes that value, and a value not
  // built in storage is stored into its place.
  void StoreInto(
      const DestinationRef& destination, llvm::Value* value,
      llvm::IRBuilderBase& at);
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
  // The lifecycle of an owned value. `EndValue` ends the value `value` names,
  // where ending one has anything to do. `CopyValue` builds a second value
  // equal to it in the unbuilt storage `out`, and answers `out`.
  // `RelocateValue` moves it into the unbuilt storage `out` and ends what the
  // move left behind, so the value lives in `out` and nowhere else.
  // `AssignValue` writes it into the value already built at `out`, which goes
  // on being that value. Each fails where the step is the library's to carry
  // out and has to be handed a type the library does not realize.
  auto EndValue(lir::TypeId type, llvm::Value* value) -> diag::Result<void>;
  auto CopyValue(lir::TypeId type, llvm::Value* value, llvm::Value* out)
      -> diag::Result<llvm::Value*>;
  auto RelocateValue(lir::TypeId type, llvm::Value* value, llvm::Value* out)
      -> diag::Result<void>;
  auto AssignValue(lir::TypeId type, llvm::Value* value, llvm::Value* out)
      -> diag::Result<void>;

  // A value an operation is handed, with the type it is a value of.
  struct OperandRef {
    llvm::Value* value = nullptr;
    lir::TypeId type;
  };
  // The two destinations of a call that can be unwound out of, as LLVM names
  // those of an invoke (`InvokeInst::getNormalDest`, `getUnwindDest`): the
  // block the call returns to, and the landing unwinding out of it reaches.
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
  // `op` applied to `operands`, answering at `answer_type`: the instructions
  // it is on planes where the operation is stated that way, and a call on the
  // library's entry otherwise.
  // An integral answer is laid out in `out`, which is what this answers with;
  // a machine answer is the value itself. `out` may be an operand's own
  // storage, every operand being read before anything is written.
  auto LowerIntegral(
      support::IntegralOp op, std::span<const OperandRef> operands,
      lir::TypeId answer_type, llvm::Value* out) -> llvm::Value*;
  // One part of an integral value: where it starts within what it is a part
  // of, and the type that fixes how many bits it has.
  struct BitsPartRef {
    OperandRef start;
    lir::TypeId type;
  };
  // `bits` written over the bits `parts` name, outermost first, in the value
  // of type `whole` lying at `at`, as instructions on planes. Emits nothing
  // and answers false where the value, a part or the bits is not stated on
  // planes.
  auto StoreBitsOnPlanes(
      llvm::Value* at, lir::TypeId whole, std::span<const BitsPartRef> parts,
      OperandRef bits) -> bool;
  // Each operand a call states, with the type it is a value of.
  auto LowerOperandRefs(std::span<const lir::Operand> operands)
      -> diag::Result<std::vector<OperandRef>>;
  // A call on the entry `target` names, which is the operation `op` over
  // integral values, applied to the operands the call states.
  auto LowerIntegralCall(
      support::IntegralOp op, const lir::BuiltinTarget& target,
      const lir::CallInstr& call, lir::TypeId result_type, llvm::Value* out)
      -> diag::Result<llvm::Value*>;
  // Where component `index` of a tuple, or member `index` of a union, lives,
  // given where the aggregate does.
  auto ComponentAddress(
      lir::TypeId aggregate, llvm::Value* value, std::size_t index)
      -> llvm::Value*;

  // The call settled against its convention, which alone decides whether the
  // callee builds its return value in a return slot, as clang's `EmitCall`
  // and rustc's `make_return_dest` read the return's ABI; `return_slot` is
  // asked for one only where it does.
  auto ResolveCall(
      const lir::CallInstr& call, lir::TypeId result_type,
      llvm::function_ref<llvm::Value*()> return_slot)
      -> diag::Result<ResolvedCall>;
  // What the call answers with: the value the callee builds in storage -- the
  // place `destination` names, or storage of its own for a transient -- the
  // callee's own answer otherwise, and nothing for a call that answers
  // nothing. Whether a target is entered as a callee or
  // realized as instructions of this body is decided by the target's kind and
  // the types it states, never by the values in hand.
  auto LowerCall(const lir::CallInstr& call, const DestinationRef& destination)
      -> diag::Result<llvm::Value*>;
  // Opens a landing: the pad the platform transfers to, answering what is
  // unwinding as the pointer the unwinder carries.
  auto LowerLanding(const lir::LandingInstr& landing) -> llvm::Value*;
  // A handler's extent, spelled as the C++ ABI's own begin and end of a catch,
  // as clang spells it; neither can be unwound out of.
  auto LowerBeginHandling(const lir::BeginHandlingInstr& begun)
      -> diag::Result<llvm::Value*>;
  auto LowerEndHandling() -> llvm::Value*;
  // What a landing pad yields and `resume` takes: the unwinder's pointer and a
  // selector no clause here reads.
  auto LandingPadType() -> llvm::StructType*;
  // What a call arranged as `abi` enters. A dispatch through an interface
  // class's part enters a body with the value's start instead of the part, as
  // C++ adjusts `this` for a virtual base, so the receiver in `args` is
  // rewritten there.
  auto ResolveCallee(
      const lir::CallInstr& call, lir::TypeId result_type, const FnAbi& abi,
      std::span<llvm::Value*> args) -> diag::Result<llvm::FunctionCallee>;
  // A {pointer, length} view over `storage` of `element`, which this fills
  // with `values`, for an entry that takes a sequence of them.
  auto SpanOver(
      std::span<llvm::Value* const> values, llvm::Type* element,
      llvm::Value* storage) -> llvm::Value*;
  // The {pointer, length} view over `count` values lying at `storage`.
  auto ViewOver(llvm::Value* storage, std::uint64_t count) -> llvm::Value*;
  auto LowerArray(
      const lir::ArrayInstr& array, const DestinationRef& destination)
      -> diag::Result<llvm::Value*>;
  auto LowerProduct(
      const lir::ProductInstr& product, lir::TypeId result_type,
      llvm::Value* out) -> diag::Result<llvm::Value*>;
  // A union laid out in place: its storage zeroed, the member it holds built
  // at its start, and a tagged union's tag stored after its members.
  auto LowerUnion(
      const lir::UnionInstr& built, lir::TypeId result_type, llvm::Value* out)
      -> diag::Result<llvm::Value*>;
  auto LowerVectorGet(const lir::VectorGetInstr& get)
      -> diag::Result<llvm::Value*>;
  auto LowerDiscriminant(
      const lir::DiscriminantInstr& read, lir::TypeId result_type)
      -> diag::Result<llvm::Value*>;
  // What `place` holds, as a value of `result_type` is handed on: the value
  // itself, or the address of one laid out by its type.
  auto LoadPlace(const lir::Place& place, lir::TypeId result_type)
      -> diag::Result<llvm::Value*>;
  // A load through a member of an untagged union, whose bits may be another
  // member's (LRM 7.3.2): the value copied into `out` and every position its
  // type keeps clear cleared there.
  auto LowerOverlaidLoad(
      const lir::OverlaidLoadInstr& load, lir::TypeId result_type,
      llvm::Value* out) -> diag::Result<llvm::Value*>;
  auto LowerStore(const lir::StoreInstr& store) -> diag::Result<llvm::Value*>;
  auto LowerUse(const lir::UseInstr& use, const DestinationRef& destination)
      -> diag::Result<llvm::Value*>;
  // Bits written into the integral value at a place, where it lies: the
  // insertion as instructions where the value, every part and the bits are
  // each stated on planes, and a call on the library's insertion otherwise.
  auto LowerBitsStore(const lir::BitsStoreInstr& store)
      -> diag::Result<llvm::Value*>;
  // Bits written into the integral value a write in progress designates, by
  // that value's own entry, which hears which of them moved.
  auto LowerDesignatedBitsWrite(
      const lir::DesignatedBitsTarget& bits, const lir::CallInstr& call,
      lir::TypeId result_type) -> diag::Result<llvm::Value*>;
  // The parts a statement of bits names, each with where it starts lowered.
  auto LowerBitsParts(std::span<const lir::BitsPart> parts)
      -> diag::Result<std::vector<BitsPartRef>>;
  // The parts some bits lie in, outermost first, as an entry writing the bits
  // takes them: a span over where each starts and how many bits it has.
  auto OuterBitsParts(std::span<const BitsPartRef> parts) -> llvm::Value*;
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
  auto Variables() -> diag::Result<const PlacedMembers*>;
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
  // What an address of `address_type` may be used for: a read-only pointer
  // only reads, and every other address may be written through.
  auto AccessGranted(lir::TypeId address_type) const -> Access;
  // The planes of `position`, which has to be stated as the position type
  // every ordinal of the layer above is brought to.
  auto LoadPosition(const OperandRef& position) -> Planes;
  // Where the element `position` names lies in the fixed-size unpacked array
  // lying at `array`: arithmetic on where the array lies. A position naming no
  // element reaches what the array's type answers for one (LRM 7.4.5), the
  // element default for a read and storage no read reaches for a write.
  auto ArrayElementAddress(
      lir::TypeId array_type, llvm::Value* array, const OperandRef& position,
      Access access) -> diag::Result<llvm::Value*>;
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

  // Where the value `storage` holds lies, or nothing where what `held` states
  // is storage that hands its contents out through its own access.
  auto HeldAt(llvm::Value* storage, support::DeclaredMemberStorage held)
      -> std::optional<llvm::Value*>;
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
  // Building a closure in `out`: its start is the address of its definition,
  // the library's part of every closure, and each operand is taken into the
  // capture it initializes. A closure's captures are laid out on this side, so
  // it is this side that fills them.
  auto LowerClosure(
      const lir::ClosureInstr& built, lir::TypeId result_type, llvm::Value* out)
      -> diag::Result<llvm::Value*>;

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
  std::optional<PlacedMembers> variables_;
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
