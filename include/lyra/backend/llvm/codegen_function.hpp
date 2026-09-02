#pragma once

#include <cstddef>
#include <cstdint>
#include <optional>
#include <span>
#include <string_view>
#include <unordered_map>
#include <vector>

#include <llvm/IR/IRBuilder.h>

#include "lyra/backend/llvm/runtime_entry.hpp"
#include "lyra/diag/diagnostic.hpp"
#include "lyra/lir/function.hpp"
#include "lyra/lir/type_id.hpp"
#include "lyra/support/value_domain.hpp"

namespace llvm {
class BasicBlock;
class Function;
class FunctionCallee;
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
  CodeGenFunction(
      CodeGenModule& module, const lir::Function& fn, llvm::Function* value);

  auto Run() -> diag::Result<void>;

 private:
  auto LowerInstr(const lir::Instr& instr) -> diag::Result<llvm::Value*>;
  auto LowerCall(const lir::CallInstr& call, lir::TypeId result_type)
      -> diag::Result<llvm::Value*>;
  auto ResolveCallee(
      const lir::CallInstr& call, lir::TypeId result_type,
      std::span<llvm::Value* const> args) -> diag::Result<llvm::FunctionCallee>;
  // An entry the runtime publishes, declared with the types of the values
  // crossing to it: the call is where an entry's signature comes from, so the
  // two cannot disagree about what is passed.
  auto Entry(
      std::string_view symbol, llvm::Type* result,
      std::span<llvm::Value* const> args) -> llvm::FunctionCallee;
  auto Entry(
      std::string_view symbol, lir::TypeId result,
      std::span<llvm::Value* const> args) -> llvm::FunctionCallee;
  // A {pointer, length} view over a scratch buffer of `element` this function
  // fills with `values`, for an entry that takes a run of them.
  auto SpanOver(std::span<llvm::Value* const> values, llvm::Type* element)
      -> llvm::Value*;
  auto LowerArray(const lir::ArrayInstr& array, lir::TypeId result_type)
      -> diag::Result<llvm::Value*>;
  auto LowerProduct(const lir::ProductInstr& product, lir::TypeId result_type)
      -> diag::Result<llvm::Value*>;
  auto LowerUnion(const lir::UnionInstr& u, lir::TypeId result_type)
      -> diag::Result<llvm::Value*>;
  // The runtime domain a union's member `index` boxes as. Both union kinds hold
  // their member types positionally, so this reads either one.
  auto UnionMemberDomain(lir::TypeId union_type, std::uint32_t index)
      -> diag::Result<support::ValueDomain>;
  auto LowerAggregateExtract(const lir::AggregateExtractInstr& extract)
      -> diag::Result<llvm::Value*>;
  auto LowerAggregateUpdate(const lir::AggregateUpdateInstr& update)
      -> diag::Result<llvm::Value*>;
  auto LowerTagTest(const lir::TagTestInstr& test, lir::TypeId result_type)
      -> diag::Result<llvm::Value*>;
  auto LowerLoad(const lir::LoadInstr& load, lir::TypeId result_type)
      -> diag::Result<llvm::Value*>;
  auto LowerStore(const lir::StoreInstr& store) -> diag::Result<llvm::Value*>;
  auto LowerBinary(const lir::BinaryInstr& binary, lir::TypeId result_type)
      -> diag::Result<llvm::Value*>;
  auto LowerMachineBinary(const lir::BinaryInstr& binary)
      -> diag::Result<llvm::Value*>;
  auto LowerUnary(const lir::UnaryInstr& unary, lir::TypeId result_type)
      -> diag::Result<llvm::Value*>;
  auto LowerMachineUnary(const lir::UnaryInstr& unary)
      -> diag::Result<llvm::Value*>;
  auto LowerBoolCast(const lir::BoolCastInstr& cast, lir::TypeId result_type)
      -> diag::Result<llvm::Value*>;
  auto LowerIntCast(const lir::IntCastInstr& cast, lir::TypeId result_type)
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
  // at `resume`, returns to the caller, or enters cleanup.
  void EmitCoroutineSuspend(llvm::BasicBlock* resume, bool is_final);

  // The address a place names. The base contributes the storage the chain
  // starts from, either a place local's own frame slot or the referent of a
  // reference value, and each further step walks one projection.
  auto ResolvePlaceAddress(const lir::Place& place)
      -> diag::Result<llvm::Value*>;
  auto LowerIntConst(const lir::IntConst& constant)
      -> diag::Result<llvm::Value*>;
  auto LowerStrConst(const lir::StrConst& constant) -> llvm::Value*;
  auto LowerRealConst(const lir::RealConst& constant)
      -> diag::Result<llvm::Value*>;
  auto LowerNullConst(const lir::NullConst& constant) -> llvm::Value*;
  auto LowerPackedTypeRef(const lir::PackedTypeRef& ref)
      -> diag::Result<llvm::Value*>;
  auto LowerTerminatorInto(const lir::Terminator& terminator)
      -> diag::Result<void>;

  auto BuiltinCallee(
      const lir::BuiltinTarget& target, const lir::CallInstr& call,
      lir::TypeId result_type, std::span<llvm::Value* const> args)
      -> diag::Result<llvm::FunctionCallee>;
  auto ConstructCallee(
      const lir::CallInstr& call, lir::TypeId result,
      std::span<llvm::Value* const> args) -> diag::Result<llvm::FunctionCallee>;

  // What a call's entry is handed, given the operands the call states. Nothing
  // here is anything the call means; it is this target's encoding of it.
  auto CallArgs(const lir::CallInstr& call, std::vector<llvm::Value*> operands)
      -> diag::Result<std::vector<llvm::Value*>>;

  // A leading reference to the definition of what a construct builds where the
  // entry needs one, and the operands as one span where it takes them that way
  // -- read from the result type the same way the entry itself is.
  auto ConstructArgs(
      lir::TypeId result, const std::vector<llvm::Value*>& operands)
      -> diag::Result<std::vector<llvm::Value*>>;

  // Which operand states the shape of what a call produces, absent for a call
  // whose result the operands already shape. A construction leads with the
  // element default it is seeded from, except where a size precedes it -- the
  // LRM 7.5.1 run-time-sized forms state how many elements there are before
  // what each one is -- and an LRM 7.12 method trails the result element the
  // producer supplied behind the operands the method itself needs.
  [[nodiscard]] auto ResultShapeOperand(const lir::CallInstr& call) const
      -> std::optional<std::size_t>;

  // Which operand of a call crosses erased, and in which representation. A
  // value crosses erased where it states a representation the entry has no
  // other way to know: the shape a call's result takes, which follows the call
  // rather than the entry's own name, and the index a keyed container selects
  // by, which states the one that container's declared index type names.
  struct ErasedArgument {
    std::size_t position;
    support::ValueDomain domain;
  };
  [[nodiscard]] auto ErasedOperand(const lir::CallInstr& call) const
      -> diag::Result<std::optional<ErasedArgument>>;

  // The representation a container's coordinates cross in, absent where they
  // cross as the bare handles their own types name.
  [[nodiscard]] auto CoordinateDomain(lir::TypeId container) const
      -> diag::Result<std::optional<support::ValueDomain>>;

  auto SelectorArgs(
      lir::TypeId container, const std::vector<lir::Operand>& operands,
      std::vector<llvm::Value*>& shape) -> diag::Result<void>;

  // The type of an operand, and the value domain a library entry is chosen by.
  [[nodiscard]] auto OperandType(const lir::Operand& operand) const
      -> lir::TypeId;
  [[nodiscard]] auto DomainOf(lir::TypeId type) const
      -> diag::Result<support::ValueDomain>;
  // The domain of the cell a reference addresses, for a cell operation. The
  // reference is named by its type, since a cell is allocated before there is
  // an operand holding it.
  [[nodiscard]] auto CellDomain(lir::TypeId reference) const
      -> diag::Result<support::ValueDomain>;
  // Place access: the cell a place names the contents of, and the domain that
  // picks its library entries; nothing when the place names ordinary
  // addressable storage. This is the one entry that decides how a load and a
  // store through a cell are realized.
  struct CellPlace {
    support::ValueDomain domain{};
    lir::Place cell;
  };
  [[nodiscard]] auto CellPlaceOf(const lir::Place& place) const
      -> diag::Result<std::optional<CellPlace>>;
  // A capture read: the closure value whose captures the place reaches, and
  // which of them it names; nothing when the place reaches an instance's own
  // members instead. A capture lives in storage the closure owns, so it is
  // reached on the closure rather than through the instance member entry.
  struct CapturePlace {
    lir::Place closure;
    std::uint32_t index{};
  };
  [[nodiscard]] auto CapturePlaceOf(const lir::Place& place) const
      -> std::optional<CapturePlace>;

  // Whether a type is the sequence of handles a declaration standing for
  // several objects builds. It belongs to no value domain -- what it holds are
  // objects, not values -- so an operation over one is answered by the entry
  // that knows sequences rather than through the value model.
  [[nodiscard]] auto IsHandleSequence(lir::TypeId type) const -> bool;

  [[nodiscard]] auto MemberAddressOp(lir::TypeId owner) const -> RuntimeOp;

  [[nodiscard]] auto ReachedType(
      const lir::Place& place, std::ptrdiff_t index) const -> lir::TypeId;

  auto OpenedReferent(llvm::Value* reference, lir::TypeId type) -> llvm::Value*;

  CodeGenModule* module_;
  const lir::Function* fn_;
  llvm::Function* value_;
  llvm::IRBuilder<> builder_;
  std::unordered_map<lir::ValueId, llvm::Value*> values_;
  std::vector<llvm::BasicBlock*> blocks_;
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
