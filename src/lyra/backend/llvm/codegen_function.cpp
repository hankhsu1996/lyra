#include "lyra/backend/llvm/codegen_function.hpp"

#include <array>
#include <cstddef>
#include <cstdint>
#include <format>
#include <optional>
#include <span>
#include <utility>
#include <variant>
#include <vector>

#include <llvm/IR/BasicBlock.h>
#include <llvm/IR/Constants.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/Instructions.h>
#include <llvm/IR/Intrinsics.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/Type.h>

#include "lyra/backend/llvm/codegen_module.hpp"
#include "lyra/backend/llvm/codegen_tuple.hpp"
#include "lyra/backend/llvm/runtime_entry.hpp"
#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/diag/diag_code.hpp"
#include "lyra/diag/diagnostic.hpp"
#include "lyra/lir/compilation_unit.hpp"
#include "lyra/lir/place_query.hpp"
#include "lyra/runtime/object_layout.hpp"
#include "lyra/support/runtime_object.hpp"
#include "lyra/support/value_domain.hpp"
#include "lyra/value/integral.hpp"

namespace lyra::backend::llvm_backend {

CodeGenFunction::CodeGenFunction(CodeGenModule& module, lir::FunctionId id)
    : module_(&module),
      id_(id),
      fn_(&module.Unit().functions.Get(id)),
      value_(module.UnitFunction(id)),
      builder_(module.Context()),
      arranger_(module, *fn_) {
}

auto CodeGenFunction::IsCoroutine() const -> bool {
  return module_->Unit().types.Get(fn_->result_type).Is<lir::CoroutineType>();
}

auto CodeGenFunction::BindParameters() -> void {
  for (std::size_t i = 0; i < fn_->params.size(); ++i) {
    values_.emplace(fn_->params[i], value_->getArg(i));
  }
}

auto CodeGenFunction::Run() -> diag::Result<void> {
  BindParameters();

  // Every block exists before any is filled, so a branch resolves its successor
  // regardless of the order the blocks are emitted in.
  blocks_.reserve(fn_->blocks.size());
  for (std::uint32_t i = 0; i < fn_->blocks.size(); ++i) {
    blocks_.push_back(
        llvm::BasicBlock::Create(
            module_->Context(), std::format("bb{}", i), value_));
  }

  // A coroutine body opens with its ramp -- identity, frame, begin -- ahead of
  // the body, and the slots that must survive a suspension are allocated there,
  // since only a slot allocated once the frame exists is moved into it.
  llvm::BasicBlock* entry = blocks_.front();
  if (IsCoroutine()) {
    entry = llvm::BasicBlock::Create(
        module_->Context(), "coro.ramp", value_, blocks_.front());
    builder_.SetInsertPoint(entry);
    OpenCoroutine();
  }

  // A place local is frame storage: its slot is allocated once, in the entry
  // block, so every path that reaches it names the same address. A slot of an
  // owned type holds the value itself, as a C++ local of class type does.
  builder_.SetInsertPoint(entry);
  frame_storage_point_ =
      builder_.CreateAlloca(builder_.getInt8Ty(), nullptr, "frame.storage");
  DeclareHeapPointers();
  for (const lir::ValueId id : fn_->values.Ids()) {
    const lir::Local& local = fn_->values.Get(id);
    if (!local.NamesStorage() || NeedsHeapStorage(local.type)) {
      continue;
    }
    llvm::Value* owned = StorageFor(local.type);
    values_.emplace(
        id, owned != nullptr ? owned
                             : FrameStorage(module_->Types().Map(local.type)));
  }
  if (IsCoroutine()) {
    // The ramp places the arguments in the frame and stops before the body's
    // first statement. An execution's stretches all belong to whoever drives
    // it -- including the first -- so none of them may run where the frame
    // happened to be built. No scope of the body has opened yet, so an
    // execution ended here owes nothing.
    EmitCoroutineSuspend(blocks_.front(), coro_cleanup_, false);
  }

  for (std::uint32_t i = 0; i < fn_->blocks.size(); ++i) {
    builder_.SetInsertPoint(blocks_[i]);
    const lir::BasicBlock& block = fn_->blocks[i];
    for (const lir::Instr& instr : block.instrs) {
      auto lowered = LowerInstr(instr);
      if (!lowered) {
        return std::unexpected(std::move(lowered.error()));
      }
      values_.emplace(instr.result, *lowered);
    }
    auto terminated = LowerTerminatorInto(block.terminator);
    if (!terminated) {
      return std::unexpected(std::move(terminated.error()));
    }
  }
  frame_storage_point_->eraseFromParent();
  frame_storage_point_ = nullptr;
  return {};
}

auto CodeGenFunction::FrameStorage(llvm::Type* type) -> llvm::Value* {
  llvm::IRBuilder<> at(frame_storage_point_);
  return at.CreateAlloca(type);
}

auto CodeGenFunction::LaidOutStorage(support::ObjectLayout layout)
    -> llvm::Value* {
  llvm::IRBuilder<> at(frame_storage_point_);
  llvm::AllocaInst* storage =
      at.CreateAlloca(llvm::ArrayType::get(at.getInt8Ty(), layout.size));
  storage->setAlignment(llvm::Align(layout.align));
  return storage;
}

auto CodeGenFunction::ObjectStorage(support::RuntimeObject object)
    -> llvm::Value* {
  return LaidOutStorage(runtime::LayoutOf(object));
}

auto CodeGenFunction::StorageFor(lir::TypeId type) -> llvm::Value* {
  if (!module_->Types().LiesInStorage(type)) {
    return nullptr;
  }
  return LaidOutStorage(module_->Types().LayoutOf(type));
}

auto CodeGenFunction::NeedsHeapStorage(lir::TypeId type) const -> bool {
  return module_->Types().LiesInStorage(type) &&
         module_->Types().LayoutOf(type).size > value::kMaxInlineBytes;
}

void CodeGenFunction::DeclareHeapPointers() {
  const auto declare = [&](lir::ValueId id) {
    llvm::IRBuilder<> at(frame_storage_point_);
    llvm::AllocaInst* const pointer = at.CreateAlloca(module_->Types().Ptr());
    at.CreateStore(
        llvm::ConstantPointerNull::get(module_->Types().Ptr()), pointer);
    heap_pointers_.emplace(id, pointer);
  };
  for (const lir::ValueId id : fn_->values.Ids()) {
    const lir::Local& local = fn_->values.Get(id);
    if (local.NamesStorage() && NeedsHeapStorage(local.type)) {
      declare(id);
    }
  }
  for (const lir::BasicBlock& block : fn_->blocks) {
    for (const lir::Instr& instr : block.instrs) {
      if (lir::MakesValue(instr.data) &&
          NeedsHeapStorage(fn_->values.Get(instr.result).type)) {
        declare(instr.result);
      }
    }
    if (const auto* call =
            std::get_if<lir::DepartingCallInstr>(&block.terminator.data);
        call != nullptr && lir::CallMakesValue(call->target) &&
        NeedsHeapStorage(fn_->values.Get(call->result).type)) {
      declare(call->result);
    }
  }
}

auto CodeGenFunction::StorageForResult(lir::ValueId made, lir::TypeId type)
    -> llvm::Value* {
  return NeedsHeapStorage(type) ? AllocateBytes(made) : StorageFor(type);
}

auto CodeGenFunction::AllocateBytes(lir::ValueId of) -> llvm::Value* {
  const auto kept = heap_pointers_.find(of);
  if (kept == heap_pointers_.end()) {
    throw InternalError(
        "llvm codegen: bytes are obtained for a value that was given no "
        "pointer "
        "to keep their address in -- please report this as a bug");
  }
  const std::array<llvm::Value*, 1> args{builder_.getInt64(
      module_->Types().LayoutOf(fn_->values.Get(of).type).size)};
  llvm::Value* const bytes = CallEntry(
      RuntimeSymbol(RuntimeOp::kAllocBytes), module_->Types().Ptr(),
      {CallArranger::Direct(builder_.getInt64Ty())}, args);
  builder_.CreateStore(bytes, kept->second);
  return bytes;
}

void CodeGenFunction::DeallocateBytes(lir::ValueId of) {
  const auto kept = heap_pointers_.find(of);
  if (kept == heap_pointers_.end()) {
    return;
  }
  const std::array<llvm::Value*, 2> args{
      builder_.CreateLoad(module_->Types().Ptr(), kept->second),
      builder_.getInt64(
          module_->Types().LayoutOf(fn_->values.Get(of).type).size)};
  CallEntry(
      RuntimeSymbol(RuntimeOp::kDeallocBytes), builder_.getVoidTy(),
      {CallArranger::Direct(module_->Types().Ptr()),
       CallArranger::Direct(builder_.getInt64Ty())},
      args);
  builder_.CreateStore(
      llvm::ConstantPointerNull::get(module_->Types().Ptr()), kept->second);
}

auto CodeGenFunction::AddressOf(lir::ValueId id) -> llvm::Value* {
  const auto kept = heap_pointers_.find(id);
  if (kept != heap_pointers_.end()) {
    return builder_.CreateLoad(module_->Types().Ptr(), kept->second);
  }
  return values_.at(id);
}

auto CodeGenFunction::Addresses(std::size_t count) const
    -> std::vector<ArgAbi> {
  return std::vector<ArgAbi>(
      count, CallArranger::Direct(module_->Types().Ptr()));
}

auto CodeGenFunction::CallEntry(
    std::string_view symbol, llvm::Type* result, std::vector<ArgAbi> arranged,
    std::span<llvm::Value* const> args) -> llvm::Value* {
  return builder_.CreateCall(
      module_->RuntimeFunction(
          symbol, CallArranger::Arrange(
                      std::move(arranged), ReturnDirect{.type = result})),
      args);
}

auto CodeGenFunction::BuildInto(
    std::string_view symbol, std::vector<ArgAbi> arranged,
    std::vector<llvm::Value*> args, llvm::Value* out) -> llvm::Value* {
  args.push_back(out);
  builder_.CreateCall(
      module_->RuntimeFunction(
          symbol, CallArranger::Arrange(
                      std::move(arranged),
                      ReturnIndirect{.returned = module_->Types().Ptr()})),
      args);
  return out;
}

auto CodeGenFunction::EndValue(lir::TypeId type, llvm::Value* value)
    -> diag::Result<void> {
  // A pointer owning what it points at ends that where it lies and gives the
  // storage back, as Rust drops the value in a `Box` and then frees it.
  const lir::Type& ended = module_->Unit().types.Get(type);
  if (ended.IsMoveOnly()) {
    const lir::TypeId pointee = *ended.Pointee();
    if (!module_->Types().LiesInStorage(pointee)) {
      return diag::Fail(
          diag::DiagCode::kUnsupportedExpressionForm,
          "llvm codegen: ending an object a unique pointer owns is not yet "
          "supported");
    }
    if (auto inner = EndValue(pointee, value); !inner) {
      return inner;
    }
    auto size = module_->CompleteObjectSize(pointee);
    if (!size) {
      return std::unexpected(std::move(size.error()));
    }
    builder_.CreateCall(
        module_->SizedOperatorDelete(), {value, builder_.getInt64(*size)});
    return {};
  }
  if (module_->Types().LayoutOf(type).ends_with_nothing_to_do) {
    return {};
  }
  const std::array<llvm::Value*, 1> args{value};
  return module_->EmitLifecycle(builder_, type, LifecycleStep::kDestroy, args);
}

auto CodeGenFunction::CopyValue(
    lir::TypeId type, llvm::Value* value, llvm::Value* out)
    -> diag::Result<llvm::Value*> {
  if (module_->Unit().types.Get(type).IsMoveOnly()) {
    throw InternalError(
        "llvm codegen: a value with no copy is copied -- please report this "
        "as a bug");
  }
  const std::array<llvm::Value*, 2> args{value, out};
  auto copied =
      module_->EmitLifecycle(builder_, type, LifecycleStep::kCopy, args);
  if (!copied) {
    return std::unexpected(std::move(copied.error()));
  }
  return out;
}

auto CodeGenFunction::AssignValue(
    lir::TypeId type, llvm::Value* value, llvm::Value* out)
    -> diag::Result<void> {
  const std::array<llvm::Value*, 2> args{out, value};
  return module_->EmitLifecycle(builder_, type, LifecycleStep::kAssign, args);
}

auto CodeGenFunction::RelocateValue(
    lir::TypeId type, llvm::Value* value, llvm::Value* out)
    -> diag::Result<void> {
  const std::array<llvm::Value*, 2> args{value, out};
  auto moved =
      module_->EmitLifecycle(builder_, type, LifecycleStep::kMove, args);
  if (!moved) {
    return moved;
  }
  return EndValue(type, value);
}

auto CodeGenFunction::ComponentAddress(
    lir::TypeId tuple, llvm::Value* value, std::size_t index) -> llvm::Value* {
  return builder_.CreateConstInBoundsGEP1_64(
      builder_.getInt8Ty(), value,
      module_->Types().LayoutOfTuple(tuple).offsets.at(index));
}

auto CodeGenFunction::Variables() -> diag::Result<const RecordLayout*> {
  if (variables_.has_value()) {
    return &*variables_;
  }
  auto record = module_->PlaceMembers(
      fn_->variables, MemberSlotRole::kVariable, RecordLayout{}, "a variable");
  if (!record) {
    return std::unexpected(std::move(record.error()));
  }
  return &variables_.emplace(*std::move(record));
}

auto CodeGenFunction::LowerOpenVariables() -> diag::Result<llvm::Value*> {
  auto record = Variables();
  if (!record) {
    return std::unexpected(std::move(record.error()));
  }
  llvm::IRBuilder<> at(frame_storage_point_);
  llvm::AllocaInst* storage = at.CreateAlloca(
      llvm::ArrayType::get(at.getInt8Ty(), (*record)->size), nullptr,
      "variables");
  storage->setAlignment(llvm::Align((*record)->align));
  if (auto begun = module_->BeginMembers(builder_, storage, **record); !begun) {
    return std::unexpected(std::move(begun.error()));
  }
  return storage;
}

auto CodeGenFunction::LowerVariableAddress(
    const lir::VariableAddressInstr& reached) -> diag::Result<llvm::Value*> {
  auto record = Variables();
  if (!record) {
    return std::unexpected(std::move(record.error()));
  }
  auto storage = LowerOperand(reached.variables);
  if (!storage) {
    return std::unexpected(std::move(storage.error()));
  }
  return builder_.CreateConstInBoundsGEP1_64(
      builder_.getInt8Ty(), *storage,
      (*record)->offsets.at(reached.position.value));
}

auto CodeGenFunction::LowerCloseVariables(
    const lir::CloseVariablesInstr& closed) -> diag::Result<llvm::Value*> {
  auto record = Variables();
  if (!record) {
    return std::unexpected(std::move(record.error()));
  }
  auto storage = LowerOperand(closed.variables);
  if (!storage) {
    return std::unexpected(std::move(storage.error()));
  }
  auto ended = module_->EndMembers(builder_, *storage, **record);
  if (!ended) {
    return std::unexpected(std::move(ended.error()));
  }
  return nullptr;
}

void CodeGenFunction::OpenCoroutine() {
  llvm::Module& mod = module_->Module();
  llvm::LLVMContext& ctx = module_->Context();
  llvm::PointerType* ptr_ty = builder_.getPtrTy();
  llvm::Type* size_ty = builder_.getInt64Ty();

  // The coroutine passes split only a body that declares itself one.
  value_->setPresplitCoroutine();

  llvm::Constant* null_ptr = llvm::ConstantPointerNull::get(ptr_ty);
  coro_id_ = builder_.CreateCall(
      llvm::Intrinsic::getOrInsertDeclaration(&mod, llvm::Intrinsic::coro_id),
      {builder_.getInt32(0), null_ptr, null_ptr, null_ptr});
  // A frame is storage like any other the program asks for, so it comes from
  // the allocation function the rest comes from, and a request the host
  // refuses ends the program where every other refused request does.
  const auto frame_size = [&] {
    return builder_.CreateCall(
        llvm::Intrinsic::getOrInsertDeclaration(
            &mod, llvm::Intrinsic::coro_size, {size_ty}),
        {});
  };
  llvm::Value* memory =
      builder_.CreateCall(module_->OperatorNew(), {frame_size()});
  coro_handle_ = builder_.CreateCall(
      llvm::Intrinsic::getOrInsertDeclaration(
          &mod, llvm::Intrinsic::coro_begin),
      {coro_id_, memory});

  coro_final_ = llvm::BasicBlock::Create(ctx, "coro.final", value_);
  coro_cleanup_ = llvm::BasicBlock::Create(ctx, "coro.cleanup", value_);
  coro_end_ = llvm::BasicBlock::Create(ctx, "coro.end", value_);

  builder_.SetInsertPoint(coro_cleanup_);
  llvm::Value* frame = builder_.CreateCall(
      llvm::Intrinsic::getOrInsertDeclaration(&mod, llvm::Intrinsic::coro_free),
      {coro_id_, coro_handle_});
  builder_.CreateCall(module_->SizedOperatorDelete(), {frame, frame_size()});
  builder_.CreateBr(coro_end_);

  builder_.SetInsertPoint(coro_end_);
  builder_.CreateCall(
      llvm::Intrinsic::getOrInsertDeclaration(&mod, llvm::Intrinsic::coro_end),
      {coro_handle_, builder_.getInt1(false),
       llvm::ConstantTokenNone::get(module_->Context())});
  builder_.CreateRet(coro_handle_);

  // A body that completes, by returning or by departing, suspends one final
  // time, so its owner still reads the handle as done before destroying it.
  // Every scope it opened has already ended, so there is nothing left to run on
  // the way out.
  builder_.SetInsertPoint(coro_final_);
  EmitCoroutineSuspend(nullptr, coro_cleanup_, true);
}

void CodeGenFunction::EmitCoroutineSuspend(
    llvm::BasicBlock* resume, llvm::BasicBlock* abandoned, bool is_final) {
  llvm::Module& mod = module_->Module();
  llvm::Value* save =
      is_final ? llvm::cast<llvm::Value>(
                     llvm::ConstantTokenNone::get(module_->Context()))
               : llvm::cast<llvm::Value>(builder_.CreateCall(
                     llvm::Intrinsic::getOrInsertDeclaration(
                         &mod, llvm::Intrinsic::coro_save),
                     {coro_handle_}));
  llvm::Value* arm = builder_.CreateCall(
      llvm::Intrinsic::getOrInsertDeclaration(
          &mod, llvm::Intrinsic::coro_suspend),
      {save, builder_.getInt1(is_final)});
  // The suspension's three arms: the caller regains control (the default), the
  // body resumes where it left off, or the execution is ended where it stands.
  llvm::SwitchInst* arms = builder_.CreateSwitch(arm, coro_end_, 2);
  if (resume != nullptr) {
    arms->addCase(builder_.getInt8(0), resume);
  }
  arms->addCase(builder_.getInt8(1), abandoned);
}

auto CodeGenFunction::LowerTerminatorInto(const lir::Terminator& terminator)
    -> diag::Result<void> {
  return std::visit(
      Overloaded{
          [&](const lir::ReturnTerm& ret) -> diag::Result<void> {
            if (IsCoroutine()) {
              // A coroutine completes through its final suspension; its owner
              // reads completion from the handle, never from a returned value.
              builder_.CreateBr(coro_final_);
              return {};
            }
            if (!ret.value.has_value()) {
              builder_.CreateRetVoid();
              return {};
            }
            auto value = LowerOperand(*ret.value);
            if (!value) {
              return std::unexpected(std::move(value.error()));
            }
            // An answer lying in storage is built in the storage the caller
            // gave, which the body answers with as an entry does.
            if (module_->Types().LiesInStorage(fn_->result_type)) {
              llvm::Value* out = value_->getArg(value_->arg_size() - 1);
              auto relocated = RelocateValue(fn_->result_type, *value, out);
              if (!relocated) {
                return relocated;
              }
              // The bytes the answer was made in go out of use with the
              // return, once the answer lies where the caller reads it.
              if (const auto* made = std::get_if<lir::Use>(&*ret.value)) {
                DeallocateBytes(made->value);
              }
              builder_.CreateRet(out);
              return {};
            }
            builder_.CreateRet(*value);
            return {};
          },
          [&](const lir::BranchTerm& br) -> diag::Result<void> {
            builder_.CreateBr(blocks_[br.target.value]);
            return {};
          },
          [&](const lir::CondBranchTerm& br) -> diag::Result<void> {
            auto condition = LowerOperand(br.condition);
            if (!condition) {
              return std::unexpected(std::move(condition.error()));
            }
            builder_.CreateCondBr(
                *condition, blocks_[br.if_true.value],
                blocks_[br.if_false.value]);
            return {};
          },
          [&](const lir::SuspendTerm& s) -> diag::Result<void> {
            // The wakeup source was registered by the calls preceding this
            // terminator; the suspension only hands control back and names the
            // two blocks control can reach from here.
            EmitCoroutineSuspend(
                blocks_[s.resume.value], blocks_[s.abandoned.value], false);
            return {};
          },
          [&](const lir::AbandonTerm&) -> diag::Result<void> {
            builder_.CreateBr(coro_cleanup_);
            return {};
          },
          [&](const lir::DepartTerm& depart) -> diag::Result<void> {
            auto departure = LowerOperand(depart.departure);
            if (!departure) {
              return std::unexpected(std::move(departure.error()));
            }
            const std::array<llvm::Value*, 1> args{*departure};
            if (IsCoroutine()) {
              // A coroutine hands the departure to the activation it completes
              // and leaves through its final suspension, as a return does.
              // That suspension is where releasing the frame finds it stopped;
              // a frame unwound out of is found stopped wherever it last
              // waited, and releasing it runs that wait's abandonment again.
              CallEntry(
                  RuntimeSymbol(RuntimeOp::kSettleDeparture),
                  builder_.getVoidTy(), Addresses(1), args);
              builder_.CreateBr(coro_final_);
              return {};
            }
            CallEntry(
                RuntimeSymbol(lir::ControlEffectTarget::Op::kDeclineDeparture),
                builder_.getVoidTy(), Addresses(1), args);
            builder_.CreateUnreachable();
            return {};
          },
          [&](const lir::UnreachableTerm&) -> diag::Result<void> {
            builder_.CreateUnreachable();
            return {};
          },
          [&](const lir::DepartingCallInstr& call) -> diag::Result<void> {
            const lir::TypeId result_type = fn_->values.Get(call.result).type;
            llvm::Value* const out =
                lir::CallMakesValue(call.target)
                    ? StorageForResult(call.result, result_type)
                    : nullptr;
            const lir::CallInstr stated{
                .target = call.target, .args = call.args};
            invoke_dest_ = InvokeDest{
                .normal = blocks_[call.returned.value],
                .unwind = blocks_[call.landing.value]};
            auto answered = LowerCall(stated, result_type, out);
            invoke_dest_.reset();
            if (!answered) {
              return std::unexpected(std::move(answered.error()));
            }
            values_[call.result] = *answered;
            // A call emitted as instructions of this body departs from
            // nowhere, so control goes on to where the call returns.
            if (!builder_.GetInsertBlock()->hasTerminator()) {
              builder_.CreateBr(blocks_[call.returned.value]);
            }
            return {};
          }},
      terminator.data);
}

auto CodeGenFunction::OperandType(const lir::Operand& operand) const
    -> lir::TypeId {
  return lir::OperandType(*fn_, operand);
}

auto CodeGenFunction::DomainOf(lir::TypeId type) const
    -> diag::Result<support::ValueDomain> {
  return module_->DomainOf(type);
}

}  // namespace lyra::backend::llvm_backend
