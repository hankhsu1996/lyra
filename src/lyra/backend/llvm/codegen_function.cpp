#include "lyra/backend/llvm/codegen_function.hpp"

#include <array>
#include <cstdint>
#include <format>
#include <optional>
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
#include "lyra/backend/llvm/runtime_entry.hpp"
#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/diag/diag_code.hpp"
#include "lyra/lir/compilation_unit.hpp"
#include "lyra/lir/place_query.hpp"
#include "lyra/support/runtime_object.hpp"
#include "lyra/support/value_domain.hpp"

namespace lyra::backend::llvm_backend {

CodeGenFunction::CodeGenFunction(CodeGenModule& module, lir::FunctionId id)
    : module_(&module),
      id_(id),
      fn_(&module.Unit().functions.Get(id)),
      value_(module.UnitFunction(id)),
      builder_(module.Context()) {
}

auto CodeGenFunction::IsCoroutine() const -> bool {
  return module_->Unit().types.Get(fn_->result_type).Is<lir::CoroutineType>();
}

auto CodeGenFunction::BindParameters() -> void {
  const std::size_t positional = module_->IsScopeConstruction(id_)
                                     ? kScopeConstructSharedParams
                                     : fn_->params.size();
  for (std::size_t i = 0; i < positional; ++i) {
    values_.emplace(fn_->params[i], value_->getArg(i));
  }
}

auto CodeGenFunction::BindConstructionArguments() -> void {
  if (!module_->IsScopeConstruction(id_)) {
    return;
  }
  llvm::Value* arguments =
      builder_.CreateExtractValue(value_->getArg(value_->arg_size() - 1), {0});
  for (std::size_t i = kScopeConstructSharedParams; i < fn_->params.size();
       ++i) {
    const lir::ValueId param = fn_->params[i];
    llvm::Type* type = module_->Types().Map(fn_->values.Get(param).type);
    llvm::Value* slot = builder_.CreateConstInBoundsGEP1_64(
        module_->Types().Ptr(), arguments,
        static_cast<std::uint64_t>(i - kScopeConstructSharedParams));
    values_.emplace(param, builder_.CreateLoad(type, slot));
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
  // owned type holds the object itself, as a C++ local of class type does.
  builder_.SetInsertPoint(entry);
  BindConstructionArguments();
  frame_storage_point_ =
      builder_.CreateAlloca(builder_.getInt8Ty(), nullptr, "frame.storage");
  LayOutRuntimeParameters();
  for (const lir::ValueId id : fn_->values.Ids()) {
    const lir::Local& local = fn_->values.Get(id);
    if (!local.NamesStorage()) {
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
    // What the call that leads here left owed is ended as the block opens --
    // after the landing pad, where the block is a landing, since nothing may
    // stand ahead of that.
    OwedOnEntry owed;
    if (const auto found = owed_on_entry_.find(blocks_[i]);
        found != owed_on_entry_.end()) {
      owed = std::move(found->second);
      owed_on_entry_.erase(found);
    }
    const auto settle_owed = [&] {
      for (const Receipt& receipt : owed.receipts) {
        Received(receipt.type, receipt.received, receipt.out);
      }
      for (const Lent& lent : owed.lent) {
        EndObject(lent.object, lent.value);
      }
      owed = {};
    };
    for (const lir::Instr& instr : block.instrs) {
      if (!std::holds_alternative<lir::ReceiveDepartureInstr>(instr.data)) {
        settle_owed();
      }
      auto lowered = LowerInstr(instr);
      if (!lowered) {
        return std::unexpected(std::move(lowered.error()));
      }
      values_.emplace(instr.result, *lowered);
      EndLent();
    }
    settle_owed();
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
  return LaidOutStorage(support::LayoutOf(object));
}

auto CodeGenFunction::StorageFor(lir::TypeId type) -> llvm::Value* {
  if (!module_->Unit().types.Get(type).IsOwnedValue()) {
    return nullptr;
  }
  return LaidOutStorage(module_->Types().StorageOf(type));
}

auto CodeGenFunction::BuildInto(
    std::string_view symbol, std::vector<llvm::Value*> args, llvm::Value* out)
    -> llvm::Value* {
  args.push_back(out);
  builder_.CreateCall(Entry(symbol, module_->Types().Ptr(), args), args);
  return out;
}

void CodeGenFunction::EndObject(
    support::RuntimeObject object, llvm::Value* value) {
  if (support::LayoutOf(object).ends_with_nothing_to_do) {
    return;
  }
  const std::array<llvm::Value*, 1> args{value};
  builder_.CreateCall(
      Entry(
          RuntimeSymbol(object, RuntimeOp::kDestroy), module_->Types().Void(),
          args),
      args);
}

void CodeGenFunction::AssignObject(
    support::RuntimeObject object, llvm::Value* storage, llvm::Value* value) {
  const std::array<llvm::Value*, 2> args{storage, value};
  builder_.CreateCall(
      Entry(
          RuntimeSymbol(object, RuntimeOp::kAssign), module_->Types().Void(),
          args),
      args);
}

void CodeGenFunction::RelocateObject(
    support::RuntimeObject object, llvm::Value* value, llvm::Value* out) {
  BuildInto(RuntimeSymbol(object, RuntimeOp::kMove), {value}, out);
  EndObject(object, value);
}

auto CodeGenFunction::ComponentAddress(
    lir::TypeId product, llvm::Value* value, std::size_t index)
    -> llvm::Value* {
  return builder_.CreateConstInBoundsGEP1_64(
      builder_.getInt8Ty(), value,
      module_->Types().LayoutOfProduct(product).offsets.at(index));
}

void CodeGenFunction::EndValue(lir::TypeId type, llvm::Value* value) {
  if (!module_->Unit().types.Get(type).IsProduct()) {
    EndObject(ObjectOf(type), value);
    return;
  }
  const ProductLayout& layout = module_->Types().LayoutOfProduct(type);
  for (std::size_t i = 0; i < layout.components.size(); ++i) {
    EndValue(layout.components[i], ComponentAddress(type, value, i));
  }
}

auto CodeGenFunction::CopyValue(
    lir::TypeId type, llvm::Value* value, llvm::Value* out) -> llvm::Value* {
  if (!module_->Unit().types.Get(type).IsProduct()) {
    return CopyObject(ObjectOf(type), value, out);
  }
  const ProductLayout& layout = module_->Types().LayoutOfProduct(type);
  for (std::size_t i = 0; i < layout.components.size(); ++i) {
    CopyValue(
        layout.components[i], ComponentAddress(type, value, i),
        ComponentAddress(type, out, i));
  }
  return out;
}

void CodeGenFunction::RelocateValue(
    lir::TypeId type, llvm::Value* value, llvm::Value* out) {
  if (!module_->Unit().types.Get(type).IsProduct()) {
    RelocateObject(ObjectOf(type), value, out);
    return;
  }
  const ProductLayout& layout = module_->Types().LayoutOfProduct(type);
  for (std::size_t i = 0; i < layout.components.size(); ++i) {
    RelocateValue(
        layout.components[i], ComponentAddress(type, value, i),
        ComponentAddress(type, out, i));
  }
}

auto CodeGenFunction::ToRuntime(lir::TypeId type, llvm::Value* value)
    -> llvm::Value* {
  const lir::TypePool& types = module_->Unit().types;
  // A run of values handed over together is read one element at a time, so a
  // run of products crosses as a run of their erased forms.
  if (const auto* run = types.Get(type).As<lir::MachineArrayType>();
      run != nullptr && types.Get(run->element).IsProduct()) {
    llvm::Value* elements = builder_.CreateExtractValue(value, {0});
    std::vector<llvm::Value*> erased;
    erased.reserve(run->size);
    for (std::uint64_t i = 0; i < run->size; ++i) {
      llvm::Value* element = builder_.CreateLoad(
          module_->Types().Ptr(), builder_.CreateConstInBoundsGEP1_64(
                                      module_->Types().Ptr(), elements, i));
      erased.push_back(ToRuntime(run->element, element));
    }
    return SpanOver(erased, module_->Types().Ptr());
  }
  if (!types.Get(type).IsProduct()) {
    return value;
  }
  llvm::Value* erased = ObjectStorage(support::ValueDomain::kTuple);
  ToRuntimeInto(type, value, erased);
  lent_.push_back(
      Lent{.object = support::ValueDomain::kTuple, .value = erased});
  return erased;
}

void CodeGenFunction::ToRuntimeInto(
    lir::TypeId product, llvm::Value* value, llvm::Value* out) {
  const ProductLayout& layout = module_->Types().LayoutOfProduct(product);
  std::vector<llvm::Value*> boxed;
  boxed.reserve(layout.components.size());
  for (std::size_t i = 0; i < layout.components.size(); ++i) {
    const lir::TypeId component = layout.components[i];
    auto domain = DomainOf(component);
    if (!domain) {
      throw InternalError(
          "llvm codegen: a product component is no value of a runtime domain "
          "-- please report this as a bug");
    }
    boxed.push_back(Box(
        *domain, ToRuntime(component, ComponentAddress(product, value, i))));
  }
  BuildInto(
      RuntimeSymbol(support::ValueDomain::kTuple, RuntimeOp::kMake),
      {SpanOver(boxed, module_->Types().Ptr())}, out);
}

// Each component comes out in its own domain's form, so a component that is
// itself a product comes out erased and is laid out in turn, from storage of
// its own that nothing else holds.
void CodeGenFunction::FromRuntime(
    lir::TypeId type, llvm::Value* erased, llvm::Value* out) {
  const ProductLayout& layout = module_->Types().LayoutOfProduct(type);
  for (std::size_t i = 0; i < layout.components.size(); ++i) {
    const lir::TypeId component = layout.components[i];
    llvm::Value* index = llvm::ConstantInt::get(
        builder_.getInt64Ty(), static_cast<std::uint64_t>(i));
    llvm::Value* into = ComponentAddress(type, out, i);
    llvm::Value* extracted = BuildInto(
        RuntimeSymbol(support::ValueDomain::kTuple, RuntimeOp::kExtract),
        {erased, index},
        ReceivingStorage(ProductCrossing::kErased, component, into));
    Received(component, extracted, into);
  }
}

auto CodeGenFunction::ReceivingStorage(
    ProductCrossing crossing, lir::TypeId type, llvm::Value* out)
    -> llvm::Value* {
  switch (crossing) {
    case ProductCrossing::kLaidOut:
      return out;
    case ProductCrossing::kErased:
      if (out == nullptr || !module_->Unit().types.Get(type).IsProduct()) {
        return out;
      }
      return ObjectStorage(support::ValueDomain::kTuple);
  }
  throw InternalError("llvm codegen: unknown product crossing");
}

auto CodeGenFunction::Received(
    lir::TypeId type, llvm::Value* received, llvm::Value* out) -> llvm::Value* {
  if (received == out) {
    return out;
  }
  FromRuntime(type, received, out);
  EndObject(support::ValueDomain::kTuple, received);
  return out;
}

auto CodeGenFunction::Box(support::ValueDomain domain, llvm::Value* value)
    -> llvm::Value* {
  llvm::Value* box = BuildInto(
      RuntimeSymbol(domain, RuntimeOp::kValueBox), {value},
      ObjectStorage(support::LibraryObject::kErasedValue));
  lent_.push_back(
      Lent{.object = support::LibraryObject::kErasedValue, .value = box});
  return box;
}

void CodeGenFunction::EndLent() {
  for (const Lent& lent : lent_) {
    EndObject(lent.object, lent.value);
  }
  lent_.clear();
}

void CodeGenFunction::OweLentOnEntry(
    std::initializer_list<lir::BlockId> successors) {
  for (const lir::BlockId successor : successors) {
    std::vector<Lent>& owed = owed_on_entry_[blocks_[successor.value]].lent;
    owed.insert(owed.end(), lent_.begin(), lent_.end());
  }
  lent_.clear();
}

void CodeGenFunction::LayOutRuntimeParameters() {
  if (!module_->IsClosureBody(id_)) {
    return;
  }
  for (const lir::ValueId param : fn_->params) {
    const lir::TypeId type = fn_->values.Get(param).type;
    if (!module_->Unit().types.Get(type).IsProduct()) {
      continue;
    }
    llvm::Value* laid_out = StorageFor(type);
    FromRuntime(type, values_.at(param), laid_out);
    values_[param] = laid_out;
    laid_out_parameters_.emplace_back(type, laid_out);
  }
}

void CodeGenFunction::EndLaidOutParameters() {
  for (const auto& [type, laid_out] : laid_out_parameters_) {
    EndValue(type, laid_out);
  }
}

auto CodeGenFunction::ObjectOf(lir::TypeId type) const
    -> support::RuntimeObject {
  const lir::Type& ty = module_->Unit().types.Get(type);
  const std::optional<support::RuntimeObject> object = ty.HeldObject();
  if (!object.has_value() || ty.IsProduct()) {
    throw InternalError(
        "llvm codegen: an owned value's operation names a type that is no "
        "runtime object -- please report this as a bug");
  }
  return *object;
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
  llvm::Value* size = builder_.CreateCall(
      llvm::Intrinsic::getOrInsertDeclaration(
          &mod, llvm::Intrinsic::coro_size, {size_ty}),
      {});
  llvm::Value* memory = builder_.CreateCall(
      mod.getOrInsertFunction("malloc", ptr_ty, size_ty), {size});
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
  builder_.CreateCall(
      mod.getOrInsertFunction("free", builder_.getVoidTy(), ptr_ty), {frame});
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
              EndLaidOutParameters();
              builder_.CreateRetVoid();
              return {};
            }
            auto value = LowerOperand(*ret.value);
            if (!value) {
              return std::unexpected(std::move(value.error()));
            }
            // An owned answer is built in the storage the caller gave, which
            // the body answers with as an entry does -- and in the form the
            // caller holds it in, which for the runtime is the erased one.
            if (module_->Unit().types.Get(fn_->result_type).IsOwnedValue()) {
              llvm::Value* out = value_->getArg(value_->arg_size() - 1);
              if (module_->IsClosureBody(id_) &&
                  module_->Unit().types.Get(fn_->result_type).IsProduct()) {
                ToRuntimeInto(fn_->result_type, *value, out);
                EndValue(fn_->result_type, *value);
                EndLent();
              } else {
                RelocateValue(fn_->result_type, *value, out);
              }
              EndLaidOutParameters();
              builder_.CreateRet(out);
              return {};
            }
            EndLaidOutParameters();
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
            EndLaidOutParameters();
            if (IsCoroutine()) {
              // A coroutine hands the departure to the activation it completes
              // and leaves through its final suspension, as a return does.
              // That suspension is where releasing the frame finds it stopped;
              // a frame unwound out of is found stopped wherever it last
              // waited, and releasing it runs that wait's abandonment again.
              builder_.CreateCall(
                  Entry(
                      RuntimeSymbol(RuntimeOp::kSettleDeparture),
                      builder_.getVoidTy(), args),
                  args);
              builder_.CreateBr(coro_final_);
              return {};
            }
            builder_.CreateCall(
                Entry(
                    RuntimeSymbol(
                        lir::ControlEffectTarget::Op::kDeclineDeparture),
                    builder_.getVoidTy(), args),
                args);
            builder_.CreateUnreachable();
            return {};
          },
          [&](const lir::UnreachableTerm&) -> diag::Result<void> {
            builder_.CreateUnreachable();
            return {};
          },
          [&](const lir::DepartingCallInstr& call) -> diag::Result<void> {
            const lir::TypeId result_type = fn_->values.Get(call.result).type;
            llvm::Value* const out = lir::CallMakesValue(call.target)
                                         ? StorageFor(result_type)
                                         : nullptr;
            llvm::Value* const receiving = ReceivingStorage(
                ProductCrossingOf(call.target), result_type, out);
            auto resolved = ResolveCall(
                lir::CallInstr{.target = call.target, .args = call.args},
                result_type, receiving);
            if (!resolved) {
              return std::unexpected(std::move(resolved.error()));
            }
            llvm::Value* invoked = builder_.CreateInvoke(
                resolved->callee, blocks_[call.returned.value],
                blocks_[call.landing.value], resolved->args);
            values_[call.result] = out != nullptr ? out : invoked;
            if (receiving != out) {
              owed_on_entry_[blocks_[call.returned.value]].receipts.push_back(
                  Receipt{
                      .type = result_type, .received = receiving, .out = out});
            }
            OweLentOnEntry({call.returned, call.landing});
            return {};
          }},
      terminator.data);
}

auto CodeGenFunction::OperandType(const lir::Operand& operand) const
    -> lir::TypeId {
  const std::optional<lir::TypeId> type = lir::OperandType(*fn_, operand);
  if (!type) {
    throw InternalError("llvm codegen: a code reference has no type");
  }
  return *type;
}

auto CodeGenFunction::DomainOf(lir::TypeId type) const
    -> diag::Result<support::ValueDomain> {
  const std::optional<support::ValueDomain> domain =
      ValueDomainOf(module_->Unit(), type);
  if (!domain) {
    return diag::Fail(
        diag::DiagCode::kUnsupportedTypeKind,
        std::format(
            "llvm codegen: a value of type {} has no runtime library "
            "realization",
            module_->Unit().types.Get(type).KindName()));
  }
  return *domain;
}

}  // namespace lyra::backend::llvm_backend
