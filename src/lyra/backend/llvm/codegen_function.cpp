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
#include "lyra/backend/llvm/codegen_value_type.hpp"
#include "lyra/backend/llvm/runtime_entry.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/lir/compilation_unit.hpp"
#include "lyra/runtime/object_layout.hpp"
#include "lyra/support/runtime_object.hpp"
#include "lyra/support/value_domain.hpp"

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
  // owned type holds the value itself, as a C++ local of class type does. The
  // return place of an answer lying in storage is the storage the caller gave,
  // as clang's return value is the address an indirect result arrives in.
  builder_.SetInsertPoint(entry);
  frame_storage_point_ =
      builder_.CreateAlloca(builder_.getInt8Ty(), nullptr, "frame.storage");
  for (const lir::ValueId id : fn_->values.Ids()) {
    const lir::Local& local = fn_->values.Get(id);
    if (!local.NamesStorage()) {
      continue;
    }
    const auto own_storage = [&] -> llvm::Value* {
      llvm::Value* owned = StorageFor(local.type);
      return owned != nullptr ? owned
                              : FrameStorage(module_->Types().Map(local.type));
    };
    if (id != fn_->return_place) {
      values_.emplace(id, own_storage());
      continue;
    }
    values_.emplace(
        id,
        std::visit(
            Overloaded{
                [&](const ReturnDirect&) { return own_storage(); },
                [&](const ReturnIndirect&) -> llvm::Value* {
                  return value_->getArg(value_->arg_size() - 1);
                }},
            CallArranger::ReturnOfValue(module_->Types(), fn_->result_type)));
  }
  if (IsCoroutine()) {
    // The ramp only places the arguments in the frame. Where the body stops
    // before its first statement, and what ending it there owes, is the body's
    // own to state.
    builder_.CreateBr(blocks_.front());
  }

  for (std::uint32_t i = 0; i < fn_->blocks.size(); ++i) {
    builder_.SetInsertPoint(blocks_[i]);
    const lir::BasicBlock& block = fn_->blocks[i];
    for (const lir::Instr& instr : block.instrs) {
      auto lowered = LowerInstrInto(instr);
      if (!lowered) {
        return std::unexpected(std::move(lowered.error()));
      }
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
      at.CreateAlloca(llvm::ArrayType::get(at.getInt8Ty(), layout.layout.size));
  storage->setAlignment(llvm::Align(layout.layout.align));
  return storage;
}

auto CodeGenFunction::ObjectStorage(support::RuntimeObject object)
    -> llvm::Value* {
  return LaidOutStorage(runtime::LayoutOf(object));
}

auto CodeGenFunction::StorageFor(lir::TypeId type) -> llvm::Value* {
  if (!IsBuiltInStorage(type)) {
    return nullptr;
  }
  return LaidOutStorage(module_->Types().LayoutOf(type));
}

auto CodeGenFunction::IsBuiltInStorage(lir::TypeId type) const -> bool {
  return module_->Types().HasAggregateEvaluationKind(type) ||
         module_->Types().IsReadAsView(type);
}

auto CodeGenFunction::ResolveDestination(const lir::Destination& destination)
    -> diag::Result<DestinationRef> {
  return std::visit(
      Overloaded{
          [&](const lir::Transient& defined) -> diag::Result<DestinationRef> {
            return PendingOperand{.value = defined.value};
          },
          [&](const lir::Place& place) -> diag::Result<DestinationRef> {
            auto address = ResolvePlaceAddress(place, Access::kWrite);
            if (!address) {
              return std::unexpected(std::move(address.error()));
            }
            return PlaceRef{
                .address = *address,
                .type = ReachedType(place, std::ssize(place.chain))};
          }},
      destination);
}

auto CodeGenFunction::TypeOf(const DestinationRef& destination) const
    -> lir::TypeId {
  return std::visit(
      Overloaded{
          [&](const PendingOperand& pending) {
            return fn_->values.Get(pending.value).type;
          },
          [](const PlaceRef& place) { return place.type; }},
      destination);
}

auto CodeGenFunction::EnsureDestination(const DestinationRef& destination)
    -> llvm::Value* {
  return std::visit(
      Overloaded{
          [&](const PendingOperand& pending) {
            return StorageFor(fn_->values.Get(pending.value).type);
          },
          [&](const PlaceRef& place) -> llvm::Value* {
            if (!IsBuiltInStorage(place.type)) {
              return nullptr;
            }
            return place.address;
          }},
      destination);
}

void CodeGenFunction::StoreInto(
    const DestinationRef& destination, llvm::Value* value,
    llvm::IRBuilderBase& at) {
  std::visit(
      Overloaded{
          [&](const PendingOperand& pending) {
            values_.emplace(pending.value, value);
          },
          // What lies in storage of its own, and a sequence of machine values,
          // was built where the place is.
          [&](const PlaceRef& place) {
            if (!IsBuiltInStorage(place.type)) {
              at.CreateStore(value, place.address);
            }
          }},
      destination);
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
  // A pointer owning what it points at is that address where it is an operand.
  if (module_->Unit().types.Get(type).IsMoveOnly()) {
    return module_->EndOwnedStorage(builder_, type, value);
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
    lir::TypeId aggregate, llvm::Value* value, std::size_t index)
    -> llvm::Value* {
  return std::visit(
      Overloaded{
          [&](const ProductLayout* product) -> llvm::Value* {
            return builder_.CreateConstInBoundsGEP1_64(
                builder_.getInt8Ty(), value, product->offsets.at(index));
          },
          // Every member of a union lies at its start (LRM 7.3), as C reaches
          // one by retyping the union's own address.
          [&](const UnionLayout*) -> llvm::Value* { return value; },
          [&](const ArrayLayout* array) -> llvm::Value* {
            return builder_.CreateConstInBoundsGEP1_64(
                builder_.getInt8Ty(), value, array->stride * index);
          }},
      module_->Types().FieldsOf(aggregate));
}

auto CodeGenFunction::Variables() -> diag::Result<const PlacedMembers*> {
  if (variables_.has_value()) {
    return &*variables_;
  }
  auto placed = module_->PlaceMembers(
      fn_->variables, MemberSlotRole::kVariable, PlacedMembers{}, "a variable");
  if (!placed) {
    return std::unexpected(std::move(placed.error()));
  }
  return &variables_.emplace(*std::move(placed));
}

auto CodeGenFunction::LowerOpenVariables() -> diag::Result<llvm::Value*> {
  auto placed = Variables();
  if (!placed) {
    return std::unexpected(std::move(placed.error()));
  }
  llvm::IRBuilder<> at(frame_storage_point_);
  llvm::AllocaInst* storage = at.CreateAlloca(
      llvm::ArrayType::get(at.getInt8Ty(), (*placed)->layout.size), nullptr,
      "variables");
  storage->setAlignment(llvm::Align((*placed)->layout.align));
  if (auto begun = module_->BeginMembers(builder_, storage, **placed); !begun) {
    return std::unexpected(std::move(begun.error()));
  }
  return storage;
}

auto CodeGenFunction::LowerVariableAddress(
    const lir::VariableAddressInstr& reached) -> diag::Result<llvm::Value*> {
  auto placed = Variables();
  if (!placed) {
    return std::unexpected(std::move(placed.error()));
  }
  auto storage = LowerOperand(reached.variables);
  if (!storage) {
    return std::unexpected(std::move(storage.error()));
  }
  return builder_.CreateConstInBoundsGEP1_64(
      builder_.getInt8Ty(), *storage,
      (*placed)->offsets.at(reached.position.value));
}

auto CodeGenFunction::LowerCloseVariables(
    const lir::CloseVariablesInstr& closed) -> diag::Result<llvm::Value*> {
  auto placed = Variables();
  if (!placed) {
    return std::unexpected(std::move(placed.error()));
  }
  auto storage = LowerOperand(closed.variables);
  if (!storage) {
    return std::unexpected(std::move(storage.error()));
  }
  auto ended = module_->EndMembers(builder_, *storage, **placed);
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
          [&](const lir::ReturnTerm&) -> diag::Result<void> {
            if (IsCoroutine()) {
              // A coroutine completes through its final suspension; its owner
              // reads completion from the handle, never from a returned value.
              builder_.CreateBr(coro_final_);
              return {};
            }
            if (!fn_->return_place.has_value()) {
              builder_.CreateRetVoid();
              return {};
            }
            // An answer lying in storage is already in the storage the caller
            // gave, which the body answers with as an entry does.
            llvm::Value* answer = values_.at(*fn_->return_place);
            builder_.CreateRet(
                std::visit(
                    Overloaded{
                        [&](const ReturnDirect& direct) -> llvm::Value* {
                          return builder_.CreateLoad(direct.type, answer);
                        },
                        [&](const ReturnIndirect&) { return answer; }},
                    CallArranger::ReturnOfValue(
                        module_->Types(), fn_->result_type)));
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
          [&](const lir::SwitchTerm& s) -> diag::Result<void> {
            auto discriminant = LowerOperand(s.discriminant);
            if (!discriminant) {
              return std::unexpected(std::move(discriminant.error()));
            }
            auto* const type =
                llvm::cast<llvm::IntegerType>((*discriminant)->getType());
            llvm::SwitchInst* const dispatch = builder_.CreateSwitch(
                *discriminant, blocks_[s.otherwise.value],
                static_cast<unsigned>(s.targets.size()));
            for (const lir::SwitchTarget& target : s.targets) {
              dispatch->addCase(
                  llvm::ConstantInt::getSigned(type, target.value),
                  blocks_[target.target.value]);
            }
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
            auto destination = ResolveDestination(call.destination);
            if (!destination) {
              return std::unexpected(std::move(destination.error()));
            }
            const lir::CallInstr stated{
                .target = call.target, .args = call.args};
            invoke_dest_ = InvokeDest{
                .normal = blocks_[call.returned.value],
                .unwind = blocks_[call.landing.value]};
            auto answered = LowerCall(stated, *destination);
            invoke_dest_.reset();
            if (!answered) {
              return std::unexpected(std::move(answered.error()));
            }
            // An answer exists only where the call returned to, so that is
            // where it is put.
            llvm::IRBuilder<> at(blocks_[call.returned.value]);
            StoreInto(*destination, *answered, at);
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
