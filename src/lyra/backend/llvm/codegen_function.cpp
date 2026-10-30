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
#include "lyra/diag/diagnostic.hpp"
#include "lyra/lir/compilation_unit.hpp"
#include "lyra/lir/place_query.hpp"
#include "lyra/lir/type.hpp"
#include "lyra/lir/value_step.hpp"
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
  const std::array<llvm::Value*, 1> args{LocationOf(type, value)};
  return module_->EmitStep(builder_, type, lir::ValueStep::kEnd, args);
}

auto CodeGenFunction::CopyValue(
    lir::TypeId type, llvm::Value* value, llvm::Value* out)
    -> diag::Result<llvm::Value*> {
  const std::array<llvm::Value*, 2> args{value, out};
  auto copied = module_->EmitStep(builder_, type, lir::ValueStep::kCopy, args);
  if (!copied) {
    return std::unexpected(std::move(copied.error()));
  }
  return out;
}

auto CodeGenFunction::MoveValue(
    lir::TypeId type, llvm::Value* value, llvm::Value* out)
    -> diag::Result<llvm::Value*> {
  const std::array<llvm::Value*, 2> args{value, out};
  auto moved = module_->EmitStep(builder_, type, lir::ValueStep::kMove, args);
  if (!moved) {
    return std::unexpected(std::move(moved.error()));
  }
  return out;
}

auto CodeGenFunction::AssignValue(
    lir::TypeId type, llvm::Value* value, llvm::Value* out)
    -> diag::Result<void> {
  const std::array<llvm::Value*, 2> args{out, value};
  return module_->EmitStep(builder_, type, lir::ValueStep::kAssign, args);
}

auto CodeGenFunction::RelocateValue(
    lir::TypeId type, llvm::Value* value, llvm::Value* out)
    -> diag::Result<void> {
  auto moved = MoveValue(type, value, out);
  if (!moved) {
    return std::unexpected(std::move(moved.error()));
  }
  return EndValue(type, value);
}

// A value lying in storage of its own is named by that storage already; one
// held as itself -- a pointer owning what it points at -- is put in a slot of
// the frame, since a step takes where the value lies.
auto CodeGenFunction::LocationOf(lir::TypeId type, llvm::Value* value)
    -> llvm::Value* {
  if (module_->Types().HasAggregateEvaluationKind(type)) {
    return value;
  }
  llvm::Value* const slot = FrameStorage(value->getType());
  builder_.CreateStore(value, slot);
  return slot;
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

auto CodeGenFunction::LowerVariableStorage() -> diag::Result<llvm::Value*> {
  auto placed = Variables();
  if (!placed) {
    return std::unexpected(std::move(placed.error()));
  }
  llvm::IRBuilder<> at(frame_storage_point_);
  llvm::AllocaInst* storage = at.CreateAlloca(
      llvm::ArrayType::get(at.getInt8Ty(), (*placed)->layout.size), nullptr,
      "variables");
  storage->setAlignment(llvm::Align((*placed)->layout.align));
  return storage;
}

auto CodeGenFunction::LowerBeginVariable(const lir::BeginVariableInstr& begun)
    -> diag::Result<llvm::Value*> {
  auto placed = Variables();
  if (!placed) {
    return std::unexpected(std::move(placed.error()));
  }
  auto storage = LowerOperand(begun.variables);
  if (!storage) {
    return std::unexpected(std::move(storage.error()));
  }
  if (auto built = module_->BeginMember(
          builder_, *storage, **placed, begun.position.value);
      !built) {
    return std::unexpected(std::move(built.error()));
  }
  return builder_.CreateConstInBoundsGEP1_64(
      builder_.getInt8Ty(), *storage,
      (*placed)->offsets.at(begun.position.value));
}

// The storage a place names, as its type says: the part a class of the
// library defines, the parts an object of a class is built of, or a member's
// storage as the declaration it lies in gives a member of its type -- the
// members a closure captures are copies it was built with -- which a variable's
// cell reached by a dereference is too.
auto CodeGenFunction::StorageAtPlace(const lir::Place& place)
    -> diag::Result<NamedStorage> {
  const lir::TypeId type = lir::PlaceType(module_->Unit(), *fn_, place);
  auto address = ResolvePlaceAddress(place, Access::kRead);
  if (!address) {
    return std::unexpected(std::move(address.error()));
  }
  const lir::Type& stated = module_->Unit().types.Get(type);
  if (stated.Is<lir::RuntimeClassType>() || stated.Is<lir::ObjectType>()) {
    return NamedStorage{.type = type, .storage = std::nullopt, .at = *address};
  }
  MemberSlotRole role = MemberSlotRole::kVariable;
  if (!place.chain.empty()) {
    if (const auto* member =
            std::get_if<lir::MemberProjection>(&place.chain.back())) {
      const std::optional<lir::TypeDeclaration> declaration =
          module_->Unit().types.Get(member->member.declared_by).Declaration();
      if (declaration.has_value()) {
        role = MemberSlotRoleOf(*declaration);
      }
    }
  }
  const std::optional<support::DeclaredMemberStorage> storage =
      DeclaredStorageOf(module_->Unit(), type, role);
  if (!storage) {
    return diag::Fail(
        diag::DiagCode::kUnsupportedTypeKind,
        "storage of type '{}' is not yet supported with '--backend llvm'",
        stated.KindName());
  }
  return NamedStorage{.type = type, .storage = *storage, .at = *address};
}

auto CodeGenFunction::LowerBeginStorage(const lir::BeginStorageInstr& begun)
    -> diag::Result<llvm::Value*> {
  auto named = StorageAtPlace(begun.place);
  if (!named) {
    return std::unexpected(std::move(named.error()));
  }
  if (!named->storage.has_value()) {
    module_->BeginParts(builder_, named->type, named->at);
    return nullptr;
  }
  if (auto built = module_->BeginStorageAt(
          builder_, named->type, *named->storage, named->at);
      !built) {
    return std::unexpected(std::move(built.error()));
  }
  return nullptr;
}

auto CodeGenFunction::LowerEndStorage(const lir::EndStorageInstr& ended)
    -> diag::Result<llvm::Value*> {
  auto named = StorageAtPlace(ended.place);
  if (!named) {
    return std::unexpected(std::move(named.error()));
  }
  if (!named->storage.has_value()) {
    module_->EndParts(builder_, named->type, named->at);
    return nullptr;
  }
  if (auto ended = module_->EndStorageAt(
          builder_, named->type, *named->storage, named->at);
      !ended) {
    return std::unexpected(std::move(ended.error()));
  }
  return nullptr;
}

// The class is kept as the object's definition, at its start, where the part
// every object begins with keeps it.
auto CodeGenFunction::LowerSetObjectClass(const lir::SetObjectClassInstr& set)
    -> diag::Result<llvm::Value*> {
  auto object = LowerOperand(set.object);
  if (!object) {
    return std::unexpected(std::move(object.error()));
  }
  auto definition = module_->DefinitionOf(set.cls);
  if (!definition) {
    return std::unexpected(std::move(definition.error()));
  }
  builder_.CreateStore(*definition, *object);
  return nullptr;
}

auto CodeGenFunction::LowerEndVariable(const lir::EndVariableInstr& ended)
    -> diag::Result<llvm::Value*> {
  auto placed = Variables();
  if (!placed) {
    return std::unexpected(std::move(placed.error()));
  }
  auto storage = LowerOperand(ended.variables);
  if (!storage) {
    return std::unexpected(std::move(storage.error()));
  }
  if (auto finished = module_->EndMember(
          builder_, *storage, **placed, ended.position.value);
      !finished) {
    return std::unexpected(std::move(finished.error()));
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
            // The departure goes to the activation the body completes, which
            // it leaves through its final suspension, as a return does. That
            // suspension is where releasing the frame finds it stopped.
            const std::array<llvm::Value*, 1> args{*departure};
            CallEntry(
                RuntimeSymbol(RuntimeOp::kSettleDeparture),
                builder_.getVoidTy(), Addresses(1), args);
            builder_.CreateBr(coro_final_);
            return {};
          },
          [&](const lir::UnwindResumeTerm& resumed) -> diag::Result<void> {
            auto exception = LowerOperand(resumed.exception);
            if (!exception) {
              return std::unexpected(std::move(exception.error()));
            }
            // A coroutine unwound out of is done, and says so before the
            // unwinding leaves it, as LLVM's coroutines ask of a landing pad.
            // Its body starts at its initial suspension, so nothing unwinds
            // while it is first entered and no frame is left to free here.
            if (IsCoroutine()) {
              builder_.CreateCall(
                  llvm::Intrinsic::getOrInsertDeclaration(
                      &module_->Module(), llvm::Intrinsic::coro_end),
                  {llvm::ConstantPointerNull::get(module_->Types().Ptr()),
                   builder_.getInt1(true),
                   llvm::ConstantTokenNone::get(module_->Context())});
            }
            builder_.CreateResume(builder_.CreateInsertValue(
                llvm::PoisonValue::get(LandingPadType()), *exception, 0));
            return {};
          },
          [&](const lir::UnreachableTerm&) -> diag::Result<void> {
            builder_.CreateUnreachable();
            return {};
          },
          [&](const lir::InvokeTerm& call) -> diag::Result<void> {
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
            // A call this body realizes as its own instructions cannot be
            // unwound out of, so control goes on to where the call returns.
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
