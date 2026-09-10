#include <array>
#include <cstddef>
#include <cstdint>
#include <format>
#include <iterator>
#include <optional>
#include <span>
#include <string>
#include <string_view>
#include <utility>
#include <variant>
#include <vector>

#include <llvm/IR/Constants.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/Type.h>

#include "lyra/backend/llvm/codegen_function.hpp"
#include "lyra/backend/llvm/codegen_module.hpp"
#include "lyra/backend/llvm/runtime_entry.hpp"
#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/diag/diag_code.hpp"
#include "lyra/lir/compilation_unit.hpp"
#include "lyra/lir/integral_constant.hpp"
#include "lyra/lir/place_query.hpp"
#include "lyra/lir/type.hpp"
#include "lyra/support/builtin_fn.hpp"

namespace lyra::backend::llvm_backend {

namespace {

auto Unsupported(std::string message) -> std::unexpected<diag::Diagnostic> {
  return diag::Fail(
      diag::DiagCode::kUnsupportedExpressionForm, std::move(message));
}

// Which capability wrapper a type is, and the value that wrapper represents;
// nothing for a type that represents no storage. Classifying a wrapper in one
// place is what keeps an access reached through a place and an operation
// reached through a reference from disagreeing about what a wrapper is.
auto WrapperOf(const lir::Type& type)
    -> std::optional<std::pair<WrapperKind, lir::TypeId>> {
  if (const auto* observable = type.As<lir::ObservableType>()) {
    return std::pair{WrapperKind::kCell, observable->value};
  }
  if (const auto* net = type.As<lir::ResolvedType>()) {
    return std::pair{WrapperKind::kNet, net->value};
  }
  if (const auto* driver = type.As<lir::DriverType>()) {
    return std::pair{WrapperKind::kDriver, driver->value};
  }
  return std::nullopt;
}

// The values a storage holds, where they are all of one representation;
// nothing for a type that is not such storage. A capability wrapper holds the
// value it represents, and a history holds what each tick of one clocking
// event settled for one expression -- one representation either way, which is
// what lets an entry reaching the storage be named once per representation
// rather than per call.
auto ValuesHeldBy(const lir::Type& type) -> std::optional<lir::TypeId> {
  if (const std::optional<std::pair<WrapperKind, lir::TypeId>> wrapper =
          WrapperOf(type)) {
    return wrapper->second;
  }
  if (const auto* history = type.As<lir::SampledHistoryType>()) {
    return history->value;
  }
  return std::nullopt;
}

// A leaf of a wait carries the observation that decides what a change there
// means only where an event control is what waits (LRM 9.4.2); an implicit
// sensitivity names none and supplies the cell and its bit range alone.
auto TriggerConstruction(std::size_t argument_count) -> RuntimeOp {
  return argument_count == 3 ? RuntimeOp::kMakeTrigger
                             : RuntimeOp::kMakeObservedTrigger;
}

// What an observation is built over: the watched expression and its edge, both
// of those plus an `iff` qualifier, or the qualifier alone -- which is a named
// event's, whose trigger is the event itself so there is no value to watch
// (LRM 9.4.2, 9.4.2.3, 15.5).
auto ObservationConstruction(std::size_t argument_count) -> RuntimeOp {
  switch (argument_count) {
    case 1:
      return RuntimeOp::kMakeConditionObservation;
    case 2:
      return RuntimeOp::kMakeObservation;
    default:
      return RuntimeOp::kMakeQualifiedObservation;
  }
}

}  // namespace

auto CodeGenFunction::LowerInstr(const lir::Instr& instr)
    -> diag::Result<llvm::Value*> {
  const lir::TypeId result_type = fn_->values.Get(instr.result).type;
  return std::visit(
      Overloaded{
          [&](const lir::CallInstr& call) -> diag::Result<llvm::Value*> {
            return LowerCall(call, result_type);
          },
          [&](const lir::ProductInstr& product) -> diag::Result<llvm::Value*> {
            return LowerProduct(product, result_type);
          },
          [&](const lir::ArrayInstr& array) -> diag::Result<llvm::Value*> {
            return LowerArray(array, result_type);
          },
          [&](const lir::UnionInstr& u) -> diag::Result<llvm::Value*> {
            return LowerUnion(u, result_type);
          },
          [&](const lir::AggregateExtractInstr& extract)
              -> diag::Result<llvm::Value*> {
            return LowerAggregateExtract(extract);
          },
          [&](const lir::AggregateUpdateInstr& update)
              -> diag::Result<llvm::Value*> {
            return LowerAggregateUpdate(update);
          },
          [&](const lir::TagTestInstr& test) -> diag::Result<llvm::Value*> {
            return LowerTagTest(test, result_type);
          },
          [&](const lir::LoadInstr& load) -> diag::Result<llvm::Value*> {
            return LowerLoad(load, result_type);
          },
          [&](const lir::StoreInstr& store) -> diag::Result<llvm::Value*> {
            return LowerStore(store);
          },
          [&](const lir::AddrOfInstr& addr) -> diag::Result<llvm::Value*> {
            return ResolvePlaceAddress(addr.place);
          },
          [&](const lir::BinaryInstr& binary) -> diag::Result<llvm::Value*> {
            return LowerBinary(binary, result_type);
          },
          [&](const lir::UnaryInstr& unary) -> diag::Result<llvm::Value*> {
            return LowerUnary(unary, result_type);
          },
          [&](const lir::BoolCastInstr& cast) -> diag::Result<llvm::Value*> {
            return LowerBoolCast(cast, result_type);
          },
          [&](const lir::PointerCastInstr& cast) -> diag::Result<llvm::Value*> {
            // Every reference crosses as the same opaque handle, so retyping it
            // moves no bits.
            return LowerOperand(cast.operand);
          },
          [&](const lir::ValueCastInstr& cast) -> diag::Result<llvm::Value*> {
            // The value's handle is what it was; only the type the program
            // holds it to differs, and a handle carries no type.
            return LowerOperand(cast.operand);
          },
          [&](const lir::IntCastInstr& cast) -> diag::Result<llvm::Value*> {
            return LowerIntCast(cast, result_type);
          }},
      instr.data);
}

// Reading a place reads whatever storage it names, except where the storage
// decides what reading it means: a cell's contents have no address of their
// own, and a capture's storage is the closure's rather than an instance's, so
// each comes out through its own access rather than from an address.
auto CodeGenFunction::LowerLoad(
    const lir::LoadInstr& load, lir::TypeId result_type)
    -> diag::Result<llvm::Value*> {
  if (const std::optional<CapturePlace> capture = CapturePlaceOf(load.place)) {
    auto closure = ResolvePlaceAddress(capture->closure);
    if (!closure) {
      return std::unexpected(std::move(closure.error()));
    }
    const std::array<llvm::Value*, 2> args{
        *closure,
        llvm::ConstantInt::get(
            llvm::Type::getInt32Ty(module_->Context()), capture->index)};
    return builder_.CreateCall(
        Entry(RuntimeSymbol(RuntimeOp::kClosureCapture), result_type, args),
        args);
  }
  auto wrapper = WrapperPlaceOf(load.place);
  if (!wrapper) {
    return std::unexpected(std::move(wrapper.error()));
  }
  if (!wrapper->has_value()) {
    auto address = ResolvePlaceAddress(load.place);
    if (!address) {
      return std::unexpected(std::move(address.error()));
    }
    if (const std::optional<support::ValueDomain> cell =
            MemberValueCellDomain(load.place, result_type)) {
      const std::array<llvm::Value*, 1> args{*address};
      return builder_.CreateCall(
          Entry(
              RuntimeSymbol(*cell, lir::ValueCellTarget::Op::kLoad),
              result_type, args),
          args);
    }
    return builder_.CreateLoad(module_->Types().Map(result_type), *address);
  }
  const WrapperPlace& through = **wrapper;
  auto address = ResolvePlaceAddress(through.wrapper);
  if (!address) {
    return std::unexpected(std::move(address.error()));
  }
  const std::array<llvm::Value*, 1> args{*address};
  return builder_.CreateCall(
      Entry(
          RuntimeSymbol(
              through.domain, through.kind, support::BuiltinFn::kLoad),
          result_type, args),
      args);
}

// Writing a place mirrors reading one, and a write through a wrapper is what
// gives the write its meaning -- waking whoever subscribed to a cell, or moving
// one driver's contribution and re-resolving the net it feeds -- which is why
// it is the wrapper that performs the write rather than a store to an address.
auto CodeGenFunction::LowerStore(const lir::StoreInstr& store)
    -> diag::Result<llvm::Value*> {
  auto wrapper = WrapperPlaceOf(store.place);
  if (!wrapper) {
    return std::unexpected(std::move(wrapper.error()));
  }
  if (!wrapper->has_value()) {
    auto value = LowerOperand(store.value);
    if (!value) {
      return std::unexpected(std::move(value.error()));
    }
    auto address = ResolvePlaceAddress(store.place);
    if (!address) {
      return std::unexpected(std::move(address.error()));
    }
    if (const std::optional<support::ValueDomain> cell =
            MemberValueCellDomain(store.place, OperandType(store.value))) {
      const std::array<llvm::Value*, 2> args{*address, *value};
      return builder_.CreateCall(
          Entry(
              RuntimeSymbol(*cell, lir::ValueCellTarget::Op::kStore),
              module_->Types().Void(), args),
          args);
    }
    return builder_.CreateStore(*value, *address);
  }
  const WrapperPlace& through = **wrapper;
  auto address = ResolvePlaceAddress(through.wrapper);
  if (!address) {
    return std::unexpected(std::move(address.error()));
  }
  auto value = LowerOperand(store.value);
  if (!value) {
    return std::unexpected(std::move(value.error()));
  }
  const std::array<llvm::Value*, 2> args{*address, *value};
  return builder_.CreateCall(
      Entry(
          RuntimeSymbol(
              through.domain, through.kind, support::BuiltinFn::kStore),
          module_->Types().Void(), args),
      args);
}

// A place resolves to an address. A place local's storage is its frame slot;
// any other base is a reference value, whose referent the opening dereference
// names. Each further dereference reads the reference held in the storage
// reached so far, and each member step asks the instance for that member's
// storage.
auto CodeGenFunction::ResolvePlaceAddress(const lir::Place& place)
    -> diag::Result<llvm::Value*> {
  auto step = place.chain.begin();
  llvm::Value* address = nullptr;

  const auto* use = std::get_if<lir::Use>(&place.base);
  if (lir::IsPlaceLocal(*fn_, place.base)) {
    address = values_.at(use->value);
  } else {
    // A value base refers to storage, so opening it is what the base value
    // already answers; the dereference is the step that says the storage is
    // what the place names. A prefix that stops before that step -- the wrapper
    // half of an access whose wrapper is itself a value, as a net driver's
    // handle is -- has no dereference left to consume and names that same
    // storage.
    if (step != place.chain.end() &&
        !std::holds_alternative<lir::DerefProjection>(*step)) {
      throw InternalError(
          "llvm codegen: a place over a value base must open with a "
          "dereference");
    }
    auto base = LowerOperand(place.base);
    if (!base) {
      return std::unexpected(std::move(base.error()));
    }
    address = OpenedReferent(*base, OperandType(place.base));
    if (step != place.chain.end()) {
      ++step;
    }
  }

  for (; step != place.chain.end(); ++step) {
    // What the chain holds where this step applies, which decides how the step
    // reaches what it names: crossing a class handle is an operation rather
    // than a load, and a member's address comes from whatever declares it.
    const lir::TypeId reached =
        ReachedType(place, std::distance(place.chain.begin(), step));
    auto reached_storage = std::visit(
        Overloaded{
            [&](const lir::DerefProjection&) -> diag::Result<llvm::Value*> {
              return OpenedReferent(
                  builder_.CreateLoad(module_->Types().Ptr(), address),
                  reached);
            },
            [&](const lir::MemberProjection& projection)
                -> diag::Result<llvm::Value*> {
              return MemberStorage(address, reached, projection.member);
            }},
        *step);
    if (!reached_storage) {
      return std::unexpected(std::move(reached_storage.error()));
    }
    address = *reached_storage;
  }
  return address;
}

auto CodeGenFunction::MemberStorage(
    llvm::Value* owner, lir::TypeId reached, lir::MemberRef member)
    -> diag::Result<llvm::Value*> {
  llvm::Value* const slot = llvm::ConstantInt::get(
      llvm::Type::getInt32Ty(module_->Context()), member.slot.value);
  switch (MemberOwnerOf(reached)) {
    case MemberOwner::kScope: {
      const std::array<llvm::Value*, 2> args{owner, slot};
      return builder_.CreateCall(
          Entry(
              RuntimeSymbol(RuntimeOp::kMemberAddress), module_->Types().Ptr(),
              args),
          args);
    }
    case MemberOwner::kObject: {
      auto declared_by = module_->DefinitionRef(member.declared_by);
      if (!declared_by) {
        return std::unexpected(std::move(declared_by.error()));
      }
      const std::array<llvm::Value*, 3> args{owner, *declared_by, slot};
      return builder_.CreateCall(
          Entry(
              RuntimeSymbol(RuntimeOp::kObjectMemberAddress),
              module_->Types().Ptr(), args),
          args);
    }
  }
  throw InternalError("llvm codegen: a member step reached unknown storage");
}

// The type of the storage the chain has arrived at where step `index` applies,
// which is the type of the place that stops just before it.
auto CodeGenFunction::ReachedType(
    const lir::Place& place, std::ptrdiff_t index) const -> lir::TypeId {
  const lir::Place prefix{
      .base = place.base,
      .chain = {place.chain.begin(), place.chain.begin() + index}};
  return prefix.chain.empty() ? OperandType(prefix.base)
                              : lir::PlaceType(module_->Unit(), *fn_, prefix);
}

// The address of what `reference` refers to, given the type it is. A pointer is
// the address already; a class handle is not, since which object it refers to
// is a fact the handle holds rather than is, so the runtime answers it -- and
// that is where a handle referring to no object is caught (LRM 8.3).
auto CodeGenFunction::OpenedReferent(llvm::Value* reference, lir::TypeId type)
    -> llvm::Value* {
  if (!module_->Unit().types.Get(type).Is<lir::ManagedRefType>()) {
    return reference;
  }
  const std::array<llvm::Value*, 1> args{reference};
  return builder_.CreateCall(
      Entry(
          RuntimeSymbol(RuntimeOp::kObjectDeref), module_->Types().Ptr(), args),
      args);
}

auto CodeGenFunction::IsHandleSequence(lir::TypeId type) const -> bool {
  return module_->Unit().types.Get(type).Is<lir::VectorType>();
}

// Which entry answers the address of a member of what `owner` names. Every
// owner holds a block of storage described the same way, so what an entry
// differs in is the runtime type the address it is handed names.
auto CodeGenFunction::MemberOwnerOf(lir::TypeId owner) const -> MemberOwner {
  const lir::Type& type = module_->Unit().types.Get(owner);
  if (const auto* object = type.As<lir::ObjectType>()) {
    return lir::IsObjectTreeNode(module_->Unit().classes.Get(object->class_id))
               ? MemberOwner::kScope
               : MemberOwner::kObject;
  }
  // What another unit published is an object of its own tree.
  if (type.Is<lir::ExternalUnitObjectType>()) {
    return MemberOwner::kScope;
  }
  // A class another unit declares is the source language's own class, reached
  // the way this unit's are.
  if (type.Is<lir::CrossUnitClassType>()) {
    return MemberOwner::kObject;
  }
  // A struct's fields are the same storage block a heap object's properties
  // are, reached through the same handle.
  if (type.Is<lir::StructType>()) {
    return MemberOwner::kObject;
  }
  throw InternalError(
      std::format(
          "llvm codegen: a member of {} has no address entry",
          type.KindName()));
}

auto CodeGenFunction::LowerBinary(
    const lir::BinaryInstr& binary, lir::TypeId result_type)
    -> diag::Result<llvm::Value*> {
  const lir::TypeId operand_type = OperandType(binary.lhs);
  // A machine integer is a native value, not a value-domain handle: its
  // operator is a machine instruction, not a runtime-library call. Two things
  // arrive this way -- the reduced predicates a real- or string-family `&&` /
  // `||` / `<->` composes, combined before `from_bool` widens the result back
  // to a 1-bit packed, and the words a synthesized transition computes over,
  // which stand for no value of the design at all.
  if (const std::optional<lir::Signedness> signedness =
          module_->Unit().types.Get(operand_type).MachineIntegerSignedness()) {
    return LowerMachineBinary(binary, *signedness);
  }
  auto domain = DomainOf(operand_type);
  if (!domain) {
    return std::unexpected(std::move(domain.error()));
  }
  auto lhs = LowerOperand(binary.lhs);
  if (!lhs) {
    return std::unexpected(std::move(lhs.error()));
  }
  auto rhs = LowerOperand(binary.rhs);
  if (!rhs) {
    return std::unexpected(std::move(rhs.error()));
  }
  const std::array<llvm::Value*, 2> args{*lhs, *rhs};
  return builder_.CreateCall(
      Entry(RuntimeSymbol(*domain, binary.op), result_type, args), args);
}

auto CodeGenFunction::LowerMachineBinary(
    const lir::BinaryInstr& binary, lir::Signedness signedness)
    -> diag::Result<llvm::Value*> {
  auto lhs = LowerOperand(binary.lhs);
  if (!lhs) {
    return std::unexpected(std::move(lhs.error()));
  }
  auto rhs = LowerOperand(binary.rhs);
  if (!rhs) {
    return std::unexpected(std::move(rhs.error()));
  }
  // Every operator of the set has a machine integer's answer, and the operand's
  // own signedness is what division, remainder and the ordering comparisons
  // need. The logical pair is the bitwise one here, because a machine value
  // carrying a predicate is one bit wide and a value of any other width is
  // reduced to a predicate before it reaches a logical operator.
  const bool is_signed = signedness == lir::Signedness::kSigned;
  switch (binary.op) {
    case lir::BinaryOp::kAdd:
      return builder_.CreateAdd(*lhs, *rhs);
    case lir::BinaryOp::kSub:
      return builder_.CreateSub(*lhs, *rhs);
    case lir::BinaryOp::kMul:
      return builder_.CreateMul(*lhs, *rhs);
    case lir::BinaryOp::kDiv:
      return is_signed ? builder_.CreateSDiv(*lhs, *rhs)
                       : builder_.CreateUDiv(*lhs, *rhs);
    case lir::BinaryOp::kMod:
      return is_signed ? builder_.CreateSRem(*lhs, *rhs)
                       : builder_.CreateURem(*lhs, *rhs);
    case lir::BinaryOp::kBitwiseAnd:
    case lir::BinaryOp::kLogicalAnd:
      return builder_.CreateAnd(*lhs, *rhs);
    case lir::BinaryOp::kBitwiseOr:
    case lir::BinaryOp::kLogicalOr:
      return builder_.CreateOr(*lhs, *rhs);
    case lir::BinaryOp::kBitwiseXor:
      return builder_.CreateXor(*lhs, *rhs);
    case lir::BinaryOp::kEquality:
      return builder_.CreateICmpEQ(*lhs, *rhs);
    case lir::BinaryOp::kInequality:
      return builder_.CreateICmpNE(*lhs, *rhs);
    case lir::BinaryOp::kLessThan:
      return is_signed ? builder_.CreateICmpSLT(*lhs, *rhs)
                       : builder_.CreateICmpULT(*lhs, *rhs);
    case lir::BinaryOp::kLessEqual:
      return is_signed ? builder_.CreateICmpSLE(*lhs, *rhs)
                       : builder_.CreateICmpULE(*lhs, *rhs);
    case lir::BinaryOp::kGreaterThan:
      return is_signed ? builder_.CreateICmpSGT(*lhs, *rhs)
                       : builder_.CreateICmpUGT(*lhs, *rhs);
    case lir::BinaryOp::kGreaterEqual:
      return is_signed ? builder_.CreateICmpSGE(*lhs, *rhs)
                       : builder_.CreateICmpUGE(*lhs, *rhs);
  }
  throw InternalError("llvm codegen: unknown binary operator");
}

auto CodeGenFunction::LowerUnary(
    const lir::UnaryInstr& unary, lir::TypeId result_type)
    -> diag::Result<llvm::Value*> {
  const lir::TypeId operand_type = OperandType(unary.operand);
  // A machine integer is a native value, not a value-domain handle: its
  // operator is a machine instruction, not a runtime-library call. This is how
  // the reduced predicate a real- or chandle-family `!` produces is negated
  // before `from_bool` widens it back to a 1-bit packed.
  if (module_->Unit()
          .types.Get(operand_type)
          .MachineIntegerSignedness()
          .has_value()) {
    return LowerMachineUnary(unary);
  }
  auto domain = DomainOf(operand_type);
  if (!domain) {
    return std::unexpected(std::move(domain.error()));
  }
  auto operand = LowerOperand(unary.operand);
  if (!operand) {
    return std::unexpected(std::move(operand.error()));
  }
  const std::array<llvm::Value*, 1> args{*operand};
  return builder_.CreateCall(
      Entry(RuntimeSymbol(*domain, unary.op), result_type, args), args);
}

auto CodeGenFunction::LowerMachineUnary(const lir::UnaryInstr& unary)
    -> diag::Result<llvm::Value*> {
  auto operand = LowerOperand(unary.operand);
  if (!operand) {
    return std::unexpected(std::move(operand.error()));
  }
  // Signedness decides none of these: negation, complement and the successor
  // and predecessor are one instruction whichever way the sign bit is read.
  llvm::Value* const zero = llvm::ConstantInt::get((*operand)->getType(), 0);
  llvm::Value* const one = llvm::ConstantInt::get((*operand)->getType(), 1);
  switch (unary.op) {
    case lir::UnaryOp::kLogicalNot:
      return builder_.CreateICmpEQ(*operand, zero);
    case lir::UnaryOp::kMinus:
      return builder_.CreateSub(zero, *operand);
    case lir::UnaryOp::kBitwiseNot:
      return builder_.CreateNot(*operand);
    case lir::UnaryOp::kIncrement:
      return builder_.CreateAdd(*operand, one);
    case lir::UnaryOp::kDecrement:
      return builder_.CreateSub(*operand, one);
  }
  throw InternalError("llvm codegen: unknown unary operator");
}

auto CodeGenFunction::LowerBoolCast(
    const lir::BoolCastInstr& cast, lir::TypeId result_type)
    -> diag::Result<llvm::Value*> {
  auto domain = DomainOf(OperandType(cast.operand));
  if (!domain) {
    return std::unexpected(std::move(domain.error()));
  }
  auto operand = LowerOperand(cast.operand);
  if (!operand) {
    return std::unexpected(std::move(operand.error()));
  }
  const std::array<llvm::Value*, 1> args{*operand};
  return builder_.CreateCall(
      Entry(RuntimeSymbol(*domain, RuntimeOp::kToBool), result_type, args),
      args);
}

// Widening repeats the sign bit only when the *source* is signed; the
// destination's signedness says how the result is later read, not what the
// added high bits hold. Narrowing discards high bits either way.
auto CodeGenFunction::LowerIntCast(
    const lir::IntCastInstr& cast, lir::TypeId result_type)
    -> diag::Result<llvm::Value*> {
  const auto& source = module_->Unit()
                           .types.Get(OperandType(cast.operand))
                           .Get<lir::MachineIntType>();
  auto operand = LowerOperand(cast.operand);
  if (!operand) {
    return std::unexpected(std::move(operand.error()));
  }
  return builder_.CreateIntCast(
      *operand, module_->Types().Map(result_type),
      source.signedness == lir::Signedness::kSigned);
}

auto CodeGenFunction::LowerCall(
    const lir::CallInstr& call, lir::TypeId result_type)
    -> diag::Result<llvm::Value*> {
  std::vector<llvm::Value*> operands;
  operands.reserve(call.args.size());
  for (const lir::Operand& arg : call.args) {
    auto lowered = LowerOperand(arg);
    if (!lowered) {
      return std::unexpected(std::move(lowered.error()));
    }
    operands.push_back(*lowered);
  }
  // The entry is resolved against what it is actually handed, so this target's
  // own encoding of the call is already in the argument list by the time the
  // entry's signature is read off it.
  auto args = CallArgs(call, std::move(operands));
  if (!args) {
    return std::unexpected(std::move(args.error()));
  }
  auto callee = ResolveCallee(call, result_type, *args);
  if (!callee) {
    return std::unexpected(std::move(callee.error()));
  }
  return builder_.CreateCall(*callee, *args);
}

// An entry the runtime publishes, typed by what the call hands it: the values
// crossing are its parameters by construction, so an entry and its call cannot
// disagree about what is passed.
auto CodeGenFunction::Entry(
    std::string_view symbol, llvm::Type* result,
    std::span<llvm::Value* const> args) -> llvm::FunctionCallee {
  std::vector<llvm::Type*> params;
  params.reserve(args.size());
  for (llvm::Value* arg : args) {
    params.push_back(arg->getType());
  }
  return module_->Module().getOrInsertFunction(
      symbol, llvm::FunctionType::get(result, params, false));
}

auto CodeGenFunction::Entry(
    std::string_view symbol, lir::TypeId result,
    std::span<llvm::Value* const> args) -> llvm::FunctionCallee {
  return Entry(symbol, module_->Types().Map(result), args);
}

auto CodeGenFunction::CallArgs(
    const lir::CallInstr& call, std::vector<llvm::Value*> operands)
    -> diag::Result<std::vector<llvm::Value*>> {
  auto encoding = EncodingOf(call);
  if (!encoding) {
    return std::unexpected(std::move(encoding.error()));
  }
  if (const std::optional<ErasedArgument>& argument = encoding->erased) {
    const std::array<llvm::Value*, 1> boxed{operands[argument->position]};
    operands[argument->position] = builder_.CreateCall(
        Entry(
            RuntimeSymbol(argument->domain, RuntimeOp::kValueBox),
            module_->Types().Ptr(), boxed),
        boxed);
  }
  // A position the callee names is not a value the program computed, so this
  // target writes it where its calls take values: right after the object whose
  // part it names, and first where the entry acts on no object -- which is what
  // its declaration says by being a factory on the type it builds.
  if (const auto* builtin = std::get_if<lir::BuiltinTarget>(&call.target);
      builtin != nullptr && builtin->position.has_value()) {
    const bool acts_on_an_object =
        !std::holds_alternative<support::StaticFactory>(
            support::RuntimeEntryOf(builtin->fn).declaration);
    operands.insert(
        operands.begin() + (acts_on_an_object ? 1 : 0),
        llvm::ConstantInt::get(
            llvm::Type::getInt64Ty(module_->Context()),
            builtin->position->value));
  }
  return ArgsInForm(encoding->operand_form, operands);
}

auto CodeGenFunction::ArgsInForm(
    const OperandForm& form, const std::vector<llvm::Value*>& operands)
    -> diag::Result<std::vector<llvm::Value*>> {
  return std::visit(
      Overloaded{
          [&](const OperandsAsStated&)
              -> diag::Result<std::vector<llvm::Value*>> { return operands; },
          [&](const OperandsAfterDefinition& f)
              -> diag::Result<std::vector<llvm::Value*>> {
            auto definition = module_->DefinitionRef(f.defined);
            if (!definition) {
              return std::unexpected(std::move(definition.error()));
            }
            std::vector<llvm::Value*> args{*definition};
            args.insert(args.end(), operands.begin(), operands.end());
            return args;
          },
          [&](const OperandsAsSpanAfterDefinition& f)
              -> diag::Result<std::vector<llvm::Value*>> {
            auto definition = module_->DefinitionRef(f.defined);
            if (!definition) {
              return std::unexpected(std::move(definition.error()));
            }
            return std::vector<llvm::Value*>{
                *definition, SpanOver(operands, module_->Types().Ptr())};
          }},
      form);
}

// Every call is a symbol invoked with arguments; the target kinds differ only
// in how the symbol is resolved.
auto CodeGenFunction::ResolveCallee(
    const lir::CallInstr& call, lir::TypeId result_type,
    std::span<llvm::Value* const> args) -> diag::Result<llvm::FunctionCallee> {
  return std::visit(
      Overloaded{
          [&](const lir::BuiltinTarget& t)
              -> diag::Result<llvm::FunctionCallee> {
            return BuiltinCallee(t, call, result_type, args);
          },
          [&](const lir::FunctionTarget& t)
              -> diag::Result<llvm::FunctionCallee> {
            return module_->UnitFunction(t.function);
          },
          // A dispatched call is two operations over one boundary. What class a
          // value is, is the only half this side cannot know, so that is the
          // only half that crosses: the runtime answers with the address of the
          // body that class holds for this behavior, and entering it with the
          // arguments already in hand is this side's own.
          [&](const lir::DispatchTarget& t)
              -> diag::Result<llvm::FunctionCallee> {
            if (args.empty()) {
              throw InternalError(
                  "llvm codegen: a dispatched call states no value to dispatch "
                  "on");
            }
            auto introduced_by = module_->DefinitionRef(t.method.introduced_by);
            if (!introduced_by) {
              return std::unexpected(std::move(introduced_by.error()));
            }
            const std::array<llvm::Value*, 3> lookup{
                args[0], *introduced_by,
                llvm::ConstantInt::get(
                    llvm::Type::getInt32Ty(module_->Context()),
                    t.method.ordinal.value)};
            llvm::Value* body = builder_.CreateCall(
                Entry(
                    RuntimeSymbol(RuntimeOp::kObjectMethod),
                    module_->Types().Ptr(), lookup),
                lookup);
            std::vector<llvm::Type*> params;
            params.reserve(args.size());
            for (llvm::Value* arg : args) {
              params.push_back(arg->getType());
            }
            return llvm::FunctionCallee(
                llvm::FunctionType::get(
                    module_->Types().Map(result_type), params, false),
                body);
          },
          [&](const lir::ConstructTarget& t)
              -> diag::Result<llvm::FunctionCallee> {
            auto construction = ConstructionOf(call, t.result);
            if (!construction) {
              return std::unexpected(std::move(construction.error()));
            }
            return Entry(construction->symbol, t.result, args);
          },
          // A foreign symbol is declared, never defined: the host resolves it.
          // The boundary already marshaled its operands and result to the
          // carriers the foreign side declared (LRM 35.5.6), so what crosses is
          // what the entry takes.
          [&](const lir::ForeignTarget& t)
              -> diag::Result<llvm::FunctionCallee> {
            return Entry(t.symbol, result_type, args);
          },
          // A method of a class the runtime library defines and every unit
          // imports (LRM 9.7). The library realizes it once, whatever it is
          // called on, so the method alone names the entry.
          [&](const lir::ImportedRuntimeTarget& t)
              -> diag::Result<llvm::FunctionCallee> {
            return Entry(RuntimeSymbol(t.method), result_type, args);
          },
          [&](const lir::ValueCellTarget& t)
              -> diag::Result<llvm::FunctionCallee> {
            auto domain = DomainOf(t.value);
            if (!domain) {
              return std::unexpected(std::move(domain.error()));
            }
            return Entry(RuntimeSymbol(*domain, t.op), result_type, args);
          },
          [&](const lir::ControlEffectTarget& t)
              -> diag::Result<llvm::FunctionCallee> {
            return Entry(RuntimeSymbol(t.op), result_type, args);
          },
          [&](const lir::CoroutineTarget& t)
              -> diag::Result<llvm::FunctionCallee> {
            return Entry(RuntimeSymbol(t.op), result_type, args);
          }},
      call.target);
}

// A {pointer, length} span over a scratch buffer this function fills with
// `values`. The element type is the caller's to state: the machine element a
// LIR type names where the span carries plain data, and the ABI's opaque
// handle where it carries a run of runtime-owned values. Nothing here reads
// what the values mean, so nothing depends on which entry the span feeds.
auto CodeGenFunction::SpanOver(
    std::span<llvm::Value* const> values, llvm::Type* element) -> llvm::Value* {
  auto* storage_ty = llvm::ArrayType::get(element, values.size());
  llvm::Value* storage = builder_.CreateAlloca(storage_ty);
  for (std::uint32_t i = 0; i < values.size(); ++i) {
    llvm::Value* slot =
        builder_.CreateConstInBoundsGEP2_64(storage_ty, storage, 0, i);
    builder_.CreateStore(values[i], slot);
  }
  llvm::Value* span = llvm::UndefValue::get(module_->Types().Span());
  span = builder_.CreateInsertValue(span, storage, {0});
  return builder_.CreateInsertValue(
      span,
      llvm::ConstantInt::get(
          llvm::Type::getInt64Ty(module_->Context()),
          static_cast<std::uint64_t>(values.size())),
      {1});
}

auto CodeGenFunction::LowerArray(
    const lir::ArrayInstr& array, lir::TypeId result_type)
    -> diag::Result<llvm::Value*> {
  const lir::TypeId element_type = module_->Unit()
                                       .types.Get(result_type)
                                       .Get<lir::MachineArrayType>()
                                       .element;
  std::vector<llvm::Value*> elements;
  elements.reserve(array.elements.size());
  for (const lir::Operand& element : array.elements) {
    auto lowered = LowerOperand(element);
    if (!lowered) {
      return std::unexpected(std::move(lowered.error()));
    }
    elements.push_back(*lowered);
  }
  return SpanOver(elements, module_->Types().Map(element_type));
}

// A product value is assembled by boxing each component into the erased
// representation its own domain names, then collecting the boxed components.
// The domains come from the result product type, so the generated side never
// inspects a component's runtime representation.
//
// The components each have a domain of their own, so no entry can be named by
// one of them and the caller is the only side that knows them all. A
// homogeneous value's entry is named by its single domain instead, so what
// crosses to one erased is decided at that entry rather than here.
auto CodeGenFunction::LowerProduct(
    const lir::ProductInstr& product, lir::TypeId result_type)
    -> diag::Result<llvm::Value*> {
  const auto* tuple =
      module_->Unit().types.Get(result_type).As<lir::TupleType>();
  if (tuple == nullptr || tuple->elements.size() != product.components.size()) {
    throw InternalError(
        "llvm codegen: a product's result type does not describe the "
        "components it is built from");
  }
  std::vector<llvm::Value*> boxed;
  boxed.reserve(product.components.size());
  for (std::uint32_t i = 0; i < product.components.size(); ++i) {
    auto domain = DomainOf(tuple->elements[i]);
    if (!domain) {
      return std::unexpected(std::move(domain.error()));
    }
    auto component = LowerOperand(product.components[i]);
    if (!component) {
      return std::unexpected(std::move(component.error()));
    }
    const std::array<llvm::Value*, 1> box{*component};
    boxed.push_back(builder_.CreateCall(
        Entry(
            RuntimeSymbol(*domain, RuntimeOp::kValueBox),
            module_->Types().Ptr(), box),
        box));
  }
  const std::array<llvm::Value*, 1> args{
      SpanOver(boxed, module_->Types().Ptr())};
  return builder_.CreateCall(
      Entry(
          RuntimeSymbol(support::ValueDomain::kTuple, RuntimeOp::kMake),
          result_type, args),
      args);
}

auto CodeGenFunction::UnionMemberDomain(
    lir::TypeId union_type, std::uint32_t index) const
    -> diag::Result<support::ValueDomain> {
  const lir::Type& ty = module_->Unit().types.Get(union_type);
  const std::vector<lir::TypeId>* members = nullptr;
  if (const auto* untagged = ty.As<lir::UnionType>()) {
    members = &untagged->elements;
  } else if (const auto* tagged = ty.As<lir::TaggedUnionType>()) {
    members = &tagged->elements;
  } else {
    throw InternalError(
        "llvm codegen: a union member selects into a non-union type");
  }
  if (index >= members->size()) {
    throw InternalError("llvm codegen: a union member index is out of range");
  }
  return DomainOf((*members)[index]);
}

auto CodeGenFunction::LowerUnion(
    const lir::UnionInstr& u, lir::TypeId result_type)
    -> diag::Result<llvm::Value*> {
  // The live member's value crosses boxed, the way a product's components do;
  // the union domain's `make` takes which member is live beside it.
  auto member_domain = UnionMemberDomain(result_type, u.index.value);
  if (!member_domain) {
    return std::unexpected(std::move(member_domain.error()));
  }
  auto value = LowerOperand(u.value);
  if (!value) {
    return std::unexpected(std::move(value.error()));
  }
  const std::array<llvm::Value*, 1> box{*value};
  llvm::Value* boxed = builder_.CreateCall(
      Entry(
          RuntimeSymbol(*member_domain, RuntimeOp::kValueBox),
          module_->Types().Ptr(), box),
      box);
  auto union_domain = DomainOf(result_type);
  if (!union_domain) {
    return std::unexpected(std::move(union_domain.error()));
  }
  const std::array<llvm::Value*, 2> args{
      llvm::ConstantInt::get(
          llvm::Type::getInt64Ty(module_->Context()), u.index.value),
      boxed};
  return builder_.CreateCall(
      Entry(RuntimeSymbol(*union_domain, RuntimeOp::kMake), result_type, args),
      args);
}

auto CodeGenFunction::LowerTagTest(
    const lir::TagTestInstr& test, lir::TypeId result_type)
    -> diag::Result<llvm::Value*> {
  auto aggregate = LowerOperand(test.aggregate);
  if (!aggregate) {
    return std::unexpected(std::move(aggregate.error()));
  }
  auto domain = DomainOf(OperandType(test.aggregate));
  if (!domain) {
    return std::unexpected(std::move(domain.error()));
  }
  // The predicate is a machine boolean, the shape a value's `to_bool` yields;
  // the packed one-bit surface a pattern expression wears is restored by the
  // enclosing `from_bool`, not here.
  const std::array<llvm::Value*, 2> args{
      *aggregate,
      llvm::ConstantInt::get(
          llvm::Type::getInt64Ty(module_->Context()), test.index.value)};
  return builder_.CreateCall(
      Entry(RuntimeSymbol(*domain, RuntimeOp::kTagMatches), result_type, args),
      args);
}

auto CodeGenFunction::CoordinateDomain(lir::TypeId container) const
    -> diag::Result<std::optional<support::ValueDomain>> {
  const std::optional<lir::TypeId> index =
      DeclaredIndexType(module_->Unit(), container);
  if (!index.has_value()) {
    return std::nullopt;
  }
  auto domain = DomainOf(*index);
  if (!domain) {
    return std::unexpected(std::move(domain.error()));
  }
  return *domain;
}

// The domain a value crossing into a positional part states for itself, and
// nothing where the value it goes into already holds one for that part. A
// product holds every part at once, so a part conforms to the prototype the
// product carries and crosses as the bare handle it is; an active-member value
// holds one part at a time and carries no prototype for the others, so a value
// replacing one has to say which domain it is in. The coordinate rule one
// entry over is the same rule over a keyed container.
auto CodeGenFunction::PartDomain(
    lir::TypeId container, base::ComponentIndex position) const
    -> diag::Result<std::optional<support::ValueDomain>> {
  const lir::Type& ty = module_->Unit().types.Get(container);
  if (!ty.Is<lir::UnionType>() && !ty.Is<lir::TaggedUnionType>()) {
    return std::nullopt;
  }
  auto domain = UnionMemberDomain(container, position.value);
  if (!domain) {
    return std::unexpected(std::move(domain.error()));
  }
  return *domain;
}

auto CodeGenFunction::SelectorArgs(
    lir::TypeId container, const std::vector<lir::Operand>& operands,
    std::vector<llvm::Value*>& shape) -> diag::Result<void> {
  auto coordinate = CoordinateDomain(container);
  if (!coordinate) {
    return std::unexpected(std::move(coordinate.error()));
  }
  for (const lir::Operand& operand : operands) {
    auto lowered = LowerOperand(operand);
    if (!lowered) {
      return std::unexpected(std::move(lowered.error()));
    }
    if (!coordinate->has_value()) {
      shape.push_back(*lowered);
      continue;
    }
    const std::array<llvm::Value*, 1> box{*lowered};
    shape.push_back(builder_.CreateCall(
        Entry(
            RuntimeSymbol(**coordinate, RuntimeOp::kValueBox),
            module_->Types().Ptr(), box),
        box));
  }
  return {};
}

auto CodeGenFunction::LowerAggregateExtract(
    const lir::AggregateExtractInstr& extract) -> diag::Result<llvm::Value*> {
  auto aggregate = LowerOperand(extract.aggregate);
  if (!aggregate) {
    return std::unexpected(std::move(aggregate.error()));
  }
  const lir::TypeId container = OperandType(extract.aggregate);
  // A sequence of handles belongs to no value domain, so which object a
  // coordinate names is answered by the entry that knows the sequence rather
  // than by a value's own element read.
  if (IsHandleSequence(container)) {
    const auto* element = std::get_if<lir::ContainerElement>(&extract.selector);
    if (element == nullptr || element->operands.size() != 1) {
      throw InternalError(
          "llvm codegen: a sequence of handles is reached by one coordinate, "
          "which is what the step naming an element carries");
    }
    auto index = LowerOperand(element->operands.front());
    if (!index) {
      return std::unexpected(std::move(index.error()));
    }
    const std::array<llvm::Value*, 2> args{*aggregate, *index};
    return builder_.CreateCall(
        Entry(
            RuntimeSymbol(RuntimeOp::kSequenceElement), module_->Types().Ptr(),
            args),
        args);
  }
  auto domain = DomainOf(container);
  if (!domain) {
    return std::unexpected(std::move(domain.error()));
  }
  const auto coordinates = [&](const std::vector<lir::Operand>& operands)
      -> diag::Result<std::vector<llvm::Value*>> {
    std::vector<llvm::Value*> shape{*aggregate};
    auto filled = SelectorArgs(container, operands, shape);
    if (!filled) {
      return std::unexpected(std::move(filled.error()));
    }
    return shape;
  };
  // A positional part is named by its index alone. Reaching a product's
  // component and reaching an active-member value's live member ask the same
  // thing of the value, and the aggregate's own domain is what names the entry
  // that answers, so one composition serves both.
  const auto positional = [&](base::ComponentIndex index) -> llvm::Value* {
    const std::array<llvm::Value*, 2> args{
        *aggregate,
        llvm::ConstantInt::get(
            llvm::Type::getInt64Ty(module_->Context()), index.value)};
    return builder_.CreateCall(
        Entry(
            RuntimeSymbol(*domain, RuntimeOp::kExtract), module_->Types().Ptr(),
            args),
        args);
  };
  return std::visit(
      Overloaded{
          [&](const lir::Part& part) -> diag::Result<llvm::Value*> {
            return positional(part.index);
          },
          [&](const lir::ContainerElement& e) -> diag::Result<llvm::Value*> {
            auto shape = coordinates(e.operands);
            if (!shape) {
              return std::unexpected(std::move(shape.error()));
            }
            return builder_.CreateCall(
                Entry(
                    RuntimeSymbol(*domain, support::BuiltinFn::kElement),
                    module_->Types().Ptr(), *shape),
                *shape);
          },
          [&](const lir::ContainerSlice& s) -> diag::Result<llvm::Value*> {
            auto shape = coordinates(s.operands);
            if (!shape) {
              return std::unexpected(std::move(shape.error()));
            }
            return builder_.CreateCall(
                Entry(
                    RuntimeSymbol(*domain, support::BuiltinFn::kSlice),
                    module_->Types().Ptr(), *shape),
                *shape);
          }},
      extract.selector);
}

auto CodeGenFunction::LowerAggregateUpdate(
    const lir::AggregateUpdateInstr& update) -> diag::Result<llvm::Value*> {
  auto aggregate = LowerOperand(update.aggregate);
  if (!aggregate) {
    return std::unexpected(std::move(aggregate.error()));
  }
  auto replacement = LowerOperand(update.replacement);
  if (!replacement) {
    return std::unexpected(std::move(replacement.error()));
  }
  const lir::TypeId container = OperandType(update.aggregate);
  auto domain = DomainOf(container);
  if (!domain) {
    return std::unexpected(std::move(domain.error()));
  }
  const auto coordinates = [&](const std::vector<lir::Operand>& operands)
      -> diag::Result<std::vector<llvm::Value*>> {
    std::vector<llvm::Value*> shape{*aggregate};
    auto filled = SelectorArgs(container, operands, shape);
    if (!filled) {
      return std::unexpected(std::move(filled.error()));
    }
    shape.push_back(*replacement);
    return shape;
  };
  // A positional part is replaced by naming its index and the value that takes
  // its place; the aggregate's own domain names the entry that performs it.
  const auto positional = [&](base::ComponentIndex index,
                              llvm::Value* written) -> llvm::Value* {
    const std::array<llvm::Value*, 3> args{
        *aggregate,
        llvm::ConstantInt::get(
            llvm::Type::getInt64Ty(module_->Context()), index.value),
        written};
    return builder_.CreateCall(
        Entry(
            RuntimeSymbol(*domain, RuntimeOp::kUpdate), module_->Types().Ptr(),
            args),
        args);
  };
  return std::visit(
      Overloaded{
          [&](const lir::Part& part) -> diag::Result<llvm::Value*> {
            // An active-member value keeps no per-member prototype, so the
            // runtime cannot recover which domain a raw handle is in and the
            // caller states it by boxing the replacement in the part's own
            // domain. Whether the write then makes the member live or faults a
            // mismatched tag follows from the domain the entry is named in. A
            // product answers with no such domain, because its parts keep their
            // own and the runtime reads them back.
            auto part_domain = PartDomain(container, part.index);
            if (!part_domain) {
              return std::unexpected(std::move(part_domain.error()));
            }
            llvm::Value* written = *replacement;
            if (part_domain->has_value()) {
              const std::array<llvm::Value*, 1> box{written};
              written = builder_.CreateCall(
                  Entry(
                      RuntimeSymbol(**part_domain, RuntimeOp::kValueBox),
                      module_->Types().Ptr(), box),
                  box);
            }
            return positional(part.index, written);
          },
          [&](const lir::ContainerElement& e) -> diag::Result<llvm::Value*> {
            auto shape = coordinates(e.operands);
            if (!shape) {
              return std::unexpected(std::move(shape.error()));
            }
            return builder_.CreateCall(
                Entry(
                    RuntimeSymbol(*domain, RuntimeOp::kWithElement),
                    module_->Types().Ptr(), *shape),
                *shape);
          },
          [&](const lir::ContainerSlice& s) -> diag::Result<llvm::Value*> {
            auto shape = coordinates(s.operands);
            if (!shape) {
              return std::unexpected(std::move(shape.error()));
            }
            return builder_.CreateCall(
                Entry(
                    RuntimeSymbol(*domain, RuntimeOp::kWithSlice),
                    module_->Types().Ptr(), *shape),
                *shape);
          }},
      update.selector);
}

auto CodeGenFunction::LowerOperand(const lir::Operand& operand)
    -> diag::Result<llvm::Value*> {
  return std::visit(
      Overloaded{
          [&](const lir::Use& use) -> diag::Result<llvm::Value*> {
            return values_.at(use.value);
          },
          [&](const lir::IntConst& c) -> diag::Result<llvm::Value*> {
            return LowerIntConst(c);
          },
          [&](const lir::StrConst& c) -> diag::Result<llvm::Value*> {
            return LowerStrConst(c);
          },
          [&](const lir::RealConst& c) -> diag::Result<llvm::Value*> {
            return LowerRealConst(c);
          },
          [&](const lir::NullConst& c) -> diag::Result<llvm::Value*> {
            return LowerNullConst(c);
          },
          [&](const lir::BoolConst& c) -> diag::Result<llvm::Value*> {
            return llvm::ConstantInt::get(
                llvm::Type::getInt1Ty(module_->Context()),
                static_cast<std::uint64_t>(c.value));
          },
          [&](const lir::PackedTypeRef& c) -> diag::Result<llvm::Value*> {
            return LowerPackedTypeRef(c);
          },
          [&](const lir::FuncRef& f) -> diag::Result<llvm::Value*> {
            return module_->UnitFunction(f.function);
          },
          // The storage behind the symbol is opaque to generated code, which
          // only forwards its address; an i8 placeholder gives the reference a
          // type without encoding what the runtime laid out there. The unit
          // that declares the storage is the one that publishes it, so every
          // reference is a declaration and the host resolves them all.
          [&](const lir::StaticRef& s) -> diag::Result<llvm::Value*> {
            return module_->Module().getOrInsertGlobal(
                s.symbol, llvm::Type::getInt8Ty(module_->Context()));
          }},
      operand);
}

// An integral constant is a machine integer, a native LLVM constant. A packed
// value has no native constant form in the opaque value model -- it is a
// runtime object -- so a constant of one reaches this backend as a call.
auto CodeGenFunction::LowerIntConst(const lir::IntConst& constant)
    -> diag::Result<llvm::Value*> {
  const auto* machine =
      module_->Unit().types.Get(constant.type).As<lir::MachineIntType>();
  if (machine == nullptr) {
    return Unsupported(
        std::format(
            "llvm codegen: a constant of type {} has no native form on this "
            "backend",
            module_->Unit().types.Get(constant.type).KindName()));
  }
  return llvm::ConstantInt::get(
      llvm::cast<llvm::IntegerType>(module_->Types().Map(constant.type)),
      constant.value.value_words.front(),
      machine->signedness == lir::Signedness::kSigned);
}

// The descriptor is built by the first use that reaches it; every later use in
// the run loads the same pointer. It is built once because the type it
// describes settles it once, and the cell is what gives it an address that
// outlives the call that built it.
auto CodeGenFunction::LowerPackedTypeRef(const lir::PackedTypeRef& ref)
    -> diag::Result<llvm::Value*> {
  const std::optional<lir::FunctionId>& initializer =
      module_->Unit().packed_type_initializers.Get(ref.integral);
  if (!initializer.has_value()) {
    throw InternalError(
        "llvm codegen: a described type reached a use with no description");
  }
  llvm::GlobalVariable* cell = module_->PackedTypeCell(ref.integral);
  auto* ptr_ty = module_->Types().Ptr();
  llvm::Value* cached = builder_.CreateLoad(ptr_ty, cell);

  llvm::Function* fn = builder_.GetInsertBlock()->getParent();
  auto* build = llvm::BasicBlock::Create(module_->Context(), "", fn);
  auto* ready = llvm::BasicBlock::Create(module_->Context(), "", fn);
  llvm::BasicBlock* entry = builder_.GetInsertBlock();
  builder_.CreateCondBr(builder_.CreateIsNull(cached), build, ready);

  builder_.SetInsertPoint(build);
  llvm::Value* built =
      builder_.CreateCall(module_->UnitFunction(*initializer), {});
  builder_.CreateStore(built, cell);
  builder_.CreateBr(ready);

  builder_.SetInsertPoint(ready);
  llvm::PHINode* packed_type = builder_.CreatePHI(ptr_ty, 2);
  packed_type->addIncoming(cached, entry);
  packed_type->addIncoming(built, build);
  return packed_type;
}

// A string literal materializes as its native constant bytes; the owning
// runtime String is built from them by a constructor, not at the use site.
auto CodeGenFunction::LowerStrConst(const lir::StrConst& constant)
    -> llvm::Value* {
  return builder_.CreateGlobalStringPtr(constant.value);
}

// A real constant is a machine float, a native LLVM constant. A real-family
// value is a runtime object, so a constant of one reaches this backend as a
// construction over a machine float.
auto CodeGenFunction::LowerRealConst(const lir::RealConst& constant)
    -> diag::Result<llvm::Value*> {
  const auto* machine =
      module_->Unit().types.Get(constant.type).As<lir::MachineFloatType>();
  if (machine == nullptr) {
    return Unsupported(
        std::format(
            "llvm codegen: a real constant of type {} has no native form on "
            "this backend",
            module_->Unit().types.Get(constant.type).KindName()));
  }
  return llvm::ConstantFP::get(
      module_->Types().Map(constant.type), constant.value);
}

// A null value is the host null pointer, a native LLVM constant. Every
// pointer-like domain (chandle, class handle, pointer) shares it: the value is
// the pointer, so its null needs no runtime constructor.
auto CodeGenFunction::LowerNullConst(const lir::NullConst& constant)
    -> llvm::Value* {
  // Referring to nothing is a value of the type like any other, so where the
  // type's values live in storage the null one does too and the constant is a
  // handle to it. A pointer, and a domain whose value is its own handle, have
  // no storage behind the handle for it to name, so there the null is the null
  // pointer itself.
  const std::optional<support::ValueDomain> domain =
      ValueDomainOf(module_->Unit(), constant.type);
  if (domain && !support::ValueDomainIsItsOwnHandle(*domain)) {
    return builder_.CreateCall(
        Entry(
            RuntimeSymbol(*domain, RuntimeOp::kDefault), constant.type,
            std::span<llvm::Value* const>{}),
        std::array<llvm::Value*, 0>{});
  }
  return llvm::ConstantPointerNull::get(
      llvm::cast<llvm::PointerType>(module_->Types().Map(constant.type)));
}

// The entry behind a builtin. What names it is the operation, plus -- where the
// library realizes an operation once per value representation -- the
// representation of the value it acts on. Which of those the builtin takes is
// the builtin's own property, so it is read from its identity.
auto CodeGenFunction::BuiltinCallee(
    const lir::BuiltinTarget& target, const lir::CallInstr& call,
    lir::TypeId result_type, std::span<llvm::Value* const> args)
    -> diag::Result<llvm::FunctionCallee> {
  // The value that names an entry is the one the call qualifies itself with,
  // or the argument at the position the builtin states where it qualifies
  // itself with nothing.
  const auto acted_on = [&](std::size_t operand) -> lir::TypeId {
    if (target.qualifier.has_value()) {
      return *target.qualifier;
    }
    if (operand >= call.args.size()) {
      throw InternalError(
          "llvm codegen: an entry named by a value names it through a "
          "qualifier or an argument, and this call has neither");
    }
    return OperandType(call.args.at(operand));
  };
  const auto over = [&](diag::Result<support::ValueDomain> domain)
      -> diag::Result<llvm::FunctionCallee> {
    if (!domain) {
      return std::unexpected(std::move(domain.error()));
    }
    return Entry(RuntimeSymbol(*domain, target.fn), result_type, args);
  };
  return std::visit(
      Overloaded{
          [&](const NamedAlone&) -> diag::Result<llvm::FunctionCallee> {
            return Entry(RuntimeSymbol(target.fn), result_type, args);
          },
          [&](const NamedByValue& named) -> diag::Result<llvm::FunctionCallee> {
            return over(DomainOf(acted_on(named.operand)));
          },
          [&](const NamedByWrapper&) -> diag::Result<llvm::FunctionCallee> {
            auto wrapper = WrapperBehind(OperandType(call.args.at(0)));
            if (!wrapper) {
              return std::unexpected(std::move(wrapper.error()));
            }
            return Entry(
                RuntimeSymbol(wrapper->domain, wrapper->kind, target.fn),
                result_type, args);
          },
          [&](const NamedByStorageDomain&)
              -> diag::Result<llvm::FunctionCallee> {
            return over(StorageDomainBehind(OperandType(call.args.at(0))));
          },
          [&](const NamedByConversion&) -> diag::Result<llvm::FunctionCallee> {
            auto destination = DomainOf(acted_on(0));
            if (!destination) {
              return std::unexpected(std::move(destination.error()));
            }
            auto source = DomainOf(OperandType(call.args.front()));
            if (!source) {
              return std::unexpected(std::move(source.error()));
            }
            return Entry(
                RuntimeSymbol(*destination, target.fn, *source), result_type,
                args);
          },
          [&](const NotRealized& unrealized)
              -> diag::Result<llvm::FunctionCallee> {
            return Unsupported(
                std::format(
                    "llvm codegen: the {} builtin {} and the library has no "
                    "entry of that shape",
                    support::RuntimeEntryOf(target.fn).name, unrealized.shape));
          }},
      EntryNamingOf(target.fn));
}

auto CodeGenFunction::WrapperPlaceOf(const lir::Place& place) const
    -> diag::Result<std::optional<CodeGenFunction::WrapperPlace>> {
  // The last step names the storage behind whatever the chain had reached. When
  // that is a capability wrapper, the storage is the wrapper's contents, which
  // have no address of their own -- the wrapper decides what reading and
  // writing them mean, so the prefix names the wrapper and the access goes
  // through it.
  if (place.chain.empty() ||
      !std::holds_alternative<lir::DerefProjection>(place.chain.back())) {
    return std::nullopt;
  }
  lir::Place wrapper{
      .base = place.base,
      .chain = {place.chain.begin(), std::prev(place.chain.end())}};
  const lir::Type& opened_type = module_->Unit().types.Get(
      ReachedType(place, std::ssize(place.chain) - 1));
  const std::optional<std::pair<WrapperKind, lir::TypeId>> reached =
      WrapperOf(opened_type);
  if (!reached.has_value()) {
    return std::nullopt;
  }
  auto domain = DomainOf(reached->second);
  if (!domain) {
    return std::unexpected(std::move(domain.error()));
  }
  return WrapperPlace{
      .domain = *domain, .kind = reached->first, .wrapper = std::move(wrapper)};
}

auto CodeGenFunction::CapturePlaceOf(const lir::Place& place) const
    -> std::optional<CapturePlace> {
  // The last step names storage inside whatever the chain had reached, and only
  // a member step reaches a capture.
  const auto* member =
      place.chain.empty()
          ? nullptr
          : std::get_if<lir::MemberProjection>(&place.chain.back());
  if (member == nullptr) {
    return std::nullopt;
  }
  lir::Place holder{
      .base = place.base,
      .chain = {place.chain.begin(), std::prev(place.chain.end())}};
  const lir::TypeId reached = ReachedType(place, std::ssize(place.chain) - 1);
  if (!module_->Unit().types.Get(reached).Is<lir::ClosureType>()) {
    return std::nullopt;
  }
  // A closure extends nothing, so the slot its declaration gave a capture is
  // already where that capture sits in the value.
  return CapturePlace{
      .closure = std::move(holder), .index = member->member.slot.value};
}

auto CodeGenFunction::StorageReached(lir::TypeId operand) const
    -> const lir::Type& {
  const lir::TypePool& types = module_->Unit().types;
  const std::optional<lir::TypeId> pointee = types.Get(operand).Pointee();
  return types.Get(pointee.value_or(operand));
}

auto CodeGenFunction::WrapperBehind(lir::TypeId operand) const
    -> diag::Result<CodeGenFunction::WrapperBehindRef> {
  const std::optional<std::pair<WrapperKind, lir::TypeId>> reached =
      WrapperOf(StorageReached(operand));
  if (!reached.has_value()) {
    throw InternalError(
        "llvm codegen: an operation on a wrapper needs one to act on");
  }
  auto domain = DomainOf(reached->second);
  if (!domain) {
    return std::unexpected(std::move(domain.error()));
  }
  return WrapperBehindRef{.domain = *domain, .kind = reached->first};
}

auto CodeGenFunction::StorageDomainBehind(lir::TypeId operand) const
    -> diag::Result<support::ValueDomain> {
  const std::optional<lir::TypeId> held = ValuesHeldBy(StorageReached(operand));
  if (!held.has_value()) {
    throw InternalError(
        "llvm codegen: an entry named by the representation of what a storage "
        "holds needs a storage whose values are all of one");
  }
  return DomainOf(*held);
}

auto CodeGenFunction::MemberValueCellDomain(
    const lir::Place& place, lir::TypeId value) const
    -> std::optional<support::ValueDomain> {
  // Only a member has storage of its own; a frame slot holds the handle rather
  // than the value it names. A member a place reaches is a variable of its
  // owner -- a snapshot is filled where the owner is built and read through the
  // entry that hands its captures out, never through a place.
  if (place.chain.empty() ||
      !std::holds_alternative<lir::MemberProjection>(place.chain.back())) {
    return std::nullopt;
  }
  if (MemberStorageKindOf(module_->Unit(), value, MemberSlotRole::kVariable) !=
      MemberStorageKind::kValueCell) {
    return std::nullopt;
  }
  return ValueDomainOf(module_->Unit(), value);
}

auto CodeGenFunction::CellDomain(lir::TypeId reference) const
    -> diag::Result<support::ValueDomain> {
  auto wrapper = WrapperBehind(reference);
  if (!wrapper) {
    return std::unexpected(std::move(wrapper.error()));
  }
  if (wrapper->kind != WrapperKind::kCell) {
    throw InternalError("llvm codegen: a cell operation needs a cell address");
  }
  return wrapper->domain;
}

auto CodeGenFunction::ConstructionOf(
    const lir::CallInstr& call, lir::TypeId result) const
    -> diag::Result<Construction> {
  const auto entry = [](std::string symbol) -> Construction {
    return Construction{.symbol = std::move(symbol)};
  };
  // A container holds elements of a representation nothing else states, so
  // building one takes a prototype of the element it is to hold.
  const auto seeded = [](std::string symbol) -> Construction {
    return Construction{.symbol = std::move(symbol), .shape_operand = 0};
  };
  const auto no_construct = [&]() -> std::unexpected<diag::Diagnostic> {
    return Unsupported(
        std::format(
            "llvm codegen: a value of type {} has no construct on this backend",
            module_->Unit().types.Get(result).KindName()));
  };
  const auto no_real_from_host = [&]() -> std::unexpected<diag::Diagnostic> {
    return Unsupported(
        std::format(
            "llvm codegen: building a {} from a host scalar has no entry on "
            "this backend",
            module_->Unit().types.Get(result).KindName()));
  };
  const auto real_from_host = [&]() -> diag::Result<Construction> {
    const lir::Type& arg =
        module_->Unit().types.Get(OperandType(call.args.at(0)));
    if (!arg.Is<lir::MachineFloatType>()) {
      return no_real_from_host();
    }
    auto domain = DomainOf(result);
    if (!domain) {
      return std::unexpected(std::move(domain.error()));
    }
    return entry(RuntimeSymbol(*domain, RuntimeOp::kConst));
  };
  return module_->Unit().types.Get(result).Visit(
      Overloaded{
          [&](const lir::StringType&) -> diag::Result<Construction> {
            return entry(
                RuntimeSymbol(support::ValueDomain::kString, RuntimeOp::kMake));
          },
          // A reference comes into existence as the cell it binds, since that
          // is the one storage it can name. The cell is empty until its
          // initializer installs a representation, so the entry takes nothing
          // but the domain it is chosen by.
          [&](const lir::RefType&) -> diag::Result<Construction> {
            auto domain = CellDomain(result);
            if (!domain) {
              return std::unexpected(std::move(domain.error()));
            }
            return entry(RuntimeSymbol(*domain, RuntimeOp::kCellAlloc));
          },
          [&](const lir::ClosureType&) -> diag::Result<Construction> {
            return Construction{
                .symbol = RuntimeSymbol(RuntimeOp::kClosureMake),
                .operand_form =
                    OperandsAsSpanAfterDefinition{.defined = result}};
          },
          // A container is built over an element list laid down a stated number
          // of times.
          [&](const lir::DynamicArrayType&) -> diag::Result<Construction> {
            return seeded(RuntimeSymbol(
                support::ValueDomain::kDynArray, RuntimeOp::kFromLiteral));
          },
          [&](const lir::UnpackedArrayType&) -> diag::Result<Construction> {
            return seeded(RuntimeSymbol(
                support::ValueDomain::kUnpackedArray, RuntimeOp::kFromLiteral));
          },
          // LRM 7.10.5: a bounded queue enforces a maximum index, which its own
          // type declares, so which of the two entries builds one follows from
          // the type being built.
          [&](const lir::QueueType& q) -> diag::Result<Construction> {
            return seeded(RuntimeSymbol(
                support::ValueDomain::kQueue,
                q.max_bound.has_value() ? RuntimeOp::kFromLiteralBounded
                                        : RuntimeOp::kFromLiteral));
          },
          [&](const lir::AssociativeArrayType&) -> diag::Result<Construction> {
            return seeded(RuntimeSymbol(
                support::ValueDomain::kAssocArray,
                RuntimeOp::kFromEntriesDefault));
          },
          // A sequence outlives the stretch that built it -- the owner keeps
          // its address, and a dimension above it keeps that address as an
          // ordinary element -- so the runtime takes the handles rather than
          // the element list standing as the value.
          [&](const lir::VectorType&) -> diag::Result<Construction> {
            return entry(RuntimeSymbol(RuntimeOp::kSequenceMake));
          },
          [&](const lir::RuntimeLibraryType& r) -> diag::Result<Construction> {
            switch (r.kind) {
              case lir::RuntimeLibraryKind::kPrintLiteralItem:
                return entry(RuntimeSymbol(RuntimeOp::kMakePrintLiteralItem));
              case lir::RuntimeLibraryKind::kHierarchySegment:
                return entry(RuntimeSymbol(RuntimeOp::kMakeSegment));
              case lir::RuntimeLibraryKind::kTrigger:
                return entry(
                    RuntimeSymbol(TriggerConstruction(call.args.size())));
              case lir::RuntimeLibraryKind::kObservation:
                return entry(
                    RuntimeSymbol(ObservationConstruction(call.args.size())));
              case lir::RuntimeLibraryKind::kFormatSpec:
                return entry(RuntimeSymbol(RuntimeOp::kMakeFormatSpec));
              case lir::RuntimeLibraryKind::kPackedRange:
                return entry(RuntimeSymbol(RuntimeOp::kMakePackedRange));
              case lir::RuntimeLibraryKind::kPackedType:
                return entry(RuntimeSymbol(RuntimeOp::kMakePackedType));
              case lir::RuntimeLibraryKind::kPrintValueItem: {
                auto domain = DomainOf(OperandType(call.args.at(0)));
                if (!domain) {
                  return std::unexpected(std::move(domain.error()));
                }
                return entry(
                    RuntimeSymbol(*domain, RuntimeOp::kMakePrintValueItem));
              }
              default:
                return no_construct();
            }
          },
          // A wrapper that owns storage brings that storage into existence with
          // itself. The runtime owns the object tree, so it is the runtime that
          // builds a node of it. A shared owner instead keeps its storage alive
          // for as long as anything holds one, which takes a slot the collector
          // can see, and generated storage is not yet visible to it.
          [&](const lir::PointerType& p) -> diag::Result<Construction> {
            if (p.ownership != lir::PointerOwnership::kUnique) {
              return Unsupported(
                  "llvm codegen: storage kept alive by a shared owner needs a "
                  "slot the collector can see, which generated storage is not "
                  "yet");
            }
            return Construction{
                .symbol = RuntimeSymbol(RuntimeOp::kMakeScope),
                .operand_form = OperandsAfterDefinition{.defined = p.pointee}};
          },
          // An object the program owns rather than the object tree, brought
          // into existence together with the handle that refers to it (LRM
          // 8.3). The definition its class carries says what storage its
          // properties need, so the entry takes that and nothing else.
          [&](const lir::ManagedRefType& m) -> diag::Result<Construction> {
            return Construction{
                .symbol = RuntimeSymbol(RuntimeOp::kObjectMake),
                .operand_form = OperandsAfterDefinition{.defined = m.pointee}};
          },
          // Landing a machine integer in a real and reshaping across precisions
          // are named conversions, so what reaches the real family here is a
          // build over a host scalar: a constant of the destination's own
          // precision. Anything else the boundary hands back has no entry.
          [&](const lir::RealType&) -> diag::Result<Construction> {
            return real_from_host();
          },
          [&](const lir::RealTimeType&) -> diag::Result<Construction> {
            return real_from_host();
          },
          [&](const lir::ShortRealType&) -> diag::Result<Construction> {
            return real_from_host();
          },
          // A value carrying no bits has exactly one value (a tagged union's
          // void member, LRM 7.3.2), so its construction takes nothing and
          // yields that value.
          [&](const lir::EmptyType&) -> diag::Result<Construction> {
            return entry(RuntimeSymbol(
                support::ValueDomain::kEmpty, RuntimeOp::kDefault));
          },
          [&](const auto&) -> diag::Result<Construction> {
            return no_construct();
          }});
}

auto CodeGenFunction::InItsOwnDomain(
    const lir::CallInstr& call, std::size_t position) const
    -> diag::Result<ErasedArgument> {
  if (position >= call.args.size()) {
    throw InternalError(
        "llvm codegen: an entry names an operand it was not handed -- please "
        "report this as a bug");
  }
  auto domain = DomainOf(OperandType(call.args.at(position)));
  if (!domain) {
    return std::unexpected(std::move(domain.error()));
  }
  return ErasedArgument{.position = position, .domain = *domain};
}

auto CodeGenFunction::BuiltinErasedOperand(
    const lir::BuiltinTarget& target, const lir::CallInstr& call) const
    -> diag::Result<std::optional<ErasedArgument>> {
  const support::RuntimeEntry entry = support::RuntimeEntryOf(target.fn);
  // A value put into a positional part crosses in the domain the part states,
  // where the value it goes into holds no prototype for that part. It is the
  // call's last operand, everything before it naming which part. The value it
  // goes into is the type the call is qualified with, because an entry that
  // builds one takes no object to read it from.
  if (target.fn == support::BuiltinFn::kMakeActiveMember) {
    auto part = PartDomain(*target.qualifier, *target.position);
    if (!part) {
      return std::unexpected(std::move(part.error()));
    }
    if (!part->has_value()) {
      return std::nullopt;
    }
    return ErasedArgument{.position = call.args.size() - 1, .domain = **part};
  }
  if (entry.result_prototype_operand.has_value()) {
    return InItsOwnDomain(call, *entry.result_prototype_operand);
  }
  if (entry.spread_operand.has_value()) {
    return InItsOwnDomain(call, *entry.spread_operand);
  }
  if (!entry.index_operand.has_value()) {
    return std::nullopt;
  }
  // A coordinate is the one erased operand whose domain is not its own: what a
  // keyed container selects by is the type that container declares, and a
  // container declaring none takes the coordinate as the value it already is.
  auto coordinate = CoordinateDomain(OperandType(call.args.front()));
  if (!coordinate) {
    return std::unexpected(std::move(coordinate.error()));
  }
  if (!coordinate->has_value()) {
    return std::nullopt;
  }
  return ErasedArgument{
      .position = *entry.index_operand, .domain = **coordinate};
}

auto CodeGenFunction::EncodingOf(const lir::CallInstr& call) const
    -> diag::Result<CallEncoding> {
  using Encoded = diag::Result<CallEncoding>;
  // Only a target named by something other than a signature encodes anything: a
  // library entry is named by its identity and says on its declaration which
  // operand states a representation, and a construction is named by the type it
  // builds, which says both that and the form its entry takes the rest in.
  // Every other target names code whose parameters are already typed, so what
  // the call states is what crosses.
  return std::visit(
      Overloaded{
          [&](const lir::BuiltinTarget& t) -> Encoded {
            auto erased = BuiltinErasedOperand(t, call);
            if (!erased) {
              return std::unexpected(std::move(erased.error()));
            }
            return CallEncoding{.erased = *erased};
          },
          [&](const lir::ConstructTarget& t) -> Encoded {
            auto construction = ConstructionOf(call, t.result);
            if (!construction) {
              return std::unexpected(std::move(construction.error()));
            }
            if (!construction->shape_operand.has_value()) {
              return CallEncoding{.operand_form = construction->operand_form};
            }
            auto erased = InItsOwnDomain(call, *construction->shape_operand);
            if (!erased) {
              return std::unexpected(std::move(erased.error()));
            }
            return CallEncoding{
                .erased = *erased, .operand_form = construction->operand_form};
          },
          [](const lir::FunctionTarget&) -> Encoded { return CallEncoding{}; },
          [](const lir::DispatchTarget&) -> Encoded { return CallEncoding{}; },
          [](const lir::ForeignTarget&) -> Encoded { return CallEncoding{}; },
          [](const lir::ImportedRuntimeTarget&) -> Encoded {
            return CallEncoding{};
          },
          [](const lir::ValueCellTarget&) -> Encoded { return CallEncoding{}; },
          [](const lir::ControlEffectTarget&) -> Encoded {
            return CallEncoding{};
          },
          [](const lir::CoroutineTarget&) -> Encoded {
            return CallEncoding{};
          }},
      call.target);
}

}  // namespace lyra::backend::llvm_backend
