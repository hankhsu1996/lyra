#include <cstdint>
#include <format>
#include <optional>
#include <span>
#include <string>
#include <utility>
#include <variant>
#include <vector>

#include <llvm/IR/Constants.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/Type.h>

#include "lyra/backend/llvm/codegen_function.hpp"
#include "lyra/backend/llvm/codegen_module.hpp"
#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/diag/diag_code.hpp"
#include "lyra/lir/compilation_unit.hpp"
#include "lyra/lir/integral_constant.hpp"
#include "lyra/lir/place_query.hpp"
#include "lyra/lir/type.hpp"
#include "lyra/lir/type_query.hpp"
#include "lyra/support/builtin_fn.hpp"

namespace lyra::backend::llvm_backend {

namespace {

auto Unsupported(std::string message) -> std::unexpected<diag::Diagnostic> {
  return diag::Fail(
      diag::DiagCode::kUnsupportedExpressionForm, std::move(message));
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
          [&](const lir::AggregateExtractInstr& extract)
              -> diag::Result<llvm::Value*> {
            return LowerAggregateExtract(extract);
          },
          [&](const lir::AggregateUpdateInstr& update)
              -> diag::Result<llvm::Value*> {
            return LowerAggregateUpdate(update);
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
            return LowerBinary(binary);
          },
          [&](const lir::UnaryInstr& unary) -> diag::Result<llvm::Value*> {
            return LowerUnary(unary);
          },
          [&](const lir::BoolCastInstr& cast) -> diag::Result<llvm::Value*> {
            return LowerBoolCast(cast);
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
    return builder_.CreateCall(
        module_->Runtime().ClosureCapture(),
        {*closure,
         llvm::ConstantInt::get(
             llvm::Type::getInt32Ty(module_->Context()), capture->index)});
  }
  auto cell = CellPlaceOf(load.place);
  if (!cell) {
    return std::unexpected(std::move(cell.error()));
  }
  if (!cell->has_value()) {
    auto address = ResolvePlaceAddress(load.place);
    if (!address) {
      return std::unexpected(std::move(address.error()));
    }
    return builder_.CreateLoad(module_->Types().Map(result_type), *address);
  }
  const CellPlace& through = **cell;
  auto address = ResolvePlaceAddress(through.cell);
  if (!address) {
    return std::unexpected(std::move(address.error()));
  }
  return builder_.CreateCall(
      module_->Runtime().CellGet(through.domain), {*address});
}

// Writing a place mirrors reading one, and a write through a cell is what wakes
// whoever subscribed to it -- which is why it is the cell that performs the
// write rather than a store to an address.
auto CodeGenFunction::LowerStore(const lir::StoreInstr& store)
    -> diag::Result<llvm::Value*> {
  auto cell = CellPlaceOf(store.place);
  if (!cell) {
    return std::unexpected(std::move(cell.error()));
  }
  if (!cell->has_value()) {
    auto value = LowerOperand(store.value);
    if (!value) {
      return std::unexpected(std::move(value.error()));
    }
    auto address = ResolvePlaceAddress(store.place);
    if (!address) {
      return std::unexpected(std::move(address.error()));
    }
    return builder_.CreateStore(*value, *address);
  }
  const CellPlace& through = **cell;
  auto address = ResolvePlaceAddress(through.cell);
  if (!address) {
    return std::unexpected(std::move(address.error()));
  }
  auto value = LowerOperand(store.value);
  if (!value) {
    return std::unexpected(std::move(value.error()));
  }
  return builder_.CreateCall(
      module_->Runtime().CellSet(through.domain), {*address, *value});
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
    if (step == place.chain.end() ||
        !std::holds_alternative<lir::DerefProjection>(*step)) {
      throw InternalError(
          "llvm codegen: a place over a value base must open with a "
          "dereference");
    }
    auto base = LowerOperand(place.base);
    if (!base) {
      return std::unexpected(std::move(base.error()));
    }
    address = *base;
    ++step;
  }

  for (; step != place.chain.end(); ++step) {
    address = std::visit(
        Overloaded{
            [&](const lir::DerefProjection&) -> llvm::Value* {
              return builder_.CreateLoad(module_->Types().Ptr(), address);
            },
            [&](const lir::MemberProjection& member) -> llvm::Value* {
              return builder_.CreateCall(
                  module_->Runtime().MemberAddress(),
                  {address, llvm::ConstantInt::get(
                                llvm::Type::getInt32Ty(module_->Context()),
                                member.member.value)});
            }},
        *step);
  }
  return address;
}

auto CodeGenFunction::LowerBinary(const lir::BinaryInstr& binary)
    -> diag::Result<llvm::Value*> {
  const lir::TypeId operand_type = OperandType(binary.lhs);
  // A machine boolean is a native value, not a value-domain handle: its
  // operator is a machine instruction, not a runtime-library call. This is how
  // the reduced predicates a real- or string-family `&&` / `||` / `<->`
  // composes are combined before `from_bool` widens the result back to a 1-bit
  // packed.
  if (std::holds_alternative<lir::MachineBoolType>(
          module_->Unit().types.Get(operand_type).data)) {
    return LowerMachineBinary(binary);
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
  return builder_.CreateCall(
      module_->Runtime().Binary(*domain, binary.op), {*lhs, *rhs});
}

auto CodeGenFunction::LowerMachineBinary(const lir::BinaryInstr& binary)
    -> diag::Result<llvm::Value*> {
  auto lhs = LowerOperand(binary.lhs);
  if (!lhs) {
    return std::unexpected(std::move(lhs.error()));
  }
  auto rhs = LowerOperand(binary.rhs);
  if (!rhs) {
    return std::unexpected(std::move(rhs.error()));
  }
  // The only binary operators that reach a machine boolean compose predicates:
  // `&&` and `||` combine two, and `<->` arrives as an equality of two. Every
  // other operator acts on a value domain.
  switch (binary.op) {
    case lir::BinaryOp::kLogicalAnd:
      return builder_.CreateAnd(*lhs, *rhs);
    case lir::BinaryOp::kLogicalOr:
      return builder_.CreateOr(*lhs, *rhs);
    case lir::BinaryOp::kEquality:
      return builder_.CreateICmpEQ(*lhs, *rhs);
    default:
      throw InternalError(
          "llvm codegen: binary operator does not apply to machine values");
  }
}

auto CodeGenFunction::LowerUnary(const lir::UnaryInstr& unary)
    -> diag::Result<llvm::Value*> {
  const lir::TypeId operand_type = OperandType(unary.operand);
  // A machine boolean is a native value, not a value-domain handle: its
  // operator is a machine instruction, not a runtime-library call. This is how
  // the reduced predicate a real- or chandle-family `!` produces is negated
  // before `from_bool` widens it back to a 1-bit packed.
  if (std::holds_alternative<lir::MachineBoolType>(
          module_->Unit().types.Get(operand_type).data)) {
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
  return builder_.CreateCall(
      module_->Runtime().Unary(*domain, unary.op), {*operand});
}

auto CodeGenFunction::LowerMachineUnary(const lir::UnaryInstr& unary)
    -> diag::Result<llvm::Value*> {
  auto operand = LowerOperand(unary.operand);
  if (!operand) {
    return std::unexpected(std::move(operand.error()));
  }
  switch (unary.op) {
    case lir::UnaryOp::kLogicalNot:
      return builder_.CreateICmpEQ(
          *operand, llvm::ConstantInt::get((*operand)->getType(), 0));
    default:
      throw InternalError(
          "llvm codegen: machine-typed unary operator is not lowerable");
  }
}

auto CodeGenFunction::LowerBoolCast(const lir::BoolCastInstr& cast)
    -> diag::Result<llvm::Value*> {
  auto domain = DomainOf(OperandType(cast.operand));
  if (!domain) {
    return std::unexpected(std::move(domain.error()));
  }
  auto operand = LowerOperand(cast.operand);
  if (!operand) {
    return std::unexpected(std::move(operand.error()));
  }
  return builder_.CreateCall(module_->Runtime().ToBool(*domain), {*operand});
}

// Widening repeats the sign bit only when the *source* is signed; the
// destination's signedness says how the result is later read, not what the
// added high bits hold. Narrowing discards high bits either way.
auto CodeGenFunction::LowerIntCast(
    const lir::IntCastInstr& cast, lir::TypeId result_type)
    -> diag::Result<llvm::Value*> {
  const auto& source = std::get<lir::MachineIntType>(
      module_->Unit().types.Get(OperandType(cast.operand)).data);
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
  auto callee = ResolveCallee(call, result_type);
  if (!callee) {
    return std::unexpected(std::move(callee.error()));
  }
  auto args = CallArgs(call, std::move(operands));
  if (!args) {
    return std::unexpected(std::move(args.error()));
  }
  return builder_.CreateCall(*callee, *args);
}

auto CodeGenFunction::CallArgs(
    const lir::CallInstr& call, std::vector<llvm::Value*> operands)
    -> diag::Result<std::vector<llvm::Value*>> {
  auto erased = ErasedOperand(call);
  if (!erased) {
    return std::unexpected(std::move(erased.error()));
  }
  if (const std::optional<ErasedArgument>& argument = *erased) {
    operands[argument->position] = builder_.CreateCall(
        module_->Runtime().ValueBox(argument->domain),
        {operands[argument->position]});
  }
  const auto* construct = std::get_if<lir::ConstructTarget>(&call.target);
  if (construct == nullptr) {
    return operands;
  }
  return ConstructArgs(construct->result, operands);
}

// A type comes into existence one way, so which entry that is and what it takes
// are one answer read from the result type.
auto CodeGenFunction::ConstructArgs(
    lir::TypeId result, const std::vector<llvm::Value*>& operands)
    -> std::vector<llvm::Value*> {
  return std::visit(
      Overloaded{
          [&](const lir::ClosureType&) -> std::vector<llvm::Value*> {
            return {
                module_->DefinitionRef(result),
                SpanOver(operands, module_->Types().Ptr())};
          },
          [&](const lir::PointerType& p) -> std::vector<llvm::Value*> {
            std::vector<llvm::Value*> args{module_->DefinitionRef(p.pointee)};
            args.insert(args.end(), operands.begin(), operands.end());
            return args;
          },
          [&](const auto&) -> std::vector<llvm::Value*> { return operands; }},
      module_->Unit().types.Get(result).data);
}

// Every call is a symbol invoked with arguments; the target kinds differ only
// in how the symbol is resolved.
auto CodeGenFunction::ResolveCallee(
    const lir::CallInstr& call, lir::TypeId result_type)
    -> diag::Result<llvm::FunctionCallee> {
  return std::visit(
      Overloaded{
          [&](const lir::BuiltinTarget& t)
              -> diag::Result<llvm::FunctionCallee> {
            return BuiltinCallee(t, call, result_type);
          },
          [&](const lir::FunctionTarget& t)
              -> diag::Result<llvm::FunctionCallee> {
            return module_->UnitFunction(t.function);
          },
          [&](const lir::ConstructTarget&)
              -> diag::Result<llvm::FunctionCallee> {
            return ConstructCallee(call);
          },
          [&](const lir::ForeignTarget& t)
              -> diag::Result<llvm::FunctionCallee> {
            return ForeignCallee(t, call, result_type);
          },
          [&](const lir::ActivationFrameTarget& t)
              -> diag::Result<llvm::FunctionCallee> {
            // The value domain an activation-frame call works in is read from
            // the value it moves: the cell's own value type for an allocation
            // or a load, the stored value's type for a store.
            const lir::TypeId moved =
                t.op == lir::ActivationFrameTarget::Op::kStore
                    ? OperandType(call.args.at(1))
                    : result_type;
            auto domain = DomainOf(moved);
            if (!domain) {
              return std::unexpected(std::move(domain.error()));
            }
            switch (t.op) {
              case lir::ActivationFrameTarget::Op::kAllocate:
                return module_->Runtime().ActivationFrameAlloc(*domain);
              case lir::ActivationFrameTarget::Op::kLoad:
                return module_->Runtime().ActivationFrameLoad(*domain);
              case lir::ActivationFrameTarget::Op::kStore:
                return module_->Runtime().ActivationFrameStore(*domain);
            }
            throw InternalError(
                "llvm codegen: unknown activation-frame operation");
          },
          [&](const lir::ControlEffectTarget& t)
              -> diag::Result<llvm::FunctionCallee> {
            switch (t.op) {
              case lir::ControlEffectTarget::Op::kHasInvalidatedTarget:
                return module_->Runtime().HasInvalidatedTarget();
              case lir::ControlEffectTarget::Op::kInvalidatedTarget:
                return module_->Runtime().InvalidatedTarget();
              case lir::ControlEffectTarget::Op::kSettleCancelled:
                return module_->Runtime().SettleCancelled();
            }
            throw InternalError(
                "llvm codegen: unknown control-effect operation");
          }},
      call.target);
}

// A foreign symbol is declared, never defined: the host resolves it. Its
// signature is read off the call, whose operands and result the boundary
// already marshaled to the carriers the foreign side declared (LRM 35.5.6), so
// no separate ABI table is consulted here.
auto CodeGenFunction::ForeignCallee(
    const lir::ForeignTarget& target, const lir::CallInstr& call,
    lir::TypeId result_type) -> diag::Result<llvm::FunctionCallee> {
  std::vector<llvm::Type*> params;
  params.reserve(call.args.size());
  for (const lir::Operand& arg : call.args) {
    params.push_back(module_->Types().Map(OperandType(arg)));
  }
  return module_->Module().getOrInsertFunction(
      target.symbol, llvm::FunctionType::get(
                         module_->Types().Map(result_type), params, false));
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
  const auto& machine_array = std::get<lir::MachineArrayType>(
      module_->Unit().types.Get(result_type).data);
  std::vector<llvm::Value*> elements;
  elements.reserve(array.elements.size());
  for (const lir::Operand& element : array.elements) {
    auto lowered = LowerOperand(element);
    if (!lowered) {
      return std::unexpected(std::move(lowered.error()));
    }
    elements.push_back(*lowered);
  }
  return SpanOver(elements, module_->Types().Map(machine_array.element));
}

// A product value is assembled by boxing each component into the erased
// representation its own domain names, then collecting the boxed components.
// The domains come from the result product type, so the generated side never
// inspects a component's runtime representation.
//
// This is the one place the generated side erases anything. A product's
// components each have a domain of their own, so no entry can be named by one
// of them and the caller is the only side that knows them all; every
// homogeneous operation is named by its single domain instead and erases its
// own operands.
auto CodeGenFunction::LowerProduct(
    const lir::ProductInstr& product, lir::TypeId result_type)
    -> diag::Result<llvm::Value*> {
  const auto* tuple =
      std::get_if<lir::TupleType>(&module_->Unit().types.Get(result_type).data);
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
    boxed.push_back(builder_.CreateCall(
        module_->Runtime().ValueBox(*domain), {*component}));
  }
  return builder_.CreateCall(
      module_->Runtime().TupleMake(),
      {SpanOver(boxed, module_->Types().Ptr())});
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

auto CodeGenFunction::SelectorArgs(
    lir::TypeId container, const std::vector<lir::Operand>& operands,
    CallShape& shape) -> diag::Result<void> {
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
      shape.args.push_back(*lowered);
      shape.params.push_back(module_->Types().Map(OperandType(operand)));
      continue;
    }
    shape.args.push_back(builder_.CreateCall(
        module_->Runtime().ValueBox(**coordinate), {*lowered}));
    shape.params.push_back(module_->Types().Ptr());
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
  auto domain = DomainOf(container);
  if (!domain) {
    return std::unexpected(std::move(domain.error()));
  }
  const auto coordinates = [&](const std::vector<lir::Operand>& operands)
      -> diag::Result<CallShape> {
    CallShape shape{.args = {*aggregate}, .params = {module_->Types().Ptr()}};
    auto filled = SelectorArgs(container, operands, shape);
    if (!filled) {
      return std::unexpected(std::move(filled.error()));
    }
    return shape;
  };
  return std::visit(
      Overloaded{
          [&](const lir::TupleElement& element) -> diag::Result<llvm::Value*> {
            return builder_.CreateCall(
                module_->Runtime().TupleExtract(),
                {*aggregate, llvm::ConstantInt::get(
                                 llvm::Type::getInt64Ty(module_->Context()),
                                 element.index.value)});
          },
          [&](const lir::UnionMember&) -> diag::Result<llvm::Value*> {
            return Unsupported(
                "llvm codegen: a union value has no member read on this "
                "backend");
          },
          [&](const lir::ContainerElement& e) -> diag::Result<llvm::Value*> {
            auto shape = coordinates(e.operands);
            if (!shape) {
              return std::unexpected(std::move(shape.error()));
            }
            return builder_.CreateCall(
                module_->Runtime().ElementExtract(*domain, shape->params),
                shape->args);
          },
          [&](const lir::ContainerSlice& s) -> diag::Result<llvm::Value*> {
            auto shape = coordinates(s.operands);
            if (!shape) {
              return std::unexpected(std::move(shape.error()));
            }
            return builder_.CreateCall(
                module_->Runtime().SliceExtract(*domain, shape->params),
                shape->args);
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
      -> diag::Result<CallShape> {
    CallShape shape{.args = {*aggregate}, .params = {module_->Types().Ptr()}};
    auto filled = SelectorArgs(container, operands, shape);
    if (!filled) {
      return std::unexpected(std::move(filled.error()));
    }
    shape.args.push_back(*replacement);
    shape.params.push_back(
        module_->Types().Map(OperandType(update.replacement)));
    return shape;
  };
  return std::visit(
      Overloaded{
          [&](const lir::TupleElement& element) -> diag::Result<llvm::Value*> {
            return builder_.CreateCall(
                module_->Runtime().TupleUpdate(),
                {*aggregate,
                 llvm::ConstantInt::get(
                     llvm::Type::getInt64Ty(module_->Context()),
                     element.index.value),
                 *replacement});
          },
          [&](const lir::UnionMember&) -> diag::Result<llvm::Value*> {
            return Unsupported(
                "llvm codegen: a union value has no member write on this "
                "backend");
          },
          [&](const lir::ContainerElement& e) -> diag::Result<llvm::Value*> {
            auto shape = coordinates(e.operands);
            if (!shape) {
              return std::unexpected(std::move(shape.error()));
            }
            return builder_.CreateCall(
                module_->Runtime().ElementUpdate(*domain, shape->params),
                shape->args);
          },
          [&](const lir::ContainerSlice& s) -> diag::Result<llvm::Value*> {
            auto shape = coordinates(s.operands);
            if (!shape) {
              return std::unexpected(std::move(shape.error()));
            }
            return builder_.CreateCall(
                module_->Runtime().SliceUpdate(*domain, shape->params),
                shape->args);
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
  const auto* machine = std::get_if<lir::MachineIntType>(
      &module_->Unit().types.Get(constant.type).data);
  if (machine == nullptr) {
    return Unsupported(
        std::format(
            "llvm codegen: a constant of type {} has no native form on this "
            "backend",
            lir::TypeKindName(module_->Unit().types.Get(constant.type))));
  }
  return llvm::ConstantInt::get(
      llvm::IntegerType::get(module_->Context(), machine->bit_width),
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
  llvm::PHINode* descriptor = builder_.CreatePHI(ptr_ty, 2);
  descriptor->addIncoming(cached, entry);
  descriptor->addIncoming(built, build);
  return descriptor;
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
  const auto* machine = std::get_if<lir::MachineFloatType>(
      &module_->Unit().types.Get(constant.type).data);
  if (machine == nullptr) {
    return Unsupported(
        std::format(
            "llvm codegen: a real constant of type {} has no native form on "
            "this backend",
            lir::TypeKindName(module_->Unit().types.Get(constant.type))));
  }
  return llvm::ConstantFP::get(
      machine->bit_width == 32 ? llvm::Type::getFloatTy(module_->Context())
                               : llvm::Type::getDoubleTy(module_->Context()),
      constant.value);
}

// A null value is the host null pointer, a native LLVM constant. Every
// pointer-like domain (chandle, class handle, pointer) shares it: the value is
// the pointer, so its null needs no runtime constructor.
auto CodeGenFunction::LowerNullConst(const lir::NullConst& constant)
    -> llvm::Value* {
  return llvm::ConstantPointerNull::get(
      llvm::cast<llvm::PointerType>(module_->Types().Map(constant.type)));
}

auto CodeGenFunction::BuiltinCallee(
    const lir::BuiltinTarget& target, const lir::CallInstr& call,
    lir::TypeId result_type) -> diag::Result<llvm::FunctionCallee> {
  switch (target.fn) {
    case support::BuiltinFn::kCurrentRuntime:
      return module_->Runtime().CurrentServices();
    case support::BuiltinFn::kFiles:
      return module_->Runtime().Files();
    case support::BuiltinFn::kTimeFormat:
      return module_->Runtime().TimeFormat();
    case support::BuiltinFn::kSetTimeFormat:
      return module_->Runtime().SetTimeFormat();
    case support::BuiltinFn::kResetTimeFormat:
      return module_->Runtime().ResetTimeFormat();
    case support::BuiltinFn::kFileOpen:
      return module_->Runtime().FileOpen(call.args.size());
    case support::BuiltinFn::kFileClose:
      return module_->Runtime().FileClose();
    case support::BuiltinFn::kFileGetc:
      return module_->Runtime().FileGetc();
    case support::BuiltinFn::kFileUngetc:
      return module_->Runtime().FileUngetc();
    case support::BuiltinFn::kFileSeek:
      return module_->Runtime().FileSeek();
    case support::BuiltinFn::kFileRewind:
      return module_->Runtime().FileRewind();
    case support::BuiltinFn::kFileTell:
      return module_->Runtime().FileTell();
    case support::BuiltinFn::kFileEof:
      return module_->Runtime().FileEof();
    case support::BuiltinFn::kFileFlush:
      return module_->Runtime().FileFlush(call.args.size());
    case support::BuiltinFn::kCancellationFor:
      return module_->Runtime().CancellationFor();
    case support::BuiltinFn::kIsCancelled:
      return module_->Runtime().IsCancelled();
    case support::BuiltinFn::kFormat:
      return module_->Runtime().Format();
    case support::BuiltinFn::kWriteln:
      return module_->Runtime().Writeln();
    case support::BuiltinFn::kWrite:
      return module_->Runtime().Write();
    case support::BuiltinFn::kDiagnostic:
      return module_->Runtime().Diagnostic();
    case support::BuiltinFn::kEmitInfo:
      return module_->Runtime().EmitInfo();
    case support::BuiltinFn::kEmitWarning:
      return module_->Runtime().EmitWarning();
    case support::BuiltinFn::kEmitError:
      return module_->Runtime().EmitError();
    case support::BuiltinFn::kEmitFatal:
      return module_->Runtime().EmitFatal();
    case support::BuiltinFn::kRegisterInitial:
      return module_->Runtime().RegisterInitial();
    case support::BuiltinFn::kRegisterFinal:
      return module_->Runtime().RegisterFinal();
    case support::BuiltinFn::kSubmitNba:
      return module_->Runtime().SubmitNba();
    case support::BuiltinFn::kSubmitPostponed:
      return module_->Runtime().SubmitPostponed();
    case support::BuiltinFn::kSubmitObserved:
      return module_->Runtime().SubmitObserved();
    case support::BuiltinFn::kDelay:
      return module_->Runtime().Delay();
    case support::BuiltinFn::kWaitAny:
      return module_->Runtime().WaitAny();
    case support::BuiltinFn::kEnterTarget:
      return module_->Runtime().EnterTarget();
    case support::BuiltinFn::kLeaveTarget:
      return module_->Runtime().LeaveTarget();
    case support::BuiltinFn::kDisable:
      return module_->Runtime().Disable();
    case support::BuiltinFn::kEffectNamesTarget:
      return module_->Runtime().EffectNamesTarget();
    case support::BuiltinFn::kSimTime:
      return module_->Runtime().SimTime();
    case support::BuiltinFn::kSTime:
      return module_->Runtime().STime();
    case support::BuiltinFn::kRealTime:
      return module_->Runtime().RealTime();
    case support::BuiltinFn::kFinish:
      return module_->Runtime().Finish();
    case support::BuiltinFn::kFatalFinish:
      return module_->Runtime().FatalFinish();
    case support::BuiltinFn::kRunHostCommand:
      return module_->Runtime().RunHostCommand(call.args.size());
    case support::BuiltinFn::kTestPlusargs:
      return module_->Runtime().TestPlusargs();
    case support::BuiltinFn::kUrandom:
      return module_->Runtime().Urandom();
    case support::BuiltinFn::kUrandomSeeded:
      return module_->Runtime().UrandomSeeded();
    case support::BuiltinFn::kUrandomRange:
      return module_->Runtime().UrandomRange();
    case support::BuiltinFn::kRandom:
      return module_->Runtime().Random();
    case support::BuiltinFn::kDistUniform:
      return module_->Runtime().DistUniform();
    case support::BuiltinFn::kDistNormal:
      return module_->Runtime().DistNormal();
    case support::BuiltinFn::kDistExponential:
      return module_->Runtime().DistExponential();
    case support::BuiltinFn::kDistPoisson:
      return module_->Runtime().DistPoisson();
    case support::BuiltinFn::kDistChiSquare:
      return module_->Runtime().DistChiSquare();
    case support::BuiltinFn::kDistT:
      return module_->Runtime().DistT();
    case support::BuiltinFn::kDistErlang:
      return module_->Runtime().DistErlang();
    case support::BuiltinFn::kAddOwnedChild:
      return module_->Runtime().AddOwnedChild();
    case support::BuiltinFn::kResolveVisibleChild:
      return module_->Runtime().ResolveVisibleChild();
    case support::BuiltinFn::kGetChild:
      return module_->Runtime().GetChild();
    case support::BuiltinFn::kHierarchicalPath:
      return module_->Runtime().HierarchicalPath();
    case support::BuiltinFn::kRegisterSignal:
      return module_->Runtime().RegisterSignal();
    case support::BuiltinFn::kGetSignal:
      return module_->Runtime().GetSignal();
    // The run-time-sized dynamic-array constructions (LRM 7.5.1).
    case support::BuiltinFn::kMakeDynamicArrayDefault:
      return module_->Runtime().MakeDynamicArrayDefault();
    case support::BuiltinFn::kMakeDynamicArrayNew:
      return module_->Runtime().MakeDynamicArrayNew();
    case support::BuiltinFn::kMakeDynamicArrayNewCopy:
      return module_->Runtime().MakeDynamicArrayNewCopy();
    // The word planes cross as spans, which an entry whose signature is minted
    // from its operands' mapped types cannot state: a machine array maps to the
    // storage, and what a call passes is a pointer and a length over it.
    case support::BuiltinFn::kFromWords:
      return module_->Runtime().PackedFromWords();
    case support::BuiltinFn::kInitialize: {
      auto domain = CellDomain(call.args.at(0));
      if (!domain) {
        return std::unexpected(std::move(domain.error()));
      }
      return module_->Runtime().CellInitialize(*domain);
    }
    default:
      return ValueBuiltinCallee(target, call, result_type);
  }
}

// Every remaining builtin is an operation on a value: a static factory of the
// type its qualifier names, or a method of its receiver's type. Either way the
// value domain names the library entry, and the call's own operand and result
// types are its signature. A value whose domain has no library realization --
// a container, an aggregate -- is rejected here rather than resolved to a
// plausible-looking wrong entry.
auto CodeGenFunction::ValueBuiltinCallee(
    const lir::BuiltinTarget& target, const lir::CallInstr& call,
    lir::TypeId result_type) -> diag::Result<llvm::FunctionCallee> {
  if (!target.qualifier.has_value() && call.args.empty()) {
    throw InternalError(
        "llvm codegen: a value builtin names its type through a qualifier or a "
        "receiver, and this call has neither");
  }
  // A builtin whose operand names a domain the library realizes can still ask
  // for work that library does not do. The entry's name is minted from the
  // pair, so nothing about the name itself says whether an entry stands behind
  // it: a builtin with none is refused here, or it becomes a declaration that
  // resolves to no address once the module is brought up.
  switch (target.fn) {
    // An enumeration's own entries read its declared members, which no library
    // over the packed representation can answer. They belong to the
    // enumeration's generated artifact, not to the value domain its
    // representation shares.
    case support::BuiltinFn::kEnumFirst:
    case support::BuiltinFn::kEnumLast:
    case support::BuiltinFn::kEnumNum:
    case support::BuiltinFn::kEnumName:
      return Unsupported(
          std::format(
              "llvm codegen: the {} builtin reads an enumeration's declared "
              "members and has no entry on this backend",
              support::BuiltinFnName(target.fn)));
    // A scan assigns what it parses to the arguments that follow its format
    // (LRM 21.3.4.3), so its entry varies with both how many there are and
    // what each one is; the value library holds no entry of that shape.
    case support::BuiltinFn::kScanString:
    case support::BuiltinFn::kScanFile:
      return Unsupported(
          std::format(
              "llvm codegen: the {} builtin assigns to the output arguments "
              "the call names and has no entry on this backend",
              support::BuiltinFnName(target.fn)));
    // An associative traversal (LRM 7.9.4 -- 7.9.7) answers with an index by
    // writing it into an argument the call names. A value handle is immutable
    // from the generated side, so an entry here would have to yield the index
    // as a result the caller stores, which is the shape every other apparent
    // mutation of a value already takes.
    case support::BuiltinFn::kAssocFirst:
    case support::BuiltinFn::kAssocLast:
    case support::BuiltinFn::kAssocNext:
    case support::BuiltinFn::kAssocPrev:
      return Unsupported(
          std::format(
              "llvm codegen: the {} builtin answers by writing through an "
              "argument the call names and has no entry on this backend",
              support::BuiltinFnName(target.fn)));
    // An unpacked concatenation (LRM 10.10) takes as many parts as the source
    // wrote, each contributing either itself or its own elements, and no C ABI
    // names an entry per arity. Reaching the machine needs the parts folded
    // into a chain of appends, the way a packed join already is.
    case support::BuiltinFn::kMakeQueueConcat:
    case support::BuiltinFn::kSpread:
      return Unsupported(
          std::format(
              "llvm codegen: the {} builtin builds a container from as many "
              "parts as the source wrote and has no entry on this backend",
              support::BuiltinFnName(target.fn)));
    // An array manipulation method (LRM 7.12) runs a body the call supplies,
    // once per entry. The value library reaches that body only as a template
    // its own compiler expands, which is a compiler this backend does not have,
    // so the whole family needs an entry taking the body as a value.
    case support::BuiltinFn::kReverse:
    case support::BuiltinFn::kSort:
    case support::BuiltinFn::kRsort:
    case support::BuiltinFn::kSum:
    case support::BuiltinFn::kProduct:
    case support::BuiltinFn::kAnd:
    case support::BuiltinFn::kOr:
    case support::BuiltinFn::kXor:
    case support::BuiltinFn::kFind:
    case support::BuiltinFn::kFindIndex:
    case support::BuiltinFn::kFindFirst:
    case support::BuiltinFn::kFindFirstIndex:
    case support::BuiltinFn::kFindLast:
    case support::BuiltinFn::kFindLastIndex:
    case support::BuiltinFn::kMin:
    case support::BuiltinFn::kMax:
    case support::BuiltinFn::kUnique:
    case support::BuiltinFn::kUniqueIndex:
    case support::BuiltinFn::kMap:
      return Unsupported(
          std::format(
              "llvm codegen: the {} builtin runs a body the call supplies once "
              "per entry and has no entry on this backend",
              support::BuiltinFnName(target.fn)));
    default:
      break;
  }
  const bool qualified = target.qualifier.has_value();
  const lir::TypeId named =
      qualified ? *target.qualifier : OperandType(call.args.front());
  const std::optional<support::ValueDomain> domain =
      ValueDomainOf(module_->Unit(), named);
  // Two different gaps reach here alike -- a service builtin this backend never
  // declared an entry for, and a value type the runtime library does not
  // realize -- so the refusal carries both the builtin and the type.
  if (!domain) {
    return Unsupported(
        std::format(
            "llvm codegen: the {} builtin has no entry on this backend; its {} "
            "type is {}",
            support::BuiltinFnName(target.fn),
            qualified ? "qualifier" : "receiver",
            lir::TypeKindName(module_->Unit().types.Get(named))));
  }
  // The real family's two conversions cross precisions rather than reshaping
  // within one, so their entries are named by the pair of domains and not by
  // the destination alone.
  if (*domain == support::ValueDomain::kReal ||
      *domain == support::ValueDomain::kShortReal) {
    if (target.fn == support::BuiltinFn::kFromInt) {
      return module_->Runtime().RealFromInt(*domain);
    }
    if (target.fn == support::BuiltinFn::kConvertFrom) {
      auto source = DomainOf(OperandType(call.args.front()));
      if (!source) {
        return std::unexpected(std::move(source.error()));
      }
      return module_->Runtime().RealReshape(*domain, *source);
    }
  }
  std::vector<llvm::Type*> params;
  params.reserve(call.args.size());
  for (const lir::Operand& arg : call.args) {
    params.push_back(module_->Types().Map(OperandType(arg)));
  }
  return module_->Runtime().ValueBuiltin(
      *domain, target.fn, module_->Types().Map(result_type), params);
}

auto CodeGenFunction::CellPlaceOf(const lir::Place& place) const
    -> diag::Result<std::optional<CodeGenFunction::CellPlace>> {
  // The last step names the storage behind whatever the chain had reached. When
  // that is a cell, the storage is the cell's contents, which have no address
  // of their own -- the cell decides what reading and writing them mean, so the
  // prefix names the cell and the access goes through it.
  if (place.chain.empty() ||
      !std::holds_alternative<lir::DerefProjection>(place.chain.back())) {
    return std::nullopt;
  }
  lir::Place cell{
      .base = place.base,
      .chain = {place.chain.begin(), std::prev(place.chain.end())}};
  // The chain up to that final step names what the dereference opens. With no
  // step left it is the base itself, whose type is the base's own: a place over
  // a value base becomes one only once its opening dereference is applied, so
  // the prefix is not yet a place to ask about.
  const lir::TypeId opened = cell.chain.empty()
                                 ? OperandType(cell.base)
                                 : lir::PlaceType(module_->Unit(), *fn_, cell);
  const auto* observable =
      std::get_if<lir::ObservableType>(&module_->Unit().types.Get(opened).data);
  if (observable == nullptr) {
    return std::nullopt;
  }
  auto domain = DomainOf(observable->value);
  if (!domain) {
    return std::unexpected(std::move(domain.error()));
  }
  return CellPlace{.domain = *domain, .cell = std::move(cell)};
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
  // The chain up to that step names what holds the member. With no step left it
  // is the base itself, whose type is the base's own, since a place over a
  // value base becomes one only once its opening dereference is applied.
  lir::Place holder{
      .base = place.base,
      .chain = {place.chain.begin(), std::prev(place.chain.end())}};
  const lir::TypeId reached =
      holder.chain.empty() ? OperandType(holder.base)
                           : lir::PlaceType(module_->Unit(), *fn_, holder);
  if (!std::holds_alternative<lir::ClosureType>(
          module_->Unit().types.Get(reached).data)) {
    return std::nullopt;
  }
  return CapturePlace{
      .closure = std::move(holder), .index = member->member.value};
}

auto CodeGenFunction::CellDomain(const lir::Operand& cell) const
    -> diag::Result<support::ValueDomain> {
  const lir::TypeArena& types = module_->Unit().types;
  const std::optional<lir::TypeId> pointee =
      lir::Pointee(types, OperandType(cell));
  if (!pointee) {
    throw InternalError("llvm codegen: a cell operation needs a cell address");
  }
  const auto* observable =
      std::get_if<lir::ObservableType>(&types.Get(*pointee).data);
  if (observable == nullptr) {
    throw InternalError("llvm codegen: a cell operation needs an observable");
  }
  return DomainOf(observable->value);
}

auto CodeGenFunction::ConstructCallee(const lir::CallInstr& call)
    -> diag::Result<llvm::FunctionCallee> {
  const lir::TypeId result = std::get<lir::ConstructTarget>(call.target).result;
  const auto no_construct = [&]() -> std::unexpected<diag::Diagnostic> {
    return Unsupported(
        std::format(
            "llvm codegen: a value of type {} has no construct on this backend",
            lir::TypeKindName(module_->Unit().types.Get(result))));
  };
  const auto no_real_from_host = [&]() -> std::unexpected<diag::Diagnostic> {
    return Unsupported(
        std::format(
            "llvm codegen: building a {} from a host scalar has no entry on "
            "this backend",
            lir::TypeKindName(module_->Unit().types.Get(result))));
  };
  const auto real_from_host = [&]() -> diag::Result<llvm::FunctionCallee> {
    const lir::TypeData& arg =
        module_->Unit().types.Get(OperandType(call.args.at(0))).data;
    if (!std::holds_alternative<lir::MachineFloatType>(arg)) {
      return no_real_from_host();
    }
    auto domain = DomainOf(result);
    if (!domain) {
      return std::unexpected(std::move(domain.error()));
    }
    return module_->Runtime().RealConst(*domain);
  };
  return std::visit(
      Overloaded{
          [&](const lir::StringType&) -> diag::Result<llvm::FunctionCallee> {
            return module_->Runtime().MakeString();
          },
          [&](const lir::CoroutineType&) -> diag::Result<llvm::FunctionCallee> {
            return module_->Runtime().MakeCoroutine();
          },
          [&](const lir::ClosureType&) -> diag::Result<llvm::FunctionCallee> {
            return module_->Runtime().MakeClosure();
          },
          // A container is built over an element list laid down a stated number
          // of times.
          [&](const lir::DynamicArrayType&)
              -> diag::Result<llvm::FunctionCallee> {
            return module_->Runtime().MakeDynamicArrayFromLiteral();
          },
          [&](const lir::UnpackedArrayType&)
              -> diag::Result<llvm::FunctionCallee> {
            return module_->Runtime().MakeUnpackedArrayFromLiteral();
          },
          [&](const lir::QueueType&) -> diag::Result<llvm::FunctionCallee> {
            return module_->Runtime().MakeQueue(call.args.size());
          },
          [&](const lir::AssociativeArrayType&)
              -> diag::Result<llvm::FunctionCallee> {
            return module_->Runtime().MakeAssociativeArray(call.args.size());
          },
          [&](const lir::RuntimeLibraryType& r)
              -> diag::Result<llvm::FunctionCallee> {
            switch (r.kind) {
              case lir::RuntimeLibraryKind::kPrintLiteralItem:
                return module_->Runtime().MakePrintLiteralItem();
              case lir::RuntimeLibraryKind::kHierarchySegment:
                return module_->Runtime().MakeSegment();
              case lir::RuntimeLibraryKind::kTrigger:
                return module_->Runtime().MakeTrigger();
              case lir::RuntimeLibraryKind::kFormatSpec:
                return module_->Runtime().MakeFormatSpec(call.args.size());
              case lir::RuntimeLibraryKind::kPackedRange:
                return module_->Runtime().MakePackedRange();
              case lir::RuntimeLibraryKind::kPackedType:
                return module_->Runtime().MakePackedType();
              case lir::RuntimeLibraryKind::kPrintValueItem: {
                auto domain = DomainOf(OperandType(call.args.at(0)));
                if (!domain) {
                  return std::unexpected(std::move(domain.error()));
                }
                return module_->Runtime().MakePrintValueItem(*domain);
              }
              default:
                return no_construct();
            }
          },
          // A wrapper that owns an object brings the object into existence with
          // itself. The runtime owns the object tree, so it is the runtime that
          // builds a node of it; a shared owner has no realization here yet.
          [&](const lir::PointerType& p) -> diag::Result<llvm::FunctionCallee> {
            if (p.ownership != lir::PointerOwnership::kUnique) {
              return Unsupported(
                  "llvm codegen: building an object under a shared owner is "
                  "not yet supported on this backend");
            }
            return module_->Runtime().MakeScope();
          },
          // An object the program owns rather than the object tree, reclaimed
          // when the last handle drops.
          [&](const lir::ManagedRefType&)
              -> diag::Result<llvm::FunctionCallee> {
            return Unsupported(
                "llvm codegen: building an object on the managed heap is not "
                "yet supported on this backend");
          },
          // Landing a machine integer in a real and reshaping across precisions
          // are named conversions, so what reaches the real family here is a
          // build over a host scalar: a constant of the destination's own
          // precision. Anything else the boundary hands back has no entry.
          [&](const lir::RealType&) -> diag::Result<llvm::FunctionCallee> {
            return real_from_host();
          },
          [&](const lir::RealTimeType&) -> diag::Result<llvm::FunctionCallee> {
            return real_from_host();
          },
          [&](const lir::ShortRealType&) -> diag::Result<llvm::FunctionCallee> {
            return real_from_host();
          },
          [&](const auto&) -> diag::Result<llvm::FunctionCallee> {
            return no_construct();
          }},
      module_->Unit().types.Get(result).data);
}

auto CodeGenFunction::ElementPrototypeOperand(const lir::CallInstr& call) const
    -> std::optional<std::size_t> {
  if (const auto* construct = std::get_if<lir::ConstructTarget>(&call.target)) {
    return std::visit(
        Overloaded{
            [](const lir::DynamicArrayType&) -> std::optional<std::size_t> {
              return 0;
            },
            [](const lir::UnpackedArrayType&) -> std::optional<std::size_t> {
              return 0;
            },
            [](const lir::QueueType&) -> std::optional<std::size_t> {
              return 0;
            },
            [](const lir::AssociativeArrayType&) -> std::optional<std::size_t> {
              return 0;
            },
            [](const auto&) -> std::optional<std::size_t> {
              return std::nullopt;
            }},
        module_->Unit().types.Get(construct->result).data);
  }
  const auto* builtin = std::get_if<lir::BuiltinTarget>(&call.target);
  if (builtin == nullptr) {
    return std::nullopt;
  }
  switch (builtin->fn) {
    case support::BuiltinFn::kMakeDynamicArrayDefault:
      return 0;
    case support::BuiltinFn::kMakeDynamicArrayNew:
    case support::BuiltinFn::kMakeDynamicArrayNewCopy:
      return 1;
    default:
      return std::nullopt;
  }
}

auto CodeGenFunction::ErasedOperand(const lir::CallInstr& call) const
    -> diag::Result<std::optional<ErasedArgument>> {
  if (const std::optional<std::size_t> prototype =
          ElementPrototypeOperand(call)) {
    auto domain = DomainOf(OperandType(call.args.at(*prototype)));
    if (!domain) {
      return std::unexpected(std::move(domain.error()));
    }
    return ErasedArgument{.position = *prototype, .domain = *domain};
  }
  const auto* builtin = std::get_if<lir::BuiltinTarget>(&call.target);
  if (builtin == nullptr) {
    return std::nullopt;
  }
  const std::optional<std::size_t> index =
      support::ContainerIndexOperand(builtin->fn);
  if (!index.has_value() || *index >= call.args.size()) {
    return std::nullopt;
  }
  auto coordinate = CoordinateDomain(OperandType(call.args.front()));
  if (!coordinate) {
    return std::unexpected(std::move(coordinate.error()));
  }
  if (!coordinate->has_value()) {
    return std::nullopt;
  }
  return ErasedArgument{.position = *index, .domain = **coordinate};
}

}  // namespace lyra::backend::llvm_backend
