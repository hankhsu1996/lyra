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

// A value-domain entry's signature is the call site's own, so the arguments a
// call passes and the parameter types the entry is declared with are built
// together.
struct CallShape {
  std::vector<llvm::Value*> args;
  std::vector<llvm::Type*> params;
};

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
  // Machine-typed operands are native values, not value-domain handles: their
  // operator is a machine instruction, not a runtime-library call. This is how
  // the reduced predicates a real- or string-family `&&` / `||` / `<->`
  // composes (machine booleans) are combined before `from_bool` widens the
  // result back to a 1-bit packed.
  if (std::holds_alternative<lir::MachineIntType>(
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
  // The only binary operators that reach machine values compose machine
  // booleans: `&&` and `||` combine two predicates, and `<->` arrives as an
  // equality of the two predicates. Every other operator acts on a value
  // domain, never on a machine value.
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
  // A machine-typed operand is a native value, not a value-domain handle: its
  // operator is a machine instruction, not a runtime-library call. This is how
  // the reduced predicate a real- or chandle-family `!` produces (a machine
  // boolean) is negated before `from_bool` widens it back to a 1-bit packed.
  if (std::holds_alternative<lir::MachineIntType>(
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
  std::vector<llvm::Value*> args;
  // What a construct builds is named by its result type, not by its operand
  // list: a value whose entry takes its operands as one span is built here
  // outright, and a construct that builds a child unit leads with the child's
  // definition reference.
  if (const auto* construct = std::get_if<lir::ConstructTarget>(&call.target)) {
    const lir::TypeData& built =
        module_->Unit().types.Get(construct->result).data;
    if (const auto* dynamic_array =
            std::get_if<lir::DynamicArrayType>(&built)) {
      return LowerErasedDynamicArrayConstruct(call, *dynamic_array);
    }
    if (const auto* unpacked_array =
            std::get_if<lir::UnpackedArrayType>(&built)) {
      return LowerErasedUnpackedArrayConstruct(call, *unpacked_array);
    }
    if (std::holds_alternative<lir::ClosureType>(built)) {
      return LowerClosureConstruct(call, construct->result);
    }
    if (llvm::Value* definition = ConstructDefinitionArg(construct->result)) {
      args.push_back(definition);
    }
  }
  args.reserve(args.size() + call.args.size());
  for (const lir::Operand& arg : call.args) {
    auto lowered = LowerOperand(arg);
    if (!lowered) {
      return std::unexpected(std::move(lowered.error()));
    }
    args.push_back(*lowered);
  }
  auto callee = ResolveCallee(call, result_type);
  if (!callee) {
    return std::unexpected(std::move(callee.error()));
  }
  return builder_.CreateCall(*callee, args);
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

// Building a closure value allocates the storage its captures need and fills
// it. The definition names both that storage and the body, so the construction
// carries nothing beside it but the initializers, in declaration order; each
// crosses as the handle its own storage kind takes, so nothing here inspects a
// capture's representation.
auto CodeGenFunction::LowerClosureConstruct(
    const lir::CallInstr& call, lir::TypeId result)
    -> diag::Result<llvm::Value*> {
  std::vector<llvm::Value*> captures;
  captures.reserve(call.args.size());
  for (const lir::Operand& arg : call.args) {
    auto lowered = LowerOperand(arg);
    if (!lowered) {
      return std::unexpected(std::move(lowered.error()));
    }
    captures.push_back(*lowered);
  }
  return builder_.CreateCall(
      module_->Runtime().MakeClosure(),
      {module_->DefinitionRef(result),
       SpanOver(captures, module_->Types().Ptr())});
}

auto CodeGenFunction::LowerErasedDynamicArrayConstruct(
    const lir::CallInstr& call, const lir::DynamicArrayType& type)
    -> diag::Result<llvm::Value*> {
  auto element_domain = DomainOf(type.element_type);
  if (!element_domain) {
    return std::unexpected(std::move(element_domain.error()));
  }
  auto box = [&](const lir::Operand& operand) -> diag::Result<llvm::Value*> {
    auto lowered = LowerOperand(operand);
    if (!lowered) {
      return std::unexpected(std::move(lowered.error()));
    }
    return builder_.CreateCall(
        module_->Runtime().ValueBox(*element_domain), {*lowered});
  };
  const std::vector<lir::Operand>& args = call.args;
  if (args.size() == 1) {
    auto prototype = box(args[0]);
    if (!prototype) {
      return std::unexpected(std::move(prototype.error()));
    }
    return builder_.CreateCall(
        module_->Runtime().MakeDynamicArrayDefault(), {*prototype});
  }
  // The second argument is either the literal's storage or an element count,
  // which its own type says: storage is a machine array, a count is not. That
  // is what tells a replicated pattern (prototype, unit, count) apart from
  // `new[N](src)` (size, prototype, source), which are both three operands.
  const bool from_literal = std::holds_alternative<lir::MachineArrayType>(
      module_->Unit().types.Get(OperandType(args[1])).data);
  if (args.size() == 3 && !from_literal) {
    auto size = LowerOperand(args[0]);
    if (!size) {
      return std::unexpected(std::move(size.error()));
    }
    auto prototype = box(args[1]);
    if (!prototype) {
      return std::unexpected(std::move(prototype.error()));
    }
    auto source = LowerOperand(args[2]);
    if (!source) {
      return std::unexpected(std::move(source.error()));
    }
    return builder_.CreateCall(
        module_->Runtime().MakeDynamicArrayNewCopy(),
        {*size, *prototype, *source});
  }
  if (from_literal) {
    auto prototype = box(args[0]);
    if (!prototype) {
      return std::unexpected(std::move(prototype.error()));
    }
    // The elements cross as they are; the array erases them itself, and which
    // domain they are in rides the entry name.
    auto elements = LowerOperand(args[1]);
    if (!elements) {
      return std::unexpected(std::move(elements.error()));
    }
    // An enumerated element list omits the count, being the unit repeated
    // once, so both source forms reach one entry.
    llvm::Value* count =
        llvm::ConstantInt::get(llvm::Type::getInt64Ty(module_->Context()), 1);
    if (args.size() > 2) {
      auto given = LowerOperand(args[2]);
      if (!given) {
        return std::unexpected(std::move(given.error()));
      }
      count = *given;
    }
    return builder_.CreateCall(
        module_->Runtime().MakeDynamicArrayFromLiteral(*element_domain),
        {*prototype, *elements, count});
  }
  auto size = LowerOperand(args[0]);
  if (!size) {
    return std::unexpected(std::move(size.error()));
  }
  auto prototype = box(args[1]);
  if (!prototype) {
    return std::unexpected(std::move(prototype.error()));
  }
  return builder_.CreateCall(
      module_->Runtime().MakeDynamicArrayNew(), {*size, *prototype});
}

// A fixed-size array is built from an element default, a repeat unit, and how
// many times that unit repeats (LRM 10.9.1; LRM Table 7-1 for the all-default
// form). An enumerated element list omits the count, being the unit repeated
// once, so the two source forms are the same operation and reach one entry.
// The declared range is not among the operands -- the coordinate system
// belongs to the receiver's static type and reaches a select as its own
// operand, never the payload.
auto CodeGenFunction::LowerErasedUnpackedArrayConstruct(
    const lir::CallInstr& call, const lir::UnpackedArrayType& type)
    -> diag::Result<llvm::Value*> {
  auto element_domain = DomainOf(type.element_type);
  if (!element_domain) {
    return std::unexpected(std::move(element_domain.error()));
  }
  auto prototype = LowerOperand(call.args.at(0));
  if (!prototype) {
    return std::unexpected(std::move(prototype.error()));
  }
  llvm::Value* boxed = builder_.CreateCall(
      module_->Runtime().ValueBox(*element_domain), {*prototype});
  // The elements cross as they are; the array erases them itself, and which
  // domain they are in rides the entry name.
  auto unit = LowerOperand(call.args.at(1));
  if (!unit) {
    return std::unexpected(std::move(unit.error()));
  }
  llvm::Value* count =
      llvm::ConstantInt::get(llvm::Type::getInt64Ty(module_->Context()), 1);
  if (call.args.size() > 2) {
    auto given = LowerOperand(call.args[2]);
    if (!given) {
      return std::unexpected(std::move(given.error()));
    }
    count = *given;
  }
  return builder_.CreateCall(
      module_->Runtime().MakeUnpackedArrayFromLiteral(*element_domain),
      {boxed, *unit, count});
}

// A product value is assembled by boxing each component into the erased
// representation its own domain names, then collecting the boxed components.
// The domains come from the result product type, so the generated side never
// inspects a component's runtime representation.
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

auto CodeGenFunction::LowerAggregateExtract(
    const lir::AggregateExtractInstr& extract) -> diag::Result<llvm::Value*> {
  auto aggregate = LowerOperand(extract.aggregate);
  if (!aggregate) {
    return std::unexpected(std::move(aggregate.error()));
  }
  auto domain = DomainOf(OperandType(extract.aggregate));
  if (!domain) {
    return std::unexpected(std::move(domain.error()));
  }
  const auto coordinates = [&](const std::vector<lir::Operand>& operands)
      -> diag::Result<CallShape> {
    CallShape shape{.args = {*aggregate}, .params = {module_->Types().Ptr()}};
    for (const lir::Operand& operand : operands) {
      auto lowered = LowerOperand(operand);
      if (!lowered) {
        return std::unexpected(std::move(lowered.error()));
      }
      shape.args.push_back(*lowered);
      shape.params.push_back(module_->Types().Map(OperandType(operand)));
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
  auto domain = DomainOf(OperandType(update.aggregate));
  if (!domain) {
    return std::unexpected(std::move(domain.error()));
  }
  const auto coordinates = [&](const std::vector<lir::Operand>& operands)
      -> diag::Result<CallShape> {
    CallShape shape{.args = {*aggregate}, .params = {module_->Types().Ptr()}};
    for (const lir::Operand& operand : operands) {
      auto lowered = LowerOperand(operand);
      if (!lowered) {
        return std::unexpected(std::move(lowered.error()));
      }
      shape.args.push_back(*lowered);
      shape.params.push_back(module_->Types().Map(OperandType(operand)));
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
          [&](const lir::FuncRef& f) -> diag::Result<llvm::Value*> {
            return module_->UnitFunction(f.function);
          }},
      operand);
}

// A machine integer is a native LLVM constant. A packed value has no native
// constant form in the opaque value model -- it is a runtime object -- so its
// constant is materialized by a runtime constructor rather than emitted inline.
// Its physical layout, which would yield a native aggregate constant, is
// derived below this layer.
auto CodeGenFunction::LowerIntConst(const lir::IntConst& constant)
    -> diag::Result<llvm::Value*> {
  if (const auto* machine = std::get_if<lir::MachineIntType>(
          &module_->Unit().types.Get(constant.type).data)) {
    return llvm::ConstantInt::get(
        llvm::IntegerType::get(module_->Context(), machine->bit_width),
        constant.value.value_words.front(),
        machine->signedness == lir::Signedness::kSigned);
  }
  auto* i64_ty = llvm::Type::getInt64Ty(module_->Context());
  auto* i1_ty = llvm::Type::getInt1Ty(module_->Context());
  const auto words_global = [&](const std::vector<llvm::Constant*>& entries) {
    auto* array_ty = llvm::ArrayType::get(i64_ty, entries.size());
    auto* global = new llvm::GlobalVariable(
        module_->Module(), array_ty, true, llvm::GlobalValue::PrivateLinkage,
        llvm::ConstantArray::get(array_ty, entries));
    global->setUnnamedAddr(llvm::GlobalValue::UnnamedAddr::Global);
    return global;
  };
  const auto plane = [&](const std::vector<std::uint64_t>& words) {
    std::vector<llvm::Constant*> entries;
    entries.reserve(words.size());
    for (const std::uint64_t word : words) {
      entries.push_back(llvm::ConstantInt::get(i64_ty, word));
    }
    return words_global(entries);
  };

  // The declared shape travels with the constant: its dimension stack so a
  // multi-dim packed value keeps its shape into element / slice access, and its
  // signedness and state-ness so the runtime builds the value the destination
  // type declares -- the literal is already in that type by here.
  const lir::PackedArrayType& shape =
      lir::PackedShape(module_->Unit().types, constant.type);
  std::vector<llvm::Constant*> bounds;
  bounds.reserve(shape.dims.size() * 2);
  for (const lir::PackedRange& range : shape.dims) {
    bounds.push_back(llvm::ConstantInt::get(i64_ty, range.left));
    bounds.push_back(llvm::ConstantInt::get(i64_ty, range.right));
  }

  // Both planes cross exactly as the constant holds them, so a width past one
  // machine word and the X / Z bits of a 4-state literal reach the runtime
  // intact. Checking them against the width the shape spans is the runtime's:
  // a concrete size is derived below this layer, never here.
  return builder_.CreateCall(
      module_->Runtime().PackedConst(),
      {plane(constant.value.value_words),
       llvm::ConstantInt::get(i64_ty, constant.value.value_words.size()),
       plane(constant.value.state_words),
       llvm::ConstantInt::get(i64_ty, constant.value.state_words.size()),
       words_global(bounds),
       llvm::ConstantInt::get(
           i64_ty, static_cast<std::uint64_t>(shape.dims.size())),
       llvm::ConstantInt::get(
           i1_ty, shape.signedness == lir::Signedness::kSigned ? 1 : 0),
       llvm::ConstantInt::get(
           i1_ty, shape.atom != lir::BitAtom::kBit ? 1 : 0)});
}

// A string literal materializes as its native constant bytes; the owning
// runtime String is built from them by a constructor, not at the use site.
auto CodeGenFunction::LowerStrConst(const lir::StrConst& constant)
    -> llvm::Value* {
  return builder_.CreateGlobalStringPtr(constant.value);
}

// A real literal has no native constant form in the opaque value model -- it is
// a runtime object -- so its constant is a host-precision immediate handed to a
// runtime constructor, the same shape a packed constant takes.
auto CodeGenFunction::LowerRealConst(const lir::RealConst& constant)
    -> diag::Result<llvm::Value*> {
  auto domain = DomainOf(constant.type);
  if (!domain) {
    return std::unexpected(std::move(domain.error()));
  }
  llvm::Type* host = *domain == ValueDomain::kShortReal
                         ? llvm::Type::getFloatTy(module_->Context())
                         : llvm::Type::getDoubleTy(module_->Context());
  return builder_.CreateCall(
      module_->Runtime().RealConst(*domain),
      {llvm::ConstantFP::get(host, constant.value)});
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
  const std::optional<ValueDomain> domain =
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
    -> diag::Result<ValueDomain> {
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
  return std::visit(
      Overloaded{
          [&](const lir::StringType&) -> diag::Result<llvm::FunctionCallee> {
            return module_->Runtime().MakeString();
          },
          [&](const lir::CoroutineType&) -> diag::Result<llvm::FunctionCallee> {
            return module_->Runtime().MakeCoroutine();
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
          // A construct whose result is a pointer to an object builds a scope:
          // the runtime owns the object tree, so it is the runtime that builds
          // a node of it. An object the program owns instead is a managed
          // reference, a different result type reaching a different arm.
          [&](const lir::PointerType&) -> diag::Result<llvm::FunctionCallee> {
            return module_->Runtime().MakeScope();
          },
          [&](const lir::RealType&) -> diag::Result<llvm::FunctionCallee> {
            return RealConstructCallee(call, ValueDomain::kReal);
          },
          [&](const lir::RealTimeType&) -> diag::Result<llvm::FunctionCallee> {
            return RealConstructCallee(call, ValueDomain::kReal);
          },
          [&](const lir::ShortRealType&) -> diag::Result<llvm::FunctionCallee> {
            return RealConstructCallee(call, ValueDomain::kShortReal);
          },
          [&](const auto&) -> diag::Result<llvm::FunctionCallee> {
            return no_construct();
          }},
      module_->Unit().types.Get(result).data);
}

// A real-family construct is a conversion into `dst`: from a machine int64 (the
// integral-to-real bridge, whose inner step already read the operand out as a
// host integer) or from another real precision (`shortreal` <-> `real`). The
// single operand's type selects which, since the result type fixes only the
// destination precision.
auto CodeGenFunction::RealConstructCallee(
    const lir::CallInstr& call, ValueDomain dst)
    -> diag::Result<llvm::FunctionCallee> {
  const lir::TypeId arg_type = OperandType(call.args.at(0));
  if (std::holds_alternative<lir::MachineIntType>(
          module_->Unit().types.Get(arg_type).data)) {
    return module_->Runtime().RealFromInt(dst);
  }
  auto source = DomainOf(arg_type);
  if (!source) {
    return std::unexpected(std::move(source.error()));
  }
  return module_->Runtime().RealReshape(dst, *source);
}

auto CodeGenFunction::ConstructDefinitionArg(lir::TypeId result)
    -> llvm::Value* {
  // A construct of a scope leads with that scope class's definition; the
  // reference comes from the type-keyed projection, so this call knows the
  // symbol is needed without knowing how it is named.
  const auto* pointer =
      std::get_if<lir::PointerType>(&module_->Unit().types.Get(result).data);
  if (pointer == nullptr) {
    return nullptr;
  }
  return module_->DefinitionRef(pointer->pointee);
}

}  // namespace lyra::backend::llvm_backend
