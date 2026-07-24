#include <cstdint>
#include <optional>
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
#include "lyra/lir/compilation_unit.hpp"
#include "lyra/lir/integral_constant.hpp"
#include "lyra/lir/type.hpp"
#include "lyra/lir/type_query.hpp"

namespace lyra::backend::llvm_backend {

auto CodeGenFunction::LowerInstr(const lir::Instr& instr) -> llvm::Value* {
  const lir::TypeId result_type = fn_->values.Get(instr.result).type;
  return std::visit(
      Overloaded{
          [&](const lir::CallInstr& call) -> llvm::Value* {
            return LowerCall(call, result_type);
          },
          [&](const lir::AggregateInstr& agg) -> llvm::Value* {
            return LowerAggregate(agg, result_type);
          },
          [&](const lir::AggregateExtractInstr& extract) -> llvm::Value* {
            return LowerAggregateExtract(extract);
          },
          [&](const lir::AggregateUpdateInstr& update) -> llvm::Value* {
            return LowerAggregateUpdate(update);
          },
          [&](const lir::LoadInstr& load) -> llvm::Value* {
            return builder_.CreateLoad(
                module_->Types().Map(result_type),
                ResolvePlaceAddress(load.place));
          },
          [&](const lir::StoreInstr& store) -> llvm::Value* {
            return builder_.CreateStore(
                LowerOperand(store.value), ResolvePlaceAddress(store.place));
          },
          [&](const lir::AddrOfInstr& addr) -> llvm::Value* {
            return ResolvePlaceAddress(addr.place);
          },
          [&](const lir::BinaryInstr& binary) -> llvm::Value* {
            return LowerBinary(binary);
          },
          [&](const lir::UnaryInstr& unary) -> llvm::Value* {
            return LowerUnary(unary);
          },
          [&](const lir::BoolCastInstr& cast) -> llvm::Value* {
            return LowerBoolCast(cast);
          },
          [&](const lir::PointerCastInstr& cast) -> llvm::Value* {
            // Every reference crosses as the same opaque handle, so retyping it
            // moves no bits.
            return LowerOperand(cast.operand);
          },
          [&](const lir::IntCastInstr& cast) -> llvm::Value* {
            return LowerIntCast(cast, result_type);
          }},
      instr.data);
}

// A place resolves to an address. A place local's storage is its frame slot;
// any other base is a reference value, whose referent the opening dereference
// names. Each further dereference reads the reference held in the storage
// reached so far, and each member step asks the instance for that member's
// storage.
auto CodeGenFunction::ResolvePlaceAddress(const lir::Place& place)
    -> llvm::Value* {
  auto step = place.chain.begin();
  llvm::Value* address = nullptr;

  const auto* use = std::get_if<lir::Use>(&place.base);
  const bool is_place_local =
      use != nullptr &&
      fn_->values.Get(use->value).kind == lir::LocalKind::kPlace;
  if (is_place_local) {
    address = values_.at(use->value);
  } else {
    if (step == place.chain.end() ||
        !std::holds_alternative<lir::DerefProjection>(*step)) {
      throw InternalError(
          "llvm codegen: a place over a value base must open with a "
          "dereference");
    }
    address = LowerOperand(place.base);
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
    -> llvm::Value* {
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
  const ValueDomain domain = DomainOf(operand_type);
  return builder_.CreateCall(
      module_->Runtime().Binary(domain, binary.op),
      {LowerOperand(binary.lhs), LowerOperand(binary.rhs)});
}

auto CodeGenFunction::LowerMachineBinary(const lir::BinaryInstr& binary)
    -> llvm::Value* {
  llvm::Value* lhs = LowerOperand(binary.lhs);
  llvm::Value* rhs = LowerOperand(binary.rhs);
  // The only binary operators that reach machine values compose machine
  // booleans: `&&` and `||` combine two predicates, and `<->` arrives as an
  // equality of the two predicates. Every other operator acts on a value
  // domain, never on a machine value.
  switch (binary.op) {
    case lir::BinaryOp::kLogicalAnd:
      return builder_.CreateAnd(lhs, rhs);
    case lir::BinaryOp::kLogicalOr:
      return builder_.CreateOr(lhs, rhs);
    case lir::BinaryOp::kEquality:
      return builder_.CreateICmpEQ(lhs, rhs);
    default:
      throw InternalError(
          "llvm codegen: binary operator does not apply to machine values");
  }
}

auto CodeGenFunction::LowerUnary(const lir::UnaryInstr& unary) -> llvm::Value* {
  const lir::TypeId operand_type = OperandType(unary.operand);
  // A machine-typed operand is a native value, not a value-domain handle: its
  // operator is a machine instruction, not a runtime-library call. This is how
  // the reduced predicate a real- or chandle-family `!` produces (a machine
  // boolean) is negated before `from_bool` widens it back to a 1-bit packed.
  if (std::holds_alternative<lir::MachineIntType>(
          module_->Unit().types.Get(operand_type).data)) {
    return LowerMachineUnary(unary);
  }
  const ValueDomain domain = DomainOf(operand_type);
  return builder_.CreateCall(
      module_->Runtime().Unary(domain, unary.op),
      {LowerOperand(unary.operand)});
}

auto CodeGenFunction::LowerMachineUnary(const lir::UnaryInstr& unary)
    -> llvm::Value* {
  llvm::Value* operand = LowerOperand(unary.operand);
  switch (unary.op) {
    case lir::UnaryOp::kLogicalNot:
      return builder_.CreateICmpEQ(
          operand, llvm::ConstantInt::get(operand->getType(), 0));
    default:
      throw InternalError(
          "llvm codegen: machine-typed unary operator is not lowerable");
  }
}

auto CodeGenFunction::LowerBoolCast(const lir::BoolCastInstr& cast)
    -> llvm::Value* {
  const ValueDomain domain = DomainOf(OperandType(cast.operand));
  return builder_.CreateCall(
      module_->Runtime().ToBool(domain), {LowerOperand(cast.operand)});
}

// Widening repeats the sign bit only when the *source* is signed; the
// destination's signedness says how the result is later read, not what the
// added high bits hold. Narrowing discards high bits either way.
auto CodeGenFunction::LowerIntCast(
    const lir::IntCastInstr& cast, lir::TypeId result_type) -> llvm::Value* {
  const auto& source = std::get<lir::MachineIntType>(
      module_->Unit().types.Get(OperandType(cast.operand)).data);
  return builder_.CreateIntCast(
      LowerOperand(cast.operand), module_->Types().Map(result_type),
      source.signedness == lir::Signedness::kSigned);
}

auto CodeGenFunction::LowerCall(
    const lir::CallInstr& call, lir::TypeId result_type) -> llvm::Value* {
  std::vector<llvm::Value*> args;
  // A construct that builds a child unit leads with the child's definition
  // reference, which the result type names rather than the operand list.
  if (const auto* construct = std::get_if<lir::ConstructTarget>(&call.target)) {
    if (const auto* dynamic_array = std::get_if<lir::DynamicArrayType>(
            &module_->Unit().types.Get(construct->result).data)) {
      return LowerErasedDynamicArrayConstruct(call, *dynamic_array);
    }
    if (llvm::Value* definition = ConstructDefinitionArg(construct->result)) {
      args.push_back(definition);
    }
  }
  args.reserve(args.size() + call.args.size());
  for (const lir::Operand& arg : call.args) {
    args.push_back(LowerOperand(arg));
  }
  return builder_.CreateCall(ResolveCallee(call, result_type), args);
}

// Every call is a symbol invoked with arguments; the target kinds differ only
// in how the symbol is resolved.
auto CodeGenFunction::ResolveCallee(
    const lir::CallInstr& call, lir::TypeId result_type)
    -> llvm::FunctionCallee {
  return std::visit(
      Overloaded{
          [&](const lir::BuiltinTarget& t) -> llvm::FunctionCallee {
            return BuiltinCallee(t, call, result_type);
          },
          [&](const lir::MethodTarget& t) -> llvm::FunctionCallee {
            return module_->MethodFunction(t.method.class_id, t.method.index);
          },
          [&](const lir::ConstructTarget&) -> llvm::FunctionCallee {
            return ConstructCallee(call);
          },
          [&](const lir::ForeignTarget& t) -> llvm::FunctionCallee {
            return ForeignCallee(t, call, result_type);
          },
          [&](const lir::ActivationFrameTarget& t) -> llvm::FunctionCallee {
            // The value domain an activation-frame call works in is read from
            // the value it moves: the cell's own value type for an allocation
            // or a load, the stored value's type for a store.
            switch (t.op) {
              case lir::ActivationFrameTarget::Op::kAllocate:
                return module_->Runtime().ActivationFrameAlloc(
                    DomainOf(result_type));
              case lir::ActivationFrameTarget::Op::kLoad:
                return module_->Runtime().ActivationFrameLoad(
                    DomainOf(result_type));
              case lir::ActivationFrameTarget::Op::kStore:
                return module_->Runtime().ActivationFrameStore(
                    DomainOf(OperandType(call.args.at(1))));
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
    lir::TypeId result_type) -> llvm::FunctionCallee {
  std::vector<llvm::Type*> params;
  params.reserve(call.args.size());
  for (const lir::Operand& arg : call.args) {
    params.push_back(module_->Types().Map(OperandType(arg)));
  }
  return module_->Module().getOrInsertFunction(
      target.symbol, llvm::FunctionType::get(
                         module_->Types().Map(result_type), params, false));
}

// An array literal is a value built in place, not a runtime call: its elements
// are stored into contiguous storage and named by a {pointer, length} span. A
// dynamic array is a type-erased value, so its literal boxes each element into
// the erased representation first; a fixed unpacked array carries its elements'
// own handles directly.
auto CodeGenFunction::LowerAggregate(
    const lir::AggregateInstr& agg, lir::TypeId result_type) -> llvm::Value* {
  const auto& result_data = module_->Unit().types.Get(result_type).data;
  if (const auto* tuple = std::get_if<lir::TupleType>(&result_data)) {
    return LowerTupleAggregate(agg, *tuple);
  }
  lir::TypeId element_type{};
  bool box_elements = false;
  if (const auto* dynamic_array =
          std::get_if<lir::DynamicArrayType>(&result_data)) {
    element_type = dynamic_array->element_type;
    box_elements = true;
  } else if (
      const auto* array = std::get_if<lir::UnpackedArrayType>(&result_data)) {
    element_type = array->element_type;
  } else {
    throw InternalError(
        "llvm codegen: aggregate result is not an unpacked array, dynamic "
        "array, or tuple");
  }
  auto* storage_ty = llvm::ArrayType::get(
      box_elements ? module_->Types().Ptr()
                   : module_->Types().Map(element_type),
      agg.elements.size());
  llvm::Value* storage = builder_.CreateAlloca(storage_ty);
  for (std::uint32_t i = 0; i < agg.elements.size(); ++i) {
    llvm::Value* element = LowerOperand(agg.elements[i]);
    if (box_elements) {
      element = builder_.CreateCall(
          module_->Runtime().ValueBox(DomainOf(element_type)), {element});
    }
    llvm::Value* slot =
        builder_.CreateConstInBoundsGEP2_64(storage_ty, storage, 0, i);
    builder_.CreateStore(element, slot);
  }
  llvm::Value* span = llvm::UndefValue::get(module_->Types().Span());
  span = builder_.CreateInsertValue(span, storage, {0});
  span = builder_.CreateInsertValue(
      span,
      llvm::ConstantInt::get(
          llvm::Type::getInt64Ty(module_->Context()), agg.elements.size()),
      {1});
  return span;
}

// Realizes the construction of a dynamic-array runtime value: it selects the
// runtime-ABI constructor from the operand shape, boxes the element prototype
// into the erased representation, and emits the call. This is representation
// lowering, not source semantics -- the container's value semantics, its
// out-of-range behavior, and its element-write model belong to the runtime
// object and the MIR-to-LIR lowering, never here. The four shapes are the
// runtime constructors' operand lists: `[proto]` empty, `[size, proto]` sized,
// `[size, proto, src]` sized-from-source, `[proto, literal]` assignment pattern
// -- the literal's span is the one operand whose own type is the array type,
// which separates it from the sized form's leading size.
auto CodeGenFunction::LowerErasedDynamicArrayConstruct(
    const lir::CallInstr& call, const lir::DynamicArrayType& type)
    -> llvm::Value* {
  const ValueDomain element_domain = DomainOf(type.element_type);
  auto box = [&](const lir::Operand& operand) -> llvm::Value* {
    return builder_.CreateCall(
        module_->Runtime().ValueBox(element_domain), {LowerOperand(operand)});
  };
  const std::vector<lir::Operand>& args = call.args;
  if (args.size() == 1) {
    return builder_.CreateCall(
        module_->Runtime().MakeDynamicArrayDefault(), {box(args[0])});
  }
  if (args.size() == 3) {
    return builder_.CreateCall(
        module_->Runtime().MakeDynamicArrayNewCopy(),
        {LowerOperand(args[0]), box(args[1]), LowerOperand(args[2])});
  }
  const lir::TypeId result = std::get<lir::ConstructTarget>(call.target).result;
  if (OperandType(args[1]) == result) {
    return builder_.CreateCall(
        module_->Runtime().MakeDynamicArrayFromLiteral(),
        {box(args[0]), LowerOperand(args[1])});
  }
  return builder_.CreateCall(
      module_->Runtime().MakeDynamicArrayNew(),
      {LowerOperand(args[0]), box(args[1])});
}

// A product value is assembled by boxing each component into a product
// component -- the component's domain names the box entry -- then collecting
// the boxed components into the product. The component domains come from the
// result product type, so the generated side never inspects the components'
// runtime representation.
auto CodeGenFunction::LowerTupleAggregate(
    const lir::AggregateInstr& agg, const lir::TupleType& tuple)
    -> llvm::Value* {
  llvm::Type* handle_ty = module_->Types().Ptr();
  auto* storage_ty = llvm::ArrayType::get(handle_ty, agg.elements.size());
  llvm::Value* storage = builder_.CreateAlloca(storage_ty);
  for (std::uint32_t i = 0; i < agg.elements.size(); ++i) {
    const ValueDomain domain =
        ValueDomainOf(module_->Unit(), tuple.elements[i]);
    llvm::Value* boxed = builder_.CreateCall(
        module_->Runtime().ValueBox(domain), {LowerOperand(agg.elements[i])});
    llvm::Value* slot =
        builder_.CreateConstInBoundsGEP2_64(storage_ty, storage, 0, i);
    builder_.CreateStore(boxed, slot);
  }
  llvm::Value* span = llvm::UndefValue::get(module_->Types().Span());
  span = builder_.CreateInsertValue(span, storage, {0});
  span = builder_.CreateInsertValue(
      span,
      llvm::ConstantInt::get(
          llvm::Type::getInt64Ty(module_->Context()), agg.elements.size()),
      {1});
  return builder_.CreateCall(module_->Runtime().TupleMake(), {span});
}

auto CodeGenFunction::LowerAggregateExtract(
    const lir::AggregateExtractInstr& extract) -> llvm::Value* {
  llvm::Value* aggregate = LowerOperand(extract.aggregate);
  return std::visit(
      Overloaded{[&](const lir::TupleElement& element) -> llvm::Value* {
        return builder_.CreateCall(
            module_->Runtime().TupleExtract(),
            {aggregate,
             llvm::ConstantInt::get(
                 llvm::Type::getInt64Ty(module_->Context()), element.index)});
      }},
      extract.selector);
}

auto CodeGenFunction::LowerAggregateUpdate(
    const lir::AggregateUpdateInstr& update) -> llvm::Value* {
  llvm::Value* aggregate = LowerOperand(update.aggregate);
  llvm::Value* replacement = LowerOperand(update.replacement);
  return std::visit(
      Overloaded{[&](const lir::TupleElement& element) -> llvm::Value* {
        return builder_.CreateCall(
            module_->Runtime().TupleUpdate(),
            {aggregate,
             llvm::ConstantInt::get(
                 llvm::Type::getInt64Ty(module_->Context()), element.index),
             replacement});
      }},
      update.selector);
}

auto CodeGenFunction::LowerOperand(const lir::Operand& operand)
    -> llvm::Value* {
  return std::visit(
      Overloaded{
          [&](const lir::Use& use) -> llvm::Value* {
            return values_.at(use.value);
          },
          [&](const lir::IntConst& c) -> llvm::Value* {
            return LowerIntConst(c);
          },
          [&](const lir::StrConst& c) -> llvm::Value* {
            return LowerStrConst(c);
          },
          [&](const lir::RealConst& c) -> llvm::Value* {
            return LowerRealConst(c);
          },
          [&](const lir::NullConst& c) -> llvm::Value* {
            return LowerNullConst(c);
          },
          [&](const lir::FuncRef& f) -> llvm::Value* {
            return module_->MethodFunction(f.method.class_id, f.method.index);
          }},
      operand);
}

// A machine integer is a native LLVM constant. A packed value has no native
// constant form in the opaque value model -- it is a runtime object -- so its
// constant is materialized by a runtime constructor rather than emitted inline.
// Its physical layout, which would yield a native aggregate constant, is
// derived below this layer.
auto CodeGenFunction::LowerIntConst(const lir::IntConst& constant)
    -> llvm::Value* {
  const std::uint64_t value = constant.value.value_words.empty()
                                  ? 0
                                  : constant.value.value_words.front();
  const bool is_signed = constant.value.signedness == lir::Signedness::kSigned;
  if (const auto* machine = std::get_if<lir::MachineIntType>(
          &module_->Unit().types.Get(constant.type).data)) {
    return llvm::ConstantInt::get(
        llvm::IntegerType::get(module_->Context(), machine->bit_width), value,
        is_signed);
  }
  const bool is_four_state =
      constant.value.state_kind == lir::IntegralStateKind::kFourState;
  auto* i64_ty = llvm::Type::getInt64Ty(module_->Context());
  auto* i1_ty = llvm::Type::getInt1Ty(module_->Context());
  return builder_.CreateCall(
      module_->Runtime().PackedConst(),
      {llvm::ConstantInt::get(i64_ty, value),
       llvm::ConstantInt::get(
           llvm::Type::getInt32Ty(module_->Context()), constant.value.width),
       llvm::ConstantInt::get(i1_ty, is_signed ? 1 : 0),
       llvm::ConstantInt::get(i1_ty, is_four_state ? 1 : 0)});
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
    -> llvm::Value* {
  const ValueDomain domain = DomainOf(constant.type);
  llvm::Type* host = domain == ValueDomain::kShortReal
                         ? llvm::Type::getFloatTy(module_->Context())
                         : llvm::Type::getDoubleTy(module_->Context());
  return builder_.CreateCall(
      module_->Runtime().RealConst(domain),
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
    lir::TypeId result_type) -> llvm::FunctionCallee {
  switch (target.fn) {
    case support::BuiltinFn::kCurrentRuntime:
      return module_->Runtime().CurrentServices();
    case support::BuiltinFn::kFiles:
      return module_->Runtime().Files();
    case support::BuiltinFn::kTimeFormat:
      return module_->Runtime().TimeFormat();
    case support::BuiltinFn::kFormat:
      return module_->Runtime().Format();
    case support::BuiltinFn::kWriteln:
      return module_->Runtime().Writeln();
    case support::BuiltinFn::kWrite:
      return module_->Runtime().Write();
    case support::BuiltinFn::kRegisterInitial:
      return module_->Runtime().RegisterInitial();
    case support::BuiltinFn::kRegisterFinal:
      return module_->Runtime().RegisterFinal();
    case support::BuiltinFn::kDelay:
      return module_->Runtime().Delay();
    case support::BuiltinFn::kWaitAny:
      return module_->Runtime().WaitAny();
    case support::BuiltinFn::kAddOwnedChild:
      return module_->Runtime().AddOwnedChild();
    case support::BuiltinFn::kRegisterSignal:
      return module_->Runtime().RegisterSignal();
    case support::BuiltinFn::kGet:
      return module_->Runtime().CellGet(CellDomain(call.args.at(0)));
    case support::BuiltinFn::kInitialize:
      return module_->Runtime().CellInitialize(CellDomain(call.args.at(0)));
    case support::BuiltinFn::kSet:
      return module_->Runtime().CellSet(CellDomain(call.args.at(0)));
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
    lir::TypeId result_type) -> llvm::FunctionCallee {
  if (!target.qualifier.has_value() && call.args.empty()) {
    throw InternalError(
        "llvm codegen: a value builtin names its type through a qualifier or a "
        "receiver, and this call has neither");
  }
  // An enumeration's own entries read its declared members, which no library
  // over the packed representation can answer. They belong to the enumeration's
  // generated artifact, not to the value domain its representation shares.
  switch (target.fn) {
    case support::BuiltinFn::kEnumFirst:
    case support::BuiltinFn::kEnumLast:
    case support::BuiltinFn::kEnumNum:
    case support::BuiltinFn::kEnumName:
      throw InternalError(
          "llvm codegen: an enumeration's own entries are not yet lowerable");
    default:
      break;
  }
  const ValueDomain domain = target.qualifier.has_value()
                                 ? DomainOf(*target.qualifier)
                                 : DomainOf(OperandType(call.args.front()));
  std::vector<llvm::Type*> params;
  params.reserve(call.args.size());
  for (const lir::Operand& arg : call.args) {
    params.push_back(module_->Types().Map(OperandType(arg)));
  }
  return module_->Runtime().ValueBuiltin(
      domain, target.fn, module_->Types().Map(result_type), params);
}

auto CodeGenFunction::CellDomain(const lir::Operand& cell) const
    -> ValueDomain {
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
    -> llvm::FunctionCallee {
  const lir::TypeId result = std::get<lir::ConstructTarget>(call.target).result;
  return std::visit(
      Overloaded{
          [&](const lir::StringType&) -> llvm::FunctionCallee {
            return module_->Runtime().MakeString();
          },
          [&](const lir::CoroutineType&) -> llvm::FunctionCallee {
            return module_->Runtime().MakeCoroutine();
          },
          [&](const lir::RuntimeLibraryType& r) -> llvm::FunctionCallee {
            switch (r.kind) {
              case lir::RuntimeLibraryKind::kPrintLiteralItem:
                return module_->Runtime().MakePrintLiteralItem();
              case lir::RuntimeLibraryKind::kHierarchySegment:
                return module_->Runtime().MakeSegment();
              case lir::RuntimeLibraryKind::kTrigger:
                return module_->Runtime().MakeTrigger();
              case lir::RuntimeLibraryKind::kFormatSpec:
                return module_->Runtime().MakeFormatSpec(call.args.size());
              case lir::RuntimeLibraryKind::kPrintValueItem:
                return module_->Runtime().MakePrintValueItem(
                    DomainOf(OperandType(call.args.at(0))));
              default:
                throw InternalError(
                    "llvm codegen: runtime-library construct is not yet "
                    "lowerable");
            }
          },
          [&](const lir::PointerType& p) -> llvm::FunctionCallee {
            if (std::holds_alternative<lir::ExternalUnitObjectType>(
                    module_->Unit().types.Get(p.pointee).data)) {
              return module_->Runtime().MakeUnit();
            }
            throw InternalError(
                "llvm codegen: pointer construct is not yet lowerable");
          },
          [&](const lir::RealType&) -> llvm::FunctionCallee {
            return RealConstructCallee(call, ValueDomain::kReal);
          },
          [&](const lir::RealTimeType&) -> llvm::FunctionCallee {
            return RealConstructCallee(call, ValueDomain::kReal);
          },
          [&](const lir::ShortRealType&) -> llvm::FunctionCallee {
            return RealConstructCallee(call, ValueDomain::kShortReal);
          },
          [&](const auto&) -> llvm::FunctionCallee {
            throw InternalError(
                "llvm codegen: construct result type is not yet lowerable");
          }},
      module_->Unit().types.Get(result).data);
}

// A real-family construct is a conversion into `dst`: from a machine int64 (the
// integral-to-real bridge, whose inner step already read the operand out as a
// host integer) or from another real precision (`shortreal` <-> `real`). The
// single operand's type selects which, since the result type fixes only the
// destination precision.
auto CodeGenFunction::RealConstructCallee(
    const lir::CallInstr& call, ValueDomain dst) -> llvm::FunctionCallee {
  const lir::TypeId arg_type = OperandType(call.args.at(0));
  if (std::holds_alternative<lir::MachineIntType>(
          module_->Unit().types.Get(arg_type).data)) {
    return module_->Runtime().RealFromInt(dst);
  }
  return module_->Runtime().RealReshape(dst, DomainOf(arg_type));
}

auto CodeGenFunction::ConstructDefinitionArg(lir::TypeId result)
    -> llvm::Value* {
  // Only a construct of a pointer to an external unit leads with a definition;
  // its reference comes from the type-keyed projection, so this call knows the
  // symbol is needed without knowing how it is named.
  const auto* pointer =
      std::get_if<lir::PointerType>(&module_->Unit().types.Get(result).data);
  if (pointer == nullptr) {
    return nullptr;
  }
  if (!std::holds_alternative<lir::ExternalUnitObjectType>(
          module_->Unit().types.Get(pointer->pointee).data)) {
    return nullptr;
  }
  return module_->UnitDefinitionRef(pointer->pointee);
}

}  // namespace lyra::backend::llvm_backend
