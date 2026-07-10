#include <cstdint>
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

namespace lyra::backend::llvm_backend {

auto CodeGenFunction::LowerInstr(const lir::Instr& instr) -> llvm::Value* {
  const lir::TypeId result_type = fn_->values.Get(instr.result).type;
  return std::visit(
      Overloaded{
          [&](const lir::CallInstr& call) -> llvm::Value* {
            return LowerCall(call);
          },
          [&](const lir::AggregateInstr& agg) -> llvm::Value* {
            return LowerAggregate(agg, result_type);
          },
          [&](const lir::LoadInstr& load) -> llvm::Value* {
            const auto [base, member] = ResolveMemberSlot(load.place);
            return builder_.CreateCall(
                module_->Runtime().LoadField(), {base, member});
          },
          [&](const lir::StoreInstr& store) -> llvm::Value* {
            const auto [base, member] = ResolveMemberSlot(store.place);
            return builder_.CreateCall(
                module_->Runtime().StoreField(),
                {base, member, LowerOperand(store.value)});
          }},
      instr.data);
}

auto CodeGenFunction::LowerCall(const lir::CallInstr& call) -> llvm::Value* {
  std::vector<llvm::Value*> args;
  // A construct that builds a child unit leads with the child's definition
  // reference, which the result type names rather than the operand list.
  if (const auto* construct = std::get_if<lir::ConstructTarget>(&call.target)) {
    if (llvm::Value* definition = ConstructDefinitionArg(construct->result)) {
      args.push_back(definition);
    }
  }
  args.reserve(args.size() + call.args.size());
  for (const lir::Operand& arg : call.args) {
    args.push_back(LowerOperand(arg));
  }
  return builder_.CreateCall(ResolveCallee(call.target), args);
}

// Every call is a symbol invoked with arguments; the target kinds differ only
// in how the symbol is resolved.
auto CodeGenFunction::ResolveCallee(const lir::CallTarget& target)
    -> llvm::FunctionCallee {
  return std::visit(
      Overloaded{
          [&](const lir::BuiltinTarget& t) -> llvm::FunctionCallee {
            return BuiltinCallee(t.fn);
          },
          [&](const lir::MethodTarget& t) -> llvm::FunctionCallee {
            return module_->MethodFunction(t.method.class_id, t.method.index);
          },
          [&](const lir::ConstructTarget& t) -> llvm::FunctionCallee {
            return ConstructCallee(t.result);
          }},
      target);
}

// An array literal is a value built in place, not a runtime call: its elements
// are stored into contiguous storage and named by a {pointer, length} span.
auto CodeGenFunction::LowerAggregate(
    const lir::AggregateInstr& agg, lir::TypeId result_type) -> llvm::Value* {
  const auto* array = std::get_if<lir::UnpackedArrayType>(
      &module_->Unit().types.Get(result_type).data);
  if (array == nullptr) {
    throw InternalError(
        "llvm codegen: aggregate result is not an unpacked array");
  }
  auto* storage_ty = llvm::ArrayType::get(
      module_->Types().Map(array->element_type), agg.elements.size());
  llvm::Value* storage = builder_.CreateAlloca(storage_ty);
  for (std::uint32_t i = 0; i < agg.elements.size(); ++i) {
    llvm::Value* slot =
        builder_.CreateConstInBoundsGEP2_64(storage_ty, storage, 0, i);
    builder_.CreateStore(LowerOperand(agg.elements[i]), slot);
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

auto CodeGenFunction::LowerOperand(const lir::Operand& operand)
    -> llvm::Value* {
  return std::visit(
      Overloaded{
          [&](const lir::Use& use) -> llvm::Value* {
            return values_.at(use.value.value);
          },
          [&](const lir::IntConst& c) -> llvm::Value* {
            return LowerIntConst(c);
          },
          [&](const lir::StrConst& c) -> llvm::Value* {
            return LowerStrConst(c);
          },
          [&](const lir::FuncRef& f) -> llvm::Value* {
            return module_->MethodFunction(f.method.class_id, f.method.index);
          }},
      operand);
}

// The JIT realizes a member place as a runtime-owned slot on the base instance,
// addressed by the member index. Only a single member step is produced so far;
// a deeper projection chain (index, dereference) is realized when its steps
// land.
auto CodeGenFunction::ResolveMemberSlot(const lir::Place& place)
    -> std::pair<llvm::Value*, llvm::Value*> {
  if (place.chain.size() != 1) {
    throw InternalError(
        "llvm codegen: only a single-step member place is yet lowerable");
  }
  const auto* member = std::get_if<lir::MemberProjection>(&place.chain.front());
  if (member == nullptr) {
    throw InternalError(
        "llvm codegen: only a member projection is yet lowerable");
  }
  return {
      LowerOperand(place.base),
      llvm::ConstantInt::get(
          llvm::Type::getInt32Ty(module_->Context()), member->member.value)};
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

auto CodeGenFunction::BuiltinCallee(support::BuiltinFn fn)
    -> llvm::FunctionCallee {
  switch (fn) {
    case support::BuiltinFn::kServices:
      return module_->Runtime().Services();
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
    case support::BuiltinFn::kAddOwnedChild:
      return module_->Runtime().AddOwnedChild();
    default:
      throw InternalError(
          "llvm codegen: builtin is not yet lowerable to the runtime ABI");
  }
}

auto CodeGenFunction::ConstructCallee(lir::TypeId result)
    -> llvm::FunctionCallee {
  return std::visit(
      Overloaded{
          [&](const lir::StringType&) -> llvm::FunctionCallee {
            return module_->Runtime().MakeString();
          },
          [&](const lir::CoroutineType&) -> llvm::FunctionCallee {
            return module_->Runtime().MakeCoroutine();
          },
          [&](const lir::RuntimeLibraryType& r) -> llvm::FunctionCallee {
            if (r.kind == lir::RuntimeLibraryKind::kPrintLiteralItem) {
              return module_->Runtime().MakePrintLiteralItem();
            }
            if (r.kind == lir::RuntimeLibraryKind::kHierarchySegment) {
              return module_->Runtime().MakeSegment();
            }
            throw InternalError(
                "llvm codegen: runtime-library construct is not yet lowerable");
          },
          [&](const lir::PointerType& p) -> llvm::FunctionCallee {
            if (std::holds_alternative<lir::ExternalUnitObjectType>(
                    module_->Unit().types.Get(p.pointee).data)) {
              return module_->Runtime().MakeUnit();
            }
            throw InternalError(
                "llvm codegen: pointer construct is not yet lowerable");
          },
          [&](const auto&) -> llvm::FunctionCallee {
            throw InternalError(
                "llvm codegen: construct result type is not yet lowerable");
          }},
      module_->Unit().types.Get(result).data);
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
