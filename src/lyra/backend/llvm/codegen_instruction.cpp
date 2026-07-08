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
          }},
      instr.data);
}

auto CodeGenFunction::LowerCall(const lir::CallInstr& call) -> llvm::Value* {
  std::vector<llvm::Value*> args;
  args.reserve(call.args.size());
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
            throw InternalError(
                "llvm codegen: runtime-library construct is not yet lowerable");
          },
          [&](const auto&) -> llvm::FunctionCallee {
            throw InternalError(
                "llvm codegen: construct result type is not yet lowerable");
          }},
      module_->Unit().types.Get(result).data);
}

}  // namespace lyra::backend::llvm_backend
