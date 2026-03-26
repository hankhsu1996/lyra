#include "lyra/llvm_backend/dpi_abi.hpp"

#include <format>
#include <vector>

#include <llvm/IR/CallingConv.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/Instructions.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/Type.h>

#include "lyra/common/internal_error.hpp"
#include "lyra/llvm_backend/commit.hpp"
#include "lyra/llvm_backend/compute/operand.hpp"
#include "lyra/llvm_backend/context.hpp"

namespace lyra::lowering::mir_to_llvm::dpi {

namespace {

void ValidateDpiSignature(const mir::DpiSignature& sig, const char* caller) {
  if (!IsValidDpiReturnType(sig.return_type)) {
    throw common::InternalError(
        caller, "invalid DPI return type in frozen signature");
  }
  for (DpiAbiTypeClass t : sig.param_types) {
    if (!IsValidDpiParamType(t)) {
      throw common::InternalError(
          caller, "invalid DPI parameter type in frozen signature");
    }
  }
}

// Coerce a Lyra-internal SSA value to the DPI C ABI type.
// Internal types may differ from ABI types (e.g., bit is i1 internally
// but i8 at the C boundary).
auto CoerceToDpiAbiType(
    Context& context, llvm::Value* value, DpiAbiTypeClass abi_type)
    -> llvm::Value* {
  auto& builder = context.GetBuilder();
  llvm::Type* target = GetLlvmDpiType(context.GetLlvmContext(), abi_type);

  if (value->getType() == target) {
    return value;
  }

  // Integer widening/narrowing (e.g., i1 -> i8 for bit, or width mismatch).
  if (value->getType()->isIntegerTy() && target->isIntegerTy()) {
    unsigned src_bits = value->getType()->getIntegerBitWidth();
    unsigned dst_bits = target->getIntegerBitWidth();
    if (src_bits < dst_bits) {
      return builder.CreateZExt(value, target);
    }
    return builder.CreateTrunc(value, target);
  }

  // Float <-> double (shortreal <-> real boundary, unlikely in D1 but safe).
  if (value->getType()->isFloatTy() && target->isDoubleTy()) {
    return builder.CreateFPExt(value, target);
  }
  if (value->getType()->isDoubleTy() && target->isFloatTy()) {
    return builder.CreateFPTrunc(value, target);
  }

  throw common::InternalError(
      "CoerceToDpiAbiType",
      "cannot coerce between incompatible LLVM types at DPI boundary");
}

// Coerce a DPI C ABI return value back to the Lyra-internal type expected
// by the staging temp.
auto CoerceFromDpiAbiType(
    Context& context, llvm::Value* value, DpiAbiTypeClass abi_type,
    llvm::Type* internal_type) -> llvm::Value* {
  llvm::Type* abi_llvm_type =
      GetLlvmDpiType(context.GetLlvmContext(), abi_type);
  if (value->getType() != abi_llvm_type) {
    throw common::InternalError(
        "CoerceFromDpiAbiType",
        "DPI return value does not match frozen ABI type");
  }
  if (value->getType() == internal_type) {
    return value;
  }

  auto& builder = context.GetBuilder();

  if (value->getType()->isIntegerTy() && internal_type->isIntegerTy()) {
    unsigned src_bits = value->getType()->getIntegerBitWidth();
    unsigned dst_bits = internal_type->getIntegerBitWidth();
    if (src_bits < dst_bits) {
      return builder.CreateZExt(value, internal_type);
    }
    return builder.CreateTrunc(value, internal_type);
  }

  if (value->getType()->isFloatTy() && internal_type->isDoubleTy()) {
    return builder.CreateFPExt(value, internal_type);
  }
  if (value->getType()->isDoubleTy() && internal_type->isFloatTy()) {
    return builder.CreateFPTrunc(value, internal_type);
  }

  throw common::InternalError(
      "CoerceFromDpiAbiType",
      "cannot coerce DPI return value to internal type");
}

}  // namespace

auto GetLlvmDpiType(llvm::LLVMContext& ctx, DpiAbiTypeClass t) -> llvm::Type* {
  switch (t) {
    case DpiAbiTypeClass::kBit:
      return llvm::Type::getInt8Ty(ctx);
    case DpiAbiTypeClass::kByte:
      return llvm::Type::getInt8Ty(ctx);
    case DpiAbiTypeClass::kShortInt:
      return llvm::Type::getInt16Ty(ctx);
    case DpiAbiTypeClass::kInt:
      return llvm::Type::getInt32Ty(ctx);
    case DpiAbiTypeClass::kLongInt:
      return llvm::Type::getInt64Ty(ctx);
    case DpiAbiTypeClass::kReal:
      return llvm::Type::getDoubleTy(ctx);
    case DpiAbiTypeClass::kShortReal:
      return llvm::Type::getFloatTy(ctx);
    case DpiAbiTypeClass::kString:
      return llvm::PointerType::getUnqual(ctx);
    case DpiAbiTypeClass::kVoid:
      return llvm::Type::getVoidTy(ctx);
    case DpiAbiTypeClass::kInvalid:
      break;
  }
  throw common::InternalError("GetLlvmDpiType", "invalid DPI ABI type class");
}

auto GetOrDeclareDpiImport(
    Context& context, const std::string& c_name, const mir::DpiSignature& sig)
    -> llvm::Function* {
  ValidateDpiSignature(sig, "GetOrDeclareDpiImport");

  auto& llvm_ctx = context.GetLlvmContext();
  auto& module = context.GetModule();

  std::vector<llvm::Type*> param_types;
  param_types.reserve(sig.param_types.size());
  for (DpiAbiTypeClass t : sig.param_types) {
    param_types.push_back(GetLlvmDpiType(llvm_ctx, t));
  }

  llvm::Type* ret_type = GetLlvmDpiType(llvm_ctx, sig.return_type);
  auto* fn_type = llvm::FunctionType::get(ret_type, param_types, false);

  if (auto* existing = module.getFunction(c_name)) {
    if (existing->getFunctionType() != fn_type) {
      throw common::InternalError(
          "GetOrDeclareDpiImport",
          std::format(
              "DPI import '{}' already declared with mismatched type", c_name));
    }
    if (existing->getCallingConv() != llvm::CallingConv::C) {
      throw common::InternalError(
          "GetOrDeclareDpiImport",
          std::format(
              "DPI import '{}' already declared with non-C calling convention",
              c_name));
    }
    return existing;
  }

  auto* fn = llvm::Function::Create(
      fn_type, llvm::Function::ExternalLinkage, c_name, &module);
  fn->setCallingConv(llvm::CallingConv::C);
  return fn;
}

auto LowerDpiImportCall(
    Context& context, const mir::Call& call, const mir::DpiImportRef& ref)
    -> Result<void> {
  ValidateDpiSignature(ref.signature, "LowerDpiImportCall");

  if ((ref.signature.return_kind == mir::DpiReturnKind::kVoid) !=
      (ref.signature.return_type == DpiAbiTypeClass::kVoid)) {
    throw common::InternalError(
        "LowerDpiImportCall",
        "inconsistent DPI return kind/type in frozen signature");
  }

  if (call.in_args.size() != ref.signature.param_types.size()) {
    throw common::InternalError(
        "LowerDpiImportCall",
        std::format(
            "DPI argument count mismatch: {} args vs {} params",
            call.in_args.size(), ref.signature.param_types.size()));
  }

  llvm::Function* fn =
      GetOrDeclareDpiImport(context, ref.c_name, ref.signature);
  auto& builder = context.GetBuilder();

  // Marshal arguments: lower from internal representation, coerce to C ABI.
  // String arguments are borrowed views (handle -> const char*) via
  // LyraStringGetCStr. Scalar arguments use bitwise coercion.
  std::vector<llvm::Value*> args;
  args.reserve(call.in_args.size());
  for (size_t i = 0; i < call.in_args.size(); ++i) {
    auto val = LowerOperand(context, call.in_args[i]);
    if (!val) return std::unexpected(val.error());
    if (ref.signature.param_types[i] == DpiAbiTypeClass::kString) {
      // Null handle is a valid internal representation for empty string.
      // LyraStringGetCStr requires non-null, so normalize null to "" here.
      llvm::Value* str_handle = *val;
      auto* ptr_ty = llvm::PointerType::getUnqual(context.GetLlvmContext());
      auto* is_null = builder.CreateICmpEQ(
          str_handle, llvm::ConstantPointerNull::get(ptr_ty), "dpi.str.null");

      auto* cur_bb = builder.GetInsertBlock();
      auto* parent_fn = cur_bb->getParent();
      auto* nonnull_bb = llvm::BasicBlock::Create(
          context.GetLlvmContext(), "dpi.str.nonnull", parent_fn);
      auto* join_bb = llvm::BasicBlock::Create(
          context.GetLlvmContext(), "dpi.str.join", parent_fn);

      builder.CreateCondBr(is_null, join_bb, nonnull_bb);

      builder.SetInsertPoint(nonnull_bb);
      auto* handle_cstr = builder.CreateCall(
          context.GetLyraStringGetCStr(), {str_handle}, "dpi.str.cstr");
      builder.CreateBr(join_bb);

      builder.SetInsertPoint(join_bb);
      auto* empty_cstr = builder.CreateGlobalStringPtr("", "dpi.str.empty");
      auto* phi = builder.CreatePHI(ptr_ty, 2, "dpi.str.arg");
      phi->addIncoming(empty_cstr, cur_bb);
      phi->addIncoming(handle_cstr, nonnull_bb);

      args.push_back(phi);
    } else {
      args.push_back(
          CoerceToDpiAbiType(context, *val, ref.signature.param_types[i]));
    }
  }

  auto* call_inst = builder.CreateCall(fn, args);
  call_inst->setCallingConv(llvm::CallingConv::C);

  bool is_void = ref.signature.return_kind == mir::DpiReturnKind::kVoid;
  if (is_void || !call.ret) {
    return {};
  }

  // Marshal return value back to internal representation.
  // String returns are owned copies (const char* -> new LyraStringHandle) via
  // LyraStringFromCStr. Scalar returns use bitwise coercion.
  auto tmp_ptr = context.GetPlacePointer(call.ret->tmp);
  if (!tmp_ptr) return std::unexpected(tmp_ptr.error());
  auto tmp_type = context.GetPlaceLlvmType(call.ret->tmp);
  if (!tmp_type) return std::unexpected(tmp_type.error());

  if (ref.signature.return_type == DpiAbiTypeClass::kString) {
    llvm::Value* handle = builder.CreateCall(
        context.GetLyraStringFromCStr(), {call_inst}, "dpi.str.ret");
    builder.CreateStore(handle, *tmp_ptr);
  } else {
    llvm::Value* internal_result = CoerceFromDpiAbiType(
        context, call_inst, ref.signature.return_type, *tmp_type);
    builder.CreateStore(internal_result, *tmp_ptr);
  }

  if (call.ret->dest.has_value()) {
    llvm::Value* ret_val = builder.CreateLoad(*tmp_type, *tmp_ptr);
    return CommitValue(
        context, *call.ret->dest, ret_val, call.ret->type,
        OwnershipPolicy::kMove);
  }

  return {};
}

}  // namespace lyra::lowering::mir_to_llvm::dpi
