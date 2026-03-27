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
#include "lyra/mir/dpi_verify.hpp"

namespace lyra::lowering::mir_to_llvm::dpi {

namespace {

// Coerce a Lyra-internal SSA value to the DPI C ABI type.
// Handles integer widening/narrowing (e.g., i1 -> i8 for bit, packed i7 ->
// i32), float conversion, and pointer identity (chandle, string handles).
auto CoerceToDpiAbiType(
    Context& context, llvm::Value* value, DpiAbiTypeClass abi_type)
    -> llvm::Value* {
  auto& builder = context.GetBuilder();
  llvm::Type* target = GetLlvmDpiType(context.GetLlvmContext(), abi_type);

  if (value->getType() == target) {
    return value;
  }

  if (value->getType()->isIntegerTy() && target->isIntegerTy()) {
    unsigned src_bits = value->getType()->getIntegerBitWidth();
    unsigned dst_bits = target->getIntegerBitWidth();
    if (src_bits < dst_bits) {
      return builder.CreateZExt(value, target);
    }
    return builder.CreateTrunc(value, target);
  }

  if (value->getType()->isFloatTy() && target->isDoubleTy()) {
    return builder.CreateFPExt(value, target);
  }
  if (value->getType()->isDoubleTy() && target->isFloatTy()) {
    return builder.CreateFPTrunc(value, target);
  }

  if (value->getType()->isPointerTy() && target->isPointerTy()) {
    return value;
  }

  throw common::InternalError(
      "CoerceToDpiAbiType",
      "cannot coerce between incompatible LLVM types at DPI boundary");
}

// Coerce a DPI C ABI value back to the Lyra-internal type.
// Used for both return values and output/inout writeback.
// Validates that the value matches the expected ABI carrier type before
// coercing to internal representation.
auto CoerceFromDpiAbiType(
    Context& context, llvm::Value* value, DpiAbiTypeClass abi_type,
    llvm::Type* internal_type) -> llvm::Value* {
  llvm::Type* expected_abi = GetLlvmDpiType(context.GetLlvmContext(), abi_type);

  // Structural ABI check: value must match the expected carrier shape.
  bool shape_ok = false;
  if (expected_abi->isIntegerTy() && value->getType()->isIntegerTy()) {
    shape_ok = value->getType()->getIntegerBitWidth() ==
               expected_abi->getIntegerBitWidth();
  } else if (expected_abi->isFloatTy()) {
    shape_ok = value->getType()->isFloatTy();
  } else if (expected_abi->isDoubleTy()) {
    shape_ok = value->getType()->isDoubleTy();
  } else if (expected_abi->isPointerTy()) {
    shape_ok = value->getType()->isPointerTy();
  }
  if (!shape_ok) {
    throw common::InternalError(
        "CoerceFromDpiAbiType",
        std::format(
            "DPI value does not match expected ABI carrier for type {}",
            static_cast<int>(abi_type)));
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

  if (value->getType()->isPointerTy() && internal_type->isPointerTy()) {
    return value;
  }

  throw common::InternalError(
      "CoerceFromDpiAbiType",
      std::format(
          "cannot coerce DPI ABI type {} to internal type",
          static_cast<int>(abi_type)));
}

// Marshal an input value for a DPI argument. Handles string null-check
// normalization and scalar/pointer coercion.
auto MarshalInputValue(
    Context& context, const mir::Operand& operand,
    const mir::DpiParamDesc& param) -> Result<llvm::Value*> {
  auto val = LowerOperand(context, operand);
  if (!val) return std::unexpected(val.error());

  auto& builder = context.GetBuilder();

  if (param.abi_type == DpiAbiTypeClass::kString) {
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

    return phi;
  }

  return CoerceToDpiAbiType(context, *val, param.abi_type);
}

// Create an entry-block alloca for DPI staged temps.
// Inserts before the first non-alloca instruction in the entry block,
// keeping all entry allocas grouped together.
auto CreateDpiStagedAlloca(
    llvm::IRBuilder<>& builder, llvm::Type* ty, const char* name)
    -> llvm::AllocaInst* {
  auto* func = builder.GetInsertBlock()->getParent();
  auto& entry_bb = func->getEntryBlock();
  auto insert_pt = entry_bb.begin();
  while (insert_pt != entry_bb.end() &&
         llvm::isa<llvm::AllocaInst>(&*insert_pt)) {
    ++insert_pt;
  }
  llvm::IRBuilder<> entry_builder(&entry_bb, insert_pt);
  return entry_builder.CreateAlloca(ty, nullptr, name);
}

// Materialize a Lyra string handle from a C string pointer returned by
// foreign code. Null-safe: normalizes null C pointer to the canonical empty
// string handle via LyraStringFromCStr(""), matching input-side null
// normalization symmetry.
auto MaterializeStringWriteback(Context& context, llvm::Value* c_str_ptr)
    -> llvm::Value* {
  auto& builder = context.GetBuilder();
  auto* ptr_ty = llvm::PointerType::getUnqual(context.GetLlvmContext());
  auto* is_null = builder.CreateICmpEQ(
      c_str_ptr, llvm::ConstantPointerNull::get(ptr_ty), "dpi.str.wb.null");

  auto* cur_bb = builder.GetInsertBlock();
  auto* parent_fn = cur_bb->getParent();
  auto* null_bb = llvm::BasicBlock::Create(
      context.GetLlvmContext(), "dpi.str.wb.null_br", parent_fn);
  auto* nonnull_bb = llvm::BasicBlock::Create(
      context.GetLlvmContext(), "dpi.str.wb.nonnull", parent_fn);
  auto* join_bb = llvm::BasicBlock::Create(
      context.GetLlvmContext(), "dpi.str.wb.join", parent_fn);

  builder.CreateCondBr(is_null, null_bb, nonnull_bb);

  // Null branch: normalize to empty string handle.
  builder.SetInsertPoint(null_bb);
  auto* empty_cstr = builder.CreateGlobalStringPtr("", "dpi.str.wb.empty");
  auto* empty_handle = builder.CreateCall(
      context.GetLyraStringFromCStr(), {empty_cstr}, "dpi.str.wb.empty_h");
  builder.CreateBr(join_bb);

  // Non-null branch: create owned handle from C string.
  builder.SetInsertPoint(nonnull_bb);
  auto* handle = builder.CreateCall(
      context.GetLyraStringFromCStr(), {c_str_ptr}, "dpi.str.wb.handle");
  builder.CreateBr(join_bb);

  builder.SetInsertPoint(join_bb);
  auto* phi = builder.CreatePHI(ptr_ty, 2, "dpi.str.wb.result");
  phi->addIncoming(empty_handle, null_bb);
  phi->addIncoming(handle, nonnull_bb);

  return phi;
}

// Per-argument prepared state for the 3-phase lowering.
struct PreparedDpiArg {
  llvm::AllocaInst* staged_tmp = nullptr;
};

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
    case DpiAbiTypeClass::kChandle:
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
  mir::ValidateDpiSignatureContract(sig, "GetOrDeclareDpiImport");

  auto& llvm_ctx = context.GetLlvmContext();
  auto& module = context.GetModule();

  std::vector<llvm::Type*> param_types;
  param_types.reserve(sig.params.size());
  for (const auto& p : sig.params) {
    llvm::Type* base = GetLlvmDpiType(llvm_ctx, p.abi_type);
    if (p.passing == mir::DpiPassingMode::kByPointer) {
      param_types.push_back(llvm::PointerType::getUnqual(llvm_ctx));
    } else {
      param_types.push_back(base);
    }
  }

  llvm::Type* ret_type = GetLlvmDpiType(llvm_ctx, sig.result.abi_type);
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

auto LowerDpiImportCall(Context& context, const mir::DpiCall& call)
    -> Result<void> {
  const auto& ref = call.callee;
  const auto& sig = ref.signature;
  mir::ValidateDpiSignatureContract(sig, "LowerDpiImportCall");
  mir::ValidateDpiCallContract(sig, call.args, "LowerDpiImportCall");

  llvm::Function* fn = GetOrDeclareDpiImport(context, ref.c_name, sig);
  auto& builder = context.GetBuilder();

  // -- Phase A: prepare ABI arguments --
  std::vector<llvm::Value*> abi_args;
  std::vector<PreparedDpiArg> prepared;
  abi_args.reserve(call.args.size());
  prepared.resize(call.args.size());

  for (size_t i = 0; i < call.args.size(); ++i) {
    const auto& binding = call.args[i];
    const auto& param = sig.params[i];

    if (param.passing == mir::DpiPassingMode::kByValue) {
      auto marshaled = MarshalInputValue(context, *binding.input_value, param);
      if (!marshaled) return std::unexpected(marshaled.error());
      abi_args.push_back(*marshaled);
    } else {
      // Output or inout: allocate C-ABI staged temp and pass pointer.
      llvm::Type* abi_ty =
          GetLlvmDpiType(context.GetLlvmContext(), param.abi_type);
      auto* alloca = CreateDpiStagedAlloca(builder, abi_ty, "dpi.staged");
      prepared[i].staged_tmp = alloca;

      // Inout: copy-in the current value to the staged temp.
      if (binding.input_value) {
        auto marshaled =
            MarshalInputValue(context, *binding.input_value, param);
        if (!marshaled) return std::unexpected(marshaled.error());
        builder.CreateStore(*marshaled, alloca);
      }

      abi_args.push_back(alloca);
    }
  }

  // -- Phase B: emit foreign call --
  auto* call_inst = builder.CreateCall(fn, abi_args);
  call_inst->setCallingConv(llvm::CallingConv::C);

  // -- Phase C: post-call commit --

  // C.1: writeback output/inout params from staged temps.
  for (size_t i = 0; i < call.args.size(); ++i) {
    if (prepared[i].staged_tmp == nullptr) {
      continue;
    }
    const auto& binding = call.args[i];
    const auto& param = sig.params[i];

    llvm::Type* abi_ty =
        GetLlvmDpiType(context.GetLlvmContext(), param.abi_type);
    llvm::Value* raw = builder.CreateLoad(abi_ty, prepared[i].staged_tmp);

    // Convert from C ABI representation to internal Lyra representation.
    if (param.abi_type == DpiAbiTypeClass::kString) {
      llvm::Value* handle = MaterializeStringWriteback(context, raw);
      auto result = CommitValue(
          context, *binding.writeback_dest, handle, param.sv_type,
          OwnershipPolicy::kMove);
      if (!result) return std::unexpected(result.error());
    } else {
      auto dest_type = context.GetPlaceLlvmType(*binding.writeback_dest);
      if (!dest_type) return std::unexpected(dest_type.error());
      llvm::Value* internal_val =
          CoerceFromDpiAbiType(context, raw, param.abi_type, *dest_type);
      auto result = CommitValue(
          context, *binding.writeback_dest, internal_val, param.sv_type,
          OwnershipPolicy::kMove);
      if (!result) return std::unexpected(result.error());
    }
  }

  // C.2: handle return value.
  bool is_void = sig.result.kind == mir::DpiReturnKind::kVoid;
  if (is_void || !call.ret) {
    return {};
  }

  auto tmp_ptr = context.GetPlacePointer(call.ret->tmp);
  if (!tmp_ptr) return std::unexpected(tmp_ptr.error());
  auto tmp_type = context.GetPlaceLlvmType(call.ret->tmp);
  if (!tmp_type) return std::unexpected(tmp_type.error());

  if (sig.result.abi_type == DpiAbiTypeClass::kString) {
    llvm::Value* handle = MaterializeStringWriteback(context, call_inst);
    builder.CreateStore(handle, *tmp_ptr);
  } else {
    llvm::Value* internal_result = CoerceFromDpiAbiType(
        context, call_inst, sig.result.abi_type, *tmp_type);
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
