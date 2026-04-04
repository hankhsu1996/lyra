#pragma once

#include <optional>

#include <llvm/IR/Constants.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/Type.h>
#include <llvm/IR/Value.h>

#include "lyra/common/diagnostic/diagnostic.hpp"
#include "lyra/common/origin_id.hpp"
#include "lyra/llvm_backend/context.hpp"
#include "lyra/llvm_backend/process_meta_utils.hpp"
#include "lyra/mir/effect.hpp"

namespace lyra::lowering::mir_to_llvm {

// Resolved origin as LLVM constants for the ABI payload.
struct LoweredOrigin {
  llvm::Value* file_ptr;
  llvm::Value* line_val;
  llvm::Value* col_val;
};

// Canonical origin-resolution helper for all report producers.
// Resolves OriginId to {file, line, col} LLVM constants.
// Returns null/zero values if origin is absent or unresolvable.
inline auto LowerOptionalReportOrigin(
    std::optional<common::OriginId> origin, Context& ctx) -> LoweredOrigin {
  auto& builder = ctx.GetBuilder();
  auto& llvm_ctx = ctx.GetLlvmContext();
  auto* i32_ty = llvm::Type::getInt32Ty(llvm_ctx);
  auto* ptr_ty = llvm::PointerType::getUnqual(llvm_ctx);

  if (origin.has_value()) {
    // Use the explicit OriginId if valid, otherwise fall back to the
    // codegen Context's current origin (set by OriginScope from the
    // enclosing MIR Statement).
    auto resolve_id = origin->IsValid() ? *origin : ctx.GetCurrentOrigin();
    auto loc = ResolveProcessOrigin(
        resolve_id, &ctx.GetDiagnosticContext(), ctx.GetSourceManager());
    if (loc.line > 0 && !loc.file.empty()) {
      return {
          .file_ptr = builder.CreateGlobalStringPtr(loc.file),
          .line_val = llvm::ConstantInt::get(i32_ty, loc.line),
          .col_val = llvm::ConstantInt::get(i32_ty, loc.col),
      };
    }
  }
  return {
      .file_ptr = llvm::ConstantPointerNull::get(ptr_ty),
      .line_val = llvm::ConstantInt::get(i32_ty, 0),
      .col_val = llvm::ConstantInt::get(i32_ty, 0),
  };
}

// Build an ABI report payload on the stack and call LyraEmitReport.
// Used by all semantic report producers (severity, assertion, fatal).
inline void EmitAbiReportCall(
    Context& context, uint8_t kind, uint8_t severity,
    const LoweredOrigin& origin, llvm::Value* msg_handle, uint8_t action) {
  auto& builder = context.GetBuilder();
  auto& llvm_ctx = context.GetLlvmContext();
  auto* i8_ty = llvm::Type::getInt8Ty(llvm_ctx);
  auto* i32_ty = llvm::Type::getInt32Ty(llvm_ctx);
  auto* ptr_ty = llvm::PointerType::getUnqual(llvm_ctx);

  auto* payload_ty = llvm::StructType::get(
      llvm_ctx, {i8_ty, i8_ty, i8_ty, i8_ty, ptr_ty, i32_ty, i32_ty, ptr_ty});
  auto* payload = builder.CreateAlloca(payload_ty, nullptr, "report.payload");

  auto store_field = [&](unsigned idx, llvm::Value* val) {
    builder.CreateStore(val, builder.CreateStructGEP(payload_ty, payload, idx));
  };

  store_field(0, llvm::ConstantInt::get(i8_ty, kind));
  store_field(1, llvm::ConstantInt::get(i8_ty, severity));
  store_field(2, llvm::ConstantInt::get(i8_ty, action));
  store_field(3, llvm::ConstantInt::get(i8_ty, 0));
  store_field(4, origin.file_ptr);
  store_field(5, origin.line_val);
  store_field(6, origin.col_val);
  store_field(7, msg_handle);

  builder.CreateCall(
      context.GetLyraEmitReport(), {context.GetEnginePointer(), payload});
}

class SlotAccessResolver;

auto LowerReportEffect(Context& context, const mir::ReportEffect& report)
    -> Result<void>;

auto LowerReportEffect(
    Context& context, SlotAccessResolver& resolver,
    const mir::ReportEffect& report) -> Result<void>;

}  // namespace lyra::lowering::mir_to_llvm
