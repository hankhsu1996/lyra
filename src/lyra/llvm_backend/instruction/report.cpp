#include "lyra/llvm_backend/instruction/report.hpp"

#include <cstdint>

#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/Type.h>

#include "lyra/common/diagnostic/diagnostic.hpp"
#include "lyra/llvm_backend/context.hpp"
#include "lyra/llvm_backend/cu_facts.hpp"
#include "lyra/llvm_backend/format_lowering.hpp"
#include "lyra/llvm_backend/slot_access.hpp"
#include "lyra/mir/effect.hpp"
#include "lyra/runtime/reporting.hpp"

namespace lyra::lowering::mir_to_llvm {

namespace {

auto MapReportIntent(mir::ReportIntent intent) -> uint8_t {
  switch (intent) {
    case mir::ReportIntent::kUserSeverity:
      return static_cast<uint8_t>(runtime::ReportKind::kUserSeverity);
    case mir::ReportIntent::kAssertionFailure:
      return static_cast<uint8_t>(runtime::ReportKind::kAssertionFailure);
  }
  return static_cast<uint8_t>(runtime::ReportKind::kUserSeverity);
}

auto MapReportContinuation(mir::ReportContinuation c) -> uint8_t {
  switch (c) {
    case mir::ReportContinuation::kContinue:
      return static_cast<uint8_t>(runtime::ReportAction::kContinue);
    case mir::ReportContinuation::kFinish:
      return static_cast<uint8_t>(runtime::ReportAction::kFinish);
  }
  return static_cast<uint8_t>(runtime::ReportAction::kContinue);
}

}  // namespace

auto LowerReportEffect(
    Context& context, const CuFacts& facts, SlotAccessResolver& resolver,
    const mir::ReportEffect& report) -> Result<void> {
  auto& builder = context.GetBuilder();
  auto* ptr_ty = llvm::PointerType::getUnqual(context.GetLlvmContext());

  llvm::Value* msg_handle = nullptr;
  if (report.ops.empty()) {
    msg_handle = llvm::ConstantPointerNull::get(ptr_ty);
  } else {
    auto validate_result = ValidateFormatOps(context, facts, report.ops);
    if (!validate_result) return validate_result;

    auto* buf = builder.CreateCall(
        context.GetLyraStringFormatStart(), {}, "report.buf");
    for (const auto& op : report.ops) {
      auto result = LowerFormatOpToBuffer(context, facts, resolver, buf, op);
      if (!result) return result;
    }
    msg_handle = builder.CreateCall(
        context.GetLyraStringFormatFinish(), {buf}, "report.msg");
  }

  auto origin = LowerOptionalReportOrigin(report.origin, context, facts);

  EmitAbiReportCall(
      context, MapReportIntent(report.intent),
      static_cast<uint8_t>(report.severity), origin, msg_handle,
      MapReportContinuation(report.continuation));

  if (!report.ops.empty()) {
    builder.CreateCall(context.GetLyraStringRelease(), {msg_handle});
  }

  return {};
}

auto LowerReportEffect(
    Context& context, const CuFacts& facts, const mir::ReportEffect& report)
    -> Result<void> {
  CanonicalSlotAccess canonical(context, facts);
  return LowerReportEffect(context, facts, canonical, report);
}

}  // namespace lyra::lowering::mir_to_llvm
