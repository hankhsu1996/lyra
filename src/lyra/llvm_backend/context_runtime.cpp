#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/Type.h>

#include "lyra/llvm_backend/context.hpp"

namespace lyra::lowering::mir_to_llvm {

auto Context::GetLyraPrintLiteral() -> llvm::Function* {
  if (lyra_print_literal_ == nullptr) {
    // void LyraPrintLiteral(void* engine, const char* str)
    auto* ptr_ty = llvm::PointerType::getUnqual(*llvm_context_);
    auto* fn_type = llvm::FunctionType::get(
        llvm::Type::getVoidTy(*llvm_context_), {ptr_ty, ptr_ty}, false);
    lyra_print_literal_ = llvm::Function::Create(
        fn_type, llvm::Function::ExternalLinkage, "LyraPrintLiteral",
        llvm_module_.get());
  }
  return lyra_print_literal_;
}

auto Context::GetLyraWarnRateLimited() -> llvm::Function* {
  if (lyra_warn_rate_limited_ == nullptr) {
    // void LyraWarnRateLimited(void* engine, const char* msg, uint32_t* ctr)
    auto* ptr_ty = llvm::PointerType::getUnqual(*llvm_context_);
    auto* fn_type = llvm::FunctionType::get(
        llvm::Type::getVoidTy(*llvm_context_), {ptr_ty, ptr_ty, ptr_ty}, false);
    lyra_warn_rate_limited_ = llvm::Function::Create(
        fn_type, llvm::Function::ExternalLinkage, "LyraWarnRateLimited",
        llvm_module_.get());
  }
  return lyra_warn_rate_limited_;
}

auto Context::GetLyraEmitReport() -> llvm::Function* {
  if (lyra_emit_report_ == nullptr) {
    // void LyraEmitReport(void* engine, const AbiReportPayload* payload)
    auto* ptr_ty = llvm::PointerType::getUnqual(*llvm_context_);
    auto* fn_type = llvm::FunctionType::get(
        llvm::Type::getVoidTy(*llvm_context_), {ptr_ty, ptr_ty}, false);
    lyra_emit_report_ = llvm::Function::Create(
        fn_type, llvm::Function::ExternalLinkage, "LyraEmitReport",
        llvm_module_.get());
  }
  return lyra_emit_report_;
}

auto Context::GetLyraRecordDecisionObservation() -> llvm::Function* {
  if (lyra_record_decision_observation_ == nullptr) {
    // void LyraRecordDecisionObservation(
    //     void* engine, uint32_t process_id, uint32_t decision_id,
    //     uint8_t match_class, uint8_t selected_kind, uint16_t selected_arm)
    auto* void_ty = llvm::Type::getVoidTy(*llvm_context_);
    auto* ptr_ty = llvm::PointerType::getUnqual(*llvm_context_);
    auto* i32_ty = llvm::Type::getInt32Ty(*llvm_context_);
    auto* i8_ty = llvm::Type::getInt8Ty(*llvm_context_);
    auto* i16_ty = llvm::Type::getInt16Ty(*llvm_context_);
    auto* fn_type = llvm::FunctionType::get(
        void_ty, {ptr_ty, i32_ty, i32_ty, i8_ty, i8_ty, i16_ty}, false);
    lyra_record_decision_observation_ = llvm::Function::Create(
        fn_type, llvm::Function::ExternalLinkage,
        "LyraRecordDecisionObservation", llvm_module_.get());
  }
  return lyra_record_decision_observation_;
}

auto Context::GetLyraRecordImmediateCoverHit() -> llvm::Function* {
  if (lyra_record_immediate_cover_hit_ == nullptr) {
    // void LyraRecordImmediateCoverHit(void* engine, uint32_t site_index)
    auto* void_ty = llvm::Type::getVoidTy(*llvm_context_);
    auto* ptr_ty = llvm::PointerType::getUnqual(*llvm_context_);
    auto* i32_ty = llvm::Type::getInt32Ty(*llvm_context_);
    auto* fn_type = llvm::FunctionType::get(void_ty, {ptr_ty, i32_ty}, false);
    lyra_record_immediate_cover_hit_ = llvm::Function::Create(
        fn_type, llvm::Function::ExternalLinkage, "LyraRecordImmediateCoverHit",
        llvm_module_.get());
  }
  return lyra_record_immediate_cover_hit_;
}

auto Context::GetLyraEnqueueObservedDeferredAssertion() -> llvm::Function* {
  if (lyra_enqueue_observed_deferred_assertion_ == nullptr) {
    // void LyraEnqueueObservedDeferredAssertion(
    //     void* engine, uint32_t process_id, void* instance,
    //     uint32_t site_id, uint8_t disposition,
    //     const void* payload_ptr, uint32_t payload_size,
    //     const DeferredAssertionRefBindingAbi* ref_ptr, uint32_t ref_count)
    auto* void_ty = llvm::Type::getVoidTy(*llvm_context_);
    auto* ptr_ty = llvm::PointerType::getUnqual(*llvm_context_);
    auto* i32_ty = llvm::Type::getInt32Ty(*llvm_context_);
    auto* i8_ty = llvm::Type::getInt8Ty(*llvm_context_);
    auto* fn_type = llvm::FunctionType::get(
        void_ty,
        {ptr_ty, i32_ty, ptr_ty, i32_ty, i8_ty, ptr_ty, i32_ty, ptr_ty, i32_ty},
        false);
    lyra_enqueue_observed_deferred_assertion_ = llvm::Function::Create(
        fn_type, llvm::Function::ExternalLinkage,
        "LyraEnqueueObservedDeferredAssertion", llvm_module_.get());
  }
  return lyra_enqueue_observed_deferred_assertion_;
}

auto Context::GetLyraPrintValue() -> llvm::Function* {
  if (lyra_print_value_ == nullptr) {
    // void LyraPrintValue(void* engine, i32 format, i32 value_kind,
    //                     ptr data, i32 width, i1 is_signed,
    //                     i32 output_width, i32 precision,
    //                     i1 zero_pad, i1 left_align,
    //                     ptr unknown_data, i8 module_timeunit_power)
    auto* i32_ty = llvm::Type::getInt32Ty(*llvm_context_);
    auto* ptr_ty = llvm::PointerType::getUnqual(*llvm_context_);
    auto* i1_ty = llvm::Type::getInt1Ty(*llvm_context_);
    auto* i8_ty = llvm::Type::getInt8Ty(*llvm_context_);
    auto* fn_type = llvm::FunctionType::get(
        llvm::Type::getVoidTy(*llvm_context_),
        {ptr_ty, i32_ty, i32_ty, ptr_ty, i32_ty, i1_ty, i32_ty, i32_ty, i1_ty,
         i1_ty, ptr_ty, i8_ty},
        false);
    lyra_print_value_ = llvm::Function::Create(
        fn_type, llvm::Function::ExternalLinkage, "LyraPrintValue",
        llvm_module_.get());
  }
  return lyra_print_value_;
}

auto Context::GetFormatSpecType() -> llvm::StructType* {
  if (format_spec_type_ == nullptr) {
    // LyraFormatSpec: { i32 kind, i32 width, i32 precision, i8 flags, [3 x i8]
    // }
    auto* i32_ty = llvm::Type::getInt32Ty(*llvm_context_);
    auto* i8_ty = llvm::Type::getInt8Ty(*llvm_context_);
    auto* reserved_ty = llvm::ArrayType::get(i8_ty, 3);
    format_spec_type_ = llvm::StructType::get(
        *llvm_context_, {i32_ty, i32_ty, i32_ty, i8_ty, reserved_ty});
  }
  return format_spec_type_;
}

auto Context::GetLyraPrintString() -> llvm::Function* {
  if (lyra_print_string_ == nullptr) {
    // void LyraPrintString(void* engine, void* handle, const LyraFormatSpec*)
    auto* ptr_ty = llvm::PointerType::getUnqual(*llvm_context_);
    auto* fn_type = llvm::FunctionType::get(
        llvm::Type::getVoidTy(*llvm_context_), {ptr_ty, ptr_ty, ptr_ty}, false);
    lyra_print_string_ = llvm::Function::Create(
        fn_type, llvm::Function::ExternalLinkage, "LyraPrintString",
        llvm_module_.get());
  }
  return lyra_print_string_;
}

auto Context::GetLyraPrintEnd() -> llvm::Function* {
  if (lyra_print_end_ == nullptr) {
    // void LyraPrintEnd(void* engine, int32_t kind)
    auto* ptr_ty = llvm::PointerType::getUnqual(*llvm_context_);
    auto* i32_ty = llvm::Type::getInt32Ty(*llvm_context_);
    auto* fn_type = llvm::FunctionType::get(
        llvm::Type::getVoidTy(*llvm_context_), {ptr_ty, i32_ty}, false);
    lyra_print_end_ = llvm::Function::Create(
        fn_type, llvm::Function::ExternalLinkage, "LyraPrintEnd",
        llvm_module_.get());
  }
  return lyra_print_end_;
}

auto Context::GetLyraCreateVarSnapshotBuffer() -> llvm::Function* {
  if (lyra_create_var_snapshot_buffer_ == nullptr) {
    // ptr LyraCreateVarSnapshotBuffer()
    auto* ptr_ty = llvm::PointerType::getUnqual(*llvm_context_);
    auto* fn_type = llvm::FunctionType::get(ptr_ty, false);
    lyra_create_var_snapshot_buffer_ = llvm::Function::Create(
        fn_type, llvm::Function::ExternalLinkage, "LyraCreateVarSnapshotBuffer",
        llvm_module_.get());
  }
  return lyra_create_var_snapshot_buffer_;
}

auto Context::GetLyraRegisterVar() -> llvm::Function* {
  if (lyra_register_var_ == nullptr) {
    // void LyraRegisterVar(buf, name, addr, kind, width, is_signed, is_4s)
    auto* ptr_ty = llvm::PointerType::getUnqual(*llvm_context_);
    auto* i32_ty = llvm::Type::getInt32Ty(*llvm_context_);
    auto* i1_ty = llvm::Type::getInt1Ty(*llvm_context_);
    auto* fn_type = llvm::FunctionType::get(
        llvm::Type::getVoidTy(*llvm_context_),
        {ptr_ty, ptr_ty, ptr_ty, i32_ty, i32_ty, i1_ty, i1_ty}, false);
    lyra_register_var_ = llvm::Function::Create(
        fn_type, llvm::Function::ExternalLinkage, "LyraRegisterVar",
        llvm_module_.get());
  }
  return lyra_register_var_;
}

auto Context::GetLyraSnapshotVars() -> llvm::Function* {
  if (lyra_snapshot_vars_ == nullptr) {
    // void LyraSnapshotVars(buf, run_session_ptr)
    auto* ptr_ty = llvm::PointerType::getUnqual(*llvm_context_);
    auto* fn_type = llvm::FunctionType::get(
        llvm::Type::getVoidTy(*llvm_context_), {ptr_ty, ptr_ty}, false);
    lyra_snapshot_vars_ = llvm::Function::Create(
        fn_type, llvm::Function::ExternalLinkage, "LyraSnapshotVars",
        llvm_module_.get());
  }
  return lyra_snapshot_vars_;
}

auto Context::GetLyraStringFromLiteral() -> llvm::Function* {
  if (lyra_string_from_literal_ == nullptr) {
    // ptr LyraStringFromLiteral(const char* data, int64_t len)
    auto* ptr_ty = llvm::PointerType::getUnqual(*llvm_context_);
    auto* i64_ty = llvm::Type::getInt64Ty(*llvm_context_);
    auto* fn_type = llvm::FunctionType::get(ptr_ty, {ptr_ty, i64_ty}, false);
    lyra_string_from_literal_ = llvm::Function::Create(
        fn_type, llvm::Function::ExternalLinkage, "LyraStringFromLiteral",
        llvm_module_.get());
  }
  return lyra_string_from_literal_;
}

auto Context::GetLyraStringFromCStr() -> llvm::Function* {
  if (lyra_string_from_cstr_ == nullptr) {
    // ptr LyraStringFromCStr(const char* ptr)
    auto* ptr_ty = llvm::PointerType::getUnqual(*llvm_context_);
    auto* fn_type = llvm::FunctionType::get(ptr_ty, {ptr_ty}, false);
    lyra_string_from_cstr_ = llvm::Function::Create(
        fn_type, llvm::Function::ExternalLinkage, "LyraStringFromCStr",
        llvm_module_.get());
  }
  return lyra_string_from_cstr_;
}

auto Context::GetLyraStringCmp() -> llvm::Function* {
  if (lyra_string_cmp_ == nullptr) {
    // int32_t LyraStringCmp(ptr a, ptr b)
    auto* ptr_ty = llvm::PointerType::getUnqual(*llvm_context_);
    auto* i32_ty = llvm::Type::getInt32Ty(*llvm_context_);
    auto* fn_type = llvm::FunctionType::get(i32_ty, {ptr_ty, ptr_ty}, false);
    lyra_string_cmp_ = llvm::Function::Create(
        fn_type, llvm::Function::ExternalLinkage, "LyraStringCmp",
        llvm_module_.get());
  }
  return lyra_string_cmp_;
}

auto Context::GetLyraStringRetain() -> llvm::Function* {
  if (lyra_string_retain_ == nullptr) {
    // ptr LyraStringRetain(ptr handle)
    auto* ptr_ty = llvm::PointerType::getUnqual(*llvm_context_);
    auto* fn_type = llvm::FunctionType::get(ptr_ty, {ptr_ty}, false);
    lyra_string_retain_ = llvm::Function::Create(
        fn_type, llvm::Function::ExternalLinkage, "LyraStringRetain",
        llvm_module_.get());
  }
  return lyra_string_retain_;
}

auto Context::GetLyraStringRelease() -> llvm::Function* {
  if (lyra_string_release_ == nullptr) {
    // void LyraStringRelease(ptr handle)
    auto* ptr_ty = llvm::PointerType::getUnqual(*llvm_context_);
    auto* fn_type = llvm::FunctionType::get(
        llvm::Type::getVoidTy(*llvm_context_), {ptr_ty}, false);
    lyra_string_release_ = llvm::Function::Create(
        fn_type, llvm::Function::ExternalLinkage, "LyraStringRelease",
        llvm_module_.get());
  }
  return lyra_string_release_;
}

auto Context::GetLyraStringConcat() -> llvm::Function* {
  if (lyra_string_concat_ == nullptr) {
    // ptr LyraStringConcat(ptr elems, i64 count)
    auto* ptr_ty = llvm::PointerType::getUnqual(*llvm_context_);
    auto* i64_ty = llvm::Type::getInt64Ty(*llvm_context_);
    auto* fn_type = llvm::FunctionType::get(ptr_ty, {ptr_ty, i64_ty}, false);
    lyra_string_concat_ = llvm::Function::Create(
        fn_type, llvm::Function::ExternalLinkage, "LyraStringConcat",
        llvm_module_.get());
  }
  return lyra_string_concat_;
}

auto Context::GetLyraStringFromPacked() -> llvm::Function* {
  if (lyra_string_from_packed_ == nullptr) {
    // ptr LyraStringFromPacked(ptr data, i32 bit_width)
    auto* ptr_ty = llvm::PointerType::getUnqual(*llvm_context_);
    auto* i32_ty = llvm::Type::getInt32Ty(*llvm_context_);
    auto* fn_type = llvm::FunctionType::get(ptr_ty, {ptr_ty, i32_ty}, false);
    lyra_string_from_packed_ = llvm::Function::Create(
        fn_type, llvm::Function::ExternalLinkage, "LyraStringFromPacked",
        llvm_module_.get());
  }
  return lyra_string_from_packed_;
}

auto Context::GetLyraStringGetView() -> llvm::Function* {
  if (lyra_string_get_view_ == nullptr) {
    // void LyraStringGetView(ptr handle, ptr out_ptr, ptr out_len)
    auto* void_ty = llvm::Type::getVoidTy(*llvm_context_);
    auto* ptr_ty = llvm::PointerType::getUnqual(*llvm_context_);
    auto* fn_type =
        llvm::FunctionType::get(void_ty, {ptr_ty, ptr_ty, ptr_ty}, false);
    lyra_string_get_view_ = llvm::Function::Create(
        fn_type, llvm::Function::ExternalLinkage, "LyraStringGetView",
        llvm_module_.get());
  }
  return lyra_string_get_view_;
}

auto Context::GetLyraStringGetCStr() -> llvm::Function* {
  if (lyra_string_get_cstr_ == nullptr) {
    // const char* LyraStringGetCStr(ptr handle)
    auto* ptr_ty = llvm::PointerType::getUnqual(*llvm_context_);
    auto* fn_type = llvm::FunctionType::get(ptr_ty, {ptr_ty}, false);
    lyra_string_get_cstr_ = llvm::Function::Create(
        fn_type, llvm::Function::ExternalLinkage, "LyraStringGetCStr",
        llvm_module_.get());
  }
  return lyra_string_get_cstr_;
}

auto Context::GetLyraPackedFromString() -> llvm::Function* {
  if (lyra_packed_from_string_ == nullptr) {
    // void LyraPackedFromString(ptr handle, ptr out_data, i32 bit_width)
    auto* ptr_ty = llvm::PointerType::getUnqual(*llvm_context_);
    auto* i32_ty = llvm::Type::getInt32Ty(*llvm_context_);
    auto* void_ty = llvm::Type::getVoidTy(*llvm_context_);
    auto* fn_type =
        llvm::FunctionType::get(void_ty, {ptr_ty, ptr_ty, i32_ty}, false);
    lyra_packed_from_string_ = llvm::Function::Create(
        fn_type, llvm::Function::ExternalLinkage, "LyraPackedFromString",
        llvm_module_.get());
  }
  return lyra_packed_from_string_;
}

auto Context::GetLyraRunSimulation() -> llvm::Function* {
  if (lyra_run_simulation_ == nullptr) {
    // void LyraRunSimulation(ptr* processes, ptr* states, uint32_t num,
    //                        const char** plusargs, uint32_t num_plusargs,
    //                        const LyraRuntimeAbi* abi,
    //                        void* run_session_ptr)
    auto* ptr_ty = llvm::PointerType::getUnqual(*llvm_context_);
    auto* i32_ty = llvm::Type::getInt32Ty(*llvm_context_);
    auto* fn_type = llvm::FunctionType::get(
        llvm::Type::getVoidTy(*llvm_context_),
        {ptr_ty, ptr_ty, i32_ty, ptr_ty, i32_ty, ptr_ty, ptr_ty}, false);
    lyra_run_simulation_ = llvm::Function::Create(
        fn_type, llvm::Function::ExternalLinkage, "LyraRunSimulation",
        llvm_module_.get());
  }
  return lyra_run_simulation_;
}

auto Context::GetLyraConstructProcessStates() -> llvm::Function* {
  if (lyra_construct_process_states_ == nullptr) {
    // void* LyraConstructProcessStates(
    //     void** states_out, uint32_t num_processes,
    //     const ProcessStateSchema* state_schemas, uint32_t num_state_schemas,
    //     const ProcessConstructorRecord* records)
    auto* ptr_ty = llvm::PointerType::getUnqual(*llvm_context_);
    auto* i32_ty = llvm::Type::getInt32Ty(*llvm_context_);
    auto* fn_type = llvm::FunctionType::get(
        ptr_ty, {ptr_ty, i32_ty, ptr_ty, i32_ty, ptr_ty}, false);
    lyra_construct_process_states_ = llvm::Function::Create(
        fn_type, llvm::Function::ExternalLinkage, "LyraConstructProcessStates",
        llvm_module_.get());
  }
  return lyra_construct_process_states_;
}

auto Context::GetLyraDestroyProcessStates() -> llvm::Function* {
  if (lyra_destroy_process_states_ == nullptr) {
    // void LyraDestroyProcessStates(void* packed_buffer)
    auto* ptr_ty = llvm::PointerType::getUnqual(*llvm_context_);
    auto* void_ty = llvm::Type::getVoidTy(*llvm_context_);
    auto* fn_type = llvm::FunctionType::get(void_ty, {ptr_ty}, false);
    lyra_destroy_process_states_ = llvm::Function::Create(
        fn_type, llvm::Function::ExternalLinkage, "LyraDestroyProcessStates",
        llvm_module_.get());
  }
  return lyra_destroy_process_states_;
}

auto Context::GetLyraRunProcessSync() -> llvm::Function* {
  if (lyra_run_process_sync_ == nullptr) {
    // void LyraRunProcessSync(ptr process, ptr state)
    auto* ptr_ty = llvm::PointerType::getUnqual(*llvm_context_);
    auto* fn_type = llvm::FunctionType::get(
        llvm::Type::getVoidTy(*llvm_context_), {ptr_ty, ptr_ty}, false);
    lyra_run_process_sync_ = llvm::Function::Create(
        fn_type, llvm::Function::ExternalLinkage, "LyraRunProcessSync",
        llvm_module_.get());
  }
  return lyra_run_process_sync_;
}

auto Context::GetLyraIterationLimitPtr() -> llvm::Function* {
  if (lyra_iteration_limit_ptr_ == nullptr) {
    // uint32_t* LyraIterationLimitPtr()
    auto* ptr_ty = llvm::PointerType::getUnqual(*llvm_context_);
    auto* fn_type = llvm::FunctionType::get(ptr_ty, {}, false);
    lyra_iteration_limit_ptr_ = llvm::Function::Create(
        fn_type, llvm::Function::ExternalLinkage, "LyraIterationLimitPtr",
        llvm_module_.get());
  }
  return lyra_iteration_limit_ptr_;
}

auto Context::GetLyraPlusargsTest() -> llvm::Function* {
  if (lyra_plusargs_test_ == nullptr) {
    // int32_t LyraPlusargsTest(ptr engine, ptr query)
    auto* ptr_ty = llvm::PointerType::getUnqual(*llvm_context_);
    auto* i32_ty = llvm::Type::getInt32Ty(*llvm_context_);
    auto* fn_type = llvm::FunctionType::get(i32_ty, {ptr_ty, ptr_ty}, false);
    lyra_plusargs_test_ = llvm::Function::Create(
        fn_type, llvm::Function::ExternalLinkage, "LyraPlusargsTest",
        llvm_module_.get());
  }
  return lyra_plusargs_test_;
}

auto Context::GetLyraPlusargsValueInt() -> llvm::Function* {
  if (lyra_plusargs_value_int_ == nullptr) {
    // int32_t LyraPlusargsValueInt(ptr engine, ptr format, ptr output)
    auto* ptr_ty = llvm::PointerType::getUnqual(*llvm_context_);
    auto* i32_ty = llvm::Type::getInt32Ty(*llvm_context_);
    auto* fn_type =
        llvm::FunctionType::get(i32_ty, {ptr_ty, ptr_ty, ptr_ty}, false);
    lyra_plusargs_value_int_ = llvm::Function::Create(
        fn_type, llvm::Function::ExternalLinkage, "LyraPlusargsValueInt",
        llvm_module_.get());
  }
  return lyra_plusargs_value_int_;
}

auto Context::GetLyraPlusargsValueString() -> llvm::Function* {
  if (lyra_plusargs_value_string_ == nullptr) {
    // int32_t LyraPlusargsValueString(ptr engine, ptr format, ptr output)
    auto* ptr_ty = llvm::PointerType::getUnqual(*llvm_context_);
    auto* i32_ty = llvm::Type::getInt32Ty(*llvm_context_);
    auto* fn_type =
        llvm::FunctionType::get(i32_ty, {ptr_ty, ptr_ty, ptr_ty}, false);
    lyra_plusargs_value_string_ = llvm::Function::Create(
        fn_type, llvm::Function::ExternalLinkage, "LyraPlusargsValueString",
        llvm_module_.get());
  }
  return lyra_plusargs_value_string_;
}

auto Context::GetLyraSuspendDelay() -> llvm::Function* {
  if (lyra_suspend_delay_ == nullptr) {
    // void LyraSuspendDelay(ptr state, i64 ticks, i32 resume_block)
    auto* ptr_ty = llvm::PointerType::getUnqual(*llvm_context_);
    auto* i64_ty = llvm::Type::getInt64Ty(*llvm_context_);
    auto* i32_ty = llvm::Type::getInt32Ty(*llvm_context_);
    auto* fn_type = llvm::FunctionType::get(
        llvm::Type::getVoidTy(*llvm_context_), {ptr_ty, i64_ty, i32_ty}, false);
    lyra_suspend_delay_ = llvm::Function::Create(
        fn_type, llvm::Function::ExternalLinkage, "LyraSuspendDelay",
        llvm_module_.get());
  }
  return lyra_suspend_delay_;
}

auto Context::GetLyraSuspendWait() -> llvm::Function* {
  if (lyra_suspend_wait_ == nullptr) {
    // void LyraSuspendWait(ptr state, i32 resume_block, ptr triggers,
    //                      i32 num_triggers, i32 wait_site_id)
    auto* ptr_ty = llvm::PointerType::getUnqual(*llvm_context_);
    auto* i32_ty = llvm::Type::getInt32Ty(*llvm_context_);
    auto* fn_type = llvm::FunctionType::get(
        llvm::Type::getVoidTy(*llvm_context_),
        {ptr_ty, i32_ty, ptr_ty, i32_ty, i32_ty}, false);
    lyra_suspend_wait_ = llvm::Function::Create(
        fn_type, llvm::Function::ExternalLinkage, "LyraSuspendWait",
        llvm_module_.get());
  }
  return lyra_suspend_wait_;
}

auto Context::GetLyraSuspendWaitStatic() -> llvm::Function* {
  if (lyra_suspend_wait_static_ == nullptr) {
    // void LyraSuspendWaitStatic(ptr state, i32 resume_block,
    //                            i32 wait_site_id)
    auto* ptr_ty = llvm::PointerType::getUnqual(*llvm_context_);
    auto* i32_ty = llvm::Type::getInt32Ty(*llvm_context_);
    auto* fn_type = llvm::FunctionType::get(
        llvm::Type::getVoidTy(*llvm_context_), {ptr_ty, i32_ty, i32_ty}, false);
    lyra_suspend_wait_static_ = llvm::Function::Create(
        fn_type, llvm::Function::ExternalLinkage, "LyraSuspendWaitStatic",
        llvm_module_.get());
  }
  return lyra_suspend_wait_static_;
}

auto Context::GetLyraSuspendWaitWithLateBound() -> llvm::Function* {
  if (lyra_suspend_wait_with_late_bound_ == nullptr) {
    // void LyraSuspendWaitWithLateBound(
    //     ptr state, i32 resume_block,
    //     ptr triggers, i32 num_triggers,
    //     ptr headers, i32 num_headers,
    //     ptr plan_ops, i32 num_plan_ops,
    //     ptr dep_slots, i32 num_dep_slots,
    //     i32 wait_site_id)
    auto* ptr_ty = llvm::PointerType::getUnqual(*llvm_context_);
    auto* i32_ty = llvm::Type::getInt32Ty(*llvm_context_);
    auto* fn_type = llvm::FunctionType::get(
        llvm::Type::getVoidTy(*llvm_context_),
        {ptr_ty, i32_ty, ptr_ty, i32_ty, ptr_ty, i32_ty, ptr_ty, i32_ty, ptr_ty,
         i32_ty, i32_ty},
        false);
    lyra_suspend_wait_with_late_bound_ = llvm::Function::Create(
        fn_type, llvm::Function::ExternalLinkage,
        "LyraSuspendWaitWithLateBound", llvm_module_.get());
  }
  return lyra_suspend_wait_with_late_bound_;
}

auto Context::GetLyraSuspendRepeat() -> llvm::Function* {
  if (lyra_suspend_repeat_ == nullptr) {
    // void LyraSuspendRepeat(ptr state)
    auto* ptr_ty = llvm::PointerType::getUnqual(*llvm_context_);
    auto* fn_type = llvm::FunctionType::get(
        llvm::Type::getVoidTy(*llvm_context_), {ptr_ty}, false);
    lyra_suspend_repeat_ = llvm::Function::Create(
        fn_type, llvm::Function::ExternalLinkage, "LyraSuspendRepeat",
        llvm_module_.get());
  }
  return lyra_suspend_repeat_;
}

auto Context::GetLyraAllocTriggers() -> llvm::Function* {
  if (lyra_alloc_triggers_ == nullptr) {
    // ptr LyraAllocTriggers(i32 count)
    auto* ptr_ty = llvm::PointerType::getUnqual(*llvm_context_);
    auto* i32_ty = llvm::Type::getInt32Ty(*llvm_context_);
    auto* fn_type = llvm::FunctionType::get(ptr_ty, {i32_ty}, false);
    lyra_alloc_triggers_ = llvm::Function::Create(
        fn_type, llvm::Function::ExternalLinkage, "LyraAllocTriggers",
        llvm_module_.get());
  }
  return lyra_alloc_triggers_;
}

auto Context::GetLyraFreeTriggers() -> llvm::Function* {
  if (lyra_free_triggers_ == nullptr) {
    // void LyraFreeTriggers(ptr triggers)
    auto* ptr_ty = llvm::PointerType::getUnqual(*llvm_context_);
    auto* fn_type = llvm::FunctionType::get(
        llvm::Type::getVoidTy(*llvm_context_), {ptr_ty}, false);
    lyra_free_triggers_ = llvm::Function::Create(
        fn_type, llvm::Function::ExternalLinkage, "LyraFreeTriggers",
        llvm_module_.get());
  }
  return lyra_free_triggers_;
}

auto Context::GetLyraSuspendWaitEvent() -> llvm::Function* {
  if (lyra_suspend_wait_event_ == nullptr) {
    auto* ptr_ty = llvm::PointerType::getUnqual(*llvm_context_);
    auto* i32_ty = llvm::Type::getInt32Ty(*llvm_context_);
    auto* fn_type = llvm::FunctionType::get(
        llvm::Type::getVoidTy(*llvm_context_), {ptr_ty, i32_ty, i32_ty}, false);
    lyra_suspend_wait_event_ = llvm::Function::Create(
        fn_type, llvm::Function::ExternalLinkage, "LyraSuspendWaitEvent",
        llvm_module_.get());
  }
  return lyra_suspend_wait_event_;
}

auto Context::GetLyraTriggerEvent() -> llvm::Function* {
  if (lyra_trigger_event_ == nullptr) {
    // void LyraTriggerEvent(ptr engine, ptr instance, i32 event_id)
    auto* ptr_ty = llvm::PointerType::getUnqual(*llvm_context_);
    auto* i32_ty = llvm::Type::getInt32Ty(*llvm_context_);
    auto* fn_type = llvm::FunctionType::get(
        llvm::Type::getVoidTy(*llvm_context_), {ptr_ty, ptr_ty, i32_ty}, false);
    lyra_trigger_event_ = llvm::Function::Create(
        fn_type, llvm::Function::ExternalLinkage, "LyraTriggerEvent",
        llvm_module_.get());
  }
  return lyra_trigger_event_;
}

auto Context::GetLyraResolveGlobalSlotPtr() -> llvm::Function* {
  if (lyra_resolve_global_slot_ptr_ == nullptr) {
    auto* ptr_ty = llvm::PointerType::getUnqual(*llvm_context_);
    auto* i32_ty = llvm::Type::getInt32Ty(*llvm_context_);
    auto* fn_type = llvm::FunctionType::get(ptr_ty, {ptr_ty, i32_ty}, false);
    lyra_resolve_global_slot_ptr_ = llvm::Function::Create(
        fn_type, llvm::Function::ExternalLinkage, "LyraResolveGlobalSlotPtr",
        llvm_module_.get());
  }
  return lyra_resolve_global_slot_ptr_;
}

// R3 typed coordination helpers.
// Local: (ptr engine, ptr instance, ..., i32 local_id, ...)
// Global: (ptr engine, ..., i32 global_id, ...)

auto Context::GetLyraResolveInstancePtr() -> llvm::Function* {
  if (lyra_resolve_instance_ptr_ == nullptr) {
    auto* ptr_ty = llvm::PointerType::getUnqual(*llvm_context_);
    auto* i32_ty = llvm::Type::getInt32Ty(*llvm_context_);
    // (eng, instance_id) -> ptr
    auto* fn_type = llvm::FunctionType::get(ptr_ty, {ptr_ty, i32_ty}, false);
    lyra_resolve_instance_ptr_ = llvm::Function::Create(
        fn_type, llvm::Function::ExternalLinkage, "LyraResolveInstancePtr",
        llvm_module_.get());
  }
  return lyra_resolve_instance_ptr_;
}

auto Context::GetLyraGetInstanceOrdinal() -> llvm::Function* {
  if (lyra_get_instance_ordinal_ == nullptr) {
    auto* ptr_ty = llvm::PointerType::getUnqual(*llvm_context_);
    auto* i32_ty = llvm::Type::getInt32Ty(*llvm_context_);
    // (ptr engine, ptr instance) -> i32 ordinal
    auto* fn_type = llvm::FunctionType::get(i32_ty, {ptr_ty, ptr_ty}, false);
    lyra_get_instance_ordinal_ = llvm::Function::Create(
        fn_type, llvm::Function::ExternalLinkage, "LyraGetInstanceOrdinal",
        llvm_module_.get());
  }
  return lyra_get_instance_ordinal_;
}

auto Context::GetLyraMarkDirtyLocal() -> llvm::Function* {
  if (lyra_mark_dirty_local_ == nullptr) {
    auto* ptr_ty = llvm::PointerType::getUnqual(*llvm_context_);
    auto* i32_ty = llvm::Type::getInt32Ty(*llvm_context_);
    // (eng, inst, local_id, off, size)
    auto* fn_type = llvm::FunctionType::get(
        llvm::Type::getVoidTy(*llvm_context_),
        {ptr_ty, ptr_ty, i32_ty, i32_ty, i32_ty}, false);
    lyra_mark_dirty_local_ = llvm::Function::Create(
        fn_type, llvm::Function::ExternalLinkage, "LyraMarkDirtyLocal",
        llvm_module_.get());
  }
  return lyra_mark_dirty_local_;
}

auto Context::GetLyraMarkDirtyExtRef() -> llvm::Function* {
  if (lyra_mark_dirty_ext_ref_ == nullptr) {
    auto* ptr_ty = llvm::PointerType::getUnqual(*llvm_context_);
    auto* i32_ty = llvm::Type::getInt32Ty(*llvm_context_);
    // (eng, inst, ref_id, off, size)
    auto* fn_type = llvm::FunctionType::get(
        llvm::Type::getVoidTy(*llvm_context_),
        {ptr_ty, ptr_ty, i32_ty, i32_ty, i32_ty}, false);
    lyra_mark_dirty_ext_ref_ = llvm::Function::Create(
        fn_type, llvm::Function::ExternalLinkage, "LyraMarkDirtyExtRef",
        llvm_module_.get());
  }
  return lyra_mark_dirty_ext_ref_;
}

auto Context::GetLyraMarkDirtyGlobal() -> llvm::Function* {
  if (lyra_mark_dirty_global_ == nullptr) {
    auto* ptr_ty = llvm::PointerType::getUnqual(*llvm_context_);
    auto* i32_ty = llvm::Type::getInt32Ty(*llvm_context_);
    auto* fn_type = llvm::FunctionType::get(
        llvm::Type::getVoidTy(*llvm_context_), {ptr_ty, i32_ty, i32_ty, i32_ty},
        false);
    lyra_mark_dirty_global_ = llvm::Function::Create(
        fn_type, llvm::Function::ExternalLinkage, "LyraMarkDirtyGlobal",
        llvm_module_.get());
  }
  return lyra_mark_dirty_global_;
}

auto Context::GetLyraStorePackedLocal() -> llvm::Function* {
  if (lyra_store_packed_local_ == nullptr) {
    auto* ptr_ty = llvm::PointerType::getUnqual(*llvm_context_);
    auto* i32_ty = llvm::Type::getInt32Ty(*llvm_context_);
    // (eng, inst, slot, val, bsz, id, off, dsz)
    auto* fn_type = llvm::FunctionType::get(
        llvm::Type::getVoidTy(*llvm_context_),
        {ptr_ty, ptr_ty, ptr_ty, ptr_ty, i32_ty, i32_ty, i32_ty, i32_ty},
        false);
    lyra_store_packed_local_ = llvm::Function::Create(
        fn_type, llvm::Function::ExternalLinkage, "LyraStorePackedLocal",
        llvm_module_.get());
  }
  return lyra_store_packed_local_;
}

auto Context::GetLyraStorePackedGlobal() -> llvm::Function* {
  if (lyra_store_packed_global_ == nullptr) {
    auto* ptr_ty = llvm::PointerType::getUnqual(*llvm_context_);
    auto* i32_ty = llvm::Type::getInt32Ty(*llvm_context_);
    auto* fn_type = llvm::FunctionType::get(
        llvm::Type::getVoidTy(*llvm_context_),
        {ptr_ty, ptr_ty, ptr_ty, i32_ty, i32_ty, i32_ty, i32_ty}, false);
    lyra_store_packed_global_ = llvm::Function::Create(
        fn_type, llvm::Function::ExternalLinkage, "LyraStorePackedGlobal",
        llvm_module_.get());
  }
  return lyra_store_packed_global_;
}

auto Context::GetLyraStoreStringLocal() -> llvm::Function* {
  if (lyra_store_string_local_ == nullptr) {
    auto* ptr_ty = llvm::PointerType::getUnqual(*llvm_context_);
    auto* i32_ty = llvm::Type::getInt32Ty(*llvm_context_);
    // (eng, inst, slot, str, id)
    auto* fn_type = llvm::FunctionType::get(
        llvm::Type::getVoidTy(*llvm_context_),
        {ptr_ty, ptr_ty, ptr_ty, ptr_ty, i32_ty}, false);
    lyra_store_string_local_ = llvm::Function::Create(
        fn_type, llvm::Function::ExternalLinkage, "LyraStoreStringLocal",
        llvm_module_.get());
  }
  return lyra_store_string_local_;
}

auto Context::GetLyraStoreStringGlobal() -> llvm::Function* {
  if (lyra_store_string_global_ == nullptr) {
    auto* ptr_ty = llvm::PointerType::getUnqual(*llvm_context_);
    auto* i32_ty = llvm::Type::getInt32Ty(*llvm_context_);
    auto* fn_type = llvm::FunctionType::get(
        llvm::Type::getVoidTy(*llvm_context_), {ptr_ty, ptr_ty, ptr_ty, i32_ty},
        false);
    lyra_store_string_global_ = llvm::Function::Create(
        fn_type, llvm::Function::ExternalLinkage, "LyraStoreStringGlobal",
        llvm_module_.get());
  }
  return lyra_store_string_global_;
}

auto Context::GetLyraDeferredWriteLocal() -> llvm::Function* {
  if (lyra_deferred_write_local_ == nullptr) {
    auto* ptr_ty = llvm::PointerType::getUnqual(*llvm_context_);
    auto* i32_ty = llvm::Type::getInt32Ty(*llvm_context_);
    // (eng, inst, vp, bsz, id, body_offset, is_partial)
    auto* fn_type = llvm::FunctionType::get(
        llvm::Type::getVoidTy(*llvm_context_),
        {ptr_ty, ptr_ty, ptr_ty, i32_ty, i32_ty, i32_ty, i32_ty}, false);
    lyra_deferred_write_local_ = llvm::Function::Create(
        fn_type, llvm::Function::ExternalLinkage, "LyraDeferredWriteLocal",
        llvm_module_.get());
  }
  return lyra_deferred_write_local_;
}

auto Context::GetLyraDeferredMaskedWriteLocal() -> llvm::Function* {
  if (lyra_deferred_masked_write_local_ == nullptr) {
    auto* ptr_ty = llvm::PointerType::getUnqual(*llvm_context_);
    auto* i32_ty = llvm::Type::getInt32Ty(*llvm_context_);
    // (eng, inst, vp, mp, bsz, id, body_offset)
    auto* fn_type = llvm::FunctionType::get(
        llvm::Type::getVoidTy(*llvm_context_),
        {ptr_ty, ptr_ty, ptr_ty, ptr_ty, i32_ty, i32_ty, i32_ty}, false);
    lyra_deferred_masked_write_local_ = llvm::Function::Create(
        fn_type, llvm::Function::ExternalLinkage,
        "LyraDeferredMaskedWriteLocal", llvm_module_.get());
  }
  return lyra_deferred_masked_write_local_;
}

auto Context::GetLyraDeferredCanonicalPackedWriteLocal() -> llvm::Function* {
  if (lyra_deferred_canonical_packed_write_local_ == nullptr) {
    auto* ptr_ty = llvm::PointerType::getUnqual(*llvm_context_);
    auto* i32_ty = llvm::Type::getInt32Ty(*llvm_context_);
    // (eng, inst, val, unk, region_bsz, id, body_offset, second_region_offset)
    auto* fn_type = llvm::FunctionType::get(
        llvm::Type::getVoidTy(*llvm_context_),
        {ptr_ty, ptr_ty, ptr_ty, ptr_ty, i32_ty, i32_ty, i32_ty, i32_ty},
        false);
    lyra_deferred_canonical_packed_write_local_ = llvm::Function::Create(
        fn_type, llvm::Function::ExternalLinkage,
        "LyraDeferredCanonicalPackedWriteLocal", llvm_module_.get());
  }
  return lyra_deferred_canonical_packed_write_local_;
}

auto Context::GetLyraScheduleNbaLocal() -> llvm::Function* {
  if (lyra_schedule_nba_local_ == nullptr) {
    auto* ptr_ty = llvm::PointerType::getUnqual(*llvm_context_);
    auto* i32_ty = llvm::Type::getInt32Ty(*llvm_context_);
    // (eng, inst, wp, nb, vp, mp, bsz, id)
    auto* fn_type = llvm::FunctionType::get(
        llvm::Type::getVoidTy(*llvm_context_),
        {ptr_ty, ptr_ty, ptr_ty, ptr_ty, ptr_ty, ptr_ty, i32_ty, i32_ty},
        false);
    lyra_schedule_nba_local_ = llvm::Function::Create(
        fn_type, llvm::Function::ExternalLinkage, "LyraScheduleNbaLocal",
        llvm_module_.get());
  }
  return lyra_schedule_nba_local_;
}

auto Context::GetLyraScheduleNbaGlobal() -> llvm::Function* {
  if (lyra_schedule_nba_global_ == nullptr) {
    auto* ptr_ty = llvm::PointerType::getUnqual(*llvm_context_);
    auto* i32_ty = llvm::Type::getInt32Ty(*llvm_context_);
    auto* fn_type = llvm::FunctionType::get(
        llvm::Type::getVoidTy(*llvm_context_),
        {ptr_ty, ptr_ty, ptr_ty, ptr_ty, ptr_ty, i32_ty, i32_ty}, false);
    lyra_schedule_nba_global_ = llvm::Function::Create(
        fn_type, llvm::Function::ExternalLinkage, "LyraScheduleNbaGlobal",
        llvm_module_.get());
  }
  return lyra_schedule_nba_global_;
}

auto Context::GetLyraScheduleNbaCanonicalPackedLocal() -> llvm::Function* {
  if (lyra_schedule_nba_canonical_packed_local_ == nullptr) {
    auto* ptr_ty = llvm::PointerType::getUnqual(*llvm_context_);
    auto* i32_ty = llvm::Type::getInt32Ty(*llvm_context_);
    // (eng, inst, wp, nb, vp, up, rsz, sro, id)
    auto* fn_type = llvm::FunctionType::get(
        llvm::Type::getVoidTy(*llvm_context_),
        {ptr_ty, ptr_ty, ptr_ty, ptr_ty, ptr_ty, ptr_ty, i32_ty, i32_ty,
         i32_ty},
        false);
    lyra_schedule_nba_canonical_packed_local_ = llvm::Function::Create(
        fn_type, llvm::Function::ExternalLinkage,
        "LyraScheduleNbaCanonicalPackedLocal", llvm_module_.get());
  }
  return lyra_schedule_nba_canonical_packed_local_;
}

auto Context::GetLyraScheduleNbaCanonicalPackedGlobal() -> llvm::Function* {
  if (lyra_schedule_nba_canonical_packed_global_ == nullptr) {
    auto* ptr_ty = llvm::PointerType::getUnqual(*llvm_context_);
    auto* i32_ty = llvm::Type::getInt32Ty(*llvm_context_);
    auto* fn_type = llvm::FunctionType::get(
        llvm::Type::getVoidTy(*llvm_context_),
        {ptr_ty, ptr_ty, ptr_ty, ptr_ty, ptr_ty, i32_ty, i32_ty, i32_ty},
        false);
    lyra_schedule_nba_canonical_packed_global_ = llvm::Function::Create(
        fn_type, llvm::Function::ExternalLinkage,
        "LyraScheduleNbaCanonicalPackedGlobal", llvm_module_.get());
  }
  return lyra_schedule_nba_canonical_packed_global_;
}

auto Context::GetLyraScheduleNbaExtRef() -> llvm::Function* {
  if (lyra_schedule_nba_ext_ref_ == nullptr) {
    auto* ptr_ty = llvm::PointerType::getUnqual(*llvm_context_);
    auto* i32_ty = llvm::Type::getInt32Ty(*llvm_context_);
    // (eng, inst, ref_id, wp, nb, vp, mp, bsz)
    auto* fn_type = llvm::FunctionType::get(
        llvm::Type::getVoidTy(*llvm_context_),
        {ptr_ty, ptr_ty, i32_ty, ptr_ty, ptr_ty, ptr_ty, ptr_ty, i32_ty},
        false);
    lyra_schedule_nba_ext_ref_ = llvm::Function::Create(
        fn_type, llvm::Function::ExternalLinkage, "LyraScheduleNbaExtRef",
        llvm_module_.get());
  }
  return lyra_schedule_nba_ext_ref_;
}

auto Context::GetLyraScheduleNbaCanonicalPackedExtRef() -> llvm::Function* {
  if (lyra_schedule_nba_canonical_packed_ext_ref_ == nullptr) {
    auto* ptr_ty = llvm::PointerType::getUnqual(*llvm_context_);
    auto* i32_ty = llvm::Type::getInt32Ty(*llvm_context_);
    // (eng, inst, ref_id, wp, nb, vp, up, rsz, sro)
    auto* fn_type = llvm::FunctionType::get(
        llvm::Type::getVoidTy(*llvm_context_),
        {ptr_ty, ptr_ty, i32_ty, ptr_ty, ptr_ty, ptr_ty, ptr_ty, i32_ty,
         i32_ty},
        false);
    lyra_schedule_nba_canonical_packed_ext_ref_ = llvm::Function::Create(
        fn_type, llvm::Function::ExternalLinkage,
        "LyraScheduleNbaCanonicalPackedExtRef", llvm_module_.get());
  }
  return lyra_schedule_nba_canonical_packed_ext_ref_;
}

auto Context::GetLyraIsTraceObservedLocal() -> llvm::Function* {
  if (lyra_is_trace_observed_local_ == nullptr) {
    auto* ptr_ty = llvm::PointerType::getUnqual(*llvm_context_);
    auto* i32_ty = llvm::Type::getInt32Ty(*llvm_context_);
    auto* i1_ty = llvm::Type::getInt1Ty(*llvm_context_);
    // (eng, inst, id)
    auto* fn_type =
        llvm::FunctionType::get(i1_ty, {ptr_ty, ptr_ty, i32_ty}, false);
    lyra_is_trace_observed_local_ = llvm::Function::Create(
        fn_type, llvm::Function::ExternalLinkage, "LyraIsTraceObservedLocal",
        llvm_module_.get());
  }
  return lyra_is_trace_observed_local_;
}

auto Context::GetLyraIsTraceObservedGlobal() -> llvm::Function* {
  if (lyra_is_trace_observed_global_ == nullptr) {
    auto* ptr_ty = llvm::PointerType::getUnqual(*llvm_context_);
    auto* i32_ty = llvm::Type::getInt32Ty(*llvm_context_);
    auto* i1_ty = llvm::Type::getInt1Ty(*llvm_context_);
    auto* fn_type = llvm::FunctionType::get(i1_ty, {ptr_ty, i32_ty}, false);
    lyra_is_trace_observed_global_ = llvm::Function::Create(
        fn_type, llvm::Function::ExternalLinkage, "LyraIsTraceObservedGlobal",
        llvm_module_.get());
  }
  return lyra_is_trace_observed_global_;
}

auto Context::GetLyraNotifyContainerMutationLocal() -> llvm::Function* {
  if (lyra_notify_container_mutation_local_ == nullptr) {
    auto* ptr_ty = llvm::PointerType::getUnqual(*llvm_context_);
    auto* i32_ty = llvm::Type::getInt32Ty(*llvm_context_);
    // (eng, inst, id, off, size)
    auto* fn_type = llvm::FunctionType::get(
        llvm::Type::getVoidTy(*llvm_context_),
        {ptr_ty, ptr_ty, i32_ty, i32_ty, i32_ty}, false);
    lyra_notify_container_mutation_local_ = llvm::Function::Create(
        fn_type, llvm::Function::ExternalLinkage,
        "LyraNotifyContainerMutationLocal", llvm_module_.get());
  }
  return lyra_notify_container_mutation_local_;
}

auto Context::GetLyraNotifyContainerMutationGlobal() -> llvm::Function* {
  if (lyra_notify_container_mutation_global_ == nullptr) {
    auto* ptr_ty = llvm::PointerType::getUnqual(*llvm_context_);
    auto* i32_ty = llvm::Type::getInt32Ty(*llvm_context_);
    auto* fn_type = llvm::FunctionType::get(
        llvm::Type::getVoidTy(*llvm_context_), {ptr_ty, i32_ty, i32_ty, i32_ty},
        false);
    lyra_notify_container_mutation_global_ = llvm::Function::Create(
        fn_type, llvm::Function::ExternalLinkage,
        "LyraNotifyContainerMutationGlobal", llvm_module_.get());
  }
  return lyra_notify_container_mutation_global_;
}

auto Context::GetLyraNotifySignalLocal() -> llvm::Function* {
  if (lyra_notify_signal_local_ == nullptr) {
    auto* ptr_ty = llvm::PointerType::getUnqual(*llvm_context_);
    auto* i32_ty = llvm::Type::getInt32Ty(*llvm_context_);
    // (eng, inst, id)
    auto* fn_type = llvm::FunctionType::get(
        llvm::Type::getVoidTy(*llvm_context_), {ptr_ty, ptr_ty, i32_ty}, false);
    lyra_notify_signal_local_ = llvm::Function::Create(
        fn_type, llvm::Function::ExternalLinkage, "LyraNotifySignalLocal",
        llvm_module_.get());
  }
  return lyra_notify_signal_local_;
}

auto Context::GetLyraNotifySignalGlobal() -> llvm::Function* {
  if (lyra_notify_signal_global_ == nullptr) {
    auto* ptr_ty = llvm::PointerType::getUnqual(*llvm_context_);
    auto* i32_ty = llvm::Type::getInt32Ty(*llvm_context_);
    // (eng, id)
    auto* fn_type = llvm::FunctionType::get(
        llvm::Type::getVoidTy(*llvm_context_), {ptr_ty, i32_ty}, false);
    lyra_notify_signal_global_ = llvm::Function::Create(
        fn_type, llvm::Function::ExternalLinkage, "LyraNotifySignalGlobal",
        llvm_module_.get());
  }
  return lyra_notify_signal_global_;
}

auto Context::GetLyraTerminate() -> llvm::Function* {
  if (lyra_terminate_ == nullptr) {
    // void LyraTerminate(ptr engine, i32 kind, i32 level, ptr message)
    auto* ptr_ty = llvm::PointerType::getUnqual(*llvm_context_);
    auto* i32_ty = llvm::Type::getInt32Ty(*llvm_context_);
    auto* fn_type = llvm::FunctionType::get(
        llvm::Type::getVoidTy(*llvm_context_), {ptr_ty, i32_ty, i32_ty, ptr_ty},
        false);
    lyra_terminate_ = llvm::Function::Create(
        fn_type, llvm::Function::ExternalLinkage, "LyraTerminate",
        llvm_module_.get());
  }
  return lyra_terminate_;
}

auto Context::GetLyraGetTime() -> llvm::Function* {
  if (lyra_get_time_ == nullptr) {
    // uint64_t LyraGetTime(ptr engine)
    auto* ptr_ty = llvm::PointerType::getUnqual(*llvm_context_);
    auto* fn_type = llvm::FunctionType::get(
        llvm::Type::getInt64Ty(*llvm_context_), {ptr_ty}, false);
    lyra_get_time_ = llvm::Function::Create(
        fn_type, llvm::Function::ExternalLinkage, "LyraGetTime",
        llvm_module_.get());
  }
  return lyra_get_time_;
}

auto Context::GetLyraInitRuntime() -> llvm::Function* {
  if (lyra_init_runtime_ == nullptr) {
    // void LyraInitRuntime(uint32_t iteration_limit)
    auto* i32_ty = llvm::Type::getInt32Ty(*llvm_context_);
    auto* fn_type = llvm::FunctionType::get(
        llvm::Type::getVoidTy(*llvm_context_), {i32_ty}, false);
    lyra_init_runtime_ = llvm::Function::Create(
        fn_type, llvm::Function::ExternalLinkage, "LyraInitRuntime",
        llvm_module_.get());
  }
  return lyra_init_runtime_;
}

auto Context::GetLyraResolveBaseDir() -> llvm::Function* {
  if (lyra_resolve_base_dir_ == nullptr) {
    // const char* LyraResolveBaseDir(const char* argv0)
    auto* ptr_ty = llvm::PointerType::getUnqual(*llvm_context_);
    auto* fn_type = llvm::FunctionType::get(ptr_ty, {ptr_ty}, false);
    lyra_resolve_base_dir_ = llvm::Function::Create(
        fn_type, llvm::Function::ExternalLinkage, "LyraResolveBaseDir",
        llvm_module_.get());
  }
  return lyra_resolve_base_dir_;
}

auto Context::GetLyraReportTime() -> llvm::Function* {
  if (lyra_report_time_ == nullptr) {
    // void LyraReportTime(void* run_session_ptr)
    auto* ptr_ty = llvm::PointerType::getUnqual(*llvm_context_);
    auto* fn_type = llvm::FunctionType::get(
        llvm::Type::getVoidTy(*llvm_context_), {ptr_ty}, false);
    lyra_report_time_ = llvm::Function::Create(
        fn_type, llvm::Function::ExternalLinkage, "LyraReportTime",
        llvm_module_.get());
  }
  return lyra_report_time_;
}

auto Context::GetLyraCreateRunSession() -> llvm::Function* {
  if (lyra_create_run_session_ == nullptr) {
    // void* LyraCreateRunSession()
    auto* ptr_ty = llvm::PointerType::getUnqual(*llvm_context_);
    auto* fn_type = llvm::FunctionType::get(ptr_ty, false);
    lyra_create_run_session_ = llvm::Function::Create(
        fn_type, llvm::Function::ExternalLinkage, "LyraCreateRunSession",
        llvm_module_.get());
  }
  return lyra_create_run_session_;
}

auto Context::GetLyraDestroyRunSession() -> llvm::Function* {
  if (lyra_destroy_run_session_ == nullptr) {
    // void LyraDestroyRunSession(void* session)
    auto* ptr_ty = llvm::PointerType::getUnqual(*llvm_context_);
    auto* fn_type = llvm::FunctionType::get(
        llvm::Type::getVoidTy(*llvm_context_), {ptr_ty}, false);
    lyra_destroy_run_session_ = llvm::Function::Create(
        fn_type, llvm::Function::ExternalLinkage, "LyraDestroyRunSession",
        llvm_module_.get());
  }
  return lyra_destroy_run_session_;
}

auto Context::GetLyraDynArrayNew() -> llvm::Function* {
  if (lyra_dynarray_new_ == nullptr) {
    // ptr LyraDynArrayNew(i64 size, i32 elem_size, ptr clone_fn, ptr
    // destroy_fn)
    auto* ptr_ty = llvm::PointerType::getUnqual(*llvm_context_);
    auto* i64_ty = llvm::Type::getInt64Ty(*llvm_context_);
    auto* i32_ty = llvm::Type::getInt32Ty(*llvm_context_);
    auto* fn_type = llvm::FunctionType::get(
        ptr_ty, {i64_ty, i32_ty, ptr_ty, ptr_ty}, false);
    lyra_dynarray_new_ = llvm::Function::Create(
        fn_type, llvm::Function::ExternalLinkage, "LyraDynArrayNew",
        llvm_module_.get());
  }
  return lyra_dynarray_new_;
}

auto Context::GetLyraDynArrayNewCopy() -> llvm::Function* {
  if (lyra_dynarray_new_copy_ == nullptr) {
    // ptr LyraDynArrayNewCopy(i64 size, i32 elem_size, ptr clone_fn,
    //                         ptr destroy_fn, ptr src)
    auto* ptr_ty = llvm::PointerType::getUnqual(*llvm_context_);
    auto* i64_ty = llvm::Type::getInt64Ty(*llvm_context_);
    auto* i32_ty = llvm::Type::getInt32Ty(*llvm_context_);
    auto* fn_type = llvm::FunctionType::get(
        ptr_ty, {i64_ty, i32_ty, ptr_ty, ptr_ty, ptr_ty}, false);
    lyra_dynarray_new_copy_ = llvm::Function::Create(
        fn_type, llvm::Function::ExternalLinkage, "LyraDynArrayNewCopy",
        llvm_module_.get());
  }
  return lyra_dynarray_new_copy_;
}

auto Context::GetLyraDynArraySize() -> llvm::Function* {
  if (lyra_dynarray_size_ == nullptr) {
    // i64 LyraDynArraySize(ptr arr)
    auto* ptr_ty = llvm::PointerType::getUnqual(*llvm_context_);
    auto* i64_ty = llvm::Type::getInt64Ty(*llvm_context_);
    auto* fn_type = llvm::FunctionType::get(i64_ty, {ptr_ty}, false);
    lyra_dynarray_size_ = llvm::Function::Create(
        fn_type, llvm::Function::ExternalLinkage, "LyraDynArraySize",
        llvm_module_.get());
  }
  return lyra_dynarray_size_;
}

auto Context::GetLyraDynArrayElementPtr() -> llvm::Function* {
  if (lyra_dynarray_element_ptr_ == nullptr) {
    // ptr LyraDynArrayElementPtr(ptr arr, i64 index)
    auto* ptr_ty = llvm::PointerType::getUnqual(*llvm_context_);
    auto* i64_ty = llvm::Type::getInt64Ty(*llvm_context_);
    auto* fn_type = llvm::FunctionType::get(ptr_ty, {ptr_ty, i64_ty}, false);
    lyra_dynarray_element_ptr_ = llvm::Function::Create(
        fn_type, llvm::Function::ExternalLinkage, "LyraDynArrayElementPtr",
        llvm_module_.get());
  }
  return lyra_dynarray_element_ptr_;
}

auto Context::GetLyraDynArrayClone() -> llvm::Function* {
  if (lyra_dynarray_clone_ == nullptr) {
    // ptr LyraDynArrayClone(ptr src)
    auto* ptr_ty = llvm::PointerType::getUnqual(*llvm_context_);
    auto* fn_type = llvm::FunctionType::get(ptr_ty, {ptr_ty}, false);
    lyra_dynarray_clone_ = llvm::Function::Create(
        fn_type, llvm::Function::ExternalLinkage, "LyraDynArrayClone",
        llvm_module_.get());
  }
  return lyra_dynarray_clone_;
}

auto Context::GetLyraDynArrayDelete() -> llvm::Function* {
  if (lyra_dynarray_delete_ == nullptr) {
    // void LyraDynArrayDelete(ptr arr)
    auto* ptr_ty = llvm::PointerType::getUnqual(*llvm_context_);
    auto* fn_type = llvm::FunctionType::get(
        llvm::Type::getVoidTy(*llvm_context_), {ptr_ty}, false);
    lyra_dynarray_delete_ = llvm::Function::Create(
        fn_type, llvm::Function::ExternalLinkage, "LyraDynArrayDelete",
        llvm_module_.get());
  }
  return lyra_dynarray_delete_;
}

auto Context::GetLyraDynArrayRelease() -> llvm::Function* {
  if (lyra_dynarray_release_ == nullptr) {
    // void LyraDynArrayRelease(ptr arr)
    auto* ptr_ty = llvm::PointerType::getUnqual(*llvm_context_);
    auto* fn_type = llvm::FunctionType::get(
        llvm::Type::getVoidTy(*llvm_context_), {ptr_ty}, false);
    lyra_dynarray_release_ = llvm::Function::Create(
        fn_type, llvm::Function::ExternalLinkage, "LyraDynArrayRelease",
        llvm_module_.get());
  }
  return lyra_dynarray_release_;
}

auto Context::GetLyraStoreDynArrayLocal() -> llvm::Function* {
  if (lyra_store_dynarray_local_ == nullptr) {
    auto* ptr_ty = llvm::PointerType::getUnqual(*llvm_context_);
    auto* i32_ty = llvm::Type::getInt32Ty(*llvm_context_);
    auto* void_ty = llvm::Type::getVoidTy(*llvm_context_);
    auto* fn_type = llvm::FunctionType::get(
        void_ty, {ptr_ty, ptr_ty, ptr_ty, ptr_ty, i32_ty}, false);
    lyra_store_dynarray_local_ = llvm::Function::Create(
        fn_type, llvm::Function::ExternalLinkage, "LyraStoreDynArrayLocal",
        llvm_module_.get());
  }
  return lyra_store_dynarray_local_;
}

auto Context::GetLyraStoreDynArrayGlobal() -> llvm::Function* {
  if (lyra_store_dynarray_global_ == nullptr) {
    auto* ptr_ty = llvm::PointerType::getUnqual(*llvm_context_);
    auto* i32_ty = llvm::Type::getInt32Ty(*llvm_context_);
    auto* void_ty = llvm::Type::getVoidTy(*llvm_context_);
    auto* fn_type = llvm::FunctionType::get(
        void_ty, {ptr_ty, ptr_ty, ptr_ty, i32_ty}, false);
    lyra_store_dynarray_global_ = llvm::Function::Create(
        fn_type, llvm::Function::ExternalLinkage, "LyraStoreDynArrayGlobal",
        llvm_module_.get());
  }
  return lyra_store_dynarray_global_;
}

auto Context::GetLyraDynArrayCloneElem() -> llvm::Function* {
  if (lyra_dynarray_clone_elem_ == nullptr) {
    // void LyraDynArrayCloneElem(ptr dst, ptr src)
    auto* ptr_ty = llvm::PointerType::getUnqual(*llvm_context_);
    auto* fn_type = llvm::FunctionType::get(
        llvm::Type::getVoidTy(*llvm_context_), {ptr_ty, ptr_ty}, false);
    lyra_dynarray_clone_elem_ = llvm::Function::Create(
        fn_type, llvm::Function::ExternalLinkage, "LyraDynArrayCloneElem",
        llvm_module_.get());
  }
  return lyra_dynarray_clone_elem_;
}

auto Context::GetLyraDynArrayDestroyElem() -> llvm::Function* {
  if (lyra_dynarray_destroy_elem_ == nullptr) {
    // void LyraDynArrayDestroyElem(ptr elem)
    auto* ptr_ty = llvm::PointerType::getUnqual(*llvm_context_);
    auto* fn_type = llvm::FunctionType::get(
        llvm::Type::getVoidTy(*llvm_context_), {ptr_ty}, false);
    lyra_dynarray_destroy_elem_ = llvm::Function::Create(
        fn_type, llvm::Function::ExternalLinkage, "LyraDynArrayDestroyElem",
        llvm_module_.get());
  }
  return lyra_dynarray_destroy_elem_;
}

auto Context::GetLyraQueuePushBack() -> llvm::Function* {
  if (lyra_queue_push_back_ == nullptr) {
    // void LyraQueuePushBack(ptr slot, ptr elem, i32 elem_size,
    //                        i32 max_bound, ptr clone_fn, ptr destroy_fn)
    auto* ptr_ty = llvm::PointerType::getUnqual(*llvm_context_);
    auto* i32_ty = llvm::Type::getInt32Ty(*llvm_context_);
    auto* fn_type = llvm::FunctionType::get(
        llvm::Type::getVoidTy(*llvm_context_),
        {ptr_ty, ptr_ty, i32_ty, i32_ty, ptr_ty, ptr_ty}, false);
    lyra_queue_push_back_ = llvm::Function::Create(
        fn_type, llvm::Function::ExternalLinkage, "LyraQueuePushBack",
        llvm_module_.get());
  }
  return lyra_queue_push_back_;
}

auto Context::GetLyraQueuePushFront() -> llvm::Function* {
  if (lyra_queue_push_front_ == nullptr) {
    // void LyraQueuePushFront(ptr slot, ptr elem, i32 elem_size,
    //                         i32 max_bound, ptr clone_fn, ptr destroy_fn)
    auto* ptr_ty = llvm::PointerType::getUnqual(*llvm_context_);
    auto* i32_ty = llvm::Type::getInt32Ty(*llvm_context_);
    auto* fn_type = llvm::FunctionType::get(
        llvm::Type::getVoidTy(*llvm_context_),
        {ptr_ty, ptr_ty, i32_ty, i32_ty, ptr_ty, ptr_ty}, false);
    lyra_queue_push_front_ = llvm::Function::Create(
        fn_type, llvm::Function::ExternalLinkage, "LyraQueuePushFront",
        llvm_module_.get());
  }
  return lyra_queue_push_front_;
}

auto Context::GetLyraQueuePopBack() -> llvm::Function* {
  if (lyra_queue_pop_back_ == nullptr) {
    // void LyraQueuePopBack(ptr handle, ptr out_elem)
    auto* ptr_ty = llvm::PointerType::getUnqual(*llvm_context_);
    auto* fn_type = llvm::FunctionType::get(
        llvm::Type::getVoidTy(*llvm_context_), {ptr_ty, ptr_ty}, false);
    lyra_queue_pop_back_ = llvm::Function::Create(
        fn_type, llvm::Function::ExternalLinkage, "LyraQueuePopBack",
        llvm_module_.get());
  }
  return lyra_queue_pop_back_;
}

auto Context::GetLyraQueuePopFront() -> llvm::Function* {
  if (lyra_queue_pop_front_ == nullptr) {
    // void LyraQueuePopFront(ptr handle, ptr out_elem)
    auto* ptr_ty = llvm::PointerType::getUnqual(*llvm_context_);
    auto* fn_type = llvm::FunctionType::get(
        llvm::Type::getVoidTy(*llvm_context_), {ptr_ty, ptr_ty}, false);
    lyra_queue_pop_front_ = llvm::Function::Create(
        fn_type, llvm::Function::ExternalLinkage, "LyraQueuePopFront",
        llvm_module_.get());
  }
  return lyra_queue_pop_front_;
}

auto Context::GetLyraQueueInsert() -> llvm::Function* {
  if (lyra_queue_insert_ == nullptr) {
    // void LyraQueueInsert(ptr slot, i64 index, ptr elem, i32 elem_size,
    //                      i32 max_bound, ptr clone_fn, ptr destroy_fn)
    auto* ptr_ty = llvm::PointerType::getUnqual(*llvm_context_);
    auto* i64_ty = llvm::Type::getInt64Ty(*llvm_context_);
    auto* i32_ty = llvm::Type::getInt32Ty(*llvm_context_);
    auto* fn_type = llvm::FunctionType::get(
        llvm::Type::getVoidTy(*llvm_context_),
        {ptr_ty, i64_ty, ptr_ty, i32_ty, i32_ty, ptr_ty, ptr_ty}, false);
    lyra_queue_insert_ = llvm::Function::Create(
        fn_type, llvm::Function::ExternalLinkage, "LyraQueueInsert",
        llvm_module_.get());
  }
  return lyra_queue_insert_;
}

auto Context::GetLyraQueueDeleteAt() -> llvm::Function* {
  if (lyra_queue_delete_at_ == nullptr) {
    // void LyraQueueDeleteAt(ptr handle, i64 index)
    auto* ptr_ty = llvm::PointerType::getUnqual(*llvm_context_);
    auto* i64_ty = llvm::Type::getInt64Ty(*llvm_context_);
    auto* fn_type = llvm::FunctionType::get(
        llvm::Type::getVoidTy(*llvm_context_), {ptr_ty, i64_ty}, false);
    lyra_queue_delete_at_ = llvm::Function::Create(
        fn_type, llvm::Function::ExternalLinkage, "LyraQueueDeleteAt",
        llvm_module_.get());
  }
  return lyra_queue_delete_at_;
}

auto Context::GetLyraStringFormatStart() -> llvm::Function* {
  if (lyra_string_format_start_ == nullptr) {
    // ptr LyraStringFormatStart()
    auto* ptr_ty = llvm::PointerType::getUnqual(*llvm_context_);
    auto* fn_type = llvm::FunctionType::get(ptr_ty, {}, false);
    lyra_string_format_start_ = llvm::Function::Create(
        fn_type, llvm::Function::ExternalLinkage, "LyraStringFormatStart",
        llvm_module_.get());
  }
  return lyra_string_format_start_;
}

auto Context::GetLyraStringFormatLiteral() -> llvm::Function* {
  if (lyra_string_format_literal_ == nullptr) {
    // void LyraStringFormatLiteral(ptr buf, ptr str, i64 len)
    auto* ptr_ty = llvm::PointerType::getUnqual(*llvm_context_);
    auto* i64_ty = llvm::Type::getInt64Ty(*llvm_context_);
    auto* fn_type = llvm::FunctionType::get(
        llvm::Type::getVoidTy(*llvm_context_), {ptr_ty, ptr_ty, i64_ty}, false);
    lyra_string_format_literal_ = llvm::Function::Create(
        fn_type, llvm::Function::ExternalLinkage, "LyraStringFormatLiteral",
        llvm_module_.get());
  }
  return lyra_string_format_literal_;
}

auto Context::GetLyraStringFormatValue() -> llvm::Function* {
  if (lyra_string_format_value_ == nullptr) {
    // void LyraStringFormatValue(ptr buf, ptr engine, i32 format,
    //                            i32 value_kind, ptr data, i32 width,
    //                            i1 is_signed, i32 output_width, i32 precision,
    //                            i1 zero_pad, i1 left_align,
    //                            i8 module_timeunit_power)
    auto* ptr_ty = llvm::PointerType::getUnqual(*llvm_context_);
    auto* i32_ty = llvm::Type::getInt32Ty(*llvm_context_);
    auto* i1_ty = llvm::Type::getInt1Ty(*llvm_context_);
    auto* i8_ty = llvm::Type::getInt8Ty(*llvm_context_);
    auto* fn_type = llvm::FunctionType::get(
        llvm::Type::getVoidTy(*llvm_context_),
        {ptr_ty, ptr_ty, i32_ty, i32_ty, ptr_ty, i32_ty, i1_ty, i32_ty, i32_ty,
         i1_ty, i1_ty, i8_ty},
        false);
    lyra_string_format_value_ = llvm::Function::Create(
        fn_type, llvm::Function::ExternalLinkage, "LyraStringFormatValue",
        llvm_module_.get());
  }
  return lyra_string_format_value_;
}

auto Context::GetLyraStringFormatString() -> llvm::Function* {
  if (lyra_string_format_string_ == nullptr) {
    // void LyraStringFormatString(ptr buf, ptr handle)
    auto* ptr_ty = llvm::PointerType::getUnqual(*llvm_context_);
    auto* fn_type = llvm::FunctionType::get(
        llvm::Type::getVoidTy(*llvm_context_), {ptr_ty, ptr_ty}, false);
    lyra_string_format_string_ = llvm::Function::Create(
        fn_type, llvm::Function::ExternalLinkage, "LyraStringFormatString",
        llvm_module_.get());
  }
  return lyra_string_format_string_;
}

auto Context::GetLyraStringFormatFinish() -> llvm::Function* {
  if (lyra_string_format_finish_ == nullptr) {
    // ptr LyraStringFormatFinish(ptr buf)
    auto* ptr_ty = llvm::PointerType::getUnqual(*llvm_context_);
    auto* fn_type = llvm::FunctionType::get(ptr_ty, {ptr_ty}, false);
    lyra_string_format_finish_ = llvm::Function::Create(
        fn_type, llvm::Function::ExternalLinkage, "LyraStringFormatFinish",
        llvm_module_.get());
  }
  return lyra_string_format_finish_;
}

auto Context::GetLyraStringFormatRuntime() -> llvm::Function* {
  if (lyra_string_format_runtime_ == nullptr) {
    // ptr LyraStringFormatRuntime(ptr fmt, ptr* data, i32* widths, i8* signeds,
    //                             i32* kinds, i64 count)
    auto* ptr_ty = llvm::PointerType::getUnqual(*llvm_context_);
    auto* i64_ty = llvm::Type::getInt64Ty(*llvm_context_);
    auto* fn_type = llvm::FunctionType::get(
        ptr_ty, {ptr_ty, ptr_ty, ptr_ty, ptr_ty, ptr_ty, i64_ty}, false);
    lyra_string_format_runtime_ = llvm::Function::Create(
        fn_type, llvm::Function::ExternalLinkage, "LyraStringFormatRuntime",
        llvm_module_.get());
  }
  return lyra_string_format_runtime_;
}

auto Context::GetLyraSetTimeFormat() -> llvm::Function* {
  if (lyra_set_timeformat_ == nullptr) {
    // void LyraSetTimeFormat(ptr engine, i8 units, i32 precision,
    //                        ptr suffix, i32 min_width)
    auto* ptr_ty = llvm::PointerType::getUnqual(*llvm_context_);
    auto* i8_ty = llvm::Type::getInt8Ty(*llvm_context_);
    auto* i32_ty = llvm::Type::getInt32Ty(*llvm_context_);
    auto* void_ty = llvm::Type::getVoidTy(*llvm_context_);
    auto* fn_type = llvm::FunctionType::get(
        void_ty, {ptr_ty, i8_ty, i32_ty, ptr_ty, i32_ty}, false);
    lyra_set_timeformat_ = llvm::Function::Create(
        fn_type, llvm::Function::ExternalLinkage, "LyraSetTimeFormat",
        llvm_module_.get());
  }
  return lyra_set_timeformat_;
}

auto Context::GetLyraFopenFd() -> llvm::Function* {
  if (lyra_fopen_fd_ == nullptr) {
    // int32_t LyraFopenFd(ptr engine, ptr filename, ptr mode)
    auto* ptr_ty = llvm::PointerType::getUnqual(*llvm_context_);
    auto* i32_ty = llvm::Type::getInt32Ty(*llvm_context_);
    auto* fn_type =
        llvm::FunctionType::get(i32_ty, {ptr_ty, ptr_ty, ptr_ty}, false);
    lyra_fopen_fd_ = llvm::Function::Create(
        fn_type, llvm::Function::ExternalLinkage, "LyraFopenFd",
        llvm_module_.get());
  }
  return lyra_fopen_fd_;
}

auto Context::GetLyraFopenMcd() -> llvm::Function* {
  if (lyra_fopen_mcd_ == nullptr) {
    // int32_t LyraFopenMcd(ptr engine, ptr filename)
    auto* ptr_ty = llvm::PointerType::getUnqual(*llvm_context_);
    auto* i32_ty = llvm::Type::getInt32Ty(*llvm_context_);
    auto* fn_type = llvm::FunctionType::get(i32_ty, {ptr_ty, ptr_ty}, false);
    lyra_fopen_mcd_ = llvm::Function::Create(
        fn_type, llvm::Function::ExternalLinkage, "LyraFopenMcd",
        llvm_module_.get());
  }
  return lyra_fopen_mcd_;
}

auto Context::GetLyraFclose() -> llvm::Function* {
  if (lyra_fclose_ == nullptr) {
    // void LyraFclose(ptr engine, i32 descriptor)
    auto* ptr_ty = llvm::PointerType::getUnqual(*llvm_context_);
    auto* i32_ty = llvm::Type::getInt32Ty(*llvm_context_);
    auto* fn_type = llvm::FunctionType::get(
        llvm::Type::getVoidTy(*llvm_context_), {ptr_ty, i32_ty}, false);
    lyra_fclose_ = llvm::Function::Create(
        fn_type, llvm::Function::ExternalLinkage, "LyraFclose",
        llvm_module_.get());
  }
  return lyra_fclose_;
}

auto Context::GetLyraFflush() -> llvm::Function* {
  if (lyra_fflush_ == nullptr) {
    // void LyraFflush(ptr engine, i1 has_desc, i32 descriptor)
    auto* ptr_ty = llvm::PointerType::getUnqual(*llvm_context_);
    auto* i1_ty = llvm::Type::getInt1Ty(*llvm_context_);
    auto* i32_ty = llvm::Type::getInt32Ty(*llvm_context_);
    auto* fn_type = llvm::FunctionType::get(
        llvm::Type::getVoidTy(*llvm_context_), {ptr_ty, i1_ty, i32_ty}, false);
    lyra_fflush_ = llvm::Function::Create(
        fn_type, llvm::Function::ExternalLinkage, "LyraFflush",
        llvm_module_.get());
  }
  return lyra_fflush_;
}

auto Context::GetLyraFWrite() -> llvm::Function* {
  if (lyra_fwrite_ == nullptr) {
    // void LyraFWrite(ptr engine, i32 descriptor, ptr message, i1 add_newline)
    auto* ptr_ty = llvm::PointerType::getUnqual(*llvm_context_);
    auto* i32_ty = llvm::Type::getInt32Ty(*llvm_context_);
    auto* i1_ty = llvm::Type::getInt1Ty(*llvm_context_);
    auto* fn_type = llvm::FunctionType::get(
        llvm::Type::getVoidTy(*llvm_context_), {ptr_ty, i32_ty, ptr_ty, i1_ty},
        false);
    lyra_fwrite_ = llvm::Function::Create(
        fn_type, llvm::Function::ExternalLinkage, "LyraFWrite",
        llvm_module_.get());
  }
  return lyra_fwrite_;
}

auto Context::GetLyraRegisterStrobe() -> llvm::Function* {
  if (lyra_register_strobe_ == nullptr) {
    // void LyraRegisterStrobe(ptr engine, ptr program, ptr design_state,
    //                         ptr this_ptr, i32 instance_id)
    auto* ptr_ty = llvm::PointerType::getUnqual(*llvm_context_);
    auto* i32_ty = llvm::Type::getInt32Ty(*llvm_context_);
    auto* fn_type = llvm::FunctionType::get(
        llvm::Type::getVoidTy(*llvm_context_),
        {ptr_ty, ptr_ty, ptr_ty, ptr_ty, i32_ty}, false);
    lyra_register_strobe_ = llvm::Function::Create(
        fn_type, llvm::Function::ExternalLinkage, "LyraRegisterStrobe",
        llvm_module_.get());
  }
  return lyra_register_strobe_;
}

auto Context::GetLyraMonitorSetEnabled() -> llvm::Function* {
  if (lyra_monitor_set_enabled_ == nullptr) {
    // void LyraMonitorSetEnabled(ptr engine, i1 enable)
    auto* ptr_ty = llvm::PointerType::getUnqual(*llvm_context_);
    auto* i1_ty = llvm::Type::getInt1Ty(*llvm_context_);
    auto* fn_type = llvm::FunctionType::get(
        llvm::Type::getVoidTy(*llvm_context_), {ptr_ty, i1_ty}, false);
    lyra_monitor_set_enabled_ = llvm::Function::Create(
        fn_type, llvm::Function::ExternalLinkage, "LyraMonitorSetEnabled",
        llvm_module_.get());
  }
  return lyra_monitor_set_enabled_;
}

auto Context::GetLyraMonitorRegister() -> llvm::Function* {
  if (lyra_monitor_register_ == nullptr) {
    // void LyraMonitorRegister(ptr engine, ptr program, ptr design,
    //                          ptr this_ptr, i32 instance_id,
    //                          ptr initial_prev, i32 size)
    auto* ptr_ty = llvm::PointerType::getUnqual(*llvm_context_);
    auto* i32_ty = llvm::Type::getInt32Ty(*llvm_context_);
    auto* fn_type = llvm::FunctionType::get(
        llvm::Type::getVoidTy(*llvm_context_),
        {ptr_ty, ptr_ty, ptr_ty, ptr_ty, i32_ty, ptr_ty, i32_ty}, false);
    lyra_monitor_register_ = llvm::Function::Create(
        fn_type, llvm::Function::ExternalLinkage, "LyraMonitorRegister",
        llvm_module_.get());
  }
  return lyra_monitor_register_;
}

auto Context::GetLyraReadmemLocal() -> llvm::Function* {
  if (lyra_readmem_local_ == nullptr) {
    auto* ptr_ty = llvm::PointerType::getUnqual(*llvm_context_);
    auto* i32_ty = llvm::Type::getInt32Ty(*llvm_context_);
    auto* i64_ty = llvm::Type::getInt64Ty(*llvm_context_);
    auto* i1_ty = llvm::Type::getInt1Ty(*llvm_context_);
    auto* void_ty = llvm::Type::getVoidTy(*llvm_context_);
    auto* fn_type = llvm::FunctionType::get(
        void_ty,
        {ptr_ty, ptr_ty, ptr_ty, i32_ty, i32_ty, i32_ty, i32_ty, i64_ty, i64_ty,
         i64_ty, i64_ty, i1_ty, i32_ty, ptr_ty, i32_ty},
        false);
    lyra_readmem_local_ = llvm::Function::Create(
        fn_type, llvm::Function::ExternalLinkage, "LyraReadmemLocal",
        llvm_module_.get());
  }
  return lyra_readmem_local_;
}

auto Context::GetLyraReadmemGlobal() -> llvm::Function* {
  if (lyra_readmem_global_ == nullptr) {
    auto* ptr_ty = llvm::PointerType::getUnqual(*llvm_context_);
    auto* i32_ty = llvm::Type::getInt32Ty(*llvm_context_);
    auto* i64_ty = llvm::Type::getInt64Ty(*llvm_context_);
    auto* i1_ty = llvm::Type::getInt1Ty(*llvm_context_);
    auto* void_ty = llvm::Type::getVoidTy(*llvm_context_);
    auto* fn_type = llvm::FunctionType::get(
        void_ty,
        {ptr_ty, ptr_ty, ptr_ty, i32_ty, i32_ty, i32_ty, i32_ty, i64_ty, i64_ty,
         i64_ty, i64_ty, i1_ty, i32_ty, i32_ty},
        false);
    lyra_readmem_global_ = llvm::Function::Create(
        fn_type, llvm::Function::ExternalLinkage, "LyraReadmemGlobal",
        llvm_module_.get());
  }
  return lyra_readmem_global_;
}

auto Context::GetLyraReadmemNoNotify() -> llvm::Function* {
  if (lyra_readmem_no_notify_ == nullptr) {
    auto* ptr_ty = llvm::PointerType::getUnqual(*llvm_context_);
    auto* i32_ty = llvm::Type::getInt32Ty(*llvm_context_);
    auto* i64_ty = llvm::Type::getInt64Ty(*llvm_context_);
    auto* i1_ty = llvm::Type::getInt1Ty(*llvm_context_);
    auto* void_ty = llvm::Type::getVoidTy(*llvm_context_);
    // No engine_ptr parameter: this variant does not notify.
    auto* fn_type = llvm::FunctionType::get(
        void_ty,
        {ptr_ty, ptr_ty, i32_ty, i32_ty, i32_ty, i32_ty, i64_ty, i64_ty, i64_ty,
         i64_ty, i1_ty, i32_ty},
        false);
    lyra_readmem_no_notify_ = llvm::Function::Create(
        fn_type, llvm::Function::ExternalLinkage, "LyraReadmemNoNotify",
        llvm_module_.get());
  }
  return lyra_readmem_no_notify_;
}

auto Context::GetLyraWritemem() -> llvm::Function* {
  if (lyra_writemem_ == nullptr) {
    // void LyraWritemem(ptr engine, ptr filename, ptr source,
    //                   i32 elem_width, i32 stride_bytes,
    //                   i32 value_size_bytes, i32 elem_count,
    //                   i64 min_addr, i64 current_addr, i64 final_addr,
    //                   i64 step, i1 is_hex, i32 element_kind)
    auto* ptr_ty = llvm::PointerType::getUnqual(*llvm_context_);
    auto* i32_ty = llvm::Type::getInt32Ty(*llvm_context_);
    auto* i64_ty = llvm::Type::getInt64Ty(*llvm_context_);
    auto* i1_ty = llvm::Type::getInt1Ty(*llvm_context_);
    auto* void_ty = llvm::Type::getVoidTy(*llvm_context_);
    auto* fn_type = llvm::FunctionType::get(
        void_ty,
        {ptr_ty, ptr_ty, ptr_ty, i32_ty, i32_ty, i32_ty, i32_ty, i64_ty, i64_ty,
         i64_ty, i64_ty, i1_ty, i32_ty},
        false);
    lyra_writemem_ = llvm::Function::Create(
        fn_type, llvm::Function::ExternalLinkage, "LyraWritemem",
        llvm_module_.get());
  }
  return lyra_writemem_;
}

auto Context::GetLyraPrintModulePath() -> llvm::Function* {
  if (lyra_print_module_path_ == nullptr) {
    // void LyraPrintModulePath(ptr instance)
    auto* ptr_ty = llvm::PointerType::getUnqual(*llvm_context_);
    auto* fn_type = llvm::FunctionType::get(
        llvm::Type::getVoidTy(*llvm_context_), {ptr_ty}, false);
    lyra_print_module_path_ = llvm::Function::Create(
        fn_type, llvm::Function::ExternalLinkage, "LyraPrintModulePath",
        llvm_module_.get());
  }
  return lyra_print_module_path_;
}

auto Context::GetLyraFillPackedElements() -> llvm::Function* {
  if (lyra_fill_packed_elements_ == nullptr) {
    // void LyraFillPackedElements(ptr dst_val, ptr dst_unk, i32 total_bits,
    //                             ptr src_val, ptr src_unk,
    //                             i32 elem_bits, i32 elem_count)
    auto* ptr_ty = llvm::PointerType::getUnqual(*llvm_context_);
    auto* i32_ty = llvm::Type::getInt32Ty(*llvm_context_);
    auto* fn_type = llvm::FunctionType::get(
        llvm::Type::getVoidTy(*llvm_context_),
        {ptr_ty, ptr_ty, i32_ty, ptr_ty, ptr_ty, i32_ty, i32_ty}, false);
    lyra_fill_packed_elements_ = llvm::Function::Create(
        fn_type, llvm::Function::ExternalLinkage, "LyraFillPackedElements",
        llvm_module_.get());
  }
  return lyra_fill_packed_elements_;
}

auto Context::GetLyraRandom() -> llvm::Function* {
  if (lyra_random_ == nullptr) {
    // int32_t LyraRandom(ptr engine)
    auto* ptr_ty = llvm::PointerType::getUnqual(*llvm_context_);
    auto* i32_ty = llvm::Type::getInt32Ty(*llvm_context_);
    auto* fn_type = llvm::FunctionType::get(i32_ty, {ptr_ty}, false);
    lyra_random_ = llvm::Function::Create(
        fn_type, llvm::Function::ExternalLinkage, "LyraRandom",
        llvm_module_.get());
  }
  return lyra_random_;
}

auto Context::GetLyraUrandom() -> llvm::Function* {
  if (lyra_urandom_ == nullptr) {
    // uint32_t LyraUrandom(ptr engine)
    auto* ptr_ty = llvm::PointerType::getUnqual(*llvm_context_);
    auto* i32_ty = llvm::Type::getInt32Ty(*llvm_context_);
    auto* fn_type = llvm::FunctionType::get(i32_ty, {ptr_ty}, false);
    lyra_urandom_ = llvm::Function::Create(
        fn_type, llvm::Function::ExternalLinkage, "LyraUrandom",
        llvm_module_.get());
  }
  return lyra_urandom_;
}

auto Context::GetLyraFgetc() -> llvm::Function* {
  if (lyra_fgetc_ == nullptr) {
    // int32_t LyraFgetc(ptr engine, int32_t descriptor)
    auto* ptr_ty = llvm::PointerType::getUnqual(*llvm_context_);
    auto* i32_ty = llvm::Type::getInt32Ty(*llvm_context_);
    auto* fn_type = llvm::FunctionType::get(i32_ty, {ptr_ty, i32_ty}, false);
    lyra_fgetc_ = llvm::Function::Create(
        fn_type, llvm::Function::ExternalLinkage, "LyraFgetc",
        llvm_module_.get());
  }
  return lyra_fgetc_;
}

auto Context::GetLyraUngetc() -> llvm::Function* {
  if (lyra_ungetc_ == nullptr) {
    // int32_t LyraUngetc(ptr engine, int32_t character, int32_t descriptor)
    auto* ptr_ty = llvm::PointerType::getUnqual(*llvm_context_);
    auto* i32_ty = llvm::Type::getInt32Ty(*llvm_context_);
    auto* fn_type =
        llvm::FunctionType::get(i32_ty, {ptr_ty, i32_ty, i32_ty}, false);
    lyra_ungetc_ = llvm::Function::Create(
        fn_type, llvm::Function::ExternalLinkage, "LyraUngetc",
        llvm_module_.get());
  }
  return lyra_ungetc_;
}

auto Context::GetLyraFgets() -> llvm::Function* {
  if (lyra_fgets_ == nullptr) {
    // int32_t LyraFgets(ptr engine, int32_t descriptor, ptr str_out)
    auto* ptr_ty = llvm::PointerType::getUnqual(*llvm_context_);
    auto* i32_ty = llvm::Type::getInt32Ty(*llvm_context_);
    auto* fn_type =
        llvm::FunctionType::get(i32_ty, {ptr_ty, i32_ty, ptr_ty}, false);
    lyra_fgets_ = llvm::Function::Create(
        fn_type, llvm::Function::ExternalLinkage, "LyraFgets",
        llvm_module_.get());
  }
  return lyra_fgets_;
}

auto Context::GetLyraFreadLocal() -> llvm::Function* {
  if (lyra_fread_local_ == nullptr) {
    auto* ptr_ty = llvm::PointerType::getUnqual(*llvm_context_);
    auto* i32_ty = llvm::Type::getInt32Ty(*llvm_context_);
    auto* i64_ty = llvm::Type::getInt64Ty(*llvm_context_);
    auto* fn_type = llvm::FunctionType::get(
        i32_ty,
        {ptr_ty, i32_ty, ptr_ty, i32_ty, i32_ty, i32_ty, i64_ty, i64_ty, i64_ty,
         ptr_ty, i32_ty},
        false);
    lyra_fread_local_ = llvm::Function::Create(
        fn_type, llvm::Function::ExternalLinkage, "LyraFreadLocal",
        llvm_module_.get());
  }
  return lyra_fread_local_;
}

auto Context::GetLyraFreadGlobal() -> llvm::Function* {
  if (lyra_fread_global_ == nullptr) {
    auto* ptr_ty = llvm::PointerType::getUnqual(*llvm_context_);
    auto* i32_ty = llvm::Type::getInt32Ty(*llvm_context_);
    auto* i64_ty = llvm::Type::getInt64Ty(*llvm_context_);
    auto* fn_type = llvm::FunctionType::get(
        i32_ty,
        {ptr_ty, i32_ty, ptr_ty, i32_ty, i32_ty, i32_ty, i64_ty, i64_ty, i64_ty,
         i32_ty},
        false);
    lyra_fread_global_ = llvm::Function::Create(
        fn_type, llvm::Function::ExternalLinkage, "LyraFreadGlobal",
        llvm_module_.get());
  }
  return lyra_fread_global_;
}

auto Context::GetLyraFreadNoNotify() -> llvm::Function* {
  if (lyra_fread_no_notify_ == nullptr) {
    auto* ptr_ty = llvm::PointerType::getUnqual(*llvm_context_);
    auto* i32_ty = llvm::Type::getInt32Ty(*llvm_context_);
    auto* i64_ty = llvm::Type::getInt64Ty(*llvm_context_);
    auto* fn_type = llvm::FunctionType::get(
        i32_ty,
        {ptr_ty, i32_ty, ptr_ty, i32_ty, i32_ty, i32_ty, i64_ty, i64_ty,
         i64_ty},
        false);
    lyra_fread_no_notify_ = llvm::Function::Create(
        fn_type, llvm::Function::ExternalLinkage, "LyraFreadNoNotify",
        llvm_module_.get());
  }
  return lyra_fread_no_notify_;
}

auto Context::GetLyraFscanf() -> llvm::Function* {
  if (lyra_fscanf_ == nullptr) {
    // int32_t LyraFscanf(ptr engine, int32_t descriptor, ptr format,
    //                    int32_t output_count, ptr output_ptrs)
    // LyraStringHandle is void* so we use ptr_ty for format
    auto* ptr_ty = llvm::PointerType::getUnqual(*llvm_context_);
    auto* i32_ty = llvm::Type::getInt32Ty(*llvm_context_);
    auto* fn_type = llvm::FunctionType::get(
        i32_ty, {ptr_ty, i32_ty, ptr_ty, i32_ty, ptr_ty}, false);
    lyra_fscanf_ = llvm::Function::Create(
        fn_type, llvm::Function::ExternalLinkage, "LyraFscanf",
        llvm_module_.get());
  }
  return lyra_fscanf_;
}

auto Context::GetLyraAssocNew() -> llvm::Function* {
  if (lyra_assoc_new_ == nullptr) {
    auto* i32_ty = llvm::Type::getInt32Ty(*llvm_context_);
    auto* ptr_ty = llvm::PointerType::getUnqual(*llvm_context_);
    auto* fn_type = llvm::FunctionType::get(
        ptr_ty, {i32_ty, i32_ty, i32_ty, i32_ty, i32_ty}, false);
    lyra_assoc_new_ = llvm::Function::Create(
        fn_type, llvm::Function::ExternalLinkage, "LyraAssocNew",
        llvm_module_.get());
  }
  return lyra_assoc_new_;
}

auto Context::GetLyraAssocRelease() -> llvm::Function* {
  if (lyra_assoc_release_ == nullptr) {
    auto* ptr_ty = llvm::PointerType::getUnqual(*llvm_context_);
    auto* fn_type = llvm::FunctionType::get(
        llvm::Type::getVoidTy(*llvm_context_), {ptr_ty}, false);
    lyra_assoc_release_ = llvm::Function::Create(
        fn_type, llvm::Function::ExternalLinkage, "LyraAssocRelease",
        llvm_module_.get());
  }
  return lyra_assoc_release_;
}

auto Context::GetLyraAssocClone() -> llvm::Function* {
  if (lyra_assoc_clone_ == nullptr) {
    auto* ptr_ty = llvm::PointerType::getUnqual(*llvm_context_);
    auto* fn_type = llvm::FunctionType::get(ptr_ty, {ptr_ty}, false);
    lyra_assoc_clone_ = llvm::Function::Create(
        fn_type, llvm::Function::ExternalLinkage, "LyraAssocClone",
        llvm_module_.get());
  }
  return lyra_assoc_clone_;
}

auto Context::GetLyraAssocGet() -> llvm::Function* {
  if (lyra_assoc_get_ == nullptr) {
    auto* i32_ty = llvm::Type::getInt32Ty(*llvm_context_);
    auto* ptr_ty = llvm::PointerType::getUnqual(*llvm_context_);
    auto* fn_type = llvm::FunctionType::get(
        i32_ty, {ptr_ty, ptr_ty, ptr_ty, i32_ty, ptr_ty, ptr_ty}, false);
    lyra_assoc_get_ = llvm::Function::Create(
        fn_type, llvm::Function::ExternalLinkage, "LyraAssocGet",
        llvm_module_.get());
  }
  return lyra_assoc_get_;
}

auto Context::GetLyraAssocSet() -> llvm::Function* {
  if (lyra_assoc_set_ == nullptr) {
    auto* i32_ty = llvm::Type::getInt32Ty(*llvm_context_);
    auto* ptr_ty = llvm::PointerType::getUnqual(*llvm_context_);
    auto* fn_type = llvm::FunctionType::get(
        i32_ty, {ptr_ty, ptr_ty, ptr_ty, i32_ty, ptr_ty, ptr_ty}, false);
    lyra_assoc_set_ = llvm::Function::Create(
        fn_type, llvm::Function::ExternalLinkage, "LyraAssocSet",
        llvm_module_.get());
  }
  return lyra_assoc_set_;
}

auto Context::GetLyraAssocExists() -> llvm::Function* {
  if (lyra_assoc_exists_ == nullptr) {
    auto* i32_ty = llvm::Type::getInt32Ty(*llvm_context_);
    auto* ptr_ty = llvm::PointerType::getUnqual(*llvm_context_);
    auto* fn_type = llvm::FunctionType::get(
        i32_ty, {ptr_ty, ptr_ty, ptr_ty, i32_ty, ptr_ty}, false);
    lyra_assoc_exists_ = llvm::Function::Create(
        fn_type, llvm::Function::ExternalLinkage, "LyraAssocExists",
        llvm_module_.get());
  }
  return lyra_assoc_exists_;
}

auto Context::GetLyraAssocDeleteKey() -> llvm::Function* {
  if (lyra_assoc_delete_key_ == nullptr) {
    auto* i32_ty = llvm::Type::getInt32Ty(*llvm_context_);
    auto* ptr_ty = llvm::PointerType::getUnqual(*llvm_context_);
    auto* fn_type = llvm::FunctionType::get(
        i32_ty, {ptr_ty, ptr_ty, ptr_ty, i32_ty, ptr_ty}, false);
    lyra_assoc_delete_key_ = llvm::Function::Create(
        fn_type, llvm::Function::ExternalLinkage, "LyraAssocDeleteKey",
        llvm_module_.get());
  }
  return lyra_assoc_delete_key_;
}

auto Context::GetLyraAssocDeleteAll() -> llvm::Function* {
  if (lyra_assoc_delete_all_ == nullptr) {
    auto* ptr_ty = llvm::PointerType::getUnqual(*llvm_context_);
    auto* fn_type = llvm::FunctionType::get(
        llvm::Type::getVoidTy(*llvm_context_), {ptr_ty}, false);
    lyra_assoc_delete_all_ = llvm::Function::Create(
        fn_type, llvm::Function::ExternalLinkage, "LyraAssocDeleteAll",
        llvm_module_.get());
  }
  return lyra_assoc_delete_all_;
}

auto Context::GetLyraAssocSize() -> llvm::Function* {
  if (lyra_assoc_size_ == nullptr) {
    auto* ptr_ty = llvm::PointerType::getUnqual(*llvm_context_);
    auto* i64_ty = llvm::Type::getInt64Ty(*llvm_context_);
    auto* fn_type = llvm::FunctionType::get(i64_ty, {ptr_ty}, false);
    lyra_assoc_size_ = llvm::Function::Create(
        fn_type, llvm::Function::ExternalLinkage, "LyraAssocSize",
        llvm_module_.get());
  }
  return lyra_assoc_size_;
}

auto Context::GetLyraAssocFirst() -> llvm::Function* {
  if (lyra_assoc_first_ == nullptr) {
    auto* i32_ty = llvm::Type::getInt32Ty(*llvm_context_);
    auto* ptr_ty = llvm::PointerType::getUnqual(*llvm_context_);
    auto* fn_type = llvm::FunctionType::get(i32_ty, {ptr_ty, ptr_ty}, false);
    lyra_assoc_first_ = llvm::Function::Create(
        fn_type, llvm::Function::ExternalLinkage, "LyraAssocFirst",
        llvm_module_.get());
  }
  return lyra_assoc_first_;
}

auto Context::GetLyraAssocLast() -> llvm::Function* {
  if (lyra_assoc_last_ == nullptr) {
    auto* i32_ty = llvm::Type::getInt32Ty(*llvm_context_);
    auto* ptr_ty = llvm::PointerType::getUnqual(*llvm_context_);
    auto* fn_type = llvm::FunctionType::get(i32_ty, {ptr_ty, ptr_ty}, false);
    lyra_assoc_last_ = llvm::Function::Create(
        fn_type, llvm::Function::ExternalLinkage, "LyraAssocLast",
        llvm_module_.get());
  }
  return lyra_assoc_last_;
}

auto Context::GetLyraAssocNext() -> llvm::Function* {
  if (lyra_assoc_next_ == nullptr) {
    auto* i32_ty = llvm::Type::getInt32Ty(*llvm_context_);
    auto* ptr_ty = llvm::PointerType::getUnqual(*llvm_context_);
    auto* fn_type = llvm::FunctionType::get(i32_ty, {ptr_ty, ptr_ty}, false);
    lyra_assoc_next_ = llvm::Function::Create(
        fn_type, llvm::Function::ExternalLinkage, "LyraAssocNext",
        llvm_module_.get());
  }
  return lyra_assoc_next_;
}

auto Context::GetLyraAssocPrev() -> llvm::Function* {
  if (lyra_assoc_prev_ == nullptr) {
    auto* i32_ty = llvm::Type::getInt32Ty(*llvm_context_);
    auto* ptr_ty = llvm::PointerType::getUnqual(*llvm_context_);
    auto* fn_type = llvm::FunctionType::get(i32_ty, {ptr_ty, ptr_ty}, false);
    lyra_assoc_prev_ = llvm::Function::Create(
        fn_type, llvm::Function::ExternalLinkage, "LyraAssocPrev",
        llvm_module_.get());
  }
  return lyra_assoc_prev_;
}

auto Context::GetLyraAssocSnapshotCreate() -> llvm::Function* {
  if (lyra_assoc_snapshot_create_ == nullptr) {
    auto* ptr_ty = llvm::PointerType::getUnqual(*llvm_context_);
    auto* fn_type = llvm::FunctionType::get(ptr_ty, {ptr_ty}, false);
    lyra_assoc_snapshot_create_ = llvm::Function::Create(
        fn_type, llvm::Function::ExternalLinkage, "LyraAssocSnapshotCreate",
        llvm_module_.get());
  }
  return lyra_assoc_snapshot_create_;
}

auto Context::GetLyraAssocSnapshotSize() -> llvm::Function* {
  if (lyra_assoc_snapshot_size_ == nullptr) {
    auto* ptr_ty = llvm::PointerType::getUnqual(*llvm_context_);
    auto* i64_ty = llvm::Type::getInt64Ty(*llvm_context_);
    auto* fn_type = llvm::FunctionType::get(i64_ty, {ptr_ty}, false);
    lyra_assoc_snapshot_size_ = llvm::Function::Create(
        fn_type, llvm::Function::ExternalLinkage, "LyraAssocSnapshotSize",
        llvm_module_.get());
  }
  return lyra_assoc_snapshot_size_;
}

auto Context::GetLyraAssocSnapshotKeyAt() -> llvm::Function* {
  if (lyra_assoc_snapshot_key_at_ == nullptr) {
    auto* ptr_ty = llvm::PointerType::getUnqual(*llvm_context_);
    auto* i64_ty = llvm::Type::getInt64Ty(*llvm_context_);
    auto* fn_type = llvm::FunctionType::get(
        llvm::Type::getVoidTy(*llvm_context_), {ptr_ty, i64_ty, ptr_ty}, false);
    lyra_assoc_snapshot_key_at_ = llvm::Function::Create(
        fn_type, llvm::Function::ExternalLinkage, "LyraAssocSnapshotKeyAt",
        llvm_module_.get());
  }
  return lyra_assoc_snapshot_key_at_;
}

auto Context::GetLyraAssocSnapshotRelease() -> llvm::Function* {
  if (lyra_assoc_snapshot_release_ == nullptr) {
    auto* ptr_ty = llvm::PointerType::getUnqual(*llvm_context_);
    auto* fn_type = llvm::FunctionType::get(
        llvm::Type::getVoidTy(*llvm_context_), {ptr_ty}, false);
    lyra_assoc_snapshot_release_ = llvm::Function::Create(
        fn_type, llvm::Function::ExternalLinkage, "LyraAssocSnapshotRelease",
        llvm_module_.get());
  }
  return lyra_assoc_snapshot_release_;
}

auto Context::GetLyraAssocCloneElem() -> llvm::Function* {
  if (lyra_assoc_clone_elem_ == nullptr) {
    auto* ptr_ty = llvm::PointerType::getUnqual(*llvm_context_);
    auto* fn_type = llvm::FunctionType::get(
        llvm::Type::getVoidTy(*llvm_context_), {ptr_ty, ptr_ty}, false);
    lyra_assoc_clone_elem_ = llvm::Function::Create(
        fn_type, llvm::Function::ExternalLinkage, "LyraAssocCloneElem",
        llvm_module_.get());
  }
  return lyra_assoc_clone_elem_;
}

auto Context::GetLyraAssocDestroyElem() -> llvm::Function* {
  if (lyra_assoc_destroy_elem_ == nullptr) {
    auto* ptr_ty = llvm::PointerType::getUnqual(*llvm_context_);
    auto* fn_type = llvm::FunctionType::get(
        llvm::Type::getVoidTy(*llvm_context_), {ptr_ty}, false);
    lyra_assoc_destroy_elem_ = llvm::Function::Create(
        fn_type, llvm::Function::ExternalLinkage, "LyraAssocDestroyElem",
        llvm_module_.get());
  }
  return lyra_assoc_destroy_elem_;
}

auto Context::GetLyraSystemCmd() -> llvm::Function* {
  if (lyra_system_cmd_ == nullptr) {
    // int32_t LyraSystemCmd(ptr engine, ptr cmd_handle)
    auto* ptr_ty = llvm::PointerType::getUnqual(*llvm_context_);
    auto* i32_ty = llvm::Type::getInt32Ty(*llvm_context_);
    auto* fn_type = llvm::FunctionType::get(i32_ty, {ptr_ty, ptr_ty}, false);
    lyra_system_cmd_ = llvm::Function::Create(
        fn_type, llvm::Function::ExternalLinkage, "LyraSystemCmd",
        llvm_module_.get());
  }
  return lyra_system_cmd_;
}

auto Context::GetLyraGetDpiExportCallContext() -> llvm::Function* {
  if (lyra_get_dpi_export_call_context_ == nullptr) {
    // ptr LyraGetDpiExportCallContext()
    auto* ptr_ty = llvm::PointerType::getUnqual(*llvm_context_);
    auto* fn_type = llvm::FunctionType::get(ptr_ty, {}, false);
    lyra_get_dpi_export_call_context_ = llvm::Function::Create(
        fn_type, llvm::Function::ExternalLinkage, "LyraGetDpiExportCallContext",
        llvm_module_.get());
    lyra_get_dpi_export_call_context_->setCallingConv(llvm::CallingConv::C);
  }
  return lyra_get_dpi_export_call_context_;
}

auto Context::GetLyraFailMissingDpiExportCallContext() -> llvm::Function* {
  if (lyra_fail_missing_dpi_export_call_context_ == nullptr) {
    // void LyraFailMissingDpiExportCallContext()  [[noreturn]]
    auto* void_ty = llvm::Type::getVoidTy(*llvm_context_);
    auto* fn_type = llvm::FunctionType::get(void_ty, {}, false);
    lyra_fail_missing_dpi_export_call_context_ = llvm::Function::Create(
        fn_type, llvm::Function::ExternalLinkage,
        "LyraFailMissingDpiExportCallContext", llvm_module_.get());
    lyra_fail_missing_dpi_export_call_context_->setCallingConv(
        llvm::CallingConv::C);
    lyra_fail_missing_dpi_export_call_context_->addFnAttr(
        llvm::Attribute::NoReturn);
  }
  return lyra_fail_missing_dpi_export_call_context_;
}

auto Context::GetLyraPushCurrentDpiScope() -> llvm::Function* {
  if (lyra_push_current_dpi_scope_ == nullptr) {
    // void LyraPushCurrentDpiScope(
    //     DpiContextSnapshot* out_prev, svScope new_scope,
    //     uint32_t owner_id_raw, bool has_owner)
    auto* ptr_ty = llvm::PointerType::getUnqual(*llvm_context_);
    auto* i32_ty = llvm::Type::getInt32Ty(*llvm_context_);
    auto* i1_ty = llvm::Type::getInt1Ty(*llvm_context_);
    auto* void_ty = llvm::Type::getVoidTy(*llvm_context_);
    auto* fn_type = llvm::FunctionType::get(
        void_ty, {ptr_ty, ptr_ty, i32_ty, i1_ty}, false);
    lyra_push_current_dpi_scope_ = llvm::Function::Create(
        fn_type, llvm::Function::ExternalLinkage, "LyraPushCurrentDpiScope",
        llvm_module_.get());
    lyra_push_current_dpi_scope_->setCallingConv(llvm::CallingConv::C);
  }
  return lyra_push_current_dpi_scope_;
}

auto Context::GetLyraPopCurrentDpiScope() -> llvm::Function* {
  if (lyra_pop_current_dpi_scope_ == nullptr) {
    // void LyraPopCurrentDpiScope(const DpiContextSnapshot* prev)
    auto* ptr_ty = llvm::PointerType::getUnqual(*llvm_context_);
    auto* void_ty = llvm::Type::getVoidTy(*llvm_context_);
    auto* fn_type = llvm::FunctionType::get(void_ty, {ptr_ty}, false);
    lyra_pop_current_dpi_scope_ = llvm::Function::Create(
        fn_type, llvm::Function::ExternalLinkage, "LyraPopCurrentDpiScope",
        llvm_module_.get());
    lyra_pop_current_dpi_scope_->setCallingConv(llvm::CallingConv::C);
  }
  return lyra_pop_current_dpi_scope_;
}

auto Context::GetLyraResolvePackageExportBinding() -> llvm::Function* {
  if (lyra_resolve_package_export_binding_ == nullptr) {
    // void LyraResolvePackageExportBinding(DpiResolvedPackageBinding* out)
    auto* ptr_ty = llvm::PointerType::getUnqual(*llvm_context_);
    auto* void_ty = llvm::Type::getVoidTy(*llvm_context_);
    auto* fn_type = llvm::FunctionType::get(void_ty, {ptr_ty}, false);
    lyra_resolve_package_export_binding_ = llvm::Function::Create(
        fn_type, llvm::Function::ExternalLinkage,
        "LyraResolvePackageExportBinding", llvm_module_.get());
    lyra_resolve_package_export_binding_->setCallingConv(llvm::CallingConv::C);
  }
  return lyra_resolve_package_export_binding_;
}

auto Context::GetLyraResolveModuleInstanceBinding() -> llvm::Function* {
  if (lyra_resolve_module_instance_binding_ == nullptr) {
    // void LyraResolveModuleInstanceBinding(DpiResolvedModuleBinding* out)
    auto* ptr_ty = llvm::PointerType::getUnqual(*llvm_context_);
    auto* void_ty = llvm::Type::getVoidTy(*llvm_context_);
    auto* fn_type = llvm::FunctionType::get(void_ty, {ptr_ty}, false);
    lyra_resolve_module_instance_binding_ = llvm::Function::Create(
        fn_type, llvm::Function::ExternalLinkage,
        "LyraResolveModuleInstanceBinding", llvm_module_.get());
    lyra_resolve_module_instance_binding_->setCallingConv(llvm::CallingConv::C);
  }
  return lyra_resolve_module_instance_binding_;
}

auto Context::GetLyraPushDpiExportCallContext() -> llvm::Function* {
  if (lyra_push_dpi_export_call_context_ == nullptr) {
    // void LyraPushDpiExportCallContext(bool suspension_disallowed)
    auto* void_ty = llvm::Type::getVoidTy(*llvm_context_);
    auto* i1_ty = llvm::Type::getInt1Ty(*llvm_context_);
    auto* fn_type = llvm::FunctionType::get(void_ty, {i1_ty}, false);
    lyra_push_dpi_export_call_context_ = llvm::Function::Create(
        fn_type, llvm::Function::ExternalLinkage,
        "LyraPushDpiExportCallContext", llvm_module_.get());
    lyra_push_dpi_export_call_context_->setCallingConv(llvm::CallingConv::C);
  }
  return lyra_push_dpi_export_call_context_;
}

auto Context::GetLyraPopDpiExportCallContext() -> llvm::Function* {
  if (lyra_pop_dpi_export_call_context_ == nullptr) {
    // void LyraPopDpiExportCallContext()
    auto* void_ty = llvm::Type::getVoidTy(*llvm_context_);
    auto* fn_type = llvm::FunctionType::get(void_ty, {}, false);
    lyra_pop_dpi_export_call_context_ = llvm::Function::Create(
        fn_type, llvm::Function::ExternalLinkage, "LyraPopDpiExportCallContext",
        llvm_module_.get());
    lyra_pop_dpi_export_call_context_->setCallingConv(llvm::CallingConv::C);
  }
  return lyra_pop_dpi_export_call_context_;
}

auto Context::GetLyraReportMissingDecisionOwnerFatal() -> llvm::Function* {
  if (lyra_report_missing_decision_owner_fatal_ == nullptr) {
    // void LyraReportMissingDecisionOwnerFatal(void* engine, const char* name)
    auto* ptr_ty = llvm::PointerType::getUnqual(*llvm_context_);
    auto* void_ty = llvm::Type::getVoidTy(*llvm_context_);
    auto* fn_type = llvm::FunctionType::get(void_ty, {ptr_ty, ptr_ty}, false);
    lyra_report_missing_decision_owner_fatal_ = llvm::Function::Create(
        fn_type, llvm::Function::ExternalLinkage,
        "LyraReportMissingDecisionOwnerFatal", llvm_module_.get());
    lyra_report_missing_decision_owner_fatal_->setCallingConv(
        llvm::CallingConv::C);
  }
  return lyra_report_missing_decision_owner_fatal_;
}

}  // namespace lyra::lowering::mir_to_llvm
