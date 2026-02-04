#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/Type.h>

#include "lyra/llvm_backend/context.hpp"

namespace lyra::lowering::mir_to_llvm {

auto Context::GetLyraPrintLiteral() -> llvm::Function* {
  if (lyra_print_literal_ == nullptr) {
    // void LyraPrintLiteral(const char* str)
    auto* fn_type = llvm::FunctionType::get(
        llvm::Type::getVoidTy(*llvm_context_),
        {llvm::PointerType::getUnqual(*llvm_context_)}, false);
    lyra_print_literal_ = llvm::Function::Create(
        fn_type, llvm::Function::ExternalLinkage, "LyraPrintLiteral",
        llvm_module_.get());
  }
  return lyra_print_literal_;
}

auto Context::GetLyraPrintValue() -> llvm::Function* {
  if (lyra_print_value_ == nullptr) {
    // void LyraPrintValue(void* engine, int32_t format, int32_t value_kind,
    //                     const void* data, int32_t width, bool is_signed,
    //                     int32_t output_width, int32_t precision,
    //                     bool zero_pad, bool left_align,
    //                     const void* x_mask, const void* z_mask,
    //                     int8_t module_timeunit_power)
    auto* i32_ty = llvm::Type::getInt32Ty(*llvm_context_);
    auto* ptr_ty = llvm::PointerType::getUnqual(*llvm_context_);
    auto* i1_ty = llvm::Type::getInt1Ty(*llvm_context_);
    auto* i8_ty = llvm::Type::getInt8Ty(*llvm_context_);
    auto* fn_type = llvm::FunctionType::get(
        llvm::Type::getVoidTy(*llvm_context_),
        {ptr_ty, i32_ty, i32_ty, ptr_ty, i32_ty, i1_ty, i32_ty, i32_ty, i1_ty,
         i1_ty, ptr_ty, ptr_ty, i8_ty},
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
    // void LyraPrintString(void* handle, const LyraFormatSpec* spec)
    auto* ptr_ty = llvm::PointerType::getUnqual(*llvm_context_);
    auto* fn_type = llvm::FunctionType::get(
        llvm::Type::getVoidTy(*llvm_context_), {ptr_ty, ptr_ty}, false);
    lyra_print_string_ = llvm::Function::Create(
        fn_type, llvm::Function::ExternalLinkage, "LyraPrintString",
        llvm_module_.get());
  }
  return lyra_print_string_;
}

auto Context::GetLyraPrintEnd() -> llvm::Function* {
  if (lyra_print_end_ == nullptr) {
    // void LyraPrintEnd(int32_t kind)
    auto* fn_type = llvm::FunctionType::get(
        llvm::Type::getVoidTy(*llvm_context_),
        {llvm::Type::getInt32Ty(*llvm_context_)}, false);
    lyra_print_end_ = llvm::Function::Create(
        fn_type, llvm::Function::ExternalLinkage, "LyraPrintEnd",
        llvm_module_.get());
  }
  return lyra_print_end_;
}

auto Context::GetLyraRegisterVar() -> llvm::Function* {
  if (lyra_register_var_ == nullptr) {
    // void LyraRegisterVar(const char*, void*, int32_t, int32_t, bool, bool)
    auto* ptr_ty = llvm::PointerType::getUnqual(*llvm_context_);
    auto* i32_ty = llvm::Type::getInt32Ty(*llvm_context_);
    auto* i1_ty = llvm::Type::getInt1Ty(*llvm_context_);
    auto* fn_type = llvm::FunctionType::get(
        llvm::Type::getVoidTy(*llvm_context_),
        {ptr_ty, ptr_ty, i32_ty, i32_ty, i1_ty, i1_ty}, false);
    lyra_register_var_ = llvm::Function::Create(
        fn_type, llvm::Function::ExternalLinkage, "LyraRegisterVar",
        llvm_module_.get());
  }
  return lyra_register_var_;
}

auto Context::GetLyraSnapshotVars() -> llvm::Function* {
  if (lyra_snapshot_vars_ == nullptr) {
    auto* fn_type =
        llvm::FunctionType::get(llvm::Type::getVoidTy(*llvm_context_), false);
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
    //                        const char** instance_paths, uint32_t num_paths)
    auto* ptr_ty = llvm::PointerType::getUnqual(*llvm_context_);
    auto* i32_ty = llvm::Type::getInt32Ty(*llvm_context_);
    auto* fn_type = llvm::FunctionType::get(
        llvm::Type::getVoidTy(*llvm_context_),
        {ptr_ty, ptr_ty, i32_ty, ptr_ty, i32_ty, ptr_ty, i32_ty}, false);
    lyra_run_simulation_ = llvm::Function::Create(
        fn_type, llvm::Function::ExternalLinkage, "LyraRunSimulation",
        llvm_module_.get());
  }
  return lyra_run_simulation_;
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
    //                      i32 num_triggers)
    auto* ptr_ty = llvm::PointerType::getUnqual(*llvm_context_);
    auto* i32_ty = llvm::Type::getInt32Ty(*llvm_context_);
    auto* fn_type = llvm::FunctionType::get(
        llvm::Type::getVoidTy(*llvm_context_), {ptr_ty, i32_ty, ptr_ty, i32_ty},
        false);
    lyra_suspend_wait_ = llvm::Function::Create(
        fn_type, llvm::Function::ExternalLinkage, "LyraSuspendWait",
        llvm_module_.get());
  }
  return lyra_suspend_wait_;
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

auto Context::GetLyraStorePacked() -> llvm::Function* {
  if (lyra_store_packed_ == nullptr) {
    // void LyraStorePacked(ptr engine, ptr slot, ptr new_value,
    //                      i32 byte_size, i32 signal_id)
    auto* ptr_ty = llvm::PointerType::getUnqual(*llvm_context_);
    auto* i32_ty = llvm::Type::getInt32Ty(*llvm_context_);
    auto* fn_type = llvm::FunctionType::get(
        llvm::Type::getVoidTy(*llvm_context_),
        {ptr_ty, ptr_ty, ptr_ty, i32_ty, i32_ty}, false);
    lyra_store_packed_ = llvm::Function::Create(
        fn_type, llvm::Function::ExternalLinkage, "LyraStorePacked",
        llvm_module_.get());
  }
  return lyra_store_packed_;
}

auto Context::GetLyraStoreString() -> llvm::Function* {
  if (lyra_store_string_ == nullptr) {
    // void LyraStoreString(ptr engine, ptr slot, ptr new_str, i32 signal_id)
    auto* ptr_ty = llvm::PointerType::getUnqual(*llvm_context_);
    auto* i32_ty = llvm::Type::getInt32Ty(*llvm_context_);
    auto* fn_type = llvm::FunctionType::get(
        llvm::Type::getVoidTy(*llvm_context_), {ptr_ty, ptr_ty, ptr_ty, i32_ty},
        false);
    lyra_store_string_ = llvm::Function::Create(
        fn_type, llvm::Function::ExternalLinkage, "LyraStoreString",
        llvm_module_.get());
  }
  return lyra_store_string_;
}

auto Context::GetLyraScheduleNba() -> llvm::Function* {
  if (lyra_schedule_nba_ == nullptr) {
    // void LyraScheduleNba(ptr engine, ptr write_ptr, ptr notify_base_ptr,
    //                      ptr value_ptr, ptr mask_ptr,
    //                      i32 byte_size, i32 notify_slot_id)
    auto* ptr_ty = llvm::PointerType::getUnqual(*llvm_context_);
    auto* i32_ty = llvm::Type::getInt32Ty(*llvm_context_);
    auto* fn_type = llvm::FunctionType::get(
        llvm::Type::getVoidTy(*llvm_context_),
        {ptr_ty, ptr_ty, ptr_ty, ptr_ty, ptr_ty, i32_ty, i32_ty}, false);
    lyra_schedule_nba_ = llvm::Function::Create(
        fn_type, llvm::Function::ExternalLinkage, "LyraScheduleNba",
        llvm_module_.get());
  }
  return lyra_schedule_nba_;
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
    // void LyraInitRuntime(const char* fs_base_dir)
    auto* ptr_ty = llvm::PointerType::getUnqual(*llvm_context_);
    auto* fn_type = llvm::FunctionType::get(
        llvm::Type::getVoidTy(*llvm_context_), {ptr_ty}, false);
    lyra_init_runtime_ = llvm::Function::Create(
        fn_type, llvm::Function::ExternalLinkage, "LyraInitRuntime",
        llvm_module_.get());
  }
  return lyra_init_runtime_;
}

auto Context::GetLyraReportTime() -> llvm::Function* {
  if (lyra_report_time_ == nullptr) {
    auto* fn_type =
        llvm::FunctionType::get(llvm::Type::getVoidTy(*llvm_context_), false);
    lyra_report_time_ = llvm::Function::Create(
        fn_type, llvm::Function::ExternalLinkage, "LyraReportTime",
        llvm_module_.get());
  }
  return lyra_report_time_;
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

auto Context::GetLyraStoreDynArray() -> llvm::Function* {
  if (lyra_store_dynarray_ == nullptr) {
    // void LyraStoreDynArray(ptr engine, ptr slot, ptr new_handle,
    //                        i32 signal_id)
    auto* ptr_ty = llvm::PointerType::getUnqual(*llvm_context_);
    auto* i32_ty = llvm::Type::getInt32Ty(*llvm_context_);
    auto* fn_type = llvm::FunctionType::get(
        llvm::Type::getVoidTy(*llvm_context_), {ptr_ty, ptr_ty, ptr_ty, i32_ty},
        false);
    lyra_store_dynarray_ = llvm::Function::Create(
        fn_type, llvm::Function::ExternalLinkage, "LyraStoreDynArray",
        llvm_module_.get());
  }
  return lyra_store_dynarray_;
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
    // void LyraStringFormatValue(ptr buf, i32 format, i32 value_kind,
    //                            ptr data, i32 width, i1 is_signed,
    //                            i32 output_width, i32 precision,
    //                            i1 zero_pad, i1 left_align,
    //                            ptr x_mask, ptr z_mask)
    auto* ptr_ty = llvm::PointerType::getUnqual(*llvm_context_);
    auto* i32_ty = llvm::Type::getInt32Ty(*llvm_context_);
    auto* i1_ty = llvm::Type::getInt1Ty(*llvm_context_);
    auto* fn_type = llvm::FunctionType::get(
        llvm::Type::getVoidTy(*llvm_context_),
        {ptr_ty, i32_ty, i32_ty, ptr_ty, i32_ty, i1_ty, i32_ty, i32_ty, i1_ty,
         i1_ty, ptr_ty, ptr_ty},
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

auto Context::GetLyraSchedulePostponed() -> llvm::Function* {
  if (lyra_schedule_postponed_ == nullptr) {
    // void LyraSchedulePostponed(ptr engine, ptr callback, ptr design_state)
    auto* ptr_ty = llvm::PointerType::getUnqual(*llvm_context_);
    auto* fn_type = llvm::FunctionType::get(
        llvm::Type::getVoidTy(*llvm_context_), {ptr_ty, ptr_ty, ptr_ty}, false);
    lyra_schedule_postponed_ = llvm::Function::Create(
        fn_type, llvm::Function::ExternalLinkage, "LyraSchedulePostponed",
        llvm_module_.get());
  }
  return lyra_schedule_postponed_;
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
    // void LyraMonitorRegister(ptr engine, ptr check_fn, ptr design,
    //                          ptr initial_prev, i32 size)
    auto* ptr_ty = llvm::PointerType::getUnqual(*llvm_context_);
    auto* i32_ty = llvm::Type::getInt32Ty(*llvm_context_);
    auto* fn_type = llvm::FunctionType::get(
        llvm::Type::getVoidTy(*llvm_context_),
        {ptr_ty, ptr_ty, ptr_ty, ptr_ty, i32_ty}, false);
    lyra_monitor_register_ = llvm::Function::Create(
        fn_type, llvm::Function::ExternalLinkage, "LyraMonitorRegister",
        llvm_module_.get());
  }
  return lyra_monitor_register_;
}

auto Context::GetLyraReadmem() -> llvm::Function* {
  if (lyra_readmem_ == nullptr) {
    // void LyraReadmem(ptr filename, ptr target, i32 elem_width,
    //                  i32 stride_bytes, i32 value_size_bytes, i32 elem_count,
    //                  i64 min_addr, i64 current_addr, i64 final_addr,
    //                  i64 step, i1 is_hex, i32 element_kind)
    auto* ptr_ty = llvm::PointerType::getUnqual(*llvm_context_);
    auto* i32_ty = llvm::Type::getInt32Ty(*llvm_context_);
    auto* i64_ty = llvm::Type::getInt64Ty(*llvm_context_);
    auto* i1_ty = llvm::Type::getInt1Ty(*llvm_context_);
    auto* void_ty = llvm::Type::getVoidTy(*llvm_context_);
    auto* fn_type = llvm::FunctionType::get(
        void_ty,
        {ptr_ty, ptr_ty, i32_ty, i32_ty, i32_ty, i32_ty, i64_ty, i64_ty, i64_ty,
         i64_ty, i1_ty, i32_ty},
        false);
    lyra_readmem_ = llvm::Function::Create(
        fn_type, llvm::Function::ExternalLinkage, "LyraReadmem",
        llvm_module_.get());
  }
  return lyra_readmem_;
}

auto Context::GetLyraWritemem() -> llvm::Function* {
  if (lyra_writemem_ == nullptr) {
    // void LyraWritemem(ptr filename, ptr source, i32 elem_width,
    //                   i32 stride_bytes, i32 value_size_bytes, i32 elem_count,
    //                   i64 min_addr, i64 current_addr, i64 final_addr,
    //                   i64 step, i1 is_hex, i32 element_kind)
    auto* ptr_ty = llvm::PointerType::getUnqual(*llvm_context_);
    auto* i32_ty = llvm::Type::getInt32Ty(*llvm_context_);
    auto* i64_ty = llvm::Type::getInt64Ty(*llvm_context_);
    auto* i1_ty = llvm::Type::getInt1Ty(*llvm_context_);
    auto* void_ty = llvm::Type::getVoidTy(*llvm_context_);
    auto* fn_type = llvm::FunctionType::get(
        void_ty,
        {ptr_ty, ptr_ty, i32_ty, i32_ty, i32_ty, i32_ty, i64_ty, i64_ty, i64_ty,
         i64_ty, i1_ty, i32_ty},
        false);
    lyra_writemem_ = llvm::Function::Create(
        fn_type, llvm::Function::ExternalLinkage, "LyraWritemem",
        llvm_module_.get());
  }
  return lyra_writemem_;
}

auto Context::GetLyraNotifySignal() -> llvm::Function* {
  if (lyra_notify_signal_ == nullptr) {
    // void LyraNotifySignal(ptr engine, ptr slot, i32 signal_id)
    auto* ptr_ty = llvm::PointerType::getUnqual(*llvm_context_);
    auto* i32_ty = llvm::Type::getInt32Ty(*llvm_context_);
    auto* fn_type = llvm::FunctionType::get(
        llvm::Type::getVoidTy(*llvm_context_), {ptr_ty, ptr_ty, i32_ty}, false);
    lyra_notify_signal_ = llvm::Function::Create(
        fn_type, llvm::Function::ExternalLinkage, "LyraNotifySignal",
        llvm_module_.get());
  }
  return lyra_notify_signal_;
}

auto Context::GetLyraPrintModulePath() -> llvm::Function* {
  if (lyra_print_module_path_ == nullptr) {
    // void LyraPrintModulePath(ptr engine, i32 instance_id)
    auto* ptr_ty = llvm::PointerType::getUnqual(*llvm_context_);
    auto* i32_ty = llvm::Type::getInt32Ty(*llvm_context_);
    auto* fn_type = llvm::FunctionType::get(
        llvm::Type::getVoidTy(*llvm_context_), {ptr_ty, i32_ty}, false);
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

auto Context::GetLyraFread() -> llvm::Function* {
  if (lyra_fread_ == nullptr) {
    // int32_t LyraFread(ptr engine, int32_t descriptor, ptr target,
    //                   int32_t element_width, int32_t stride_bytes,
    //                   int32_t is_memory, int64_t start_index,
    //                   int64_t max_count, int64_t element_count)
    auto* ptr_ty = llvm::PointerType::getUnqual(*llvm_context_);
    auto* i32_ty = llvm::Type::getInt32Ty(*llvm_context_);
    auto* i64_ty = llvm::Type::getInt64Ty(*llvm_context_);
    auto* fn_type = llvm::FunctionType::get(
        i32_ty,
        {ptr_ty, i32_ty, ptr_ty, i32_ty, i32_ty, i32_ty, i64_ty, i64_ty,
         i64_ty},
        false);
    lyra_fread_ = llvm::Function::Create(
        fn_type, llvm::Function::ExternalLinkage, "LyraFread",
        llvm_module_.get());
  }
  return lyra_fread_;
}

}  // namespace lyra::lowering::mir_to_llvm
