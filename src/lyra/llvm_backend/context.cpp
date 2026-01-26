#include "lyra/llvm_backend/context.hpp"

#include <algorithm>
#include <cstddef>
#include <cstdint>
#include <format>
#include <memory>
#include <stdexcept>
#include <unordered_set>
#include <utility>
#include <variant>
#include <vector>

#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/Value.h"
#include "llvm/Support/Casting.h"
#include "lyra/common/internal_error.hpp"
#include "lyra/common/type.hpp"
#include "lyra/common/type_arena.hpp"
#include "lyra/common/unsupported_error.hpp"
#include "lyra/llvm_backend/layout.hpp"
#include "lyra/llvm_backend/operand.hpp"
#include "lyra/llvm_backend/type_ops_store.hpp"
#include "lyra/mir/arena.hpp"
#include "lyra/mir/design.hpp"
#include "lyra/mir/handle.hpp"
#include "lyra/mir/place.hpp"
#include "lyra/mir/place_type.hpp"

namespace lyra::lowering::mir_to_llvm {

namespace {

// Get the LLVM type for storage of an integral type (local helper)
auto GetIntegralStorageType(llvm::LLVMContext& ctx, uint32_t bit_width)
    -> llvm::Type* {
  // Round up to the next power-of-2 storage size for efficient access
  if (bit_width <= 8) {
    return llvm::Type::getInt8Ty(ctx);
  }
  if (bit_width <= 16) {
    return llvm::Type::getInt16Ty(ctx);
  }
  if (bit_width <= 32) {
    return llvm::Type::getInt32Ty(ctx);
  }
  if (bit_width <= 64) {
    return llvm::Type::getInt64Ty(ctx);
  }
  // For wider types, use the exact bit width
  return llvm::Type::getIntNTy(ctx, bit_width);
}

// Get the LLVM struct type for a 4-state value: {iN_storage, iN_storage}
auto GetFourStateStructType(llvm::LLVMContext& ctx, uint32_t bit_width)
    -> llvm::StructType* {
  auto* elem = GetIntegralStorageType(ctx, bit_width);
  return llvm::StructType::get(ctx, {elem, elem});
}

}  // namespace

Context::Context(
    const mir::Design& design, const mir::Arena& arena, const TypeArena& types,
    const Layout& layout, std::unique_ptr<llvm::LLVMContext> llvm_ctx,
    std::unique_ptr<llvm::Module> module)
    : design_(design),
      arena_(arena),
      types_(types),
      layout_(layout),
      llvm_context_(std::move(llvm_ctx)),
      llvm_module_(std::move(module)),
      builder_(*llvm_context_) {
}

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
    // void LyraPrintValue(int32_t format, const void* data, int32_t width,
    //                     bool is_signed, int32_t output_width,
    //                     int32_t precision, bool zero_pad, bool left_align,
    //                     const void* x_mask, const void* z_mask)
    auto* i32_ty = llvm::Type::getInt32Ty(*llvm_context_);
    auto* ptr_ty = llvm::PointerType::getUnqual(*llvm_context_);
    auto* i1_ty = llvm::Type::getInt1Ty(*llvm_context_);
    auto* fn_type = llvm::FunctionType::get(
        llvm::Type::getVoidTy(*llvm_context_),
        {i32_ty, ptr_ty, i32_ty, i1_ty, i32_ty, i32_ty, i1_ty, i1_ty, ptr_ty,
         ptr_ty},
        false);
    lyra_print_value_ = llvm::Function::Create(
        fn_type, llvm::Function::ExternalLinkage, "LyraPrintValue",
        llvm_module_.get());
  }
  return lyra_print_value_;
}

auto Context::GetLyraPrintString() -> llvm::Function* {
  if (lyra_print_string_ == nullptr) {
    // void LyraPrintString(void* handle)
    auto* fn_type = llvm::FunctionType::get(
        llvm::Type::getVoidTy(*llvm_context_),
        {llvm::PointerType::getUnqual(*llvm_context_)}, false);
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
    // void LyraRegisterVar(const char*, void*, int32_t, int32_t, bool)
    auto* ptr_ty = llvm::PointerType::getUnqual(*llvm_context_);
    auto* i32_ty = llvm::Type::getInt32Ty(*llvm_context_);
    auto* i1_ty = llvm::Type::getInt1Ty(*llvm_context_);
    auto* fn_type = llvm::FunctionType::get(
        llvm::Type::getVoidTy(*llvm_context_),
        {ptr_ty, ptr_ty, i32_ty, i32_ty, i1_ty}, false);
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

auto Context::GetLyraRunSimulation() -> llvm::Function* {
  if (lyra_run_simulation_ == nullptr) {
    // void LyraRunSimulation(ptr* processes, ptr* states, uint32_t num)
    auto* ptr_ty = llvm::PointerType::getUnqual(*llvm_context_);
    auto* i32_ty = llvm::Type::getInt32Ty(*llvm_context_);
    auto* fn_type = llvm::FunctionType::get(
        llvm::Type::getVoidTy(*llvm_context_), {ptr_ty, ptr_ty, i32_ty}, false);
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

auto Context::GetLyraFinishSimulation() -> llvm::Function* {
  if (lyra_finish_simulation_ == nullptr) {
    // void LyraFinishSimulation(ptr engine)
    auto* ptr_ty = llvm::PointerType::getUnqual(*llvm_context_);
    auto* fn_type = llvm::FunctionType::get(
        llvm::Type::getVoidTy(*llvm_context_), {ptr_ty}, false);
    lyra_finish_simulation_ = llvm::Function::Create(
        fn_type, llvm::Function::ExternalLinkage, "LyraFinishSimulation",
        llvm_module_.get());
  }
  return lyra_finish_simulation_;
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
    auto* fn_type =
        llvm::FunctionType::get(llvm::Type::getVoidTy(*llvm_context_), false);
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
    // void LyraStringFormatValue(ptr buf, i32 format, ptr data, i32 width,
    //                            i1 is_signed, i32 output_width,
    //                            i32 precision, i1 zero_pad, i1 left_align,
    //                            ptr x_mask, ptr z_mask)
    auto* ptr_ty = llvm::PointerType::getUnqual(*llvm_context_);
    auto* i32_ty = llvm::Type::getInt32Ty(*llvm_context_);
    auto* i1_ty = llvm::Type::getInt1Ty(*llvm_context_);
    auto* fn_type = llvm::FunctionType::get(
        llvm::Type::getVoidTy(*llvm_context_),
        {ptr_ty, i32_ty, ptr_ty, i32_ty, i1_ty, i32_ty, i32_ty, i1_ty, i1_ty,
         ptr_ty, ptr_ty},
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

auto Context::GetElemOpsForType(TypeId elem_type) -> ElemOpsInfo {
  const Type& type = types_[elem_type];
  auto* ptr_ty = llvm::PointerType::getUnqual(*llvm_context_);
  auto* null_ptr =
      llvm::ConstantPointerNull::get(llvm::cast<llvm::PointerType>(ptr_ty));

  if (type.Kind() == TypeKind::kDynamicArray ||
      type.Kind() == TypeKind::kQueue) {
    // Nested dynamic array or queue: element is a pointer
    auto ptr_size =
        static_cast<int32_t>(llvm_module_->getDataLayout().getPointerSize());
    return ElemOpsInfo{
        .elem_size = ptr_size,
        .elem_llvm_type = ptr_ty,
        .clone_fn = GetLyraDynArrayCloneElem(),
        .destroy_fn = GetLyraDynArrayDestroyElem(),
        .needs_clone = true,
    };
  }

  // POD types: compute size from DataLayout, no clone/destroy needed
  llvm::Type* llvm_type = BuildLlvmTypeForTypeId(*this, elem_type);
  auto byte_size = static_cast<int32_t>(
      llvm_module_->getDataLayout().getTypeAllocSize(llvm_type));
  return ElemOpsInfo{
      .elem_size = byte_size,
      .elem_llvm_type = llvm_type,
      .clone_fn = null_ptr,
      .destroy_fn = null_ptr,
  };
}

auto Context::GetOrCreateUnionStorageInfo(
    TypeId union_type, CachedUnionInfo info) -> CachedUnionInfo {
  auto [it, inserted] = union_storage_cache_.emplace(union_type, info);
  return it->second;
}

auto Context::GetCachedUnionStorageInfo(TypeId union_type) const
    -> const CachedUnionInfo* {
  auto it = union_storage_cache_.find(union_type);
  if (it != union_storage_cache_.end()) {
    return &it->second;
  }
  return nullptr;
}

auto Context::GetOrCreateEnumValuesGlobal(TypeId enum_type)
    -> llvm::GlobalVariable* {
  auto it = enum_values_globals_.find(enum_type);
  if (it != enum_values_globals_.end()) {
    return it->second;
  }

  const Type& type = types_[enum_type];
  const auto& enum_info = type.AsEnum();
  uint32_t bit_width = PackedBitWidth(type, types_);
  uint32_t storage_bits =
      GetIntegralStorageType(*llvm_context_, bit_width)->getIntegerBitWidth();
  auto* elem_type = llvm::Type::getIntNTy(*llvm_context_, storage_bits);

  std::vector<llvm::Constant*> values;
  values.reserve(enum_info.members.size());
  for (const auto& member : enum_info.members) {
    auto build_width = static_cast<uint32_t>(std::max(
        static_cast<size_t>(storage_bits), member.value.value.size() * 64));
    llvm::APInt ap_val(build_width, 0);
    for (size_t w = 0; w < member.value.value.size(); ++w) {
      ap_val.insertBits(llvm::APInt(64, member.value.value[w]), w * 64);
    }
    ap_val = ap_val.trunc(storage_bits);
    values.push_back(llvm::ConstantInt::get(elem_type, ap_val));
  }

  auto* arr_type = llvm::ArrayType::get(elem_type, enum_info.members.size());
  auto* initializer = llvm::ConstantArray::get(arr_type, values);
  auto* global = new llvm::GlobalVariable(
      *llvm_module_, arr_type, true, llvm::GlobalValue::InternalLinkage,
      initializer, "enum.values");

  enum_values_globals_[enum_type] = global;
  return global;
}

auto Context::GetHeaderType() const -> llvm::StructType* {
  return layout_.header_type;
}

auto Context::GetDesignStateType() const -> llvm::StructType* {
  return layout_.design.llvm_type;
}

auto Context::GetProcessFrameType() const -> llvm::StructType* {
  return layout_.processes[current_process_index_].frame.llvm_type;
}

auto Context::GetProcessStateType() const -> llvm::StructType* {
  return layout_.processes[current_process_index_].state_type;
}

void Context::BeginFunction(llvm::Function& func) {
  current_function_ = &func;

  // Set up alloca builder at the start of the entry block, before any
  // non-alloca instructions. This ensures all allocas are grouped at the
  // beginning and dominate all uses.
  llvm::BasicBlock& entry = func.getEntryBlock();
  llvm::BasicBlock::iterator insert_point = entry.begin();

  // Skip past any existing allocas to keep them grouped
  while (insert_point != entry.end() &&
         llvm::isa<llvm::AllocaInst>(&*insert_point)) {
    ++insert_point;
  }

  alloca_builder_ = std::make_unique<llvm::IRBuilder<>>(&entry, insert_point);
}

void Context::EndFunction() {
  current_function_ = nullptr;
  alloca_builder_.reset();
  // Clear place storage for next function. With PlaceRootKey keying, different
  // functions' locals with the same ID would otherwise collide.
  place_storage_.clear();
}

auto Context::GetOrCreatePlaceStorage(const mir::PlaceRoot& root)
    -> llvm::AllocaInst* {
  // Check if we already have storage for this root.
  // Storage is keyed by root identity (kind + id), NOT by PlaceId.
  PlaceRootKey key{.kind = root.kind, .id = root.id};
  auto it = place_storage_.find(key);
  if (it != place_storage_.end()) {
    return it->second;
  }

  // Must be inside a function scope
  if (current_function_ == nullptr || alloca_builder_ == nullptr) {
    throw common::InternalError(
        "GetOrCreatePlaceStorage",
        "must call BeginFunction before creating place storage");
  }

  TypeId type_id = root.type;
  const Type& type = types_[type_id];

  llvm::Type* llvm_type = nullptr;
  if (type.Kind() == TypeKind::kIntegral) {
    uint32_t bit_width = type.AsIntegral().bit_width;
    if (type.AsIntegral().is_four_state) {
      llvm_type = GetFourStateStructType(*llvm_context_, bit_width);
    } else {
      llvm_type = GetIntegralStorageType(*llvm_context_, bit_width);
    }
  } else if (type.Kind() == TypeKind::kReal) {
    llvm_type = llvm::Type::getDoubleTy(*llvm_context_);
  } else if (type.Kind() == TypeKind::kShortReal) {
    llvm_type = llvm::Type::getFloatTy(*llvm_context_);
  } else if (
      type.Kind() == TypeKind::kString ||
      type.Kind() == TypeKind::kDynamicArray ||
      type.Kind() == TypeKind::kQueue) {
    llvm_type = llvm::PointerType::getUnqual(*llvm_context_);
  } else if (IsPacked(type)) {
    auto width = PackedBitWidth(type, types_);
    if (IsPackedFourState(type, types_)) {
      llvm_type = GetFourStateStructType(*llvm_context_, width);
    } else {
      llvm_type = GetIntegralStorageType(*llvm_context_, width);
    }
  } else if (
      type.Kind() == TypeKind::kUnpackedStruct ||
      type.Kind() == TypeKind::kUnpackedArray ||
      type.Kind() == TypeKind::kUnpackedUnion) {
    llvm_type = BuildLlvmTypeForTypeId(*this, type_id);
  } else {
    throw common::UnsupportedErrorException(
        common::UnsupportedLayer::kMirToLlvm, common::UnsupportedKind::kType,
        current_origin_,
        std::format("type not yet supported: {}", ToString(type)));
  }

  // Re-compute insertion point: after all existing allocas, before any
  // non-alloca This is necessary because the main builder may have inserted
  // non-alloca instructions since we last created an alloca.
  llvm::BasicBlock& entry = current_function_->getEntryBlock();
  llvm::BasicBlock::iterator insert_point = entry.begin();
  while (insert_point != entry.end() &&
         llvm::isa<llvm::AllocaInst>(&*insert_point)) {
    ++insert_point;
  }
  alloca_builder_->SetInsertPoint(&entry, insert_point);

  // Create the alloca using the dedicated alloca builder (entry block)
  auto* alloca = alloca_builder_->CreateAlloca(llvm_type, nullptr, "place");

  // Initialize pointer-typed places to nullptr (dynamic arrays, queues,
  // strings)
  if (type.Kind() == TypeKind::kDynamicArray ||
      type.Kind() == TypeKind::kQueue || type.Kind() == TypeKind::kString) {
    auto* null_val = llvm::ConstantPointerNull::get(
        llvm::PointerType::getUnqual(*llvm_context_));
    alloca_builder_->CreateStore(null_val, alloca);
  }

  // Store in the map, keyed by root identity
  place_storage_[key] = alloca;

  return alloca;
}

auto Context::GetDesignFieldIndex(mir::SlotId slot_id) const -> uint32_t {
  auto it = layout_.design.slot_to_field.find(slot_id);
  if (it == layout_.design.slot_to_field.end()) {
    throw std::runtime_error("design slot not found in layout");
  }
  return it->second;
}

auto Context::GetFrameFieldIndex(mir::PlaceId place_id) const -> uint32_t {
  const auto& place = arena_[place_id];
  PlaceRootKey key{.kind = place.root.kind, .id = place.root.id};
  const auto& frame = layout_.processes[current_process_index_].frame;
  auto it = frame.root_to_field.find(key);
  if (it == frame.root_to_field.end()) {
    throw std::runtime_error("frame place not found in layout");
  }
  return it->second;
}

void Context::SetCurrentProcess(size_t process_index) {
  current_process_index_ = process_index;
  state_ptr_ = nullptr;
  design_ptr_ = nullptr;
  frame_ptr_ = nullptr;
  engine_ptr_ = nullptr;
}

auto Context::GetCurrentProcessIndex() const -> size_t {
  return current_process_index_;
}

void Context::SetStatePointer(llvm::Value* state_ptr) {
  state_ptr_ = state_ptr;
}

auto Context::GetStatePointer() -> llvm::Value* {
  return state_ptr_;
}

void Context::SetDesignPointer(llvm::Value* design_ptr) {
  design_ptr_ = design_ptr;
}

auto Context::GetDesignPointer() -> llvm::Value* {
  return design_ptr_;
}

void Context::SetFramePointer(llvm::Value* frame_ptr) {
  frame_ptr_ = frame_ptr;
}

auto Context::GetFramePointer() -> llvm::Value* {
  return frame_ptr_;
}

void Context::SetEnginePointer(llvm::Value* engine_ptr) {
  engine_ptr_ = engine_ptr;
}

auto Context::GetEnginePointer() -> llvm::Value* {
  return engine_ptr_;
}

auto Context::ResolveAliases(mir::PlaceId place_id) -> mir::Place {
  const mir::Place& original = arena_[place_id];

  // Only kDesign roots can be aliased
  if (original.root.kind != mir::PlaceRoot::Kind::kDesign) {
    return original;
  }

  mir::SlotId slot{static_cast<uint32_t>(original.root.id)};
  auto alias_it = design_.alias_map.find(slot);
  if (alias_it == design_.alias_map.end()) {
    return original;  // Not aliased
  }

  // Flatten alias chain with cycle detection
  std::unordered_set<mir::SlotId> visited;
  visited.insert(slot);
  mir::SlotId start_slot = slot;  // For error reporting

  mir::Place resolved = original;
  std::vector<mir::Projection> accumulated_projections = original.projections;

  while (alias_it != design_.alias_map.end()) {
    mir::PlaceId target_place_id = alias_it->second;
    const mir::Place& target = arena_[target_place_id];

    // Invariant: alias_map only maps to kDesign roots
    if (target.root.kind != mir::PlaceRoot::Kind::kDesign) {
      throw common::InternalError(
          "ResolveAliases",
          std::format(
              "alias target for slot {} is not a design slot", slot.value));
    }

    // Prepend target's projections (target.proj comes before our accumulated)
    std::vector<mir::Projection> new_projections = target.projections;
    new_projections.insert(
        new_projections.end(), accumulated_projections.begin(),
        accumulated_projections.end());
    accumulated_projections = std::move(new_projections);

    // Update resolved root
    resolved.root = target.root;

    mir::SlotId target_slot{static_cast<uint32_t>(target.root.id)};

    // Cycle detection
    if (visited.contains(target_slot)) {
      throw common::InternalError(
          "ResolveAliases",
          std::format(
              "alias cycle detected: slot {} -> ... -> slot {}",
              start_slot.value, target_slot.value));
    }
    visited.insert(target_slot);

    alias_it = design_.alias_map.find(target_slot);
  }

  resolved.projections = std::move(accumulated_projections);
  return resolved;
}

auto Context::GetPlacePointer(mir::PlaceId place_id) -> llvm::Value* {
  // Resolve aliases first (handles output/inout ports)
  mir::Place place = ResolveAliases(place_id);

  // Get base pointer from root
  llvm::Value* ptr = nullptr;
  if (place.root.kind == mir::PlaceRoot::Kind::kDesign) {
    if (design_ptr_ == nullptr) {
      throw std::runtime_error("design pointer not set");
    }
    auto slot_id = mir::SlotId{static_cast<uint32_t>(place.root.id)};
    uint32_t field_index = GetDesignFieldIndex(slot_id);
    ptr = builder_.CreateStructGEP(
        GetDesignStateType(), design_ptr_, field_index, "design_slot_ptr");
  } else {
    // Local/Temp places: check place_storage_ first (user functions)
    // If not found and frame_ptr_ is set, use frame (processes)
    //
    // Storage is keyed by root identity (kind + id), NOT PlaceId.
    PlaceRootKey root_key{.kind = place.root.kind, .id = place.root.id};
    auto it = place_storage_.find(root_key);
    if (it != place_storage_.end()) {
      ptr = it->second;
    } else if (frame_ptr_ != nullptr) {
      uint32_t field_index = GetFrameFieldIndex(place_id);
      ptr = builder_.CreateStructGEP(
          GetProcessFrameType(), frame_ptr_, field_index, "frame_slot_ptr");
    } else {
      // User function: create alloca lazily for the root
      ptr = GetOrCreatePlaceStorage(place.root);
    }
  }

  // Apply projections (index into arrays, stop at BitRange)
  TypeId current_type = place.root.type;
  for (const auto& proj : place.projections) {
    if (std::holds_alternative<mir::BitRangeProjection>(proj.info)) {
      break;
    }

    // FieldProjection: struct field access
    if (const auto* field = std::get_if<mir::FieldProjection>(&proj.info)) {
      const Type& cur_type = types_[current_type];
      if (cur_type.Kind() != TypeKind::kUnpackedStruct) {
        throw common::InternalError(
            "GetPlacePointer", std::format(
                                   "FieldProjection on non-struct type: {}",
                                   static_cast<int>(cur_type.Kind())));
      }
      const auto& struct_info = cur_type.AsUnpackedStruct();

      auto field_idx = static_cast<size_t>(field->field_index);
      if (field_idx >= struct_info.fields.size()) {
        throw common::InternalError(
            "GetPlacePointer",
            std::format(
                "field index {} out of bounds (struct has {} fields)",
                field_idx, struct_info.fields.size()));
      }

      llvm::Type* struct_type = BuildLlvmTypeForTypeId(*this, current_type);
      ptr = builder_.CreateStructGEP(
          struct_type, ptr, static_cast<unsigned>(field->field_index),
          "struct_field_ptr");
      current_type = struct_info.fields[field_idx].type;
      continue;
    }

    // UnionMemberProjection: union member access (offset is always 0, type view
    // changes)
    if (const auto* umem =
            std::get_if<mir::UnionMemberProjection>(&proj.info)) {
      const Type& cur_type = types_[current_type];
      if (cur_type.Kind() != TypeKind::kUnpackedUnion) {
        throw common::InternalError(
            "GetPlacePointer",
            std::format(
                "UnionMemberProjection on non-union type: {}",
                static_cast<int>(cur_type.Kind())));
      }
      const auto& union_info = cur_type.AsUnpackedUnion();

      if (umem->member_index >= union_info.members.size()) {
        throw common::InternalError(
            "GetPlacePointer",
            std::format(
                "union member index {} out of bounds (union has {} members)",
                umem->member_index, union_info.members.size()));
      }

      // Update the type to the member type. The pointer remains unchanged
      // (offset is always 0), but subsequent operations will interpret it
      // as the member type.
      current_type = union_info.members[umem->member_index].type;
      continue;
    }

    const auto* idx = std::get_if<mir::IndexProjection>(&proj.info);
    if (idx == nullptr) {
      throw common::InternalError(
          "GetPlacePointer", "unsupported projection kind in LLVM backend");
    }

    const Type& cur_type = types_[current_type];
    if (cur_type.Kind() == TypeKind::kDynamicArray ||
        cur_type.Kind() == TypeKind::kQueue) {
      // Load the handle, call ElementPtr
      auto* ptr_ty = llvm::PointerType::getUnqual(*llvm_context_);
      llvm::Value* handle = builder_.CreateLoad(ptr_ty, ptr, "da.handle");
      llvm::Value* index = LowerOperand(*this, idx->index);
      index = builder_.CreateSExtOrTrunc(
          index, llvm::Type::getInt64Ty(*llvm_context_), "da.idx");
      ptr = builder_.CreateCall(
          GetLyraDynArrayElementPtr(), {handle, index}, "da.elem_ptr");
      current_type = (cur_type.Kind() == TypeKind::kQueue)
                         ? cur_type.AsQueue().element_type
                         : cur_type.AsDynamicArray().element_type;
    } else {
      // Unpacked array: GEP into fixed array
      llvm::Type* array_type = BuildLlvmTypeForTypeId(*this, current_type);
      llvm::Value* index = LowerOperand(*this, idx->index);
      ptr = builder_.CreateGEP(
          array_type, ptr, {builder_.getInt32(0), index}, "array_elem_ptr");
      current_type = cur_type.AsUnpackedArray().element_type;
    }
  }

  return ptr;
}

auto Context::GetPlaceLlvmType(mir::PlaceId place_id) -> llvm::Type* {
  const auto& place = arena_[place_id];

  TypeId type_id = mir::TypeOfPlace(types_, place);
  const Type& type = types_[type_id];

  if (type.Kind() == TypeKind::kIntegral) {
    uint32_t bit_width = type.AsIntegral().bit_width;
    if (type.AsIntegral().is_four_state) {
      return GetFourStateStructType(*llvm_context_, bit_width);
    }
    return GetIntegralStorageType(*llvm_context_, bit_width);
  }
  if (type.Kind() == TypeKind::kReal) {
    return llvm::Type::getDoubleTy(*llvm_context_);
  }
  if (type.Kind() == TypeKind::kShortReal) {
    return llvm::Type::getFloatTy(*llvm_context_);
  }
  if (type.Kind() == TypeKind::kString ||
      type.Kind() == TypeKind::kDynamicArray ||
      type.Kind() == TypeKind::kQueue) {
    return llvm::PointerType::getUnqual(*llvm_context_);
  }
  if (IsPacked(type)) {
    auto width = PackedBitWidth(type, types_);
    if (IsPackedFourState(type, types_)) {
      return GetFourStateStructType(*llvm_context_, width);
    }
    return GetIntegralStorageType(*llvm_context_, width);
  }

  if (type.Kind() == TypeKind::kUnpackedArray ||
      type.Kind() == TypeKind::kUnpackedStruct ||
      type.Kind() == TypeKind::kUnpackedUnion) {
    return BuildLlvmTypeForTypeId(*this, type_id);
  }

  throw common::UnsupportedErrorException(
      common::UnsupportedLayer::kMirToLlvm, common::UnsupportedKind::kType,
      current_origin_,
      std::format("type not yet supported: {}", ToString(type)));
}

auto Context::GetPlaceLlvmType4State(uint32_t bit_width) -> llvm::StructType* {
  return GetFourStateStructType(*llvm_context_, bit_width);
}

auto Context::HasBitRangeProjection(mir::PlaceId place_id) const -> bool {
  const auto& place = arena_[place_id];
  return !place.projections.empty() &&
         std::holds_alternative<mir::BitRangeProjection>(
             place.projections.back().info);
}

auto Context::GetBitRangeProjection(mir::PlaceId place_id) const
    -> const mir::BitRangeProjection& {
  const auto& place = arena_[place_id];
  return std::get<mir::BitRangeProjection>(place.projections.back().info);
}

auto Context::GetPlaceBaseType(mir::PlaceId place_id) -> llvm::Type* {
  const auto& place = arena_[place_id];

  // The base type is root.type after applying all non-BitRange projections.
  // BitRange is always the last projection, so just use root.type for the
  // common case (no intermediate projections before the BitRange).
  TypeId base_type_id = place.root.type;
  for (const auto& proj : place.projections) {
    if (std::holds_alternative<mir::BitRangeProjection>(proj.info)) {
      break;
    }
    const Type& t = types_[base_type_id];
    if (t.Kind() == TypeKind::kUnpackedArray) {
      base_type_id = t.AsUnpackedArray().element_type;
    } else if (t.Kind() == TypeKind::kDynamicArray) {
      base_type_id = t.AsDynamicArray().element_type;
    } else if (t.Kind() == TypeKind::kQueue) {
      base_type_id = t.AsQueue().element_type;
    } else if (t.Kind() == TypeKind::kUnpackedStruct) {
      const auto* field = std::get_if<mir::FieldProjection>(&proj.info);
      if (field != nullptr) {
        base_type_id = t.AsUnpackedStruct()
                           .fields[static_cast<size_t>(field->field_index)]
                           .type;
      }
    } else if (t.Kind() == TypeKind::kUnpackedUnion) {
      const auto* umem = std::get_if<mir::UnionMemberProjection>(&proj.info);
      if (umem != nullptr) {
        base_type_id = t.AsUnpackedUnion().members[umem->member_index].type;
      }
    }
  }

  return BuildLlvmTypeForTypeId(*this, base_type_id);
}

auto Context::ComposeBitRange(mir::PlaceId place_id) -> ComposedBitRange {
  const auto& place = arena_[place_id];

  // Invariant: BitRangeProjections must form a contiguous suffix.
  // Once we see a BitRange, no non-BitRange projections may follow.
  bool seen_bitrange = false;
  for (const auto& proj : place.projections) {
    bool is_bitrange =
        std::holds_alternative<mir::BitRangeProjection>(proj.info);
    if (seen_bitrange && !is_bitrange) {
      throw common::InternalError(
          "ComposeBitRange",
          "non-BitRange projection after BitRange violates suffix invariant");
    }
    seen_bitrange = seen_bitrange || is_bitrange;
  }

  // Canonicalize all offsets to i32 (MIR bit offsets are always i32).
  auto* offset_ty = llvm::Type::getInt32Ty(*llvm_context_);
  llvm::Value* total_offset = nullptr;
  uint32_t width = 0;
  for (const auto& proj : place.projections) {
    if (!std::holds_alternative<mir::BitRangeProjection>(proj.info)) {
      continue;
    }
    const auto& br = std::get<mir::BitRangeProjection>(proj.info);
    llvm::Value* br_offset = LowerOperand(*this, br.bit_offset);
    br_offset = builder_.CreateZExtOrTrunc(br_offset, offset_ty);
    if (total_offset == nullptr) {
      total_offset = br_offset;
    } else {
      total_offset = builder_.CreateAdd(total_offset, br_offset);
    }
    width = br.width;
  }
  if (total_offset == nullptr) {
    throw common::InternalError(
        "ComposeBitRange", "called on place with no BitRangeProjection");
  }
  return {.offset = total_offset, .width = width};
}

auto Context::TakeOwnership() -> std::pair<
    std::unique_ptr<llvm::LLVMContext>, std::unique_ptr<llvm::Module>> {
  return {std::move(llvm_context_), std::move(llvm_module_)};
}

void Context::RegisterOwnedTemp(llvm::Value* handle) {
  owned_temps_.push_back(handle);
}

void Context::ClearOwnedTemps() {
  owned_temps_.clear();
}

void Context::ReleaseOwnedTemps() {
  for (llvm::Value* temp : owned_temps_) {
    builder_.CreateCall(GetLyraStringRelease(), {temp});
  }
  owned_temps_.clear();
}

void Context::RegisterUserFunction(
    mir::FunctionId func_id, llvm::Function* llvm_func) {
  user_functions_[func_id] = llvm_func;
}

auto Context::GetUserFunction(mir::FunctionId func_id) const
    -> llvm::Function* {
  auto it = user_functions_.find(func_id);
  if (it == user_functions_.end()) {
    throw common::InternalError(
        "GetUserFunction",
        std::format("user function {} not found", func_id.value));
  }
  return it->second;
}

auto Context::HasUserFunction(mir::FunctionId func_id) const -> bool {
  return user_functions_.contains(func_id);
}

auto Context::BuildUserFunctionType(const mir::FunctionSignature& sig)
    -> llvm::FunctionType* {
  auto* ptr_ty = llvm::PointerType::getUnqual(*llvm_context_);

  // First two parameters are DesignState* and Engine*
  // DesignState* is for accessing design variables
  // Engine* is for write notification when modifying design state
  std::vector<llvm::Type*> param_types;
  param_types.push_back(ptr_ty);  // DesignState*
  param_types.push_back(ptr_ty);  // Engine*

  // Add parameter types from signature
  for (const auto& param : sig.params) {
    const Type& type = types_[param.type];
    llvm::Type* param_ty = nullptr;

    if (type.Kind() == TypeKind::kVoid) {
      throw common::InternalError(
          "BuildUserFunctionType", "void parameter type not allowed");
    }

    if (type.Kind() == TypeKind::kString ||
        type.Kind() == TypeKind::kDynamicArray ||
        type.Kind() == TypeKind::kQueue) {
      param_ty = ptr_ty;
    } else if (type.Kind() == TypeKind::kReal) {
      param_ty = llvm::Type::getDoubleTy(*llvm_context_);
    } else if (type.Kind() == TypeKind::kShortReal) {
      param_ty = llvm::Type::getFloatTy(*llvm_context_);
    } else if (IsPacked(type)) {
      auto width = PackedBitWidth(type, types_);
      if (IsPackedFourState(type, types_)) {
        param_ty = GetFourStateStructType(*llvm_context_, width);
      } else {
        param_ty = GetLlvmStorageType(*llvm_context_, width);
      }
    } else {
      throw common::UnsupportedErrorException(
          common::UnsupportedLayer::kMirToLlvm, common::UnsupportedKind::kType,
          current_origin_,
          std::format("unsupported parameter type: {}", ToString(type.Kind())));
    }

    param_types.push_back(param_ty);
  }

  // Return type from signature
  const Type& ret_type = types_[sig.return_type];
  llvm::Type* llvm_ret_type = nullptr;

  if (ret_type.Kind() == TypeKind::kVoid) {
    llvm_ret_type = llvm::Type::getVoidTy(*llvm_context_);
  } else if (
      ret_type.Kind() == TypeKind::kString ||
      ret_type.Kind() == TypeKind::kDynamicArray ||
      ret_type.Kind() == TypeKind::kQueue) {
    llvm_ret_type = ptr_ty;
  } else if (ret_type.Kind() == TypeKind::kReal) {
    llvm_ret_type = llvm::Type::getDoubleTy(*llvm_context_);
  } else if (ret_type.Kind() == TypeKind::kShortReal) {
    llvm_ret_type = llvm::Type::getFloatTy(*llvm_context_);
  } else if (IsPacked(ret_type)) {
    auto width = PackedBitWidth(ret_type, types_);
    if (IsPackedFourState(ret_type, types_)) {
      llvm_ret_type = GetFourStateStructType(*llvm_context_, width);
    } else {
      llvm_ret_type = GetLlvmStorageType(*llvm_context_, width);
    }
  } else {
    throw common::UnsupportedErrorException(
        common::UnsupportedLayer::kMirToLlvm, common::UnsupportedKind::kType,
        current_origin_,
        std::format("unsupported return type: {}", ToString(ret_type.Kind())));
  }

  return llvm::FunctionType::get(llvm_ret_type, param_types, false);
}

StatementScope::StatementScope(Context& ctx) : ctx_(ctx) {
  ctx_.ClearOwnedTemps();
}

StatementScope::~StatementScope() {
  ctx_.ReleaseOwnedTemps();
}

OriginScope::OriginScope(Context& ctx, common::OriginId origin)
    : ctx_(ctx),
      saved_origin_(common::OriginId::Invalid()),
      pushed_(origin.IsValid()) {
  if (pushed_) {
    saved_origin_ = ctx_.GetCurrentOrigin();
    ctx_.SetCurrentOrigin(origin);
  }
  // If Invalid: do nothing, preserve outer origin
}

OriginScope::~OriginScope() {
  if (pushed_) {
    ctx_.SetCurrentOrigin(saved_origin_);
  }
}

}  // namespace lyra::lowering::mir_to_llvm
