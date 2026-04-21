#include "lyra/llvm_backend/emit_body_constructor.hpp"

#include <cstdint>
#include <format>
#include <span>
#include <variant>
#include <vector>

#include <llvm/ADT/APInt.h>
#include <llvm/IR/BasicBlock.h>
#include <llvm/IR/Constant.h>
#include <llvm/IR/Constants.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/Module.h>

#include "lyra/common/constant.hpp"
#include "lyra/common/integral_constant.hpp"
#include "lyra/common/internal_error.hpp"
#include "lyra/common/type.hpp"
#include "lyra/common/type_queries.hpp"
#include "lyra/llvm_backend/value_repr.hpp"
#include "lyra/mir/basic_block.hpp"
#include "lyra/mir/module_body.hpp"
#include "lyra/mir/operand.hpp"
#include "lyra/mir/rvalue.hpp"
#include "lyra/mir/statement.hpp"
#include "lyra/runtime/construction_program_abi.hpp"

namespace lyra::lowering::mir_to_llvm {

namespace {

// Emit a private string global for `text` (NUL-terminated) and return a
// GEP to its first byte. Each new_object site gets its own label global;
// sharing via path_pool is not needed for the direct-constructor path.
auto EmitCString(
    llvm::LLVMContext& ctx, llvm::Module& mod, std::string_view text,
    const std::string& global_name) -> llvm::Constant* {
  auto* i8_ty = llvm::Type::getInt8Ty(ctx);
  auto* array_ty = llvm::ArrayType::get(i8_ty, text.size() + 1);
  std::vector<llvm::Constant*> bytes;
  bytes.reserve(text.size() + 1);
  for (char c : text) {
    bytes.push_back(llvm::ConstantInt::get(i8_ty, static_cast<uint8_t>(c)));
  }
  bytes.push_back(llvm::ConstantInt::get(i8_ty, 0));
  auto* init = llvm::ConstantArray::get(array_ty, bytes);
  auto* global = new llvm::GlobalVariable(
      mod, array_ty, /*isConstant=*/true, llvm::GlobalValue::PrivateLinkage,
      init, global_name);
  global->setUnnamedAddr(llvm::GlobalValue::UnnamedAddr::Global);
  auto* i32_ty = llvm::Type::getInt32Ty(ctx);
  auto* zero = llvm::ConstantInt::get(i32_ty, 0);
  return llvm::ConstantExpr::getInBoundsGetElementPtr(
      array_ty, global, llvm::ArrayRef<llvm::Constant*>{zero, zero});
}

// Compute `&body_ref_table[body_group]` as a constant GEP. The table is
// an array of BodyDescriptorRef; advancing by one index yields a pointer
// to the next BodyDescriptorRef element.
auto ComputeBodyRefPtr(
    llvm::LLVMContext& ctx, llvm::Constant* body_ref_table_ptr,
    uint32_t body_group) -> llvm::Constant* {
  auto* ref_ty = GetBodyDescriptorRefType(ctx);
  auto* i32_ty = llvm::Type::getInt32Ty(ctx);
  return llvm::ConstantExpr::getInBoundsGetElementPtr(
      ref_ty, body_ref_table_ptr,
      llvm::ArrayRef<llvm::Constant*>{
          llvm::ConstantInt::get(i32_ty, body_group)});
}

// Count the direct NewObject sites in a body's constructor. Invariant
// check: the detection helper in hir_to_mir/module.cpp restricts the
// constructor shape to a single block of NewObject-producing PlainAssign
// statements, so a simple linear count is enough.
auto CountNewObjectSites(const mir::Constructor& ctor) -> uint32_t {
  uint32_t count = 0;
  for (const auto& block : ctor.blocks) {
    for (const auto& stmt : block.statements) {
      const auto* assign = std::get_if<mir::PlainAssign>(&stmt.data);
      if (assign == nullptr) continue;
      const auto* rv = std::get_if<mir::Rvalue>(&assign->rhs);
      if (rv == nullptr) continue;
      if (std::holds_alternative<mir::NewObjectRvalueInfo>(rv->info)) {
        ++count;
      }
    }
  }
  return count;
}

// Collect the construction-program entries representing children of the
// body at `parent_body_group`. For cut 3, each flagged body has exactly
// one compile-time instance in the design; the returned entries are the
// children of that instance, in hierarchy-program order.
auto CollectFlaggedBodyChildEntries(
    const ConstructionProgramData& prog, uint32_t parent_body_group)
    -> std::vector<uint32_t> {
  // Find the unique entry that represents an instance of this body.
  uint32_t parent_entry_index = UINT32_MAX;
  uint32_t instance_count = 0;
  for (uint32_t i = 0; i < prog.entries.size(); ++i) {
    const auto& e = prog.entries[i];
    if (e.node_kind == runtime::ConstructionNodeKind::kInstance &&
        e.body_group == parent_body_group) {
      parent_entry_index = i;
      ++instance_count;
    }
  }
  if (instance_count == 0) {
    throw common::InternalError(
        "EmitBodyConstructorFunctions",
        std::format(
            "flagged body_group {} has no construction-program instance",
            parent_body_group));
  }
  if (instance_count > 1) {
    // Cut 3 scope: multi-instance flagged bodies would need per-instance
    // instance_index resolution. Fail loud rather than silently producing
    // duplicated construction.
    throw common::InternalError(
        "EmitBodyConstructorFunctions",
        std::format(
            "flagged body_group {} has {} construction-program instances; "
            "cut-3 direct-constructor path only supports single-instance "
            "bodies",
            parent_body_group, instance_count));
  }

  std::vector<uint32_t> child_entry_indices;
  for (uint32_t i = 0; i < prog.entries.size(); ++i) {
    const auto& e = prog.entries[i];
    if (e.node_kind != runtime::ConstructionNodeKind::kInstance) continue;
    if (e.parent_scope_index == parent_entry_index) {
      child_entry_indices.push_back(i);
    }
  }
  return child_entry_indices;
}

auto ReadLabel(
    std::span<const uint8_t> path_pool, uint32_t label_offset,
    const char* caller) -> std::string {
  if (label_offset >= path_pool.size()) {
    throw common::InternalError(
        caller, std::format(
                    "label_offset {} out of path_pool size {}", label_offset,
                    path_pool.size()));
  }
  auto remaining = path_pool.subspan(label_offset);
  std::string out;
  for (uint8_t b : remaining) {
    if (b == 0) break;
    out.push_back(static_cast<char>(b));
  }
  return out;
}

// Build a backing-width LLVM ConstantInt from a MIR operand. Cut-4
// scope restricts transmitted-parameter operands to IntegralConstant
// values; anything else is a compiler bug at this stage.
auto LowerConstantArg(
    llvm::LLVMContext& ctx, const CuFacts& facts, const mir::Operand& op,
    const char* caller) -> llvm::Constant* {
  const auto* constant = std::get_if<Constant>(&op.payload);
  if (constant == nullptr) {
    throw common::InternalError(
        caller,
        "transmitted constructor argument must be a MIR Constant operand");
  }
  const auto* ic = std::get_if<IntegralConstant>(&constant->value);
  if (ic == nullptr) {
    throw common::InternalError(
        caller,
        "transmitted constructor argument must be an integral constant");
  }
  const Type& type = (*facts.types)[constant->type];
  uint32_t bit_width = PackedBitWidth(type, *facts.types);
  auto* backing_ty = GetBackingLlvmType(ctx, bit_width);
  uint32_t storage_width = backing_ty->getIntegerBitWidth();
  llvm::APInt val_ap(bit_width == 0 ? 1 : bit_width, ic->value);
  return llvm::ConstantInt::get(backing_ty, val_ap.zextOrTrunc(storage_width));
}

void EmitOneBodyConstructor(
    llvm::LLVMContext& ctx, llvm::Module& mod, const CuFacts& facts,
    llvm::Function& fn, uint32_t body_index, const mir::ModuleBody& body,
    llvm::Constant* body_ref_table_ptr, const BodyConstructorRuntimeFuncs& rt,
    const ConstructionProgramData& prog) {
  auto* entry_bb = llvm::BasicBlock::Create(ctx, "entry", &fn);
  llvm::IRBuilder<> builder(entry_bb);

  auto site_count = CountNewObjectSites(body.constructor);
  auto child_indices = CollectFlaggedBodyChildEntries(prog, body_index);
  if (site_count != child_indices.size()) {
    throw common::InternalError(
        "EmitBodyConstructorFunctions",
        std::format(
            "body_group {} new_object count {} != construction-program "
            "child count {}",
            body_index, site_count, child_indices.size()));
  }

  auto* ctor_arg = fn.getArg(0);
  auto* parent_scope_arg = fn.getArg(1);
  auto path_pool_span = std::span<const uint8_t>(prog.path_pool);
  auto* i32_ty = llvm::Type::getInt32Ty(ctx);
  auto* i64_ty = llvm::Type::getInt64Ty(ctx);
  auto* ptr_ty = llvm::PointerType::getUnqual(ctx);
  const auto& dl = mod.getDataLayout();
  auto* null_ptr = llvm::ConstantPointerNull::get(ptr_ty);

  uint32_t site_idx = 0;
  for (const auto& block : body.constructor.blocks) {
    for (const auto& stmt : block.statements) {
      const auto* assign = std::get_if<mir::PlainAssign>(&stmt.data);
      if (assign == nullptr) continue;
      const auto* rv = std::get_if<mir::Rvalue>(&assign->rhs);
      if (rv == nullptr) continue;
      if (!std::holds_alternative<mir::NewObjectRvalueInfo>(rv->info)) {
        continue;
      }

      const auto& entry = prog.entries[child_indices[site_idx]];
      auto* body_ref_ptr =
          ComputeBodyRefPtr(ctx, body_ref_table_ptr, entry.body_group);
      builder.CreateCall(rt.begin_body_by_ref, {ctor_arg, body_ref_ptr});

      // Marshal transmitted constructor arguments directly from MIR
      // operands. Each operand becomes a stack-local typed value; the
      // per-site arg_ptrs and arg_byte_sizes arrays are likewise
      // stack-allocated. No compile-time byte pool is consulted.
      auto num_args = static_cast<uint32_t>(rv->operands.size());
      llvm::Value* arg_ptrs_v = null_ptr;
      llvm::Value* arg_sizes_v = null_ptr;
      if (num_args > 0) {
        auto* ptrs_arr_ty = llvm::ArrayType::get(ptr_ty, num_args);
        auto* sizes_arr_ty = llvm::ArrayType::get(i32_ty, num_args);
        auto* ptrs_arr = builder.CreateAlloca(
            ptrs_arr_ty, nullptr,
            std::format(
                "__lyra_body_ctor_{}_site_{}_argptrs", body_index, site_idx));
        auto* sizes_arr = builder.CreateAlloca(
            sizes_arr_ty, nullptr,
            std::format(
                "__lyra_body_ctor_{}_site_{}_argsizes", body_index, site_idx));

        for (uint32_t ai = 0; ai < num_args; ++ai) {
          auto* arg_const = LowerConstantArg(
              ctx, facts, rv->operands[ai], "EmitBodyConstructorFunctions");
          auto* arg_ty = arg_const->getType();
          auto* arg_alloca = builder.CreateAlloca(
              arg_ty, nullptr,
              std::format(
                  "__lyra_body_ctor_{}_site_{}_arg{}", body_index, site_idx,
                  ai));
          builder.CreateStore(arg_const, arg_alloca);

          auto byte_size = dl.getTypeAllocSize(arg_ty);
          auto* ptr_slot =
              builder.CreateConstInBoundsGEP2_32(ptrs_arr_ty, ptrs_arr, 0, ai);
          builder.CreateStore(arg_alloca, ptr_slot);
          auto* size_slot = builder.CreateConstInBoundsGEP2_32(
              sizes_arr_ty, sizes_arr, 0, ai);
          builder.CreateStore(
              llvm::ConstantInt::get(i32_ty, static_cast<uint64_t>(byte_size)),
              size_slot);
        }

        arg_ptrs_v = ptrs_arr;
        arg_sizes_v = sizes_arr;
      }

      auto label = ReadLabel(
          path_pool_span, entry.label_offset, "EmitBodyConstructorFunctions");
      auto label_global_name =
          std::format("__lyra_body_ctor_{}_label_{}", body_index, site_idx);
      auto* label_ptr = EmitCString(ctx, mod, label, label_global_name);

      builder.CreateCall(
          rt.add_child_object,
          {ctor_arg, parent_scope_arg,
           llvm::ConstantInt::get(i32_ty, entry.ordinal_in_parent),
           llvm::ConstantInt::get(i32_ty, entry.instance_index), label_ptr,
           llvm::ConstantInt::get(i64_ty, entry.realized_inline_size),
           llvm::ConstantInt::get(i64_ty, entry.realized_appendix_size),
           llvm::ConstantInt::get(i32_ty, num_args), arg_ptrs_v, arg_sizes_v});

      ++site_idx;
    }
  }

  builder.CreateRetVoid();
}

}  // namespace

void EmitBodyConstructorFunctions(
    Context& context, const CuFacts& facts, const Layout& layout,
    const std::vector<BodyDescriptorPackageEmission>& body_descs,
    llvm::Constant* body_ref_table_ptr, const ConstructionProgramData& prog,
    std::span<const uint8_t> effective_uses_mir_ctor) {
  auto& ctx = context.GetLlvmContext();
  auto& mod = context.GetModule();

  if (body_descs.size() != layout.body_realization_infos.size()) {
    throw common::InternalError(
        "EmitBodyConstructorFunctions",
        std::format(
            "body_descs size {} != body_realization_infos size {}",
            body_descs.size(), layout.body_realization_infos.size()));
  }

  auto helpers = DeclareBodyConstructorRuntimeFuncs(ctx, mod);

  for (size_t bi = 0; bi < layout.body_realization_infos.size(); ++bi) {
    const auto& info = layout.body_realization_infos[bi];
    if (info.body == nullptr) continue;
    bool eff_flag =
        bi < effective_uses_mir_ctor.size() && effective_uses_mir_ctor[bi] != 0;
    if (!eff_flag) continue;

    auto fn_name = std::format("__lyra_body_construct_{}", bi);
    auto* fn = mod.getFunction(fn_name);
    if (fn == nullptr) {
      throw common::InternalError(
          "EmitBodyConstructorFunctions",
          std::format("missing forward declaration for {}", fn_name));
    }
    if (!fn->empty()) {
      throw common::InternalError(
          "EmitBodyConstructorFunctions",
          std::format("body-constructor {} already has a body", fn_name));
    }

    EmitOneBodyConstructor(
        ctx, mod, facts, *fn, static_cast<uint32_t>(bi), *info.body,
        body_ref_table_ptr, helpers, prog);
  }
}

}  // namespace lyra::lowering::mir_to_llvm
