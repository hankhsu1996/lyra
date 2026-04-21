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
#include "lyra/mir/place.hpp"
#include "lyra/mir/rvalue.hpp"
#include "lyra/mir/statement.hpp"
#include "lyra/runtime/construction_program_abi.hpp"
#include "lyra/runtime/runtime_instance.hpp"

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

auto LowerCtorArgOperand(
    llvm::LLVMContext& ctx, llvm::IRBuilder<>& builder, const CuFacts& facts,
    const Layout::BodyRealizationInfo& info, const mir::ModuleBody& body,
    llvm::Value* this_inline_base, const mir::Operand& op, TypeId expected_type,
    const char* caller, std::span<llvm::Value* const> local_slot_allocas)
    -> llvm::Value* {
  const Type& expected_ty = (*facts.types)[expected_type];
  uint32_t expected_bits = PackedBitWidth(expected_ty, *facts.types);
  auto* backing_ty = GetBackingLlvmType(ctx, expected_bits);
  uint32_t storage_width = backing_ty->getIntegerBitWidth();

  if (const auto* constant = std::get_if<Constant>(&op.payload)) {
    const auto* ic = std::get_if<IntegralConstant>(&constant->value);
    if (ic == nullptr) {
      throw common::InternalError(caller, "non-integral constant operand");
    }
    llvm::APInt val_ap(expected_bits == 0 ? 1 : expected_bits, ic->value);
    return llvm::ConstantInt::get(
        backing_ty, val_ap.zextOrTrunc(storage_width));
  }

  if (op.kind == mir::Operand::Kind::kUse) {
    mir::PlaceId place_id = std::get<mir::PlaceId>(op.payload);
    const mir::Place& place = body.arena[place_id];
    if (!place.projections.empty()) {
      throw common::InternalError(caller, "projected place not supported");
    }
    auto* i8_ty = llvm::Type::getInt8Ty(ctx);
    auto* i64_ty = llvm::Type::getInt64Ty(ctx);
    if (place.root.kind == mir::PlaceRoot::Kind::kModuleSlot) {
      auto slot_id = static_cast<uint32_t>(place.root.id);
      uint64_t rel_offset = info.body_layout.inline_offsets[slot_id].value;
      llvm::Value* slot_ptr = builder.CreateInBoundsGEP(
          i8_ty, this_inline_base, llvm::ConstantInt::get(i64_ty, rel_offset),
          "ctor_arg_slot");
      return builder.CreateLoad(backing_ty, slot_ptr, "ctor_arg_load");
    }
    if (place.root.kind == mir::PlaceRoot::Kind::kLocal) {
      auto local_slot = static_cast<uint32_t>(place.root.id);
      if (local_slot >= local_slot_allocas.size() ||
          local_slot_allocas[local_slot] == nullptr) {
        throw common::InternalError(
            caller, "Use(kLocal) slot has no bound alloca");
      }
      return builder.CreateLoad(
          backing_ty, local_slot_allocas[local_slot], "ctor_formal_load");
    }
    throw common::InternalError(
        caller, "Use operand root must be kModuleSlot or kLocal");
  }

  throw common::InternalError(
      caller, "unsupported constructor-argument operand");
}

void EmitOneBodyConstructor(
    llvm::LLVMContext& ctx, llvm::Module& mod, const CuFacts& facts,
    const Layout& layout, llvm::Function& fn, uint32_t body_index,
    const mir::ModuleBody& body, llvm::Constant* body_ref_table_ptr,
    const BodyConstructorRuntimeFuncs& rt,
    const ConstructionProgramData& prog) {
  auto* entry_bb = llvm::BasicBlock::Create(ctx, "entry", &fn);
  llvm::IRBuilder<> builder(entry_bb);

  auto site_count = CountNewObjectSites(body.constructor);
  std::vector<uint32_t> child_indices;
  if (site_count > 0) {
    child_indices = CollectFlaggedBodyChildEntries(prog, body_index);
    if (site_count != child_indices.size()) {
      throw common::InternalError(
          "EmitBodyConstructorFunctions",
          std::format(
              "body_group {} new_object count {} != construction-program "
              "child count {}",
              body_index, site_count, child_indices.size()));
    }
  }

  auto* ctor_arg = fn.getArg(0);
  // Second arg is the owning instance: the RuntimeInstance* this body is
  // being constructed for. Used for two distinct purposes below:
  //   (a) as the base for the inline_base load, which resolves to this
  //       body's inline storage region (used by slot stores), and
  //   (b) as the base for reaching the embedded RuntimeScope, which is
  //       passed as `parent_scope` to the runtime when allocating
  //       child objects.
  // These two pointers (this_inline_base, this_scope) are distinct
  // despite both being derived from the same RuntimeInstance*; they
  // must not be treated as interchangeable downstream.
  auto* this_instance = fn.getArg(1);
  auto path_pool_span = std::span<const uint8_t>(prog.path_pool);
  auto* i32_ty = llvm::Type::getInt32Ty(ctx);
  auto* i64_ty = llvm::Type::getInt64Ty(ctx);
  auto* ptr_ty = llvm::PointerType::getUnqual(ctx);
  auto* i8_ty = llvm::Type::getInt8Ty(ctx);

  // (a) this_inline_base: byte pointer to the start of this instance's
  // inline storage region. All body-local slot stores below use it as
  // the base for relative-offset GEPs. Obtained by typed struct GEP
  // through RuntimeInstance::storage::inline_base.
  auto* storage_ptr = builder.CreateStructGEP(
      layout.runtime_instance_type, this_instance,
      static_cast<unsigned>(runtime::RuntimeInstanceField::kStorage),
      "this_storage");
  auto* inline_base_field_ptr = builder.CreateStructGEP(
      layout.runtime_instance_storage_type, storage_ptr,
      static_cast<unsigned>(runtime::RuntimeInstanceStorageField::kInlineBase),
      "this_inline_base_field");
  auto* this_inline_base =
      builder.CreateLoad(ptr_ty, inline_base_field_ptr, "this_inline_base");

  // (b) this_scope: pointer to the RuntimeScope embedded in this
  // RuntimeInstance. Used as `parent_scope` when allocating children.
  // RuntimeScope sits past the binary-contract prefix covered by
  // runtime_instance_type; reach it via byte-GEP at the C++-authoritative
  // offsetof (same pattern as ResolveDpiCallerScope in dpi_abi.cpp).
  auto* this_scope = builder.CreateInBoundsGEP(
      i8_ty, this_instance,
      llvm::ConstantInt::get(i64_ty, offsetof(runtime::RuntimeInstance, scope)),
      "this_scope");

  // Allocate one alloca per formal (scope-local kLocal in MIR) and seed
  // it from the LLVM function argument. Body PlainAssigns that move a
  // formal into a body-owned member read from these allocas via the
  // ordinary Use(kLocal) operand path -- no side table.
  const auto& ctor_sig = body.constructor.signature;
  const auto& info = layout.body_realization_infos[body_index];
  std::vector<llvm::Value*> local_slot_allocas(
      body.constructor.local_types.size(), nullptr);
  for (uint32_t i = 0; i < ctor_sig.params.size(); ++i) {
    uint32_t local_slot = body.constructor.param_local_slots[i];
    TypeId formal_type = ctor_sig.params[i].type;
    const Type& ty = (*facts.types)[formal_type];
    uint32_t bit_width = PackedBitWidth(ty, *facts.types);
    auto* backing_ty = GetBackingLlvmType(ctx, bit_width);
    auto* alloca = builder.CreateAlloca(
        backing_ty, nullptr, std::format("formal{}_local{}", i, local_slot));
    llvm::Value* formal = fn.getArg(2 + i);
    builder.CreateStore(formal, alloca);
    local_slot_allocas[local_slot] = alloca;
  }

  // Three-step child creation per NewObject site:
  //   obj = LyraConstructorAllocateObject(ctor, this_scope, ...);
  //   __lyra_body_construct_<child>(ctor, obj, typed_args...);
  //   store obj, GEP(this_ptr, inline_offsets[child_handle_slot]);
  //
  // The destination kChildHandle slot is read from the PlainAssign's
  // dest PlaceId (the MIR assignment for `this.child = new_object(...)`).
  // No implicit ordering contract between site index and slot position.

  uint32_t site_idx = 0;
  for (const auto& block : body.constructor.blocks) {
    for (const auto& stmt : block.statements) {
      const auto* assign = std::get_if<mir::PlainAssign>(&stmt.data);
      if (assign == nullptr) {
        throw common::InternalError(
            "EmitBodyConstructorFunctions",
            "constructor body contains a non-PlainAssign statement");
      }
      mir::PlaceId dest_place_id =
          mir::RequireLocalDest(assign->dest, "EmitBodyConstructorFunctions");
      const mir::Place& dest_place = body.arena[dest_place_id];
      if (dest_place.root.kind != mir::PlaceRoot::Kind::kModuleSlot ||
          !dest_place.projections.empty()) {
        throw common::InternalError(
            "EmitBodyConstructorFunctions",
            std::format(
                "body_group {} PlainAssign dest must be a bare kModuleSlot "
                "(got kind={}, projections={})",
                body_index, static_cast<int>(dest_place.root.kind),
                dest_place.projections.size()));
      }
      auto dest_slot_id = static_cast<uint32_t>(dest_place.root.id);
      uint64_t dest_rel_offset =
          info.body_layout.inline_offsets[dest_slot_id].value;

      // Case A: simple operand copy, e.g. formal-to-member binding
      // emitted at the top of the body. RHS is an Operand read from a
      // local alloca or a constant; destination is an ordinary body
      // module slot.
      if (const auto* op = std::get_if<mir::Operand>(&assign->rhs)) {
        llvm::Value* value = LowerCtorArgOperand(
            ctx, builder, facts, info, body, this_inline_base, *op,
            body.slots[dest_slot_id].type,
            "EmitBodyConstructorFunctions (member bind)", local_slot_allocas);
        llvm::Value* slot_ptr = builder.CreateInBoundsGEP(
            i8_ty, this_inline_base,
            llvm::ConstantInt::get(i64_ty, dest_rel_offset),
            std::format(
                "__lyra_body_ctor_{}_member{}_slot", body_index, dest_slot_id));
        builder.CreateStore(value, slot_ptr);
        continue;
      }

      const auto* rv = std::get_if<mir::Rvalue>(&assign->rhs);
      if (rv == nullptr ||
          !std::holds_alternative<mir::NewObjectRvalueInfo>(rv->info)) {
        throw common::InternalError(
            "EmitBodyConstructorFunctions",
            "constructor PlainAssign rhs is neither simple operand nor "
            "NewObject rvalue");
      }

      if ((*facts.types)[body.slots[dest_slot_id].type].Kind() !=
          TypeKind::kObjectHandle) {
        throw common::InternalError(
            "EmitBodyConstructorFunctions",
            std::format(
                "body_group {} NewObject dest slot {} is not an object-handle "
                "member",
                body_index, dest_slot_id));
      }

      const auto& entry = prog.entries[child_indices[site_idx]];
      auto* body_ref_ptr =
          ComputeBodyRefPtr(ctx, body_ref_table_ptr, entry.body_group);
      builder.CreateCall(rt.begin_body_by_ref, {ctor_arg, body_ref_ptr});

      auto label = ReadLabel(
          path_pool_span, entry.label_offset, "EmitBodyConstructorFunctions");
      auto label_global_name =
          std::format("__lyra_body_ctor_{}_label_{}", body_index, site_idx);
      auto* label_ptr = EmitCString(ctx, mod, label, label_global_name);

      // Step 1a: allocate pure object storage.
      auto* child_object = builder.CreateCall(
          rt.allocate_object,
          {ctor_arg, llvm::ConstantInt::get(i64_ty, entry.realized_inline_size),
           llvm::ConstantInt::get(i64_ty, entry.realized_appendix_size)});
      // Step 1b: attach to parent scope (identity, edge, path).
      builder.CreateCall(
          rt.attach_instance_to_scope,
          {ctor_arg, child_object, this_scope,
           llvm::ConstantInt::get(i32_ty, entry.ordinal_in_parent), label_ptr});
      // Step 1c: apply body storage init recipe.
      builder.CreateCall(rt.apply_body_init, {ctor_arg, child_object});
      // Step 1d: commit the instance to the staged tables.
      auto* child_instance = builder.CreateCall(
          rt.finalize_instance,
          {ctor_arg, child_object,
           llvm::ConstantInt::get(i32_ty, entry.instance_index)});

      // Step 2: direct LLVM call to the child body-constructor with
      // typed transmitted-parameter arguments from the NewObject
      // rvalue's operand vector.
      auto child_fn_name =
          std::format("__lyra_body_construct_{}", entry.body_group);
      auto* child_fn = mod.getFunction(child_fn_name);
      if (child_fn == nullptr) {
        throw common::InternalError(
            "EmitBodyConstructorFunctions",
            std::format(
                "child body-constructor {} is not declared", child_fn_name));
      }
      const auto& child_info = layout.body_realization_infos[entry.body_group];
      const auto& child_params = child_info.body->constructor.signature.params;
      if (rv->operands.size() != child_params.size()) {
        throw common::InternalError(
            "EmitBodyConstructorFunctions",
            std::format(
                "NewObject operand count {} != child body {} formal count {}",
                rv->operands.size(), entry.body_group, child_params.size()));
      }
      std::vector<llvm::Value*> child_args;
      child_args.reserve(2 + rv->operands.size());
      child_args.push_back(ctor_arg);
      child_args.push_back(child_instance);
      for (uint32_t ai = 0; ai < rv->operands.size(); ++ai) {
        child_args.push_back(LowerCtorArgOperand(
            ctx, builder, facts, info, body, this_inline_base, rv->operands[ai],
            child_params[ai].type, "EmitBodyConstructorFunctions",
            local_slot_allocas));
      }
      builder.CreateCall(child_fn, child_args);

      // Step 3: store the child handle into the MIR-targeted kChildHandle
      // slot at its parent-relative inline offset.
      uint64_t child_slot_offset =
          info.body_layout.inline_offsets[dest_slot_id].value;
      llvm::Value* handle_slot_ptr = builder.CreateInBoundsGEP(
          i8_ty, this_inline_base,
          llvm::ConstantInt::get(i64_ty, child_slot_offset),
          std::format(
              "__lyra_body_ctor_{}_child{}_slot", body_index, site_idx));
      builder.CreateStore(child_instance, handle_slot_ptr);

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
        ctx, mod, facts, layout, *fn, static_cast<uint32_t>(bi), *info.body,
        body_ref_table_ptr, helpers, prog);
  }
}

}  // namespace lyra::lowering::mir_to_llvm
