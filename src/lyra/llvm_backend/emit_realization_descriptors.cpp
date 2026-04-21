#include "lyra/llvm_backend/emit_realization_descriptors.hpp"

#include <cstddef>
#include <cstdint>
#include <format>
#include <span>
#include <string>
#include <vector>

#include <llvm/IR/BasicBlock.h>
#include <llvm/IR/Constant.h>
#include <llvm/IR/Constants.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/Module.h>

#include "lyra/common/internal_error.hpp"
#include "lyra/common/type.hpp"
#include "lyra/llvm_backend/context.hpp"
#include "lyra/llvm_backend/cu_facts.hpp"
#include "lyra/llvm_backend/emit_body_constructor.hpp"
#include "lyra/llvm_backend/emit_descriptor_utils.hpp"
#include "lyra/llvm_backend/layout/layout.hpp"
#include "lyra/llvm_backend/type_ops/default_init.hpp"
#include "lyra/llvm_backend/type_ops/four_state_init.hpp"

namespace lyra::lowering::mir_to_llvm {

namespace {

// Internal constructor result. Uses the public result's nested types
// directly; the orchestration function lifts this into the public
// RealizationEmissionResult.
struct ConstructorEmissionResult {
  llvm::Value* states_array = nullptr;
  llvm::Value* num_total = nullptr;
  llvm::Value* result_handle = nullptr;
  llvm::Function* destroy_fn = nullptr;
  RealizationEmissionResult::ProcessMeta process_meta;
  RealizationEmissionResult::DispatchMeta dispatch_meta;
  RealizationEmissionResult::ObservationMeta observation_meta;
};

auto GetProcessStateSchemaType(llvm::LLVMContext& ctx) -> llvm::StructType* {
  auto* i64_ty = llvm::Type::getInt64Ty(ctx);
  auto* ptr_ty = llvm::PointerType::getUnqual(ctx);
  return llvm::StructType::get(ctx, {i64_ty, i64_ty, ptr_ty});
}

auto GetConstructionProgramEntryType(llvm::LLVMContext& ctx)
    -> llvm::StructType* {
  auto* i32_ty = llvm::Type::getInt32Ty(ctx);
  auto* i64_ty = llvm::Type::getInt64Ty(ctx);
  // Must match ConstructionProgramEntry field order:
  // node_kind(i32), body_group(i32), label_offset(i32),
  // param_offset(i32), param_size(i32), parent_scope_index(i32),
  // ordinal_in_parent(i32), instance_index(i32),
  // realized_inline_size(i64), realized_appendix_size(i64),
  // port_const_init_offset(i32), port_const_init_count(i32)
  return llvm::StructType::get(
      ctx, {i32_ty, i32_ty, i32_ty, i32_ty, i32_ty, i32_ty, i32_ty, i32_ty,
            i64_ty, i64_ty, i32_ty, i32_ty});
}

// Encode one ConstructionProgramEntry as an LLVM constant.
auto EncodeConstructionProgramEntry(
    llvm::StructType* ty, llvm::LLVMContext& ctx,
    const runtime::ConstructionProgramEntry& e) -> llvm::Constant* {
  auto* i32_ty = llvm::Type::getInt32Ty(ctx);
  auto* i64_ty = llvm::Type::getInt64Ty(ctx);
  return llvm::ConstantStruct::get(
      ty, {llvm::ConstantInt::get(i32_ty, static_cast<uint32_t>(e.node_kind)),
           llvm::ConstantInt::get(i32_ty, e.body_group),
           llvm::ConstantInt::get(i32_ty, e.label_offset),
           llvm::ConstantInt::get(i32_ty, e.param_offset),
           llvm::ConstantInt::get(i32_ty, e.param_size),
           llvm::ConstantInt::get(i32_ty, e.parent_scope_index),
           llvm::ConstantInt::get(i32_ty, e.ordinal_in_parent),
           llvm::ConstantInt::get(i32_ty, e.instance_index),
           llvm::ConstantInt::get(i64_ty, e.realized_inline_size),
           llvm::ConstantInt::get(i64_ty, e.realized_appendix_size),
           llvm::ConstantInt::get(i32_ty, e.port_const_init_offset),
           llvm::ConstantInt::get(i32_ty, e.port_const_init_count)});
}

// Encode one BodyDescriptorPackageEmission as a BodyDescriptorRef constant.
auto EncodeBodyDescriptorRef(
    llvm::StructType* ty, llvm::LLVMContext& ctx,
    const BodyDescriptorPackageEmission& pkg) -> llvm::Constant* {
  auto* i32_ty = llvm::Type::getInt32Ty(ctx);
  return llvm::ConstantStruct::get(
      ty, {pkg.header_ptr,
           pkg.entries_ptr,
           llvm::ConstantInt::get(i32_ty, pkg.num_processes),
           pkg.meta.entries_ptr,
           llvm::ConstantInt::get(i32_ty, pkg.meta.num_entries),
           pkg.meta.pool_ptr,
           llvm::ConstantInt::get(i32_ty, pkg.meta.pool_size),
           pkg.triggers.entries_ptr,
           llvm::ConstantInt::get(i32_ty, pkg.triggers.num_entries),
           pkg.triggers.ranges_ptr,
           llvm::ConstantInt::get(i32_ty, pkg.triggers.num_ranges),
           pkg.triggers.shapes_ptr,
           pkg.triggers.groupable_ptr,
           pkg.comb_entries_ptr,
           llvm::ConstantInt::get(i32_ty, pkg.num_comb_entries),
           pkg.comb_kernels_ptr,
           llvm::ConstantInt::get(i32_ty, pkg.num_comb_kernels),
           pkg.observable.entries_ptr,
           llvm::ConstantInt::get(i32_ty, pkg.observable.num_entries),
           pkg.observable.pool_ptr,
           llvm::ConstantInt::get(i32_ty, pkg.observable.pool_size),
           pkg.init.recipe_ptr,
           llvm::ConstantInt::get(i32_ty, pkg.init.num_recipe_ops),
           pkg.init.recipe_roots_ptr,
           llvm::ConstantInt::get(i32_ty, pkg.init.num_recipe_roots),
           pkg.init.recipe_child_indices_ptr,
           llvm::ConstantInt::get(i32_ty, pkg.init.num_recipe_child_indices),
           pkg.init.param_slots_ptr,
           llvm::ConstantInt::get(i32_ty, pkg.init.num_param_slots),
           pkg.decision_tables_ptr,
           llvm::ConstantInt::get(i32_ty, pkg.num_decision_tables)});
}

auto EmitPerSchemaFrameInitFunctions(
    Context& context, const CuFacts& facts, const Layout& layout)
    -> std::vector<llvm::Function*> {
  auto& ctx = context.GetLlvmContext();
  auto& mod = context.GetModule();
  auto* ptr_ty = llvm::PointerType::getUnqual(ctx);
  bool force_two_state = facts.force_two_state;

  std::vector<llvm::Function*> init_fns(layout.state_schemas.size(), nullptr);

  for (size_t si = 0; si < layout.state_schemas.size(); ++si) {
    const auto& schema = layout.state_schemas[si];
    if (!schema.needs_4state_init) continue;

    if (schema.representative_process_index >= layout.processes.size()) {
      throw common::InternalError(
          "EmitPerSchemaFrameInitFunctions",
          "representative_process_index out of range");
    }

    // Generate function name from schema identity.
    std::string fn_name;
    if (schema.proc_within_body.has_value()) {
      fn_name =
          std::format("schema_s{}_{}_frame_init", si, *schema.proc_within_body);
    } else {
      fn_name = std::format("conn_{}_frame_init", *schema.conn_index);
    }

    auto* fn_type =
        llvm::FunctionType::get(llvm::Type::getVoidTy(ctx), {ptr_ty}, false);
    auto* fn = llvm::Function::Create(
        fn_type, llvm::Function::InternalLinkage, fn_name, &mod);

    // Save current builder state.
    auto* saved_block = context.GetBuilder().GetInsertBlock();
    auto saved_point = context.GetBuilder().GetInsertPoint();

    auto* entry = llvm::BasicBlock::Create(ctx, "entry", fn);
    context.GetBuilder().SetInsertPoint(entry);

    auto* state_ptr = fn->getArg(0);
    const auto& proc_layout =
        layout.processes[schema.representative_process_index];
    auto* state_type = proc_layout.state_type;
    auto* frame_ptr =
        context.GetBuilder().CreateStructGEP(state_type, state_ptr, 1);

    // Apply scalar 4-state patches.
    if (!proc_layout.frame.four_state_patches.IsEmpty()) {
      EmitApply4StatePatches(
          context, frame_ptr, proc_layout.frame.four_state_patches);
    }

    // Apply composite 4-state initialization.
    auto* frame_type = proc_layout.frame.llvm_type;
    const auto& types = *facts.types;
    for (uint32_t fi = 0; fi < proc_layout.frame.field_types.size(); ++fi) {
      TypeId type_id = proc_layout.frame.field_types[fi];
      if (!IsFourState(facts, type_id)) continue;
      if (IsScalarPatchable(type_id, types, force_two_state)) continue;
      auto* field_ptr =
          context.GetBuilder().CreateStructGEP(frame_type, frame_ptr, fi);
      EmitSVDefaultInitAfterZero(context, facts, field_ptr, type_id);
    }

    context.GetBuilder().CreateRetVoid();

    // Restore builder state.
    if (saved_block != nullptr) {
      context.GetBuilder().SetInsertPoint(saved_block, saved_point);
    }

    init_fns[si] = fn;
  }

  return init_fns;
}

// Emit __lyra_state_schemas: global constant array of ProcessStateSchema.
auto EmitProcessStateSchemas(
    Context& context, const Layout& layout,
    const std::vector<llvm::Function*>& init_fns) -> llvm::Constant* {
  auto& ctx = context.GetLlvmContext();
  auto& mod = context.GetModule();
  auto* i32_ty = llvm::Type::getInt32Ty(ctx);
  auto* i64_ty = llvm::Type::getInt64Ty(ctx);
  auto* ptr_ty = llvm::PointerType::getUnqual(ctx);
  auto* schema_type = GetProcessStateSchemaType(ctx);

  auto count = static_cast<uint32_t>(layout.state_schemas.size());
  if (count == 0) {
    return llvm::ConstantPointerNull::get(
        llvm::cast<llvm::PointerType>(ptr_ty));
  }

  std::vector<llvm::Constant*> entries;
  entries.reserve(count);
  for (size_t si = 0; si < layout.state_schemas.size(); ++si) {
    const auto& schema = layout.state_schemas[si];
    llvm::Constant* init_fn = init_fns[si] != nullptr
                                  ? static_cast<llvm::Constant*>(init_fns[si])
                                  : llvm::ConstantPointerNull::get(
                                        llvm::cast<llvm::PointerType>(ptr_ty));
    entries.push_back(
        llvm::ConstantStruct::get(
            schema_type,
            {llvm::ConstantInt::get(i64_ty, schema.state_size),
             llvm::ConstantInt::get(i64_ty, schema.state_align), init_fn}));
  }

  auto* array_type = llvm::ArrayType::get(schema_type, count);
  auto* global = new llvm::GlobalVariable(
      mod, array_type, true, llvm::GlobalValue::InternalLinkage,
      llvm::ConstantArray::get(array_type, entries), "__lyra_state_schemas");
  global->setUnnamedAddr(llvm::GlobalValue::UnnamedAddr::Global);

  return llvm::ConstantExpr::getInBoundsGetElementPtr(
      array_type, global,
      llvm::ArrayRef<llvm::Constant*>{
          llvm::ConstantInt::get(i32_ty, 0),
          llvm::ConstantInt::get(i32_ty, 0)});
}

// Emit __lyra_slot_byte_offsets: compile-owned layout oracle.
// Slot-indexed, not instance-indexed. Used by the runtime constructor for
// byte-offset lookup when binding process frame headers.
auto EmitSlotByteOffsets(Context& context, const Layout& layout)
    -> llvm::Constant* {
  auto& ctx = context.GetLlvmContext();
  auto& mod = context.GetModule();
  auto* i32_ty = llvm::Type::getInt32Ty(ctx);
  auto* i64_ty = llvm::Type::getInt64Ty(ctx);

  auto count = static_cast<uint32_t>(layout.design.slots.size());
  if (count == 0) {
    return llvm::ConstantPointerNull::get(llvm::PointerType::getUnqual(ctx));
  }

  // Emit owner-resolved byte offsets for all slots (including aliases,
  // which share the canonical owner's offset).
  std::vector<llvm::Constant*> entries;
  entries.reserve(count);
  for (const auto& slot_id : layout.design.slots) {
    entries.push_back(
        llvm::ConstantInt::get(
            i64_ty, layout.design.GetStorageByteOffset(slot_id)));
  }

  auto* array_type = llvm::ArrayType::get(i64_ty, count);
  auto* global = new llvm::GlobalVariable(
      mod, array_type, true, llvm::GlobalValue::InternalLinkage,
      llvm::ConstantArray::get(array_type, entries),
      "__lyra_slot_byte_offsets");
  global->setUnnamedAddr(llvm::GlobalValue::UnnamedAddr::Global);

  return llvm::ConstantExpr::getInBoundsGetElementPtr(
      array_type, global,
      llvm::ArrayRef<llvm::Constant*>{
          llvm::ConstantInt::get(i32_ty, 0),
          llvm::ConstantInt::get(i32_ty, 0)});
}

auto EmitBytePoolGlobal(
    llvm::LLVMContext& ctx, llvm::Module& mod, llvm::ArrayRef<uint8_t> data,
    const std::string& name) -> llvm::Constant* {
  auto* ptr_ty = llvm::PointerType::getUnqual(ctx);
  if (data.empty()) {
    return llvm::ConstantPointerNull::get(
        llvm::cast<llvm::PointerType>(ptr_ty));
  }
  auto* init = llvm::ConstantDataArray::get(ctx, data);
  auto* global = new llvm::GlobalVariable(
      mod, init->getType(), true, llvm::GlobalValue::InternalLinkage, init,
      name);
  global->setUnnamedAddr(llvm::GlobalValue::UnnamedAddr::Global);
  auto* i32_ty = llvm::Type::getInt32Ty(ctx);
  auto* zero = llvm::ConstantInt::get(i32_ty, 0);
  return llvm::ConstantExpr::getInBoundsGetElementPtr(
      init->getType(), global, llvm::ArrayRef<llvm::Constant*>{zero, zero});
}

auto EmitConstructionProgramGlobal(
    llvm::LLVMContext& ctx, llvm::Module& mod,
    std::span<const runtime::ConstructionProgramEntry> entries)
    -> llvm::Constant* {
  auto* ptr_ty = llvm::PointerType::getUnqual(ctx);
  auto* i32_ty = llvm::Type::getInt32Ty(ctx);

  auto count = static_cast<uint32_t>(entries.size());
  if (count == 0) {
    return llvm::ConstantPointerNull::get(
        llvm::cast<llvm::PointerType>(ptr_ty));
  }

  auto* entry_type = GetConstructionProgramEntryType(ctx);

  std::vector<llvm::Constant*> constants;
  constants.reserve(count);
  for (const auto& e : entries) {
    constants.push_back(EncodeConstructionProgramEntry(entry_type, ctx, e));
  }

  auto* array_type = llvm::ArrayType::get(entry_type, count);
  auto* global = new llvm::GlobalVariable(
      mod, array_type, true, llvm::GlobalValue::InternalLinkage,
      llvm::ConstantArray::get(array_type, constants),
      "__lyra_construction_program");
  global->setUnnamedAddr(llvm::GlobalValue::UnnamedAddr::Global);

  auto* zero = llvm::ConstantInt::get(i32_ty, 0);
  return llvm::ConstantExpr::getInBoundsGetElementPtr(
      array_type, global, llvm::ArrayRef<llvm::Constant*>{zero, zero});
}

// Emit the body descriptor ref table: one BodyDescriptorRef per body group.
// Each entry packages all already-emitted body-shaped globals into one POD
// struct that the runtime can use to reconstruct a BodyDescriptorPackage.
auto EmitBodyDescriptorRefTable(
    llvm::LLVMContext& ctx, llvm::Module& mod,
    const std::vector<BodyDescriptorPackageEmission>& body_pkgs)
    -> llvm::Constant* {
  auto* ptr_ty = llvm::PointerType::getUnqual(ctx);
  auto* i32_ty = llvm::Type::getInt32Ty(ctx);

  auto count = static_cast<uint32_t>(body_pkgs.size());
  if (count == 0) {
    return llvm::ConstantPointerNull::get(
        llvm::cast<llvm::PointerType>(ptr_ty));
  }

  auto* ref_type = GetBodyDescriptorRefType(ctx);

  std::vector<llvm::Constant*> refs;
  refs.reserve(count);
  for (const auto& pkg : body_pkgs) {
    refs.push_back(EncodeBodyDescriptorRef(ref_type, ctx, pkg));
  }

  auto* array_type = llvm::ArrayType::get(ref_type, count);
  auto* global = new llvm::GlobalVariable(
      mod, array_type, true, llvm::GlobalValue::InternalLinkage,
      llvm::ConstantArray::get(array_type, refs), "__lyra_body_desc_refs");
  global->setUnnamedAddr(llvm::GlobalValue::UnnamedAddr::Global);

  auto* zero = llvm::ConstantInt::get(i32_ty, 0);
  return llvm::ConstantExpr::getInBoundsGetElementPtr(
      array_type, global, llvm::ArrayRef<llvm::Constant*>{zero, zero});
}

// Runtime constructor C ABI function declarations for LLVM IR emission.
struct ConstructorRuntimeFuncs {
  llvm::Function* create;
  llvm::Function* add_connection;
  llvm::Function* run_program;
  // Cut-3 direct-constructor helpers. Backend-emitted body-constructor
  // functions call begin_body_by_ref to activate the child's body
  // package, then add_child_object to materialize one plain-child
  // RuntimeInstance under the current parent scope.
  llvm::Function* begin_body_by_ref;
  llvm::Function* add_child_object;
  llvm::Function* finalize;
  llvm::Function* result_get_states;
  llvm::Function* result_get_num_total;
  llvm::Function* result_get_meta_words;
  llvm::Function* result_get_meta_word_count;
  llvm::Function* result_get_meta_pool;
  llvm::Function* result_get_meta_pool_size;
  llvm::Function* result_get_trigger_words;
  llvm::Function* result_get_trigger_word_count;
  llvm::Function* result_get_comb_words;
  llvm::Function* result_get_comb_word_count;
  llvm::Function* result_get_slot_meta_words;
  llvm::Function* result_get_slot_meta_count;
  llvm::Function* result_get_trace_meta_words;
  llvm::Function* result_get_trace_meta_word_count;
  llvm::Function* result_get_trace_meta_pool;
  llvm::Function* result_get_trace_meta_pool_size;
  llvm::Function* result_get_instances;
  llvm::Function* result_get_instance_count;
  llvm::Function* result_get_instance_bundles;
  llvm::Function* result_get_instance_bundle_count;
  llvm::Function* result_destroy;
  llvm::Function* result_set_ext_ref_bindings;
};

auto DeclareConstructorRuntimeFuncs(Context& context)
    -> ConstructorRuntimeFuncs {
  auto& ctx = context.GetLlvmContext();
  auto& mod = context.GetModule();
  auto* ptr_ty = llvm::PointerType::getUnqual(ctx);
  auto* i32_ty = llvm::Type::getInt32Ty(ctx);
  auto* i64_ty = llvm::Type::getInt64Ty(ctx);
  auto* void_ty = llvm::Type::getVoidTy(ctx);

  auto declare =
      [&](const char* name, llvm::Type* ret,
          std::initializer_list<llvm::Type*> args) -> llvm::Function* {
    auto* ft = llvm::FunctionType::get(ret, args, false);
    return llvm::Function::Create(
        ft, llvm::Function::ExternalLinkage, name, &mod);
  };

  return ConstructorRuntimeFuncs{
      .create = declare(
          "LyraConstructorCreate", ptr_ty,
          {ptr_ty, i32_ty, ptr_ty, i32_ty, i32_ty, ptr_ty, i64_ty,
           ptr_ty, i32_ty, ptr_ty, i32_ty, ptr_ty, i32_ty, ptr_ty,
           i32_ty, ptr_ty, ptr_ty, ptr_ty, i32_ty, ptr_ty, i32_ty,
           ptr_ty, i32_ty, ptr_ty, i32_ty, ptr_ty, i32_ty}),
      .add_connection =
          declare("LyraConstructorAddConnection", void_ty, {ptr_ty, ptr_ty}),
      .run_program = declare(
          "LyraConstructorRunProgram", void_ty,
          {ptr_ty, ptr_ty, i32_ty, ptr_ty, i32_ty, ptr_ty, i32_ty, ptr_ty,
           i32_ty, ptr_ty, i32_ty, ptr_ty, i32_ty}),
      // begin_body_by_ref / add_child_object are shared with the
      // body-constructor emitter; reuse the centralized declarations so
      // both callers see the same llvm::Function (no LLVM-auto-renamed
      // duplicates).
      .begin_body_by_ref =
          DeclareBodyConstructorRuntimeFuncs(ctx, mod).begin_body_by_ref,
      .add_child_object =
          DeclareBodyConstructorRuntimeFuncs(ctx, mod).add_child_object,
      .finalize = declare("LyraConstructorFinalize", ptr_ty, {ptr_ty}),
      .result_get_states =
          declare("LyraConstructionResultGetStates", ptr_ty, {ptr_ty}),
      .result_get_num_total =
          declare("LyraConstructionResultGetNumTotal", i32_ty, {ptr_ty}),
      .result_get_meta_words = declare(
          "LyraConstructionResultGetProcessMetaWords", ptr_ty, {ptr_ty}),
      .result_get_meta_word_count = declare(
          "LyraConstructionResultGetProcessMetaWordCount", i32_ty, {ptr_ty}),
      .result_get_meta_pool =
          declare("LyraConstructionResultGetProcessMetaPool", ptr_ty, {ptr_ty}),
      .result_get_meta_pool_size = declare(
          "LyraConstructionResultGetProcessMetaPoolSize", i32_ty, {ptr_ty}),
      .result_get_trigger_words =
          declare("LyraConstructionResultGetTriggerWords", ptr_ty, {ptr_ty}),
      .result_get_trigger_word_count = declare(
          "LyraConstructionResultGetTriggerWordCount", i32_ty, {ptr_ty}),
      .result_get_comb_words =
          declare("LyraConstructionResultGetCombWords", ptr_ty, {ptr_ty}),
      .result_get_comb_word_count =
          declare("LyraConstructionResultGetCombWordCount", i32_ty, {ptr_ty}),
      .result_get_slot_meta_words =
          declare("LyraConstructionResultGetSlotMetaWords", ptr_ty, {ptr_ty}),
      .result_get_slot_meta_count =
          declare("LyraConstructionResultGetSlotMetaCount", i32_ty, {ptr_ty}),
      .result_get_trace_meta_words = declare(
          "LyraConstructionResultGetTraceSignalMetaWords", ptr_ty, {ptr_ty}),
      .result_get_trace_meta_word_count = declare(
          "LyraConstructionResultGetTraceSignalMetaWordCount", i32_ty,
          {ptr_ty}),
      .result_get_trace_meta_pool = declare(
          "LyraConstructionResultGetTraceSignalMetaPool", ptr_ty, {ptr_ty}),
      .result_get_trace_meta_pool_size = declare(
          "LyraConstructionResultGetTraceSignalMetaPoolSize", i32_ty, {ptr_ty}),
      .result_get_instances =
          declare("LyraConstructionResultGetInstances", ptr_ty, {ptr_ty}),
      .result_get_instance_count =
          declare("LyraConstructionResultGetInstanceCount", i32_ty, {ptr_ty}),
      .result_get_instance_bundles =
          declare("LyraConstructionResultGetInstanceBundles", ptr_ty, {ptr_ty}),
      .result_get_instance_bundle_count = declare(
          "LyraConstructionResultGetInstanceBundleCount", i32_ty, {ptr_ty}),
      .result_destroy =
          declare("LyraConstructionResultDestroy", void_ty, {ptr_ty}),
      .result_set_ext_ref_bindings = declare(
          "LyraConstructionResultSetExtRefBindings", void_ty,
          {ptr_ty, ptr_ty, i32_ty, ptr_ty, ptr_ty, i32_ty}),
  };
}

// Emit the constructor function: creates the runtime constructor, adds
// connections, then replays the construction program via a single
// LyraConstructorRunProgram call. The runtime replays BeginBody/AddInstance
// in strict ModuleIndex order from the compact construction program.

auto EmitConstructorFunction(
    Context& context, const Layout& layout, llvm::Value* design_state,
    llvm::Constant* schemas_ptr, uint32_t num_schemas,
    llvm::Constant* slot_byte_offsets_ptr,
    const std::vector<BodyDescriptorPackageEmission>& body_descs,
    llvm::Constant* body_ref_table_ptr, llvm::Constant* conn_descs_ptr,
    const MetaTemplateEmission& conn_meta,
    const TriggerTemplateEmission& conn_triggers,
    const ObservableDescriptorEmission& pkg_observable,
    const InitDescriptorEmission& pkg_init,
    const lowering::mir_to_llvm::ConstructionProgramData& prog)
    -> ConstructorEmissionResult {
  auto& builder = context.GetBuilder();
  auto& ctx = context.GetLlvmContext();
  auto* ptr_ty = llvm::PointerType::getUnqual(ctx);
  auto* i32_ty = llvm::Type::getInt32Ty(ctx);
  auto* i64_ty = llvm::Type::getInt64Ty(ctx);

  auto rt = DeclareConstructorRuntimeFuncs(context);

  auto num_slots =
      static_cast<uint32_t>(layout.design.slot_byte_offsets.size());
  auto arena_size = layout.design.arena_size;

  // Create the runtime constructor object.
  auto* ctor = builder.CreateCall(
      rt.create,
      {schemas_ptr,
       llvm::ConstantInt::get(i32_ty, num_schemas),
       slot_byte_offsets_ptr,
       llvm::ConstantInt::get(i32_ty, num_slots),
       llvm::ConstantInt::get(i32_ty, layout.num_package_slots),
       design_state,
       llvm::ConstantInt::get(i64_ty, arena_size),
       conn_meta.entries_ptr,
       llvm::ConstantInt::get(i32_ty, conn_meta.num_entries),
       conn_meta.pool_ptr,
       llvm::ConstantInt::get(i32_ty, conn_meta.pool_size),
       conn_triggers.entries_ptr,
       llvm::ConstantInt::get(i32_ty, conn_triggers.num_entries),
       conn_triggers.ranges_ptr,
       llvm::ConstantInt::get(i32_ty, conn_triggers.num_ranges),
       conn_triggers.shapes_ptr,
       conn_triggers.groupable_ptr,
       pkg_observable.entries_ptr,
       llvm::ConstantInt::get(i32_ty, pkg_observable.num_entries),
       pkg_observable.pool_ptr,
       llvm::ConstantInt::get(i32_ty, pkg_observable.pool_size),
       pkg_init.recipe_ptr,
       llvm::ConstantInt::get(i32_ty, pkg_init.num_recipe_ops),
       pkg_init.recipe_roots_ptr,
       llvm::ConstantInt::get(i32_ty, pkg_init.num_recipe_roots),
       pkg_init.recipe_child_indices_ptr,
       llvm::ConstantInt::get(i32_ty, pkg_init.num_recipe_child_indices)},
      "ctor");

  // Add connection processes.
  const auto& conn_infos = layout.connection_realization_infos;
  for (uint32_t ci = 0; ci < conn_infos.size(); ++ci) {
    auto* conn_entry_type =
        llvm::StructType::get(ctx, {ptr_ty, i32_ty, i32_ty});
    auto* desc_ptr = builder.CreateConstInBoundsGEP1_32(
        conn_entry_type, conn_descs_ptr, ci, std::format("conn_desc_{}", ci));
    builder.CreateCall(rt.add_connection, {ctor, desc_ptr});
  }

  // Validate: body_descs must match the body-group domain of the
  // construction program entries. This is the emission-side counterpart
  // of the lowering-side topology checks in CompileDesignProcesses.
  if (body_descs.size() != layout.body_realization_infos.size()) {
    throw common::InternalError(
        "EmitConstructorFunction",
        std::format(
            "body_descs size {} != body_realization_infos size {}",
            body_descs.size(), layout.body_realization_infos.size()));
  }
  auto body_desc_count = static_cast<uint32_t>(body_descs.size());
  for (const auto& e : prog.entries) {
    if (e.body_group >= body_desc_count) {
      throw common::InternalError(
          "EmitConstructorFunction",
          std::format(
              "construction entry body_group {} >= body_desc_count {}",
              e.body_group, body_desc_count));
    }
  }

  // Emit pooled construction program globals. The body descriptor ref
  // table is created once up in EmitRealizationAndConstructor and shared
  // with any body-constructor functions that need to activate a child
  // body via LyraConstructorBeginBodyByRef.
  auto* path_pool_ptr = EmitBytePoolGlobal(
      ctx, context.GetModule(), llvm::ArrayRef<uint8_t>(prog.path_pool),
      "__lyra_path_pool");
  auto* param_pool_ptr = EmitBytePoolGlobal(
      ctx, context.GetModule(), llvm::ArrayRef<uint8_t>(prog.param_pool),
      "__lyra_param_pool");
  auto* program_ptr =
      EmitConstructionProgramGlobal(ctx, context.GetModule(), prog.entries);
  auto entry_count = static_cast<uint32_t>(prog.entries.size());

  // Emit port constant init pools if present.
  llvm::Constant* pc_inits_ptr = llvm::ConstantPointerNull::get(ptr_ty);
  uint32_t pc_inits_count = 0;
  llvm::Constant* pc_pool_ptr = llvm::ConstantPointerNull::get(ptr_ty);
  uint32_t pc_pool_size_val = 0;
  if (!prog.port_const_inits.empty()) {
    auto* pc_entry_ty = llvm::StructType::get(ctx, {i32_ty, i32_ty, i32_ty});
    std::vector<llvm::Constant*> pc_entry_constants;
    pc_entry_constants.reserve(prog.port_const_inits.size());
    for (const auto& e : prog.port_const_inits) {
      pc_entry_constants.push_back(
          llvm::ConstantStruct::get(
              pc_entry_ty, {llvm::ConstantInt::get(i32_ty, e.rel_byte_offset),
                            llvm::ConstantInt::get(i32_ty, e.value_offset),
                            llvm::ConstantInt::get(i32_ty, e.value_size)}));
    }
    pc_inits_count = static_cast<uint32_t>(prog.port_const_inits.size());
    auto* pc_array_ty = llvm::ArrayType::get(pc_entry_ty, pc_inits_count);
    auto* pc_global = new llvm::GlobalVariable(
        context.GetModule(), pc_array_ty, true,
        llvm::GlobalValue::PrivateLinkage,
        llvm::ConstantArray::get(pc_array_ty, pc_entry_constants),
        "__lyra_port_const_entries");
    auto* zero = llvm::ConstantInt::get(i32_ty, 0);
    pc_inits_ptr = llvm::ConstantExpr::getInBoundsGetElementPtr(
        pc_array_ty, pc_global, llvm::ArrayRef<llvm::Constant*>{zero, zero});
    if (!prog.port_const_pool.empty()) {
      pc_pool_ptr = EmitBytePoolGlobal(
          ctx, context.GetModule(),
          llvm::ArrayRef<uint8_t>(prog.port_const_pool),
          "__lyra_port_const_pool");
    }
    pc_pool_size_val = static_cast<uint32_t>(prog.port_const_pool.size());
  }

  // Replay the construction program: the runtime iterates entries in
  // parent-before-child order, creating scope nodes and instances.
  auto path_pool_size = static_cast<uint32_t>(prog.path_pool.size());
  auto param_pool_size = static_cast<uint32_t>(prog.param_pool.size());
  builder.CreateCall(
      rt.run_program,
      {ctor, body_ref_table_ptr,
       llvm::ConstantInt::get(i32_ty, body_desc_count), path_pool_ptr,
       llvm::ConstantInt::get(i32_ty, path_pool_size), param_pool_ptr,
       llvm::ConstantInt::get(i32_ty, param_pool_size), program_ptr,
       llvm::ConstantInt::get(i32_ty, entry_count), pc_inits_ptr,
       llvm::ConstantInt::get(i32_ty, pc_inits_count), pc_pool_ptr,
       llvm::ConstantInt::get(i32_ty, pc_pool_size_val)});

  // Finalize: returns a single constructor-owned result handle.
  auto* result = builder.CreateCall(rt.finalize, {ctor}, "ctor_result");

  // Set per-instance ext-ref binding records.
  if (!prog.ext_ref_binding_pool.empty()) {
    auto& mod = context.GetModule();
    auto* binding_ty = llvm::StructType::get(ctx, {i32_ty, i32_ty, i32_ty});

    std::vector<llvm::Constant*> binding_constants;
    binding_constants.reserve(prog.ext_ref_binding_pool.size());
    for (const auto& b : prog.ext_ref_binding_pool) {
      binding_constants.push_back(
          llvm::ConstantStruct::get(
              binding_ty,
              {llvm::ConstantInt::get(i32_ty, b.target_byte_offset),
               llvm::ConstantInt::get(i32_ty, b.target_instance_id),
               llvm::ConstantInt::get(i32_ty, b.target_local_signal.value)}));
    }
    auto pool_count = static_cast<uint32_t>(prog.ext_ref_binding_pool.size());
    auto* pool_array_ty = llvm::ArrayType::get(binding_ty, pool_count);
    auto* pool_global = new llvm::GlobalVariable(
        mod, pool_array_ty, true, llvm::GlobalValue::PrivateLinkage,
        llvm::ConstantArray::get(pool_array_ty, binding_constants),
        "__lyra_ext_ref_bindings");

    auto* offsets_global = new llvm::GlobalVariable(
        mod, llvm::ArrayType::get(i32_ty, prog.ext_ref_binding_offsets.size()),
        true, llvm::GlobalValue::PrivateLinkage,
        llvm::ConstantDataArray::get(ctx, prog.ext_ref_binding_offsets),
        "__lyra_ext_ref_binding_offsets");
    auto* counts_global = new llvm::GlobalVariable(
        mod, llvm::ArrayType::get(i32_ty, prog.ext_ref_binding_counts.size()),
        true, llvm::GlobalValue::PrivateLinkage,
        llvm::ConstantDataArray::get(ctx, prog.ext_ref_binding_counts),
        "__lyra_ext_ref_binding_counts");

    auto instance_count =
        static_cast<uint32_t>(prog.ext_ref_binding_offsets.size());
    builder.CreateCall(
        rt.result_set_ext_ref_bindings,
        {result, pool_global, llvm::ConstantInt::get(i32_ty, pool_count),
         offsets_global, counts_global,
         llvm::ConstantInt::get(i32_ty, instance_count)});
  }

  // Extract states, counts, and process metadata from the result.
  auto* states_array =
      builder.CreateCall(rt.result_get_states, {result}, "states");
  auto* num_total =
      builder.CreateCall(rt.result_get_num_total, {result}, "num_total");

  // Extract constructor-produced process metadata.
  auto* meta_words =
      builder.CreateCall(rt.result_get_meta_words, {result}, "meta_words");
  auto* meta_word_count = builder.CreateCall(
      rt.result_get_meta_word_count, {result}, "meta_word_count");
  auto* meta_pool =
      builder.CreateCall(rt.result_get_meta_pool, {result}, "meta_pool");
  auto* meta_pool_sz = builder.CreateCall(
      rt.result_get_meta_pool_size, {result}, "meta_pool_size");

  // Extract constructor-produced trigger/comb metadata.
  auto* trigger_words = builder.CreateCall(
      rt.result_get_trigger_words, {result}, "trigger_words");
  auto* trigger_word_count = builder.CreateCall(
      rt.result_get_trigger_word_count, {result}, "trigger_word_count");
  auto* comb_words =
      builder.CreateCall(rt.result_get_comb_words, {result}, "comb_words");
  auto* comb_word_count = builder.CreateCall(
      rt.result_get_comb_word_count, {result}, "comb_word_count");

  // Extract constructor-produced slot/trace metadata and instance paths.
  auto* slot_meta_words = builder.CreateCall(
      rt.result_get_slot_meta_words, {result}, "slot_meta_words");
  auto* slot_meta_count = builder.CreateCall(
      rt.result_get_slot_meta_count, {result}, "slot_meta_count");
  auto* trace_meta_words = builder.CreateCall(
      rt.result_get_trace_meta_words, {result}, "trace_meta_words");
  auto* trace_meta_word_count = builder.CreateCall(
      rt.result_get_trace_meta_word_count, {result}, "trace_meta_word_count");
  auto* trace_meta_pool = builder.CreateCall(
      rt.result_get_trace_meta_pool, {result}, "trace_meta_pool");
  auto* trace_meta_pool_size = builder.CreateCall(
      rt.result_get_trace_meta_pool_size, {result}, "trace_meta_pool_sz");
  auto* inst_ptrs =
      builder.CreateCall(rt.result_get_instances, {result}, "instance_ptrs");
  auto* inst_count = builder.CreateCall(
      rt.result_get_instance_count, {result}, "instance_count");
  auto* inst_bundles = builder.CreateCall(
      rt.result_get_instance_bundles, {result}, "instance_bundles");
  auto* inst_bundle_count = builder.CreateCall(
      rt.result_get_instance_bundle_count, {result}, "instance_bundle_count");

  return ConstructorEmissionResult{
      .states_array = states_array,
      .num_total = num_total,
      .result_handle = result,
      .destroy_fn = rt.result_destroy,
      .process_meta =
          RealizationEmissionResult::ProcessMeta{
              .words = meta_words,
              .word_count = meta_word_count,
              .pool = meta_pool,
              .pool_size = meta_pool_sz,
          },
      .dispatch_meta =
          RealizationEmissionResult::DispatchMeta{
              .trigger_words = trigger_words,
              .trigger_word_count = trigger_word_count,
              .comb_words = comb_words,
              .comb_word_count = comb_word_count,
          },
      .observation_meta =
          RealizationEmissionResult::ObservationMeta{
              .slot_meta_words = slot_meta_words,
              .slot_meta_count = slot_meta_count,
              .trace_meta_words = trace_meta_words,
              .trace_meta_word_count = trace_meta_word_count,
              .trace_meta_pool = trace_meta_pool,
              .trace_meta_pool_size = trace_meta_pool_size,
              .instance_ptrs = inst_ptrs,
              .instance_count = inst_count,
              .instance_bundles = inst_bundles,
              .instance_bundle_count = inst_bundle_count,
          },
  };
}

}  // namespace

auto DeclareBodyConstructorRuntimeFuncs(
    llvm::LLVMContext& ctx, llvm::Module& mod) -> BodyConstructorRuntimeFuncs {
  auto* ptr_ty = llvm::PointerType::getUnqual(ctx);
  auto* void_ty = llvm::Type::getVoidTy(ctx);
  auto* i32_ty = llvm::Type::getInt32Ty(ctx);
  auto* i64_ty = llvm::Type::getInt64Ty(ctx);

  auto get_or_create =
      [&](const char* name, llvm::Type* ret,
          std::initializer_list<llvm::Type*> args) -> llvm::Function* {
    if (auto* existing = mod.getFunction(name)) return existing;
    auto* ft = llvm::FunctionType::get(ret, args, /*isVarArg=*/false);
    return llvm::Function::Create(
        ft, llvm::Function::ExternalLinkage, name, &mod);
  };

  return BodyConstructorRuntimeFuncs{
      .begin_body_by_ref = get_or_create(
          "LyraConstructorBeginBodyByRef", void_ty, {ptr_ty, ptr_ty}),
      .add_child_object = get_or_create(
          "LyraConstructorAddChildObject", ptr_ty,
          {ptr_ty, ptr_ty, i32_ty, i32_ty, ptr_ty, i64_ty, i64_ty, i32_ty,
           ptr_ty, ptr_ty}),
  };
}

auto GetBodyDescriptorRefType(llvm::LLVMContext& ctx) -> llvm::StructType* {
  auto* ptr_ty = llvm::PointerType::getUnqual(ctx);
  auto* i32_ty = llvm::Type::getInt32Ty(ctx);
  return llvm::StructType::get(
      ctx,
      {// desc, entries, num_entries
       ptr_ty, ptr_ty, i32_ty,
       // meta: entries, num, pool, pool_size
       ptr_ty, i32_ty, ptr_ty, i32_ty,
       // triggers: entries, num, ranges, num_ranges, shapes, groupable
       ptr_ty, i32_ty, ptr_ty, i32_ty, ptr_ty, ptr_ty,
       // comb: entries, num, kernels, num_kernels
       ptr_ty, i32_ty, ptr_ty, i32_ty,
       // observable: entries, num, pool, pool_size
       ptr_ty, i32_ty, ptr_ty, i32_ty,
       // init: recipe, num_ops, roots, num_roots, child_indices, num,
       //       param_slots, num
       ptr_ty, i32_ty, ptr_ty, i32_ty, ptr_ty, i32_ty, ptr_ty, i32_ty,
       // decision: tables, num_tables
       ptr_ty, i32_ty});
}

auto EmitRealizationAndConstructor(
    Context& context, const CuFacts& facts, const Layout& layout,
    llvm::Value* design_state,
    std::span<const CodegenSession::BodyCompiledFuncs> body_compiled_funcs,
    const std::vector<llvm::Function*>& process_funcs, size_t num_init,
    const ConstructionProgramData& construction_program)
    -> RealizationEmissionResult {
  auto& ctx = context.GetLlvmContext();

  // Emit per-schema frame-init functions and state schemas.
  auto init_fns = EmitPerSchemaFrameInitFunctions(context, facts, layout);
  auto* schemas_ptr = EmitProcessStateSchemas(context, layout, init_fns);
  auto num_schemas = static_cast<uint32_t>(layout.state_schemas.size());

  // Emit constructor-side definition artifacts.
  auto* slot_offsets_ptr = EmitSlotByteOffsets(context, layout);

  // Compute the effective per-body direct-constructor flag. An HIR-level
  // "uses_mir_constructor" hint is only honored when the construction
  // program confirms: exactly one compile-time instance of the body, and
  // every direct child of that instance is a module instance with no
  // port-constant initializers. Transmitted parameter payloads are now
  // carried as typed MIR operands on NewObject and marshaled directly
  // into the body-constructor call; their presence is no longer a
  // disqualifier. Any remaining deviation keeps the body on the flat
  // path.
  std::vector<uint8_t> effective_uses_mir_ctor(
      layout.body_realization_infos.size(), 0);
  {
    std::vector<uint32_t> instance_count_per_body(
        layout.body_realization_infos.size(), 0);
    std::vector<uint32_t> parent_entry_of_body(
        layout.body_realization_infos.size(), UINT32_MAX);
    for (uint32_t i = 0; i < construction_program.entries.size(); ++i) {
      const auto& e = construction_program.entries[i];
      if (e.node_kind != runtime::ConstructionNodeKind::kInstance) continue;
      if (e.body_group >= instance_count_per_body.size()) continue;
      ++instance_count_per_body[e.body_group];
      parent_entry_of_body[e.body_group] = i;
    }
    for (size_t bi = 0; bi < layout.body_realization_infos.size(); ++bi) {
      const auto* body = layout.body_realization_infos[bi].body;
      if (body == nullptr || !body->uses_mir_constructor) continue;
      if (instance_count_per_body[bi] != 1) continue;

      bool all_children_direct = true;
      uint32_t parent_entry = parent_entry_of_body[bi];
      for (const auto& e : construction_program.entries) {
        if (e.parent_scope_index != parent_entry) continue;
        if (e.node_kind != runtime::ConstructionNodeKind::kInstance) {
          all_children_direct = false;
          break;
        }
        if (e.port_const_init_count != 0) {
          all_children_direct = false;
          break;
        }
      }
      if (!all_children_direct) continue;
      effective_uses_mir_ctor[bi] = 1;
    }
  }

  auto body_descs = EmitBodyRealizationDescs(
      context, layout, body_compiled_funcs,
      construction_program.child_site_to_tree_ordinal, effective_uses_mir_ctor);

  // Pack all per-body globals into a BodyDescriptorRef[] table once.
  // Backend-emitted body-constructor functions take pointers into this
  // table to activate child bodies via LyraConstructorBeginBodyByRef;
  // EmitConstructorFunction reuses it for the flat replay path.
  auto* body_ref_table_ptr =
      EmitBodyDescriptorRefTable(ctx, context.GetModule(), body_descs);

  // Emit body-constructor function bodies for bodies that opted into the
  // backend-emitted construction path. The forward declarations were
  // created by EmitBodyRealizationDescs so the body descriptor globals
  // already embed these fn pointers.
  EmitBodyConstructorFunctions(
      context, facts, layout, body_descs, body_ref_table_ptr,
      construction_program, effective_uses_mir_ctor);

  auto* conn_descs_ptr =
      EmitConnectionRealizationDescs(context, layout, process_funcs, num_init);
  auto conn_meta = EmitConnectionMetaTemplate(context, layout);
  auto conn_triggers = EmitConnectionTriggerTemplate(context, layout);

  // Emit package/global observable and init descriptor globals.
  auto pkg_obs = EmitObservableDescriptorTemplate(
      ctx, context.GetModule(), layout.package_observable_descriptors,
      "__lyra_pkg");
  auto pkg_init = EmitInitDescriptor(
      ctx, context.GetModule(), layout.package_init_descriptor.storage_recipe,
      layout.package_init_descriptor.recipe_root_indices,
      layout.package_init_descriptor.recipe_child_indices,
      std::span<const runtime::ParamInitSlotEntry>{}, "__lyra_pkg");

  // Emit the constructor function and extract runtime values.
  auto ctor = EmitConstructorFunction(
      context, layout, design_state, schemas_ptr, num_schemas, slot_offsets_ptr,
      body_descs, body_ref_table_ptr, conn_descs_ptr, conn_meta, conn_triggers,
      pkg_obs, pkg_init, construction_program);

  return RealizationEmissionResult{
      .states_array = ctor.states_array,
      .num_total = ctor.num_total,
      .result_handle = ctor.result_handle,
      .destroy_fn = ctor.destroy_fn,
      .process_meta =
          RealizationEmissionResult::ProcessMeta{
              .words = ctor.process_meta.words,
              .word_count = ctor.process_meta.word_count,
              .pool = ctor.process_meta.pool,
              .pool_size = ctor.process_meta.pool_size,
          },
      .dispatch_meta =
          RealizationEmissionResult::DispatchMeta{
              .trigger_words = ctor.dispatch_meta.trigger_words,
              .trigger_word_count = ctor.dispatch_meta.trigger_word_count,
              .comb_words = ctor.dispatch_meta.comb_words,
              .comb_word_count = ctor.dispatch_meta.comb_word_count,
          },
      .observation_meta =
          RealizationEmissionResult::ObservationMeta{
              .slot_meta_words = ctor.observation_meta.slot_meta_words,
              .slot_meta_count = ctor.observation_meta.slot_meta_count,
              .trace_meta_words = ctor.observation_meta.trace_meta_words,
              .trace_meta_word_count =
                  ctor.observation_meta.trace_meta_word_count,
              .trace_meta_pool = ctor.observation_meta.trace_meta_pool,
              .trace_meta_pool_size =
                  ctor.observation_meta.trace_meta_pool_size,
              .instance_ptrs = ctor.observation_meta.instance_ptrs,
              .instance_count = ctor.observation_meta.instance_count,
              .instance_bundles = ctor.observation_meta.instance_bundles,
              .instance_bundle_count =
                  ctor.observation_meta.instance_bundle_count,
          },
  };
}

}  // namespace lyra::lowering::mir_to_llvm
