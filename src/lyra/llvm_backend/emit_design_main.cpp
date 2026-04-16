#include "lyra/llvm_backend/emit_design_main.hpp"

#include <cstddef>
#include <cstdint>
#include <format>
#include <optional>
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
#include <llvm/Support/Casting.h>

#include "lyra/common/internal_error.hpp"
#include "lyra/common/origin_id.hpp"
#include "lyra/llvm_backend/codegen_session.hpp"
#include "lyra/llvm_backend/context.hpp"
#include "lyra/llvm_backend/cu_facts.hpp"
#include "lyra/llvm_backend/deferred_thunk_abi.hpp"
#include "lyra/llvm_backend/design_metadata_lowering.hpp"
#include "lyra/llvm_backend/emit_realization_descriptors.hpp"
#include "lyra/llvm_backend/inspection_plan.hpp"
#include "lyra/llvm_backend/layout/layout.hpp"
#include "lyra/llvm_backend/process.hpp"
#include "lyra/llvm_backend/process_meta_utils.hpp"
#include "lyra/llvm_backend/runtime_abi_codegen.hpp"
#include "lyra/llvm_backend/type_ops/default_init.hpp"
#include "lyra/llvm_backend/type_ops/four_state_init.hpp"
#include "lyra/lowering/origin_map_lookup.hpp"
#include "lyra/mir/deferred_assertion_site.hpp"
#include "lyra/realization/build_design_metadata.hpp"
#include "lyra/runtime/runtime_abi.hpp"
#include "lyra/runtime/wait_site.hpp"

namespace lyra::lowering::mir_to_llvm {

namespace {

struct WaitSiteMetaResult {
  llvm::Constant* words_ptr = nullptr;
  uint32_t count = 0;
};

auto EmitWaitSiteMetaTable(
    Context& context, std::span<const WaitSiteEntry> entries)
    -> WaitSiteMetaResult {
  auto& ctx = context.GetLlvmContext();
  auto* i32_ty = llvm::Type::getInt32Ty(ctx);
  auto* ptr_ty = llvm::PointerType::getUnqual(ctx);
  auto* null_ptr =
      llvm::ConstantPointerNull::get(llvm::cast<llvm::PointerType>(ptr_ty));
  if (entries.empty()) {
    return {.words_ptr = null_ptr, .count = 0};
  }

  std::vector<llvm::Constant*> words;
  words.reserve(2 + (entries.size() * runtime::wait_site_abi::kStride));

  words.push_back(
      llvm::ConstantInt::get(i32_ty, runtime::wait_site_abi::kVersion));
  words.push_back(
      llvm::ConstantInt::get(i32_ty, static_cast<uint32_t>(entries.size())));

  for (uint32_t i = 0; i < entries.size(); ++i) {
    const auto& entry = entries[i];

    auto shape = runtime::WaitShapeKind::kStatic;
    if (entry.has_late_bound) {
      shape = runtime::WaitShapeKind::kRebindable;
    } else if (entry.has_container) {
      shape = runtime::WaitShapeKind::kDynamic;
    }

    uint32_t shape_and_triggers =
        (static_cast<uint32_t>(shape) & 0xFF) | (entry.num_triggers << 8);
    uint32_t flags = entry.has_late_bound ? 1U : 0U;

    words.push_back(llvm::ConstantInt::get(i32_ty, i));
    words.push_back(llvm::ConstantInt::get(i32_ty, entry.resume_block));
    words.push_back(llvm::ConstantInt::get(i32_ty, shape_and_triggers));
    words.push_back(llvm::ConstantInt::get(i32_ty, flags));
  }

  auto& mod = context.GetModule();
  auto word_count = static_cast<uint32_t>(words.size());

  auto* words_array_type = llvm::ArrayType::get(i32_ty, word_count);
  auto* words_init = llvm::ConstantArray::get(words_array_type, words);
  auto* words_global = new llvm::GlobalVariable(
      mod, words_array_type, true, llvm::GlobalValue::InternalLinkage,
      words_init, "__lyra_wait_site_meta_table");
  words_global->setUnnamedAddr(llvm::GlobalValue::UnnamedAddr::Global);

  auto* zero = llvm::ConstantInt::get(i32_ty, 0);
  auto* words_gep = llvm::ConstantExpr::getInBoundsGetElementPtr(
      words_array_type, words_global,
      llvm::ArrayRef<llvm::Constant*>{zero, zero});

  return {.words_ptr = words_gep, .count = word_count};
}

void InitializeProcessState(
    Context& context, const CuFacts& facts, llvm::Value* process_state,
    llvm::Value* design_state, const ProcessLayout& proc_layout) {
  auto& builder = context.GetBuilder();
  auto* state_type = proc_layout.state_type;
  bool force_two_state = facts.force_two_state;

  EmitMemsetZero(context, process_state, state_type);
  context.EmitStoreDesignPtr(process_state, design_state);

  if (force_two_state) return;

  auto* frame_type = proc_layout.frame.llvm_type;
  auto* frame_ptr = builder.CreateStructGEP(state_type, process_state, 1);
  const auto& frame_layout = proc_layout.frame;

  EmitApply4StatePatches(context, frame_ptr, frame_layout.four_state_patches);

  const auto& types = *facts.types;
  for (uint32_t i = 0; i < frame_layout.field_types.size(); ++i) {
    TypeId type_id = frame_layout.field_types[i];
    if (!IsFourState(facts, type_id)) {
      continue;
    }
    if (IsScalarPatchable(type_id, types, force_two_state)) {
      continue;
    }
    auto* field_ptr = builder.CreateStructGEP(frame_type, frame_ptr, i);
    EmitSVDefaultInitAfterZero(context, facts, field_ptr, type_id);
  }
}

struct MainFunctionSetup {
  llvm::Function* main_func = nullptr;
  llvm::BasicBlock* exit_block = nullptr;
  llvm::Value* design_state = nullptr;
  llvm::Value* run_session_ptr = nullptr;
};

auto CreateMainFunction(
    Context& context, lowering::mir_to_llvm::MainAbi main_abi)
    -> MainFunctionSetup {
  auto& builder = context.GetBuilder();
  auto& ctx = context.GetLlvmContext();
  auto& mod = context.GetModule();
  bool argv_forwarding =
      main_abi == lowering::mir_to_llvm::MainAbi::kArgvForwarding;

  llvm::FunctionType* main_type = nullptr;
  if (argv_forwarding) {
    // AOT: int main(int argc, char** argv)
    auto* i32_ty = llvm::Type::getInt32Ty(ctx);
    auto* ptr_ty = llvm::PointerType::getUnqual(ctx);
    main_type = llvm::FunctionType::get(i32_ty, {i32_ty, ptr_ty}, false);
  } else {
    // JIT: int main(void* run_session_ptr)
    auto* ptr_ty = llvm::PointerType::getUnqual(ctx);
    main_type =
        llvm::FunctionType::get(llvm::Type::getInt32Ty(ctx), {ptr_ty}, false);
  }
  // JIT uses "lyra_entry" to avoid C runtime interference with "main".
  // AOT uses "main" as the real program entry point.
  const char* entry_name = argv_forwarding ? "main" : "lyra_entry";
  auto* main_func = llvm::Function::Create(
      main_type, llvm::Function::ExternalLinkage, entry_name, &mod);

  auto* entry = llvm::BasicBlock::Create(ctx, "entry", main_func);
  auto* exit_block = llvm::BasicBlock::Create(ctx, "exit", main_func);
  builder.SetInsertPoint(entry);

  auto* i8_ty = llvm::Type::getInt8Ty(ctx);
  auto* arena_ty = llvm::ArrayType::get(i8_ty, context.GetDesignArenaSize());
  auto* design_state = builder.CreateAlloca(arena_ty, nullptr, "design_state");

  // RunSession lifetime:
  //   JIT: caller provides run_session_ptr as main's argument
  //   AOT: main creates RunSession via LyraCreateRunSession()
  llvm::Value* run_session_ptr = nullptr;
  if (argv_forwarding) {
    run_session_ptr = builder.CreateCall(
        context.GetLyraCreateRunSession(), {}, "run_session");
  } else {
    run_session_ptr = main_func->getArg(0);
    run_session_ptr->setName("run_session_ptr");
  }

  return {
      .main_func = main_func,
      .exit_block = exit_block,
      .design_state = design_state,
      .run_session_ptr = run_session_ptr,
  };
}

void EmitDesignStateZero(Context& context, llvm::Value* design_state) {
  auto& ctx = context.GetLlvmContext();
  auto* i8_ty = llvm::Type::getInt8Ty(ctx);
  auto* arena_ty = llvm::ArrayType::get(i8_ty, context.GetDesignArenaSize());
  EmitMemsetZero(context, design_state, arena_ty);
}

auto EmitRuntimeInit(
    Context& context, llvm::Function* main_func,
    const EmitDesignMainInput& input) -> llvm::Value* {
  auto& builder = context.GetBuilder();
  auto& ctx = context.GetLlvmContext();
  bool argv_forwarding =
      input.main_abi == lowering::mir_to_llvm::MainAbi::kArgvForwarding;

  llvm::Value* fs_base_dir_str = nullptr;
  if (argv_forwarding) {
    auto* argv0 = builder.CreateLoad(
        llvm::PointerType::getUnqual(ctx), main_func->getArg(1), "argv0");
    fs_base_dir_str =
        builder.CreateCall(context.GetLyraResolveBaseDir(), {argv0});
  } else {
    fs_base_dir_str =
        builder.CreateGlobalStringPtr(input.fs_base_dir, "fs_base_dir");
  }
  auto* i32_ty = llvm::Type::getInt32Ty(ctx);
  builder.CreateCall(
      context.GetLyraInitRuntime(),
      {llvm::ConstantInt::get(i32_ty, input.iteration_limit)});
  return fs_base_dir_str;
}

void EmitInitProcesses(
    Context& context, const CuFacts& facts, llvm::Value* design_state,
    std::span<const ProcessLayout> processes,
    const std::vector<llvm::Function*>& process_funcs, size_t num_init) {
  auto& builder = context.GetBuilder();

  for (size_t i = 0; i < num_init; ++i) {
    const auto& proc_layout = processes[i];

    auto* process_state = builder.CreateAlloca(
        proc_layout.state_type, nullptr, std::format("init_state_{}", i));

    InitializeProcessState(
        context, facts, process_state, design_state, proc_layout);

    builder.CreateCall(
        context.GetLyraRunProcessSync(), {process_funcs[i], process_state});
  }
}

struct PlusargsSetup {
  llvm::Value* array = nullptr;
  llvm::Value* count = nullptr;
};

auto BuildPlusargs(
    Context& context, llvm::Function* main_func,
    const EmitDesignMainInput& input) -> PlusargsSetup {
  auto& builder = context.GetBuilder();
  auto& ctx = context.GetLlvmContext();
  auto* ptr_ty = llvm::PointerType::getUnqual(ctx);
  auto* i32_ty = llvm::Type::getInt32Ty(ctx);
  bool argv_forwarding =
      input.main_abi == lowering::mir_to_llvm::MainAbi::kArgvForwarding;

  if (argv_forwarding) {
    auto* argc_val = main_func->getArg(0);
    auto* argv_val = main_func->getArg(1);
    auto* argc_minus_1 =
        builder.CreateSub(argc_val, llvm::ConstantInt::get(i32_ty, 1));
    auto* is_positive =
        builder.CreateICmpSGT(argc_minus_1, llvm::ConstantInt::get(i32_ty, 0));
    auto* count = builder.CreateSelect(
        is_positive, argc_minus_1, llvm::ConstantInt::get(i32_ty, 0),
        "num_plusargs");
    auto* array = builder.CreateGEP(
        ptr_ty, argv_val, {llvm::ConstantInt::get(i32_ty, 1)}, "plusargs");
    return {.array = array, .count = count};
  }

  auto num_plusargs = static_cast<uint32_t>(input.plusargs.size());
  auto* count = llvm::ConstantInt::get(i32_ty, num_plusargs);
  if (num_plusargs > 0) {
    auto* array = builder.CreateAlloca(
        ptr_ty, llvm::ConstantInt::get(i32_ty, num_plusargs), "plusargs");
    for (uint32_t i = 0; i < num_plusargs; ++i) {
      auto* plusarg_str = builder.CreateGlobalStringPtr(
          input.plusargs[i], std::format("plusarg_{}", i));
      auto* slot = builder.CreateGEP(ptr_ty, array, {builder.getInt32(i)});
      builder.CreateStore(plusarg_str, slot);
    }
    return {.array = array, .count = count};
  }

  auto* null_array =
      llvm::ConstantPointerNull::get(llvm::cast<llvm::PointerType>(ptr_ty));
  return {.array = null_array, .count = count};
}

// Metadata-building outputs: emitted metadata globals plus structured
// lowering-time analysis reports surfaced to the driver boundary.
struct DesignMetadataOutputs {
  MetadataGlobals globals;
};

auto BuildDesignMetadataOutputs(
    Context& context, const Layout& layout, const EmitDesignMainInput& input,
    std::span<const common::OriginId> back_edge_origins)
    -> DesignMetadataOutputs {
  auto conn_desc_entries = ExtractConnectionDescriptorEntries(
      layout.connection_kernel_entries, layout.body_realization_infos,
      layout.instance_body_groups);
  auto back_edge_site_inputs = PrepareBackEdgeSiteInputs(
      back_edge_origins, input.diag_ctx, input.source_manager);

  metadata::DesignMetadataInputs metadata_inputs{
      .back_edge_sites = std::move(back_edge_site_inputs),
      .connection_descriptors = std::move(conn_desc_entries),
  };
  auto metadata = realization::BuildDesignMetadata(metadata_inputs);

  return DesignMetadataOutputs{
      .globals = EmitDesignMetadataGlobals(
          context.GetModule(), context.GetLlvmContext(), metadata),
  };
}

// Emit a global constant array of LyraDeferredAssertionSiteMeta structs.
// Returns the global pointer (or nullptr if no deferred sites).
auto EmitDeferredAssertionSiteMetaGlobal(
    Context& context, const CuFacts& facts,
    const std::vector<mir::DeferredAssertionSiteInfo>& sites,
    std::span<const DeferredSiteCompiledArtifact> artifacts,
    std::span<const lowering::BodyOriginProvenance::Entry* const>
        site_origin_entries,
    std::span<const uint32_t> site_cover_bases) -> llvm::Constant* {
  if (sites.empty()) return nullptr;

  if (artifacts.size() != sites.size()) {
    throw common::InternalError(
        "EmitDeferredAssertionSiteMetaGlobal",
        std::format(
            "artifact/site size mismatch: {} artifacts for {} sites",
            artifacts.size(), sites.size()));
  }

  auto& llvm_ctx = context.GetLlvmContext();
  auto* i8_ty = llvm::Type::getInt8Ty(llvm_ctx);
  auto* i16_ty = llvm::Type::getInt16Ty(llvm_ctx);
  auto* i32_ty = llvm::Type::getInt32Ty(llvm_ctx);
  auto* ptr_ty = llvm::PointerType::getUnqual(llvm_ctx);

  // Struct layout matches LyraDeferredAssertionSiteMeta:
  // { i8 kind, i8 pad0, i16 pad1, i32 cover_site_id,
  //   ptr origin_file, i32 origin_line, i32 origin_col,
  //   ptr pass_thunk, ptr fail_thunk, i32 pass_payload_size,
  //   i32 fail_payload_size }
  auto* entry_ty = llvm::StructType::get(
      llvm_ctx, {i8_ty, i8_ty, i16_ty, i32_ty, ptr_ty, i32_ty, i32_ty, ptr_ty,
                 ptr_ty, i32_ty, i32_ty});

  std::vector<llvm::Constant*> entries;
  entries.reserve(sites.size());

  for (uint32_t si = 0; si < sites.size(); ++si) {
    const auto& site = sites[si];
    const auto& artifact = artifacts[si];
    auto kind_val = static_cast<uint8_t>(site.kind);

    // Derive cover_site_id from semantic action variant.
    // cover_site_id in the action is body-local; apply per-site cover base
    // to produce the design-global runtime index.
    uint32_t cover_id = UINT32_MAX;
    if (site.pass_action.has_value()) {
      const auto* cover_hit =
          std::get_if<mir::DeferredCoverHitAction>(&*site.pass_action);
      if (cover_hit != nullptr) {
        uint32_t cover_base =
            (si < site_cover_bases.size()) ? site_cover_bases[si] : 0;
        cover_id = cover_base + cover_hit->cover_site_id.Index();
      }
    }

    // Resolve source location from canonical OriginId.
    // Sites are body-local; resolve from the owning body's origin table
    // instead of the design-global OriginMapLookup (which has no active
    // BodyScope at this point).
    llvm::Constant* file_ptr = llvm::ConstantPointerNull::get(ptr_ty);
    uint32_t line = 0;
    uint32_t col = 0;
    if (site.origin.IsValid()) {
      std::optional<lowering::BodyLocalOriginResolver> site_resolver;
      std::optional<lowering::DiagnosticContext> site_diag;
      const lowering::DiagnosticContext* site_diag_ptr =
          &context.GetDiagnosticContext();
      if (si < site_origin_entries.size() &&
          site_origin_entries[si] != nullptr &&
          site_origin_entries[si]->arena != nullptr) {
        site_resolver.emplace(
            site_origin_entries[si]->origins, *site_origin_entries[si]->arena);
        site_diag.emplace(*site_resolver);
        site_diag_ptr = &*site_diag;
      }
      auto loc = ResolveProcessOrigin(
          site.origin, site_diag_ptr, facts.source_manager);
      if (loc.line > 0 && !loc.file.empty()) {
        auto* str_const =
            llvm::ConstantDataArray::getString(llvm_ctx, loc.file, true);
        auto* str_global = new llvm::GlobalVariable(
            context.GetModule(), str_const->getType(), true,
            llvm::GlobalValue::PrivateLinkage, str_const);
        file_ptr = llvm::ConstantExpr::getBitCast(str_global, ptr_ty);
        line = loc.line;
        col = loc.col;
      }
    }

    // Validate semantic/artifact lockstep: compiled artifacts must match
    // semantic site data exactly.
    const bool need_pass_thunk = GetDeferredPassUserCallAction(site) != nullptr;
    const bool need_fail_thunk = GetDeferredFailUserCallAction(site) != nullptr;

    if (need_pass_thunk != (artifact.pass_thunk != nullptr)) {
      throw common::InternalError(
          "EmitDeferredAssertionSiteMetaGlobal",
          std::format(
              "site {} pass artifact mismatch: semantic action {}, "
              "compiled thunk {}",
              si, need_pass_thunk, artifact.pass_thunk != nullptr));
    }
    if (need_fail_thunk != (artifact.fail_thunk != nullptr)) {
      throw common::InternalError(
          "EmitDeferredAssertionSiteMetaGlobal",
          std::format(
              "site {} fail artifact mismatch: semantic action {}, "
              "compiled thunk {}",
              si, need_fail_thunk, artifact.fail_thunk != nullptr));
    }
    if (!need_pass_thunk && artifact.pass_payload_size != 0) {
      throw common::InternalError(
          "EmitDeferredAssertionSiteMetaGlobal",
          std::format(
              "site {} built-in pass path has non-zero payload size {}", si,
              artifact.pass_payload_size));
    }
    if (!need_fail_thunk && artifact.fail_payload_size != 0) {
      throw common::InternalError(
          "EmitDeferredAssertionSiteMetaGlobal",
          std::format(
              "site {} built-in fail path has non-zero payload size {}", si,
              artifact.fail_payload_size));
    }

    // Thunk pointers and payload sizes come directly from the compiled
    // artifact. No re-derivation, no scanning.
    llvm::Constant* pass_thunk_ptr =
        artifact.pass_thunk != nullptr
            ? static_cast<llvm::Constant*>(artifact.pass_thunk)
            : llvm::ConstantPointerNull::get(ptr_ty);
    llvm::Constant* fail_thunk_ptr =
        artifact.fail_thunk != nullptr
            ? static_cast<llvm::Constant*>(artifact.fail_thunk)
            : llvm::ConstantPointerNull::get(ptr_ty);

    entries.push_back(
        llvm::ConstantStruct::get(
            entry_ty,
            {llvm::ConstantInt::get(i8_ty, kind_val),
             llvm::ConstantInt::get(i8_ty, 0),
             llvm::ConstantInt::get(i16_ty, 0),
             llvm::ConstantInt::get(i32_ty, cover_id), file_ptr,
             llvm::ConstantInt::get(i32_ty, line),
             llvm::ConstantInt::get(i32_ty, col), pass_thunk_ptr,
             fail_thunk_ptr,
             llvm::ConstantInt::get(i32_ty, artifact.pass_payload_size),
             llvm::ConstantInt::get(i32_ty, artifact.fail_payload_size)}));
  }

  auto* array_ty = llvm::ArrayType::get(entry_ty, entries.size());
  auto* global = new llvm::GlobalVariable(
      context.GetModule(), array_ty, true, llvm::GlobalValue::PrivateLinkage,
      llvm::ConstantArray::get(array_ty, entries),
      "__lyra_deferred_assertion_site_meta");

  return llvm::ConstantExpr::getBitCast(global, ptr_ty);
}

auto BuildRuntimeAbi(
    Context& context, const MetadataGlobals& meta_globals,
    const WaitSiteMetaResult& wait_site_meta, uint32_t num_connection,
    uint32_t feature_flags, const std::string& signal_trace_path,
    llvm::Value* design_state,
    const RealizationEmissionResult::ProcessMeta& process_meta,
    const RealizationEmissionResult::DispatchMeta& dispatch_meta,
    const RealizationEmissionResult::ObservationMeta& observation_meta,
    uint32_t num_immediate_cover_sites, int8_t global_precision_power,
    llvm::Constant* deferred_site_meta_global,
    uint32_t num_deferred_assertion_sites, uint32_t num_events,
    llvm::Value* fs_base_dir_str, llvm::Value* conn_desc_ptr,
    llvm::Value* conn_desc_count) -> llvm::Value* {
  auto& builder = context.GetBuilder();
  auto& ctx = context.GetLlvmContext();
  auto* i32_ty = llvm::Type::getInt32Ty(ctx);
  auto* ptr_ty = llvm::PointerType::getUnqual(ctx);
  auto* null_ptr =
      llvm::ConstantPointerNull::get(llvm::cast<llvm::PointerType>(ptr_ty));

  auto* abi_struct_type = GetRuntimeAbiStructType(ctx);

  auto* abi_alloca = builder.CreateAlloca(abi_struct_type, nullptr, "abi");

  auto store_field = [&](unsigned idx, llvm::Value* val) {
    auto* gep = builder.CreateStructGEP(abi_struct_type, abi_alloca, idx);
    builder.CreateStore(val, gep);
  };

  store_field(0, llvm::ConstantInt::get(i32_ty, kRuntimeAbiVersion));
  store_field(1, observation_meta.slot_meta_words);
  store_field(2, observation_meta.slot_meta_count);
  // Process metadata: constructor-produced word table + string pool.
  store_field(3, process_meta.words);
  store_field(4, process_meta.word_count);
  store_field(5, process_meta.pool);
  store_field(6, process_meta.pool_size);
  store_field(7, meta_globals.back_edge_site_meta_words);
  store_field(
      8,
      llvm::ConstantInt::get(i32_ty, meta_globals.back_edge_site_meta_count));
  store_field(9, meta_globals.back_edge_site_meta_pool);
  store_field(
      10, llvm::ConstantInt::get(
              i32_ty, meta_globals.back_edge_site_meta_pool_size));
  store_field(11, conn_desc_ptr);
  store_field(12, conn_desc_count);
  // Comb kernel metadata: constructor-produced.
  store_field(13, dispatch_meta.comb_words);
  store_field(14, dispatch_meta.comb_word_count);
  store_field(15, llvm::ConstantInt::get(i32_ty, feature_flags));
  store_field(16, wait_site_meta.words_ptr);
  store_field(17, llvm::ConstantInt::get(i32_ty, wait_site_meta.count));
  store_field(18, observation_meta.trace_meta_words);
  store_field(19, observation_meta.trace_meta_word_count);
  store_field(20, observation_meta.trace_meta_pool);
  store_field(21, observation_meta.trace_meta_pool_size);

  if (signal_trace_path.empty()) {
    store_field(22, null_ptr);
  } else {
    store_field(
        22,
        builder.CreateGlobalStringPtr(signal_trace_path, "signal_trace_path"));
  }

  // Process trigger metadata: constructor-produced.
  store_field(23, dispatch_meta.trigger_words);
  store_field(24, dispatch_meta.trigger_word_count);

  // Dispatch partition boundary (connection/module process split).
  // Currently computed from compile-time layout topology; will move
  // to constructor result extraction when remaining dispatch partition
  // ownership migrates.
  store_field(25, llvm::ConstantInt::get(i32_ty, num_connection));

  // Design-state binding.
  store_field(26, design_state);

  // Instance pointer array for slot storage resolution.
  store_field(27, observation_meta.instance_ptrs);
  store_field(28, observation_meta.instance_count);

  // R4: Per-instance metadata bundles for engine-derived registries.
  store_field(29, observation_meta.instance_bundles);
  store_field(30, observation_meta.instance_bundle_count);
  store_field(31, llvm::ConstantInt::get(i32_ty, 0));

  // A1b: Immediate cover site count.
  store_field(32, llvm::ConstantInt::get(i32_ty, num_immediate_cover_sites));
  store_field(33, llvm::ConstantInt::get(i32_ty, 0));

  // D6d: Simulation-global precision power.
  auto* i8_ty = llvm::Type::getInt8Ty(context.GetLlvmContext());
  store_field(34, llvm::ConstantInt::get(i8_ty, global_precision_power));

  // A2: Deferred assertion site metadata table.
  store_field(
      35, deferred_site_meta_global != nullptr
              ? deferred_site_meta_global
              : llvm::ConstantPointerNull::get(ptr_ty));
  store_field(36, llvm::ConstantInt::get(i32_ty, num_deferred_assertion_sites));
  store_field(37, llvm::ConstantInt::get(i32_ty, 0));

  // L8a: Named event count.
  store_field(38, llvm::ConstantInt::get(i32_ty, num_events));
  store_field(39, llvm::ConstantInt::get(i32_ty, 0));

  // v25: Filesystem base directory.
  store_field(40, fs_base_dir_str);

  return abi_alloca;
}

void EmitRunSimulation(
    Context& context, llvm::Value* states_array, llvm::Value* num_processes,
    const PlusargsSetup& plusargs, llvm::Value* abi_alloca,
    llvm::Value* run_session_ptr) {
  auto& builder = context.GetBuilder();

  builder.CreateCall(
      context.GetLyraRunSimulation(),
      {states_array, num_processes, plusargs.array, plusargs.count, abi_alloca,
       run_session_ptr});
}

void EmitMainExit(
    Context& context, const CuFacts& facts, llvm::Value* design_state,
    const EmitDesignMainInput& input, llvm::BasicBlock* exit_block,
    llvm::Value* abi_alloca, const RealizationEmissionResult& ctor_result,
    const InspectionPlan& inspection_plan, llvm::Value* run_session_ptr) {
  auto& builder = context.GetBuilder();
  auto& ctx = context.GetLlvmContext();

  builder.CreateBr(exit_block);
  builder.SetInsertPoint(exit_block);

  // String slot release is now handled by Engine::ReleaseStringSlots()
  // inside LyraRunSimulation, before engine destruction.

  // Backend-owned variable inspection from typed placement descriptors.
  if (!inspection_plan.IsEmpty()) {
    EmitVariableInspection(
        context, facts, inspection_plan, design_state, abi_alloca,
        run_session_ptr);
  }

  if (input.hooks != nullptr) {
    input.hooks->EmitPostSimulationReports(
        context, design_state, abi_alloca, run_session_ptr);
  }

  if (ctor_result.result_handle != nullptr) {
    builder.CreateCall(ctor_result.destroy_fn, {ctor_result.result_handle});
  }

  // AOT: destroy the RunSession we created in CreateMainFunction.
  // JIT: caller owns the RunSession, no cleanup needed here.
  if (input.main_abi == lowering::mir_to_llvm::MainAbi::kArgvForwarding) {
    builder.CreateCall(context.GetLyraDestroyRunSession(), {run_session_ptr});
  }

  builder.CreateRet(llvm::ConstantInt::get(ctx, llvm::APInt(32, 0)));
}

}  // namespace

auto BuildEmitDesignMainInput(const lowering::mir_to_llvm::LoweringInput& input)
    -> EmitDesignMainInput {
  return {
      .design = input.design,
      .design_arena = input.mir_arena,
      .diag_ctx = input.diag_ctx,
      .source_manager = input.source_manager,
      .origin_provenance = input.origin_provenance,
      .hooks = input.hooks,
      .main_abi = input.main_abi,
      .fs_base_dir = input.fs_base_dir,
      .plusargs = input.plusargs,
      .feature_flags = input.feature_flags,
      .signal_trace_path = input.signal_trace_path,
      .iteration_limit = input.iteration_limit,
  };
}

auto EmitDesignMain(
    lowering::mir_to_llvm::CodegenSession& session,
    const EmitDesignMainInput& input) -> Result<LoweringReport> {
  auto& context = *session.context;
  const auto& facts = *session.facts;
  const auto& layout = *session.layout;
  const auto& process_funcs = session.process_funcs;
  size_t num_init = session.num_init_processes;
  const auto& realization = session.realization;

  auto [main_func, exit_block, design_state, run_session_ptr] =
      CreateMainFunction(context, input.main_abi);

  EmitDesignStateZero(context, design_state);

  auto num_simulation = static_cast<uint32_t>(process_funcs.size() - num_init);
  auto num_kernelized =
      static_cast<uint32_t>(layout.connection_kernel_entries.size());

  auto* null_abi_ptr = llvm::ConstantPointerNull::get(
      llvm::PointerType::getUnqual(context.GetLlvmContext()));
  llvm::Value* abi_for_exit = null_abi_ptr;
  RealizationEmissionResult ctor_result{};

  if (num_simulation > 0 || num_kernelized > 0) {
    auto& ctx = context.GetLlvmContext();
    auto* ptr_ty = llvm::PointerType::getUnqual(ctx);
    auto* i32_ty = llvm::Type::getInt32Ty(ctx);
    auto* null_ptr =
        llvm::ConstantPointerNull::get(llvm::cast<llvm::PointerType>(ptr_ty));

    // Default to null for zero-simulation case.
    llvm::Value* states_array = null_ptr;
    llvm::Value* num_total_val = llvm::ConstantInt::get(i32_ty, 0);

    if (num_simulation > 0) {
      ctor_result = EmitRealizationAndConstructor(
          context, facts, layout, design_state, session.body_compiled_funcs,
          process_funcs, num_init, realization.construction_program);
      states_array = ctor_result.states_array;
      num_total_val = ctor_result.num_total;
    }

    // Runtime init and init processes run AFTER constructor (which owns
    // design-state init) but BEFORE simulation.
    auto* fs_base_dir_str = EmitRuntimeInit(context, main_func, input);

    if (input.hooks != nullptr) {
      input.hooks->OnAfterInitializeDesignState(context, design_state);
    }

    EmitInitProcesses(
        context, facts, design_state, layout.processes, process_funcs,
        num_init);

    if (input.hooks != nullptr) {
      input.hooks->OnBeforeRunSimulation(context, design_state);
    }

    auto plusargs = BuildPlusargs(context, main_func, input);

    auto metadata_outputs = BuildDesignMetadataOutputs(
        context, layout, input, session.back_edge_origins);
    auto& meta_globals = metadata_outputs.globals;

    auto wait_site_meta = EmitWaitSiteMetaTable(context, session.wait_sites);

    // Fallback: zero metadata for the no-constructor topology case
    // (num_simulation == 0 but num_kernelized > 0). When the constructor is
    // active, constructor-produced metadata replaces this.
    RealizationEmissionResult::ProcessMeta process_meta_for_abi{
        .words = null_ptr,
        .word_count = llvm::ConstantInt::get(i32_ty, 0),
        .pool = null_ptr,
        .pool_size = llvm::ConstantInt::get(i32_ty, 0),
    };
    RealizationEmissionResult::DispatchMeta dispatch_meta_for_abi{
        .trigger_words = null_ptr,
        .trigger_word_count = llvm::ConstantInt::get(i32_ty, 0),
        .comb_words = null_ptr,
        .comb_word_count = llvm::ConstantInt::get(i32_ty, 0),
    };
    RealizationEmissionResult::ObservationMeta observation_meta_for_abi{
        .slot_meta_words = null_ptr,
        .slot_meta_count = llvm::ConstantInt::get(i32_ty, 0),
        .trace_meta_words = null_ptr,
        .trace_meta_word_count = llvm::ConstantInt::get(i32_ty, 0),
        .trace_meta_pool = null_ptr,
        .trace_meta_pool_size = llvm::ConstantInt::get(i32_ty, 0),
        .instance_ptrs = null_ptr,
        .instance_count = llvm::ConstantInt::get(i32_ty, 0),
        .instance_bundles = null_ptr,
        .instance_bundle_count = llvm::ConstantInt::get(i32_ty, 0),
    };
    if (ctor_result.result_handle != nullptr) {
      process_meta_for_abi = ctor_result.process_meta;
      dispatch_meta_for_abi = ctor_result.dispatch_meta;
      observation_meta_for_abi = ctor_result.observation_meta;
    }

    // Build per-site origin entry and cover base mappings from body-owned
    // site counts. Each site's origin/cover-base is determined by its
    // owning body.
    std::vector<const lowering::BodyOriginProvenance::Entry*>
        site_origin_entries;
    std::vector<uint32_t> site_cover_bases;
    {
      auto num_sites = input.design->deferred_assertion_sites.size();
      site_origin_entries.reserve(num_sites);
      site_cover_bases.reserve(num_sites);
      uint32_t cover_base = 0;
      for (const auto& body : input.design->module_bodies) {
        const auto* entry = (input.origin_provenance != nullptr)
                                ? input.origin_provenance->Find(&body)
                                : nullptr;
        for (size_t s = 0; s < body.deferred_assertion_sites.size(); ++s) {
          site_origin_entries.push_back(entry);
          site_cover_bases.push_back(cover_base);
        }
        cover_base += static_cast<uint32_t>(body.immediate_cover_sites.size());
      }
    }
    auto* deferred_site_meta_global = EmitDeferredAssertionSiteMetaGlobal(
        context, facts, input.design->deferred_assertion_sites,
        session.deferred_site_artifacts, site_origin_entries, site_cover_bases);
    // Connection descriptors: materialize in the constructor result,
    // then pass the RuntimeConnectionDescriptor* to the ABI.
    llvm::Value* conn_ptr = null_ptr;
    llvm::Value* conn_count = llvm::ConstantInt::get(i32_ty, 0);
    if (meta_globals.conn_desc_count > 0 &&
        ctor_result.result_handle != nullptr) {
      auto* set_fn = context.GetModule()
                         .getOrInsertFunction(
                             "LyraConstructionResultSetConnectionDescriptors",
                             llvm::FunctionType::get(
                                 llvm::Type::getVoidTy(ctx),
                                 {ptr_ty, ptr_ty, i32_ty}, false))
                         .getCallee();
      auto* get_fn = context.GetModule()
                         .getOrInsertFunction(
                             "LyraConstructionResultGetConnectionDescriptors",
                             llvm::FunctionType::get(ptr_ty, {ptr_ty}, false))
                         .getCallee();
      auto* get_count_fn =
          context.GetModule()
              .getOrInsertFunction(
                  "LyraConstructionResultGetConnectionDescriptorCount",
                  llvm::FunctionType::get(i32_ty, {ptr_ty}, false))
              .getCallee();
      auto& bld = context.GetBuilder();
      bld.CreateCall(
          llvm::cast<llvm::Function>(set_fn),
          {ctor_result.result_handle, meta_globals.conn_desc_table,
           llvm::ConstantInt::get(i32_ty, meta_globals.conn_desc_count)});
      conn_ptr = bld.CreateCall(
          llvm::cast<llvm::Function>(get_fn), {ctor_result.result_handle},
          "conn_descs_ptr");
      conn_count = bld.CreateCall(
          llvm::cast<llvm::Function>(get_count_fn), {ctor_result.result_handle},
          "conn_descs_count");
    }
    auto* abi_alloca = BuildRuntimeAbi(
        context, meta_globals, wait_site_meta, 0, input.feature_flags,
        input.signal_trace_path, design_state, process_meta_for_abi,
        dispatch_meta_for_abi, observation_meta_for_abi,
        static_cast<uint32_t>(input.design->immediate_cover_sites.size()),
        input.design->global_precision_power, deferred_site_meta_global,
        static_cast<uint32_t>(input.design->deferred_assertion_sites.size()),
        static_cast<uint32_t>(input.design->max_body_local_events),
        fs_base_dir_str, conn_ptr, conn_count);
    abi_for_exit = abi_alloca;

    EmitRunSimulation(
        context, states_array, num_total_val, plusargs, abi_alloca,
        run_session_ptr);

  } else {
    // No simulation processes or kernelized connections. Still need
    // runtime init and init process execution.
    EmitRuntimeInit(context, main_func, input);

    if (input.hooks != nullptr) {
      input.hooks->OnAfterInitializeDesignState(context, design_state);
    }

    EmitInitProcesses(
        context, facts, design_state, layout.processes, process_funcs,
        num_init);

    if (input.hooks != nullptr) {
      input.hooks->OnBeforeRunSimulation(context, design_state);
    }
  }

  // Build typed inspection plan from session data.
  // Backend owns placement construction; test framework only provides
  // identity (name + slot_id) via GetTrackedVariables().
  InspectionPlan inspection_plan;
  if (input.hooks != nullptr) {
    auto refs = input.hooks->GetTrackedVariables();
    if (!refs.empty()) {
      inspection_plan = BuildInspectionPlan(*session.layout, refs);
    }
  }

  EmitMainExit(
      context, facts, design_state, input, exit_block, abi_for_exit,
      ctor_result, inspection_plan, run_session_ptr);

  return LoweringReport{};
}

auto LowerMirToLlvm(const LoweringInput& input) -> Result<LoweringResult> {
  auto session_result = CompileDesignProcesses(input);
  if (!session_result) return std::unexpected(session_result.error());

  auto emit_result =
      EmitDesignMain(*session_result, BuildEmitDesignMainInput(input));
  if (!emit_result) return std::unexpected(emit_result.error());

  return FinalizeModule(std::move(*session_result), std::move(*emit_result));
}

}  // namespace lyra::lowering::mir_to_llvm
