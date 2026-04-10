#include "lyra/llvm_backend/spec_session.hpp"

#include <cstdint>
#include <format>
#include <iterator>
#include <optional>
#include <span>
#include <unordered_set>
#include <utility>
#include <variant>
#include <vector>

#include "absl/container/flat_hash_map.h"
#include "lyra/common/internal_error.hpp"
#include "lyra/llvm_backend/context.hpp"
#include "lyra/llvm_backend/deferred_thunk_abi.hpp"
#include "lyra/llvm_backend/execution_mode.hpp"
#include "lyra/llvm_backend/lower.hpp"
#include "lyra/llvm_backend/process.hpp"
#include "lyra/lowering/origin_map_lookup.hpp"
#include "lyra/mir/arena.hpp"
#include "lyra/mir/deferred_assertion_site.hpp"
#include "lyra/mir/routine.hpp"

namespace lyra::lowering::mir_to_llvm {

namespace {

auto CaptureDeferredCalleeInfoForBody(
    Context& context,
    std::span<const mir::DeferredAssertionSiteInfo> body_sites)
    -> std::vector<DeferredSiteCalleeInfo> {
  std::vector<DeferredSiteCalleeInfo> result(body_sites.size());
  auto capture_callee = [&](const mir::DeferredUserCallAction* action)
      -> std::optional<DeferredCalleeBackendInfo> {
    if (action == nullptr) return std::nullopt;
    return DeferredCalleeBackendInfo{
        .llvm_func = context.GetDeclaredFunction(action->callee),
        .is_module_scoped = context.GetMirArena()[action->callee]
                                .abi_contract.needs_module_binding,
    };
  };

  for (uint32_t di = 0; di < body_sites.size(); ++di) {
    const auto& site = body_sites[di];
    if (auto info = capture_callee(GetDeferredPassUserCallAction(site))) {
      result[di].pass_callee = *info;
    }
    if (auto info = capture_callee(GetDeferredFailUserCallAction(site))) {
      result[di].fail_callee = *info;
    }
  }
  return result;
}

}  // namespace

auto CompileModuleSpecSession(
    Context& context, const CompiledModuleSpecInput& input)
    -> Result<CompiledModuleSpec> {
  const auto& body = *input.body;
  Context::ArenaScope arena_scope(context, &body.arena);

  std::optional<lowering::BodyLocalOriginResolver> body_resolver;
  std::optional<lowering::DiagnosticContext> body_diag_ctx;
  std::optional<Context::DiagnosticScope> diag_scope;
  if (input.origin_entry != nullptr && input.origin_entry->arena != nullptr) {
    body_resolver.emplace(
        input.origin_entry->origins, *input.origin_entry->arena);
    body_diag_ctx.emplace(*body_resolver);
    diag_scope.emplace(context, &*body_diag_ctx);
  }

  std::optional<Context::ExternalRefResolutionEnv> ext_ref_env;
  if (!body.external_refs.empty()) {
    ext_ref_env =
        Context::ExternalRefResolutionEnv{.recipes = &body.external_refs};
  }
  Context::SpecLocalScope spec_scope(
      context, input.spec_slot_info, input.connection_notification_mask,
      std::move(ext_ref_env));

  BodySiteContext site_ctx{
      .deferred_site_base = input.deferred_site_base_index,
      .cover_site_base = input.cover_site_base_index,
      .deferred_sites = input.deferred_sites,
  };

  // Step 1: Declare all body functions and build session-local lookup.
  std::vector<std::pair<mir::FunctionId, llvm::Function*>> declared_funcs;
  absl::flat_hash_map<mir::FunctionId, llvm::Function*> declared_func_map;
  std::unordered_set<uint32_t> seen_func_ids;
  for (mir::FunctionId func_id : input.functions) {
    if (!seen_func_ids.insert(func_id.value).second) continue;
    auto llvm_func_or_err = DeclareMirFunction(
        context, func_id,
        std::format("{}_func_{}", input.name_prefix, func_id.value));
    if (!llvm_func_or_err) return std::unexpected(llvm_func_or_err.error());
    declared_funcs.emplace_back(func_id, *llvm_func_or_err);
    declared_func_map[func_id] = *llvm_func_or_err;
  }

  Context::DeclaredFunctionScope func_scope(context, declared_func_map);

  // Step 2: Define all body functions.
  for (const auto& [func_id, llvm_func] : declared_funcs) {
    auto result = DefineMirFunction(context, func_id, llvm_func, site_ctx);
    if (!result) return std::unexpected(result.error());
  }

  // Step 3: Codegen all body-owned processes.
  CompiledModuleSpec product{
      .body = input.body,
      .process_functions = {},
      .wait_sites = {},
      .process_triggers = {},
      .deferred_site_base_index = input.deferred_site_base_index,
      .deferred_artifacts = {},
      .module_export_entries = {},
  };

  for (const auto& proc_view : input.view.processes) {
    context.SetCurrentProcess(proc_view.layout_process_index);

    const auto& mir_process = body.arena[proc_view.process_id];
    auto func_result = GenerateSharedProcessFunction(
        context, mir_process, proc_view.func_name, site_ctx);
    if (!func_result) return std::unexpected(func_result.error());

    product.process_functions.push_back(func_result->function);
    product.process_triggers.push_back(std::move(func_result->process_trigger));

    product.wait_sites.insert(
        product.wait_sites.end(),
        std::make_move_iterator(func_result->wait_sites.begin()),
        std::make_move_iterator(func_result->wait_sites.end()));
  }

  // Step 4: Compile deferred assertion thunks while session state is valid.
  // Callee captures and thunk codegen happen here, inside the session,
  // because declared functions are in scope.
  if (!input.deferred_sites.empty()) {
    auto callee_info =
        CaptureDeferredCalleeInfoForBody(context, input.deferred_sites);
    auto artifacts = CompileDeferredAssertionArtifacts(
        context, input.deferred_sites, callee_info, input.name_prefix);
    if (!artifacts) return std::unexpected(artifacts.error());
    product.deferred_artifacts = std::move(*artifacts);
  }

  // Step 5: Resolve module-scoped DPI export callees while declared functions
  // and MIR ABI contracts are in scope.
  for (const auto& target : input.module_export_targets) {
    auto it = declared_func_map.find(target.function_id);
    if (it == declared_func_map.end()) continue;
    const auto& callee = body.arena[target.function_id];
    product.module_export_entries.push_back(
        ResolvedModuleExportEntry{
            .wrapper_index = target.wrapper_index,
            .info =
                {
                    .llvm_func = it->second,
                    .accepts_decision_owner =
                        callee.abi_contract.accepts_decision_owner,
                },
        });
  }

  return product;
}

auto CompileGlobalFunctions(Context& context, const LoweringInput& input)
    -> Result<void> {
  std::vector<mir::FunctionId> global_func_ids;
  std::unordered_set<uint32_t> seen_func_ids;

  for (const auto& element : input.design->elements) {
    const auto* pkg = std::get_if<mir::Package>(&element);
    if (pkg == nullptr) continue;
    for (mir::FunctionId func_id : pkg->functions) {
      if (seen_func_ids.insert(func_id.value).second) {
        global_func_ids.push_back(func_id);
      }
    }
  }
  for (mir::FunctionId func_id : input.design->generated_functions) {
    if (seen_func_ids.insert(func_id.value).second) {
      global_func_ids.push_back(func_id);
    }
  }

  absl::flat_hash_map<mir::FunctionId, llvm::Function*> global_func_map;
  global_func_map.reserve(global_func_ids.size());
  for (mir::FunctionId func_id : global_func_ids) {
    auto llvm_func_or_err = DeclareMirFunction(
        context, func_id, std::format("global_func_{}", func_id.value));
    if (!llvm_func_or_err) return std::unexpected(llvm_func_or_err.error());
    global_func_map[func_id] = *llvm_func_or_err;
  }

  Context::DeclaredFunctionScope func_scope(context, global_func_map);
  for (const auto& [func_id, llvm_func] : global_func_map) {
    BodySiteContext no_sites;
    auto result = DefineMirFunction(context, func_id, llvm_func, no_sites);
    if (!result) return std::unexpected(result.error());

    const auto& func = (*input.mir_arena)[func_id];
    if (func.canonical_symbol) {
      context.RegisterDesignFunction(func.canonical_symbol, func_id, llvm_func);
    }
  }

  return {};
}

auto CompileSpecializations(
    Context& context, const LoweringInput& input, const SpecPlan& spec_plan)
    -> Result<SpecializationProducts> {
  SpecializationProducts products;
  products.deferred_site_artifacts.resize(
      input.design->deferred_assertion_sites.size());
  if (input.dpi_export_wrappers != nullptr) {
    products.module_export_callees.resize(input.dpi_export_wrappers->size());
  }

  for (const auto& spec_input : spec_plan.inputs) {
    auto product = CompileModuleSpecSession(context, spec_input);
    if (!product) return std::unexpected(product.error());

    // Splice per-body module export callees into positional array.
    for (const auto& entry : product->module_export_entries) {
      products.module_export_callees[entry.wrapper_index] = entry.info;
    }

    // Merge per-body deferred artifacts into design-global array.
    for (uint32_t di = 0; di < product->deferred_artifacts.size(); ++di) {
      products.deferred_site_artifacts[product->deferred_site_base_index + di] =
          std::move(product->deferred_artifacts[di]);
    }

    products.body_to_process_triggers.try_emplace(
        product->body, std::move(product->process_triggers));

    auto [it, inserted] = products.body_to_compiled_funcs.try_emplace(
        product->body, std::move(product->process_functions));
    if (!inserted) {
      throw common::InternalError(
          "CompileSpecializations",
          "duplicate body in specialization products");
    }
    products.wait_sites.insert(
        products.wait_sites.end(),
        std::make_move_iterator(product->wait_sites.begin()),
        std::make_move_iterator(product->wait_sites.end()));
  }

  return products;
}

}  // namespace lyra::lowering::mir_to_llvm
