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
#include "lyra/mir/construction_input.hpp"
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
    Context& context, const CompiledModuleSpecInput& input,
    const mir::ConstructionInput* construction) -> Result<CompiledModuleSpec> {
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
  if (!body.resolved_external_ref_bindings.empty()) {
    ext_ref_env = Context::ExternalRefResolutionEnv{
        .bindings = &body.resolved_external_ref_bindings,
        .construction = construction};
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
      .deferred_sites = input.deferred_sites,
      .deferred_site_base_index = input.deferred_site_base_index,
      .deferred_callee_info = {},
      .declared_functions = {},
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

  // Step 4: Capture callee products while session state is still valid.
  product.deferred_callee_info =
      CaptureDeferredCalleeInfoForBody(context, input.deferred_sites);
  product.declared_functions = std::move(declared_funcs);

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
  products.deferred_site_callee_info.resize(
      input.design->deferred_assertion_sites.size());

  for (const auto& spec_input : spec_plan.inputs) {
    auto product =
        CompileModuleSpecSession(context, spec_input, input.construction);
    if (!product) return std::unexpected(product.error());

    // Merge DPI export callees.
    if (input.dpi_export_wrappers != nullptr) {
      for (const auto& desc : *input.dpi_export_wrappers) {
        if (desc.target.scope_kind != mir::DpiExportScopeKind::kModule) {
          continue;
        }
        if (desc.target.module_target.body != product->body) {
          continue;
        }
        auto func_id = desc.target.module_target.function_id;
        for (const auto& [fid, llvm_func] : product->declared_functions) {
          if (fid != func_id) continue;
          const auto& callee = product->body->arena[func_id];
          products.module_export_callees[desc.target.module_target] = {
              .llvm_func = llvm_func,
              .accepts_decision_owner =
                  callee.abi_contract.accepts_decision_owner,
          };
          break;
        }
      }
    }

    // Merge deferred callee info.
    for (uint32_t di = 0; di < product->deferred_callee_info.size(); ++di) {
      products
          .deferred_site_callee_info[product->deferred_site_base_index + di] =
          std::move(product->deferred_callee_info[di]);
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
