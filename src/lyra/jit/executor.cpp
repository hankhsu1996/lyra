#include "lyra/jit/executor.hpp"

#include <cstddef>
#include <cstdint>
#include <filesystem>
#include <memory>
#include <optional>
#include <string>
#include <string_view>
#include <utility>
#include <variant>
#include <vector>

#include <llvm/Analysis/CGSCCPassManager.h>
#include <llvm/Analysis/LoopAnalysisManager.h>
#include <llvm/ExecutionEngine/JITSymbol.h>
#include <llvm/ExecutionEngine/Orc/Core.h>
#include <llvm/ExecutionEngine/Orc/ExecutionUtils.h>
#include <llvm/ExecutionEngine/Orc/IRTransformLayer.h>
#include <llvm/ExecutionEngine/Orc/LLJIT.h>
#include <llvm/ExecutionEngine/Orc/Shared/ExecutorAddress.h>
#include <llvm/ExecutionEngine/Orc/ThreadSafeModule.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/PassManager.h>
#include <llvm/Passes/PassBuilder.h>
#include <llvm/Support/Error.h>
#include <llvm/Support/TargetSelect.h>
#include <llvm/Transforms/Coroutines/CoroCleanup.h>
#include <llvm/Transforms/Coroutines/CoroEarly.h>
#include <llvm/Transforms/Coroutines/CoroSplit.h>

#include "lyra/backend/llvm/emit.hpp"
#include "lyra/backend/llvm/runtime_entry.hpp"
#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/compiler/unit_metadata.hpp"
#include "lyra/diag/diag_code.hpp"
#include "lyra/lir/class_query.hpp"
#include "lyra/lir/compilation_unit.hpp"
#include "lyra/lir/type.hpp"
#include "lyra/lir/type_id.hpp"
#include "lyra/runtime/closure.hpp"
#include "lyra/runtime/design.hpp"
#include "lyra/runtime/generated_call_scope.hpp"
#include "lyra/runtime/hierarchy_segment.hpp"
#include "lyra/runtime/jit_execution.hpp"
#include "lyra/runtime/managed_object.hpp"
#include "lyra/runtime/member_storage.hpp"
#include "lyra/runtime/plusargs.hpp"
#include "lyra/runtime/runtime.hpp"
#include "lyra/runtime/scope.hpp"
#include "lyra/runtime/scope_program.hpp"
#include "lyra/runtime/simulation_entry.hpp"
#include "lyra/support/value_domain.hpp"

namespace lyra::jit {

namespace {

template <typename T>
auto Unwrap(llvm::Expected<T> value, std::string_view what) -> T {
  if (!value) {
    throw InternalError(
        "jit executor: " + std::string(what) + ": " +
        llvm::toString(value.takeError()));
  }
  return std::move(*value);
}

void Check(llvm::Error error, std::string_view what) {
  if (error) {
    throw InternalError(
        "jit executor: " + std::string(what) + ": " +
        llvm::toString(std::move(error)));
  }
}

// Splits every generated coroutine body into its resumable form before the
// module is compiled. A process body reaches the JIT as an ordinary function
// carrying coroutine intrinsics; the coroutine passes derive its frame, its
// resume state, and the values that must survive a suspension. That derivation
// is theirs -- the compiler states where a body suspends, never how it resumes.
void LowerCoroutines(llvm::orc::LLJIT& jit) {
  jit.getIRTransformLayer().setTransform(
      [](llvm::orc::ThreadSafeModule module,
         const llvm::orc::MaterializationResponsibility&)
          -> llvm::Expected<llvm::orc::ThreadSafeModule> {
        module.withModuleDo([](llvm::Module& ir) {
          llvm::PassBuilder builder;
          llvm::LoopAnalysisManager loops;
          llvm::FunctionAnalysisManager functions;
          llvm::CGSCCAnalysisManager call_graph;
          llvm::ModuleAnalysisManager modules;
          builder.registerModuleAnalyses(modules);
          builder.registerCGSCCAnalyses(call_graph);
          builder.registerFunctionAnalyses(functions);
          builder.registerLoopAnalyses(loops);
          builder.crossRegisterProxies(loops, functions, call_graph, modules);

          // Only the coroutine lowering runs: it is what makes a suspending
          // body executable, so it is a translation step, not an optimization
          // the module could also be correct without.
          llvm::ModulePassManager passes;
          passes.addPass(llvm::CoroEarlyPass());
          llvm::CGSCCPassManager split;
          split.addPass(llvm::CoroSplitPass());
          passes.addPass(
              llvm::createModuleToPostOrderCGSCCPassAdaptor(std::move(split)));
          passes.addPass(llvm::CoroCleanupPass());
          passes.run(ir, modules);
        });
        return module;
      });
}

// Binds the runtime ABI the generated module calls to the definitions linked
// into this process. Absolute addresses resolve every generated call without
// relying on the host's exported dynamic symbol table.
void DefineRuntimeAbi(llvm::orc::LLJIT& jit) {
  llvm::orc::SymbolMap symbols;
  auto add = [&](std::string_view name, auto* fn) {
    symbols[jit.getExecutionSession().intern(name)] =
        llvm::orc::ExecutorSymbolDef(
            llvm::orc::ExecutorAddr::fromPtr(fn),
            llvm::JITSymbolFlags::Exported | llvm::JITSymbolFlags::Callable);
  };
  add("lyra_rt_current_runtime", &lyra_rt_current_runtime);
  add("lyra_rt_files", &lyra_rt_files);
  add("lyra_rt_time_format", &lyra_rt_time_format);
  add("lyra_rt_set_time_format", &lyra_rt_set_time_format);
  add("lyra_rt_reset_time_format", &lyra_rt_reset_time_format);
  add("lyra_rt_file_open", &lyra_rt_file_open);
  add("lyra_rt_file_open_mode", &lyra_rt_file_open_mode);
  add("lyra_rt_file_close", &lyra_rt_file_close);
  add("lyra_rt_file_getc", &lyra_rt_file_getc);
  add("lyra_rt_file_gets", &lyra_rt_file_gets);
  add("lyra_rt_file_error", &lyra_rt_file_error);
  add("lyra_rt_file_read", &lyra_rt_file_read);
  add("lyra_rt_file_read_memory", &lyra_rt_file_read_memory);
  add("lyra_rt_file_ungetc", &lyra_rt_file_ungetc);
  add("lyra_rt_file_seek", &lyra_rt_file_seek);
  add("lyra_rt_file_rewind", &lyra_rt_file_rewind);
  add("lyra_rt_file_tell", &lyra_rt_file_tell);
  add("lyra_rt_file_eof", &lyra_rt_file_eof);
  add("lyra_rt_file_flush", &lyra_rt_file_flush);
  add("lyra_rt_file_flush_all", &lyra_rt_file_flush_all);
  add("lyra_rt_peek_buffered", &lyra_rt_peek_buffered);
  add("lyra_rt_advance_fd", &lyra_rt_advance_fd);
  add("lyra_rt_string_make", &lyra_rt_string_make);
  add("lyra_rt_make_print_literal_item", &lyra_rt_make_print_literal_item);
  add("lyra_rt_format", &lyra_rt_format);
  add("lyra_rt_writeln", &lyra_rt_writeln);
  add("lyra_rt_write", &lyra_rt_write);
  add("lyra_rt_diagnostic", &lyra_rt_diagnostic);
  add("lyra_rt_emit_info", &lyra_rt_emit_info);
  add("lyra_rt_emit_warning", &lyra_rt_emit_warning);
  add("lyra_rt_emit_error", &lyra_rt_emit_error);
  add("lyra_rt_emit_fatal", &lyra_rt_emit_fatal);
  add("lyra_rt_record_coverage", &lyra_rt_record_coverage);
  add("lyra_rt_enter_coroutine_borrowed_environment",
      &lyra_rt_enter_coroutine_borrowed_environment);
  add("lyra_rt_enter_coroutine_owned_environment",
      &lyra_rt_enter_coroutine_owned_environment);
  add("lyra_rt_await_coroutine", &lyra_rt_await_coroutine);
  add("lyra_rt_release_coroutine", &lyra_rt_release_coroutine);
  add("lyra_rt_register_initial", &lyra_rt_register_initial);
  add("lyra_rt_register_final", &lyra_rt_register_final);
  add("lyra_rt_spawn_all", &lyra_rt_spawn_all);
  add("lyra_rt_fork_wait_all", &lyra_rt_fork_wait_all);
  add("lyra_rt_fork_wait_first", &lyra_rt_fork_wait_first);
  add("lyra_rt_wait_fork", &lyra_rt_wait_fork);
  add("lyra_rt_disable_fork", &lyra_rt_disable_fork);
  add("lyra_rt_cancellation_for", &lyra_rt_cancellation_for);
  add("lyra_rt_is_cancelled", &lyra_rt_is_cancelled);
  add("lyra_rt_closure_make", &lyra_rt_closure_make);
  add("lyra_rt_object_make", &lyra_rt_object_make);
  add("lyra_rt_object_deref", &lyra_rt_object_deref);
  add("lyra_rt_object_member_addr", &lyra_rt_object_member_addr);
  add("lyra_rt_closure_capture", &lyra_rt_closure_capture);
  add("lyra_rt_submit_nba", &lyra_rt_submit_nba);
  add("lyra_rt_submit_postponed", &lyra_rt_submit_postponed);
  add("lyra_rt_submit_observed", &lyra_rt_submit_observed);
  add("lyra_rt_delay", &lyra_rt_delay);
  add("lyra_rt_delay_real", &lyra_rt_delay_real);
  add("lyra_rt_make_trigger", &lyra_rt_make_trigger);
  add("lyra_rt_wait_any", &lyra_rt_wait_any);
  add("lyra_rt_triggered", &lyra_rt_triggered);
  add("lyra_rt_trigger", &lyra_rt_trigger);
  add("lyra_rt_await", &lyra_rt_await);
  add("lyra_rt_enter_target", &lyra_rt_enter_target);
  add("lyra_rt_leave_target", &lyra_rt_leave_target);
  add("lyra_rt_disable", &lyra_rt_disable);
  add("lyra_rt_effect_names_target", &lyra_rt_effect_names_target);
  add("lyra_rt_invalidated_target", &lyra_rt_invalidated_target);
  add("lyra_rt_has_invalidated_target", &lyra_rt_has_invalidated_target);
  add("lyra_rt_settle_cancelled", &lyra_rt_settle_cancelled);
  add("lyra_rt_sim_time", &lyra_rt_sim_time);
  add("lyra_rt_stime", &lyra_rt_stime);
  add("lyra_rt_realtime", &lyra_rt_realtime);
  add("lyra_rt_finish", &lyra_rt_finish);
  add("lyra_rt_fatal_finish", &lyra_rt_fatal_finish);
  add("lyra_rt_run_host_command", &lyra_rt_run_host_command);
  add("lyra_rt_run_null_host_command", &lyra_rt_run_null_host_command);
  add("lyra_rt_test_plusargs", &lyra_rt_test_plusargs);
  add("lyra_rt_packed_value_plusargs", &lyra_rt_packed_value_plusargs);
  add("lyra_rt_string_value_plusargs", &lyra_rt_string_value_plusargs);
  add("lyra_rt_urandom", &lyra_rt_urandom);
  add("lyra_rt_urandom_seeded", &lyra_rt_urandom_seeded);
  add("lyra_rt_urandom_range", &lyra_rt_urandom_range);
  add("lyra_rt_random", &lyra_rt_random);
  add("lyra_rt_dist_uniform", &lyra_rt_dist_uniform);
  add("lyra_rt_dist_normal", &lyra_rt_dist_normal);
  add("lyra_rt_dist_exponential", &lyra_rt_dist_exponential);
  add("lyra_rt_dist_poisson", &lyra_rt_dist_poisson);
  add("lyra_rt_dist_chi_square", &lyra_rt_dist_chi_square);
  add("lyra_rt_dist_t", &lyra_rt_dist_t);
  add("lyra_rt_dist_erlang", &lyra_rt_dist_erlang);
  add("lyra_rt_make_segment", &lyra_rt_make_segment);
  add("lyra_rt_make_scope", &lyra_rt_make_scope);
  add("lyra_rt_hierarchical_path", &lyra_rt_hierarchical_path);
  add("lyra_rt_parent", &lyra_rt_parent);
  add("lyra_rt_add_owned_child", &lyra_rt_add_owned_child);
  add("lyra_rt_member_addr", &lyra_rt_member_addr);
  add("lyra_rt_sequence_make", &lyra_rt_sequence_make);
  add("lyra_rt_sequence_element", &lyra_rt_sequence_element);
  add("lyra_rt_register_signal", &lyra_rt_register_signal);
  add("lyra_rt_get_signal", &lyra_rt_get_signal);
  add("lyra_rt_resolve_visible_child", &lyra_rt_resolve_visible_child);
  add("lyra_rt_get_child", &lyra_rt_get_child);
  add("lyra_rt_packed_cell_alloc", &lyra_rt_packed_cell_alloc);
  add("lyra_rt_packed_cell_get", &lyra_rt_packed_cell_get);
  add("lyra_rt_packed_cell_initialize", &lyra_rt_packed_cell_initialize);
  add("lyra_rt_packed_cell_set", &lyra_rt_packed_cell_set);
  add("lyra_rt_string_cell_alloc", &lyra_rt_string_cell_alloc);
  add("lyra_rt_string_cell_get", &lyra_rt_string_cell_get);
  add("lyra_rt_string_cell_initialize", &lyra_rt_string_cell_initialize);
  add("lyra_rt_string_cell_set", &lyra_rt_string_cell_set);
  add("lyra_rt_real_cell_alloc", &lyra_rt_real_cell_alloc);
  add("lyra_rt_real_cell_get", &lyra_rt_real_cell_get);
  add("lyra_rt_real_cell_initialize", &lyra_rt_real_cell_initialize);
  add("lyra_rt_real_cell_set", &lyra_rt_real_cell_set);
  add("lyra_rt_shortreal_cell_alloc", &lyra_rt_shortreal_cell_alloc);
  add("lyra_rt_shortreal_cell_get", &lyra_rt_shortreal_cell_get);
  add("lyra_rt_shortreal_cell_initialize", &lyra_rt_shortreal_cell_initialize);
  add("lyra_rt_shortreal_cell_set", &lyra_rt_shortreal_cell_set);
  add("lyra_rt_packed_value_cell_alloc", &lyra_rt_packed_value_cell_alloc);
  add("lyra_rt_string_value_cell_alloc", &lyra_rt_string_value_cell_alloc);
  add("lyra_rt_packed_value_cell_store", &lyra_rt_packed_value_cell_store);
  add("lyra_rt_string_value_cell_store", &lyra_rt_string_value_cell_store);
  add("lyra_rt_packed_value_cell_load", &lyra_rt_packed_value_cell_load);
  add("lyra_rt_string_value_cell_load", &lyra_rt_string_value_cell_load);
  add("lyra_rt_packed_add", &lyra_rt_packed_add);
  add("lyra_rt_packed_replicate", &lyra_rt_packed_replicate);
  add("lyra_rt_packed_concat", &lyra_rt_packed_concat);
  add("lyra_rt_packed_sub", &lyra_rt_packed_sub);
  add("lyra_rt_packed_mul", &lyra_rt_packed_mul);
  add("lyra_rt_packed_div", &lyra_rt_packed_div);
  add("lyra_rt_packed_mod", &lyra_rt_packed_mod);
  add("lyra_rt_packed_and", &lyra_rt_packed_and);
  add("lyra_rt_packed_or", &lyra_rt_packed_or);
  add("lyra_rt_packed_xor", &lyra_rt_packed_xor);
  add("lyra_rt_packed_eq", &lyra_rt_packed_eq);
  add("lyra_rt_packed_ne", &lyra_rt_packed_ne);
  add("lyra_rt_packed_lt", &lyra_rt_packed_lt);
  add("lyra_rt_packed_le", &lyra_rt_packed_le);
  add("lyra_rt_packed_gt", &lyra_rt_packed_gt);
  add("lyra_rt_packed_ge", &lyra_rt_packed_ge);
  add("lyra_rt_packed_logical_and", &lyra_rt_packed_logical_and);
  add("lyra_rt_packed_logical_or", &lyra_rt_packed_logical_or);
  add("lyra_rt_packed_neg", &lyra_rt_packed_neg);
  add("lyra_rt_packed_not", &lyra_rt_packed_not);
  add("lyra_rt_packed_logical_not", &lyra_rt_packed_logical_not);
  add("lyra_rt_packed_inc", &lyra_rt_packed_inc);
  add("lyra_rt_packed_dec", &lyra_rt_packed_dec);
  add("lyra_rt_packed_to_bool", &lyra_rt_packed_to_bool);
  add("lyra_rt_packed_convert_from_packed",
      &lyra_rt_packed_convert_from_packed);
  add("lyra_rt_packed_from_bool", &lyra_rt_packed_from_bool);
  add("lyra_rt_packed_from_int", &lyra_rt_packed_from_int);
  add("lyra_rt_packed_to_int64", &lyra_rt_packed_to_int64);
  add("lyra_rt_packed_is_unknown", &lyra_rt_packed_is_unknown);
  add("lyra_rt_packed_count_bits", &lyra_rt_packed_count_bits);
  add("lyra_rt_make_packed_range", &lyra_rt_make_packed_range);
  add("lyra_rt_make_packed_type", &lyra_rt_make_packed_type);
  add("lyra_rt_packed_from_words", &lyra_rt_packed_from_words);
  add("lyra_rt_packed_from_string", &lyra_rt_packed_from_string);
  add("lyra_rt_packed_clog2", &lyra_rt_packed_clog2);
  add("lyra_rt_packed_pow", &lyra_rt_packed_pow);
  add("lyra_rt_packed_shift_left", &lyra_rt_packed_shift_left);
  add("lyra_rt_packed_logical_shift_right",
      &lyra_rt_packed_logical_shift_right);
  add("lyra_rt_packed_arithmetic_shift_right",
      &lyra_rt_packed_arithmetic_shift_right);
  add("lyra_rt_packed_bitwise_xnor", &lyra_rt_packed_bitwise_xnor);
  add("lyra_rt_packed_logical_implication",
      &lyra_rt_packed_logical_implication);
  add("lyra_rt_packed_logical_equivalence",
      &lyra_rt_packed_logical_equivalence);
  add("lyra_rt_packed_case_equal", &lyra_rt_packed_case_equal);
  add("lyra_rt_packed_wildcard_equals", &lyra_rt_packed_wildcard_equals);
  add("lyra_rt_packed_casez_equals", &lyra_rt_packed_casez_equals);
  add("lyra_rt_packed_casex_equals", &lyra_rt_packed_casex_equals);
  add("lyra_rt_packed_merge_conditional", &lyra_rt_packed_merge_conditional);
  add("lyra_rt_packed_reduction_and", &lyra_rt_packed_reduction_and);
  add("lyra_rt_packed_reduction_or", &lyra_rt_packed_reduction_or);
  add("lyra_rt_packed_reduction_xor", &lyra_rt_packed_reduction_xor);
  add("lyra_rt_packed_reduction_nand", &lyra_rt_packed_reduction_nand);
  add("lyra_rt_packed_reduction_nor", &lyra_rt_packed_reduction_nor);
  add("lyra_rt_packed_reduction_xnor", &lyra_rt_packed_reduction_xnor);
  add("lyra_rt_packed_to_owned", &lyra_rt_packed_to_owned);
  add("lyra_rt_packed_element", &lyra_rt_packed_element);
  add("lyra_rt_packed_with_element", &lyra_rt_packed_with_element);
  add("lyra_rt_packed_slice", &lyra_rt_packed_slice);
  add("lyra_rt_packed_with_slice", &lyra_rt_packed_with_slice);
  add("lyra_rt_string_from_packed_array", &lyra_rt_string_from_packed_array);
  add("lyra_rt_string_from_byte_array", &lyra_rt_string_from_byte_array);
  add("lyra_rt_string_count_bits", &lyra_rt_string_count_bits);
  add("lyra_rt_string_string_cstr", &lyra_rt_string_string_cstr);
  add("lyra_rt_string_len", &lyra_rt_string_len);
  add("lyra_rt_string_getc", &lyra_rt_string_getc);
  add("lyra_rt_string_element", &lyra_rt_string_element);
  add("lyra_rt_string_with_element", &lyra_rt_string_with_element);
  add("lyra_rt_string_toupper", &lyra_rt_string_toupper);
  add("lyra_rt_string_tolower", &lyra_rt_string_tolower);
  add("lyra_rt_string_compare", &lyra_rt_string_compare);
  add("lyra_rt_string_icompare", &lyra_rt_string_icompare);
  add("lyra_rt_string_substr", &lyra_rt_string_substr);
  add("lyra_rt_string_atoi", &lyra_rt_string_atoi);
  add("lyra_rt_string_atohex", &lyra_rt_string_atohex);
  add("lyra_rt_string_atooct", &lyra_rt_string_atooct);
  add("lyra_rt_string_atobin", &lyra_rt_string_atobin);
  add("lyra_rt_string_atoreal", &lyra_rt_string_atoreal);
  add("lyra_rt_string_putc", &lyra_rt_string_putc);
  add("lyra_rt_string_itoa", &lyra_rt_string_itoa);
  add("lyra_rt_string_hextoa", &lyra_rt_string_hextoa);
  add("lyra_rt_string_octtoa", &lyra_rt_string_octtoa);
  add("lyra_rt_string_bintoa", &lyra_rt_string_bintoa);
  add("lyra_rt_string_realtoa", &lyra_rt_string_realtoa);
  add("lyra_rt_string_scan_string", &lyra_rt_string_scan_string);
  add("lyra_rt_string_scan_file", &lyra_rt_string_scan_file);
  add("lyra_rt_string_add", &lyra_rt_string_add);
  add("lyra_rt_string_replicate", &lyra_rt_string_replicate);
  add("lyra_rt_string_concat", &lyra_rt_string_concat);
  add("lyra_rt_string_eq", &lyra_rt_string_eq);
  add("lyra_rt_string_case_equal", &lyra_rt_string_case_equal);
  add("lyra_rt_string_ne", &lyra_rt_string_ne);
  add("lyra_rt_string_lt", &lyra_rt_string_lt);
  add("lyra_rt_string_le", &lyra_rt_string_le);
  add("lyra_rt_string_gt", &lyra_rt_string_gt);
  add("lyra_rt_string_ge", &lyra_rt_string_ge);
  add("lyra_rt_make_format_spec_of_kind", &lyra_rt_make_format_spec_of_kind);
  add("lyra_rt_make_format_spec", &lyra_rt_make_format_spec);
  add("lyra_rt_packed_make_print_value_item",
      &lyra_rt_packed_make_print_value_item);
  add("lyra_rt_string_make_print_value_item",
      &lyra_rt_string_make_print_value_item);
  add("lyra_rt_real_add", &lyra_rt_real_add);
  add("lyra_rt_real_sub", &lyra_rt_real_sub);
  add("lyra_rt_real_mul", &lyra_rt_real_mul);
  add("lyra_rt_real_div", &lyra_rt_real_div);
  add("lyra_rt_real_neg", &lyra_rt_real_neg);
  add("lyra_rt_real_inc", &lyra_rt_real_inc);
  add("lyra_rt_real_dec", &lyra_rt_real_dec);
  add("lyra_rt_real_eq", &lyra_rt_real_eq);
  add("lyra_rt_real_ne", &lyra_rt_real_ne);
  add("lyra_rt_real_lt", &lyra_rt_real_lt);
  add("lyra_rt_real_le", &lyra_rt_real_le);
  add("lyra_rt_real_gt", &lyra_rt_real_gt);
  add("lyra_rt_real_ge", &lyra_rt_real_ge);
  add("lyra_rt_real_to_bool", &lyra_rt_real_to_bool);
  add("lyra_rt_real_pow", &lyra_rt_real_pow);
  add("lyra_rt_real_round", &lyra_rt_real_round);
  add("lyra_rt_real_real_value", &lyra_rt_real_real_value);
  add("lyra_rt_real_truncate", &lyra_rt_real_truncate);
  add("lyra_rt_real_to_bits", &lyra_rt_real_to_bits);
  add("lyra_rt_real_from_bits", &lyra_rt_real_from_bits);
  add("lyra_rt_real_ln", &lyra_rt_real_ln);
  add("lyra_rt_real_log10", &lyra_rt_real_log10);
  add("lyra_rt_real_exp", &lyra_rt_real_exp);
  add("lyra_rt_real_sqrt", &lyra_rt_real_sqrt);
  add("lyra_rt_real_floor", &lyra_rt_real_floor);
  add("lyra_rt_real_ceil", &lyra_rt_real_ceil);
  add("lyra_rt_real_sin", &lyra_rt_real_sin);
  add("lyra_rt_real_cos", &lyra_rt_real_cos);
  add("lyra_rt_real_tan", &lyra_rt_real_tan);
  add("lyra_rt_real_asin", &lyra_rt_real_asin);
  add("lyra_rt_real_acos", &lyra_rt_real_acos);
  add("lyra_rt_real_atan", &lyra_rt_real_atan);
  add("lyra_rt_real_atan2", &lyra_rt_real_atan2);
  add("lyra_rt_real_hypot", &lyra_rt_real_hypot);
  add("lyra_rt_real_sinh", &lyra_rt_real_sinh);
  add("lyra_rt_real_cosh", &lyra_rt_real_cosh);
  add("lyra_rt_real_tanh", &lyra_rt_real_tanh);
  add("lyra_rt_real_asinh", &lyra_rt_real_asinh);
  add("lyra_rt_real_acosh", &lyra_rt_real_acosh);
  add("lyra_rt_real_atanh", &lyra_rt_real_atanh);
  add("lyra_rt_real_const", &lyra_rt_real_const);
  add("lyra_rt_real_from_int", &lyra_rt_real_from_int);
  add("lyra_rt_real_convert_from_shortreal",
      &lyra_rt_real_convert_from_shortreal);
  add("lyra_rt_real_convert_from_real", &lyra_rt_real_convert_from_real);
  add("lyra_rt_real_value_cell_alloc", &lyra_rt_real_value_cell_alloc);
  add("lyra_rt_real_value_cell_store", &lyra_rt_real_value_cell_store);
  add("lyra_rt_real_value_cell_load", &lyra_rt_real_value_cell_load);
  add("lyra_rt_real_make_print_value_item",
      &lyra_rt_real_make_print_value_item);
  add("lyra_rt_shortreal_add", &lyra_rt_shortreal_add);
  add("lyra_rt_shortreal_sub", &lyra_rt_shortreal_sub);
  add("lyra_rt_shortreal_mul", &lyra_rt_shortreal_mul);
  add("lyra_rt_shortreal_div", &lyra_rt_shortreal_div);
  add("lyra_rt_shortreal_neg", &lyra_rt_shortreal_neg);
  add("lyra_rt_shortreal_inc", &lyra_rt_shortreal_inc);
  add("lyra_rt_shortreal_dec", &lyra_rt_shortreal_dec);
  add("lyra_rt_shortreal_eq", &lyra_rt_shortreal_eq);
  add("lyra_rt_shortreal_ne", &lyra_rt_shortreal_ne);
  add("lyra_rt_shortreal_lt", &lyra_rt_shortreal_lt);
  add("lyra_rt_shortreal_le", &lyra_rt_shortreal_le);
  add("lyra_rt_shortreal_gt", &lyra_rt_shortreal_gt);
  add("lyra_rt_shortreal_ge", &lyra_rt_shortreal_ge);
  add("lyra_rt_shortreal_to_bool", &lyra_rt_shortreal_to_bool);
  add("lyra_rt_shortreal_pow", &lyra_rt_shortreal_pow);
  add("lyra_rt_shortreal_round", &lyra_rt_shortreal_round);
  add("lyra_rt_shortreal_real_value", &lyra_rt_shortreal_real_value);
  add("lyra_rt_shortreal_to_bits", &lyra_rt_shortreal_to_bits);
  add("lyra_rt_shortreal_from_bits", &lyra_rt_shortreal_from_bits);
  add("lyra_rt_shortreal_const", &lyra_rt_shortreal_const);
  add("lyra_rt_shortreal_from_int", &lyra_rt_shortreal_from_int);
  add("lyra_rt_shortreal_convert_from_real",
      &lyra_rt_shortreal_convert_from_real);
  add("lyra_rt_shortreal_value_cell_alloc",
      &lyra_rt_shortreal_value_cell_alloc);
  add("lyra_rt_shortreal_value_cell_store",
      &lyra_rt_shortreal_value_cell_store);
  add("lyra_rt_shortreal_value_cell_load", &lyra_rt_shortreal_value_cell_load);
  add("lyra_rt_shortreal_make_print_value_item",
      &lyra_rt_shortreal_make_print_value_item);
  add("lyra_rt_chandle_eq", &lyra_rt_chandle_eq);
  add("lyra_rt_chandle_ne", &lyra_rt_chandle_ne);
  add("lyra_rt_chandle_case_equal", &lyra_rt_chandle_case_equal);
  add("lyra_rt_chandle_to_bool", &lyra_rt_chandle_to_bool);
  add("lyra_rt_packed_value_box", &lyra_rt_packed_value_box);
  add("lyra_rt_string_value_box", &lyra_rt_string_value_box);
  add("lyra_rt_real_value_box", &lyra_rt_real_value_box);
  add("lyra_rt_shortreal_value_box", &lyra_rt_shortreal_value_box);
  add("lyra_rt_chandle_value_box", &lyra_rt_chandle_value_box);
  add("lyra_rt_tuple_value_box", &lyra_rt_tuple_value_box);
  add("lyra_rt_dynarray_value_box", &lyra_rt_dynarray_value_box);
  add("lyra_rt_tuple_make", &lyra_rt_tuple_make);
  add("lyra_rt_tuple_extract", &lyra_rt_tuple_extract);
  add("lyra_rt_tuple_count_bits", &lyra_rt_tuple_count_bits);
  add("lyra_rt_tuple_update", &lyra_rt_tuple_update);
  add("lyra_rt_tuple_eq", &lyra_rt_tuple_eq);
  add("lyra_rt_tuple_ne", &lyra_rt_tuple_ne);
  add("lyra_rt_tuple_case_equal", &lyra_rt_tuple_case_equal);
  add("lyra_rt_tuple_is_unknown", &lyra_rt_tuple_is_unknown);
  add("lyra_rt_tuple_cell_alloc", &lyra_rt_tuple_cell_alloc);
  add("lyra_rt_tuple_cell_get", &lyra_rt_tuple_cell_get);
  add("lyra_rt_tuple_cell_initialize", &lyra_rt_tuple_cell_initialize);
  add("lyra_rt_tuple_cell_set", &lyra_rt_tuple_cell_set);
  add("lyra_rt_tuple_value_cell_alloc", &lyra_rt_tuple_value_cell_alloc);
  add("lyra_rt_tuple_value_cell_store", &lyra_rt_tuple_value_cell_store);
  add("lyra_rt_tuple_value_cell_load", &lyra_rt_tuple_value_cell_load);
  add("lyra_rt_union_value_box", &lyra_rt_union_value_box);
  add("lyra_rt_union_make", &lyra_rt_union_make);
  add("lyra_rt_union_extract", &lyra_rt_union_extract);
  add("lyra_rt_union_update", &lyra_rt_union_update);
  add("lyra_rt_union_eq", &lyra_rt_union_eq);
  add("lyra_rt_union_ne", &lyra_rt_union_ne);
  add("lyra_rt_union_case_equal", &lyra_rt_union_case_equal);
  add("lyra_rt_union_is_unknown", &lyra_rt_union_is_unknown);
  add("lyra_rt_union_cell_alloc", &lyra_rt_union_cell_alloc);
  add("lyra_rt_union_cell_get", &lyra_rt_union_cell_get);
  add("lyra_rt_union_cell_initialize", &lyra_rt_union_cell_initialize);
  add("lyra_rt_union_cell_set", &lyra_rt_union_cell_set);
  add("lyra_rt_union_value_cell_alloc", &lyra_rt_union_value_cell_alloc);
  add("lyra_rt_union_value_cell_store", &lyra_rt_union_value_cell_store);
  add("lyra_rt_union_value_cell_load", &lyra_rt_union_value_cell_load);
  add("lyra_rt_tagged_union_value_box", &lyra_rt_tagged_union_value_box);
  add("lyra_rt_tagged_union_make", &lyra_rt_tagged_union_make);
  add("lyra_rt_tagged_union_extract", &lyra_rt_tagged_union_extract);
  add("lyra_rt_tagged_union_update", &lyra_rt_tagged_union_update);
  add("lyra_rt_tagged_union_tag_matches", &lyra_rt_tagged_union_tag_matches);
  add("lyra_rt_tagged_union_eq", &lyra_rt_tagged_union_eq);
  add("lyra_rt_tagged_union_ne", &lyra_rt_tagged_union_ne);
  add("lyra_rt_tagged_union_case_equal", &lyra_rt_tagged_union_case_equal);
  add("lyra_rt_tagged_union_is_unknown", &lyra_rt_tagged_union_is_unknown);
  add("lyra_rt_tagged_union_cell_alloc", &lyra_rt_tagged_union_cell_alloc);
  add("lyra_rt_tagged_union_cell_get", &lyra_rt_tagged_union_cell_get);
  add("lyra_rt_tagged_union_cell_initialize",
      &lyra_rt_tagged_union_cell_initialize);
  add("lyra_rt_tagged_union_cell_set", &lyra_rt_tagged_union_cell_set);
  add("lyra_rt_tagged_union_value_cell_alloc",
      &lyra_rt_tagged_union_value_cell_alloc);
  add("lyra_rt_tagged_union_value_cell_store",
      &lyra_rt_tagged_union_value_cell_store);
  add("lyra_rt_tagged_union_value_cell_load",
      &lyra_rt_tagged_union_value_cell_load);
  add("lyra_rt_empty_default", &lyra_rt_empty_default);
  add("lyra_rt_empty_value_box", &lyra_rt_empty_value_box);
  add("lyra_rt_make_dynamic_array_default",
      &lyra_rt_make_dynamic_array_default);
  add("lyra_rt_make_dynamic_array_new", &lyra_rt_make_dynamic_array_new);
  add("lyra_rt_make_dynamic_array_new_copy",
      &lyra_rt_make_dynamic_array_new_copy);
  add("lyra_rt_dynarray_from_literal", &lyra_rt_dynarray_from_literal);
  add("lyra_rt_dynarray_element", &lyra_rt_dynarray_element);
  add("lyra_rt_dynarray_concat_element", &lyra_rt_dynarray_concat_element);
  add("lyra_rt_dynarray_concat_spread", &lyra_rt_dynarray_concat_spread);
  add("lyra_rt_dynarray_with_element", &lyra_rt_dynarray_with_element);
  add("lyra_rt_dynarray_delete", &lyra_rt_dynarray_delete);
  add("lyra_rt_dynarray_size", &lyra_rt_dynarray_size);
  add("lyra_rt_dynarray_eq", &lyra_rt_dynarray_eq);
  add("lyra_rt_dynarray_ne", &lyra_rt_dynarray_ne);
  add("lyra_rt_dynarray_case_equal", &lyra_rt_dynarray_case_equal);
  add("lyra_rt_dynarray_cell_alloc", &lyra_rt_dynarray_cell_alloc);
  add("lyra_rt_dynarray_cell_get", &lyra_rt_dynarray_cell_get);
  add("lyra_rt_dynarray_cell_initialize", &lyra_rt_dynarray_cell_initialize);
  add("lyra_rt_dynarray_cell_set", &lyra_rt_dynarray_cell_set);
  add("lyra_rt_dynarray_value_cell_alloc", &lyra_rt_dynarray_value_cell_alloc);
  add("lyra_rt_dynarray_value_cell_store", &lyra_rt_dynarray_value_cell_store);
  add("lyra_rt_dynarray_value_cell_load", &lyra_rt_dynarray_value_cell_load);
  add("lyra_rt_dynarray_count_bits", &lyra_rt_dynarray_count_bits);
  add("lyra_rt_unpackedarray_value_box", &lyra_rt_unpackedarray_value_box);
  add("lyra_rt_unpackedarray_from_literal",
      &lyra_rt_unpackedarray_from_literal);
  add("lyra_rt_unpackedarray_conform_size",
      &lyra_rt_unpackedarray_conform_size);
  add("lyra_rt_unpackedarray_from_string", &lyra_rt_unpackedarray_from_string);
  add("lyra_rt_queue_default", &lyra_rt_queue_default);
  add("lyra_rt_queue_default_bounded", &lyra_rt_queue_default_bounded);
  add("lyra_rt_queue_from_literal", &lyra_rt_queue_from_literal);
  add("lyra_rt_queue_from_literal_bounded",
      &lyra_rt_queue_from_literal_bounded);
  add("lyra_rt_queue_conform_bound", &lyra_rt_queue_conform_bound);
  add("lyra_rt_queue_element", &lyra_rt_queue_element);
  add("lyra_rt_queue_with_element", &lyra_rt_queue_with_element);
  add("lyra_rt_queue_slice", &lyra_rt_queue_slice);
  add("lyra_rt_queue_size", &lyra_rt_queue_size);
  add("lyra_rt_queue_push_back", &lyra_rt_queue_push_back);
  add("lyra_rt_queue_push_front", &lyra_rt_queue_push_front);
  add("lyra_rt_queue_concat_element", &lyra_rt_queue_concat_element);
  add("lyra_rt_queue_concat_spread", &lyra_rt_queue_concat_spread);
  add("lyra_rt_queue_insert", &lyra_rt_queue_insert);
  add("lyra_rt_queue_pop_front", &lyra_rt_queue_pop_front);
  add("lyra_rt_queue_pop_back", &lyra_rt_queue_pop_back);
  add("lyra_rt_queue_delete", &lyra_rt_queue_delete);
  add("lyra_rt_queue_delete_index", &lyra_rt_queue_delete_index);
  add("lyra_rt_queue_eq", &lyra_rt_queue_eq);
  add("lyra_rt_queue_ne", &lyra_rt_queue_ne);
  add("lyra_rt_queue_case_equal", &lyra_rt_queue_case_equal);
  add("lyra_rt_queue_bitstream_width", &lyra_rt_queue_bitstream_width);
  add("lyra_rt_queue_count_bits", &lyra_rt_queue_count_bits);
  add("lyra_rt_queue_value_box", &lyra_rt_queue_value_box);
  add("lyra_rt_queue_cell_alloc", &lyra_rt_queue_cell_alloc);
  add("lyra_rt_queue_cell_get", &lyra_rt_queue_cell_get);
  add("lyra_rt_queue_cell_initialize", &lyra_rt_queue_cell_initialize);
  add("lyra_rt_queue_cell_set", &lyra_rt_queue_cell_set);
  add("lyra_rt_queue_value_cell_alloc", &lyra_rt_queue_value_cell_alloc);
  add("lyra_rt_queue_value_cell_store", &lyra_rt_queue_value_cell_store);
  add("lyra_rt_queue_value_cell_load", &lyra_rt_queue_value_cell_load);
  add("lyra_rt_assocarray_default", &lyra_rt_assocarray_default);
  add("lyra_rt_assocarray_from_entries", &lyra_rt_assocarray_from_entries);
  add("lyra_rt_assocarray_from_entries_default",
      &lyra_rt_assocarray_from_entries_default);
  add("lyra_rt_assocarray_element", &lyra_rt_assocarray_element);
  add("lyra_rt_assocarray_with_element", &lyra_rt_assocarray_with_element);
  add("lyra_rt_assocarray_exists", &lyra_rt_assocarray_exists);
  add("lyra_rt_assocarray_size", &lyra_rt_assocarray_size);
  add("lyra_rt_assocarray_delete", &lyra_rt_assocarray_delete);
  add("lyra_rt_assocarray_delete_index", &lyra_rt_assocarray_delete_index);
  add("lyra_rt_assocarray_eq", &lyra_rt_assocarray_eq);
  add("lyra_rt_assocarray_ne", &lyra_rt_assocarray_ne);
  add("lyra_rt_assocarray_case_equal", &lyra_rt_assocarray_case_equal);
  add("lyra_rt_assocarray_bitstream_width",
      &lyra_rt_assocarray_bitstream_width);
  add("lyra_rt_assocarray_assoc_min_index",
      &lyra_rt_assocarray_assoc_min_index);
  add("lyra_rt_assocarray_assoc_max_index",
      &lyra_rt_assocarray_assoc_max_index);
  add("lyra_rt_assocarray_assoc_first", &lyra_rt_assocarray_assoc_first);
  add("lyra_rt_assocarray_assoc_last", &lyra_rt_assocarray_assoc_last);
  add("lyra_rt_assocarray_assoc_next", &lyra_rt_assocarray_assoc_next);
  add("lyra_rt_assocarray_assoc_prev", &lyra_rt_assocarray_assoc_prev);
  add("lyra_rt_string_bitstream_width", &lyra_rt_string_bitstream_width);
  add("lyra_rt_tuple_bitstream_width", &lyra_rt_tuple_bitstream_width);
  add("lyra_rt_dynarray_bitstream_width", &lyra_rt_dynarray_bitstream_width);
  add("lyra_rt_unpackedarray_bitstream_width",
      &lyra_rt_unpackedarray_bitstream_width);
  add("lyra_rt_assocarray_count_bits", &lyra_rt_assocarray_count_bits);
  add("lyra_rt_assocarray_value_box", &lyra_rt_assocarray_value_box);
  add("lyra_rt_assocarray_cell_alloc", &lyra_rt_assocarray_cell_alloc);
  add("lyra_rt_assocarray_cell_get", &lyra_rt_assocarray_cell_get);
  add("lyra_rt_assocarray_cell_initialize",
      &lyra_rt_assocarray_cell_initialize);
  add("lyra_rt_assocarray_cell_set", &lyra_rt_assocarray_cell_set);
  add("lyra_rt_assocarray_value_cell_alloc",
      &lyra_rt_assocarray_value_cell_alloc);
  add("lyra_rt_assocarray_value_cell_store",
      &lyra_rt_assocarray_value_cell_store);
  add("lyra_rt_assocarray_value_cell_load",
      &lyra_rt_assocarray_value_cell_load);
  add("lyra_rt_unpackedarray_sum", &lyra_rt_unpackedarray_sum);
  add("lyra_rt_unpackedarray_product", &lyra_rt_unpackedarray_product);
  add("lyra_rt_unpackedarray_and", &lyra_rt_unpackedarray_and);
  add("lyra_rt_unpackedarray_or", &lyra_rt_unpackedarray_or);
  add("lyra_rt_unpackedarray_xor", &lyra_rt_unpackedarray_xor);
  add("lyra_rt_unpackedarray_find", &lyra_rt_unpackedarray_find);
  add("lyra_rt_unpackedarray_find_index", &lyra_rt_unpackedarray_find_index);
  add("lyra_rt_unpackedarray_find_first", &lyra_rt_unpackedarray_find_first);
  add("lyra_rt_unpackedarray_find_first_index",
      &lyra_rt_unpackedarray_find_first_index);
  add("lyra_rt_unpackedarray_find_last", &lyra_rt_unpackedarray_find_last);
  add("lyra_rt_unpackedarray_find_last_index",
      &lyra_rt_unpackedarray_find_last_index);
  add("lyra_rt_unpackedarray_min", &lyra_rt_unpackedarray_min);
  add("lyra_rt_unpackedarray_max", &lyra_rt_unpackedarray_max);
  add("lyra_rt_unpackedarray_unique", &lyra_rt_unpackedarray_unique);
  add("lyra_rt_unpackedarray_unique_index",
      &lyra_rt_unpackedarray_unique_index);
  add("lyra_rt_unpackedarray_map", &lyra_rt_unpackedarray_map);
  add("lyra_rt_dynarray_sum", &lyra_rt_dynarray_sum);
  add("lyra_rt_dynarray_product", &lyra_rt_dynarray_product);
  add("lyra_rt_dynarray_and", &lyra_rt_dynarray_and);
  add("lyra_rt_dynarray_or", &lyra_rt_dynarray_or);
  add("lyra_rt_dynarray_xor", &lyra_rt_dynarray_xor);
  add("lyra_rt_dynarray_find", &lyra_rt_dynarray_find);
  add("lyra_rt_dynarray_find_index", &lyra_rt_dynarray_find_index);
  add("lyra_rt_dynarray_find_first", &lyra_rt_dynarray_find_first);
  add("lyra_rt_dynarray_find_first_index", &lyra_rt_dynarray_find_first_index);
  add("lyra_rt_dynarray_find_last", &lyra_rt_dynarray_find_last);
  add("lyra_rt_dynarray_find_last_index", &lyra_rt_dynarray_find_last_index);
  add("lyra_rt_dynarray_min", &lyra_rt_dynarray_min);
  add("lyra_rt_dynarray_max", &lyra_rt_dynarray_max);
  add("lyra_rt_dynarray_unique", &lyra_rt_dynarray_unique);
  add("lyra_rt_dynarray_unique_index", &lyra_rt_dynarray_unique_index);
  add("lyra_rt_dynarray_map", &lyra_rt_dynarray_map);
  add("lyra_rt_queue_sum", &lyra_rt_queue_sum);
  add("lyra_rt_queue_product", &lyra_rt_queue_product);
  add("lyra_rt_queue_and", &lyra_rt_queue_and);
  add("lyra_rt_queue_or", &lyra_rt_queue_or);
  add("lyra_rt_queue_xor", &lyra_rt_queue_xor);
  add("lyra_rt_queue_find", &lyra_rt_queue_find);
  add("lyra_rt_queue_find_index", &lyra_rt_queue_find_index);
  add("lyra_rt_queue_find_first", &lyra_rt_queue_find_first);
  add("lyra_rt_queue_find_first_index", &lyra_rt_queue_find_first_index);
  add("lyra_rt_queue_find_last", &lyra_rt_queue_find_last);
  add("lyra_rt_queue_find_last_index", &lyra_rt_queue_find_last_index);
  add("lyra_rt_queue_min", &lyra_rt_queue_min);
  add("lyra_rt_queue_max", &lyra_rt_queue_max);
  add("lyra_rt_queue_unique", &lyra_rt_queue_unique);
  add("lyra_rt_queue_unique_index", &lyra_rt_queue_unique_index);
  add("lyra_rt_queue_map", &lyra_rt_queue_map);
  add("lyra_rt_assocarray_sum", &lyra_rt_assocarray_sum);
  add("lyra_rt_assocarray_product", &lyra_rt_assocarray_product);
  add("lyra_rt_assocarray_and", &lyra_rt_assocarray_and);
  add("lyra_rt_assocarray_or", &lyra_rt_assocarray_or);
  add("lyra_rt_assocarray_xor", &lyra_rt_assocarray_xor);
  add("lyra_rt_assocarray_find", &lyra_rt_assocarray_find);
  add("lyra_rt_assocarray_find_index", &lyra_rt_assocarray_find_index);
  add("lyra_rt_assocarray_find_first", &lyra_rt_assocarray_find_first);
  add("lyra_rt_assocarray_find_first_index",
      &lyra_rt_assocarray_find_first_index);
  add("lyra_rt_assocarray_find_last", &lyra_rt_assocarray_find_last);
  add("lyra_rt_assocarray_find_last_index",
      &lyra_rt_assocarray_find_last_index);
  add("lyra_rt_assocarray_min", &lyra_rt_assocarray_min);
  add("lyra_rt_assocarray_max", &lyra_rt_assocarray_max);
  add("lyra_rt_assocarray_unique", &lyra_rt_assocarray_unique);
  add("lyra_rt_assocarray_unique_index", &lyra_rt_assocarray_unique_index);
  add("lyra_rt_assocarray_map", &lyra_rt_assocarray_map);
  add("lyra_rt_unpackedarray_sort", &lyra_rt_unpackedarray_sort);
  add("lyra_rt_unpackedarray_rsort", &lyra_rt_unpackedarray_rsort);
  add("lyra_rt_dynarray_sort", &lyra_rt_dynarray_sort);
  add("lyra_rt_dynarray_rsort", &lyra_rt_dynarray_rsort);
  add("lyra_rt_queue_sort", &lyra_rt_queue_sort);
  add("lyra_rt_queue_rsort", &lyra_rt_queue_rsort);
  add("lyra_rt_unpackedarray_reverse", &lyra_rt_unpackedarray_reverse);
  add("lyra_rt_dynarray_reverse", &lyra_rt_dynarray_reverse);
  add("lyra_rt_queue_reverse", &lyra_rt_queue_reverse);
  add("lyra_rt_assocarray_read_mem", &lyra_rt_assocarray_read_mem);
  add("lyra_rt_assocarray_read_mem_within",
      &lyra_rt_assocarray_read_mem_within);
  add("lyra_rt_assocarray_write_mem", &lyra_rt_assocarray_write_mem);
  add("lyra_rt_assocarray_write_mem_within",
      &lyra_rt_assocarray_write_mem_within);
  add("lyra_rt_dynarray_read_mem", &lyra_rt_dynarray_read_mem);
  add("lyra_rt_dynarray_read_mem_within", &lyra_rt_dynarray_read_mem_within);
  add("lyra_rt_dynarray_write_mem", &lyra_rt_dynarray_write_mem);
  add("lyra_rt_dynarray_write_mem_within", &lyra_rt_dynarray_write_mem_within);
  add("lyra_rt_queue_read_mem", &lyra_rt_queue_read_mem);
  add("lyra_rt_queue_read_mem_within", &lyra_rt_queue_read_mem_within);
  add("lyra_rt_queue_write_mem", &lyra_rt_queue_write_mem);
  add("lyra_rt_queue_write_mem_within", &lyra_rt_queue_write_mem_within);
  add("lyra_rt_unpackedarray_read_mem", &lyra_rt_unpackedarray_read_mem);
  add("lyra_rt_unpackedarray_read_mem_within",
      &lyra_rt_unpackedarray_read_mem_within);
  add("lyra_rt_unpackedarray_write_mem", &lyra_rt_unpackedarray_write_mem);
  add("lyra_rt_unpackedarray_write_mem_within",
      &lyra_rt_unpackedarray_write_mem_within);
  add("lyra_rt_unpackedarray_element", &lyra_rt_unpackedarray_element);
  add("lyra_rt_unpackedarray_with_element",
      &lyra_rt_unpackedarray_with_element);
  add("lyra_rt_unpackedarray_slice", &lyra_rt_unpackedarray_slice);
  add("lyra_rt_unpackedarray_with_slice", &lyra_rt_unpackedarray_with_slice);
  add("lyra_rt_unpackedarray_size", &lyra_rt_unpackedarray_size);
  add("lyra_rt_unpackedarray_count_bits", &lyra_rt_unpackedarray_count_bits);
  add("lyra_rt_unpackedarray_eq", &lyra_rt_unpackedarray_eq);
  add("lyra_rt_unpackedarray_ne", &lyra_rt_unpackedarray_ne);
  add("lyra_rt_unpackedarray_case_equal", &lyra_rt_unpackedarray_case_equal);
  add("lyra_rt_unpackedarray_is_unknown", &lyra_rt_unpackedarray_is_unknown);
  add("lyra_rt_unpackedarray_cell_alloc", &lyra_rt_unpackedarray_cell_alloc);
  add("lyra_rt_unpackedarray_cell_get", &lyra_rt_unpackedarray_cell_get);
  add("lyra_rt_unpackedarray_cell_initialize",
      &lyra_rt_unpackedarray_cell_initialize);
  add("lyra_rt_unpackedarray_cell_set", &lyra_rt_unpackedarray_cell_set);
  add("lyra_rt_unpackedarray_value_cell_alloc",
      &lyra_rt_unpackedarray_value_cell_alloc);
  add("lyra_rt_unpackedarray_value_cell_store",
      &lyra_rt_unpackedarray_value_cell_store);
  add("lyra_rt_unpackedarray_value_cell_load",
      &lyra_rt_unpackedarray_value_cell_load);
  add("lyra_rt_packed_net_get", &lyra_rt_packed_net_get);
  add("lyra_rt_packed_net_initialize", &lyra_rt_packed_net_initialize);
  add("lyra_rt_packed_attach_driver", &lyra_rt_packed_attach_driver);
  add("lyra_rt_packed_driver_get", &lyra_rt_packed_driver_get);
  add("lyra_rt_packed_driver_set", &lyra_rt_packed_driver_set);
  add("lyra_rt_tuple_net_get", &lyra_rt_tuple_net_get);
  add("lyra_rt_tuple_net_initialize", &lyra_rt_tuple_net_initialize);
  add("lyra_rt_tuple_attach_driver", &lyra_rt_tuple_attach_driver);
  add("lyra_rt_tuple_driver_get", &lyra_rt_tuple_driver_get);
  add("lyra_rt_tuple_driver_set", &lyra_rt_tuple_driver_set);
  add("lyra_rt_union_net_get", &lyra_rt_union_net_get);
  add("lyra_rt_union_net_initialize", &lyra_rt_union_net_initialize);
  add("lyra_rt_union_attach_driver", &lyra_rt_union_attach_driver);
  add("lyra_rt_union_driver_get", &lyra_rt_union_driver_get);
  add("lyra_rt_union_driver_set", &lyra_rt_union_driver_set);
  add("lyra_rt_unpackedarray_net_get", &lyra_rt_unpackedarray_net_get);
  add("lyra_rt_unpackedarray_net_initialize",
      &lyra_rt_unpackedarray_net_initialize);
  add("lyra_rt_unpackedarray_attach_driver",
      &lyra_rt_unpackedarray_attach_driver);
  add("lyra_rt_unpackedarray_driver_get", &lyra_rt_unpackedarray_driver_get);
  add("lyra_rt_unpackedarray_driver_set", &lyra_rt_unpackedarray_driver_set);
  add("lyra_rt_unpackedarray_merge_conditional",
      &lyra_rt_unpackedarray_merge_conditional);
  add("lyra_rt_unpackedarray_from_packed_array",
      &lyra_rt_unpackedarray_from_packed_array);
  Check(
      jit.getMainJITDylib().define(
          llvm::orc::absoluteSymbols(std::move(symbols))),
      "define runtime abi");
}

// Opens the design's DPI-C library to the execution session, so a generated
// foreign call finds its symbol (LRM 35.4). A generator searches the library on
// each unresolved name, which is what an ahead-of-time image's link step does
// once; the mangling prefix is the platform's, taken from the JIT's data
// layout.
void DefineForeignSymbols(
    llvm::orc::LLJIT& jit, const std::filesystem::path& library) {
  auto generator = llvm::orc::DynamicLibrarySearchGenerator::Load(
      library.c_str(), jit.getDataLayout().getGlobalPrefix());
  if (!generator) {
    throw InternalError(
        "jit executor: loading the DPI-C library '" + library.string() +
        "': " + llvm::toString(generator.takeError()));
  }
  jit.getMainJITDylib().addGenerator(std::move(*generator));
}

// What a slot is for, which two declarations answer differently for a slot of
// the same type: a variable is written through its own store for as long as its
// owner lives, and a snapshot is filled once where its owner is built and only
// read afterwards.
enum class SlotRole : std::uint8_t { kVariable, kSnapshot };

// The storage a generic value realizes for one member its declaration holds,
// projected from that member's LIR type: an observable cell holds a value other
// processes subscribe to, a reference-typed member is a box holding a borrowed
// handle, a runtime record is that record, and anything else the runtime has a
// value realization for the owner holds itself, as its role says.
auto DescribeMember(
    const lir::CompilationUnit& unit, lir::TypeId type, SlotRole role)
    -> diag::Result<runtime::MemberStorageDescriptor> {
  const auto& data = unit.types.Get(type);
  if (const auto* observable = data.As<lir::ObservableType>()) {
    if (const std::optional<support::ValueDomain> domain =
            backend::llvm_backend::ValueDomainOf(unit, observable->value)) {
      return runtime::ObservableCellStorage{.domain = *domain};
    }
  }
  // A net is the storage its drivers' contributions fold into, so the member is
  // the resolution node itself, reached only through its own access. The fold
  // travels with it, because two nets of one data type resolve differently when
  // their net types differ (LRM 6.6).
  if (const auto* net = data.As<lir::ResolvedType>()) {
    if (const std::optional<support::ValueDomain> domain =
            backend::llvm_backend::ValueDomainOf(unit, net->value)) {
      return runtime::ResolvedNetStorage{
          .domain = *domain,
          .resolution =
              backend::llvm_backend::NetResolutionOf(net->resolution)};
    }
  }
  // A driver is a handle on a contribution the net owns and issues, so the
  // member is a box holding that handle rather than storage of its own (LRM
  // 6.5).
  if (data.Is<lir::DriverType>()) {
    return runtime::BorrowedHandleStorage{};
  }
  if (const auto* library = data.As<lir::RuntimeLibraryType>()) {
    switch (library->kind) {
      case lir::RuntimeLibraryKind::kCancellationTarget:
        return runtime::CancellationTargetStorage{};
      // The cancel state a deferred file write is guarded by (LRM 21.3.2),
      // which the closure performing that write owns a copy of.
      case lir::RuntimeLibraryKind::kChannelCancellation:
        return runtime::ChannelCancellationStorage{};
      // An integral type's descriptor, which a deferred write carries so the
      // region performing it can build the value. The module holds one per
      // type for the whole run, so a member that names one points at storage
      // outliving every closure that reads it rather than owning a copy.
      case lir::RuntimeLibraryKind::kPackedType:
        return runtime::BorrowedHandleStorage{};
      default:
        break;
    }
  }
  // A class handle is the value the member holds, not a pointer it merely
  // points with: the object stays alive because the member refers to it (LRM
  // 8.3), which is what a box holding a borrowed handle does not do.
  if (data.Is<lir::ManagedRefType>()) {
    return runtime::InlineValueStorage{
        .domain = support::ValueDomain::kManagedRef};
  }
  // A declaration standing for several objects keeps a handle on the sequence
  // of them, which is built once where the owner is built and held for the rest
  // of the run. So the member is the same box a single such handle is: it owns
  // neither the sequence nor the objects in it.
  if (data.Is<lir::VectorType>()) {
    return runtime::BorrowedHandleStorage{};
  }
  if (data.Pointee().has_value()) {
    return runtime::BorrowedHandleStorage{};
  }
  // A value nothing subscribes to lives in the owner's own slot rather than
  // behind an observable cell, and what the slot is for decides how: a
  // variable keeps the representation its declaration gave it across every
  // write, and a snapshot is the value it was filled with.
  if (const std::optional<support::ValueDomain> domain =
          backend::llvm_backend::ValueDomainOf(unit, type)) {
    // A pointer-shaped value has no representation a declaration could give
    // it (LRM 6.14, 8.3), so there is nothing for a write to land at and no
    // cell to land in; it is held as it is whatever the slot is for.
    const bool pointer_shaped = *domain == support::ValueDomain::kChandle ||
                                *domain == support::ValueDomain::kManagedRef;
    if (role == SlotRole::kVariable && !pointer_shaped) {
      return runtime::ValueCellStorage{.domain = *domain};
    }
    return runtime::InlineValueStorage{.domain = *domain};
  }
  return diag::Fail(
      diag::DiagCode::kUnsupportedTypeKind,
      std::format(
          "jit executor: a member of type {} has no storage realization on "
          "this backend",
          unit.types.Get(type).KindName()));
}

auto DescribeMembers(
    const lir::CompilationUnit& unit, std::span<const lir::Member> members,
    SlotRole role)
    -> diag::Result<std::vector<runtime::MemberStorageDescriptor>> {
  std::vector<runtime::MemberStorageDescriptor> descriptors;
  descriptors.reserve(members.size());
  for (const lir::Member& member : members) {
    auto described = DescribeMember(unit, member.type, role);
    if (!described) {
      return std::unexpected(std::move(described.error()));
    }
    descriptors.push_back(*described);
  }
  return descriptors;
}

// Fills one scope class's runtime definition from its JIT-compiled entries. The
// lifecycle entries are ABI-compatible native functions over the generic scope
// receiver; an entry the class has no work for is absent and keeps the runtime
// no-op default. Looking a symbol up here materializes its module, which
// resolves that module's definition references -- every definition symbol is
// injected before any is filled, so those references find their address
// regardless of fill order.
void FillDefinition(
    llvm::orc::LLJIT& jit, std::string_view class_name,
    std::int8_t time_precision_power, runtime::ScopeDefinition& definition) {
  // An entry the scope has no work for was never emitted, and the session
  // reports exactly that: the name has no definition. Any other failure means
  // the entry does exist and could not be brought up -- typically a runtime
  // symbol its body calls that nothing defines -- which would leave the scope
  // silently missing a body it was compiled to have.
  auto lookup =
      [&](std::string_view entry) -> std::optional<llvm::orc::ExecutorAddr> {
    const std::string symbol =
        std::string(class_name) + "." + std::string(entry);
    auto found = jit.lookup(symbol);
    if (found) {
      return *found;
    }
    llvm::Error reason = found.takeError();
    if (reason.isA<llvm::orc::SymbolsNotFound>()) {
      llvm::consumeError(std::move(reason));
      return std::nullopt;
    }
    throw InternalError(
        "jit executor: the scope entry '" + symbol +
        "' did not resolve: " + llvm::toString(std::move(reason)));
  };
  definition.program.metadata.time_precision_power = time_precision_power;
  if (auto entry = lookup("ResolveState")) {
    definition.program.resolve_state = entry->toPtr<runtime::ScopeEntry>();
  }
  if (auto entry = lookup("InitializeState")) {
    definition.program.initialize_state = entry->toPtr<runtime::ScopeEntry>();
  }
  if (auto entry = lookup("CreateProcesses")) {
    definition.program.create_processes = entry->toPtr<runtime::ScopeEntry>();
  }
  if (auto entry = lookup("constructor")) {
    definition.construct = entry->toPtr<runtime::ScopeEntry>();
  }
}

// One scope class loaded into the JIT: the storage schema its instances
// realize, and the runtime definition built from its compiled entries, which
// owns a stable address every site constructing an instance references. The
// schema is held here because the definition names it as plain data it does not
// own.
struct LoadedScopeClass {
  std::string name;
  std::int8_t time_precision_power = 0;
  std::vector<runtime::MemberStorageDescriptor> members;
  std::unique_ptr<runtime::ScopeDefinition> definition;
};

// One cell a unit shares with the whole program, built here because the
// runtime owns storage and generated code only ever holds its address.
struct LoadedStaticStorage {
  std::string symbol;
  std::unique_ptr<runtime::MemberStorage> storage;
};

// The cells a unit declares outside any instance. What each needs is read from
// its type the same way a member's is, since the difference between the two is
// what reaches the storage and not what the storage is.
auto LoadStaticStorage(const lir::CompilationUnit& unit)
    -> diag::Result<std::vector<LoadedStaticStorage>> {
  std::vector<LoadedStaticStorage> loaded;
  loaded.reserve(unit.static_storage.size());
  for (const lir::StaticStorage& entry : unit.static_storage) {
    auto described = DescribeMember(unit, entry.type, SlotRole::kVariable);
    if (!described) {
      return std::unexpected(std::move(described.error()));
    }
    loaded.push_back(
        LoadedStaticStorage{
            .symbol = entry.symbol,
            .storage = std::make_unique<runtime::MemberStorage>(*described)});
  }
  return loaded;
}

// The class an owned-child member reaches, absent for a member that reaches
// storage instead.
auto OwnedChildClass(const lir::CompilationUnit& unit, lir::TypeId type)
    -> std::optional<lir::ClassId> {
  // A child is reached by a pointer at it. A class handle also points at an
  // object of this unit, but at one the program built rather than one the tree
  // owns, so the pointer kind is what tells the two apart.
  const auto* pointer = unit.types.Get(type).As<lir::PointerType>();
  if (pointer == nullptr) {
    return std::nullopt;
  }
  const auto* object = unit.types.Get(pointer->pointee).As<lir::ObjectType>();
  return object != nullptr ? std::optional{object->class_id} : std::nullopt;
}

// One entry per node of the unit's object tree, reached by descending
// containment from its root. A class no containment edge reaches is one the
// program allocates as an ordinary object; the runtime never builds it and it
// needs no definition.
auto LoadScopeClasses(
    const lir::CompilationUnit& unit,
    const compiler::ElaboratedUnitMetadata& metadata)
    -> diag::Result<std::vector<LoadedScopeClass>> {
  std::vector<LoadedScopeClass> loaded;
  const auto descend = [&](const auto& self_ref,
                           lir::ClassId id) -> diag::Result<void> {
    const lir::Class& cls = unit.classes.Get(id);
    auto members = DescribeMembers(unit, cls.members, SlotRole::kVariable);
    if (!members) {
      return std::unexpected(std::move(members.error()));
    }
    // Every scope of a unit runs at the unit's precision: a scope inside a unit
    // has no timescale declaration of its own and takes the enclosing one (LRM
    // 3.14.2.3).
    loaded.push_back(
        LoadedScopeClass{
            .name = cls.name,
            .time_precision_power = metadata.time_precision_power,
            .members = *std::move(members),
            .definition = std::make_unique<runtime::ScopeDefinition>()});
    // A member whose type reaches an object of this unit is a child this class
    // owns; one reaching a value reaches storage instead. The type says which,
    // so descending it needs nothing beside the members already declared.
    for (const lir::Member& member : cls.members) {
      if (const auto child = OwnedChildClass(unit, member.type)) {
        auto descended = self_ref(self_ref, *child);
        if (!descended) {
          return std::unexpected(std::move(descended.error()));
        }
      }
    }
    return {};
  };
  if (unit.root.has_value()) {
    auto descended = descend(descend, *unit.root);
    if (!descended) {
      return std::unexpected(std::move(descended.error()));
    }
  }
  return loaded;
}

// One class whose values the program builds with `new`, rather than a class the
// object tree owns an instance of.
struct LoadedObjectClass {
  std::string name;
  std::vector<runtime::MemberStorageDescriptor> members;
  std::unique_ptr<runtime::ObjectDefinition> definition;
};

auto LoadObjectClasses(const lir::CompilationUnit& unit)
    -> diag::Result<std::vector<LoadedObjectClass>> {
  std::vector<LoadedObjectClass> loaded;
  for (const lir::ClassId id : unit.classes.Ids()) {
    const lir::Class& cls = unit.classes.Get(id);
    if (lir::IsObjectTreeNode(cls)) {
      continue;
    }
    const std::vector<lir::Member> storage = lir::StorageMembers(unit, id);
    auto members = DescribeMembers(unit, storage, SlotRole::kVariable);
    if (!members) {
      return std::unexpected(std::move(members.error()));
    }
    loaded.push_back(
        LoadedObjectClass{
            .name = cls.name,
            .members = *std::move(members),
            .definition = std::make_unique<runtime::ObjectDefinition>()});
  }
  return loaded;
}

// The definition of one closure a unit declares, kept alive for the session
// beside the schema it names as plain data it does not own. `protocol` carries
// which alternative the body is, selected before the symbol it holds is known.
struct LoadedClosure {
  std::string name;
  std::vector<runtime::MemberStorageDescriptor> captures;
  runtime::ClosureBody protocol;
  std::unique_ptr<runtime::ClosureDefinition> definition;
};

// Which alternative a body is, read from what its invoke answers: a coroutine
// result is the coroutine protocol, no result at all is a body run to
// completion, and a value result is a body run per entry of a container -- and
// that one carries the representation its result comes back in, since a handle
// carries none.
auto ProtocolOf(const lir::CompilationUnit& unit, lir::TypeId result)
    -> runtime::ClosureBody {
  if (unit.types.Get(result).Is<lir::CoroutineType>()) {
    return runtime::CoroutineBody{};
  }
  if (unit.types.Get(result).Is<lir::VoidType>()) {
    return runtime::SynchronousBody{};
  }
  const std::optional<support::ValueDomain> domain =
      backend::llvm_backend::ValueDomainOf(unit, result);
  if (!domain) {
    throw InternalError(
        "jit executor: a body run per entry settles a runtime value");
  }
  return runtime::PerElementBody{.result_domain = *domain};
}

auto LoadClosures(const lir::CompilationUnit& unit)
    -> diag::Result<std::vector<LoadedClosure>> {
  std::vector<LoadedClosure> loaded;
  loaded.reserve(unit.closures.size());
  for (const lir::Closure& closure : unit.closures) {
    auto captures =
        DescribeMembers(unit, closure.captures, SlotRole::kSnapshot);
    if (!captures) {
      return std::unexpected(std::move(captures.error()));
    }
    const lir::TypeId result = unit.functions.Get(closure.invoke).result_type;
    loaded.push_back(
        LoadedClosure{
            .name = closure.name,
            .captures = *std::move(captures),
            .protocol = ProtocolOf(unit, result),
            .definition = std::make_unique<runtime::ClosureDefinition>()});
  }
  return loaded;
}

}  // namespace

auto Execute(
    std::span<const lir::CompilationUnit> units,
    std::span<const compiler::ElaboratedUnitMetadata> metadata,
    const lir::CompilationUnit& root_unit,
    const compiler::ElaboratedUnitMetadata& root_metadata,
    const std::optional<std::filesystem::path>& dpi_library,
    std::span<const std::string> simulation_arguments) -> diag::Result<int> {
  llvm::InitializeNativeTarget();
  llvm::InitializeNativeTargetAsmPrinter();

  auto jit = Unwrap(llvm::orc::LLJITBuilder().create(), "create jit");
  LowerCoroutines(*jit);
  DefineRuntimeAbi(*jit);
  if (dpi_library.has_value()) {
    DefineForeignSymbols(*jit, *dpi_library);
  }

  // Every unit -- the source units and the design-root -- becomes one module in
  // the shared JIT, so a construct reaches the entries and the definition of
  // the class it builds by symbol. The root is loaded and driven like any other
  // unit, distinguished only as the bootstrap entry below.
  std::vector<const lir::CompilationUnit*> loaded_units;
  loaded_units.reserve(units.size() + 1);
  for (const lir::CompilationUnit& unit : units) {
    loaded_units.push_back(&unit);
  }
  loaded_units.push_back(&root_unit);

  // Generation runs ahead of any state the run needs, so a unit this backend
  // cannot lower is refused with none of the design yet standing.
  for (const lir::CompilationUnit* unit : loaded_units) {
    auto emitted = backend::llvm_backend::EmitModule(*unit);
    if (!emitted) {
      return std::unexpected(std::move(emitted.error()));
    }
    auto owned = std::move(*emitted).Release();
    Check(
        jit->addIRModule(
            llvm::orc::ThreadSafeModule(
                std::move(owned.module), std::move(owned.context))),
        "add module");
  }

  // Each definition owns a stable address for the whole run; the runtime holds
  // pointers into it.
  std::vector<LoadedScopeClass> loaded;
  for (std::size_t i = 0; i < units.size(); ++i) {
    auto unit_classes = LoadScopeClasses(units[i], metadata[i]);
    if (!unit_classes) {
      return std::unexpected(std::move(unit_classes.error()));
    }
    loaded.insert(
        loaded.end(), std::make_move_iterator(unit_classes->begin()),
        std::make_move_iterator(unit_classes->end()));
  }
  if (!root_unit.root.has_value()) {
    throw InternalError("jit executor: the design root roots no object tree");
  }
  const std::string root_class_name =
      root_unit.classes.Get(*root_unit.root).name;
  auto root_classes = LoadScopeClasses(root_unit, root_metadata);
  if (!root_classes) {
    return std::unexpected(std::move(root_classes.error()));
  }
  loaded.insert(
      loaded.end(), std::make_move_iterator(root_classes->begin()),
      std::make_move_iterator(root_classes->end()));

  std::vector<LoadedObjectClass> objects;
  for (const lir::CompilationUnit* unit : loaded_units) {
    auto unit_objects = LoadObjectClasses(*unit);
    if (!unit_objects) {
      return std::unexpected(std::move(unit_objects.error()));
    }
    objects.insert(
        objects.end(), std::make_move_iterator(unit_objects->begin()),
        std::make_move_iterator(unit_objects->end()));
  }

  std::vector<LoadedClosure> closures;
  for (const lir::CompilationUnit* unit : loaded_units) {
    auto unit_closures = LoadClosures(*unit);
    if (!unit_closures) {
      return std::unexpected(std::move(unit_closures.error()));
    }
    closures.insert(
        closures.end(), std::make_move_iterator(unit_closures->begin()),
        std::make_move_iterator(unit_closures->end()));
  }

  // The schema is named only once every declaration is in place, so no
  // descriptor vector is reallocated out from under a definition that points at
  // it.
  for (LoadedScopeClass& entry : loaded) {
    entry.definition->members = runtime::MemberStorageSchema{
        .data = entry.members.data(),
        .size = static_cast<std::uint32_t>(entry.members.size())};
  }
  for (LoadedClosure& entry : closures) {
    entry.definition->captures = runtime::MemberStorageSchema{
        .data = entry.captures.data(),
        .size = static_cast<std::uint32_t>(entry.captures.size())};
  }
  for (LoadedObjectClass& entry : objects) {
    entry.definition->members = runtime::MemberStorageSchema{
        .data = entry.members.data(),
        .size = static_cast<std::uint32_t>(entry.members.size())};
  }

  // Each declaration the runtime builds values of publishes its definition as
  // an injected data symbol the construct references. Every definition is
  // filled from its JIT-compiled entries after every address is injected, so a
  // reference resolves regardless of the order the definitions are filled.
  llvm::orc::SymbolMap definition_symbols;
  const auto publish = [&](std::string_view name, void* definition) {
    const std::string symbol =
        backend::llvm_backend::DefinitionSymbolName(name);
    definition_symbols[jit->getExecutionSession().intern(symbol)] =
        llvm::orc::ExecutorSymbolDef(
            llvm::orc::ExecutorAddr::fromPtr(definition),
            llvm::JITSymbolFlags::Exported);
  };
  for (const LoadedScopeClass& entry : loaded) {
    publish(entry.name, entry.definition.get());
  }
  for (const LoadedClosure& entry : closures) {
    publish(entry.name, entry.definition.get());
  }
  for (const LoadedObjectClass& entry : objects) {
    publish(entry.name, entry.definition.get());
  }
  Check(
      jit->getMainJITDylib().define(
          llvm::orc::absoluteSymbols(std::move(definition_symbols))),
      "define runtime definitions");

  // A unit's namespace-level storage is published the same way: the unit that
  // declares a cell is the only one that lists it, so building from every
  // unit's list yields each cell exactly once, and every reader -- the
  // declaring unit included -- reaches it through the symbol.
  std::vector<LoadedStaticStorage> static_storage;
  for (const lir::CompilationUnit* unit : loaded_units) {
    auto unit_storage = LoadStaticStorage(*unit);
    if (!unit_storage) {
      return std::unexpected(std::move(unit_storage.error()));
    }
    static_storage.insert(
        static_storage.end(), std::make_move_iterator(unit_storage->begin()),
        std::make_move_iterator(unit_storage->end()));
  }
  llvm::orc::SymbolMap storage_symbols;
  for (const LoadedStaticStorage& entry : static_storage) {
    storage_symbols[jit->getExecutionSession().intern(entry.symbol)] =
        llvm::orc::ExecutorSymbolDef(
            llvm::orc::ExecutorAddr::fromPtr(entry.storage->Address()),
            llvm::JITSymbolFlags::Exported);
  }
  Check(
      jit->getMainJITDylib().define(
          llvm::orc::absoluteSymbols(std::move(storage_symbols))),
      "define shared storage");

  for (const LoadedScopeClass& entry : loaded) {
    FillDefinition(
        *jit, entry.name, entry.time_precision_power, *entry.definition);
  }
  // Every closure has a body, so a name that does not resolve is not an absent
  // entry but one that could not be brought up.
  for (const LoadedClosure& entry : closures) {
    const std::string symbol = entry.name + ".invoke";
    auto found = jit->lookup(symbol);
    if (!found) {
      throw InternalError(
          "jit executor: the closure body '" + symbol +
          "' did not resolve: " + llvm::toString(found.takeError()));
    }
    // The signature the address is held under is the one the invoke's result
    // type states, decided where the unit was loaded and carried here.
    entry.definition->body = std::visit(
        Overloaded{
            [&](const runtime::SynchronousBody&) -> runtime::ClosureBody {
              return runtime::SynchronousBody{
                  .run = found->toPtr<void(void*)>()};
            },
            [&](const runtime::CoroutineBody&) -> runtime::ClosureBody {
              return runtime::CoroutineBody{
                  .start = found->toPtr<void*(void*)>()};
            },
            [&](const runtime::PerElementBody& body) -> runtime::ClosureBody {
              return runtime::PerElementBody{
                  .run = found->toPtr<void*(void*, const void*, const void*)>(),
                  .result_domain = body.result_domain};
            }},
        entry.protocol);
  }
  // Every class has a constructor (LRM 8.7), so a name that does not resolve
  // is not an absent body but one that could not be brought up.
  for (const LoadedObjectClass& entry : objects) {
    const std::string symbol = entry.name + ".constructor";
    auto found = jit->lookup(symbol);
    if (!found) {
      throw InternalError(
          "jit executor: the constructor '" + symbol +
          "' did not resolve: " + llvm::toString(found.takeError()));
    }
    entry.definition->construct = found->toPtr<runtime::ObjectEntry>();
  }

  auto runtime_options = runtime::DefaultRuntimeOptions();
  runtime_options.plusargs = runtime::PlusargsFrom(simulation_arguments);
  runtime::Runtime runtime_instance{std::move(runtime_options)};

  // The design-root unit's construct elaborates the design: it builds the
  // top-level units through the cross-unit construct ABI, which recurses into
  // their subtrees. The bootstrap allocates the root instance and runs that
  // construct in a generated-call scope, exactly as the runtime enters any
  // construct entry; the runtime then walks the built tree.
  const auto root_entry =
      std::ranges::find(loaded, root_class_name, &LoadedScopeClass::name);
  if (root_entry == loaded.end()) {
    throw InternalError("jit executor: the design root has no scope class");
  }
  const runtime::ScopeDefinition& root_definition = *root_entry->definition;
  auto root = std::make_unique<runtime::GeneratedScope>(
      nullptr, runtime::HierarchySegment{"$root", {}}, &root_definition);
  {
    runtime::GeneratedCallScope construct_scope;
    root_definition.construct(root.get());
  }
  auto design = std::make_unique<runtime::Design>(std::move(root));
  runtime_instance.BindDesign(std::move(design));
  return runtime::RunSimulation(runtime_instance);
}

}  // namespace lyra::jit
