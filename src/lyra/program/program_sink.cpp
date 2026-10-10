#include "lyra/program/program_sink.hpp"

#include <cstdint>
#include <filesystem>
#include <format>
#include <map>
#include <optional>
#include <string>
#include <string_view>
#include <system_error>
#include <type_traits>
#include <utility>
#include <vector>

#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/Type.h>
#include <llvm/Passes/OptimizationLevel.h>

#include "lyra/backend/llvm/emit.hpp"
#include "lyra/backend/llvm/object_file.hpp"
#include "lyra/backend/llvm/runtime_entry.hpp"
#include "lyra/base/internal_error.hpp"
#include "lyra/compiler/unit_pipeline.hpp"
#include "lyra/diag/diag_code.hpp"
#include "lyra/dpi/abi_header.hpp"
#include "lyra/driver/artifact_store.hpp"
#include "lyra/driver/project_layout.hpp"
#include "lyra/lir/compilation_unit.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/profiling/time_trace.hpp"
#include "lyra/runtime/runtime_abi.hpp"
#include "lyra/status/status.hpp"
#include "lyra/support/statistics.hpp"

namespace lyra::program {

namespace {

// What one value crossing the runtime ABI is, coarsely enough that two sides
// disagreeing is a defect rather than a spelling difference.
//
// Every runtime value crosses as an untyped handle, so the compiler sees the
// C++ function that defines an entry and the call the generated module makes as
// two unrelated declarations and can compare neither against the other. The
// shape below is what makes them comparable: it is read off the C++ function --
// the one place an entry's shape is stated -- and off the module's own
// declaration, and the two must agree.
enum class AbiKind : std::uint8_t {
  kVoid,
  kPointer,
  kBool,
  kInt8,
  kInt32,
  kInt64,
  kFloat,
  kDouble,
  kAggregate,
  kOther,
};

template <typename>
inline constexpr bool kAbiKindUnmapped = false;

// The ABI vocabulary is closed, so a C++ type outside it fails to compile here
// rather than being classified by whichever arm its shape resembles. Where both
// spellings of a width cross, they reach one kind: signedness is not part of
// what crosses, since how the bits are read is the operation's own business at
// either end.
template <typename T>
constexpr auto AbiKindOfCpp() -> AbiKind {
  if constexpr (std::is_void_v<T>) {
    return AbiKind::kVoid;
  } else if constexpr (std::is_pointer_v<T>) {
    return AbiKind::kPointer;
  } else if constexpr (std::is_same_v<T, bool>) {
    return AbiKind::kBool;
  } else if constexpr (
      std::is_same_v<T, std::int64_t> || std::is_same_v<T, std::uint64_t>) {
    return AbiKind::kInt64;
  } else if constexpr (
      std::is_same_v<T, std::int32_t> || std::is_same_v<T, std::uint32_t>) {
    return AbiKind::kInt32;
  } else if constexpr (
      std::is_same_v<T, std::int8_t> || std::is_same_v<T, std::uint8_t>) {
    return AbiKind::kInt8;
  } else if constexpr (std::is_same_v<T, float>) {
    return AbiKind::kFloat;
  } else if constexpr (std::is_same_v<T, double>) {
    return AbiKind::kDouble;
  } else if constexpr (std::is_class_v<T>) {
    return AbiKind::kAggregate;
  } else {
    static_assert(kAbiKindUnmapped<T>, "runtime ABI: unmapped C++ type");
  }
}

// One entry's shape, as its own definition states it.
struct AbiSignature {
  AbiKind result = AbiKind::kVoid;
  std::vector<AbiKind> operands;
};

template <typename F>
struct AbiSignatureOf;

template <typename R, typename... A>
struct AbiSignatureOf<R(A...)> {
  static auto Get() -> AbiSignature {
    return AbiSignature{
        .result = AbiKindOfCpp<R>(), .operands = {AbiKindOfCpp<A>()...}};
  }
};

// Whether an entry can raise is no part of how it is called, so an entry that
// cannot is called the same way.
template <typename R, typename... A>
struct AbiSignatureOf<R(A...) noexcept> : AbiSignatureOf<R(A...)> {};

auto AbiKindOfLlvm(llvm::Type* type) -> AbiKind {
  if (type->isVoidTy()) {
    return AbiKind::kVoid;
  }
  if (type->isPointerTy()) {
    return AbiKind::kPointer;
  }
  if (type->isIntegerTy(1)) {
    return AbiKind::kBool;
  }
  if (type->isIntegerTy(8)) {
    return AbiKind::kInt8;
  }
  if (type->isIntegerTy(32)) {
    return AbiKind::kInt32;
  }
  if (type->isIntegerTy(64)) {
    return AbiKind::kInt64;
  }
  if (type->isFloatTy()) {
    return AbiKind::kFloat;
  }
  if (type->isDoubleTy()) {
    return AbiKind::kDouble;
  }
  if (type->isStructTy()) {
    return AbiKind::kAggregate;
  }
  return AbiKind::kOther;
}

auto AbiKindName(AbiKind kind) -> std::string_view {
  switch (kind) {
    case AbiKind::kVoid:
      return "void";
    case AbiKind::kPointer:
      return "pointer";
    case AbiKind::kBool:
      return "bool";
    case AbiKind::kInt8:
      return "int8";
    case AbiKind::kInt32:
      return "int32";
    case AbiKind::kInt64:
      return "int64";
    case AbiKind::kFloat:
      return "float";
    case AbiKind::kDouble:
      return "double";
    case AbiKind::kAggregate:
      return "aggregate";
    case AbiKind::kOther:
      return "an unclassified machine type";
  }
  throw InternalError("runtime abi: unknown value kind");
}

// What the engine publishes: the services a design runs against -- the host it
// runs in, its files and its output, its processes and their scheduling, the
// object tree and the names resolved through it -- and, per value domain, the
// shared cell a local outliving its block lives in (LRM 6.21).
void BindEngineEntries(const auto& add) {
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
  add("lyra_rt_enter_scope_static_init", &lyra_rt_enter_scope_static_init);
  add("lyra_rt_enter_namespace_static_init",
      &lyra_rt_enter_namespace_static_init);
  add("lyra_rt_leave_static_init", &lyra_rt_leave_static_init);
  add("lyra_rt_enter_dpi_scope", &lyra_rt_enter_dpi_scope);
  add("lyra_rt_leave_dpi_scope", &lyra_rt_leave_dpi_scope);
  add("lyra_rt_disable_is_active", &lyra_rt_disable_is_active);
  add("lyra_rt_check_import_task_acknowledged",
      &lyra_rt_check_import_task_acknowledged);
  add("lyra_rt_check_import_function_acknowledged",
      &lyra_rt_check_import_function_acknowledged);
  add("lyra_rt_check_export_reachable", &lyra_rt_check_export_reachable);
  add("lyra_rt_claim_namespace_initialize",
      &lyra_rt_claim_namespace_initialize);
  add("lyra_rt_current_export_scope", &lyra_rt_current_export_scope);
  add("lyra_rt_find_export_entry", &lyra_rt_find_export_entry);
  add("lyra_rt_run_foreign_task_on_fiber", &lyra_rt_run_foreign_task_on_fiber);
  add("lyra_rt_run_exported_task_to_completion",
      &lyra_rt_run_exported_task_to_completion);
  add("lyra_rt_spawn_all", &lyra_rt_spawn_all);
  add("lyra_rt_fork_wait_all", &lyra_rt_fork_wait_all);
  add("lyra_rt_fork_wait_first", &lyra_rt_fork_wait_first);
  add("lyra_rt_wait_fork", &lyra_rt_wait_fork);
  add("lyra_rt_disable_fork", &lyra_rt_disable_fork);
  add("lyra_rt_cancellation_for", &lyra_rt_cancellation_for);
  add("lyra_rt_is_cancelled", &lyra_rt_is_cancelled);
  add("lyra_rt_closure_make", &lyra_rt_closure_make);
  add("lyra_rt_object_adopt", &lyra_rt_object_adopt);
  add("lyra_rt_string_shared_cell_make", &lyra_rt_string_shared_cell_make);
  add("lyra_rt_real_shared_cell_make", &lyra_rt_real_shared_cell_make);
  add("lyra_rt_shortreal_shared_cell_make",
      &lyra_rt_shortreal_shared_cell_make);
  add("lyra_rt_chandle_shared_cell_make", &lyra_rt_chandle_shared_cell_make);
  add("lyra_rt_managedref_shared_cell_make",
      &lyra_rt_managedref_shared_cell_make);
  add("lyra_rt_tuple_shared_cell_make", &lyra_rt_tuple_shared_cell_make);
  add("lyra_rt_union_shared_cell_make", &lyra_rt_union_shared_cell_make);
  add("lyra_rt_tagged_union_shared_cell_make",
      &lyra_rt_tagged_union_shared_cell_make);
  add("lyra_rt_dynarray_shared_cell_make", &lyra_rt_dynarray_shared_cell_make);
  add("lyra_rt_unpackedarray_shared_cell_make",
      &lyra_rt_unpackedarray_shared_cell_make);
  add("lyra_rt_queue_shared_cell_make", &lyra_rt_queue_shared_cell_make);
  add("lyra_rt_assocarray_shared_cell_make",
      &lyra_rt_assocarray_shared_cell_make);
  add("lyra_rt_shared_pointer_deref", &lyra_rt_shared_pointer_deref);
  add("lyra_rt_handle_view", &lyra_rt_handle_view);
  add("lyra_rt_handle_with_view", &lyra_rt_handle_with_view);
  add("lyra_rt_view_of", &lyra_rt_view_of);
  add("lyra_rt_self_handle", &lyra_rt_self_handle);
  add("lyra_rt_object_event_source", &lyra_rt_object_event_source);
  add("lyra_rt_open_object_write", &lyra_rt_open_object_write);
  add("lyra_rt_written_object", &lyra_rt_written_object);
  add("lyra_rt_enumeration_has", &lyra_rt_enumeration_has);
  add("lyra_rt_enumeration_name", &lyra_rt_enumeration_name);
  add("lyra_rt_enumeration_next", &lyra_rt_enumeration_next);
  add("lyra_rt_enumeration_prev", &lyra_rt_enumeration_prev);
  add("lyra_rt_submit_nba", &lyra_rt_submit_nba);
  add("lyra_rt_submit_nba_after", &lyra_rt_submit_nba_after);
  add("lyra_rt_submit_nba_after_real", &lyra_rt_submit_nba_after_real);
  add("lyra_rt_run_detached", &lyra_rt_run_detached);
  add("lyra_rt_resume_in_nba_region", &lyra_rt_resume_in_nba_region);
  add("lyra_rt_submit_postponed", &lyra_rt_submit_postponed);
  add("lyra_rt_submit_observed", &lyra_rt_submit_observed);
  add("lyra_rt_submit_violation_report", &lyra_rt_submit_violation_report);
  add("lyra_rt_submit_deferred_observed", &lyra_rt_submit_deferred_observed);
  add("lyra_rt_submit_deferred_final", &lyra_rt_submit_deferred_final);
  add("lyra_rt_delay", &lyra_rt_delay);
  add("lyra_rt_delay_real", &lyra_rt_delay_real);
  add("lyra_rt_make_trigger", &lyra_rt_make_trigger);
  add("lyra_rt_observation_on_reaching", &lyra_rt_observation_on_reaching);
  add("lyra_rt_observation_of_value", &lyra_rt_observation_of_value);
  add("lyra_rt_observation_of_value_qualified",
      &lyra_rt_observation_of_value_qualified);
  add("lyra_rt_observation_qualified", &lyra_rt_observation_qualified);
  add("lyra_rt_observation_fires", &lyra_rt_observation_fires);
  add("lyra_rt_wait_recollecting", &lyra_rt_wait_recollecting);
  add("lyra_rt_wait_until", &lyra_rt_wait_until);
  add("lyra_rt_wait_on", &lyra_rt_wait_on);
  add("lyra_rt_wait_on_implicit_list", &lyra_rt_wait_on_implicit_list);
  add("lyra_rt_park_at", &lyra_rt_park_at);
  add("lyra_rt_read_report_empty", &lyra_rt_read_report_empty);
  add("lyra_rt_read_report_for_implicit_list",
      &lyra_rt_read_report_for_implicit_list);
  add("lyra_rt_read_report_add", &lyra_rt_read_report_add);
  add("lyra_rt_read_report_add_through_handle",
      &lyra_rt_read_report_add_through_handle);
  add("lyra_rt_read_report_enter_call_on_handle",
      &lyra_rt_read_report_enter_call_on_handle);
  add("lyra_rt_read_report_leave_call_on_handle",
      &lyra_rt_read_report_leave_call_on_handle);
  add("lyra_rt_read_report_add_every_object",
      &lyra_rt_read_report_add_every_object);
  add("lyra_rt_read_report_add_write", &lyra_rt_read_report_add_write);
  add("lyra_rt_read_report_settle_as_implicit_list",
      &lyra_rt_read_report_settle_as_implicit_list);
  add("lyra_rt_read_report_enter", &lyra_rt_read_report_enter);
  add("lyra_rt_read_report_leave", &lyra_rt_read_report_leave);
  add("lyra_rt_read_report_runs_the_body", &lyra_rt_read_report_runs_the_body);
  add("lyra_rt_refuse_report", &lyra_rt_refuse_report);
  add("lyra_rt_triggered", &lyra_rt_triggered);
  add("lyra_rt_trigger", &lyra_rt_trigger);
  add("lyra_rt_enter_target", &lyra_rt_enter_target);
  add("lyra_rt_leave_target", &lyra_rt_leave_target);
  add("lyra_rt_disable", &lyra_rt_disable);
  add("lyra_rt_effect_names_target", &lyra_rt_effect_names_target);
  add("lyra_rt_receive_departure", &lyra_rt_receive_departure);
  add("lyra_rt_finish_departure", &lyra_rt_finish_departure);
  add("lyra_rt_decline_departure", &lyra_rt_decline_departure);
  add("lyra_rt_settle_departure", &lyra_rt_settle_departure);
  add("lyra_rt_take_departure_if_due", &lyra_rt_take_departure_if_due);
  add("lyra_rt_sim_time", &lyra_rt_sim_time);
  add("lyra_rt_stime", &lyra_rt_stime);
  add("lyra_rt_realtime", &lyra_rt_realtime);
  add("lyra_rt_finish", &lyra_rt_finish);
  add("lyra_rt_stop", &lyra_rt_stop);
  add("lyra_rt_run_host_command", &lyra_rt_run_host_command);
  add("lyra_rt_run_null_host_command", &lyra_rt_run_null_host_command);
  add("lyra_rt_test_plusargs", &lyra_rt_test_plusargs);
  add("lyra_rt_integral_value_plusargs", &lyra_rt_integral_value_plusargs);
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
  add("lyra_rt_hierarchical_path", &lyra_rt_hierarchical_path);
  add("lyra_rt_parent", &lyra_rt_parent);
  add("lyra_rt_add_owned_child", &lyra_rt_add_owned_child);
  add("lyra_rt_sequence_make", &lyra_rt_sequence_make);
  add("lyra_rt_sequence_extend", &lyra_rt_sequence_extend);
  add("lyra_rt_sequence_element", &lyra_rt_sequence_element);
  add("lyra_rt_enclosing_scope", &lyra_rt_enclosing_scope);
  add("lyra_rt_is_of_class", &lyra_rt_is_of_class);
  add("lyra_rt_run_program", &lyra_rt_run_program);
}

// What a write in progress publishes: opening one on each wrapper that takes
// one, the steps taken within it, and where it lands (LRM 11.5.1, 4.3).
void BindWriteEntries(const auto& add) {
  add("lyra_rt_string_cell_open_for_write",
      &lyra_rt_string_cell_open_for_write);
  add("lyra_rt_real_cell_open_for_write", &lyra_rt_real_cell_open_for_write);
  add("lyra_rt_shortreal_cell_open_for_write",
      &lyra_rt_shortreal_cell_open_for_write);
  add("lyra_rt_chandle_cell_open_for_write",
      &lyra_rt_chandle_cell_open_for_write);
  add("lyra_rt_managedref_cell_open_for_write",
      &lyra_rt_managedref_cell_open_for_write);
  add("lyra_rt_tuple_cell_open_for_write", &lyra_rt_tuple_cell_open_for_write);
  add("lyra_rt_union_cell_open_for_write", &lyra_rt_union_cell_open_for_write);
  add("lyra_rt_tagged_union_cell_open_for_write",
      &lyra_rt_tagged_union_cell_open_for_write);
  add("lyra_rt_dynarray_cell_open_for_write",
      &lyra_rt_dynarray_cell_open_for_write);
  add("lyra_rt_unpackedarray_cell_open_for_write",
      &lyra_rt_unpackedarray_cell_open_for_write);
  add("lyra_rt_queue_cell_open_for_write", &lyra_rt_queue_cell_open_for_write);
  add("lyra_rt_assocarray_cell_open_for_write",
      &lyra_rt_assocarray_cell_open_for_write);
  add("lyra_rt_string_ref_open_for_write", &lyra_rt_string_ref_open_for_write);
  add("lyra_rt_real_ref_open_for_write", &lyra_rt_real_ref_open_for_write);
  add("lyra_rt_shortreal_ref_open_for_write",
      &lyra_rt_shortreal_ref_open_for_write);
  add("lyra_rt_chandle_ref_open_for_write",
      &lyra_rt_chandle_ref_open_for_write);
  add("lyra_rt_managedref_ref_open_for_write",
      &lyra_rt_managedref_ref_open_for_write);
  add("lyra_rt_tuple_ref_open_for_write", &lyra_rt_tuple_ref_open_for_write);
  add("lyra_rt_union_ref_open_for_write", &lyra_rt_union_ref_open_for_write);
  add("lyra_rt_tagged_union_ref_open_for_write",
      &lyra_rt_tagged_union_ref_open_for_write);
  add("lyra_rt_dynarray_ref_open_for_write",
      &lyra_rt_dynarray_ref_open_for_write);
  add("lyra_rt_unpackedarray_ref_open_for_write",
      &lyra_rt_unpackedarray_ref_open_for_write);
  add("lyra_rt_queue_ref_open_for_write", &lyra_rt_queue_ref_open_for_write);
  add("lyra_rt_assocarray_ref_open_for_write",
      &lyra_rt_assocarray_ref_open_for_write);
  add("lyra_rt_tuple_driver_open_for_write",
      &lyra_rt_tuple_driver_open_for_write);
  add("lyra_rt_union_driver_open_for_write",
      &lyra_rt_union_driver_open_for_write);
  add("lyra_rt_unpackedarray_driver_open_for_write",
      &lyra_rt_unpackedarray_driver_open_for_write);
  add("lyra_rt_designate_whole", &lyra_rt_designate_whole);
  add("lyra_rt_dynarray_designate_element",
      &lyra_rt_dynarray_designate_element);
  add("lyra_rt_unpackedarray_designate_element",
      &lyra_rt_unpackedarray_designate_element);
  add("lyra_rt_queue_designate_element", &lyra_rt_queue_designate_element);
  add("lyra_rt_assocarray_designate_element",
      &lyra_rt_assocarray_designate_element);
  add("lyra_rt_tuple_designate_component", &lyra_rt_tuple_designate_component);
  add("lyra_rt_dynarray_assign_slice", &lyra_rt_dynarray_assign_slice);
  add("lyra_rt_unpackedarray_assign_slice",
      &lyra_rt_unpackedarray_assign_slice);
  add("lyra_rt_string_land", &lyra_rt_string_land);
  add("lyra_rt_real_land", &lyra_rt_real_land);
  add("lyra_rt_shortreal_land", &lyra_rt_shortreal_land);
  add("lyra_rt_chandle_land", &lyra_rt_chandle_land);
  add("lyra_rt_empty_land", &lyra_rt_empty_land);
  add("lyra_rt_tuple_land", &lyra_rt_tuple_land);
  add("lyra_rt_union_land", &lyra_rt_union_land);
  add("lyra_rt_tagged_union_land", &lyra_rt_tagged_union_land);
  add("lyra_rt_dynarray_land", &lyra_rt_dynarray_land);
  add("lyra_rt_unpackedarray_land", &lyra_rt_unpackedarray_land);
  add("lyra_rt_queue_land", &lyra_rt_queue_land);
  add("lyra_rt_assocarray_land", &lyra_rt_assocarray_land);
  add("lyra_rt_managedref_land", &lyra_rt_managedref_land);
}

// What a value publishes: entries named for a value domain, which are the
// storage a value of it lives in and the operations the language defines over
// it. An entry here names its domain, so gaining a domain adds entries rather
// than changing any.
void BindValueEntries(const auto& add) {
  add("lyra_rt_refer_storage", &lyra_rt_refer_storage);
  add("lyra_rt_refer_property", &lyra_rt_refer_property);
  add("lyra_rt_reference_reports_to", &lyra_rt_reference_reports_to);
  add("lyra_rt_bind_member", &lyra_rt_bind_member);
  add("lyra_rt_drives_continuously", &lyra_rt_drives_continuously);
  add("lyra_rt_string_cell_refer", &lyra_rt_string_cell_refer);
  add("lyra_rt_real_cell_refer", &lyra_rt_real_cell_refer);
  add("lyra_rt_shortreal_cell_refer", &lyra_rt_shortreal_cell_refer);
  add("lyra_rt_chandle_cell_refer", &lyra_rt_chandle_cell_refer);
  add("lyra_rt_managedref_cell_refer", &lyra_rt_managedref_cell_refer);
  add("lyra_rt_tuple_cell_refer", &lyra_rt_tuple_cell_refer);
  add("lyra_rt_union_cell_refer", &lyra_rt_union_cell_refer);
  add("lyra_rt_tagged_union_cell_refer", &lyra_rt_tagged_union_cell_refer);
  add("lyra_rt_dynarray_cell_refer", &lyra_rt_dynarray_cell_refer);
  add("lyra_rt_unpackedarray_cell_refer", &lyra_rt_unpackedarray_cell_refer);
  add("lyra_rt_queue_cell_refer", &lyra_rt_queue_cell_refer);
  add("lyra_rt_assocarray_cell_refer", &lyra_rt_assocarray_cell_refer);
  add("lyra_rt_dynarray_refer_element", &lyra_rt_dynarray_refer_element);
  add("lyra_rt_unpackedarray_refer_element",
      &lyra_rt_unpackedarray_refer_element);
  add("lyra_rt_queue_refer_element", &lyra_rt_queue_refer_element);
  add("lyra_rt_assocarray_refer_element", &lyra_rt_assocarray_refer_element);
  add("lyra_rt_tuple_refer_component", &lyra_rt_tuple_refer_component);
  add("lyra_rt_string_ref_get", &lyra_rt_string_ref_get);
  add("lyra_rt_string_ref_set", &lyra_rt_string_ref_set);
  add("lyra_rt_string_ref_arm_sampling", &lyra_rt_string_ref_arm_sampling);
  add("lyra_rt_string_ref_sampled_load", &lyra_rt_string_ref_sampled_load);
  add("lyra_rt_real_ref_get", &lyra_rt_real_ref_get);
  add("lyra_rt_real_ref_set", &lyra_rt_real_ref_set);
  add("lyra_rt_real_ref_arm_sampling", &lyra_rt_real_ref_arm_sampling);
  add("lyra_rt_real_ref_sampled_load", &lyra_rt_real_ref_sampled_load);
  add("lyra_rt_shortreal_ref_get", &lyra_rt_shortreal_ref_get);
  add("lyra_rt_shortreal_ref_set", &lyra_rt_shortreal_ref_set);
  add("lyra_rt_shortreal_ref_arm_sampling",
      &lyra_rt_shortreal_ref_arm_sampling);
  add("lyra_rt_shortreal_ref_sampled_load",
      &lyra_rt_shortreal_ref_sampled_load);
  add("lyra_rt_chandle_ref_get", &lyra_rt_chandle_ref_get);
  add("lyra_rt_chandle_ref_set", &lyra_rt_chandle_ref_set);
  add("lyra_rt_chandle_ref_arm_sampling", &lyra_rt_chandle_ref_arm_sampling);
  add("lyra_rt_chandle_ref_sampled_load", &lyra_rt_chandle_ref_sampled_load);
  add("lyra_rt_managedref_ref_get", &lyra_rt_managedref_ref_get);
  add("lyra_rt_managedref_ref_set", &lyra_rt_managedref_ref_set);
  add("lyra_rt_managedref_ref_arm_sampling",
      &lyra_rt_managedref_ref_arm_sampling);
  add("lyra_rt_managedref_ref_sampled_load",
      &lyra_rt_managedref_ref_sampled_load);
  add("lyra_rt_tuple_ref_get", &lyra_rt_tuple_ref_get);
  add("lyra_rt_tuple_ref_set", &lyra_rt_tuple_ref_set);
  add("lyra_rt_tuple_ref_arm_sampling", &lyra_rt_tuple_ref_arm_sampling);
  add("lyra_rt_tuple_ref_sampled_load", &lyra_rt_tuple_ref_sampled_load);
  add("lyra_rt_union_ref_get", &lyra_rt_union_ref_get);
  add("lyra_rt_union_ref_set", &lyra_rt_union_ref_set);
  add("lyra_rt_union_ref_arm_sampling", &lyra_rt_union_ref_arm_sampling);
  add("lyra_rt_union_ref_sampled_load", &lyra_rt_union_ref_sampled_load);
  add("lyra_rt_tagged_union_ref_get", &lyra_rt_tagged_union_ref_get);
  add("lyra_rt_tagged_union_ref_set", &lyra_rt_tagged_union_ref_set);
  add("lyra_rt_tagged_union_ref_arm_sampling",
      &lyra_rt_tagged_union_ref_arm_sampling);
  add("lyra_rt_tagged_union_ref_sampled_load",
      &lyra_rt_tagged_union_ref_sampled_load);
  add("lyra_rt_dynarray_ref_get", &lyra_rt_dynarray_ref_get);
  add("lyra_rt_dynarray_ref_set", &lyra_rt_dynarray_ref_set);
  add("lyra_rt_dynarray_ref_arm_sampling", &lyra_rt_dynarray_ref_arm_sampling);
  add("lyra_rt_dynarray_ref_sampled_load", &lyra_rt_dynarray_ref_sampled_load);
  add("lyra_rt_unpackedarray_ref_get", &lyra_rt_unpackedarray_ref_get);
  add("lyra_rt_unpackedarray_ref_set", &lyra_rt_unpackedarray_ref_set);
  add("lyra_rt_unpackedarray_ref_arm_sampling",
      &lyra_rt_unpackedarray_ref_arm_sampling);
  add("lyra_rt_unpackedarray_ref_sampled_load",
      &lyra_rt_unpackedarray_ref_sampled_load);
  add("lyra_rt_queue_ref_get", &lyra_rt_queue_ref_get);
  add("lyra_rt_queue_ref_set", &lyra_rt_queue_ref_set);
  add("lyra_rt_queue_ref_arm_sampling", &lyra_rt_queue_ref_arm_sampling);
  add("lyra_rt_queue_ref_sampled_load", &lyra_rt_queue_ref_sampled_load);
  add("lyra_rt_assocarray_ref_get", &lyra_rt_assocarray_ref_get);
  add("lyra_rt_assocarray_ref_set", &lyra_rt_assocarray_ref_set);
  add("lyra_rt_assocarray_ref_arm_sampling",
      &lyra_rt_assocarray_ref_arm_sampling);
  add("lyra_rt_assocarray_ref_sampled_load",
      &lyra_rt_assocarray_ref_sampled_load);
  add("lyra_rt_string_cell_get", &lyra_rt_string_cell_get);
  add("lyra_rt_string_cell_initialize", &lyra_rt_string_cell_initialize);
  add("lyra_rt_string_cell_set", &lyra_rt_string_cell_set);
  add("lyra_rt_string_cell_arm_sampling", &lyra_rt_string_cell_arm_sampling);
  add("lyra_rt_string_cell_sampled_load", &lyra_rt_string_cell_sampled_load);
  add("lyra_rt_real_cell_get", &lyra_rt_real_cell_get);
  add("lyra_rt_real_cell_initialize", &lyra_rt_real_cell_initialize);
  add("lyra_rt_real_cell_set", &lyra_rt_real_cell_set);
  add("lyra_rt_real_cell_arm_sampling", &lyra_rt_real_cell_arm_sampling);
  add("lyra_rt_real_cell_sampled_load", &lyra_rt_real_cell_sampled_load);
  add("lyra_rt_shortreal_cell_get", &lyra_rt_shortreal_cell_get);
  add("lyra_rt_shortreal_cell_initialize", &lyra_rt_shortreal_cell_initialize);
  add("lyra_rt_shortreal_cell_set", &lyra_rt_shortreal_cell_set);
  add("lyra_rt_shortreal_cell_arm_sampling",
      &lyra_rt_shortreal_cell_arm_sampling);
  add("lyra_rt_shortreal_cell_sampled_load",
      &lyra_rt_shortreal_cell_sampled_load);
  add("lyra_rt_string_sampled_history_install",
      &lyra_rt_string_sampled_history_install);
  add("lyra_rt_string_sampled_history_push",
      &lyra_rt_string_sampled_history_push);
  add("lyra_rt_string_sampled_history_at", &lyra_rt_string_sampled_history_at);
  add("lyra_rt_real_sampled_history_install",
      &lyra_rt_real_sampled_history_install);
  add("lyra_rt_real_sampled_history_push", &lyra_rt_real_sampled_history_push);
  add("lyra_rt_real_sampled_history_at", &lyra_rt_real_sampled_history_at);
  add("lyra_rt_shortreal_sampled_history_install",
      &lyra_rt_shortreal_sampled_history_install);
  add("lyra_rt_shortreal_sampled_history_push",
      &lyra_rt_shortreal_sampled_history_push);
  add("lyra_rt_shortreal_sampled_history_at",
      &lyra_rt_shortreal_sampled_history_at);
  add("lyra_rt_tuple_sampled_history_install",
      &lyra_rt_tuple_sampled_history_install);
  add("lyra_rt_tuple_sampled_history_push",
      &lyra_rt_tuple_sampled_history_push);
  add("lyra_rt_tuple_sampled_history_at", &lyra_rt_tuple_sampled_history_at);
  add("lyra_rt_union_sampled_history_install",
      &lyra_rt_union_sampled_history_install);
  add("lyra_rt_union_sampled_history_push",
      &lyra_rt_union_sampled_history_push);
  add("lyra_rt_union_sampled_history_at", &lyra_rt_union_sampled_history_at);
  add("lyra_rt_tagged_union_sampled_history_install",
      &lyra_rt_tagged_union_sampled_history_install);
  add("lyra_rt_tagged_union_sampled_history_push",
      &lyra_rt_tagged_union_sampled_history_push);
  add("lyra_rt_tagged_union_sampled_history_at",
      &lyra_rt_tagged_union_sampled_history_at);
  add("lyra_rt_dynarray_sampled_history_install",
      &lyra_rt_dynarray_sampled_history_install);
  add("lyra_rt_dynarray_sampled_history_push",
      &lyra_rt_dynarray_sampled_history_push);
  add("lyra_rt_dynarray_sampled_history_at",
      &lyra_rt_dynarray_sampled_history_at);
  add("lyra_rt_unpackedarray_sampled_history_install",
      &lyra_rt_unpackedarray_sampled_history_install);
  add("lyra_rt_unpackedarray_sampled_history_push",
      &lyra_rt_unpackedarray_sampled_history_push);
  add("lyra_rt_unpackedarray_sampled_history_at",
      &lyra_rt_unpackedarray_sampled_history_at);
  add("lyra_rt_queue_sampled_history_install",
      &lyra_rt_queue_sampled_history_install);
  add("lyra_rt_queue_sampled_history_push",
      &lyra_rt_queue_sampled_history_push);
  add("lyra_rt_queue_sampled_history_at", &lyra_rt_queue_sampled_history_at);
  add("lyra_rt_assocarray_sampled_history_install",
      &lyra_rt_assocarray_sampled_history_install);
  add("lyra_rt_assocarray_sampled_history_push",
      &lyra_rt_assocarray_sampled_history_push);
  add("lyra_rt_assocarray_sampled_history_at",
      &lyra_rt_assocarray_sampled_history_at);
  add("lyra_rt_managedref_sampled_history_install",
      &lyra_rt_managedref_sampled_history_install);
  add("lyra_rt_managedref_sampled_history_push",
      &lyra_rt_managedref_sampled_history_push);
  add("lyra_rt_managedref_sampled_history_at",
      &lyra_rt_managedref_sampled_history_at);
  add("lyra_rt_evaluation_attempts_install",
      &lyra_rt_evaluation_attempts_install);
  add("lyra_rt_evaluation_attempts_seed_word",
      &lyra_rt_evaluation_attempts_seed_word);
  add("lyra_rt_evaluation_attempts_begin_tick",
      &lyra_rt_evaluation_attempts_begin_tick);
  add("lyra_rt_evaluation_attempts_disable_tick",
      &lyra_rt_evaluation_attempts_disable_tick);
  add("lyra_rt_evaluation_attempts_live_word",
      &lyra_rt_evaluation_attempts_live_word);
  add("lyra_rt_evaluation_attempts_next_unstepped",
      &lyra_rt_evaluation_attempts_next_unstepped);
  add("lyra_rt_evaluation_attempts_bits_at",
      &lyra_rt_evaluation_attempts_bits_at);
  add("lyra_rt_evaluation_attempts_set_word",
      &lyra_rt_evaluation_attempts_set_word);
  add("lyra_rt_evaluation_attempts_step", &lyra_rt_evaluation_attempts_step);
  add("lyra_rt_evaluation_attempts_seed", &lyra_rt_evaluation_attempts_seed);
  add("lyra_rt_evaluation_attempts_settle",
      &lyra_rt_evaluation_attempts_settle);
  add("lyra_rt_string_value_cell_alloc", &lyra_rt_string_value_cell_alloc);
  add("lyra_rt_string_value_cell_store", &lyra_rt_string_value_cell_store);
  add("lyra_rt_string_value_cell_load", &lyra_rt_string_value_cell_load);
  add("lyra_rt_require", &lyra_rt_require);
  add("lyra_rt_string_from_bits", &lyra_rt_string_from_bits);
  add("lyra_rt_string_from_byte_array", &lyra_rt_string_from_byte_array);
  add("lyra_rt_string_count_bits", &lyra_rt_string_count_bits);
  add("lyra_rt_string_cstr", &lyra_rt_string_cstr);
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
  add("lyra_rt_replicate_string", &lyra_rt_replicate_string);
  add("lyra_rt_string_concat", &lyra_rt_string_concat);
  add("lyra_rt_string_eq", &lyra_rt_string_eq);
  add("lyra_rt_string_case_equal", &lyra_rt_string_case_equal);
  add("lyra_rt_string_ne", &lyra_rt_string_ne);
  add("lyra_rt_string_lt", &lyra_rt_string_lt);
  add("lyra_rt_string_le", &lyra_rt_string_le);
  add("lyra_rt_string_gt", &lyra_rt_string_gt);
  add("lyra_rt_string_ge", &lyra_rt_string_ge);
  add("lyra_rt_make_format_spec", &lyra_rt_make_format_spec);
  add("lyra_rt_integral_make_print_value_item",
      &lyra_rt_integral_make_print_value_item);
  add("lyra_rt_string_make_print_value_item",
      &lyra_rt_string_make_print_value_item);
  add("lyra_rt_chandle_make_print_value_item",
      &lyra_rt_chandle_make_print_value_item);
  add("lyra_rt_managedref_make_print_value_item",
      &lyra_rt_managedref_make_print_value_item);
  add("lyra_rt_format_runtime", &lyra_rt_format_runtime);
  add("lyra_rt_integral_make_format_arg", &lyra_rt_integral_make_format_arg);
  add("lyra_rt_string_make_format_arg", &lyra_rt_string_make_format_arg);
  add("lyra_rt_make_patterned_format_arg", &lyra_rt_make_patterned_format_arg);
  add("lyra_rt_make_rendered_format_arg", &lyra_rt_make_rendered_format_arg);
  add("lyra_rt_chandle_make_format_arg", &lyra_rt_chandle_make_format_arg);
  add("lyra_rt_managedref_make_format_arg",
      &lyra_rt_managedref_make_format_arg);
  add("lyra_rt_make_dpi_bit_buffer", &lyra_rt_make_dpi_bit_buffer);
  add("lyra_rt_make_dpi_logic_buffer", &lyra_rt_make_dpi_logic_buffer);
  add("lyra_rt_dpi_bit_buffer_data", &lyra_rt_dpi_bit_buffer_data);
  add("lyra_rt_dpi_logic_buffer_data", &lyra_rt_dpi_logic_buffer_data);
  add("lyra_rt_write_canonical_bit_vec", &lyra_rt_write_canonical_bit_vec);
  add("lyra_rt_write_canonical_logic_vec", &lyra_rt_write_canonical_logic_vec);
  add("lyra_rt_to_sv_logic", &lyra_rt_to_sv_logic);
  add("lyra_rt_make_dpi_open_array", &lyra_rt_make_dpi_open_array);
  add("lyra_rt_dpi_open_array_handle", &lyra_rt_dpi_open_array_handle);
  add("lyra_rt_dpi_open_array_value", &lyra_rt_dpi_open_array_value);
  add("lyra_rt_string_destroy", &lyra_rt_string_destroy);
  add("lyra_rt_union_destroy", &lyra_rt_union_destroy);
  add("lyra_rt_tagged_union_destroy", &lyra_rt_tagged_union_destroy);
  add("lyra_rt_dynarray_destroy", &lyra_rt_dynarray_destroy);
  add("lyra_rt_unpackedarray_destroy", &lyra_rt_unpackedarray_destroy);
  add("lyra_rt_queue_destroy", &lyra_rt_queue_destroy);
  add("lyra_rt_assocarray_destroy", &lyra_rt_assocarray_destroy);
  add("lyra_rt_managedref_destroy", &lyra_rt_managedref_destroy);
  add("lyra_rt_closure_destroy", &lyra_rt_closure_destroy);
  add("lyra_rt_hierarchy_segment_destroy", &lyra_rt_hierarchy_segment_destroy);
  add("lyra_rt_trigger_destroy", &lyra_rt_trigger_destroy);
  add("lyra_rt_observation_destroy", &lyra_rt_observation_destroy);
  add("lyra_rt_read_report_destroy", &lyra_rt_read_report_destroy);
  add("lyra_rt_wait_destroy", &lyra_rt_wait_destroy);
  add("lyra_rt_dpi_bit_buffer_destroy", &lyra_rt_dpi_bit_buffer_destroy);
  add("lyra_rt_dpi_logic_buffer_destroy", &lyra_rt_dpi_logic_buffer_destroy);
  add("lyra_rt_dpi_open_array_destroy", &lyra_rt_dpi_open_array_destroy);
  add("lyra_rt_channel_cancellation_destroy",
      &lyra_rt_channel_cancellation_destroy);
  add("lyra_rt_shared_pointer_destroy", &lyra_rt_shared_pointer_destroy);
  add("lyra_rt_open_write_destroy", &lyra_rt_open_write_destroy);
  add("lyra_rt_object_write_destroy", &lyra_rt_object_write_destroy);
  add("lyra_rt_string_assign", &lyra_rt_string_assign);
  add("lyra_rt_real_assign", &lyra_rt_real_assign);
  add("lyra_rt_shortreal_assign", &lyra_rt_shortreal_assign);
  add("lyra_rt_chandle_assign", &lyra_rt_chandle_assign);
  add("lyra_rt_empty_assign", &lyra_rt_empty_assign);
  add("lyra_rt_union_assign", &lyra_rt_union_assign);
  add("lyra_rt_tagged_union_assign", &lyra_rt_tagged_union_assign);
  add("lyra_rt_dynarray_assign", &lyra_rt_dynarray_assign);
  add("lyra_rt_unpackedarray_assign", &lyra_rt_unpackedarray_assign);
  add("lyra_rt_queue_assign", &lyra_rt_queue_assign);
  add("lyra_rt_assocarray_assign", &lyra_rt_assocarray_assign);
  add("lyra_rt_managedref_assign", &lyra_rt_managedref_assign);
  add("lyra_rt_reference_assign", &lyra_rt_reference_assign);
  add("lyra_rt_string_copy", &lyra_rt_string_copy);
  add("lyra_rt_real_copy", &lyra_rt_real_copy);
  add("lyra_rt_shortreal_copy", &lyra_rt_shortreal_copy);
  add("lyra_rt_chandle_copy", &lyra_rt_chandle_copy);
  add("lyra_rt_empty_copy", &lyra_rt_empty_copy);
  add("lyra_rt_union_copy", &lyra_rt_union_copy);
  add("lyra_rt_tagged_union_copy", &lyra_rt_tagged_union_copy);
  add("lyra_rt_dynarray_copy", &lyra_rt_dynarray_copy);
  add("lyra_rt_unpackedarray_copy", &lyra_rt_unpackedarray_copy);
  add("lyra_rt_queue_copy", &lyra_rt_queue_copy);
  add("lyra_rt_assocarray_copy", &lyra_rt_assocarray_copy);
  add("lyra_rt_managedref_copy", &lyra_rt_managedref_copy);
  add("lyra_rt_shared_pointer_copy", &lyra_rt_shared_pointer_copy);
  add("lyra_rt_print_item_copy", &lyra_rt_print_item_copy);
  add("lyra_rt_format_spec_copy", &lyra_rt_format_spec_copy);
  add("lyra_rt_format_arg_copy", &lyra_rt_format_arg_copy);
  add("lyra_rt_hierarchy_segment_copy", &lyra_rt_hierarchy_segment_copy);
  add("lyra_rt_trigger_copy", &lyra_rt_trigger_copy);
  add("lyra_rt_observation_copy", &lyra_rt_observation_copy);
  add("lyra_rt_dpi_bit_buffer_copy", &lyra_rt_dpi_bit_buffer_copy);
  add("lyra_rt_dpi_logic_buffer_copy", &lyra_rt_dpi_logic_buffer_copy);
  add("lyra_rt_dpi_open_array_copy", &lyra_rt_dpi_open_array_copy);
  add("lyra_rt_channel_cancellation_copy", &lyra_rt_channel_cancellation_copy);
  add("lyra_rt_reference_copy", &lyra_rt_reference_copy);
  add("lyra_rt_string_move", &lyra_rt_string_move);
  add("lyra_rt_real_move", &lyra_rt_real_move);
  add("lyra_rt_shortreal_move", &lyra_rt_shortreal_move);
  add("lyra_rt_chandle_move", &lyra_rt_chandle_move);
  add("lyra_rt_empty_move", &lyra_rt_empty_move);
  add("lyra_rt_union_move", &lyra_rt_union_move);
  add("lyra_rt_tagged_union_move", &lyra_rt_tagged_union_move);
  add("lyra_rt_dynarray_move", &lyra_rt_dynarray_move);
  add("lyra_rt_unpackedarray_move", &lyra_rt_unpackedarray_move);
  add("lyra_rt_queue_move", &lyra_rt_queue_move);
  add("lyra_rt_assocarray_move", &lyra_rt_assocarray_move);
  add("lyra_rt_managedref_move", &lyra_rt_managedref_move);
  add("lyra_rt_closure_move", &lyra_rt_closure_move);
  add("lyra_rt_shared_pointer_move", &lyra_rt_shared_pointer_move);
  add("lyra_rt_print_item_move", &lyra_rt_print_item_move);
  add("lyra_rt_format_spec_move", &lyra_rt_format_spec_move);
  add("lyra_rt_format_arg_move", &lyra_rt_format_arg_move);
  add("lyra_rt_hierarchy_segment_move", &lyra_rt_hierarchy_segment_move);
  add("lyra_rt_trigger_move", &lyra_rt_trigger_move);
  add("lyra_rt_observation_move", &lyra_rt_observation_move);
  add("lyra_rt_read_report_move", &lyra_rt_read_report_move);
  add("lyra_rt_wait_move", &lyra_rt_wait_move);
  add("lyra_rt_dpi_bit_buffer_move", &lyra_rt_dpi_bit_buffer_move);
  add("lyra_rt_dpi_logic_buffer_move", &lyra_rt_dpi_logic_buffer_move);
  add("lyra_rt_dpi_open_array_move", &lyra_rt_dpi_open_array_move);
  add("lyra_rt_channel_cancellation_move", &lyra_rt_channel_cancellation_move);
  add("lyra_rt_reference_move", &lyra_rt_reference_move);
  add("lyra_rt_real_add", &lyra_rt_real_add);
  add("lyra_rt_real_sub", &lyra_rt_real_sub);
  add("lyra_rt_real_mul", &lyra_rt_real_mul);
  add("lyra_rt_real_div", &lyra_rt_real_div);
  add("lyra_rt_real_neg", &lyra_rt_real_neg);
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
  add("lyra_rt_real_make_format_arg", &lyra_rt_real_make_format_arg);
  add("lyra_rt_shortreal_add", &lyra_rt_shortreal_add);
  add("lyra_rt_shortreal_sub", &lyra_rt_shortreal_sub);
  add("lyra_rt_shortreal_mul", &lyra_rt_shortreal_mul);
  add("lyra_rt_shortreal_div", &lyra_rt_shortreal_div);
  add("lyra_rt_shortreal_neg", &lyra_rt_shortreal_neg);
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
  add("lyra_rt_shortreal_make_format_arg", &lyra_rt_shortreal_make_format_arg);
  add("lyra_rt_chandle_eq", &lyra_rt_chandle_eq);
  add("lyra_rt_chandle_ne", &lyra_rt_chandle_ne);
  add("lyra_rt_chandle_case_equal", &lyra_rt_chandle_case_equal);
  add("lyra_rt_chandle_to_bool", &lyra_rt_chandle_to_bool);
  add("lyra_rt_chandle_default", &lyra_rt_chandle_default);
  add("lyra_rt_chandle_make", &lyra_rt_chandle_make);
  add("lyra_rt_chandle_ptr", &lyra_rt_chandle_ptr);
  add("lyra_rt_chandle_value_cell_alloc", &lyra_rt_chandle_value_cell_alloc);
  add("lyra_rt_chandle_value_cell_store", &lyra_rt_chandle_value_cell_store);
  add("lyra_rt_chandle_value_cell_load", &lyra_rt_chandle_value_cell_load);
  add("lyra_rt_chandle_cell_get", &lyra_rt_chandle_cell_get);
  add("lyra_rt_chandle_cell_initialize", &lyra_rt_chandle_cell_initialize);
  add("lyra_rt_chandle_cell_set", &lyra_rt_chandle_cell_set);
  add("lyra_rt_chandle_cell_arm_sampling", &lyra_rt_chandle_cell_arm_sampling);
  add("lyra_rt_chandle_cell_sampled_load", &lyra_rt_chandle_cell_sampled_load);
  add("lyra_rt_managedref_default", &lyra_rt_managedref_default);
  add("lyra_rt_managedref_value_cell_alloc",
      &lyra_rt_managedref_value_cell_alloc);
  add("lyra_rt_managedref_value_cell_store",
      &lyra_rt_managedref_value_cell_store);
  add("lyra_rt_managedref_value_cell_load",
      &lyra_rt_managedref_value_cell_load);
  add("lyra_rt_managedref_cell_get", &lyra_rt_managedref_cell_get);
  add("lyra_rt_managedref_cell_initialize",
      &lyra_rt_managedref_cell_initialize);
  add("lyra_rt_managedref_cell_set", &lyra_rt_managedref_cell_set);
  add("lyra_rt_managedref_cell_arm_sampling",
      &lyra_rt_managedref_cell_arm_sampling);
  add("lyra_rt_managedref_cell_sampled_load",
      &lyra_rt_managedref_cell_sampled_load);
  add("lyra_rt_managedref_eq", &lyra_rt_managedref_eq);
  add("lyra_rt_managedref_ne", &lyra_rt_managedref_ne);
  add("lyra_rt_managedref_case_equal", &lyra_rt_managedref_case_equal);
  add("lyra_rt_managedref_to_bool", &lyra_rt_managedref_to_bool);
  add("lyra_rt_process_self", &lyra_rt_process_self);
  add("lyra_rt_process_status", &lyra_rt_process_status);
  add("lyra_rt_process_kill", &lyra_rt_process_kill);
  add("lyra_rt_process_await", &lyra_rt_process_await);
  add("lyra_rt_process_suspend", &lyra_rt_process_suspend);
  add("lyra_rt_process_resume", &lyra_rt_process_resume);
  add("lyra_rt_tuple_cell_get", &lyra_rt_tuple_cell_get);
  add("lyra_rt_tuple_cell_initialize", &lyra_rt_tuple_cell_initialize);
  add("lyra_rt_tuple_cell_set", &lyra_rt_tuple_cell_set);
  add("lyra_rt_tuple_cell_arm_sampling", &lyra_rt_tuple_cell_arm_sampling);
  add("lyra_rt_tuple_cell_sampled_load", &lyra_rt_tuple_cell_sampled_load);
  add("lyra_rt_tuple_value_cell_alloc", &lyra_rt_tuple_value_cell_alloc);
  add("lyra_rt_tuple_value_cell_store", &lyra_rt_tuple_value_cell_store);
  add("lyra_rt_tuple_value_cell_load", &lyra_rt_tuple_value_cell_load);
  add("lyra_rt_union_make", &lyra_rt_union_make);
  add("lyra_rt_union_component", &lyra_rt_union_component);
  add("lyra_rt_union_with_component", &lyra_rt_union_with_component);
  add("lyra_rt_union_eq", &lyra_rt_union_eq);
  add("lyra_rt_union_ne", &lyra_rt_union_ne);
  add("lyra_rt_union_case_equal", &lyra_rt_union_case_equal);
  add("lyra_rt_union_is_unknown", &lyra_rt_union_is_unknown);
  add("lyra_rt_union_cell_get", &lyra_rt_union_cell_get);
  add("lyra_rt_union_cell_initialize", &lyra_rt_union_cell_initialize);
  add("lyra_rt_union_cell_set", &lyra_rt_union_cell_set);
  add("lyra_rt_union_cell_arm_sampling", &lyra_rt_union_cell_arm_sampling);
  add("lyra_rt_union_cell_sampled_load", &lyra_rt_union_cell_sampled_load);
  add("lyra_rt_union_value_cell_alloc", &lyra_rt_union_value_cell_alloc);
  add("lyra_rt_union_value_cell_store", &lyra_rt_union_value_cell_store);
  add("lyra_rt_union_value_cell_load", &lyra_rt_union_value_cell_load);
  add("lyra_rt_tagged_union_make", &lyra_rt_tagged_union_make);
  add("lyra_rt_tagged_union_component", &lyra_rt_tagged_union_component);
  add("lyra_rt_tagged_union_with_component",
      &lyra_rt_tagged_union_with_component);
  add("lyra_rt_tagged_union_tag_matches", &lyra_rt_tagged_union_tag_matches);
  add("lyra_rt_tagged_union_eq", &lyra_rt_tagged_union_eq);
  add("lyra_rt_tagged_union_ne", &lyra_rt_tagged_union_ne);
  add("lyra_rt_tagged_union_case_equal", &lyra_rt_tagged_union_case_equal);
  add("lyra_rt_tagged_union_is_unknown", &lyra_rt_tagged_union_is_unknown);
  add("lyra_rt_tagged_union_cell_get", &lyra_rt_tagged_union_cell_get);
  add("lyra_rt_tagged_union_cell_initialize",
      &lyra_rt_tagged_union_cell_initialize);
  add("lyra_rt_tagged_union_cell_set", &lyra_rt_tagged_union_cell_set);
  add("lyra_rt_tagged_union_cell_arm_sampling",
      &lyra_rt_tagged_union_cell_arm_sampling);
  add("lyra_rt_tagged_union_cell_sampled_load",
      &lyra_rt_tagged_union_cell_sampled_load);
  add("lyra_rt_tagged_union_value_cell_alloc",
      &lyra_rt_tagged_union_value_cell_alloc);
  add("lyra_rt_tagged_union_value_cell_store",
      &lyra_rt_tagged_union_value_cell_store);
  add("lyra_rt_tagged_union_value_cell_load",
      &lyra_rt_tagged_union_value_cell_load);
  add("lyra_rt_empty_default", &lyra_rt_empty_default);
  add("lyra_rt_make_dynamic_array_default",
      &lyra_rt_make_dynamic_array_default);
  add("lyra_rt_make_dynamic_array_new", &lyra_rt_make_dynamic_array_new);
  add("lyra_rt_make_dynamic_array_new_copy",
      &lyra_rt_make_dynamic_array_new_copy);
  add("lyra_rt_dynarray_from_literal", &lyra_rt_dynarray_from_literal);
  add("lyra_rt_unpackedarray_dynamic_array_from_array",
      &lyra_rt_unpackedarray_dynamic_array_from_array);
  add("lyra_rt_queue_dynamic_array_from_array",
      &lyra_rt_queue_dynamic_array_from_array);
  add("lyra_rt_dynarray_element", &lyra_rt_dynarray_element);
  add("lyra_rt_dynarray_concat_element", &lyra_rt_dynarray_concat_element);
  add("lyra_rt_dynarray_concat_spread", &lyra_rt_dynarray_concat_spread);
  add("lyra_rt_dynarray_element_ref", &lyra_rt_dynarray_element_ref);
  add("lyra_rt_dynarray_delete", &lyra_rt_dynarray_delete);
  add("lyra_rt_dynarray_element_slice", &lyra_rt_dynarray_element_slice);
  add("lyra_rt_dynarray_element_slice_ref",
      &lyra_rt_dynarray_element_slice_ref);
  add("lyra_rt_dynarray_size", &lyra_rt_dynarray_size);
  add("lyra_rt_dynarray_eq", &lyra_rt_dynarray_eq);
  add("lyra_rt_dynarray_ne", &lyra_rt_dynarray_ne);
  add("lyra_rt_dynarray_case_equal", &lyra_rt_dynarray_case_equal);
  add("lyra_rt_dynarray_cell_get", &lyra_rt_dynarray_cell_get);
  add("lyra_rt_dynarray_cell_initialize", &lyra_rt_dynarray_cell_initialize);
  add("lyra_rt_dynarray_cell_set", &lyra_rt_dynarray_cell_set);
  add("lyra_rt_dynarray_cell_arm_sampling",
      &lyra_rt_dynarray_cell_arm_sampling);
  add("lyra_rt_dynarray_cell_sampled_load",
      &lyra_rt_dynarray_cell_sampled_load);
  add("lyra_rt_dynarray_value_cell_alloc", &lyra_rt_dynarray_value_cell_alloc);
  add("lyra_rt_dynarray_value_cell_store", &lyra_rt_dynarray_value_cell_store);
  add("lyra_rt_dynarray_value_cell_load", &lyra_rt_dynarray_value_cell_load);
  add("lyra_rt_dynarray_count_bits", &lyra_rt_dynarray_count_bits);
  add("lyra_rt_unpackedarray_from_literal",
      &lyra_rt_unpackedarray_from_literal);
  add("lyra_rt_unpackedarray_conform_size",
      &lyra_rt_unpackedarray_conform_size);
  add("lyra_rt_dynarray_unpacked_array_from_array",
      &lyra_rt_dynarray_unpacked_array_from_array);
  add("lyra_rt_queue_unpacked_array_from_array",
      &lyra_rt_queue_unpacked_array_from_array);
  add("lyra_rt_byte_array_from_string", &lyra_rt_byte_array_from_string);
  add("lyra_rt_queue_from_literal", &lyra_rt_queue_from_literal);
  add("lyra_rt_queue_from_literal_bounded",
      &lyra_rt_queue_from_literal_bounded);
  add("lyra_rt_queue_conform_bound", &lyra_rt_queue_conform_bound);
  add("lyra_rt_unpackedarray_queue_from_array",
      &lyra_rt_unpackedarray_queue_from_array);
  add("lyra_rt_dynarray_queue_from_array", &lyra_rt_dynarray_queue_from_array);
  add("lyra_rt_queue_element", &lyra_rt_queue_element);
  add("lyra_rt_queue_element_ref", &lyra_rt_queue_element_ref);
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
  add("lyra_rt_queue_cell_get", &lyra_rt_queue_cell_get);
  add("lyra_rt_queue_cell_initialize", &lyra_rt_queue_cell_initialize);
  add("lyra_rt_queue_cell_set", &lyra_rt_queue_cell_set);
  add("lyra_rt_queue_cell_arm_sampling", &lyra_rt_queue_cell_arm_sampling);
  add("lyra_rt_queue_cell_sampled_load", &lyra_rt_queue_cell_sampled_load);
  add("lyra_rt_queue_value_cell_alloc", &lyra_rt_queue_value_cell_alloc);
  add("lyra_rt_queue_value_cell_store", &lyra_rt_queue_value_cell_store);
  add("lyra_rt_queue_value_cell_load", &lyra_rt_queue_value_cell_load);
  add("lyra_rt_assocarray_from_entries_default",
      &lyra_rt_assocarray_from_entries_default);
  add("lyra_rt_assocarray_from_entries_default_wildcard",
      &lyra_rt_assocarray_from_entries_default_wildcard);
  add("lyra_rt_assocarray_element", &lyra_rt_assocarray_element);
  add("lyra_rt_assocarray_element_ref", &lyra_rt_assocarray_element_ref);
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
  add("lyra_rt_dynarray_bitstream_width", &lyra_rt_dynarray_bitstream_width);
  add("lyra_rt_unpackedarray_bitstream_width",
      &lyra_rt_unpackedarray_bitstream_width);
  add("lyra_rt_unpackedarray_to_bitstream",
      &lyra_rt_unpackedarray_to_bitstream);
  add("lyra_rt_from_bitstream", &lyra_rt_from_bitstream);
  add("lyra_rt_assocarray_count_bits", &lyra_rt_assocarray_count_bits);
  add("lyra_rt_assocarray_cell_get", &lyra_rt_assocarray_cell_get);
  add("lyra_rt_assocarray_cell_initialize",
      &lyra_rt_assocarray_cell_initialize);
  add("lyra_rt_assocarray_cell_set", &lyra_rt_assocarray_cell_set);
  add("lyra_rt_assocarray_cell_arm_sampling",
      &lyra_rt_assocarray_cell_arm_sampling);
  add("lyra_rt_assocarray_cell_sampled_load",
      &lyra_rt_assocarray_cell_sampled_load);
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
  add("lyra_rt_unpackedarray_element_ref", &lyra_rt_unpackedarray_element_ref);
  add("lyra_rt_unpackedarray_element_slice",
      &lyra_rt_unpackedarray_element_slice);
  add("lyra_rt_unpackedarray_element_slice_ref",
      &lyra_rt_unpackedarray_element_slice_ref);
  add("lyra_rt_unpackedarray_size", &lyra_rt_unpackedarray_size);
  add("lyra_rt_unpackedarray_count_bits", &lyra_rt_unpackedarray_count_bits);
  add("lyra_rt_unpackedarray_eq", &lyra_rt_unpackedarray_eq);
  add("lyra_rt_unpackedarray_ne", &lyra_rt_unpackedarray_ne);
  add("lyra_rt_unpackedarray_case_equal", &lyra_rt_unpackedarray_case_equal);
  add("lyra_rt_unpackedarray_is_unknown", &lyra_rt_unpackedarray_is_unknown);
  add("lyra_rt_unpackedarray_cell_get", &lyra_rt_unpackedarray_cell_get);
  add("lyra_rt_unpackedarray_cell_initialize",
      &lyra_rt_unpackedarray_cell_initialize);
  add("lyra_rt_unpackedarray_cell_set", &lyra_rt_unpackedarray_cell_set);
  add("lyra_rt_unpackedarray_cell_arm_sampling",
      &lyra_rt_unpackedarray_cell_arm_sampling);
  add("lyra_rt_unpackedarray_cell_sampled_load",
      &lyra_rt_unpackedarray_cell_sampled_load);
  add("lyra_rt_unpackedarray_value_cell_alloc",
      &lyra_rt_unpackedarray_value_cell_alloc);
  add("lyra_rt_unpackedarray_value_cell_store",
      &lyra_rt_unpackedarray_value_cell_store);
  add("lyra_rt_unpackedarray_value_cell_load",
      &lyra_rt_unpackedarray_value_cell_load);
  add("lyra_rt_tuple_net_get", &lyra_rt_tuple_net_get);
  add("lyra_rt_tuple_aggregate_net_initialize_tri_state",
      &lyra_rt_tuple_aggregate_net_initialize_tri_state);
  add("lyra_rt_tuple_aggregate_net_initialize_wired_and",
      &lyra_rt_tuple_aggregate_net_initialize_wired_and);
  add("lyra_rt_tuple_aggregate_net_initialize_wired_or",
      &lyra_rt_tuple_aggregate_net_initialize_wired_or);
  add("lyra_rt_tuple_aggregate_net_initialize_retaining",
      &lyra_rt_tuple_aggregate_net_initialize_retaining);
  add("lyra_rt_tuple_net_begin_takeover", &lyra_rt_tuple_net_begin_takeover);
  add("lyra_rt_tuple_net_drive_takeover", &lyra_rt_tuple_net_drive_takeover);
  add("lyra_rt_tuple_net_end_takeover", &lyra_rt_tuple_net_end_takeover);
  add("lyra_rt_tuple_attach_driver", &lyra_rt_tuple_attach_driver);
  add("lyra_rt_tuple_driver_get", &lyra_rt_tuple_driver_get);
  add("lyra_rt_tuple_driver_set", &lyra_rt_tuple_driver_set);
  add("lyra_rt_union_net_get", &lyra_rt_union_net_get);
  add("lyra_rt_union_aggregate_net_initialize_tri_state",
      &lyra_rt_union_aggregate_net_initialize_tri_state);
  add("lyra_rt_union_aggregate_net_initialize_wired_and",
      &lyra_rt_union_aggregate_net_initialize_wired_and);
  add("lyra_rt_union_aggregate_net_initialize_wired_or",
      &lyra_rt_union_aggregate_net_initialize_wired_or);
  add("lyra_rt_union_aggregate_net_initialize_retaining",
      &lyra_rt_union_aggregate_net_initialize_retaining);
  add("lyra_rt_union_net_begin_takeover", &lyra_rt_union_net_begin_takeover);
  add("lyra_rt_union_net_drive_takeover", &lyra_rt_union_net_drive_takeover);
  add("lyra_rt_union_net_end_takeover", &lyra_rt_union_net_end_takeover);
  add("lyra_rt_union_attach_driver", &lyra_rt_union_attach_driver);
  add("lyra_rt_union_driver_get", &lyra_rt_union_driver_get);
  add("lyra_rt_union_driver_set", &lyra_rt_union_driver_set);
  add("lyra_rt_unpackedarray_net_get", &lyra_rt_unpackedarray_net_get);
  add("lyra_rt_unpackedarray_aggregate_net_initialize_tri_state",
      &lyra_rt_unpackedarray_aggregate_net_initialize_tri_state);
  add("lyra_rt_unpackedarray_aggregate_net_initialize_wired_and",
      &lyra_rt_unpackedarray_aggregate_net_initialize_wired_and);
  add("lyra_rt_unpackedarray_aggregate_net_initialize_wired_or",
      &lyra_rt_unpackedarray_aggregate_net_initialize_wired_or);
  add("lyra_rt_unpackedarray_aggregate_net_initialize_retaining",
      &lyra_rt_unpackedarray_aggregate_net_initialize_retaining);
  add("lyra_rt_unpackedarray_net_begin_takeover",
      &lyra_rt_unpackedarray_net_begin_takeover);
  add("lyra_rt_unpackedarray_net_drive_takeover",
      &lyra_rt_unpackedarray_net_drive_takeover);
  add("lyra_rt_unpackedarray_net_end_takeover",
      &lyra_rt_unpackedarray_net_end_takeover);
  add("lyra_rt_unpackedarray_attach_driver",
      &lyra_rt_unpackedarray_attach_driver);
  add("lyra_rt_unpackedarray_driver_get", &lyra_rt_unpackedarray_driver_get);
  add("lyra_rt_unpackedarray_driver_set", &lyra_rt_unpackedarray_driver_set);
  add("lyra_rt_unpackedarray_merge_conditional",
      &lyra_rt_unpackedarray_merge_conditional);
  add("lyra_rt_byte_array_from_bits", &lyra_rt_byte_array_from_bits);
}

// The whole-value operations of each domain a structure's own functions apply
// to its members, which are the whole of what the library holds for a
// structure it never sees.
void BindMemberOperationEntries(const auto& add) {
  add("lyra_rt_wildcard_index_make", &lyra_rt_wildcard_index_make);
  add("lyra_rt_stream_write", &lyra_rt_stream_write);
  add("lyra_rt_stream_read", &lyra_rt_stream_read);
  add("lyra_rt_wildcard_index_copy", &lyra_rt_wildcard_index_copy);
  add("lyra_rt_wildcard_index_move", &lyra_rt_wildcard_index_move);
  add("lyra_rt_wildcard_index_destroy", &lyra_rt_wildcard_index_destroy);
  add("lyra_rt_wildcard_index_assign", &lyra_rt_wildcard_index_assign);
  add("lyra_rt_string_bit_identical", &lyra_rt_string_bit_identical);
  add("lyra_rt_real_bit_identical", &lyra_rt_real_bit_identical);
  add("lyra_rt_shortreal_bit_identical", &lyra_rt_shortreal_bit_identical);
  add("lyra_rt_chandle_bit_identical", &lyra_rt_chandle_bit_identical);
  add("lyra_rt_union_bit_identical", &lyra_rt_union_bit_identical);
  add("lyra_rt_tagged_union_bit_identical",
      &lyra_rt_tagged_union_bit_identical);
  add("lyra_rt_dynarray_bit_identical", &lyra_rt_dynarray_bit_identical);
  add("lyra_rt_unpackedarray_bit_identical",
      &lyra_rt_unpackedarray_bit_identical);
  add("lyra_rt_queue_bit_identical", &lyra_rt_queue_bit_identical);
  add("lyra_rt_assocarray_bit_identical", &lyra_rt_assocarray_bit_identical);
  add("lyra_rt_managedref_bit_identical", &lyra_rt_managedref_bit_identical);
  add("lyra_rt_string_has_unknown", &lyra_rt_string_has_unknown);
  add("lyra_rt_real_has_unknown", &lyra_rt_real_has_unknown);
  add("lyra_rt_shortreal_has_unknown", &lyra_rt_shortreal_has_unknown);
  add("lyra_rt_chandle_has_unknown", &lyra_rt_chandle_has_unknown);
  add("lyra_rt_union_has_unknown", &lyra_rt_union_has_unknown);
  add("lyra_rt_tagged_union_has_unknown", &lyra_rt_tagged_union_has_unknown);
  add("lyra_rt_dynarray_has_unknown", &lyra_rt_dynarray_has_unknown);
  add("lyra_rt_unpackedarray_has_unknown", &lyra_rt_unpackedarray_has_unknown);
  add("lyra_rt_queue_has_unknown", &lyra_rt_queue_has_unknown);
  add("lyra_rt_assocarray_has_unknown", &lyra_rt_assocarray_has_unknown);
  add("lyra_rt_managedref_has_unknown", &lyra_rt_managedref_has_unknown);
  add("lyra_rt_union_bitstream_width", &lyra_rt_union_bitstream_width);
  add("lyra_rt_tagged_union_bitstream_width",
      &lyra_rt_tagged_union_bitstream_width);
  add("lyra_rt_managedref_bitstream_width",
      &lyra_rt_managedref_bitstream_width);
  add("lyra_rt_union_count_bits", &lyra_rt_union_count_bits);
  add("lyra_rt_tagged_union_count_bits", &lyra_rt_tagged_union_count_bits);
  add("lyra_rt_managedref_count_bits", &lyra_rt_managedref_count_bits);
  add("lyra_rt_string_to_bitstream", &lyra_rt_string_to_bitstream);
  add("lyra_rt_union_to_bitstream", &lyra_rt_union_to_bitstream);
  add("lyra_rt_tagged_union_to_bitstream", &lyra_rt_tagged_union_to_bitstream);
  add("lyra_rt_dynarray_to_bitstream", &lyra_rt_dynarray_to_bitstream);
  add("lyra_rt_queue_to_bitstream", &lyra_rt_queue_to_bitstream);
  add("lyra_rt_assocarray_to_bitstream", &lyra_rt_assocarray_to_bitstream);
  add("lyra_rt_managedref_to_bitstream", &lyra_rt_managedref_to_bitstream);
  add("lyra_rt_union_resolve_tri_state", &lyra_rt_union_resolve_tri_state);
  add("lyra_rt_unpackedarray_resolve_tri_state",
      &lyra_rt_unpackedarray_resolve_tri_state);
  add("lyra_rt_union_resolve_wired_and", &lyra_rt_union_resolve_wired_and);
  add("lyra_rt_unpackedarray_resolve_wired_and",
      &lyra_rt_unpackedarray_resolve_wired_and);
  add("lyra_rt_union_resolve_wired_or", &lyra_rt_union_resolve_wired_or);
  add("lyra_rt_unpackedarray_resolve_wired_or",
      &lyra_rt_unpackedarray_resolve_wired_or);
  add("lyra_rt_union_dominating", &lyra_rt_union_dominating);
  add("lyra_rt_unpackedarray_dominating", &lyra_rt_unpackedarray_dominating);
  add("lyra_rt_union_filled_like", &lyra_rt_union_filled_like);
  add("lyra_rt_unpackedarray_filled_like", &lyra_rt_unpackedarray_filled_like);
}

// What a declaration's storage publishes: building one member's storage where
// its owner was laid out, and ending it there. An entry here names the storage
// kind, and the value domain where the kind holds values of one.
void BindStorageEntries(const auto& add) {
  add("lyra_rt_borrowed_handle_construct", &lyra_rt_borrowed_handle_construct);
  add("lyra_rt_reference_construct", &lyra_rt_reference_construct);
  add("lyra_rt_string_cell_construct", &lyra_rt_string_cell_construct);
  add("lyra_rt_real_cell_construct", &lyra_rt_real_cell_construct);
  add("lyra_rt_shortreal_cell_construct", &lyra_rt_shortreal_cell_construct);
  add("lyra_rt_chandle_cell_construct", &lyra_rt_chandle_cell_construct);
  add("lyra_rt_tuple_cell_construct", &lyra_rt_tuple_cell_construct);
  add("lyra_rt_union_cell_construct", &lyra_rt_union_cell_construct);
  add("lyra_rt_tagged_union_cell_construct",
      &lyra_rt_tagged_union_cell_construct);
  add("lyra_rt_dynarray_cell_construct", &lyra_rt_dynarray_cell_construct);
  add("lyra_rt_unpackedarray_cell_construct",
      &lyra_rt_unpackedarray_cell_construct);
  add("lyra_rt_queue_cell_construct", &lyra_rt_queue_cell_construct);
  add("lyra_rt_assocarray_cell_construct", &lyra_rt_assocarray_cell_construct);
  add("lyra_rt_managedref_cell_construct", &lyra_rt_managedref_cell_construct);
  add("lyra_rt_string_value_cell_construct",
      &lyra_rt_string_value_cell_construct);
  add("lyra_rt_real_value_cell_construct", &lyra_rt_real_value_cell_construct);
  add("lyra_rt_shortreal_value_cell_construct",
      &lyra_rt_shortreal_value_cell_construct);
  add("lyra_rt_chandle_value_cell_construct",
      &lyra_rt_chandle_value_cell_construct);
  add("lyra_rt_tuple_value_cell_construct",
      &lyra_rt_tuple_value_cell_construct);
  add("lyra_rt_union_value_cell_construct",
      &lyra_rt_union_value_cell_construct);
  add("lyra_rt_tagged_union_value_cell_construct",
      &lyra_rt_tagged_union_value_cell_construct);
  add("lyra_rt_dynarray_value_cell_construct",
      &lyra_rt_dynarray_value_cell_construct);
  add("lyra_rt_unpackedarray_value_cell_construct",
      &lyra_rt_unpackedarray_value_cell_construct);
  add("lyra_rt_queue_value_cell_construct",
      &lyra_rt_queue_value_cell_construct);
  add("lyra_rt_assocarray_value_cell_construct",
      &lyra_rt_assocarray_value_cell_construct);
  add("lyra_rt_managedref_value_cell_construct",
      &lyra_rt_managedref_value_cell_construct);
  add("lyra_rt_tuple_net_construct", &lyra_rt_tuple_net_construct);
  add("lyra_rt_union_net_construct", &lyra_rt_union_net_construct);
  add("lyra_rt_unpackedarray_net_construct",
      &lyra_rt_unpackedarray_net_construct);
  add("lyra_rt_string_sampled_history_construct",
      &lyra_rt_string_sampled_history_construct);
  add("lyra_rt_real_sampled_history_construct",
      &lyra_rt_real_sampled_history_construct);
  add("lyra_rt_shortreal_sampled_history_construct",
      &lyra_rt_shortreal_sampled_history_construct);
  add("lyra_rt_tuple_sampled_history_construct",
      &lyra_rt_tuple_sampled_history_construct);
  add("lyra_rt_union_sampled_history_construct",
      &lyra_rt_union_sampled_history_construct);
  add("lyra_rt_tagged_union_sampled_history_construct",
      &lyra_rt_tagged_union_sampled_history_construct);
  add("lyra_rt_dynarray_sampled_history_construct",
      &lyra_rt_dynarray_sampled_history_construct);
  add("lyra_rt_unpackedarray_sampled_history_construct",
      &lyra_rt_unpackedarray_sampled_history_construct);
  add("lyra_rt_queue_sampled_history_construct",
      &lyra_rt_queue_sampled_history_construct);
  add("lyra_rt_assocarray_sampled_history_construct",
      &lyra_rt_assocarray_sampled_history_construct);
  add("lyra_rt_managedref_sampled_history_construct",
      &lyra_rt_managedref_sampled_history_construct);
  add("lyra_rt_named_event_construct", &lyra_rt_named_event_construct);
  add("lyra_rt_cancellation_target_construct",
      &lyra_rt_cancellation_target_construct);
  add("lyra_rt_evaluation_attempts_construct",
      &lyra_rt_evaluation_attempts_construct);
  add("lyra_rt_channel_cancellation_construct",
      &lyra_rt_channel_cancellation_construct);
  add("lyra_rt_shared_pointer_construct", &lyra_rt_shared_pointer_construct);
  add("lyra_rt_string_cell_destroy", &lyra_rt_string_cell_destroy);
  add("lyra_rt_real_cell_destroy", &lyra_rt_real_cell_destroy);
  add("lyra_rt_shortreal_cell_destroy", &lyra_rt_shortreal_cell_destroy);
  add("lyra_rt_chandle_cell_destroy", &lyra_rt_chandle_cell_destroy);
  add("lyra_rt_tuple_cell_destroy", &lyra_rt_tuple_cell_destroy);
  add("lyra_rt_union_cell_destroy", &lyra_rt_union_cell_destroy);
  add("lyra_rt_tagged_union_cell_destroy", &lyra_rt_tagged_union_cell_destroy);
  add("lyra_rt_dynarray_cell_destroy", &lyra_rt_dynarray_cell_destroy);
  add("lyra_rt_unpackedarray_cell_destroy",
      &lyra_rt_unpackedarray_cell_destroy);
  add("lyra_rt_queue_cell_destroy", &lyra_rt_queue_cell_destroy);
  add("lyra_rt_assocarray_cell_destroy", &lyra_rt_assocarray_cell_destroy);
  add("lyra_rt_managedref_cell_destroy", &lyra_rt_managedref_cell_destroy);
  add("lyra_rt_string_value_cell_destroy", &lyra_rt_string_value_cell_destroy);
  add("lyra_rt_tuple_value_cell_destroy", &lyra_rt_tuple_value_cell_destroy);
  add("lyra_rt_union_value_cell_destroy", &lyra_rt_union_value_cell_destroy);
  add("lyra_rt_tagged_union_value_cell_destroy",
      &lyra_rt_tagged_union_value_cell_destroy);
  add("lyra_rt_dynarray_value_cell_destroy",
      &lyra_rt_dynarray_value_cell_destroy);
  add("lyra_rt_unpackedarray_value_cell_destroy",
      &lyra_rt_unpackedarray_value_cell_destroy);
  add("lyra_rt_queue_value_cell_destroy", &lyra_rt_queue_value_cell_destroy);
  add("lyra_rt_assocarray_value_cell_destroy",
      &lyra_rt_assocarray_value_cell_destroy);
  add("lyra_rt_managedref_value_cell_destroy",
      &lyra_rt_managedref_value_cell_destroy);
  add("lyra_rt_tuple_net_destroy", &lyra_rt_tuple_net_destroy);
  add("lyra_rt_union_net_destroy", &lyra_rt_union_net_destroy);
  add("lyra_rt_unpackedarray_net_destroy", &lyra_rt_unpackedarray_net_destroy);
  add("lyra_rt_string_sampled_history_destroy",
      &lyra_rt_string_sampled_history_destroy);
  add("lyra_rt_real_sampled_history_destroy",
      &lyra_rt_real_sampled_history_destroy);
  add("lyra_rt_shortreal_sampled_history_destroy",
      &lyra_rt_shortreal_sampled_history_destroy);
  add("lyra_rt_tuple_sampled_history_destroy",
      &lyra_rt_tuple_sampled_history_destroy);
  add("lyra_rt_union_sampled_history_destroy",
      &lyra_rt_union_sampled_history_destroy);
  add("lyra_rt_tagged_union_sampled_history_destroy",
      &lyra_rt_tagged_union_sampled_history_destroy);
  add("lyra_rt_dynarray_sampled_history_destroy",
      &lyra_rt_dynarray_sampled_history_destroy);
  add("lyra_rt_unpackedarray_sampled_history_destroy",
      &lyra_rt_unpackedarray_sampled_history_destroy);
  add("lyra_rt_queue_sampled_history_destroy",
      &lyra_rt_queue_sampled_history_destroy);
  add("lyra_rt_assocarray_sampled_history_destroy",
      &lyra_rt_assocarray_sampled_history_destroy);
  add("lyra_rt_managedref_sampled_history_destroy",
      &lyra_rt_managedref_sampled_history_destroy);
  add("lyra_rt_named_event_destroy", &lyra_rt_named_event_destroy);
  add("lyra_rt_cancellation_target_destroy",
      &lyra_rt_cancellation_target_destroy);
  add("lyra_rt_evaluation_attempts_destroy",
      &lyra_rt_evaluation_attempts_destroy);
}

// The holders over each layout an integral value has: the eight no wider than
// a word, and the two of words.
void BindIntegralLayoutEntries(const auto& add) {
  add("lyra_rt_bit8_cell_construct", &lyra_rt_bit8_cell_construct);
  add("lyra_rt_bit8_cell_destroy", &lyra_rt_bit8_cell_destroy);
  add("lyra_rt_bit8_cell_initialize", &lyra_rt_bit8_cell_initialize);
  add("lyra_rt_bit8_cell_set", &lyra_rt_bit8_cell_set);
  add("lyra_rt_bit8_cell_arm_sampling", &lyra_rt_bit8_cell_arm_sampling);
  add("lyra_rt_bit8_cell_sampled_load", &lyra_rt_bit8_cell_sampled_load);
  add("lyra_rt_bit8_cell_begin_takeover", &lyra_rt_bit8_cell_begin_takeover);
  add("lyra_rt_bit8_cell_drive_takeover", &lyra_rt_bit8_cell_drive_takeover);
  add("lyra_rt_bit8_cell_end_takeover", &lyra_rt_bit8_cell_end_takeover);
  add("lyra_rt_bit8_ref_begin_takeover", &lyra_rt_bit8_ref_begin_takeover);
  add("lyra_rt_bit8_ref_drive_takeover", &lyra_rt_bit8_ref_drive_takeover);
  add("lyra_rt_bit8_ref_end_takeover", &lyra_rt_bit8_ref_end_takeover);
  add("lyra_rt_bit8_cell_refer", &lyra_rt_bit8_cell_refer);
  add("lyra_rt_bit8_cell_open_for_write", &lyra_rt_bit8_cell_open_for_write);
  add("lyra_rt_bit8_shared_cell_make", &lyra_rt_bit8_shared_cell_make);
  add("lyra_rt_bit8_ref_get", &lyra_rt_bit8_ref_get);
  add("lyra_rt_bit8_ref_set", &lyra_rt_bit8_ref_set);
  add("lyra_rt_bit8_ref_arm_sampling", &lyra_rt_bit8_ref_arm_sampling);
  add("lyra_rt_bit8_ref_sampled_load", &lyra_rt_bit8_ref_sampled_load);
  add("lyra_rt_bit8_ref_open_for_write", &lyra_rt_bit8_ref_open_for_write);
  add("lyra_rt_bit8_value_cell_alloc", &lyra_rt_bit8_value_cell_alloc);
  add("lyra_rt_bit8_value_cell_construct", &lyra_rt_bit8_value_cell_construct);
  add("lyra_rt_bit8_sampled_history_construct",
      &lyra_rt_bit8_sampled_history_construct);
  add("lyra_rt_bit8_sampled_history_destroy",
      &lyra_rt_bit8_sampled_history_destroy);
  add("lyra_rt_bit8_sampled_history_install",
      &lyra_rt_bit8_sampled_history_install);
  add("lyra_rt_bit8_sampled_history_push", &lyra_rt_bit8_sampled_history_push);
  add("lyra_rt_bit8_sampled_history_at", &lyra_rt_bit8_sampled_history_at);
  add("lyra_rt_bit8_land", &lyra_rt_bit8_land);
  add("lyra_rt_bit8_report_bits", &lyra_rt_bit8_report_bits);
  add("lyra_rt_bit16_cell_construct", &lyra_rt_bit16_cell_construct);
  add("lyra_rt_bit16_cell_destroy", &lyra_rt_bit16_cell_destroy);
  add("lyra_rt_bit16_cell_initialize", &lyra_rt_bit16_cell_initialize);
  add("lyra_rt_bit16_cell_set", &lyra_rt_bit16_cell_set);
  add("lyra_rt_bit16_cell_arm_sampling", &lyra_rt_bit16_cell_arm_sampling);
  add("lyra_rt_bit16_cell_sampled_load", &lyra_rt_bit16_cell_sampled_load);
  add("lyra_rt_bit16_cell_begin_takeover", &lyra_rt_bit16_cell_begin_takeover);
  add("lyra_rt_bit16_cell_drive_takeover", &lyra_rt_bit16_cell_drive_takeover);
  add("lyra_rt_bit16_cell_end_takeover", &lyra_rt_bit16_cell_end_takeover);
  add("lyra_rt_bit16_ref_begin_takeover", &lyra_rt_bit16_ref_begin_takeover);
  add("lyra_rt_bit16_ref_drive_takeover", &lyra_rt_bit16_ref_drive_takeover);
  add("lyra_rt_bit16_ref_end_takeover", &lyra_rt_bit16_ref_end_takeover);
  add("lyra_rt_bit16_cell_refer", &lyra_rt_bit16_cell_refer);
  add("lyra_rt_bit16_cell_open_for_write", &lyra_rt_bit16_cell_open_for_write);
  add("lyra_rt_bit16_shared_cell_make", &lyra_rt_bit16_shared_cell_make);
  add("lyra_rt_bit16_ref_get", &lyra_rt_bit16_ref_get);
  add("lyra_rt_bit16_ref_set", &lyra_rt_bit16_ref_set);
  add("lyra_rt_bit16_ref_arm_sampling", &lyra_rt_bit16_ref_arm_sampling);
  add("lyra_rt_bit16_ref_sampled_load", &lyra_rt_bit16_ref_sampled_load);
  add("lyra_rt_bit16_ref_open_for_write", &lyra_rt_bit16_ref_open_for_write);
  add("lyra_rt_bit16_value_cell_alloc", &lyra_rt_bit16_value_cell_alloc);
  add("lyra_rt_bit16_value_cell_construct",
      &lyra_rt_bit16_value_cell_construct);
  add("lyra_rt_bit16_sampled_history_construct",
      &lyra_rt_bit16_sampled_history_construct);
  add("lyra_rt_bit16_sampled_history_destroy",
      &lyra_rt_bit16_sampled_history_destroy);
  add("lyra_rt_bit16_sampled_history_install",
      &lyra_rt_bit16_sampled_history_install);
  add("lyra_rt_bit16_sampled_history_push",
      &lyra_rt_bit16_sampled_history_push);
  add("lyra_rt_bit16_sampled_history_at", &lyra_rt_bit16_sampled_history_at);
  add("lyra_rt_bit16_land", &lyra_rt_bit16_land);
  add("lyra_rt_bit16_report_bits", &lyra_rt_bit16_report_bits);
  add("lyra_rt_bit32_cell_construct", &lyra_rt_bit32_cell_construct);
  add("lyra_rt_bit32_cell_destroy", &lyra_rt_bit32_cell_destroy);
  add("lyra_rt_bit32_cell_initialize", &lyra_rt_bit32_cell_initialize);
  add("lyra_rt_bit32_cell_set", &lyra_rt_bit32_cell_set);
  add("lyra_rt_bit32_cell_arm_sampling", &lyra_rt_bit32_cell_arm_sampling);
  add("lyra_rt_bit32_cell_sampled_load", &lyra_rt_bit32_cell_sampled_load);
  add("lyra_rt_bit32_cell_begin_takeover", &lyra_rt_bit32_cell_begin_takeover);
  add("lyra_rt_bit32_cell_drive_takeover", &lyra_rt_bit32_cell_drive_takeover);
  add("lyra_rt_bit32_cell_end_takeover", &lyra_rt_bit32_cell_end_takeover);
  add("lyra_rt_bit32_ref_begin_takeover", &lyra_rt_bit32_ref_begin_takeover);
  add("lyra_rt_bit32_ref_drive_takeover", &lyra_rt_bit32_ref_drive_takeover);
  add("lyra_rt_bit32_ref_end_takeover", &lyra_rt_bit32_ref_end_takeover);
  add("lyra_rt_bit32_cell_refer", &lyra_rt_bit32_cell_refer);
  add("lyra_rt_bit32_cell_open_for_write", &lyra_rt_bit32_cell_open_for_write);
  add("lyra_rt_bit32_shared_cell_make", &lyra_rt_bit32_shared_cell_make);
  add("lyra_rt_bit32_ref_get", &lyra_rt_bit32_ref_get);
  add("lyra_rt_bit32_ref_set", &lyra_rt_bit32_ref_set);
  add("lyra_rt_bit32_ref_arm_sampling", &lyra_rt_bit32_ref_arm_sampling);
  add("lyra_rt_bit32_ref_sampled_load", &lyra_rt_bit32_ref_sampled_load);
  add("lyra_rt_bit32_ref_open_for_write", &lyra_rt_bit32_ref_open_for_write);
  add("lyra_rt_bit32_value_cell_alloc", &lyra_rt_bit32_value_cell_alloc);
  add("lyra_rt_bit32_value_cell_construct",
      &lyra_rt_bit32_value_cell_construct);
  add("lyra_rt_bit32_sampled_history_construct",
      &lyra_rt_bit32_sampled_history_construct);
  add("lyra_rt_bit32_sampled_history_destroy",
      &lyra_rt_bit32_sampled_history_destroy);
  add("lyra_rt_bit32_sampled_history_install",
      &lyra_rt_bit32_sampled_history_install);
  add("lyra_rt_bit32_sampled_history_push",
      &lyra_rt_bit32_sampled_history_push);
  add("lyra_rt_bit32_sampled_history_at", &lyra_rt_bit32_sampled_history_at);
  add("lyra_rt_bit32_land", &lyra_rt_bit32_land);
  add("lyra_rt_bit32_report_bits", &lyra_rt_bit32_report_bits);
  add("lyra_rt_bit64_cell_construct", &lyra_rt_bit64_cell_construct);
  add("lyra_rt_bit64_cell_destroy", &lyra_rt_bit64_cell_destroy);
  add("lyra_rt_bit64_cell_initialize", &lyra_rt_bit64_cell_initialize);
  add("lyra_rt_bit64_cell_set", &lyra_rt_bit64_cell_set);
  add("lyra_rt_bit64_cell_arm_sampling", &lyra_rt_bit64_cell_arm_sampling);
  add("lyra_rt_bit64_cell_sampled_load", &lyra_rt_bit64_cell_sampled_load);
  add("lyra_rt_bit64_cell_begin_takeover", &lyra_rt_bit64_cell_begin_takeover);
  add("lyra_rt_bit64_cell_drive_takeover", &lyra_rt_bit64_cell_drive_takeover);
  add("lyra_rt_bit64_cell_end_takeover", &lyra_rt_bit64_cell_end_takeover);
  add("lyra_rt_bit64_ref_begin_takeover", &lyra_rt_bit64_ref_begin_takeover);
  add("lyra_rt_bit64_ref_drive_takeover", &lyra_rt_bit64_ref_drive_takeover);
  add("lyra_rt_bit64_ref_end_takeover", &lyra_rt_bit64_ref_end_takeover);
  add("lyra_rt_bit64_cell_refer", &lyra_rt_bit64_cell_refer);
  add("lyra_rt_bit64_cell_open_for_write", &lyra_rt_bit64_cell_open_for_write);
  add("lyra_rt_bit64_shared_cell_make", &lyra_rt_bit64_shared_cell_make);
  add("lyra_rt_bit64_ref_get", &lyra_rt_bit64_ref_get);
  add("lyra_rt_bit64_ref_set", &lyra_rt_bit64_ref_set);
  add("lyra_rt_bit64_ref_arm_sampling", &lyra_rt_bit64_ref_arm_sampling);
  add("lyra_rt_bit64_ref_sampled_load", &lyra_rt_bit64_ref_sampled_load);
  add("lyra_rt_bit64_ref_open_for_write", &lyra_rt_bit64_ref_open_for_write);
  add("lyra_rt_bit64_value_cell_alloc", &lyra_rt_bit64_value_cell_alloc);
  add("lyra_rt_bit64_value_cell_construct",
      &lyra_rt_bit64_value_cell_construct);
  add("lyra_rt_bit64_sampled_history_construct",
      &lyra_rt_bit64_sampled_history_construct);
  add("lyra_rt_bit64_sampled_history_destroy",
      &lyra_rt_bit64_sampled_history_destroy);
  add("lyra_rt_bit64_sampled_history_install",
      &lyra_rt_bit64_sampled_history_install);
  add("lyra_rt_bit64_sampled_history_push",
      &lyra_rt_bit64_sampled_history_push);
  add("lyra_rt_bit64_sampled_history_at", &lyra_rt_bit64_sampled_history_at);
  add("lyra_rt_bit64_land", &lyra_rt_bit64_land);
  add("lyra_rt_bit64_report_bits", &lyra_rt_bit64_report_bits);
  add("lyra_rt_logic8_cell_construct", &lyra_rt_logic8_cell_construct);
  add("lyra_rt_logic8_cell_destroy", &lyra_rt_logic8_cell_destroy);
  add("lyra_rt_logic8_cell_initialize", &lyra_rt_logic8_cell_initialize);
  add("lyra_rt_logic8_cell_set", &lyra_rt_logic8_cell_set);
  add("lyra_rt_logic8_cell_arm_sampling", &lyra_rt_logic8_cell_arm_sampling);
  add("lyra_rt_logic8_cell_sampled_load", &lyra_rt_logic8_cell_sampled_load);
  add("lyra_rt_logic8_cell_begin_takeover",
      &lyra_rt_logic8_cell_begin_takeover);
  add("lyra_rt_logic8_cell_drive_takeover",
      &lyra_rt_logic8_cell_drive_takeover);
  add("lyra_rt_logic8_cell_end_takeover", &lyra_rt_logic8_cell_end_takeover);
  add("lyra_rt_logic8_ref_begin_takeover", &lyra_rt_logic8_ref_begin_takeover);
  add("lyra_rt_logic8_ref_drive_takeover", &lyra_rt_logic8_ref_drive_takeover);
  add("lyra_rt_logic8_ref_end_takeover", &lyra_rt_logic8_ref_end_takeover);
  add("lyra_rt_logic8_cell_refer", &lyra_rt_logic8_cell_refer);
  add("lyra_rt_logic8_cell_open_for_write",
      &lyra_rt_logic8_cell_open_for_write);
  add("lyra_rt_logic8_shared_cell_make", &lyra_rt_logic8_shared_cell_make);
  add("lyra_rt_logic8_ref_get", &lyra_rt_logic8_ref_get);
  add("lyra_rt_logic8_ref_set", &lyra_rt_logic8_ref_set);
  add("lyra_rt_logic8_ref_arm_sampling", &lyra_rt_logic8_ref_arm_sampling);
  add("lyra_rt_logic8_ref_sampled_load", &lyra_rt_logic8_ref_sampled_load);
  add("lyra_rt_logic8_ref_open_for_write", &lyra_rt_logic8_ref_open_for_write);
  add("lyra_rt_logic8_value_cell_alloc", &lyra_rt_logic8_value_cell_alloc);
  add("lyra_rt_logic8_value_cell_construct",
      &lyra_rt_logic8_value_cell_construct);
  add("lyra_rt_logic8_sampled_history_construct",
      &lyra_rt_logic8_sampled_history_construct);
  add("lyra_rt_logic8_sampled_history_destroy",
      &lyra_rt_logic8_sampled_history_destroy);
  add("lyra_rt_logic8_sampled_history_install",
      &lyra_rt_logic8_sampled_history_install);
  add("lyra_rt_logic8_sampled_history_push",
      &lyra_rt_logic8_sampled_history_push);
  add("lyra_rt_logic8_sampled_history_at", &lyra_rt_logic8_sampled_history_at);
  add("lyra_rt_logic8_land", &lyra_rt_logic8_land);
  add("lyra_rt_logic8_report_bits", &lyra_rt_logic8_report_bits);
  add("lyra_rt_logic8_net_construct", &lyra_rt_logic8_net_construct);
  add("lyra_rt_logic8_net_destroy", &lyra_rt_logic8_net_destroy);
  add("lyra_rt_logic8_net_initialize_tri_state",
      &lyra_rt_logic8_net_initialize_tri_state);
  add("lyra_rt_logic8_net_initialize_wired_and",
      &lyra_rt_logic8_net_initialize_wired_and);
  add("lyra_rt_logic8_net_initialize_wired_or",
      &lyra_rt_logic8_net_initialize_wired_or);
  add("lyra_rt_logic8_net_initialize_retaining",
      &lyra_rt_logic8_net_initialize_retaining);
  add("lyra_rt_logic8_net_begin_takeover", &lyra_rt_logic8_net_begin_takeover);
  add("lyra_rt_logic8_net_drive_takeover", &lyra_rt_logic8_net_drive_takeover);
  add("lyra_rt_logic8_net_end_takeover", &lyra_rt_logic8_net_end_takeover);
  add("lyra_rt_logic8_attach_driver", &lyra_rt_logic8_attach_driver);
  add("lyra_rt_logic8_net_join", &lyra_rt_logic8_net_join);
  add("lyra_rt_logic8_driver_get", &lyra_rt_logic8_driver_get);
  add("lyra_rt_logic8_driver_set", &lyra_rt_logic8_driver_set);
  add("lyra_rt_logic8_driver_open_for_write",
      &lyra_rt_logic8_driver_open_for_write);
  add("lyra_rt_logic16_cell_construct", &lyra_rt_logic16_cell_construct);
  add("lyra_rt_logic16_cell_destroy", &lyra_rt_logic16_cell_destroy);
  add("lyra_rt_logic16_cell_initialize", &lyra_rt_logic16_cell_initialize);
  add("lyra_rt_logic16_cell_set", &lyra_rt_logic16_cell_set);
  add("lyra_rt_logic16_cell_arm_sampling", &lyra_rt_logic16_cell_arm_sampling);
  add("lyra_rt_logic16_cell_sampled_load", &lyra_rt_logic16_cell_sampled_load);
  add("lyra_rt_logic16_cell_begin_takeover",
      &lyra_rt_logic16_cell_begin_takeover);
  add("lyra_rt_logic16_cell_drive_takeover",
      &lyra_rt_logic16_cell_drive_takeover);
  add("lyra_rt_logic16_cell_end_takeover", &lyra_rt_logic16_cell_end_takeover);
  add("lyra_rt_logic16_ref_begin_takeover",
      &lyra_rt_logic16_ref_begin_takeover);
  add("lyra_rt_logic16_ref_drive_takeover",
      &lyra_rt_logic16_ref_drive_takeover);
  add("lyra_rt_logic16_ref_end_takeover", &lyra_rt_logic16_ref_end_takeover);
  add("lyra_rt_logic16_cell_refer", &lyra_rt_logic16_cell_refer);
  add("lyra_rt_logic16_cell_open_for_write",
      &lyra_rt_logic16_cell_open_for_write);
  add("lyra_rt_logic16_shared_cell_make", &lyra_rt_logic16_shared_cell_make);
  add("lyra_rt_logic16_ref_get", &lyra_rt_logic16_ref_get);
  add("lyra_rt_logic16_ref_set", &lyra_rt_logic16_ref_set);
  add("lyra_rt_logic16_ref_arm_sampling", &lyra_rt_logic16_ref_arm_sampling);
  add("lyra_rt_logic16_ref_sampled_load", &lyra_rt_logic16_ref_sampled_load);
  add("lyra_rt_logic16_ref_open_for_write",
      &lyra_rt_logic16_ref_open_for_write);
  add("lyra_rt_logic16_value_cell_alloc", &lyra_rt_logic16_value_cell_alloc);
  add("lyra_rt_logic16_value_cell_construct",
      &lyra_rt_logic16_value_cell_construct);
  add("lyra_rt_logic16_sampled_history_construct",
      &lyra_rt_logic16_sampled_history_construct);
  add("lyra_rt_logic16_sampled_history_destroy",
      &lyra_rt_logic16_sampled_history_destroy);
  add("lyra_rt_logic16_sampled_history_install",
      &lyra_rt_logic16_sampled_history_install);
  add("lyra_rt_logic16_sampled_history_push",
      &lyra_rt_logic16_sampled_history_push);
  add("lyra_rt_logic16_sampled_history_at",
      &lyra_rt_logic16_sampled_history_at);
  add("lyra_rt_logic16_land", &lyra_rt_logic16_land);
  add("lyra_rt_logic16_report_bits", &lyra_rt_logic16_report_bits);
  add("lyra_rt_logic16_net_construct", &lyra_rt_logic16_net_construct);
  add("lyra_rt_logic16_net_destroy", &lyra_rt_logic16_net_destroy);
  add("lyra_rt_logic16_net_initialize_tri_state",
      &lyra_rt_logic16_net_initialize_tri_state);
  add("lyra_rt_logic16_net_initialize_wired_and",
      &lyra_rt_logic16_net_initialize_wired_and);
  add("lyra_rt_logic16_net_initialize_wired_or",
      &lyra_rt_logic16_net_initialize_wired_or);
  add("lyra_rt_logic16_net_initialize_retaining",
      &lyra_rt_logic16_net_initialize_retaining);
  add("lyra_rt_logic16_net_begin_takeover",
      &lyra_rt_logic16_net_begin_takeover);
  add("lyra_rt_logic16_net_drive_takeover",
      &lyra_rt_logic16_net_drive_takeover);
  add("lyra_rt_logic16_net_end_takeover", &lyra_rt_logic16_net_end_takeover);
  add("lyra_rt_logic16_attach_driver", &lyra_rt_logic16_attach_driver);
  add("lyra_rt_logic16_net_join", &lyra_rt_logic16_net_join);
  add("lyra_rt_logic16_driver_get", &lyra_rt_logic16_driver_get);
  add("lyra_rt_logic16_driver_set", &lyra_rt_logic16_driver_set);
  add("lyra_rt_logic16_driver_open_for_write",
      &lyra_rt_logic16_driver_open_for_write);
  add("lyra_rt_logic32_cell_construct", &lyra_rt_logic32_cell_construct);
  add("lyra_rt_logic32_cell_destroy", &lyra_rt_logic32_cell_destroy);
  add("lyra_rt_logic32_cell_initialize", &lyra_rt_logic32_cell_initialize);
  add("lyra_rt_logic32_cell_set", &lyra_rt_logic32_cell_set);
  add("lyra_rt_logic32_cell_arm_sampling", &lyra_rt_logic32_cell_arm_sampling);
  add("lyra_rt_logic32_cell_sampled_load", &lyra_rt_logic32_cell_sampled_load);
  add("lyra_rt_logic32_cell_begin_takeover",
      &lyra_rt_logic32_cell_begin_takeover);
  add("lyra_rt_logic32_cell_drive_takeover",
      &lyra_rt_logic32_cell_drive_takeover);
  add("lyra_rt_logic32_cell_end_takeover", &lyra_rt_logic32_cell_end_takeover);
  add("lyra_rt_logic32_ref_begin_takeover",
      &lyra_rt_logic32_ref_begin_takeover);
  add("lyra_rt_logic32_ref_drive_takeover",
      &lyra_rt_logic32_ref_drive_takeover);
  add("lyra_rt_logic32_ref_end_takeover", &lyra_rt_logic32_ref_end_takeover);
  add("lyra_rt_logic32_cell_refer", &lyra_rt_logic32_cell_refer);
  add("lyra_rt_logic32_cell_open_for_write",
      &lyra_rt_logic32_cell_open_for_write);
  add("lyra_rt_logic32_shared_cell_make", &lyra_rt_logic32_shared_cell_make);
  add("lyra_rt_logic32_ref_get", &lyra_rt_logic32_ref_get);
  add("lyra_rt_logic32_ref_set", &lyra_rt_logic32_ref_set);
  add("lyra_rt_logic32_ref_arm_sampling", &lyra_rt_logic32_ref_arm_sampling);
  add("lyra_rt_logic32_ref_sampled_load", &lyra_rt_logic32_ref_sampled_load);
  add("lyra_rt_logic32_ref_open_for_write",
      &lyra_rt_logic32_ref_open_for_write);
  add("lyra_rt_logic32_value_cell_alloc", &lyra_rt_logic32_value_cell_alloc);
  add("lyra_rt_logic32_value_cell_construct",
      &lyra_rt_logic32_value_cell_construct);
  add("lyra_rt_logic32_sampled_history_construct",
      &lyra_rt_logic32_sampled_history_construct);
  add("lyra_rt_logic32_sampled_history_destroy",
      &lyra_rt_logic32_sampled_history_destroy);
  add("lyra_rt_logic32_sampled_history_install",
      &lyra_rt_logic32_sampled_history_install);
  add("lyra_rt_logic32_sampled_history_push",
      &lyra_rt_logic32_sampled_history_push);
  add("lyra_rt_logic32_sampled_history_at",
      &lyra_rt_logic32_sampled_history_at);
  add("lyra_rt_logic32_land", &lyra_rt_logic32_land);
  add("lyra_rt_logic32_report_bits", &lyra_rt_logic32_report_bits);
  add("lyra_rt_logic32_net_construct", &lyra_rt_logic32_net_construct);
  add("lyra_rt_logic32_net_destroy", &lyra_rt_logic32_net_destroy);
  add("lyra_rt_logic32_net_initialize_tri_state",
      &lyra_rt_logic32_net_initialize_tri_state);
  add("lyra_rt_logic32_net_initialize_wired_and",
      &lyra_rt_logic32_net_initialize_wired_and);
  add("lyra_rt_logic32_net_initialize_wired_or",
      &lyra_rt_logic32_net_initialize_wired_or);
  add("lyra_rt_logic32_net_initialize_retaining",
      &lyra_rt_logic32_net_initialize_retaining);
  add("lyra_rt_logic32_net_begin_takeover",
      &lyra_rt_logic32_net_begin_takeover);
  add("lyra_rt_logic32_net_drive_takeover",
      &lyra_rt_logic32_net_drive_takeover);
  add("lyra_rt_logic32_net_end_takeover", &lyra_rt_logic32_net_end_takeover);
  add("lyra_rt_logic32_attach_driver", &lyra_rt_logic32_attach_driver);
  add("lyra_rt_logic32_net_join", &lyra_rt_logic32_net_join);
  add("lyra_rt_logic32_driver_get", &lyra_rt_logic32_driver_get);
  add("lyra_rt_logic32_driver_set", &lyra_rt_logic32_driver_set);
  add("lyra_rt_logic32_driver_open_for_write",
      &lyra_rt_logic32_driver_open_for_write);
  add("lyra_rt_logic64_cell_construct", &lyra_rt_logic64_cell_construct);
  add("lyra_rt_logic64_cell_destroy", &lyra_rt_logic64_cell_destroy);
  add("lyra_rt_logic64_cell_initialize", &lyra_rt_logic64_cell_initialize);
  add("lyra_rt_logic64_cell_set", &lyra_rt_logic64_cell_set);
  add("lyra_rt_logic64_cell_arm_sampling", &lyra_rt_logic64_cell_arm_sampling);
  add("lyra_rt_logic64_cell_sampled_load", &lyra_rt_logic64_cell_sampled_load);
  add("lyra_rt_logic64_cell_begin_takeover",
      &lyra_rt_logic64_cell_begin_takeover);
  add("lyra_rt_logic64_cell_drive_takeover",
      &lyra_rt_logic64_cell_drive_takeover);
  add("lyra_rt_logic64_cell_end_takeover", &lyra_rt_logic64_cell_end_takeover);
  add("lyra_rt_logic64_ref_begin_takeover",
      &lyra_rt_logic64_ref_begin_takeover);
  add("lyra_rt_logic64_ref_drive_takeover",
      &lyra_rt_logic64_ref_drive_takeover);
  add("lyra_rt_logic64_ref_end_takeover", &lyra_rt_logic64_ref_end_takeover);
  add("lyra_rt_logic64_cell_refer", &lyra_rt_logic64_cell_refer);
  add("lyra_rt_logic64_cell_open_for_write",
      &lyra_rt_logic64_cell_open_for_write);
  add("lyra_rt_logic64_shared_cell_make", &lyra_rt_logic64_shared_cell_make);
  add("lyra_rt_logic64_ref_get", &lyra_rt_logic64_ref_get);
  add("lyra_rt_logic64_ref_set", &lyra_rt_logic64_ref_set);
  add("lyra_rt_logic64_ref_arm_sampling", &lyra_rt_logic64_ref_arm_sampling);
  add("lyra_rt_logic64_ref_sampled_load", &lyra_rt_logic64_ref_sampled_load);
  add("lyra_rt_logic64_ref_open_for_write",
      &lyra_rt_logic64_ref_open_for_write);
  add("lyra_rt_logic64_value_cell_alloc", &lyra_rt_logic64_value_cell_alloc);
  add("lyra_rt_logic64_value_cell_construct",
      &lyra_rt_logic64_value_cell_construct);
  add("lyra_rt_logic64_sampled_history_construct",
      &lyra_rt_logic64_sampled_history_construct);
  add("lyra_rt_logic64_sampled_history_destroy",
      &lyra_rt_logic64_sampled_history_destroy);
  add("lyra_rt_logic64_sampled_history_install",
      &lyra_rt_logic64_sampled_history_install);
  add("lyra_rt_logic64_sampled_history_push",
      &lyra_rt_logic64_sampled_history_push);
  add("lyra_rt_logic64_sampled_history_at",
      &lyra_rt_logic64_sampled_history_at);
  add("lyra_rt_logic64_land", &lyra_rt_logic64_land);
  add("lyra_rt_logic64_report_bits", &lyra_rt_logic64_report_bits);
  add("lyra_rt_logic64_net_construct", &lyra_rt_logic64_net_construct);
  add("lyra_rt_logic64_net_destroy", &lyra_rt_logic64_net_destroy);
  add("lyra_rt_logic64_net_initialize_tri_state",
      &lyra_rt_logic64_net_initialize_tri_state);
  add("lyra_rt_logic64_net_initialize_wired_and",
      &lyra_rt_logic64_net_initialize_wired_and);
  add("lyra_rt_logic64_net_initialize_wired_or",
      &lyra_rt_logic64_net_initialize_wired_or);
  add("lyra_rt_logic64_net_initialize_retaining",
      &lyra_rt_logic64_net_initialize_retaining);
  add("lyra_rt_logic64_net_begin_takeover",
      &lyra_rt_logic64_net_begin_takeover);
  add("lyra_rt_logic64_net_drive_takeover",
      &lyra_rt_logic64_net_drive_takeover);
  add("lyra_rt_logic64_net_end_takeover", &lyra_rt_logic64_net_end_takeover);
  add("lyra_rt_logic64_attach_driver", &lyra_rt_logic64_attach_driver);
  add("lyra_rt_logic64_net_join", &lyra_rt_logic64_net_join);
  add("lyra_rt_logic64_driver_get", &lyra_rt_logic64_driver_get);
  add("lyra_rt_logic64_driver_set", &lyra_rt_logic64_driver_set);
  add("lyra_rt_logic64_driver_open_for_write",
      &lyra_rt_logic64_driver_open_for_write);

  add("lyra_rt_bit_wide_cell_construct", &lyra_rt_bit_wide_cell_construct);
  add("lyra_rt_bit_wide_cell_destroy", &lyra_rt_bit_wide_cell_destroy);
  add("lyra_rt_bit_wide_cell_get", &lyra_rt_bit_wide_cell_get);
  add("lyra_rt_bit_wide_cell_initialize", &lyra_rt_bit_wide_cell_initialize);
  add("lyra_rt_bit_wide_cell_set", &lyra_rt_bit_wide_cell_set);
  add("lyra_rt_bit_wide_cell_arm_sampling",
      &lyra_rt_bit_wide_cell_arm_sampling);
  add("lyra_rt_bit_wide_cell_sampled_load",
      &lyra_rt_bit_wide_cell_sampled_load);
  add("lyra_rt_bit_wide_cell_begin_takeover",
      &lyra_rt_bit_wide_cell_begin_takeover);
  add("lyra_rt_bit_wide_cell_drive_takeover",
      &lyra_rt_bit_wide_cell_drive_takeover);
  add("lyra_rt_bit_wide_cell_end_takeover",
      &lyra_rt_bit_wide_cell_end_takeover);
  add("lyra_rt_bit_wide_ref_begin_takeover",
      &lyra_rt_bit_wide_ref_begin_takeover);
  add("lyra_rt_bit_wide_ref_drive_takeover",
      &lyra_rt_bit_wide_ref_drive_takeover);
  add("lyra_rt_bit_wide_ref_end_takeover", &lyra_rt_bit_wide_ref_end_takeover);
  add("lyra_rt_bit_wide_cell_refer", &lyra_rt_bit_wide_cell_refer);
  add("lyra_rt_bit_wide_cell_open_for_write",
      &lyra_rt_bit_wide_cell_open_for_write);
  add("lyra_rt_bit_wide_shared_cell_make", &lyra_rt_bit_wide_shared_cell_make);
  add("lyra_rt_bit_wide_ref_get", &lyra_rt_bit_wide_ref_get);
  add("lyra_rt_bit_wide_ref_set", &lyra_rt_bit_wide_ref_set);
  add("lyra_rt_bit_wide_ref_arm_sampling", &lyra_rt_bit_wide_ref_arm_sampling);
  add("lyra_rt_bit_wide_ref_sampled_load", &lyra_rt_bit_wide_ref_sampled_load);
  add("lyra_rt_bit_wide_ref_open_for_write",
      &lyra_rt_bit_wide_ref_open_for_write);
  add("lyra_rt_bit_wide_value_cell_alloc", &lyra_rt_bit_wide_value_cell_alloc);
  add("lyra_rt_bit_wide_value_cell_construct",
      &lyra_rt_bit_wide_value_cell_construct);
  add("lyra_rt_bit_wide_value_cell_destroy",
      &lyra_rt_bit_wide_value_cell_destroy);
  add("lyra_rt_bit_wide_value_cell_store", &lyra_rt_bit_wide_value_cell_store);
  add("lyra_rt_bit_wide_value_cell_load", &lyra_rt_bit_wide_value_cell_load);
  add("lyra_rt_bit_wide_sampled_history_construct",
      &lyra_rt_bit_wide_sampled_history_construct);
  add("lyra_rt_bit_wide_sampled_history_destroy",
      &lyra_rt_bit_wide_sampled_history_destroy);
  add("lyra_rt_bit_wide_sampled_history_install",
      &lyra_rt_bit_wide_sampled_history_install);
  add("lyra_rt_bit_wide_sampled_history_push",
      &lyra_rt_bit_wide_sampled_history_push);
  add("lyra_rt_bit_wide_sampled_history_at",
      &lyra_rt_bit_wide_sampled_history_at);
  add("lyra_rt_bit_wide_land", &lyra_rt_bit_wide_land);
  add("lyra_rt_bit_wide_report_bits", &lyra_rt_bit_wide_report_bits);

  add("lyra_rt_logic_wide_cell_construct", &lyra_rt_logic_wide_cell_construct);
  add("lyra_rt_logic_wide_cell_destroy", &lyra_rt_logic_wide_cell_destroy);
  add("lyra_rt_logic_wide_cell_get", &lyra_rt_logic_wide_cell_get);
  add("lyra_rt_logic_wide_cell_initialize",
      &lyra_rt_logic_wide_cell_initialize);
  add("lyra_rt_logic_wide_cell_set", &lyra_rt_logic_wide_cell_set);
  add("lyra_rt_logic_wide_cell_arm_sampling",
      &lyra_rt_logic_wide_cell_arm_sampling);
  add("lyra_rt_logic_wide_cell_sampled_load",
      &lyra_rt_logic_wide_cell_sampled_load);
  add("lyra_rt_logic_wide_cell_begin_takeover",
      &lyra_rt_logic_wide_cell_begin_takeover);
  add("lyra_rt_logic_wide_cell_drive_takeover",
      &lyra_rt_logic_wide_cell_drive_takeover);
  add("lyra_rt_logic_wide_cell_end_takeover",
      &lyra_rt_logic_wide_cell_end_takeover);
  add("lyra_rt_logic_wide_ref_begin_takeover",
      &lyra_rt_logic_wide_ref_begin_takeover);
  add("lyra_rt_logic_wide_ref_drive_takeover",
      &lyra_rt_logic_wide_ref_drive_takeover);
  add("lyra_rt_logic_wide_ref_end_takeover",
      &lyra_rt_logic_wide_ref_end_takeover);
  add("lyra_rt_logic_wide_cell_refer", &lyra_rt_logic_wide_cell_refer);
  add("lyra_rt_logic_wide_cell_open_for_write",
      &lyra_rt_logic_wide_cell_open_for_write);
  add("lyra_rt_logic_wide_shared_cell_make",
      &lyra_rt_logic_wide_shared_cell_make);
  add("lyra_rt_logic_wide_ref_get", &lyra_rt_logic_wide_ref_get);
  add("lyra_rt_logic_wide_ref_set", &lyra_rt_logic_wide_ref_set);
  add("lyra_rt_logic_wide_ref_arm_sampling",
      &lyra_rt_logic_wide_ref_arm_sampling);
  add("lyra_rt_logic_wide_ref_sampled_load",
      &lyra_rt_logic_wide_ref_sampled_load);
  add("lyra_rt_logic_wide_ref_open_for_write",
      &lyra_rt_logic_wide_ref_open_for_write);
  add("lyra_rt_logic_wide_value_cell_alloc",
      &lyra_rt_logic_wide_value_cell_alloc);
  add("lyra_rt_logic_wide_value_cell_construct",
      &lyra_rt_logic_wide_value_cell_construct);
  add("lyra_rt_logic_wide_value_cell_destroy",
      &lyra_rt_logic_wide_value_cell_destroy);
  add("lyra_rt_logic_wide_value_cell_store",
      &lyra_rt_logic_wide_value_cell_store);
  add("lyra_rt_logic_wide_value_cell_load",
      &lyra_rt_logic_wide_value_cell_load);
  add("lyra_rt_logic_wide_sampled_history_construct",
      &lyra_rt_logic_wide_sampled_history_construct);
  add("lyra_rt_logic_wide_sampled_history_destroy",
      &lyra_rt_logic_wide_sampled_history_destroy);
  add("lyra_rt_logic_wide_sampled_history_install",
      &lyra_rt_logic_wide_sampled_history_install);
  add("lyra_rt_logic_wide_sampled_history_push",
      &lyra_rt_logic_wide_sampled_history_push);
  add("lyra_rt_logic_wide_sampled_history_at",
      &lyra_rt_logic_wide_sampled_history_at);
  add("lyra_rt_logic_wide_land", &lyra_rt_logic_wide_land);
  add("lyra_rt_logic_wide_report_bits", &lyra_rt_logic_wide_report_bits);
  add("lyra_rt_logic_wide_net_construct", &lyra_rt_logic_wide_net_construct);
  add("lyra_rt_logic_wide_net_destroy", &lyra_rt_logic_wide_net_destroy);
  add("lyra_rt_logic_wide_net_get", &lyra_rt_logic_wide_net_get);
  add("lyra_rt_logic_wide_net_initialize_tri_state",
      &lyra_rt_logic_wide_net_initialize_tri_state);
  add("lyra_rt_logic_wide_net_initialize_wired_and",
      &lyra_rt_logic_wide_net_initialize_wired_and);
  add("lyra_rt_logic_wide_net_initialize_wired_or",
      &lyra_rt_logic_wide_net_initialize_wired_or);
  add("lyra_rt_logic_wide_net_initialize_retaining",
      &lyra_rt_logic_wide_net_initialize_retaining);
  add("lyra_rt_logic_wide_net_begin_takeover",
      &lyra_rt_logic_wide_net_begin_takeover);
  add("lyra_rt_logic_wide_net_drive_takeover",
      &lyra_rt_logic_wide_net_drive_takeover);
  add("lyra_rt_logic_wide_net_end_takeover",
      &lyra_rt_logic_wide_net_end_takeover);
  add("lyra_rt_logic_wide_attach_driver", &lyra_rt_logic_wide_attach_driver);
  add("lyra_rt_logic_wide_net_join", &lyra_rt_logic_wide_net_join);
  add("lyra_rt_logic_wide_driver_get", &lyra_rt_logic_wide_driver_get);
  add("lyra_rt_logic_wide_driver_set", &lyra_rt_logic_wide_driver_set);
  add("lyra_rt_logic_wide_driver_open_for_write",
      &lyra_rt_logic_wide_driver_open_for_write);
}

// What the runtime library publishes, each entry at the shape its own
// definition states. The entries are also the answer to what this backend can
// carry out: an entry's name composes a value domain with an operation, and the
// pairs the library implements are a subset of the pairs that compose.
//
// Each shape is read off the function the library defines, which is linked
// into this compiler from the same sources the shipped library is built from,
// so no shape here can differ from the one a program links. Which entries are
// listed, and under what name, is written out by hand.
auto PublishedEntries() -> const std::map<std::string, AbiSignature>& {
  static const std::map<std::string, AbiSignature> published = [] {
    std::map<std::string, AbiSignature> listed;
    auto add = [&](std::string_view name, auto* fn) {
      listed.emplace(
          name, AbiSignatureOf<std::remove_pointer_t<decltype(fn)>>::Get());
    };
    BindEngineEntries(add);
    BindValueEntries(add);
    BindWriteEntries(add);
    BindMemberOperationEntries(add);
    BindStorageEntries(add);
    BindIntegralLayoutEntries(add);
    return listed;
  }();
  return published;
}

// The runtime entries a generated module calls that the library does not
// publish, listed in the order the module names them and empty when it names
// none. Everything else a module leaves undefined is resolved elsewhere --
// another unit's generated symbol, a foreign function, the host's allocator --
// so the runtime entry family is the only one answerable here.
auto UnpublishedEntries(
    const llvm::Module& module,
    const std::map<std::string, AbiSignature>& published) -> std::string {
  std::string unpublished;
  for (const llvm::Function& fn : module.functions()) {
    if (!fn.isDeclaration()) {
      continue;
    }
    const std::string name = fn.getName().str();
    if (!name.starts_with(backend::llvm_backend::kRuntimeSymbolPrefix)) {
      continue;
    }
    if (published.contains(name)) {
      continue;
    }
    if (!unpublished.empty()) {
      unpublished += ", ";
    }
    unpublished += name;
  }
  return unpublished;
}

// The entries this module calls at a shape other than the one their definitions
// have. The module states a shape by how it declares the call, and it derives
// that from the types at the call site; the definition states one of its own.
// Nothing else compares them -- an untyped handle carries no type to disagree
// about -- so a mismatch runs, and reads whatever the machine passed in the
// register the other side used for something else.
auto MismatchedEntries(
    const llvm::Module& module,
    const std::map<std::string, AbiSignature>& published) -> std::string {
  std::string mismatched;
  const auto report = [&](std::string_view name, std::string_view what,
                          AbiKind called, AbiKind defined) {
    if (!mismatched.empty()) {
      mismatched += "; ";
    }
    mismatched += std::format(
        "{} is called with {} {} where its definition has {}", name, what,
        AbiKindName(called), AbiKindName(defined));
  };
  for (const llvm::Function& fn : module.functions()) {
    if (!fn.isDeclaration()) {
      continue;
    }
    const std::string name = fn.getName().str();
    const auto entry = published.find(name);
    if (entry == published.end()) {
      continue;
    }
    const AbiSignature& defined = entry->second;
    const llvm::FunctionType& called = *fn.getFunctionType();
    const AbiKind result = AbiKindOfLlvm(called.getReturnType());
    if (result != defined.result) {
      report(name, "a result of", result, defined.result);
    }
    if (called.getNumParams() != defined.operands.size()) {
      if (!mismatched.empty()) {
        mismatched += "; ";
      }
      mismatched += std::format(
          "{} is called with {} operands where its definition takes {}", name,
          called.getNumParams(), defined.operands.size());
      continue;
    }
    for (unsigned i = 0; i < called.getNumParams(); ++i) {
      const AbiKind operand = AbiKindOfLlvm(called.getParamType(i));
      if (operand != defined.operands[i]) {
        report(
            name, std::format("operand {} of", i), operand,
            defined.operands[i]);
      }
    }
  }
  return mismatched;
}

auto CheckAgainstRuntime(const backend::llvm_backend::EmittedModule& module)
    -> diag::Result<void> {
  // A module may name an entry the library does not publish, because the
  // naming composes a domain with an operation and not every pair is
  // implemented. Said here it names the entry and refuses the design; left to
  // the link the same absence arrives as a symbol nothing defines, which reads
  // as a compiler bug rather than as an operation nobody wrote.
  const std::string unpublished =
      UnpublishedEntries(module.Module(), PublishedEntries());
  if (!unpublished.empty()) {
    return diag::Fail(
        diag::DiagCode::kUnsupportedExpressionForm,
        std::format(
            "llvm codegen: the runtime library publishes no entry named {}",
            unpublished));
  }
  // Two sides of the ABI disagreeing is this compiler's own defect, and it is
  // caught here because it is the one place both are visible. Left to run, a
  // call reads whatever the machine left in the register the other side used
  // for something else -- a plausible value, not a failure.
  const std::string mismatched =
      MismatchedEntries(module.Module(), PublishedEntries());
  if (!mismatched.empty()) {
    throw InternalError(
        std::format(
            "runtime abi: the generated module and the runtime "
            "library disagree on an entry's shape: {}",
            mismatched));
  }
  return {};
}

// A unit's module, and the form it was emitted from, which the design root's
// entry is emitted from as well.
struct UnitModule {
  lir::CompilationUnit lowered;
  backend::llvm_backend::EmittedModule module;
};

auto EmitUnitModule(const mir::CompilationUnit& unit)
    -> diag::Result<UnitModule> {
  auto lowered = compiler::LowerUnitToExecutable(unit);
  if (!lowered) {
    return std::unexpected(std::move(lowered.error()));
  }
  auto emitted = [&] {
    const profiling::TimeTraceScope span(
        "emit LLVM IR", [&] { return unit.name; });
    return backend::llvm_backend::EmitModule(*lowered);
  }();
  if (!emitted) {
    return std::unexpected(std::move(emitted.error()));
  }
  if (auto checked = CheckAgainstRuntime(*emitted); !checked) {
    return std::unexpected(std::move(checked.error()));
  }
  return UnitModule{
      .lowered = *std::move(lowered), .module = *std::move(emitted)};
}

// The code generator's reading of how hard a build was asked to work: the same
// two levels the host C++ compiler is handed as `-O0` and `-O2`, so a design
// is optimized alike whichever backend compiles it.
auto PipelineLevel(driver::Optimization optimization)
    -> llvm::OptimizationLevel {
  switch (optimization) {
    case driver::Optimization::kIterate:
      return llvm::OptimizationLevel::O0;
    case driver::Optimization::kRelease:
      return llvm::OptimizationLevel::O2;
  }
  throw InternalError("a build names no optimization level");
}

// An object this run has in hand, and whether it made it or took it from the
// store, which is the difference between a unit that cost the run its code
// generation and one that cost it nothing.
struct CompiledObject {
  ObjectFile file;
  bool made = true;
};

// The object a module compiles to under this build: the kept one when there is
// one and it may be taken, and otherwise one compiled now and kept. It is
// written under its own name, which no other unit's object shares, since every
// unit's module carries that unit's own symbols.
auto CompileObject(
    backend::llvm_backend::EmittedModule module, const ObjectBuild& build)
    -> diag::Result<CompiledObject> {
  const llvm::OptimizationLevel level = PipelineLevel(build.optimization);
  driver::ContentNamer namer;
  namer.Add("module", module.Print());
  namer.Add("code generator", build.code_generator.hex);
  namer.Add("pipeline level", std::format("O{}", std::to_underlying(level)));
  driver::ContentName name = namer.Finish();
  ObjectFile object{
      .path = build.object_dir / (name.hex + ".o"), .name = std::move(name)};
  if (build.store.has_value() && build.reuse_kept) {
    auto kept = driver::CopyStored(
        *build.store, driver::kStoredObjectDir, object.name, object.path);
    if (!kept) {
      return std::unexpected(std::move(kept.error()));
    }
    if (*kept) {
      return CompiledObject{.file = std::move(object), .made = false};
    }
  }
  if (auto r = backend::llvm_backend::WriteObjectFile(
          std::move(module), object.path, level);
      !r) {
    return std::unexpected(std::move(r.error()));
  }
  if (build.store.has_value()) {
    driver::KeepStored(
        *build.store, driver::kStoredObjectDir, object.name, object.path);
  }
  return CompiledObject{.file = std::move(object), .made = true};
}

auto ArtifactOf(const CompiledObject& object)
    -> diag::Result<support::Artifact> {
  std::error_code ec;
  const std::uintmax_t bytes = std::filesystem::file_size(object.file.path, ec);
  if (ec) {
    return diag::Fail(
        diag::DiagCode::kHostIoError,
        std::format(
            "failed to read the size of '{}': {}", object.file.path.string(),
            ec.message()));
  }
  return support::Artifact{
      .kind = support::ArtifactKind::kObject,
      .name = object.file.path.filename().string(),
      .bytes = static_cast<std::uint64_t>(bytes),
      .made = object.made};
}

}  // namespace

auto BuildUnit(const mir::CompilationUnit& unit, const ObjectBuild& build)
    -> diag::Result<BuiltUnit> {
  auto emitted = EmitUnitModule(unit);
  if (!emitted) {
    return std::unexpected(std::move(emitted.error()));
  }
  auto object = [&] {
    const profiling::TimeTraceScope span(
        "generate object", [&] { return unit.name; });
    return CompileObject(std::move(emitted->module), build);
  }();
  if (!object) {
    return std::unexpected(std::move(object.error()));
  }
  if (!object->made) {
    status::UpToDate();
  }
  auto artifact = ArtifactOf(*object);
  if (!artifact) {
    return std::unexpected(std::move(artifact.error()));
  }
  support::RecordUnit(unit.name, {*std::move(artifact)});
  return BuiltUnit{
      .object = std::move(object->file),
      .dpi_fragment = dpi::AbiFragmentOf(unit)};
}

void ProgramSink::Collect(BuiltUnit unit) {
  program_.objects.push_back(std::move(unit.object));
  if (unit.dpi_fragment.has_value()) {
    dpi::AddAbiFragment(program_.dpi_fragments, *std::move(unit.dpi_fragment));
  }
}

auto ProgramSink::Finish(
    const mir::CompilationUnit& root,
    const ObjectBuild& build) && -> diag::Result<ProgramObjects> {
  auto emitted = EmitUnitModule(root);
  if (!emitted) {
    return std::unexpected(std::move(emitted.error()));
  }
  backend::llvm_backend::EmittedModule entry =
      backend::llvm_backend::EmitProgramEntry(emitted->lowered);
  if (auto checked = CheckAgainstRuntime(entry); !checked) {
    return std::unexpected(std::move(checked.error()));
  }
  auto root_object = CompileObject(std::move(emitted->module), build);
  if (!root_object) {
    return std::unexpected(std::move(root_object.error()));
  }
  auto entry_object = CompileObject(std::move(entry), build);
  if (!entry_object) {
    return std::unexpected(std::move(entry_object.error()));
  }
  std::vector<support::Artifact> artifacts;
  for (const CompiledObject* object : {&*root_object, &*entry_object}) {
    auto artifact = ArtifactOf(*object);
    if (!artifact) {
      return std::unexpected(std::move(artifact.error()));
    }
    artifacts.push_back(*std::move(artifact));
  }
  support::RecordUnit(root.name, std::move(artifacts));
  program_.objects.push_back(std::move(root_object->file));
  program_.objects.push_back(std::move(entry_object->file));
  return std::move(program_);
}

}  // namespace lyra::program
