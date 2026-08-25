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
#include "lyra/backend/llvm/runtime_abi.hpp"
#include "lyra/base/internal_error.hpp"
#include "lyra/compiler/unit_metadata.hpp"
#include "lyra/lir/compilation_unit.hpp"
#include "lyra/lir/type.hpp"
#include "lyra/lir/type_id.hpp"
#include "lyra/lir/type_query.hpp"
#include "lyra/runtime/design.hpp"
#include "lyra/runtime/generated_call_scope.hpp"
#include "lyra/runtime/hierarchy_segment.hpp"
#include "lyra/runtime/jit_execution.hpp"
#include "lyra/runtime/runtime.hpp"
#include "lyra/runtime/scope.hpp"
#include "lyra/runtime/scope_program.hpp"
#include "lyra/runtime/simulation_entry.hpp"

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
        return std::move(module);
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
  add("lyra_rt_make_string", &lyra_rt_make_string);
  add("lyra_rt_make_print_literal_item", &lyra_rt_make_print_literal_item);
  add("lyra_rt_format", &lyra_rt_format);
  add("lyra_rt_packed_const", &lyra_rt_packed_const);
  add("lyra_rt_writeln", &lyra_rt_writeln);
  add("lyra_rt_write", &lyra_rt_write);
  add("lyra_rt_make_coroutine", &lyra_rt_make_coroutine);
  add("lyra_rt_register_initial", &lyra_rt_register_initial);
  add("lyra_rt_register_final", &lyra_rt_register_final);
  add("lyra_rt_delay", &lyra_rt_delay);
  add("lyra_rt_make_trigger", &lyra_rt_make_trigger);
  add("lyra_rt_wait_any", &lyra_rt_wait_any);
  add("lyra_rt_make_segment", &lyra_rt_make_segment);
  add("lyra_rt_make_scope", &lyra_rt_make_scope);
  add("lyra_rt_hierarchical_path", &lyra_rt_hierarchical_path);
  add("lyra_rt_add_owned_child", &lyra_rt_add_owned_child);
  add("lyra_rt_member_addr", &lyra_rt_member_addr);
  add("lyra_rt_register_signal", &lyra_rt_register_signal);
  add("lyra_rt_cell_packed_get", &lyra_rt_cell_packed_get);
  add("lyra_rt_cell_packed_initialize", &lyra_rt_cell_packed_initialize);
  add("lyra_rt_cell_packed_set", &lyra_rt_cell_packed_set);
  add("lyra_rt_cell_string_get", &lyra_rt_cell_string_get);
  add("lyra_rt_cell_string_initialize", &lyra_rt_cell_string_initialize);
  add("lyra_rt_cell_string_set", &lyra_rt_cell_string_set);
  add("lyra_rt_cell_real_get", &lyra_rt_cell_real_get);
  add("lyra_rt_cell_real_initialize", &lyra_rt_cell_real_initialize);
  add("lyra_rt_cell_real_set", &lyra_rt_cell_real_set);
  add("lyra_rt_cell_shortreal_get", &lyra_rt_cell_shortreal_get);
  add("lyra_rt_cell_shortreal_initialize", &lyra_rt_cell_shortreal_initialize);
  add("lyra_rt_cell_shortreal_set", &lyra_rt_cell_shortreal_set);
  add("lyra_rt_activation_frame_alloc_packed",
      &lyra_rt_activation_frame_alloc_packed);
  add("lyra_rt_activation_frame_alloc_string",
      &lyra_rt_activation_frame_alloc_string);
  add("lyra_rt_activation_frame_store_packed",
      &lyra_rt_activation_frame_store_packed);
  add("lyra_rt_activation_frame_store_string",
      &lyra_rt_activation_frame_store_string);
  add("lyra_rt_activation_frame_load_packed",
      &lyra_rt_activation_frame_load_packed);
  add("lyra_rt_activation_frame_load_string",
      &lyra_rt_activation_frame_load_string);
  add("lyra_rt_packed_add", &lyra_rt_packed_add);
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
  add("lyra_rt_packed_convert_from", &lyra_rt_packed_convert_from);
  add("lyra_rt_packed_from_bool", &lyra_rt_packed_from_bool);
  add("lyra_rt_packed_from_int", &lyra_rt_packed_from_int);
  add("lyra_rt_packed_to_int64", &lyra_rt_packed_to_int64);
  add("lyra_rt_packed_is_unknown", &lyra_rt_packed_is_unknown);
  add("lyra_rt_packed_count_bits", &lyra_rt_packed_count_bits);
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
  add("lyra_rt_string_add", &lyra_rt_string_add);
  add("lyra_rt_string_eq", &lyra_rt_string_eq);
  add("lyra_rt_string_case_equal", &lyra_rt_string_case_equal);
  add("lyra_rt_string_ne", &lyra_rt_string_ne);
  add("lyra_rt_string_lt", &lyra_rt_string_lt);
  add("lyra_rt_string_le", &lyra_rt_string_le);
  add("lyra_rt_string_gt", &lyra_rt_string_gt);
  add("lyra_rt_string_ge", &lyra_rt_string_ge);
  add("lyra_rt_make_format_spec_of_kind", &lyra_rt_make_format_spec_of_kind);
  add("lyra_rt_make_format_spec", &lyra_rt_make_format_spec);
  add("lyra_rt_make_print_value_item_packed",
      &lyra_rt_make_print_value_item_packed);
  add("lyra_rt_make_print_value_item_string",
      &lyra_rt_make_print_value_item_string);
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
  add("lyra_rt_real_const", &lyra_rt_real_const);
  add("lyra_rt_real_from_int64", &lyra_rt_real_from_int64);
  add("lyra_rt_real_from_shortreal", &lyra_rt_real_from_shortreal);
  add("lyra_rt_real_from_real", &lyra_rt_real_from_real);
  add("lyra_rt_activation_frame_alloc_real",
      &lyra_rt_activation_frame_alloc_real);
  add("lyra_rt_activation_frame_store_real",
      &lyra_rt_activation_frame_store_real);
  add("lyra_rt_activation_frame_load_real",
      &lyra_rt_activation_frame_load_real);
  add("lyra_rt_make_print_value_item_real",
      &lyra_rt_make_print_value_item_real);
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
  add("lyra_rt_shortreal_const", &lyra_rt_shortreal_const);
  add("lyra_rt_shortreal_from_int64", &lyra_rt_shortreal_from_int64);
  add("lyra_rt_shortreal_from_real", &lyra_rt_shortreal_from_real);
  add("lyra_rt_activation_frame_alloc_shortreal",
      &lyra_rt_activation_frame_alloc_shortreal);
  add("lyra_rt_activation_frame_store_shortreal",
      &lyra_rt_activation_frame_store_shortreal);
  add("lyra_rt_activation_frame_load_shortreal",
      &lyra_rt_activation_frame_load_shortreal);
  add("lyra_rt_make_print_value_item_shortreal",
      &lyra_rt_make_print_value_item_shortreal);
  add("lyra_rt_chandle_eq", &lyra_rt_chandle_eq);
  add("lyra_rt_chandle_ne", &lyra_rt_chandle_ne);
  add("lyra_rt_chandle_case_equal", &lyra_rt_chandle_case_equal);
  add("lyra_rt_chandle_to_bool", &lyra_rt_chandle_to_bool);
  add("lyra_rt_value_box_packed", &lyra_rt_value_box_packed);
  add("lyra_rt_value_box_string", &lyra_rt_value_box_string);
  add("lyra_rt_value_box_real", &lyra_rt_value_box_real);
  add("lyra_rt_value_box_shortreal", &lyra_rt_value_box_shortreal);
  add("lyra_rt_value_box_chandle", &lyra_rt_value_box_chandle);
  add("lyra_rt_value_box_tuple", &lyra_rt_value_box_tuple);
  add("lyra_rt_value_box_dynarray", &lyra_rt_value_box_dynarray);
  add("lyra_rt_tuple_make", &lyra_rt_tuple_make);
  add("lyra_rt_tuple_extract", &lyra_rt_tuple_extract);
  add("lyra_rt_tuple_update", &lyra_rt_tuple_update);
  add("lyra_rt_tuple_eq", &lyra_rt_tuple_eq);
  add("lyra_rt_tuple_ne", &lyra_rt_tuple_ne);
  add("lyra_rt_tuple_case_equal", &lyra_rt_tuple_case_equal);
  add("lyra_rt_tuple_is_unknown", &lyra_rt_tuple_is_unknown);
  add("lyra_rt_cell_tuple_get", &lyra_rt_cell_tuple_get);
  add("lyra_rt_cell_tuple_initialize", &lyra_rt_cell_tuple_initialize);
  add("lyra_rt_cell_tuple_set", &lyra_rt_cell_tuple_set);
  add("lyra_rt_activation_frame_alloc_tuple",
      &lyra_rt_activation_frame_alloc_tuple);
  add("lyra_rt_activation_frame_store_tuple",
      &lyra_rt_activation_frame_store_tuple);
  add("lyra_rt_activation_frame_load_tuple",
      &lyra_rt_activation_frame_load_tuple);
  add("lyra_rt_dynarray_default", &lyra_rt_dynarray_default);
  add("lyra_rt_dynarray_new", &lyra_rt_dynarray_new);
  add("lyra_rt_dynarray_new_copy", &lyra_rt_dynarray_new_copy);
  add("lyra_rt_dynarray_from_literal_packed",
      &lyra_rt_dynarray_from_literal_packed);
  add("lyra_rt_dynarray_from_literal_string",
      &lyra_rt_dynarray_from_literal_string);
  add("lyra_rt_dynarray_from_literal_real",
      &lyra_rt_dynarray_from_literal_real);
  add("lyra_rt_dynarray_from_literal_shortreal",
      &lyra_rt_dynarray_from_literal_shortreal);
  add("lyra_rt_dynarray_from_literal_chandle",
      &lyra_rt_dynarray_from_literal_chandle);
  add("lyra_rt_dynarray_from_literal_tuple",
      &lyra_rt_dynarray_from_literal_tuple);
  add("lyra_rt_dynarray_from_literal_dynarray",
      &lyra_rt_dynarray_from_literal_dynarray);
  add("lyra_rt_dynarray_element", &lyra_rt_dynarray_element);
  add("lyra_rt_dynarray_with_element", &lyra_rt_dynarray_with_element);
  add("lyra_rt_dynarray_delete", &lyra_rt_dynarray_delete);
  add("lyra_rt_dynarray_size", &lyra_rt_dynarray_size);
  add("lyra_rt_dynarray_eq", &lyra_rt_dynarray_eq);
  add("lyra_rt_dynarray_ne", &lyra_rt_dynarray_ne);
  add("lyra_rt_dynarray_case_equal", &lyra_rt_dynarray_case_equal);
  add("lyra_rt_cell_dynarray_get", &lyra_rt_cell_dynarray_get);
  add("lyra_rt_cell_dynarray_initialize", &lyra_rt_cell_dynarray_initialize);
  add("lyra_rt_cell_dynarray_set", &lyra_rt_cell_dynarray_set);
  add("lyra_rt_activation_frame_alloc_dynarray",
      &lyra_rt_activation_frame_alloc_dynarray);
  add("lyra_rt_activation_frame_store_dynarray",
      &lyra_rt_activation_frame_store_dynarray);
  add("lyra_rt_activation_frame_load_dynarray",
      &lyra_rt_activation_frame_load_dynarray);
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

// The runtime's spelling of a domain the execution backend classified a type
// into. The two enumerations are the two sides of one ABI -- the backend names
// the library entry, the runtime realizes the storage -- and only the backend
// classifies a LIR type, so the entry a call names and the storage a cell owns
// cannot disagree.
auto AbiDomain(backend::llvm_backend::ValueDomain domain)
    -> runtime::ValueDomain {
  switch (domain) {
    case backend::llvm_backend::ValueDomain::kPacked:
      return runtime::ValueDomain::kPacked;
    case backend::llvm_backend::ValueDomain::kString:
      return runtime::ValueDomain::kString;
    case backend::llvm_backend::ValueDomain::kReal:
      return runtime::ValueDomain::kReal;
    case backend::llvm_backend::ValueDomain::kShortReal:
      return runtime::ValueDomain::kShortReal;
    case backend::llvm_backend::ValueDomain::kChandle:
      return runtime::ValueDomain::kChandle;
    case backend::llvm_backend::ValueDomain::kTuple:
      return runtime::ValueDomain::kTuple;
    case backend::llvm_backend::ValueDomain::kDynArray:
      return runtime::ValueDomain::kDynArray;
  }
  throw InternalError("jit executor: unknown value domain");
}

// The storage a generic instance realizes for one declared member, projected
// from the member's LIR type: an observable cell holds a value other processes
// subscribe to, and a reference-typed member is a box holding a borrowed
// handle.
auto DescribeMember(const lir::CompilationUnit& unit, lir::TypeId type)
    -> runtime::MemberStorageDescriptor {
  const auto& data = unit.types.Get(type).data;
  if (const auto* observable = std::get_if<lir::ObservableType>(&data)) {
    return runtime::MemberStorageDescriptor{
        .kind = runtime::MemberStorageKind::kObservableCell,
        .domain = AbiDomain(
            backend::llvm_backend::ValueDomainOf(unit, observable->value))};
  }
  if (const auto* library = std::get_if<lir::RuntimeLibraryType>(&data);
      library != nullptr &&
      library->kind == lir::RuntimeLibraryKind::kCancellationSource) {
    return runtime::MemberStorageDescriptor{
        .kind = runtime::MemberStorageKind::kCancellationSource,
        .domain = runtime::ValueDomain::kNone};
  }
  if (lir::Pointee(unit.types, type).has_value()) {
    return runtime::MemberStorageDescriptor{
        .kind = runtime::MemberStorageKind::kBorrowedHandle,
        .domain = runtime::ValueDomain::kNone};
  }
  // A chandle member is a value the instance owns but no process subscribes to
  // (LRM 6.14), stored inline in its slot rather than behind an observable
  // cell.
  if (std::holds_alternative<lir::ChandleType>(data)) {
    return runtime::MemberStorageDescriptor{
        .kind = runtime::MemberStorageKind::kInlineValue,
        .domain = runtime::ValueDomain::kChandle};
  }
  throw InternalError("jit executor: member type has no storage realization");
}

auto DescribeMembers(const lir::CompilationUnit& unit, const lir::Class& cls)
    -> std::vector<runtime::MemberStorageDescriptor> {
  std::vector<runtime::MemberStorageDescriptor> descriptors;
  descriptors.reserve(cls.members.size());
  for (const lir::Member& member : cls.members) {
    descriptors.push_back(DescribeMember(unit, member.type));
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

// The class an owned-child member reaches, absent for a member that reaches
// storage instead.
auto OwnedChildClass(const lir::CompilationUnit& unit, lir::TypeId type)
    -> std::optional<lir::ClassId> {
  const std::optional<lir::TypeId> pointee = lir::Pointee(unit.types, type);
  if (!pointee) {
    return std::nullopt;
  }
  const auto* object =
      std::get_if<lir::ObjectType>(&unit.types.Get(*pointee).data);
  return object != nullptr ? std::optional{object->class_id} : std::nullopt;
}

// One entry per node of the unit's object tree, reached by descending
// containment from its root. A class no containment edge reaches is one the
// program allocates as an ordinary object; the runtime never builds it and it
// needs no definition.
auto LoadScopeClasses(
    const lir::CompilationUnit& unit,
    const compiler::ElaboratedUnitMetadata& metadata)
    -> std::vector<LoadedScopeClass> {
  std::vector<LoadedScopeClass> loaded;
  const auto descend = [&](const auto& self_ref, lir::ClassId id) -> void {
    const lir::Class& cls = unit.classes.Get(id);
    // Every scope of a unit runs at the unit's precision: a scope inside a unit
    // has no timescale declaration of its own and takes the enclosing one (LRM
    // 3.14.2.3).
    loaded.push_back(
        LoadedScopeClass{
            .name = cls.name,
            .time_precision_power = metadata.time_precision_power,
            .members = DescribeMembers(unit, cls),
            .definition = std::make_unique<runtime::ScopeDefinition>()});
    // A member whose type reaches an object of this unit is a child this class
    // owns; one reaching a value reaches storage instead. The type says which,
    // so descending it needs nothing beside the members already declared.
    for (const lir::Member& member : cls.members) {
      if (const auto child = OwnedChildClass(unit, member.type)) {
        self_ref(self_ref, *child);
      }
    }
  };
  if (unit.root.has_value()) {
    descend(descend, *unit.root);
  }
  return loaded;
}

}  // namespace

auto Execute(
    std::span<const lir::CompilationUnit> units,
    std::span<const compiler::ElaboratedUnitMetadata> metadata,
    const lir::CompilationUnit& root_unit,
    const compiler::ElaboratedUnitMetadata& root_metadata,
    const std::optional<std::filesystem::path>& dpi_library) -> int {
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
  // the class it builds by symbol. Each definition owns a stable address for
  // the whole run; the runtime holds pointers into it. The root is loaded and
  // driven like any other unit, distinguished only as the bootstrap entry
  // below.
  std::vector<const lir::CompilationUnit*> loaded_units;
  std::vector<LoadedScopeClass> loaded;
  loaded_units.reserve(units.size() + 1);
  for (std::size_t i = 0; i < units.size(); ++i) {
    loaded_units.push_back(&units[i]);
    std::vector<LoadedScopeClass> unit_classes =
        LoadScopeClasses(units[i], metadata[i]);
    loaded.insert(
        loaded.end(), std::make_move_iterator(unit_classes.begin()),
        std::make_move_iterator(unit_classes.end()));
  }
  loaded_units.push_back(&root_unit);
  if (!root_unit.root.has_value()) {
    throw InternalError("jit executor: the design root roots no object tree");
  }
  const std::string root_class_name =
      root_unit.classes.Get(*root_unit.root).name;
  std::vector<LoadedScopeClass> root_classes =
      LoadScopeClasses(root_unit, root_metadata);
  loaded.insert(
      loaded.end(), std::make_move_iterator(root_classes.begin()),
      std::make_move_iterator(root_classes.end()));

  // The schema is named only once every class is in place, so no descriptor
  // vector is reallocated out from under a definition that points at it.
  for (LoadedScopeClass& entry : loaded) {
    entry.definition->members = runtime::MemberStorageSchema{
        .data = entry.members.data(),
        .size = static_cast<std::uint32_t>(entry.members.size())};
  }

  for (const lir::CompilationUnit* unit : loaded_units) {
    auto owned = backend::llvm_backend::EmitModule(*unit).Release();
    Check(
        jit->addIRModule(
            llvm::orc::ThreadSafeModule(
                std::move(owned.module), std::move(owned.context))),
        "add module");
  }

  // Each scope class publishes its definition as an injected data symbol the
  // construct that builds an instance of it references. Every definition is
  // filled from its JIT-compiled entries after every address is injected, so a
  // reference resolves regardless of the order the definitions are filled.
  llvm::orc::SymbolMap definition_symbols;
  for (const LoadedScopeClass& entry : loaded) {
    const std::string symbol =
        backend::llvm_backend::ScopeDefinitionSymbolName(entry.name);
    definition_symbols[jit->getExecutionSession().intern(symbol)] =
        llvm::orc::ExecutorSymbolDef(
            llvm::orc::ExecutorAddr::fromPtr(entry.definition.get()),
            llvm::JITSymbolFlags::Exported);
  }
  Check(
      jit->getMainJITDylib().define(
          llvm::orc::absoluteSymbols(std::move(definition_symbols))),
      "define scope definitions");

  for (const LoadedScopeClass& entry : loaded) {
    FillDefinition(
        *jit, entry.name, entry.time_precision_power, *entry.definition);
  }

  runtime::Runtime runtime_instance;

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
