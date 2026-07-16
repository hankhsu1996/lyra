#include "lyra/jit/executor.hpp"

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
#include "lyra/runtime/engine.hpp"
#include "lyra/runtime/generated_call_scope.hpp"
#include "lyra/runtime/hierarchy_segment.hpp"
#include "lyra/runtime/jit_execution.hpp"
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
  add("lyra_rt_services", &lyra_rt_services);
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
  add("lyra_rt_make_unit", &lyra_rt_make_unit);
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
  add("lyra_rt_string_from_packed_array", &lyra_rt_string_from_packed_array);
  add("lyra_rt_string_string_cstr", &lyra_rt_string_string_cstr);
  add("lyra_rt_string_len", &lyra_rt_string_len);
  add("lyra_rt_string_getc", &lyra_rt_string_getc);
  add("lyra_rt_string_toupper", &lyra_rt_string_toupper);
  add("lyra_rt_string_tolower", &lyra_rt_string_tolower);
  add("lyra_rt_string_compare", &lyra_rt_string_compare);
  add("lyra_rt_string_icompare", &lyra_rt_string_icompare);
  add("lyra_rt_string_substr", &lyra_rt_string_substr);
  add("lyra_rt_string_atoi", &lyra_rt_string_atoi);
  add("lyra_rt_string_atohex", &lyra_rt_string_atohex);
  add("lyra_rt_string_atooct", &lyra_rt_string_atooct);
  add("lyra_rt_string_atobin", &lyra_rt_string_atobin);
  add("lyra_rt_string_add", &lyra_rt_string_add);
  add("lyra_rt_string_eq", &lyra_rt_string_eq);
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
  if (lir::Pointee(unit.types, type).has_value()) {
    return runtime::MemberStorageDescriptor{
        .kind = runtime::MemberStorageKind::kBorrowedHandle,
        .domain = runtime::ValueDomain::kNone};
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

// Fills a unit's runtime definition from its JIT-compiled entries and its
// source-level metadata. The lifecycle entries are ABI-compatible native
// functions over the generic scope receiver; an entry the scope has no work for
// is absent and keeps the runtime no-op default. Looking a symbol up here
// materializes its module, which resolves that module's cross-unit definition
// references -- every definition symbol is injected before any is filled, so
// those references find their address regardless of fill order.
void FillDefinition(
    llvm::orc::LLJIT& jit, const lir::CompilationUnit& unit,
    const compiler::ElaboratedUnitMetadata& metadata,
    runtime::UnitDefinition& definition) {
  const lir::Class& root_class = unit.classes.Get(unit.root);
  auto lookup =
      [&](std::string_view entry) -> std::optional<llvm::orc::ExecutorAddr> {
    auto symbol = jit.lookup(root_class.name + "." + std::string(entry));
    if (!symbol) {
      llvm::consumeError(symbol.takeError());
      return std::nullopt;
    }
    return *symbol;
  };
  definition.root.metadata.def_name = runtime::AbiStringRef(
      metadata.def_name.data(),
      static_cast<std::uint32_t>(metadata.def_name.size()));
  definition.root.metadata.time_precision_power = metadata.time_precision_power;
  if (auto entry = lookup("ResolveState")) {
    definition.root.resolve_state = entry->toPtr<runtime::ScopeEntry>();
  }
  if (auto entry = lookup("InitializeState")) {
    definition.root.initialize_state = entry->toPtr<runtime::ScopeEntry>();
  }
  if (auto entry = lookup("CreateProcesses")) {
    definition.root.create_processes = entry->toPtr<runtime::ScopeEntry>();
  }
  if (auto entry = lookup("constructor")) {
    definition.construct = entry->toPtr<runtime::ScopeEntry>();
  }
}

// One design unit loaded into the JIT: its executable body, its source-level
// metadata, the storage schema its instances realize, and the runtime
// definition built from those, which owns a stable address the runtime and peer
// units reference. The schema is held here because the definition names it as
// plain data it does not own.
struct LoadedUnit {
  const lir::CompilationUnit* unit;
  const compiler::ElaboratedUnitMetadata* metadata;
  std::vector<runtime::MemberStorageDescriptor> members;
  std::unique_ptr<runtime::UnitDefinition> definition;
};

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
  // the shared JIT, so a unit's construct reaches another unit's entries and
  // definition by symbol. Each unit's definition owns a stable address for the
  // whole run; the runtime holds pointers into it. The root is loaded and
  // driven like any other unit, distinguished only as the bootstrap entry
  // below.
  const auto load = [](const lir::CompilationUnit& unit,
                       const compiler::ElaboratedUnitMetadata& unit_metadata) {
    return LoadedUnit{
        .unit = &unit,
        .metadata = &unit_metadata,
        .members = DescribeMembers(unit, unit.classes.Get(unit.root)),
        .definition = std::make_unique<runtime::UnitDefinition>()};
  };

  std::vector<LoadedUnit> loaded;
  loaded.reserve(units.size() + 1);
  for (std::size_t i = 0; i < units.size(); ++i) {
    loaded.push_back(load(units[i], metadata[i]));
  }
  loaded.push_back(load(root_unit, root_metadata));

  // The schema is named only once every unit is in place, so no descriptor
  // vector is reallocated out from under a definition that points at it.
  for (LoadedUnit& entry : loaded) {
    entry.definition->members = runtime::MemberStorageSchema{
        .data = entry.members.data(),
        .size = static_cast<std::uint32_t>(entry.members.size())};
  }

  for (const LoadedUnit& entry : loaded) {
    auto owned = backend::llvm_backend::EmitModule(*entry.unit).Release();
    Check(
        jit->addIRModule(
            llvm::orc::ThreadSafeModule(
                std::move(owned.module), std::move(owned.context))),
        "add module");
  }

  // A unit publishes its definition as an injected data symbol its peers'
  // construct entries reference. Each definition is filled from the unit's
  // JIT-compiled entries after every definition's address is injected, so a
  // cross-unit reference resolves regardless of the order the units are filled.
  llvm::orc::SymbolMap definition_symbols;
  for (const LoadedUnit& entry : loaded) {
    const std::string symbol = backend::llvm_backend::UnitDefinitionSymbolName(
        entry.unit->classes.Get(entry.unit->root).name);
    definition_symbols[jit->getExecutionSession().intern(symbol)] =
        llvm::orc::ExecutorSymbolDef(
            llvm::orc::ExecutorAddr::fromPtr(entry.definition.get()),
            llvm::JITSymbolFlags::Exported);
  }
  Check(
      jit->getMainJITDylib().define(
          llvm::orc::absoluteSymbols(std::move(definition_symbols))),
      "define unit definitions");

  for (const LoadedUnit& entry : loaded) {
    FillDefinition(*jit, *entry.unit, *entry.metadata, *entry.definition);
  }

  runtime::Engine engine;

  // The design-root unit's construct elaborates the design: it builds the
  // top-level units through the cross-unit construct ABI, which recurses into
  // their subtrees. The bootstrap allocates the root instance and runs that
  // construct in a generated-call scope, exactly as the runtime enters any
  // construct entry; the engine then walks the built tree.
  const runtime::UnitDefinition& root_definition = *loaded.back().definition;
  auto root = std::make_unique<runtime::GeneratedInstance>(
      nullptr, runtime::HierarchySegment{"$root", {}}, engine.Services(),
      &root_definition);
  {
    runtime::GeneratedCallScope construct_scope;
    root_definition.construct(root.get());
  }
  engine.BindDesign(std::move(root));
  return runtime::RunSimulation(engine);
}

}  // namespace lyra::jit
