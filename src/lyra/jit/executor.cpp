#include "lyra/jit/executor.hpp"

#include <cstdint>
#include <memory>
#include <optional>
#include <string>
#include <string_view>
#include <utility>
#include <vector>

#include <llvm/ExecutionEngine/JITSymbol.h>
#include <llvm/ExecutionEngine/Orc/Core.h>
#include <llvm/ExecutionEngine/Orc/LLJIT.h>
#include <llvm/ExecutionEngine/Orc/Shared/ExecutorAddress.h>
#include <llvm/ExecutionEngine/Orc/ThreadSafeModule.h>
#include <llvm/Support/Error.h>
#include <llvm/Support/TargetSelect.h>

#include "lyra/backend/llvm/emit.hpp"
#include "lyra/base/internal_error.hpp"
#include "lyra/compiler/unit_metadata.hpp"
#include "lyra/lir/compilation_unit.hpp"
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
  add("lyra_rt_make_segment", &lyra_rt_make_segment);
  add("lyra_rt_make_unit", &lyra_rt_make_unit);
  add("lyra_rt_add_owned_child", &lyra_rt_add_owned_child);
  add("lyra_rt_load_field", &lyra_rt_load_field);
  add("lyra_rt_store_field", &lyra_rt_store_field);
  Check(
      jit.getMainJITDylib().define(
          llvm::orc::absoluteSymbols(std::move(symbols))),
      "define runtime abi");
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
  definition.member_slot_count =
      static_cast<std::uint32_t>(root_class.members.size());
}

// One design unit loaded into the JIT: its executable body, its source-level
// metadata, and the runtime definition built from the two, which owns a stable
// address the runtime and peer units reference.
struct LoadedUnit {
  const lir::CompilationUnit* unit;
  const compiler::ElaboratedUnitMetadata* metadata;
  std::unique_ptr<runtime::UnitDefinition> definition;
};

}  // namespace

auto Execute(
    std::span<const lir::CompilationUnit> units,
    std::span<const compiler::ElaboratedUnitMetadata> metadata,
    const lir::CompilationUnit& root_unit,
    const compiler::ElaboratedUnitMetadata& root_metadata) -> int {
  llvm::InitializeNativeTarget();
  llvm::InitializeNativeTargetAsmPrinter();

  auto jit = Unwrap(llvm::orc::LLJITBuilder().create(), "create jit");
  DefineRuntimeAbi(*jit);

  // Every unit -- the source units and the design-root -- becomes one module in
  // the shared JIT, so a unit's construct reaches another unit's entries and
  // definition by symbol. Each unit's definition owns a stable address for the
  // whole run; the runtime holds pointers into it. The root is loaded and
  // driven like any other unit, distinguished only as the bootstrap entry
  // below.
  std::vector<LoadedUnit> loaded;
  loaded.reserve(units.size() + 1);
  for (std::size_t i = 0; i < units.size(); ++i) {
    loaded.push_back(
        LoadedUnit{
            .unit = &units[i],
            .metadata = &metadata[i],
            .definition = std::make_unique<runtime::UnitDefinition>()});
  }
  loaded.push_back(
      LoadedUnit{
          .unit = &root_unit,
          .metadata = &root_metadata,
          .definition = std::make_unique<runtime::UnitDefinition>()});

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
