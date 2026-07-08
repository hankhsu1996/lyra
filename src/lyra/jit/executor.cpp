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
  Check(
      jit.getMainJITDylib().define(
          llvm::orc::absoluteSymbols(std::move(symbols))),
      "define runtime abi");
}

}  // namespace

auto Execute(
    const lir::CompilationUnit& unit,
    const compiler::ElaboratedUnitMetadata& metadata) -> int {
  llvm::InitializeNativeTarget();
  llvm::InitializeNativeTargetAsmPrinter();

  auto jit = Unwrap(llvm::orc::LLJITBuilder().create(), "create jit");
  DefineRuntimeAbi(*jit);

  auto owned = backend::llvm_backend::EmitModule(unit).Release();
  Check(
      jit->addIRModule(
          llvm::orc::ThreadSafeModule(
              std::move(owned.module), std::move(owned.context))),
      "add module");

  const lir::Class& root_class = unit.classes.Get(unit.root);
  auto lookup =
      [&](std::string_view entry) -> std::optional<llvm::orc::ExecutorAddr> {
    auto symbol = jit->lookup(root_class.name + "." + std::string(entry));
    if (!symbol) {
      llvm::consumeError(symbol.takeError());
      return std::nullopt;
    }
    return *symbol;
  };

  // The linked module supplies each lifecycle entry as an ABI-compatible native
  // function over the generic scope receiver (`void(Scope*)`); an entry the
  // scope has no work for is absent and keeps the runtime no-op default. The
  // constant metadata comes from the unit's definition metadata, not from the
  // executable body -- LIR carries no time precision or def name. The program,
  // definition, and the metadata whose name the def-name borrows all outlive
  // the simulation, which runs to completion below before this frame returns.
  runtime::ScopeProgram program;
  program.metadata.def_name = runtime::AbiStringRef(
      metadata.def_name.data(),
      static_cast<std::uint32_t>(metadata.def_name.size()));
  program.metadata.time_precision_power = metadata.time_precision_power;
  if (auto entry = lookup("ResolveState")) {
    program.resolve_state = entry->toPtr<runtime::ScopeEntry>();
  }
  if (auto entry = lookup("InitializeState")) {
    program.initialize_state = entry->toPtr<runtime::ScopeEntry>();
  }
  if (auto entry = lookup("CreateProcesses")) {
    program.create_processes = entry->toPtr<runtime::ScopeEntry>();
  }

  runtime::UnitDefinition definition;
  definition.root = program;
  if (auto entry = lookup("constructor")) {
    definition.construct = entry->toPtr<runtime::ScopeEntry>();
  }

  runtime::Engine engine;
  auto top = std::make_unique<runtime::Instance>(
      nullptr, runtime::HierarchySegment{root_class.name, {}},
      engine.Services(), &definition);

  // Construction runs generated code that allocates opaque value temporaries in
  // the ambient arena, so the bootstrap driver wraps the construct call in a
  // generated-call scope. The lifecycle phases in BindDesign each establish
  // their own scope at the runtime's per-call boundary; each process body does
  // the same when the simulation resumes it.
  {
    runtime::GeneratedCallScope construct_scope;
    definition.construct(top.get());
  }
  std::vector<std::unique_ptr<runtime::Scope>> tops;
  tops.push_back(std::move(top));
  engine.BindDesign(std::move(tops));
  return runtime::RunSimulation(engine);
}

}  // namespace lyra::jit
