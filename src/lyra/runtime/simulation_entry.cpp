#include "lyra/runtime/simulation_entry.hpp"

#include <cstddef>
#include <cstdlib>
#include <exception>
#include <memory>
#include <span>
#include <string>
#include <utility>
#include <vector>

#include "lyra/runtime/ambient_run_context.hpp"
#include "lyra/runtime/class_definition.hpp"
#include "lyra/runtime/design.hpp"
#include "lyra/runtime/hierarchy_segment.hpp"
#include "lyra/runtime/plusargs.hpp"
#include "lyra/runtime/program_declarations.hpp"
#include "lyra/runtime/runtime.hpp"
#include "lyra/runtime/runtime_effects.hpp"
#include "lyra/runtime/scope.hpp"

namespace lyra::runtime {

auto RunDesignRoot(
    int argc, char** argv, std::string_view root_name, const RootFactory& make)
    -> int {
  // A built program's own argv leads with its name, which is not one of the
  // simulation's arguments.
  const std::span<char*> args{argv, static_cast<std::size_t>(argc)};
  std::vector<std::string> arguments;
  for (std::size_t i = 1; i < args.size(); ++i) {
    arguments.emplace_back(args[i]);
  }
  auto options = DefaultRuntimeOptions();
  options.plusargs = PlusargsFrom(arguments);
  Runtime runtime{std::move(options)};

  // Building the design and resolving its references is elaboration, which
  // precedes the simulation (LRM 3.12). An error here has no activation to
  // leave and no final procedure to reach, so it is reported and the run never
  // starts.
  try {
    runtime.BindDesign(
        std::make_unique<Design>(
            make(nullptr, HierarchySegment{std::string{root_name}, {}})));
  } catch (const std::exception&) {
    ReportRaisedError(runtime, std::current_exception());
    return EXIT_FAILURE;
  }

  return RunSimulation(runtime);
}

auto RunDeclaredProgram(
    int argc, char** argv, std::string_view root_name,
    const ScopeDefinition& root) -> int {
  RealizeDeclarations();
  return RunDesignRoot(
      argc, argv, root_name,
      [&root](
          Scope* parent, HierarchySegment segment) -> std::unique_ptr<Scope> {
        auto scope = std::make_unique<Scope>(parent, segment, &root);
        root.construct(scope.get(), parent, &segment, {});
        return scope;
      });
}

auto RunSimulation(Runtime& runtime) -> int {
  try {
    // Foreign code reaches the run with plain C arguments and nothing to find
    // it by, so what it resolves a scope against is anchored for as long as the
    // run lasts -- which starts at the first thing a run does, initializing
    // state, since a context import can already be reached from there (LRM
    // 10.5, 35.5.3).
    const AmbientRunContext run_context{&runtime.DesignRoot(), runtime};
    return runtime.Run();
  } catch (const std::exception&) {
    ReportRaisedError(runtime, std::current_exception());
    return EXIT_FAILURE;
  }
}

}  // namespace lyra::runtime
