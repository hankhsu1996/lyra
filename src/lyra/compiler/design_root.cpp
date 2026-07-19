#include "lyra/compiler/design_root.hpp"

#include <algorithm>
#include <cstdint>
#include <expected>
#include <optional>
#include <string>
#include <string_view>
#include <unordered_map>
#include <unordered_set>
#include <utility>
#include <vector>

#include "lyra/compiler/unit_metadata.hpp"
#include "lyra/hir/compilation_unit.hpp"
#include "lyra/hir/structural_scope.hpp"
#include "lyra/lir/verify.hpp"
#include "lyra/lowering/hir_to_mir/package_initialization.hpp"
#include "lyra/lowering/hir_to_mir/unit_lowerer.hpp"
#include "lyra/lowering/mir_to_lir/lower.hpp"
#include "lyra/mir/callable_id.hpp"
#include "lyra/mir/compilation_unit.hpp"

namespace lyra::compiler {

namespace {

// The design-root unit is a module whose only members are the top-level units,
// instantiated as its owned children. Its constructor then elaborates the
// design through the same owned-child construction any parent uses for a
// submodule, so no code path is special-cased for the top level. Its HIR
// carries only this source-faithful structure; running the packages'
// initializers is a whole-design composition step handled at lowering, not HIR
// content.
auto BuildDesignRootHir(std::span<const std::string> top_names)
    -> hir::CompilationUnit {
  hir::CompilationUnit root{std::string{kDesignRootUnitName}};
  for (const auto& name : top_names) {
    root.root_scope.instance_members.Add(
        hir::InstanceMemberDecl{
            .instance_name = name, .target_unit = name, .array_dims = {}});
  }
  return root;
}

// Whether the unit owns a callable of this name -- used to tell a package that
// has a value initializer (and so a synthesized initialize callable) from one
// whose variables all take their default.
auto HasCallableNamed(const mir::CompilationUnit& unit, std::string_view name)
    -> bool {
  for (std::uint32_t i = 0; i < unit.callables.size(); ++i) {
    if (unit.callables.Get(mir::CallableId{i}).name == name) {
      return true;
    }
  }
  return false;
}

// Post-order DFS emitting `name` after the packages its initializer reads (LRM
// 26.2 / 10.5), so a dependency precedes its dependent. The LRM leaves the
// relative order of initializers unspecified; this is a stable, best-effort
// preference, not a correctness input -- every cell is installed with its
// default before any initializer runs, so a missed or cyclic dependency only
// means a read observes a default, never an uninstalled cell. Visiting `name`
// before descending makes a cyclic dependency terminate with a deterministic
// order rather than recur; `deps` is walked in the caller's stable
// (name-sorted) order, so the whole result is reproducible for a given design.
void OrderPackageInit(
    const std::string& name,
    const std::unordered_map<std::string, std::vector<std::string>>&
        package_deps,
    const std::unordered_set<std::string>& init_set,
    std::unordered_set<std::string>& placed,
    std::vector<std::string>& ordered) {
  if (!placed.insert(name).second) {
    return;
  }
  if (const auto it = package_deps.find(name); it != package_deps.end()) {
    for (const std::string& dep : it->second) {
      if (init_set.contains(dep)) {
        OrderPackageInit(dep, package_deps, init_set, placed, ordered);
      }
    }
  }
  ordered.push_back(name);
}

// Resolves the whole-design package initialization plan from the compiled
// units. A package unit is the one with no root class (`root == nullopt`); the
// install pass covers every package that declares a variable, and the
// value-initialize pass every package that has a value initializer, ordered so
// a package a given initializer reads directly comes first where that is
// possible. Both passes are name-sorted first so the result is deterministic
// for a given design.
auto BuildPackageInitializationPlan(std::span<const mir::CompilationUnit> units)
    -> lowering::hir_to_mir::PackageInitializationPlan {
  std::vector<std::string> packages_with_value_init;
  std::unordered_map<std::string, std::vector<std::string>> package_init_deps;
  lowering::hir_to_mir::PackageInitializationPlan plan;
  for (const auto& unit : units) {
    if (unit.root.has_value()) {
      continue;
    }
    if (!unit.static_variables.empty()) {
      plan.install_order.push_back(unit.name);
    }
    if (HasCallableNamed(
            unit, lowering::hir_to_mir::kPackageInitializeCallableName)) {
      packages_with_value_init.push_back(unit.name);
      package_init_deps.emplace(
          unit.name, unit.direct_initializer_package_reads);
    }
  }
  std::ranges::sort(plan.install_order);
  std::ranges::sort(packages_with_value_init);
  const std::unordered_set<std::string> init_set(
      packages_with_value_init.begin(), packages_with_value_init.end());
  std::unordered_set<std::string> placed;
  for (const std::string& name : packages_with_value_init) {
    OrderPackageInit(
        name, package_init_deps, init_set, placed, plan.value_initialize_order);
  }
  return plan;
}

}  // namespace

auto LinkDesign(
    std::span<const mir::CompilationUnit> units,
    std::span<const std::string> top_names, StopAfter stop_after,
    const diag::SourceManager& source_manager)
    -> diag::Result<DesignRootArtifacts> {
  const hir::CompilationUnit root_hir = BuildDesignRootHir(top_names);
  lowering::hir_to_mir::UnitLowerer root_lowerer(root_hir, source_manager);
  auto root_mir =
      root_lowerer.RunDesignRoot(BuildPackageInitializationPlan(units));
  if (!root_mir) {
    return std::unexpected(std::move(root_mir.error()));
  }
  DesignRootArtifacts artifacts{
      .mir = *std::move(root_mir),
      .lir = std::nullopt,
      .metadata = std::nullopt};

  if (stop_after < StopAfter::kLir) {
    return artifacts;
  }
  auto root_lir = lowering::mir_to_lir::LowerUnit(artifacts.mir);
  if (!root_lir) {
    return std::unexpected(std::move(root_lir.error()));
  }
  artifacts.lir = *std::move(root_lir);
  lir::Verify(*artifacts.lir);
  artifacts.metadata = BuildUnitMetadata(artifacts.mir);
  return artifacts;
}

}  // namespace lyra::compiler
