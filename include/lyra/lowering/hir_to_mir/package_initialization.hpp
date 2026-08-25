#pragma once

#include <string>
#include <string_view>
#include <vector>

namespace lyra::lowering::hir_to_mir {

// Package variable initialization at time zero runs in two design-wide phases,
// each a synthesized receiver-less callable the design root calls. Named
// through constants so the package that defines each callable and the design
// root that calls it agree on the spelling.
//
// Phase 1 (`Install`): install every package cell's declared type and language
// default. It fires nothing and takes no runtime handle. The design root calls
// it for every package before any value initializer runs anywhere, so a
// cross-package read always reaches installed storage -- at worst a default.
//
// Phase 2 (`Initialize`): run each LRM 10.5 value initializer through its cell,
// firing subscribers; it takes the runtime handle. The design root calls it
// in a stable, best-effort dependency order (a quality-of-implementation choice
// -- the LRM leaves the relative order of initializers unspecified, not a
// race).
inline constexpr std::string_view kPackageInstallCallableName =
    "InstallPackageVariables";
inline constexpr std::string_view kPackageInitializeCallableName =
    "InitializePackageVariables";

// The design root's plan for initializing package variables (LRM 26.2 / 10.5),
// resolved once by the whole-design assembly. Both lists name every package of
// the design: a package that declares nothing runs an empty body, so nothing
// asks first whether a package has work. `install_order` is any stable order,
// the phase-1 install being order-independent; `value_initialize_order` is the
// best-effort order the assembly chose (a dependency before its dependent where
// a direct read makes it known, a stable order otherwise). The design root
// realizes both into calls; it never re-derives the plan.
struct PackageInitializationPlan {
  std::vector<std::string> install_order;
  std::vector<std::string> value_initialize_order;
};

}  // namespace lyra::lowering::hir_to_mir
