#pragma once

#include <string>
#include <vector>

namespace lyra::lowering::hir_to_mir {

// The design root's plan for bringing up what every unit's namespace owns (LRM
// 26.2 / 8.9 / 10.5), resolved once by the whole-design assembly: every
// namespace unit of the design, ordered so a unit comes before the ones whose
// initializers read it wherever a direct read makes that known, and in a stable
// order otherwise. LRM 10.5 leaves the relative order of initializers
// unspecified, so the order is what makes one run's output match the next
// rather than a correctness input.
//
// A unit that declares nothing takes part like any other, so nothing asks first
// what a given one supplied. The design root realizes the list into calls; it
// never re-derives the plan.
struct NamespaceStorageInitializationPlan {
  std::vector<std::string> units;
};

}  // namespace lyra::lowering::hir_to_mir
