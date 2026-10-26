#pragma once

#include <string>

#include "lyra/hir/compilation_unit.hpp"
#include "lyra/hir/scope_construction.hpp"

namespace lyra::hir {

// One specialization of a unit (LRM 23.10): its name, how its objects are built
// beyond what their code states, and the implementation that code is.
// Specializations whose implementations are equal share one, and differ only
// in how their objects are built.
struct Specialization {
  std::string name;
  ScopeConstruction construction;
  CompilationUnit implementation;

  // Whether its implementation is named for it: no specialization of its unit
  // before it, in the design's order, came out as that implementation, so it
  // is the one everything after HIR runs on.
  [[nodiscard]] auto IsFirstOfItsImplementation() const -> bool {
    return implementation.body_name == name;
  }

  auto operator==(const Specialization&) const -> bool = default;
};

}  // namespace lyra::hir
