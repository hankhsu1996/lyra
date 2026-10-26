#pragma once

#include <functional>
#include <map>
#include <string>

#include "lyra/hir/scope_instantiations.hpp"
#include "lyra/hir/specialization.hpp"

namespace lyra::lowering::hir_to_mir {

// One specialization of the design: the body its code is, and what each scope
// of its objects makes at each of its instantiations and each place of each of
// its generates.
struct SpecializationMade {
  std::string body;
  hir::ScopeInstantiations made;

  // What `specialization` builds, read off it while its implementation is
  // still held, since only the first specialization of an implementation keeps
  // it past HIR.
  [[nodiscard]] static auto Of(const hir::Specialization& specialization)
      -> SpecializationMade {
    const hir::CompilationUnit& implementation = specialization.implementation;
    return SpecializationMade{
        .body = implementation.body_name,
        .made = hir::ScopeInstantiations(
            implementation, implementation.root_scope,
            specialization.construction)};
  }
};

// What each specialization of the design builds (LRM 23.3.2, 27), by its name.
// The design's own link-level unit states every specialization's tables from
// this, because which body is made at an instantiation depends on the instance
// being built and never on the code a referrer compiled.
struct DesignInstantiations {
  std::map<std::string, SpecializationMade, std::less<>> by_specialization;
};

}  // namespace lyra::lowering::hir_to_mir
