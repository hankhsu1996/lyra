#pragma once

#include <functional>
#include <map>
#include <string>

#include "lyra/hir/lowering.hpp"
#include "lyra/hir/scope_instantiations.hpp"

namespace lyra::lowering::hir_to_mir {

// One lowering of the design: the body its code is, and what each scope of its
// objects makes at each of its instantiations and each place of each of its
// generates.
struct LoweringMade {
  std::string body;
  hir::ScopeInstantiations made;

  // What `lowering` builds, read off it while its body is still held, since
  // only the first lowering of a body keeps it past HIR.
  [[nodiscard]] static auto Of(const hir::Lowering& lowering) -> LoweringMade {
    const hir::CompilationUnit& body = lowering.body;
    return LoweringMade{
        .body = body.body_name,
        .made = hir::ScopeInstantiations(
            body, body.root_scope, lowering.construction)};
  }
};

// What each lowering of the design builds (LRM 23.3.2, 27), by its name.
// The design's own link-level unit states every lowering's tables from
// this, because which body is made at an instantiation depends on the instance
// being built and never on the code a referrer compiled.
struct DesignInstantiations {
  std::map<std::string, LoweringMade, std::less<>> by_lowering;
};

}  // namespace lyra::lowering::hir_to_mir
