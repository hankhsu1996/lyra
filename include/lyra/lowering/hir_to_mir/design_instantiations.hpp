#pragma once

#include <functional>
#include <map>
#include <string>

#include "lyra/hir/scope_instantiations.hpp"

namespace lyra::lowering::hir_to_mir {

// What each compiled body of the design elaborated at its instantiations (LRM
// 23.3.2): for every body an object is built of, what each scope of such an
// object makes at each of its instantiations, in the order their construction
// tables list them. The design's own link-level unit states every body's
// tables from this, because which body is made at an instantiation depends on
// the instance being built and never on the code a referrer compiled.
struct DesignInstantiations {
  std::map<std::string, hir::ScopeInstantiations, std::less<>> by_body;
};

}  // namespace lyra::lowering::hir_to_mir
