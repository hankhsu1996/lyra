#pragma once

#include <functional>
#include <map>
#include <string>
#include <vector>

#include "lyra/hir/unit_body_ref.hpp"

namespace lyra::lowering::hir_to_mir {

// What each compiled body of the design elaborated at its instantiations (LRM
// 23.3.2): for every body an object is built of, the body made at each of its
// instantiations, in the order its construction table lists them. The design's
// own link-level unit states every body's table from this, because which body
// is made at an instantiation depends on the instance being built and never on
// the code a referrer compiled.
struct DesignInstantiations {
  std::map<std::string, std::vector<hir::UnitBodyRef>, std::less<>> by_body;
};

}  // namespace lyra::lowering::hir_to_mir
