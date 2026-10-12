#pragma once

#include <functional>
#include <map>
#include <string>
#include <vector>

namespace lyra::lowering::hir_to_mir {

// What each unit of the design elaborated at its instantiations (LRM 23.3.2):
// for every unit an object is built of, the unit made at each of its
// instantiations, in the order its construction table lists them. The design's
// own link-level unit states every unit's table from this, because which unit
// is made at an instantiation depends on the instance being built and never on
// the code compiled for it.
struct DesignInstantiations {
  std::map<std::string, std::vector<std::string>, std::less<>> by_unit;
};

}  // namespace lyra::lowering::hir_to_mir
