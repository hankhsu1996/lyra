#pragma once

#include <functional>
#include <map>
#include <string>
#include <vector>

#include "lyra/hir/type_id.hpp"
#include "lyra/lowering/hir_to_mir/design_instantiations.hpp"
#include "lyra/lowering/hir_to_mir/design_namespaces.hpp"
#include "lyra/support/def_path.hpp"

namespace lyra::lowering::hir_to_mir {

// The object entry of a unit the design root's tables name, as that unit
// published it: the class an object of it is published as, which is what the
// entry answers an owning pointer to, and the type of each value the entry
// takes after where the object hangs, its segment and its construction table,
// in the design root's own types.
struct PublishedObjectEntry {
  support::DefPath published_class;
  std::vector<hir::TypeId> handed;
};

// What the design root is lowered with beside its own scope, and no source
// unit is: every namespace the root brings up, what every body elaborated at
// its instantiations, from which the root states every construction table, and
// the object entry of every unit those tables make, by the unit's name.
struct DesignRootInputs {
  DesignNamespaces namespaces;
  DesignInstantiations instantiations;
  std::map<std::string, PublishedObjectEntry, std::less<>> object_entries;
};

}  // namespace lyra::lowering::hir_to_mir
