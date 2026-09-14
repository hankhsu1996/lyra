#pragma once

#include <string>
#include <vector>

namespace lyra::lowering::hir_to_mir {

// The namespace units of the design, in name order: every unit that roots no
// object, which is what a package and the `$unit` file-set scope are (LRM 26.2,
// 3.12.1). The design's own link-level unit brings all of them up, because a
// namespace's variable declaration assignments must have run before any
// procedure starts whether or not anything reads them.
//
// Names and nothing else. Which of them has to be initialized before which is
// not here and is not anyone's to hold: each namespace's initialize entry
// claims its one bring-up and calls the entries of the namespaces its own
// initializers read, so the order LRM 26.2 leaves open is those calls executed.
struct DesignNamespaces {
  std::vector<std::string> units;
};

}  // namespace lyra::lowering::hir_to_mir
