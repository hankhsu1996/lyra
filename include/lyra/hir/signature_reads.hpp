#pragma once

#include <set>
#include <string>
#include <variant>

namespace lyra::hir {

// A unit's signature: what it declares for another unit to name.
struct UnitSignatureRead {
  std::string unit_name;

  auto operator<=>(const UnitSignatureRead&) const = default;
};

// What one lowering of a unit holds for another unit to read, under the
// lowering's name: a package's parameter values.
struct ValuesRead {
  std::string lowering;

  auto operator<=>(const ValuesRead&) const = default;
};

// Which units the design holds. Reading which namespaces there are reads the
// set, so a namespace added or removed is a change to what was read, though no
// signature read changed.
struct UnitListRead {
  auto operator<=>(const UnitListRead&) const = default;
};

using ReadOfAnotherUnit =
    std::variant<UnitSignatureRead, ValuesRead, UnitListRead>;

// What one lowering read of what other units publish: the inputs its result
// depends on besides its own unit's text. A read is recorded when it is
// answered, so a fact the lowering learned and then wrote nothing of -- that
// no class of a lineage declares a hook -- is among them. Ordered, so it is
// written out the same way each time.
using SignatureReads = std::set<ReadOfAnotherUnit>;

}  // namespace lyra::hir
