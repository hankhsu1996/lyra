#pragma once

#include <string>
#include <unordered_map>
#include <utility>

#include "lyra/base/internal_error.hpp"
#include "lyra/hir/unit_signature.hpp"

namespace lyra::lowering::ast_to_hir {

// What every unit in the design publishes: filled by the declaration phase,
// read by the body phase, and the only route a lowering has to another unit.
// Holding them in one place is what makes the set of facts that crossed
// enumerable, rather than whatever the frontend's whole-design graph happened
// to reach.
//
// A unit is found by name, which is the identity a reference carries across the
// boundary -- there is no shared table of ids to match on, by design.
class UnitSignatures {
 public:
  // Records what one unit publishes, under the name the signature states. The
  // name cannot disagree with the signature it keys, because there is only one
  // of them.
  void Publish(hir::UnitSignature signature) {
    std::string name = signature.unit_name;
    if (!by_name_.emplace(std::move(name), std::move(signature)).second) {
      throw InternalError(
          "UnitSignatures::Publish: two units of the design publish under one "
          "name; a specialization name identifies exactly one unit");
    }
  }

  // What the unit named `unit_name` publishes, or nullptr when no unit in the
  // design declares that name.
  [[nodiscard]] auto Find(const std::string& unit_name) const
      -> const hir::UnitSignature* {
    const auto it = by_name_.find(unit_name);
    return it == by_name_.end() ? nullptr : &it->second;
  }

 private:
  std::unordered_map<std::string, hir::UnitSignature> by_name_;
};

}  // namespace lyra::lowering::ast_to_hir
