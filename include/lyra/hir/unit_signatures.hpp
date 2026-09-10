#pragma once

#include <string>
#include <unordered_map>
#include <utility>

#include "lyra/base/internal_error.hpp"
#include "lyra/hir/unit_signature.hpp"

namespace lyra::hir {

// What every unit in the design publishes: filled by the declaration phase and
// read by the body phase. Holding them in one place is what makes the set of
// facts that crossed enumerable, rather than whatever the frontend's
// whole-design graph happened to reach.
//
// A unit is found by name, which is the identity a reference carries across the
// boundary; there is no shared table of ids to match on. Which units a unit
// ends up depending on is exactly the set of promises it read, so reading is
// the act that records a dependency -- a set decided in advance would be an
// approximation, and it cannot be one: a class first named inside a body is
// reached after any such set was fixed, and a name is on a promise or not
// whether or not the reader declared anything.
class UnitSignatures {
 public:
  // Records what one unit publishes, under the name the signature states. The
  // name cannot disagree with the signature it keys, because there is only one
  // of them.
  void Publish(UnitSignature signature) {
    std::string name = signature.unit_name;
    if (!by_name_.emplace(std::move(name), std::move(signature)).second) {
      throw InternalError(
          "UnitSignatures::Publish: two units of the design publish under one "
          "name; a specialization name identifies exactly one unit");
    }
  }

  // What the unit named `unit_name` publishes, or nullptr when the design
  // compiles no such unit. An absent answer means there is nothing to compile
  // against, which is the same answer a name the unit never published gets.
  [[nodiscard]] auto Find(const std::string& unit_name) const
      -> const UnitSignature* {
    const auto it = by_name_.find(unit_name);
    return it == by_name_.end() ? nullptr : &it->second;
  }

  // What the unit an instance is built from publishes. Every instance the
  // design holds belongs to a scope the design compiles, and compiling a scope
  // compiles the units it instantiates, so an instantiated name is always here
  // and the absence is a compiler bug rather than a case a caller handles.
  [[nodiscard]] auto Instantiated(const std::string& unit_name) const
      -> const UnitSignature& {
    const UnitSignature* signature = Find(unit_name);
    if (signature == nullptr) {
      throw InternalError(
          "UnitSignatures::Instantiated: no signature for an instantiated "
          "unit; the design compiles every unit it instantiates");
    }
    return *signature;
  }

  // The object an instance of `unit_name` is.
  [[nodiscard]] auto InstantiatedClass(const std::string& unit_name) const
      -> const InstanceClassSignature& {
    return InstanceClassOf(Instantiated(unit_name));
  }

 private:
  std::unordered_map<std::string, UnitSignature> by_name_;
};

}  // namespace lyra::hir
