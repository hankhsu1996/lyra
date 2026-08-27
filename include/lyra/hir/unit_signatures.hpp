#pragma once

#include <span>
#include <string>
#include <unordered_map>
#include <utility>

#include "lyra/base/internal_error.hpp"
#include "lyra/hir/unit_signature.hpp"

namespace lyra::hir {

class UnitSignatures;

// The signatures one unit may read, being those of the units its own
// declarations name. Anything else is absent, so what a unit's lowering can
// learn about the rest of the design is bounded by what the unit itself
// declared, rather than by what the design happens to contain.
//
// A unit is found by name, which is the identity a reference carries across the
// boundary; there is no shared table of ids to match on.
class ConsumedSignatures {
 public:
  // What the unit named `unit_name` publishes, or nullptr when this unit
  // declared no dependency on it or the design compiles no such unit. Both
  // answers mean the same thing to a referrer: nothing to compile against.
  [[nodiscard]] auto Find(const std::string& unit_name) const
      -> const UnitSignature* {
    const auto it = by_name_.find(unit_name);
    return it == by_name_.end() ? nullptr : it->second;
  }

  // What the unit an instance is built from publishes. Instantiating a unit is
  // how a dependency on it gets declared, so its signature is always in reach
  // and the absence is a compiler bug rather than a case a caller handles.
  [[nodiscard]] auto Instantiated(const std::string& unit_name) const
      -> const UnitSignature& {
    const UnitSignature* signature = Find(unit_name);
    if (signature == nullptr) {
      throw InternalError(
          "ConsumedSignatures::Instantiated: no signature for an instantiated "
          "unit; instantiating one is what declares the dependency, so it is "
          "always among these");
    }
    return *signature;
  }

  // The object an instance of `unit_name` is.
  [[nodiscard]] auto InstantiatedClass(const std::string& unit_name) const
      -> const InstanceClassSignature& {
    return InstanceClassOf(Instantiated(unit_name));
  }

 private:
  friend class UnitSignatures;

  explicit ConsumedSignatures(
      std::unordered_map<std::string, const UnitSignature*> by_name)
      : by_name_(std::move(by_name)) {
  }

  std::unordered_map<std::string, const UnitSignature*> by_name_;
};

// What every unit in the design publishes: filled by the declaration phase and
// read by the body phase. Holding them in one place is what makes the set of
// facts that crossed enumerable, rather than whatever the frontend's
// whole-design graph happened to reach.
//
// A reader names the units it depends on to get at any of them, so no lowering
// holds the whole set while it runs.
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

  // The signatures a unit naming `unit_names` as its dependencies may read. A
  // name the design compiles no unit for contributes no entry rather than
  // failing: a reference to it has nothing to compile against, which is the
  // same answer an unpublished name gets.
  [[nodiscard]] auto Consumed(std::span<const std::string> unit_names) const
      -> ConsumedSignatures {
    std::unordered_map<std::string, const UnitSignature*> consumed;
    consumed.reserve(unit_names.size());
    for (const std::string& unit_name : unit_names) {
      if (const auto it = by_name_.find(unit_name); it != by_name_.end()) {
        consumed.emplace(unit_name, &it->second);
      }
    }
    return ConsumedSignatures(std::move(consumed));
  }

 private:
  std::unordered_map<std::string, UnitSignature> by_name_;
};

}  // namespace lyra::hir
