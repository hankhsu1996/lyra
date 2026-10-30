#pragma once

#include <algorithm>
#include <format>
#include <string>
#include <unordered_map>
#include <utility>
#include <variant>
#include <vector>

#include "lyra/base/internal_error.hpp"
#include "lyra/hir/specialization_values.hpp"
#include "lyra/hir/type.hpp"
#include "lyra/hir/unit_signature.hpp"
#include "lyra/support/def_path.hpp"

namespace lyra::hir {

// What every unit in the design publishes, and the values each of its
// specializations holds: filled by the declaration phase and read by the body
// phase. Holding them in one place is what makes the set of facts that crossed
// enumerable, rather than whatever the frontend's whole-design graph happened
// to reach.
//
// A unit is found by name, and a specialization's values by the
// specialization's name, which is the identity a reference carries across the
// boundary; there is no shared table of ids to match on. Which units a unit
// ends up depending on is read off what it compiled to rather than decided in
// advance -- a set decided in advance would be an approximation, and it
// cannot be one: a class first named inside a body is
// reached after any such set was fixed, and a name is on a signature or not
// whether or not the reader declared anything.
class UnitSignatures {
 public:
  // Records what one specialization of a unit publishes, under the unit's
  // name. The first one the design declares publishes for the unit, and each
  // later one adds the generate blocks its objects built that none before it
  // did.
  void PublishSpecialization(UnitSignature signature) {
    const auto published = by_name_.find(signature.unit_name);
    if (published == by_name_.end()) {
      std::string name = signature.unit_name;
      by_name_.emplace(std::move(name), std::move(signature));
      return;
    }
    PublishBlocksBuiltBy(published->second, signature);
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
          std::format(
              "UnitSignatures::Instantiated: no signature for the instantiated "
              "unit '{}'; the design compiles every unit it instantiates",
              unit_name));
    }
    return *signature;
  }

  // What the unit declaring `declaration` publishes. A unit names a declared
  // type only of a unit the design compiles, so the absence is a compiler bug.
  [[nodiscard]] auto Declaring(const TypeDeclarationRef& declaration) const
      -> const UnitSignature& {
    const UnitSignature* signature = Find(declaration.unit_name);
    if (signature == nullptr) {
      throw InternalError(
          std::format(
              "UnitSignatures::Declaring: no signature of '{}', which declares "
              "'{}'; the design compiles every unit a type names",
              declaration.unit_name, support::DisplayOf(declaration.path)));
    }
    return *signature;
  }

  // The units of the design that root no object, in name order. A namespace is
  // exactly such a unit (LRM 26.2), and a signature says which it is, so the
  // one artifact that has to bring every namespace up reads the set off the
  // signatures it already consumes. Sorted because the set is walked to emit
  // calls, and a design compiles to the same program each time.
  [[nodiscard]] auto NamespaceUnitNames() const -> std::vector<std::string> {
    std::vector<std::string> names;
    for (const auto& [name, signature] : by_name_) {
      if (std::holds_alternative<PublishedNamespace>(signature.unit)) {
        names.push_back(name);
      }
    }
    std::ranges::sort(names);
    return names;
  }

  // Records what the specialization named `specialization` holds.
  void PublishValues(std::string specialization, SpecializationValues values) {
    if (!values_.emplace(std::move(specialization), std::move(values)).second) {
      throw InternalError(
          "UnitSignatures::PublishValues: one specialization publishes its "
          "values once");
    }
  }

  // What the specialization named `specialization` holds. Every
  // specialization of the design publishes, so an absent one is a compiler
  // bug.
  [[nodiscard]] auto ValuesOf(const std::string& specialization) const
      -> const SpecializationValues& {
    const auto it = values_.find(specialization);
    if (it == values_.end()) {
      throw InternalError(
          std::format(
              "UnitSignatures::ValuesOf: specialization '{}' published no "
              "values; every specialization of the design publishes",
              specialization));
    }
    return it->second;
  }

 private:
  std::unordered_map<std::string, UnitSignature> by_name_;
  std::unordered_map<std::string, SpecializationValues> values_;
};

}  // namespace lyra::hir
