#pragma once

#include <algorithm>
#include <format>
#include <string>
#include <unordered_map>
#include <utility>
#include <variant>
#include <vector>

#include "lyra/base/internal_error.hpp"
#include "lyra/hir/lowering_values.hpp"
#include "lyra/hir/signature_reads.hpp"
#include "lyra/hir/type.hpp"
#include "lyra/hir/unit_signature.hpp"
#include "lyra/support/def_path.hpp"

namespace lyra::hir {

// What every unit in the design publishes, and the values each of its
// lowerings holds: filled by the declaration phase and read by the body
// phase. Holding them in one place is what makes the set of facts that crossed
// enumerable, rather than whatever the frontend's whole-design graph happened
// to reach.
//
// A unit is found by name, which is the identity a reference carries across
// the boundary, and a lowering's values by the lowering's name, the place of
// the instance it was lowered for, which is what a construction names it by;
// there is no shared table of ids to match on.
//
// Only the reader below reads it, and a reader records every read in the
// record of the lowering it serves, so what a lowering depends on is what it
// took, recorded as it took it, as rustc records an edge where a query reads.
// The record is taken at the read because a read can decide what a construct
// means and leave nothing of the unit it read in what is written.
class UnitSignatures {
 public:
  // Records what one lowering of a unit publishes, under the unit's name. The
  // first one the design declares publishes for the unit, and each later one
  // adds the generate blocks its objects built that none before it did.
  void PublishSignature(UnitSignature signature) {
    const auto published = by_name_.find(signature.unit_name);
    if (published == by_name_.end()) {
      std::string name = signature.unit_name;
      by_name_.emplace(std::move(name), std::move(signature));
      return;
    }
    PublishBlocksBuiltBy(published->second, signature);
  }

  // Records the values the lowering named `lowering` holds.
  void PublishValues(std::string lowering, LoweringValues values) {
    if (!values_.emplace(std::move(lowering), std::move(values)).second) {
      throw InternalError(
          "UnitSignatures::PublishValues: one lowering publishes its "
          "values once");
    }
  }

 private:
  friend class SignatureReader;

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
  // signatures it already reads. Sorted because the set is walked to emit
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

  // The values the lowering named `lowering` holds. Every lowering of the
  // design publishes, so an absent one is a compiler bug.
  [[nodiscard]] auto ValuesOf(const std::string& lowering) const
      -> const LoweringValues& {
    const auto it = values_.find(lowering);
    if (it == values_.end()) {
      throw InternalError(
          std::format(
              "UnitSignatures::ValuesOf: lowering '{}' published no "
              "values; every lowering of the design publishes",
              lowering));
    }
    return it->second;
  }

  std::unordered_map<std::string, UnitSignature> by_name_;
  std::unordered_map<std::string, LoweringValues> values_;
};

// The one way a lowering reads what the design's units publish. Each answer is
// recorded in `reads` as it is given, whatever the lowering then makes of it,
// and a read of the reading unit's own signature records nothing, since a unit
// depends on its own text anyway. Recording changes nothing the lowering
// computes, so it is done through a reader the lowering holds as const.
class SignatureReader {
 public:
  SignatureReader(
      const UnitSignatures& signatures, std::string reading_unit,
      SignatureReads& reads)
      : SignatureReader(signatures, std::move(reading_unit), &reads) {
  }

  // A reader whose reads nothing records, for a computation made from the
  // whole design and so computed again on every build whatever it read, as
  // rustc names a region that reads without recording.
  [[nodiscard]] static auto Untracked(
      const UnitSignatures& signatures, std::string reading_unit)
      -> SignatureReader {
    return {signatures, std::move(reading_unit), nullptr};
  }

  [[nodiscard]] auto Find(const std::string& unit_name) const
      -> const UnitSignature* {
    ReadSignatureOf(unit_name);
    return signatures_->Find(unit_name);
  }

  [[nodiscard]] auto Instantiated(const std::string& unit_name) const
      -> const UnitSignature& {
    ReadSignatureOf(unit_name);
    return signatures_->Instantiated(unit_name);
  }

  [[nodiscard]] auto Declaring(const TypeDeclarationRef& declaration) const
      -> const UnitSignature& {
    ReadSignatureOf(declaration.unit_name);
    return signatures_->Declaring(declaration);
  }

  [[nodiscard]] auto NamespaceUnitNames() const -> std::vector<std::string> {
    Record(UnitListRead{});
    return signatures_->NamespaceUnitNames();
  }

  [[nodiscard]] auto ValuesOf(const std::string& lowering) const
      -> const LoweringValues& {
    Record(ValuesRead{.lowering = lowering});
    return signatures_->ValuesOf(lowering);
  }

 private:
  SignatureReader(
      const UnitSignatures& signatures, std::string reading_unit,
      SignatureReads* reads)
      : signatures_(&signatures),
        reading_unit_(std::move(reading_unit)),
        reads_(reads) {
  }

  void ReadSignatureOf(const std::string& unit_name) const {
    if (unit_name != reading_unit_) {
      Record(UnitSignatureRead{.unit_name = unit_name});
    }
  }

  void Record(ReadOfAnotherUnit read) const {
    if (reads_ != nullptr) reads_->insert(std::move(read));
  }

  const UnitSignatures* signatures_;
  std::string reading_unit_;
  // Where reads are recorded, or nothing for an untracked reader.
  SignatureReads* reads_;
};

}  // namespace lyra::hir
