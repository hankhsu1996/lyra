#pragma once

#include <cstddef>
#include <optional>
#include <string>
#include <utility>
#include <vector>

#include "lyra/base/id_range.hpp"
#include "lyra/base/interner.hpp"

namespace lyra::hir {

// The identities one compilation unit gives the declarations of one kind it
// names -- a class, a covergroup, the class of a scope -- each under the pair
// every unit names it by: the declaring unit and the path it was published
// under (LRM 26.3). The first time a pair enters it is given an identity, which
// every later name of the same pair lands on, so one declaration is one
// identity here whichever unit declares it and however the name reaching it was
// written. This is the one place a pair becomes an identity, as rustc maps a
// crate number read from metadata to its own once, where it decodes it.
template <typename Key, typename Id>
class KeyedIdentities {
 public:
  explicit KeyedIdentities(std::string unit_name)
      : unit_name_(std::move(unit_name)) {
  }

  // The identity `key` has here, and whether it entered just now.
  auto Enter(Key key) -> std::pair<Id, bool> {
    if (const std::optional<Id> known = keys_.Find(key)) {
      return {*known, false};
    }
    return {keys_.Intern(std::move(key)), true};
  }

  // The identity `key` has here, where it has entered.
  [[nodiscard]] auto Find(const Key& key) const -> std::optional<Id> {
    return keys_.Find(key);
  }

  [[nodiscard]] auto KeyOf(Id id) const -> const Key& {
    return keys_.Get(id);
  }

  // Whether this unit declares what `id` names, rather than reaching it in
  // another.
  [[nodiscard]] auto IsOwn(Id id) const -> bool {
    return keys_.Get(id).unit_name == unit_name_;
  }

  [[nodiscard]] auto UnitName() const -> const std::string& {
    return unit_name_;
  }

  [[nodiscard]] auto Ids() const -> base::IdRange<Id> {
    return keys_.Ids();
  }

  [[nodiscard]] auto size() const -> std::size_t {
    return keys_.size();
  }

  [[nodiscard]] auto empty() const -> bool {
    return keys_.empty();
  }

  // What this unit declares, and what it reaches in other units, each in the
  // order it entered.
  [[nodiscard]] auto OwnIds() const -> std::vector<Id> {
    return Where(true);
  }
  [[nodiscard]] auto ImportedIds() const -> std::vector<Id> {
    return Where(false);
  }

  auto operator==(const KeyedIdentities&) const -> bool = default;

 private:
  [[nodiscard]] auto Where(bool own) const -> std::vector<Id> {
    std::vector<Id> ids;
    for (const Id id : Ids()) {
      if (IsOwn(id) == own) ids.push_back(id);
    }
    return ids;
  }

  std::string unit_name_;
  base::Interner<Key, Id, typename Key::Hash> keys_;
};

}  // namespace lyra::hir
