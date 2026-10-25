#pragma once

#include <compare>
#include <cstddef>
#include <functional>
#include <optional>
#include <string>
#include <utility>

#include "lyra/base/hash.hpp"
#include "lyra/base/id_range.hpp"
#include "lyra/base/registry.hpp"
#include "lyra/hir/keyed_identities.hpp"
#include "lyra/hir/published_scope.hpp"
#include "lyra/hir/scope_class_id.hpp"
#include "lyra/support/def_path.hpp"

namespace lyra::hir {

// The name every unit gives the class a scope of the design hierarchy is -- an
// instance of a unit, or a generate block inside one: the unit and the path it
// published the scope's class under, as a class's key is.
struct ScopeClassKey {
  std::string unit_name;
  support::DefPath class_path;

  auto operator<=>(const ScopeClassKey&) const
      -> std::strong_ordering = default;
  // Two equal keys display alike, which is all a pool needs of a hash; equality
  // decides which key is which.
  struct Hash {
    auto operator()(const ScopeClassKey& key) const -> std::size_t {
      std::size_t seed = std::hash<std::string>{}(key.unit_name);
      base::HashField(seed, support::DisplayOf(key.class_path));
      return seed;
    }
  };
};

// Every scope class one unit names, each under one identity, the first time
// its key enters: the ones this unit publishes of its own scopes, and those of
// other units it reaches. A class of this unit's own reached by a name --
// another instance of it, which a hierarchical name may reach (LRM 23.6) --
// enters with the key it published under, so it is one identity however it was
// reached.
class ScopeClassRegistry {
 public:
  explicit ScopeClassRegistry(std::string unit_name)
      : identities_(std::move(unit_name)) {
  }

  // The class this unit publishes its scopes at `path` as -- one for every
  // scope published under it.
  auto DeclareOwn(support::DefPath path) -> ScopeClassId {
    return Import(
        ScopeClassKey{
            .unit_name = identities_.UnitName(),
            .class_path = std::move(path)});
  }

  // The scope class `key` names, given an identity the first time it enters.
  auto Import(const ScopeClassKey& key) -> ScopeClassId {
    const auto [id, entered] = identities_.Enter(key);
    if (entered) {
      records_.Declare();
    }
    return id;
  }

  // What the unit defining the class published of it, every type it names
  // taken into this unit's pool, so nothing reading it reads a signature or a
  // type this unit does not own.
  void Record(ScopeClassId id, ScopeClassSignature record) {
    records_.Define(id, std::move(record));
  }

  [[nodiscard]] auto IsRecorded(ScopeClassId id) const -> bool {
    return records_.IsDefined(id);
  }

  [[nodiscard]] auto Get(ScopeClassId id) const -> const ScopeClassSignature& {
    return records_.Get(id);
  }

  [[nodiscard]] auto KeyOf(ScopeClassId id) const -> const ScopeClassKey& {
    return identities_.KeyOf(id);
  }

  // Whether this unit publishes the class, rather than reaching it in another.
  [[nodiscard]] auto IsOwn(ScopeClassId id) const -> bool {
    return identities_.IsOwn(id);
  }

  [[nodiscard]] auto Ids() const -> base::IdRange<ScopeClassId> {
    return identities_.Ids();
  }

  [[nodiscard]] auto size() const -> std::size_t {
    return identities_.size();
  }

  [[nodiscard]] auto empty() const -> bool {
    return identities_.empty();
  }

  auto operator==(const ScopeClassRegistry&) const -> bool = default;

 private:
  KeyedIdentities<ScopeClassKey, ScopeClassId> identities_;
  base::Registry<ScopeClassSignature, ScopeClassId> records_;
};

}  // namespace lyra::hir
