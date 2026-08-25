#pragma once

#include <cstddef>
#include <cstdint>
#include <optional>
#include <utility>
#include <vector>

#include "lyra/base/id_range.hpp"
#include "lyra/base/internal_error.hpp"

namespace lyra::base {

// A pool of `T` indexed by a typed id `Id`, where an identity may be minted
// before its value exists: `Declare` mints an id whose value is absent,
// `Define` fills it exactly once, and `Get` reads a defined value. This serves
// entities that must be referable before they are complete -- a forward or
// mutually recursive declaration names another's identity before either body is
// built. `Id` is a struct carrying a single `std::uint32_t value`, its position
// in the pool; an identity, once minted, is stable for the pool's life.
//
// Whether an identity may precede its value is a property of the entity, so a
// pool holding entities of both kinds answers yes and offers both forms: `Add`
// is the degenerate one, minting identity and value together for an entity
// nothing names early. It composes the other two and relaxes nothing -- every
// id still resolves to a value written exactly once.
//
// The gap is a cost, not a feature. A pool whose entities are all named only
// after they are complete wants `base::Arena` instead; reach for this one when
// at least one entity has to be referable before its value exists.
template <typename T, typename Id>
class Registry {
 public:
  auto Declare() -> Id {
    const Id id{static_cast<std::uint32_t>(slots_.size())};
    slots_.emplace_back(std::nullopt);
    return id;
  }

  void Define(Id id, T value) {
    auto& slot = slots_.at(id.value);
    if (slot.has_value()) {
      throw InternalError("Registry::Define: identity already defined");
    }
    slot = std::move(value);
  }

  // Mints an identity already carrying its value, for an entity no peer names
  // before it exists. Equivalent to `Declare` followed by `Define`, stated in
  // one step so a site with nothing to reserve does not read as if it had.
  auto Add(T value) -> Id {
    const Id id = Declare();
    Define(id, std::move(value));
    return id;
  }

  // A question only a caller running mid-construction has: once the pass that
  // builds a registry finishes, every declared identity is defined, so a
  // consumer walks the whole range and reads each entry -- which reports a
  // broken invariant rather than letting the consumer skip silently past it.
  [[nodiscard]] auto IsDefined(Id id) const -> bool {
    return slots_.at(id.value).has_value();
  }

  [[nodiscard]] auto size() const -> std::size_t {
    return slots_.size();
  }

  // The identities this registry has handed out, whether or not each is defined
  // yet. Reading them from here is what keeps a walk from rebuilding an id out
  // of its own loop counter.
  [[nodiscard]] auto Ids() const -> IdRange<Id> {
    return IdRange<Id>{static_cast<std::uint32_t>(slots_.size())};
  }

  [[nodiscard]] auto Get(Id id) const -> const T& {
    const auto& slot = slots_.at(id.value);
    if (!slot.has_value()) {
      throw InternalError(
          "Registry::Get: identity is declared but not defined");
    }
    return *slot;
  }

  // Walks the values in identity order. A registry is read only once the pass
  // that builds it has finished, when every declared identity is defined, so
  // reaching an undefined one reports the broken invariant rather than letting
  // the walk skip silently past it.
  class Iterator {
   public:
    Iterator(const Registry* registry, std::uint32_t index)
        : registry_(registry), index_(index) {
    }

    auto operator*() const -> const T& {
      return registry_->Get(Id{index_});
    }

    auto operator++() -> Iterator& {
      ++index_;
      return *this;
    }

    auto operator==(const Iterator& other) const -> bool {
      return index_ == other.index_;
    }

   private:
    const Registry* registry_;
    std::uint32_t index_;
  };

  [[nodiscard]] auto begin() const -> Iterator {
    return Iterator{this, 0};
  }

  [[nodiscard]] auto end() const -> Iterator {
    return Iterator{this, static_cast<std::uint32_t>(slots_.size())};
  }

 private:
  std::vector<std::optional<T>> slots_;
};

}  // namespace lyra::base
