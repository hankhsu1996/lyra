#pragma once

#include <cstdint>
#include <optional>
#include <utility>
#include <vector>

#include "lyra/base/internal_error.hpp"

namespace lyra::base {

// A pool of `T` indexed by a typed id `Id`, where an identity is minted before
// its value exists: `Declare` mints an id whose value is absent, `Define` fills
// it exactly once, and `Get` reads a defined value. This serves entities that
// must be referable before they are complete -- a forward or mutually recursive
// declaration names another's identity before either body is built. `Id` is a
// struct carrying a single `std::uint32_t value`, its position in the pool; an
// identity, once minted, is stable for the pool's life.
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

  [[nodiscard]] auto IsDefined(Id id) const -> bool {
    return slots_.at(id.value).has_value();
  }

  [[nodiscard]] auto Get(Id id) const -> const T& {
    const auto& slot = slots_.at(id.value);
    if (!slot.has_value()) {
      throw InternalError(
          "Registry::Get: identity is declared but not defined");
    }
    return *slot;
  }

 private:
  std::vector<std::optional<T>> slots_;
};

}  // namespace lyra::base
