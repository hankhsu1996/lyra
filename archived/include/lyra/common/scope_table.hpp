#pragma once

#include <cassert>
#include <vector>

#include "lyra/common/scope_types.hpp"

namespace lyra {

class ScopeTable {
 public:
  auto Add(Scope scope) -> ScopeId {
    ScopeId id{static_cast<uint32_t>(scopes_.size())};
    scopes_.push_back(std::move(scope));
    return id;
  }

  [[nodiscard]] auto operator[](ScopeId id) const -> const Scope& {
    assert(id.value < scopes_.size());
    return scopes_[id.value];
  }

  [[nodiscard]] auto Size() const -> size_t {
    return scopes_.size();
  }

 private:
  std::vector<Scope> scopes_;
};

}  // namespace lyra
