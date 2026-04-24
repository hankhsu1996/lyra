#pragma once

#include <string_view>

#include "lyra/common/symbol_types.hpp"

namespace lyra {

class NameResolver {
 public:
  NameResolver() = default;
  NameResolver(const NameResolver&) = default;
  NameResolver(NameResolver&&) = default;
  auto operator=(const NameResolver&) -> NameResolver& = default;
  auto operator=(NameResolver&&) -> NameResolver& = default;
  virtual ~NameResolver() = default;

  [[nodiscard]] virtual auto Name(SymbolId id) const -> std::string_view = 0;
  [[nodiscard]] virtual auto Kind(SymbolId id) const -> SymbolKind = 0;
};

}  // namespace lyra
