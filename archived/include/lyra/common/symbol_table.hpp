#pragma once

#include <cassert>
#include <vector>

#include "lyra/common/symbol_types.hpp"

namespace lyra {

class SymbolTable {
 public:
  auto Add(Symbol symbol) -> SymbolId {
    SymbolId id{static_cast<uint32_t>(symbols_.size())};
    symbols_.push_back(std::move(symbol));
    return id;
  }

  [[nodiscard]] auto operator[](SymbolId id) const -> const Symbol& {
    assert(id.value < symbols_.size());
    return symbols_[id.value];
  }

  [[nodiscard]] auto Size() const -> size_t {
    return symbols_.size();
  }

 private:
  std::vector<Symbol> symbols_;
};

}  // namespace lyra
