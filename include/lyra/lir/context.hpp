#pragma once

#include <string>
#include <string_view>
#include <unordered_set>
#include <vector>

#include "lyra/common/literal.hpp"

namespace lyra::lir {

struct TempSymbol {
  std::string name;
};

struct LabelRef {
  const std::string* ptr;
};

struct TempRef {
  TempSymbol* ptr;
};

struct LiteralRef {
  const common::Literal* ptr;

  [[nodiscard]] auto operator*() const -> const common::Literal& {
    return *ptr;
  }
  [[nodiscard]] auto operator->() const -> const common::Literal* {
    return ptr;
  }
};

class LirContext {
 public:
  auto AllocateTemp(std::string name) -> TempRef;
  auto InternLabel(std::string_view name) -> LabelRef;
  auto InternLiteral(const common::Literal& literal) -> LiteralRef;

 private:
  std::vector<TempSymbol> temp_storage_;
  std::unordered_set<std::string> label_pool_;
  std::vector<common::Literal> literal_storage_;

  struct LiteralPtrHash {
    auto operator()(const common::Literal* ptr) const -> std::size_t;
  };

  struct LiteralPtrEqual {
    auto operator()(const common::Literal* a, const common::Literal* b) const
        -> bool;
  };

  std::unordered_set<const common::Literal*, LiteralPtrHash, LiteralPtrEqual>
      literal_set_;
};

}  // namespace lyra::lir
