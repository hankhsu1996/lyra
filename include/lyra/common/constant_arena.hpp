#pragma once

#include <vector>

#include "absl/container/flat_hash_map.h"
#include "absl/hash/hash.h"
#include "lyra/common/constant.hpp"

namespace lyra {

struct ConstantKeyHash {
  auto operator()(const Constant& key) const -> size_t {
    return absl::HashOf(key.type, key.value);
  }
};

class ConstantArena final {
 public:
  ConstantArena() = default;
  ~ConstantArena() = default;

  ConstantArena(const ConstantArena&) = delete;
  auto operator=(const ConstantArena&) -> ConstantArena& = delete;

  ConstantArena(ConstantArena&&) = default;
  auto operator=(ConstantArena&&) -> ConstantArena& = default;

  auto Intern(TypeId type, ConstantValue value) -> ConstId;

  [[nodiscard]] auto operator[](ConstId id) const -> const Constant&;

 private:
  std::vector<Constant> constants_;
  absl::flat_hash_map<Constant, ConstId, ConstantKeyHash> map_;
};

}  // namespace lyra
