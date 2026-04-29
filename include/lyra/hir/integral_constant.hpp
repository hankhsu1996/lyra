#pragma once

#include <cstdint>
#include <vector>

#include "lyra/hir/type.hpp"

namespace lyra::hir {

enum class IntegralStateKind : std::uint8_t {
  kTwoState,
  kFourState,
};

// Word layout is LSB-first; top word's unused high bits are zero-masked.
// state_words is empty for 2-state, otherwise same length as value_words.
// 4-state encoding: (v=0,s=0)=0, (v=1,s=0)=1, (v=0,s=1)=Z, (v=1,s=1)=X.
struct IntegralConstant {
  std::vector<std::uint64_t> value_words;
  std::vector<std::uint64_t> state_words;
  std::uint32_t width = 0;
  Signedness signedness = Signedness::kUnsigned;
  IntegralStateKind state_kind = IntegralStateKind::kTwoState;

  auto operator==(const IntegralConstant&) const -> bool = default;
};

}  // namespace lyra::hir
