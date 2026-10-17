#pragma once

#include <cstdint>

namespace lyra::hir {

// LRM A.6.7 `case_keyword`: how a case statement compares two values. `case`
// compares every bit exactly, an x or a z matching only itself (LRM 12.5);
// `casez` skips a z on either side and `casex` an x or a z (LRM 12.5.1). The
// patterns of a pattern-matching case statement compare every pair of bits,
// tag bits and members alike, the same way (LRM 12.6.1).
enum class CaseKeyword : std::uint8_t {
  kCase,
  kCasez,
  kCasex,
};

}  // namespace lyra::hir
