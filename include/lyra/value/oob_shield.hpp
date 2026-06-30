#pragma once

#include <utility>

namespace lyra::value::detail {

// The out-of-bounds shield a variable-shaped container carries. `Default()` is
// the element-type canonical default (LRM Table 7-1) an invalid-index read
// returns and every fill / slice / new slot copies; it is only ever read, so it
// stays canonical with no scrub. `DiscardTarget()` is the throwaway an
// invalid-index write lands on (LRM 7.4.5), scrubbed to canonical before each
// use so a discarded write never leaks into a later access. Both slots carry
// the element's runtime shape -- for `T = PackedArray`, the bit width,
// signedness, and state domain the C++ type alone cannot recover -- supplied at
// construction.
template <typename T>
class OobShield {
 public:
  OobShield() = default;
  explicit OobShield(T element_default)
      : default_(element_default), discard_sink_(std::move(element_default)) {
  }

  [[nodiscard]] auto Default() const -> const T& {
    return default_;
  }

  [[nodiscard]] auto DiscardTarget() -> T& {
    discard_sink_.ResetToDefault();
    return discard_sink_;
  }

 private:
  T default_{};
  T discard_sink_{};
};

}  // namespace lyra::value::detail
