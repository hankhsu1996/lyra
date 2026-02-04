#include <cstdint>
#include <span>
#include <string>
#include <string_view>

#include "lyra/common/plusargs.hpp"
#include "lyra/runtime/engine.hpp"

namespace lyra::runtime {

auto Engine::TestPlusargs(std::string_view query) const -> int32_t {
  return common::TestPlusargs(plusargs_, query);
}

auto Engine::ValuePlusargsInt(std::string_view format, int32_t* output) const
    -> int32_t {
  return common::MatchPlusargsInt(plusargs_, format, output);
}

auto Engine::ValuePlusargsString(
    std::string_view format, std::string* output) const -> int32_t {
  return common::MatchPlusargsString(plusargs_, format, output);
}

// LCG with glibc constants: a=1103515245, c=12345, m=2^31
// Returns unsigned 32-bit value (advances state).
auto Engine::Urandom() -> uint32_t {
  constexpr uint32_t kMultiplier = 1103515245;
  constexpr uint32_t kIncrement = 12345;
  prng_state_ = prng_state_ * kMultiplier + kIncrement;
  return prng_state_;
}

// $random returns signed 32-bit value.
auto Engine::Random() -> int32_t {
  return static_cast<int32_t>(Urandom());
}

}  // namespace lyra::runtime
