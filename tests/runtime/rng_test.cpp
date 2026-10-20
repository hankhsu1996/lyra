#include "lyra/runtime/rng.hpp"

#include <array>
#include <cstdint>
#include <gtest/gtest.h>
#include <optional>
#include <string>
#include <string_view>
#include <tuple>

namespace {

using lyra::runtime::DrawRng;
using lyra::runtime::RandomSeed;

auto Draws(DrawRng rng) -> std::array<std::uint32_t, 4> {
  return {rng.NextValue(), rng.NextValue(), rng.NextValue(), rng.NextValue()};
}

// LRM 18.13.4 and 18.13.5: a state read out and put back makes the generator
// draw what it would have drawn from where the state was read, whether it was
// seeded or split from another; the text names the generator, so two that draw
// differently read out differently.
TEST(DrawRngTest, AStateReadOutAndPutBackReplaysTheDraws) {
  DrawRng seeded{RandomSeed{7}};
  std::ignore = seeded.NextValue();
  DrawRng parent{RandomSeed{3}};
  const DrawRng split = parent.Split();
  for (const DrawRng& rng : {seeded, split}) {
    const std::string state = rng.StateText();
    const auto restored = DrawRng::FromStateText(state);
    ASSERT_TRUE(restored.has_value());
    EXPECT_EQ(Draws(*restored), Draws(rng));
    EXPECT_EQ(restored->StateText(), state);
  }
  EXPECT_NE(
      DrawRng{RandomSeed{1}}.StateText(), DrawRng{RandomSeed{2}}.StateText());
}

// LRM 18.13.5 leaves a state this implementation never produced undefined; a
// text is read back only where it names a state of this generator, and every
// other text is refused rather than turned into some generator.
TEST(DrawRngTest, OnlyATextNamingAStateIsReadBack) {
  const std::string valid = DrawRng{RandomSeed{5}}.StateText();
  ASSERT_TRUE(DrawRng::FromStateText(valid).has_value());
  EXPECT_TRUE(DrawRng::FromStateText("18446744073709551615 0 1 0").has_value());

  constexpr std::array<std::string_view, 12> kNotAState = {
      "",
      "1 2 3",
      "1 2 3 4 5",
      "x 2 3 4",
      "1 2 3 4 ",
      " 1 2 3 4",
      "1  2 3 4",
      "-1 2 3 4",
      "+1 2 3 4",
      "18446744073709551616 2 3 4",
      // The congruential addend is odd in every generator this one makes.
      "2 2 3 4",
      // The xoroshiro half is never all zero.
      "1 2 0 0",
  };
  for (const std::string_view text : kNotAState) {
    EXPECT_FALSE(DrawRng::FromStateText(text).has_value())
        << "'" << text << "'";
  }
}

}  // namespace
