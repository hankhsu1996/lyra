#include "lyra/runtime/constraint_sampler.hpp"

#include <algorithm>
#include <array>
#include <cmath>
#include <cstddef>
#include <cstdint>
#include <gtest/gtest.h>
#include <map>
#include <optional>
#include <span>
#include <string>
#include <vector>

#include "lyra/runtime/constraint_circuit.hpp"
#include "lyra/runtime/rng.hpp"

namespace {

using lyra::runtime::CircuitBits;
using lyra::runtime::CircuitLiteral;
using lyra::runtime::ConstraintCircuit;
using lyra::runtime::ConstraintSampler;
using lyra::runtime::DrawnVariable;
using lyra::runtime::DrawRng;
using lyra::runtime::kFalseLiteral;
using lyra::runtime::RandomSeed;

auto Inputs(ConstraintCircuit& c, std::size_t width) -> CircuitBits {
  CircuitBits out;
  for (std::size_t i = 0; i < width; ++i) {
    out.push_back(c.Input());
  }
  return out;
}

auto ConstantOf(std::uint64_t value, std::size_t width) -> CircuitBits {
  std::vector<bool> bits;
  for (std::size_t i = 0; i < width; ++i) {
    bits.push_back(((value >> i) & 1U) != 0);
  }
  return ConstraintCircuit::Constant(bits);
}

auto ValueOf(
    const ConstraintCircuit& c, const CircuitBits& bits,
    const std::vector<bool>& inputs) -> std::uint64_t {
  std::uint64_t out = 0;
  for (std::size_t i = 0; i < bits.size(); ++i) {
    if (c.Evaluate(bits[i], inputs)) {
      out |= std::uint64_t{1} << i;
    }
  }
  return out;
}

auto AllInputs(const ConstraintCircuit& c) -> std::vector<std::uint32_t> {
  std::vector<std::uint32_t> out;
  for (std::uint32_t i = 0; i < c.InputCount(); ++i) {
    out.push_back(i);
  }
  return out;
}

auto SignedOf(std::uint64_t value, std::size_t width) -> std::int64_t {
  const std::uint64_t sign = std::uint64_t{1} << (width - 1);
  return static_cast<std::int64_t>(value ^ sign) -
         static_cast<std::int64_t>(sign);
}

// The arithmetic a circuit states agrees with the machine's at every pair of
// four-bit values, signed and unsigned (LRM 11.4.2, 11.4.4, 11.4.10).
TEST(ConstraintCircuitTest, ArithmeticAgreesWithTheMachineAtEveryValue) {
  constexpr std::size_t kWidth = 4;
  constexpr std::uint64_t kMask = (1U << kWidth) - 1;
  ConstraintCircuit c;
  const CircuitBits a = Inputs(c, kWidth);
  const CircuitBits b = Inputs(c, kWidth);
  const CircuitBits sum = c.Add(a, b);
  const CircuitBits difference = c.Sub(a, b);
  const CircuitBits product = c.Mul(a, b);
  const CircuitBits quotient = c.Div(a, b, false);
  const CircuitBits remainder = c.Rem(a, b, false);
  const CircuitBits signed_quotient = c.Div(a, b, true);
  const CircuitBits signed_remainder = c.Rem(a, b, true);
  const CircuitBits left = c.ShiftLeft(a, b);
  const CircuitBits right = c.ShiftRight(a, b, false);
  const CircuitBits arithmetic = c.ShiftRight(a, b, true);
  const CircuitLiteral less = c.Less(a, b, false);
  const CircuitLiteral signed_less = c.Less(a, b, true);
  for (std::uint64_t x = 0; x <= kMask; ++x) {
    for (std::uint64_t y = 0; y <= kMask; ++y) {
      std::vector<bool> in;
      for (std::size_t i = 0; i < kWidth; ++i) {
        in.push_back(((x >> i) & 1U) != 0);
      }
      for (std::size_t i = 0; i < kWidth; ++i) {
        in.push_back(((y >> i) & 1U) != 0);
      }
      const std::int64_t sx = SignedOf(x, kWidth);
      const std::int64_t sy = SignedOf(y, kWidth);
      EXPECT_EQ(ValueOf(c, sum, in), (x + y) & kMask);
      EXPECT_EQ(ValueOf(c, difference, in), (x - y) & kMask);
      EXPECT_EQ(ValueOf(c, product, in), (x * y) & kMask);
      EXPECT_EQ(ValueOf(c, quotient, in), y == 0 ? 0 : x / y);
      EXPECT_EQ(ValueOf(c, remainder, in), y == 0 ? 0 : x % y);
      EXPECT_EQ(
          ValueOf(c, signed_quotient, in),
          y == 0 ? 0 : static_cast<std::uint64_t>(sx / sy) & kMask);
      EXPECT_EQ(
          ValueOf(c, signed_remainder, in),
          y == 0 ? 0 : static_cast<std::uint64_t>(sx % sy) & kMask);
      EXPECT_EQ(ValueOf(c, left, in), y >= kWidth ? 0 : (x << y) & kMask);
      EXPECT_EQ(ValueOf(c, right, in), y >= kWidth ? 0 : x >> y);
      EXPECT_EQ(
          ValueOf(c, arithmetic, in),
          static_cast<std::uint64_t>(sx >> std::min<std::uint64_t>(y, 63)) &
              kMask);
      EXPECT_EQ(c.Evaluate(less, in), x < y);
      EXPECT_EQ(c.Evaluate(signed_less, in), sx < sy);
    }
  }
}

// Whether `counts`, drawn from `solutions` equally likely values, are as even
// as a uniform draw makes them: Pearson's statistic below the value a uniform
// draw exceeds about once in a million runs (the Wilson-Hilferty
// approximation of the chi-square quantile).
auto LooksUniform(
    const std::map<std::vector<bool>, int>& counts, std::size_t solutions,
    int draws) -> bool {
  const double expected =
      static_cast<double>(draws) / static_cast<double>(solutions);
  double statistic = static_cast<double>(solutions - counts.size()) * expected;
  for (const auto& [value, count] : counts) {
    const double off = static_cast<double>(count) - expected;
    statistic += off * off / expected;
  }
  const double df = static_cast<double>(solutions) - 1;
  constexpr double kZ = 4.75;
  const double t = 1 - 2 / (9 * df) + kZ * std::sqrt(2 / (9 * df));
  return statistic < df * t * t * t;
}

// Each `width`-bit variable of `inputs`, in order, as one a draw chooses.
auto VariablesOf(const std::vector<std::uint32_t>& inputs, std::size_t width)
    -> std::vector<DrawnVariable> {
  std::vector<DrawnVariable> out;
  for (std::size_t at = 0; at < inputs.size(); at += width) {
    out.push_back(
        DrawnVariable{
            .inputs = std::span(inputs).subspan(at, width), .members = {}});
  }
  return out;
}

struct Shape {
  const char* name;
  std::size_t solutions;
  std::size_t width;
  CircuitLiteral (*build)(ConstraintCircuit&);
};

// x < y, where the solutions are many and alike; the LRM's own `s -> d == 0`
// drawn whole, where one solution stands alone beside a run of them; and an
// easy region beside a few scattered solutions, where a solver reaches some
// values first.
const std::array kShapes{
    Shape{
        .name = "x < y",
        .solutions = 28,
        .width = 3,
        .build =
            [](ConstraintCircuit& c) {
              const CircuitBits x = Inputs(c, 3);
              const CircuitBits y = Inputs(c, 3);
              return c.Less(x, y, false);
            }},
    Shape{
        .name = "s -> d == 0",
        .solutions = 33,
        .width = 6,
        .build =
            [](ConstraintCircuit& c) {
              const CircuitBits s = Inputs(c, 1);
              const CircuitBits d = Inputs(c, 5);
              return c.Or(s[0].Negated(), c.Equal(d, ConstantOf(0, 5)));
            }},
    // Five-bit x == 0 has 32 solutions; x * y == 21 modulo 32 has one y for
    // each odd x, 15 of them above 1.
    Shape{
        .name = "x == 0 or x * y == 21, x > 1",
        .solutions = 47,
        .width = 5,
        .build =
            [](ConstraintCircuit& c) {
              const CircuitBits x = Inputs(c, 5);
              const CircuitBits y = Inputs(c, 5);
              return c.Or(
                  c.Equal(x, ConstantOf(0, 5)),
                  c.And(
                      c.Equal(c.Mul(x, y), ConstantOf(21, 5)),
                      c.Less(ConstantOf(1, 5), x, false)));
            }},
};

constexpr std::size_t kRoomy = std::size_t{1} << 16U;
constexpr std::size_t kCramped = 4;

// Where the condition fits a decision diagram of the budget, the drawn values
// spread evenly over the assignments making it true (LRM 18.5.9).
TEST(ConstraintSamplerTest, AnExactDrawIsUniform) {
  for (const Shape& shape : kShapes) {
    SCOPED_TRACE(shape.name);
    ConstraintCircuit c;
    const CircuitLiteral condition = shape.build(c);
    const std::vector<std::uint32_t> inputs = AllInputs(c);
    const std::vector<DrawnVariable> drawn = VariablesOf(inputs, shape.width);
    ConstraintSampler sampler(c, kRoomy);
    DrawRng rng{RandomSeed{1}};
    std::map<std::vector<bool>, int> seen;
    const int draws = static_cast<int>(shape.solutions) * 300;
    for (int i = 0; i < draws; ++i) {
      const std::optional<std::vector<bool>> value =
          sampler.Draw(condition, drawn, rng);
      ASSERT_TRUE(value.has_value());
      ASSERT_TRUE(c.Evaluate(condition, *value));
      ++seen[*value];
    }
    EXPECT_EQ(seen.size(), shape.solutions);
    EXPECT_TRUE(LooksUniform(seen, shape.solutions, draws));
  }
}

// Past the budget every draw still makes the condition true and reaches every
// assignment that does, though not evenly.
TEST(ConstraintSamplerTest, PastTheBudgetEveryDrawHolds) {
  for (const Shape& shape : kShapes) {
    SCOPED_TRACE(shape.name);
    ConstraintCircuit c;
    const CircuitLiteral condition = shape.build(c);
    const std::vector<std::uint32_t> inputs = AllInputs(c);
    const std::vector<DrawnVariable> drawn = VariablesOf(inputs, shape.width);
    ConstraintSampler sampler(c, kCramped);
    DrawRng rng{RandomSeed{1}};
    std::map<std::vector<bool>, int> seen;
    const int draws = static_cast<int>(shape.solutions) * 100;
    for (int i = 0; i < draws; ++i) {
      const std::optional<std::vector<bool>> value =
          sampler.Draw(condition, drawn, rng);
      ASSERT_TRUE(value.has_value());
      ASSERT_TRUE(c.Evaluate(condition, *value));
      ++seen[*value];
    }
    EXPECT_EQ(seen.size(), shape.solutions);
  }
}

// What a draw returns follows from the condition and the generator alone
// (LRM 18.14): the first draws of a condition, which build what they draw from,
// return what later draws return once that is kept and other conditions have
// been drawn beside it. The condition is one no other test draws, so the first
// sequence is the one that builds.
TEST(ConstraintSamplerTest, ADrawDependsOnlyOnTheSeed) {
  for (const std::size_t budget : {kRoomy, kCramped}) {
    SCOPED_TRACE("budget " + std::to_string(budget));
    const auto sequence = [&](bool drawn_before) {
      ConstraintCircuit c;
      const CircuitLiteral other = kShapes[2].build(c);
      const CircuitBits x = Inputs(c, 4);
      const CircuitBits y = Inputs(c, 4);
      const CircuitLiteral condition =
          c.Less(c.Add(x, y), ConstantOf(9, 4), false);
      const std::vector<std::uint32_t> inputs = AllInputs(c);
      const std::vector<std::uint32_t> other_inputs(
          inputs.begin(), inputs.begin() + 10);
      const std::vector<std::uint32_t> drawn_inputs(
          inputs.begin() + 10, inputs.end());
      const std::vector<DrawnVariable> others = VariablesOf(other_inputs, 5);
      const std::vector<DrawnVariable> drawn = VariablesOf(drawn_inputs, 4);
      ConstraintSampler sampler(c, budget);
      if (drawn_before) {
        DrawRng warm{RandomSeed{77}};
        for (int i = 0; i < 50; ++i) {
          EXPECT_TRUE(sampler.Draw(other, others, warm).has_value());
        }
      }
      DrawRng rng{RandomSeed{3}};
      std::vector<std::vector<bool>> values;
      for (int i = 0; i < 50; ++i) {
        values.push_back(*sampler.Draw(condition, drawn, rng));
      }
      return values;
    };
    const std::vector<std::vector<bool>> first = sequence(false);
    EXPECT_EQ(first, sequence(true));
  }
}

// A draw of some of the inputs is uniform over the values of those inputs
// that the rest can complete, not over whole assignments (LRM 18.5.9's
// ordered draw): of `s -> d == 0` with s drawn alone, s is 1 half the time.
TEST(ConstraintSamplerTest, AProjectedDrawWeighsEachValueOfWhatIsDrawn) {
  ConstraintCircuit c;
  const CircuitBits s = Inputs(c, 1);
  const CircuitBits d = Inputs(c, 8);
  const CircuitLiteral condition =
      c.Or(s[0].Negated(), c.Equal(d, ConstantOf(0, 8)));
  ConstraintSampler sampler(c, kRoomy);
  DrawRng rng{RandomSeed{5}};
  const std::array<std::uint32_t, 1> s_input{0};
  const std::array drawn{DrawnVariable{.inputs = s_input, .members = {}}};
  int ones = 0;
  constexpr int kDraws = 4000;
  for (int i = 0; i < kDraws; ++i) {
    const auto value = sampler.Draw(condition, drawn, rng);
    ASSERT_TRUE(value.has_value());
    ones += (*value)[0] ? 1 : 0;
  }
  EXPECT_GT(ones, 1800);
  EXPECT_LT(ones, 2200);
}

// Past the budget a variable that may take only some values is drawn among
// them: a three-bit variable limited to 1, 4 and 6 reaches each of them.
TEST(ConstraintSamplerTest, PastTheBudgetAVariableTakesItsMembers) {
  ConstraintCircuit c;
  const CircuitBits x = Inputs(c, 3);
  CircuitLiteral member = kFalseLiteral;
  std::vector<std::vector<bool>> members;
  for (const std::uint64_t value : {1U, 4U, 6U}) {
    member = c.Or(member, c.Equal(x, ConstantOf(value, 3)));
    std::vector<bool> bits;
    for (std::size_t i = 0; i < 3; ++i) {
      bits.push_back(((value >> i) & 1U) != 0);
    }
    members.push_back(bits);
  }
  const std::vector<std::uint32_t> inputs = AllInputs(c);
  const std::array drawn{DrawnVariable{.inputs = inputs, .members = members}};
  ConstraintSampler sampler(c, kCramped);
  DrawRng rng{RandomSeed{4}};
  std::map<std::vector<bool>, int> seen;
  for (int i = 0; i < 300; ++i) {
    const auto value = sampler.Draw(member, drawn, rng);
    ASSERT_TRUE(value.has_value());
    ASSERT_TRUE(c.Evaluate(member, *value));
    ++seen[*value];
  }
  EXPECT_EQ(seen.size(), 3U);
}

// Nothing satisfies a contradiction, by either way of drawing.
TEST(ConstraintSamplerTest, AContradictionHasNoDraw) {
  ConstraintCircuit c;
  const CircuitBits x = Inputs(c, 6);
  const CircuitLiteral condition = c.And(
      c.Less(x, ConstantOf(3, 6), false), c.Less(ConstantOf(40, 6), x, false));
  const std::vector<std::uint32_t> inputs = AllInputs(c);
  const std::vector<DrawnVariable> drawn = VariablesOf(inputs, 6);
  DrawRng rng{RandomSeed{2}};
  for (const std::size_t budget : {kRoomy, kCramped}) {
    ConstraintSampler sampler(c, budget);
    EXPECT_FALSE(sampler.Draw(condition, drawn, rng).has_value());
    EXPECT_FALSE(sampler.Satisfiable(condition));
  }
}

}  // namespace
