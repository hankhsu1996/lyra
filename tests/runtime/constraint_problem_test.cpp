#include "lyra/runtime/constraint_problem.hpp"

#include <array>
#include <cstddef>
#include <cstdint>
#include <functional>
#include <gtest/gtest.h>
#include <optional>
#include <string>

#include "lyra/runtime/constraint_cycle.hpp"
#include "lyra/runtime/rng.hpp"
#include "lyra/runtime/runtime.hpp"
#include "lyra/runtime/var.hpp"
#include "lyra/value/integral.hpp"
#include "lyra/value/integral_value_type.hpp"
#include "lyra/value/object_ref.hpp"
#include "lyra/value/value_type.hpp"

namespace {

using lyra::runtime::ConstraintProblem;
using lyra::runtime::DrawRng;
using lyra::runtime::ErasedReference;
using lyra::runtime::RandomSeed;
using lyra::value::BitVector;
using lyra::value::IntegralValueType;

constexpr auto kRand = lyra::runtime::RandomQualifier::kRand;

// The run the problems are made in, at the budget a run has by default.
auto TheRun() -> lyra::runtime::Runtime& {
  static lyra::runtime::Runtime run;
  return run;
}

template <typename T>
auto TypeOf() -> const lyra::value::ValueType& {
  return IntegralValueType::Of<T>().Type();
}

template <typename T>
auto Lent(T& storage) -> ErasedReference {
  return ErasedReference{.holder = {}, .storage = &storage};
}

// The term of `storage` once it is declared as `declared` says, as a
// constraint naming it reads it.
template <typename T>
auto TermOf(
    ConstraintProblem& problem, T& storage,
    lyra::runtime::RandomQualifier declared,
    lyra::runtime::ConstraintCycle* cycle = nullptr) -> std::int64_t {
  problem.Declare(
      Lent(storage), &storage, TypeOf<T>(), declared, true, cycle, nullptr);
  return problem.Named(Lent(storage), &storage, TypeOf<T>());
}

// LRM 18.6.1: an answer lands in every variable chosen and makes every
// constraint true; a variable held reads as the constant it holds.
TEST(ConstraintProblemTest, AnAnswerMeetsEveryConstraintAndLands) {
  DrawRng rng{RandomSeed{3}};
  for (int round = 0; round < 200; ++round) {
    BitVector<8> x = BitVector<8>::FromInt(0);
    BitVector<8> y = BitVector<8>::FromInt(0);
    BitVector<8> held = BitVector<8>::FromInt(40);
    ConstraintProblem problem(TheRun(), rng, lyra::value::ObjectRef{}, false);
    const std::int64_t tx = TermOf(problem, x, kRand);
    const std::int64_t ty = TermOf(problem, y, kRand);
    const std::int64_t th =
        problem.Named(Lent(held), &held, TypeOf<BitVector<8>>());
    // x < y, and x + y == held.
    problem.Require(problem.Less(tx, ty, false));
    problem.Require(problem.Equal(problem.Add(tx, ty), th));
    ASSERT_TRUE(problem.Solve());
    problem.Commit();
    EXPECT_LT(x.ToInt64(), y.ToInt64());
    EXPECT_EQ((x.ToInt64() + y.ToInt64()) % 256, 40);
    EXPECT_EQ(held.ToInt64(), 40);
  }
}

// LRM 18.6.3: where nothing meets the constraints, randomize() fails and every
// variable keeps what it held.
TEST(ConstraintProblemTest, AnInfeasibleProblemChangesNothing) {
  DrawRng rng{RandomSeed{4}};
  BitVector<8> x = BitVector<8>::FromInt(17);
  ConstraintProblem problem(TheRun(), rng, lyra::value::ObjectRef{}, false);
  const std::int64_t tx = TermOf(problem, x, kRand);
  BitVector<8> five = BitVector<8>::FromInt(5);
  const std::int64_t t5 = problem.Constant(&five, TypeOf<BitVector<8>>());
  problem.Require(problem.Less(tx, t5, false));
  problem.Require(problem.Less(t5, tx, false));
  EXPECT_FALSE(problem.Solve());
  EXPECT_EQ(x.ToInt64(), 17);
}

// LRM 18.14: what one call draws does not move what the object's next call
// draws. A call that draws one easy value and one that draws many leave the
// generator they were handed in the same state.
TEST(ConstraintProblemTest, ACallAdvancesTheGeneratorTheSameWhateverItDraws) {
  DrawRng easy_rng{RandomSeed{30}};
  DrawRng hard_rng{RandomSeed{30}};
  {
    BitVector<8> x = BitVector<8>::FromInt(0);
    BitVector<8> two = BitVector<8>::FromInt(2);
    ConstraintProblem problem(
        TheRun(), easy_rng, lyra::value::ObjectRef{}, false);
    const std::int64_t tx = TermOf(problem, x, kRand);
    problem.Require(problem.Less(
        tx, problem.Constant(&two, TypeOf<BitVector<8>>()), false));
    ASSERT_TRUE(problem.Solve());
  }
  {
    BitVector<32> a = BitVector<32>::FromInt(0);
    BitVector<32> b = BitVector<32>::FromInt(0);
    BitVector<32> c = BitVector<32>::FromInt(0);
    ConstraintProblem problem(
        TheRun(), hard_rng, lyra::value::ObjectRef{}, false);
    const std::int64_t ta = TermOf(problem, a, kRand);
    const std::int64_t tb = TermOf(problem, b, kRand);
    const std::int64_t tc = TermOf(problem, c, kRand);
    problem.Require(problem.Less(ta, tb, false));
    problem.Require(problem.Less(tb, tc, false));
    ASSERT_TRUE(problem.Solve());
  }
  EXPECT_EQ(easy_rng.StateText(), hard_rng.StateText());
}

// LRM 18.5.9: an answer drawn past what the run can count still meets every
// constraint, and the run says once that it was not drawn uniformly, with how
// to raise the limit a run's command line sets.
TEST(ConstraintProblemTest, ADrawPastTheBudgetIsReportedOnce) {
  lyra::runtime::Runtime run{
      lyra::runtime::RuntimeOptions{.plusargs = {"lyra+randomize+nodes=4"}}};
  DrawRng rng{RandomSeed{40}};
  testing::internal::CaptureStderr();
  for (int round = 0; round < 3; ++round) {
    BitVector<8> x = BitVector<8>::FromInt(0);
    BitVector<8> y = BitVector<8>::FromInt(0);
    ConstraintProblem problem(run, rng, lyra::value::ObjectRef{}, false);
    const std::int64_t tx = TermOf(problem, x, kRand);
    const std::int64_t ty = TermOf(problem, y, kRand);
    problem.Require(problem.Less(tx, ty, false));
    ASSERT_TRUE(problem.Solve());
    problem.Commit();
    EXPECT_LT(x.ToInt64(), y.ToInt64());
  }
  const std::string printed = testing::internal::GetCapturedStderr();
  const std::string warning =
      "warning: 'randomize' drew values without a uniform distribution; its "
      "constraints have too many solutions to count\n";
  const std::size_t first = printed.find(warning);
  ASSERT_NE(first, std::string::npos) << printed;
  EXPECT_EQ(printed.find(warning, first + 1), std::string::npos) << printed;
  EXPECT_NE(
      printed.find(
          "note: raise the limit with '+lyra+randomize+nodes=<count>'"),
      std::string::npos)
      << printed;
}

// LRM 18.5.9: `s -> d == 0` unordered makes s = 1 one draw in about 257; with
// `solve s before d` s is 1 half the time.
TEST(ConstraintProblemTest, AnOrderingWeighsTheEarlierVariableOnItsOwn) {
  DrawRng rng{RandomSeed{8}};
  int unordered_ones = 0;
  int ordered_ones = 0;
  constexpr int kDraws = 2000;
  for (int i = 0; i < kDraws; ++i) {
    for (const bool ordered : {false, true}) {
      BitVector<1> s = BitVector<1>::FromInt(0);
      BitVector<8> d = BitVector<8>::FromInt(0);
      BitVector<8> zero = BitVector<8>::FromInt(0);
      ConstraintProblem problem(TheRun(), rng, lyra::value::ObjectRef{}, false);
      const std::int64_t ts = TermOf(problem, s, kRand);
      const std::int64_t td = TermOf(problem, d, kRand);
      const std::int64_t tz = problem.Constant(&zero, TypeOf<BitVector<8>>());
      problem.Require(
          problem.LogicalOr(problem.LogicalNot(ts), problem.Equal(td, tz)));
      if (ordered) {
        problem.SolveBefore(ts, td);
      }
      ASSERT_TRUE(problem.Solve());
      problem.Commit();
      (ordered ? ordered_ones : unordered_ones) +=
          static_cast<int>(s.ToInt64());
    }
  }
  EXPECT_LT(unordered_ones, 40);
  EXPECT_GT(ordered_ones, 900);
  EXPECT_LT(ordered_ones, 1100);
}

// LRM 18.5.13: a soft constraint holds wherever the hard ones allow it, and
// of two that conflict the later one wins.
TEST(ConstraintProblemTest, ALaterSoftConstraintWinsAConflict) {
  DrawRng rng{RandomSeed{6}};
  BitVector<8> x = BitVector<8>::FromInt(0);
  BitVector<8> three = BitVector<8>::FromInt(3);
  BitVector<8> nine = BitVector<8>::FromInt(9);
  ConstraintProblem problem(TheRun(), rng, lyra::value::ObjectRef{}, false);
  const std::int64_t tx = TermOf(problem, x, kRand);
  problem.Prefer(
      problem.Equal(tx, problem.Constant(&three, TypeOf<BitVector<8>>())));
  problem.Prefer(
      problem.Equal(tx, problem.Constant(&nine, TypeOf<BitVector<8>>())));
  ASSERT_TRUE(problem.Solve());
  problem.Commit();
  EXPECT_EQ(x.ToInt64(), 9);
}

// LRM 18.4.2: a randc variable takes every value of its range once before
// any value repeats, and a value the constraints rule out at one call is
// taken at a later one.
TEST(ConstraintProblemTest, ACyclicVariableTakesEachValueOncePerCycle) {
  DrawRng rng{RandomSeed{12}};
  lyra::runtime::ConstraintCycle cycle(3, std::nullopt);
  for (int round = 0; round < 5; ++round) {
    std::array<int, 8> seen{};
    for (int call = 0; call < 8; ++call) {
      BitVector<3> y = BitVector<3>::FromInt(0);
      ConstraintProblem problem(TheRun(), rng, lyra::value::ObjectRef{}, false);
      const std::int64_t term =
          TermOf(problem, y, lyra::runtime::RandomQualifier::kRandc, &cycle);
      ASSERT_GE(term, 0);
      ASSERT_TRUE(problem.Solve());
      problem.Commit();
      ++seen[static_cast<std::size_t>(y.ToInt64())];
    }
    for (const int count : seen) {
      EXPECT_EQ(count, 1);
    }
  }
}

// LRM 18.5.3: a distribution draws only values of nonzero weight, an item
// whose weight is shared (`:/`) weighing its weight together and one whose
// weight is each value's (`:=`) weighing it once per value.
TEST(ConstraintProblemTest, ADistributionDrawsByWeight) {
  DrawRng rng{RandomSeed{21}};
  const auto constant = [](ConstraintProblem& problem, std::int64_t value) {
    BitVector<4> bits = BitVector<4>::FromInt(value);
    return problem.Constant(&bits, TypeOf<BitVector<4>>());
  };
  std::array<int, 16> seen{};
  constexpr int kDraws = 3000;
  for (int i = 0; i < kDraws; ++i) {
    BitVector<4> v = BitVector<4>::FromInt(9);
    BitVector<1> on = BitVector<1>::FromInt(1);
    ConstraintProblem problem(TheRun(), rng, lyra::value::ObjectRef{}, false);
    const std::int64_t tv = TermOf(problem, v, kRand);
    const std::int64_t guard = problem.Constant(&on, TypeOf<BitVector<1>>());
    const std::int64_t dist = problem.Distribution(tv, guard, false, false);
    problem.DistributionItem(
        dist, constant(problem, 0), constant(problem, 0), constant(problem, 1),
        true);
    problem.DistributionItem(
        dist, constant(problem, 1), constant(problem, 3), constant(problem, 2),
        false);
    problem.DistributionItem(
        dist, constant(problem, 15), constant(problem, 15),
        constant(problem, 0), true);
    ASSERT_TRUE(problem.Solve());
    problem.Commit();
    ++seen[static_cast<std::size_t>(v.ToInt64())];
  }
  for (std::size_t value = 4; value < seen.size(); ++value) {
    EXPECT_EQ(seen[value], 0) << value;
  }
  EXPECT_GT(seen[0], 850);
  EXPECT_LT(seen[0], 1150);
}

// LRM 18.5.4: the members of a uniqueness constraint differ pairwise.
TEST(ConstraintProblemTest, AUniqueSetDiffersPairwise) {
  DrawRng rng{RandomSeed{22}};
  for (int i = 0; i < 50; ++i) {
    std::array<BitVector<2>, 3> values{};
    BitVector<2> three = BitVector<2>::FromInt(3);
    BitVector<1> on = BitVector<1>::FromInt(1);
    ConstraintProblem problem(TheRun(), rng, lyra::value::ObjectRef{}, false);
    const std::int64_t set = problem.UniqueSet();
    const std::int64_t limit = problem.Constant(&three, TypeOf<BitVector<2>>());
    for (BitVector<2>& value : values) {
      const std::int64_t term = TermOf(problem, value, kRand);
      problem.UniqueMember(set, term);
      problem.Require(problem.Less(term, limit, false));
    }
    problem.UniqueRequire(set, problem.Constant(&on, TypeOf<BitVector<1>>()));
    ASSERT_TRUE(problem.Solve());
    problem.Commit();
    EXPECT_NE(values[0].ToInt64(), values[1].ToInt64());
    EXPECT_NE(values[0].ToInt64(), values[2].ToInt64());
    EXPECT_NE(values[1].ToInt64(), values[2].ToInt64());
  }
}

// LRM 18.5.11: a variable a function is handed is chosen first, by the
// constraints reading it alone, and held after; LRM 18.6.3: a call that then
// fails leaves it as it was.
TEST(ConstraintProblemTest, WhatIsChosenFirstIsHeldThenPutBack) {
  DrawRng rng{RandomSeed{23}};
  BitVector<8> x = BitVector<8>::FromInt(0);
  BitVector<8> y = BitVector<8>::FromInt(0);
  BitVector<8> seven = BitVector<8>::FromInt(7);
  ConstraintProblem problem(TheRun(), rng, lyra::value::ObjectRef{}, false);
  const std::int64_t first_y = TermOf(problem, y, kRand);
  const std::int64_t first_x = TermOf(problem, x, kRand);
  problem.Want(first_y);
  problem.Require(
      problem.Equal(first_y, problem.Constant(&seven, TypeOf<BitVector<8>>())));
  problem.Require(problem.Less(first_x, first_y, false));
  ASSERT_TRUE(problem.SettleFirst());
  EXPECT_EQ(y.ToInt64(), 7);

  // The description is stated again, `y` held at what was chosen.
  const std::int64_t ty = TermOf(problem, y, kRand);
  const std::int64_t tx = TermOf(problem, x, kRand);
  problem.Require(problem.Less(ty, tx, false));
  problem.Require(problem.Less(tx, ty, false));
  EXPECT_FALSE(problem.Solve());
  problem.Abandon();
  EXPECT_EQ(y.ToInt64(), 0);
}

// LRM 18.5.7.1 with 18.5.13: a size is chosen first under the soft
// constraints that reach it.
TEST(ConstraintProblemTest, ASizeChosenFirstKeepsItsSoftConstraints) {
  DrawRng rng{RandomSeed{31}};
  int whole = 0;
  BitVector<32> three = BitVector<32>::FromInt(3);
  for (int n = 0; n < 20; ++n) {
    ConstraintProblem problem(TheRun(), rng, lyra::value::ObjectRef{}, false);
    std::size_t chosen = 0;
    const std::int64_t array = problem.DeclareArray(
        Lent(whole),
        [&](std::size_t count) -> std::function<void()> {
          chosen = count;
          return [] {};
        },
        true);
    EXPECT_EQ(array, 0);
    const std::int64_t size = problem.ArraySize(Lent(whole), 0);
    problem.Prefer(
        problem.Equal(size, problem.Constant(&three, TypeOf<BitVector<32>>())));
    ASSERT_TRUE(problem.SettleFirst());
    EXPECT_EQ(chosen, 3U);
  }
}

}  // namespace
