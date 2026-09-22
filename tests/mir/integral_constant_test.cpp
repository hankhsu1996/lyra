#include "lyra/mir/integral_constant.hpp"

#include <cstdint>
#include <gtest/gtest.h>

#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/integral_constant_id.hpp"
#include "lyra/mir/type.hpp"

namespace lyra::mir {
namespace {

auto Bits(std::uint64_t word) -> IntegralConstant {
  return IntegralConstant{.value_words = {word}, .state_words = {}};
}

auto Intern(
    const CompilationUnit& unit, TypeId type, const IntegralConstant& value)
    -> IntegralConstantId {
  return unit.integral_constants.Intern(
      IntegralConstantDecl{.type = type, .value = value});
}

// Two occurrences of the same value at the same type are one entity, which is
// what makes a use a reference rather than a construction.
TEST(MirIntegralConstantTest, OneValueIsOneEntry) {
  const CompilationUnit unit;
  const IntegralConstantId first =
      Intern(unit, unit.builtins.int_type, Bits(7));
  const IntegralConstantId again =
      Intern(unit, unit.builtins.int_type, Bits(7));
  EXPECT_EQ(first, again);
  EXPECT_EQ(unit.integral_constants.size(), 1U);
}

// The type is part of what a constant is: the same bits read as a different
// width, a different signedness, or with an unknown plane are different values,
// and folding them would hand a use the wrong one.
TEST(MirIntegralConstantTest, TheTypeIsPartOfTheValue) {
  const CompilationUnit unit;
  const IntegralConstantId as_int =
      Intern(unit, unit.builtins.int_type, Bits(1));
  const IntegralConstantId as_unsigned =
      Intern(unit, unit.builtins.int_unsigned, Bits(1));
  const IntegralConstantId as_integer =
      Intern(unit, unit.builtins.integer, Bits(1));
  const IntegralConstantId as_bit = Intern(unit, unit.builtins.bit1, Bits(1));

  EXPECT_NE(as_int, as_unsigned);
  EXPECT_NE(as_int, as_integer);
  EXPECT_NE(as_int, as_bit);
  EXPECT_EQ(unit.integral_constants.size(), 4U);
}

// A value and the same value carrying an unknown plane are different entries.
// The plane is absent for a two-state type, which is an empty run of words
// rather than a run of zeros, so the two spellings must not meet.
TEST(MirIntegralConstantTest, AnUnknownPlaneIsPartOfTheValue) {
  const CompilationUnit unit;
  const IntegralConstantId known = Intern(
      unit, unit.builtins.integer,
      IntegralConstant{.value_words = {1}, .state_words = {0}});
  const IntegralConstantId unknown = Intern(
      unit, unit.builtins.integer,
      IntegralConstant{.value_words = {1}, .state_words = {1}});
  EXPECT_NE(known, unknown);
}

// The pool hands back the entry it was asked for, so a consumer emitting one
// reads the bits and the type the occurrence wrote.
TEST(MirIntegralConstantTest, AnEntryAnswersWithWhatWasInterned) {
  const CompilationUnit unit;
  const IntegralConstantId id =
      Intern(unit, unit.builtins.int_type, Bits(0x2A));
  const IntegralConstantDecl& decl = unit.integral_constants.Get(id);
  EXPECT_EQ(decl.type, unit.builtins.int_type);
  EXPECT_EQ(decl.value.value_words.size(), 1U);
  EXPECT_EQ(decl.value.value_words[0], 0x2AU);
  EXPECT_TRUE(decl.value.state_words.empty());
}

}  // namespace
}  // namespace lyra::mir
