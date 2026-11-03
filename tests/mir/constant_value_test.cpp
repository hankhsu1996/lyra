#include "lyra/mir/constant_value.hpp"

#include <cstdint>
#include <gtest/gtest.h>
#include <variant>

#include "lyra/diag/source_manager.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/constant_id.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/expr_builders.hpp"
#include "lyra/mir/type_builders.hpp"

namespace lyra::mir {
namespace {

auto Bits(std::uint64_t word) -> IntegralConstant {
  return IntegralConstant{.value_words = {word}, .state_words = {}};
}

auto Intern(
    const CompilationUnit& unit, TypeId type, const IntegralConstant& value)
    -> ConstantId {
  return unit.constants.Intern(
      ConstantDecl{
          .type = type,
          .value = ConstantValue{IntegralValue{.planes = value}}});
}

// Two occurrences of the same value at the same type are one entity, which is
// what makes a use a reference rather than a construction.
TEST(MirConstantValueTest, OneValueIsOneEntry) {
  const CompilationUnit unit;
  const ConstantId first = Intern(unit, unit.builtins.int_type, Bits(7));
  const ConstantId again = Intern(unit, unit.builtins.int_type, Bits(7));
  EXPECT_EQ(first, again);
  EXPECT_EQ(unit.constants.size(), 1U);
}

// The type is part of what a constant is: the same bits read as a different
// width, a different signedness, or with an unknown plane are different values,
// and folding them would hand a use the wrong one.
TEST(MirConstantValueTest, TheTypeIsPartOfTheValue) {
  const CompilationUnit unit;
  const ConstantId as_int = Intern(unit, unit.builtins.int_type, Bits(1));
  const ConstantId as_unsigned =
      Intern(unit, unit.builtins.int_unsigned, Bits(1));
  const ConstantId as_integer = Intern(unit, unit.builtins.integer, Bits(1));
  const ConstantId as_bit = Intern(unit, unit.builtins.bit1, Bits(1));

  EXPECT_NE(as_int, as_unsigned);
  EXPECT_NE(as_int, as_integer);
  EXPECT_NE(as_int, as_bit);
  EXPECT_EQ(unit.constants.size(), 4U);
}

// A value and the same value carrying an unknown plane are different entries.
// The plane is absent for a two-state type, which is an empty run of words
// rather than a run of zeros, so the two spellings must not meet.
TEST(MirConstantValueTest, AnUnknownPlaneIsPartOfTheValue) {
  const CompilationUnit unit;
  const ConstantId known = Intern(
      unit, unit.builtins.integer,
      IntegralConstant{.value_words = {1}, .state_words = {0}});
  const ConstantId unknown = Intern(
      unit, unit.builtins.integer,
      IntegralConstant{.value_words = {1}, .state_words = {1}});
  EXPECT_NE(known, unknown);
}

// A location is what it says, so two operations written at one place are one
// constant, and any difference in what is said is another; the constant is the
// library's record of the file, the line and the column.
TEST(MirConstantValueTest, ALocationIsWhatItSays) {
  const CompilationUnit unit;
  const diag::SourcePosition at{.file = "a.sv", .line = 1, .column = 2};
  const ConstantId first = InternSourceLocation(unit, at);
  EXPECT_EQ(first, InternSourceLocation(unit, at));
  EXPECT_NE(
      first,
      InternSourceLocation(unit, {.file = "b.sv", .line = 1, .column = 2}));
  EXPECT_NE(
      first,
      InternSourceLocation(unit, {.file = "a.sv", .line = 3, .column = 2}));
  EXPECT_NE(
      first,
      InternSourceLocation(unit, {.file = "a.sv", .line = 1, .column = 4}));
  const ConstantDecl& stated = unit.constants.Get(first);
  EXPECT_EQ(stated.type, unit.builtins.source_location_record);
  const auto* record = std::get_if<RecordValue>(&stated.value.value);
  ASSERT_NE(record, nullptr);
  EXPECT_EQ(record->parts.at(0), ConstantValue{CStringValue{.text = "a.sv"}});
}

// A table is what it holds: two tables holding the same entries, each pointing
// at the same constant, are one constant, and one holding another entry is
// another, so whoever states a table states it once however many ask for it.
TEST(MirConstantValueTest, ATableIsWhatItHolds) {
  const CompilationUnit unit;
  const ConstantId location = InternSourceLocation(
      unit, diag::SourcePosition{.file = "a.sv", .line = 1, .column = 2});
  const TypeId record = unit.builtins.source_location_record;
  const auto table_of = [&](std::uint32_t entries) {
    ArrayValue array;
    for (std::uint32_t i = 0; i < entries; ++i) {
      array.elements.push_back(
          ConstantValue{PointerValue{
              .to = ConstantRef{.constant = location}, .type = record}});
    }
    return unit.constants.Intern(
        ConstantDecl{
            .type = MachineArrayOf(
                unit.types, unit.builtins.source_location_record_ptr, entries),
            .value = ConstantValue{std::move(array)}});
  };
  EXPECT_EQ(table_of(2), table_of(2));
  EXPECT_NE(table_of(2), table_of(1));
}

}  // namespace
}  // namespace lyra::mir
