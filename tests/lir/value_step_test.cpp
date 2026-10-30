#include "lyra/lir/value_step.hpp"

#include <gtest/gtest.h>
#include <optional>
#include <utility>
#include <variant>
#include <vector>

#include "lyra/lir/compilation_unit.hpp"
#include "lyra/lir/linkage.hpp"
#include "lyra/lir/record_id.hpp"
#include "lyra/lir/type.hpp"
#include "lyra/lir/type_id.hpp"
#include "lyra/support/runtime_object.hpp"
#include "lyra/support/value_domain.hpp"

namespace lyra::lir {
namespace {

// A unit naming an integral type, a string, and what is made of them.
struct Types {
  CompilationUnit unit;
  TypeId integral = unit.types.Intern(
      Type{IntegralType{
          .bit_width = 32,
          .signedness = Signedness::kSigned,
          .state_kind = IntegralStateKind::kFourState}});
  TypeId string = unit.types.Intern(Type{StringType{}});

  auto Record(RecordKind kind, std::vector<TypeId> members) -> TypeId {
    const RecordId id = unit.records.Add(
        RecordDecl{
            .defining_unit = std::nullopt,
            .path = {},
            .linkage = Linkage::kExternal,
            .kind = kind,
            .elements = std::move(members),
            .methods = {}});
    return unit.types.Intern(Type{RecordType{.declaration = id}});
  }

  [[nodiscard]] auto Tuple(std::vector<TypeId> components) const -> TypeId {
    return unit.types.Intern(
        Type{TupleType{.elements = std::move(components)}});
  }

  [[nodiscard]] auto Owning(TypeId pointee) const -> TypeId {
    return unit.types.Intern(
        Type{PointerType{
            .pointee = pointee,
            .ownership = PointerOwnership::kUnique,
            .mutability = Mutability::kMutable}});
  }

  [[nodiscard]] auto Step(TypeId type, ValueStep step) const
      -> StepRealization {
    return StepOf(unit, type, step);
  }
};

template <typename Realization>
auto Is(const StepRealization& realized) -> bool {
  return std::holds_alternative<Realization>(realized);
}

// A value made only of what ends with nothing to do is its bytes for every
// step, however it is laid out; an untagged union is too, whatever its members
// hold, since they are only bits another member may have written.
TEST(ValueStepTest, AValueWithNothingToEndIsItsBytes) {
  Types t;
  const TypeId plain = t.Record(RecordKind::kStruct, {t.integral, t.integral});
  const TypeId overlay = t.Record(RecordKind::kUnion, {t.integral, t.string});
  for (const ValueStep step :
       {ValueStep::kCopy, ValueStep::kMove, ValueStep::kAssign,
        ValueStep::kEnd}) {
    EXPECT_TRUE(Is<StepIsBytes>(t.Step(t.integral, step)));
    EXPECT_TRUE(Is<StepIsBytes>(t.Step(plain, step)));
    EXPECT_TRUE(Is<StepIsBytes>(t.Step(overlay, step)));
  }
  EXPECT_FALSE(NeedsEnd(t.unit, plain));
  EXPECT_FALSE(NeedsEnd(t.unit, overlay));
}

// A value the library holds is stepped by the library's entry for what it is
// held as.
TEST(ValueStepTest, ALibraryValueIsSteppedByTheLibrary) {
  Types t;
  for (const ValueStep step :
       {ValueStep::kCopy, ValueStep::kMove, ValueStep::kAssign,
        ValueStep::kEnd}) {
    const StepRealization realized = t.Step(t.string, step);
    ASSERT_TRUE(Is<StepByLibrary>(realized));
    EXPECT_EQ(
        std::get<StepByLibrary>(realized).object,
        support::RuntimeObject{support::ValueDomain::kString});
  }
}

// A value laid out by its type that holds something to end is stepped by its
// own function, part by part, at every depth and whichever kind of product
// or tagged union it is.
TEST(ValueStepTest, AValueHoldingSomethingToEndIsSteppedByItsFunction) {
  Types t;
  const TypeId named = t.Record(RecordKind::kStruct, {t.integral, t.string});
  const TypeId nested = t.Tuple({t.integral, named});
  const TypeId elements = t.unit.types.Intern(
      Type{UnpackedArrayType{.element_type = t.string, .size = 4}});
  const TypeId tagged =
      t.Record(RecordKind::kTaggedUnion, {t.integral, t.string});
  for (const TypeId type : {named, nested, elements, tagged}) {
    EXPECT_TRUE(NeedsEnd(t.unit, type));
    for (const ValueStep step :
         {ValueStep::kCopy, ValueStep::kMove, ValueStep::kAssign,
          ValueStep::kEnd}) {
      EXPECT_TRUE(Is<StepByFunction>(t.Step(type, step)));
    }
  }
}

// An owning pointer moves as the address it is and is never copied; ending it
// ends what it points at, as its type says where the pointer ends, and through
// the definition an object opens with where only that knows.
// A value holding one has no copy and no move: a move leaves what it took to be
// ended, which would give back what the moved pointer now owns. It still ends.
TEST(ValueStepTest, AnOwningPointerMovesAndEndsButIsNeverCopied) {
  Types t;
  const TypeId boxed = t.Owning(t.string);
  EXPECT_TRUE(Is<StepIsBytes>(t.Step(boxed, ValueStep::kMove)));
  EXPECT_TRUE(Is<NoSuchStep>(t.Step(boxed, ValueStep::kCopy)));
  EXPECT_TRUE(Is<NoSuchStep>(t.Step(boxed, ValueStep::kAssign)));
  EXPECT_TRUE(Is<StepThroughPointee>(t.Step(boxed, ValueStep::kEnd)));

  const ClassId cls = t.unit.classes.Add(Class{});
  const TypeId object = t.unit.types.Intern(Type{ObjectType{.class_id = cls}});
  EXPECT_TRUE(
      Is<StepThroughDefinition>(t.Step(t.Owning(object), ValueStep::kEnd)));

  const TypeId holding = t.Tuple({t.integral, boxed});
  EXPECT_TRUE(Is<NoSuchStep>(t.Step(holding, ValueStep::kCopy)));
  EXPECT_TRUE(Is<NoSuchStep>(t.Step(holding, ValueStep::kAssign)));
  EXPECT_TRUE(Is<NoSuchStep>(t.Step(holding, ValueStep::kMove)));
  EXPECT_TRUE(Is<StepByFunction>(t.Step(holding, ValueStep::kEnd)));
}

}  // namespace
}  // namespace lyra::lir
