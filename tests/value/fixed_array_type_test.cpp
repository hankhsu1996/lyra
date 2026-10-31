#include "lyra/value/fixed_array_type.hpp"

#include <array>
#include <bit>
#include <cstddef>
#include <cstdint>
#include <gtest/gtest.h>
#include <memory>
#include <new>
#include <optional>
#include <string>
#include <tuple>
#include <utility>

#include "lyra/base/internal_error.hpp"
#include "lyra/base/simulation_error.hpp"
#include "lyra/value/any_value.hpp"
#include "lyra/value/element_type.hpp"
#include "lyra/value/fixed_array_value.hpp"
#include "lyra/value/formation.hpp"
#include "lyra/value/integral.hpp"
#include "lyra/value/integral_value_type.hpp"
#include "lyra/value/integral_words.hpp"
#include "lyra/value/library_value_types.hpp"
#include "lyra/value/queue_type.hpp"
#include "lyra/value/runtime_queue.hpp"
#include "lyra/value/string.hpp"
#include "lyra/value/value_type.hpp"

namespace lyra::value {
namespace {

using Element = LogicVector<8>;

template <typename T>
auto BuildDefault(void* out) -> void* {
  return std::construct_at(static_cast<T*>(out));
}

// The element type of `T`, stated once with a scratch of its own, as a unit
// states one for each element type it names.
template <typename T>
auto ElementTypeOf() -> const ElementType& {
  static ElementScratch scratch;
  static const ElementType element(
      LibraryTypeOf<T>(), BuildDefault<T>, scratch);
  return element;
}

// An operation no test here asks of a type stated below.
[[noreturn]] void Unused() {
  throw InternalError("an operation no test here asks for");
}

// A table asked nothing past a value's lifecycle, each of whose functions
// fails the test. The witnesses below state how their values are copied, moved,
// assigned and ended, answer what their tests ask, and take the rest from here.
struct NothingAsked {
  static constexpr auto BitIdentical = [](const ValueType&, const void*,
                                          const void*) -> bool { Unused(); };
  static constexpr const PartsWitnessTable* kParts = nullptr;
};

// The functions of an array type as a unit states them: each operation on the
// whole value is code compiled for it, which here is each element's operation
// in turn. The library's own part is what reaches an element, and that is
// what these tests hold to account.
struct ArrayWitnesses {
  static auto ArrayOf(const ValueType& type) -> const FixedArrayType& {
    return *type.AsFixedArray();
  }
  static auto ElementOf(const ValueType& type) -> const ValueType& {
    return ArrayOf(type).ElementType().Type();
  }
  template <typename Holds>
  static auto Every(const ValueType& type, Holds holds) -> bool {
    for (std::size_t i = 0; i < ArrayOf(type).Count(); ++i) {
      if (!holds(i)) {
        return false;
      }
    }
    return true;
  }

  static void Copy(const ValueType& type, const void* value, void* out) {
    const FixedArrayType& array = ArrayOf(type);
    array.BuildEach(out, [&](std::size_t i, void* element) {
      ElementOf(type).Copy(array.ElementAt(value, i), element);
    });
  }
  static void Move(const ValueType& type, void* value, void* out) noexcept {
    const FixedArrayType& array = ArrayOf(type);
    for (std::size_t i = 0; i < array.Count(); ++i) {
      ElementOf(type).Move(array.ElementAt(value, i), array.ElementAt(out, i));
    }
  }
  static void Destroy(const ValueType& type, void* value) noexcept {
    const FixedArrayType& array = ArrayOf(type);
    for (std::size_t i = 0; i < array.Count(); ++i) {
      ElementOf(type).Destroy(array.ElementAt(value, i));
    }
  }
  static void Assign(const ValueType& type, void* storage, const void* value) {
    const FixedArrayType& array = ArrayOf(type);
    for (std::size_t i = 0; i < array.Count(); ++i) {
      ElementOf(type).Assign(
          array.ElementAt(storage, i), array.ElementAt(value, i));
    }
  }
  static auto BitIdentical(
      const ValueType& type, const void* lhs, const void* rhs) -> bool {
    const FixedArrayType& array = ArrayOf(type);
    return Every(type, [&](std::size_t i) {
      return ElementOf(type).BitIdentical(
          array.ElementAt(lhs, i), array.ElementAt(rhs, i));
    });
  }

  static constexpr const PartsWitnessTable* kParts =
      &lyra_rt_fixed_array_parts_witness_table;
};

constexpr const ValueWitnessTable& kArrayWitnesses =
    kValueWitnessTableFor<ArrayWitnesses>;

// Storage for one array of `type`, holding none until the test builds one.
class Storage {
 public:
  explicit Storage(const ValueType& type)
      : align_(std::align_val_t{type.Align()}),
        bytes_(::operator new(type.Size(), align_)) {
  }
  Storage(const Storage&) = delete;
  auto operator=(const Storage&) -> Storage& = delete;
  Storage(Storage&&) = delete;
  auto operator=(Storage&&) -> Storage& = delete;
  ~Storage() {
    ::operator delete(bytes_, align_);
  }

  [[nodiscard]] auto Bytes() const -> void* {
    return bytes_;
  }

 private:
  std::align_val_t align_;
  void* bytes_;
};

// An array of `type` whose element at each position is the number `first`
// plus the position.
void BuildCounting(const FixedArrayType& type, void* out, std::int64_t first) {
  type.BuildEach(out, [&](std::size_t i, void* element) {
    std::construct_at(
        static_cast<Element*>(element),
        Element::FromInt(first + static_cast<std::int64_t>(i)));
  });
}

auto NumberAt(const FixedArrayType& type, const void* array, std::size_t i)
    -> std::int64_t {
  return static_cast<const Element*>(type.ElementAt(array, i))->ToInt64();
}

auto CountingArray(const FixedArrayType& type, std::int64_t first)
    -> LaidOutValue {
  return BuiltArray(type, [&](std::size_t i, void* element) {
    std::construct_at(
        static_cast<Element*>(element),
        Element::FromInt(first + static_cast<std::int64_t>(i)));
  });
}

struct Lifecycle {
  std::size_t copies = 0;
  std::size_t destroyed = 0;
  std::size_t failing_copy = 0;
};

// The byte element type, counting each copy and each ending it is asked for
// and failing the copy whose number `counts` names.
class CountedElementType {
 public:
  explicit CountedElementType(Lifecycle& counts);

  [[nodiscard]] auto Type() const -> const ValueType& {
    return type_;
  }

  static void Copy(const ValueType& type, const void* value, void* out) {
    Lifecycle& counts = CountsOf(type);
    ++counts.copies;
    if (counts.copies == counts.failing_copy) {
      throw InternalError("CountedElementType: the copy the test fails");
    }
    Inner().Copy(value, out);
  }
  static void Destroy(const ValueType& type, void* value) noexcept {
    ++CountsOf(type).destroyed;
    Inner().Destroy(value);
  }

 private:
  [[nodiscard]] static auto Inner() -> const ValueType& {
    return IntegralValueType::Of<Element>().Type();
  }
  [[nodiscard]] static auto CountsOf(const ValueType& type) -> Lifecycle& {
    return *std::bit_cast<const CountedElementType*>(&type)->counts_;
  }

  ValueType type_;
  Lifecycle* counts_;
};

// A copy and an ending are counted, a move and an assignment are the byte's
// own, and nothing else is asked of an element here.
struct CountedWitnesses : NothingAsked {
  static constexpr auto Copy = &CountedElementType::Copy;
  static constexpr auto Destroy = &CountedElementType::Destroy;
  static constexpr auto Move = [](const ValueType&, void* value,
                                  void* out) noexcept {
    IntegralValueType::Of<Element>().Type().Move(value, out);
  };
  static constexpr auto Assign = [](const ValueType&, void* storage,
                                    const void* value) {
    IntegralValueType::Of<Element>().Type().Assign(storage, value);
  };
};

CountedElementType::CountedElementType(Lifecycle& counts)
    : type_(
          kValueWitnessTableFor<CountedWitnesses>,
          static_cast<std::uint32_t>(Inner().Size()),
          static_cast<std::uint32_t>(Inner().Align()), ValueTypeKind::kOther),
      counts_(&counts) {
}

// An array is its elements one after another where the array lies, whatever
// its size.
TEST(FixedArrayType, AnArrayIsItsElementsWhereItLiesAtEverySize) {
  for (const std::uint32_t count : {4U, 4000U}) {
    const FixedArrayType type(kArrayWitnesses, ElementTypeOf<Element>(), count);
    EXPECT_EQ(type.Type().Size(), count * sizeof(Element));
    EXPECT_EQ(type.Type().Align(), alignof(Element));

    const Storage array(type.Type());
    BuildCounting(type, array.Bytes(), 10);
    EXPECT_EQ(type.ElementAt(array.Bytes(), 0), array.Bytes());
    EXPECT_EQ(NumberAt(type, array.Bytes(), 3), 13);
    type.Type().Destroy(array.Bytes());
  }
}

TEST(FixedArrayType, ABuildThatFailsEndsWhatItBuilt) {
  for (const std::uint32_t count : {5U, 4000U}) {
    static ElementScratch scratch;
    Lifecycle counts;
    const CountedElementType element(counts);
    const ElementType type_element(
        element.Type(), BuildDefault<Element>, scratch);
    const FixedArrayType type(kArrayWitnesses, type_element, count);
    const Storage source(type.Type());
    BuildCounting(type, source.Bytes(), 0);

    counts.failing_copy = 3;
    const Storage target(type.Type());
    EXPECT_THROW(
        type.Type().Copy(source.Bytes(), target.Bytes()), InternalError);
    EXPECT_EQ(counts.copies, 3U);
    EXPECT_EQ(counts.destroyed, 2U);

    // A holder building its own copy ends the same two and keeps nothing.
    counts = Lifecycle{.failing_copy = 3};
    EXPECT_THROW(
        {
          const LaidOutValue kept =
              LaidOutValue::CopyOf(type.Type(), source.Bytes());
        },
        InternalError);
    EXPECT_EQ(counts.destroyed, 2U);

    type.Type().Destroy(source.Bytes());
  }
}

// The program holds one description of each array type, so a type is the
// same as itself and as no other, whatever count and element the other states.
TEST(FixedArrayType, AnArrayTypeIsTheSameTypeOnlyAsItself) {
  const FixedArrayType four(kArrayWitnesses, ElementTypeOf<Element>(), 4);
  const FixedArrayType four_again(kArrayWitnesses, ElementTypeOf<Element>(), 4);
  const FixedArrayType five(kArrayWitnesses, ElementTypeOf<Element>(), 5);

  EXPECT_TRUE(SameType(four.Type(), four.Type()));
  EXPECT_FALSE(SameType(four.Type(), four_again.Type()));
  EXPECT_FALSE(SameType(four.Type(), five.Type()));
  EXPECT_FALSE(SameType(four.Type(), IntegralValueType::Of<Element>().Type()));
  EXPECT_FALSE(SameType(IntegralValueType::Of<Element>().Type(), four.Type()));
}

TEST(FixedArrayType, AnIndexNamingNoElementLandsInTheDiscardedWrite) {
  const FixedArrayType type(kArrayWitnesses, ElementTypeOf<Element>(), 3);
  const Storage array(type.Type());
  BuildCounting(type, array.Bytes(), 0);
  Formation formed = Formation::kMade;
  EXPECT_EQ(
      type.ElementMut(array.Bytes(), 2, formed),
      type.ElementAt(array.Bytes(), 2));
  EXPECT_EQ(formed, Formation::kExisting);
  for (const std::optional<std::int64_t> position :
       {std::optional<std::int64_t>{3}, std::optional<std::int64_t>{-1},
        std::optional<std::int64_t>{}}) {
    formed = Formation::kMade;
    EXPECT_EQ(
        type.ElementMut(array.Bytes(), position, formed),
        type.ElementType().DiscardedWrite());
    EXPECT_EQ(formed, Formation::kNowhere);
  }
  type.Type().Destroy(array.Bytes());
}

// How often the counted default below was built, and whether the next build
// fails.
struct DefaultBuilds {
  std::size_t count = 0;
  bool failing = false;
};

auto Builds() -> DefaultBuilds& {
  static DefaultBuilds builds;
  return builds;
}

auto BuildCountedDefault(void* out) -> void* {
  ++Builds().count;
  if (Builds().failing) {
    throw InternalError("BuildCountedDefault: the build the test fails");
  }
  return std::construct_at(static_cast<Element*>(out));
}

TEST(ElementType, BuildsItsDefaultOnceAndResetsTheDiscardedWrite) {
  static ElementScratch scratch;
  const ElementType element(
      IntegralValueType::Of<Element>().Type(), BuildCountedDefault, scratch);
  Builds() = DefaultBuilds{};

  // A build that fails leaves the scratch as it was, and the next one asked
  // for is built again.
  Builds().failing = true;
  EXPECT_THROW(std::ignore = element.Default(), InternalError);
  EXPECT_EQ(scratch.element_default, nullptr);
  Builds().failing = false;

  const void* first = element.Default();
  EXPECT_EQ(element.Default(), first);
  EXPECT_EQ(Builds().count, 2U);
  EXPECT_EQ(scratch.element_default, first);
  EXPECT_TRUE(static_cast<const Element*>(first)->HasUnknown());

  // A write that lands in the discarded write never shows through the next
  // one handed out, which is the same storage.
  auto* discarded = static_cast<Element*>(element.DiscardedWrite());
  *discarded = Element::FromInt(5);
  EXPECT_EQ(element.DiscardedWrite(), discarded);
  EXPECT_TRUE(discarded->HasUnknown());
  EXPECT_EQ(Builds().count, 2U);
}

TEST(FixedArrayType, ASliceAssignmentWritesTheElementsItReaches) {
  const FixedArrayType type(kArrayWitnesses, ElementTypeOf<Element>(), 5);
  const Storage array(type.Type());
  BuildCounting(type, array.Bytes(), 0);
  const std::array<Element, 3> values{
      Element::FromInt(20), Element::FromInt(21), Element::FromInt(22)};
  const std::array<const void*, 3> replacement{
      values.data(), &values[1], &values[2]};
  const void* second = type.ElementAt(array.Bytes(), 1);

  // Wholly inside the array: the window changes and nothing beside it does,
  // and writing the same values again changes nothing.
  EXPECT_TRUE(type.AssignSlice(array.Bytes(), 1, 3, replacement));
  EXPECT_EQ(NumberAt(type, array.Bytes(), 0), 0);
  EXPECT_EQ(NumberAt(type, array.Bytes(), 1), 20);
  EXPECT_EQ(NumberAt(type, array.Bytes(), 3), 22);
  EXPECT_EQ(NumberAt(type, array.Bytes(), 4), 4);
  EXPECT_EQ(type.ElementAt(array.Bytes(), 1), second);
  EXPECT_FALSE(type.AssignSlice(array.Bytes(), 1, 3, replacement));

  // Partly past either end: each element in the array takes the replacement
  // at its own place in the window.
  EXPECT_TRUE(type.AssignSlice(array.Bytes(), 3, 3, replacement));
  EXPECT_EQ(NumberAt(type, array.Bytes(), 3), 20);
  EXPECT_EQ(NumberAt(type, array.Bytes(), 4), 21);
  EXPECT_TRUE(type.AssignSlice(array.Bytes(), -2, 3, replacement));
  EXPECT_EQ(NumberAt(type, array.Bytes(), 0), 22);
  EXPECT_EQ(NumberAt(type, array.Bytes(), 1), 20);

  // Wholly outside, or from a start naming no position: nothing is written.
  EXPECT_FALSE(type.AssignSlice(array.Bytes(), 5, 3, replacement));
  EXPECT_FALSE(type.AssignSlice(array.Bytes(), std::nullopt, 3, replacement));
  EXPECT_EQ(NumberAt(type, array.Bytes(), 0), 22);
  EXPECT_EQ(NumberAt(type, array.Bytes(), 2), 21);
  EXPECT_EQ(NumberAt(type, array.Bytes(), 4), 21);

  type.Type().Destroy(array.Bytes());
}

TEST(FixedArrayType, HoldsElementsThatOwnStorage) {
  const FixedArrayType type(kArrayWitnesses, ElementTypeOf<String>(), 3);
  const std::string text(200, 'a');
  const Storage array(type.Type());
  type.BuildEach(array.Bytes(), [&](std::size_t, void* element) {
    std::construct_at(static_cast<String*>(element), text);
  });
  const Storage copy(type.Type());
  type.Type().Copy(array.Bytes(), copy.Bytes());
  for (std::size_t i = 0; i < type.Count(); ++i) {
    EXPECT_EQ(
        static_cast<const String*>(type.ElementAt(copy.Bytes(), i))->View(),
        text);
  }

  type.Type().Destroy(array.Bytes());
  type.Type().Destroy(copy.Bytes());
}

TEST(LaidOutValue, HoldsACopyInStorageOfItsOwn) {
  for (const std::uint32_t count : {4U, 4000U}) {
    const FixedArrayType type(kArrayWitnesses, ElementTypeOf<Element>(), count);
    const Storage laid_out(type.Type());
    BuildCounting(type, laid_out.Bytes(), 30);

    const LaidOutValue copy =
        LaidOutValue::CopyOf(type.Type(), laid_out.Bytes());
    EXPECT_EQ(copy.Type().AsFixedArray(), &type);
    EXPECT_NE(copy.Bytes(), laid_out.Bytes());
    EXPECT_NE(
        type.ElementAt(copy.Bytes(), 0), type.ElementAt(laid_out.Bytes(), 0));
    EXPECT_EQ(NumberAt(type, copy.Bytes(), 3), 33);
    EXPECT_TRUE(copy.IsBitIdenticalTo(laid_out.Bytes()));

    const LaidOutValue built = CountingArray(type, 30);
    EXPECT_EQ(built.Type().AsFixedArray(), &type);
    EXPECT_TRUE(built.IsBitIdentical(copy));

    type.Type().Destroy(laid_out.Bytes());
  }
}

TEST(LaidOutValue, IsBuiltFromElementsTextAndBits) {
  const FixedArrayType type(kArrayWitnesses, ElementTypeOf<Element>(), 4);
  const std::array<Element, 4> values{
      Element::FromInt(7), Element::FromInt(8), Element::FromInt(9),
      Element::FromInt(10)};
  const std::array<const void*, 4> items{
      values.data(), &values[1], &values[2], &values[3]};

  const LaidOutValue from_elements = ArrayFromElements(type, items);
  EXPECT_EQ(NumberAt(type, from_elements.Bytes(), 0), 7);
  EXPECT_EQ(NumberAt(type, from_elements.Bytes(), 3), 10);
  // LRM 7.6: a source of another size is a run-time error.
  const std::array<const void*, 3> too_few{
      values.data(), &values[1], &values[2]};
  EXPECT_THROW(
      { const LaidOutValue built = ArrayFromElements(type, too_few); },
      SimulationError);

  // LRM 5.9: the text is left-justified and the elements past its end keep
  // the element default.
  const LaidOutValue from_string = ArrayFromString(type, String("AB"));
  EXPECT_EQ(NumberAt(type, from_string.Bytes(), 0), 'A');
  EXPECT_EQ(NumberAt(type, from_string.Bytes(), 1), 'B');
  EXPECT_TRUE(
      static_cast<const Element*>(type.ElementAt(from_string.Bytes(), 2))
          ->HasUnknown());
  EXPECT_TRUE(
      static_cast<const Element*>(type.ElementAt(from_string.Bytes(), 3))
          ->HasUnknown());

  const LogicVector<16> literal = LogicVector<16>::FromInt(0x4142);
  const LaidOutValue from_integral =
      ArrayFromIntegral(type, literal.AsWords().View());
  EXPECT_TRUE(from_integral.IsBitIdentical(from_string));

  // Text longer than the array loses what does not fit.
  const LaidOutValue truncated = ArrayFromString(type, String("ABCDEF"));
  EXPECT_EQ(NumberAt(type, truncated.Bytes(), 3), 'D');
}

TEST(LaidOutValue, AnAssignmentKeepsEveryElementWhereItLies) {
  for (const std::uint32_t count : {4U, 4000U}) {
    const FixedArrayType type(kArrayWitnesses, ElementTypeOf<Element>(), count);
    LaidOutValue held = CountingArray(type, 0);
    const void* bytes = held.Bytes();
    const void* third = type.ElementAt(held.Bytes(), 2);

    const LaidOutValue source = CountingArray(type, 50);
    held = source;
    EXPECT_EQ(held.Bytes(), bytes);
    EXPECT_EQ(type.ElementAt(held.Bytes(), 2), third);
    EXPECT_EQ(NumberAt(type, held.Bytes(), 2), 52);
    EXPECT_EQ(held.Type().AsFixedArray(), &type);

    const Storage laid_out(type.Type());
    BuildCounting(type, laid_out.Bytes(), 80);
    EXPECT_FALSE(held.IsBitIdenticalTo(laid_out.Bytes()));
    held.AssignBytes(laid_out.Bytes());
    EXPECT_TRUE(held.IsBitIdenticalTo(laid_out.Bytes()));
    EXPECT_EQ(type.ElementAt(held.Bytes(), 2), third);
    EXPECT_EQ(NumberAt(type, held.Bytes(), 2), 82);

    // The bytes handed in may be the ones held.
    EXPECT_TRUE(held.IsBitIdenticalTo(held.Bytes()));
    held.AssignBytes(held.Bytes());
    EXPECT_EQ(NumberAt(type, held.Bytes(), 2), 82);

    type.Type().Destroy(laid_out.Bytes());
  }
}

TEST(LaidOutValue, LaysItsArrayOutWhereItIsAsked) {
  for (const std::uint32_t count : {4U, 4000U}) {
    const FixedArrayType type(kArrayWitnesses, ElementTypeOf<Element>(), count);
    LaidOutValue held = CountingArray(type, 60);

    const Storage copied(type.Type());
    EXPECT_EQ(held.CopyInto(copied.Bytes()), copied.Bytes());
    EXPECT_NE(
        type.ElementAt(copied.Bytes(), 0), type.ElementAt(held.Bytes(), 0));
    EXPECT_EQ(NumberAt(type, copied.Bytes(), 3), 63);
    EXPECT_EQ(NumberAt(type, held.Bytes(), 3), 63);

    const Storage moved(type.Type());
    EXPECT_EQ(std::move(held).MoveInto(moved.Bytes()), moved.Bytes());
    EXPECT_EQ(NumberAt(type, moved.Bytes(), 3), 63);
    EXPECT_NE(
        type.ElementAt(moved.Bytes(), 0), type.ElementAt(copied.Bytes(), 0));

    type.Type().Destroy(copied.Bytes());
    type.Type().Destroy(moved.Bytes());
  }
}

TEST(RuntimeQueue, HoldsTheBoundItsTypeStates) {
  const QueueType bounded(ElementTypeOf<Element>(), 1);
  const QueueType unbounded(ElementTypeOf<Element>(), -1);
  const std::array<Element, 3> values{
      Element::FromInt(1), Element::FromInt(2), Element::FromInt(3)};
  const std::array<const void*, 3> items{values.data(), &values[1], &values[2]};

  // LRM 7.10.5: no element past index 1 is kept, however the queue grows.
  RuntimeQueue held = RuntimeQueue::FromElements(bounded, items);
  EXPECT_EQ(held.Count(), 2U);
  held.PushBack(&values[2]);
  EXPECT_EQ(held.Count(), 2U);

  // A queue of a type with no bound keeps every element it is built over.
  EXPECT_EQ(RuntimeQueue::FromElements(unbounded, items).Count(), 3U);
}

}  // namespace
}  // namespace lyra::value
