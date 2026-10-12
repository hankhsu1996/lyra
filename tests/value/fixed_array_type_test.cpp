#include "lyra/value/fixed_array_type.hpp"

#include <array>
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
#include "lyra/value/formation.hpp"
#include "lyra/value/integral.hpp"
#include "lyra/value/integral_value_type.hpp"
#include "lyra/value/integral_words.hpp"
#include "lyra/value/library_value_types.hpp"
#include "lyra/value/queue_type.hpp"
#include "lyra/value/reduction.hpp"
#include "lyra/value/runtime_fixed_array.hpp"
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

template <typename T>
auto ValueTypeOfElement() -> const ValueType& {
  if constexpr (IntegralValue<T>) {
    return IntegralValueType::Of<T>();
  } else {
    return LibraryTypeOf<T>();
  }
}

// The element type of `T`, stated once with a scratch of its own, as a unit
// states one for each element type it names.
template <typename T>
auto ElementTypeOf() -> const ElementType& {
  static ElementScratch scratch;
  static const ElementType element(
      ValueTypeOfElement<T>(), BuildDefault<T>, scratch);
  return element;
}

// An array of `N` elements, and the default of one, which is what an array of
// such arrays is told its element type starts as.
template <std::uint32_t N>
auto ArrayOfElements() -> const FixedArrayType& {
  static const FixedArrayType array(ElementTypeOf<Element>(), N);
  return array;
}

template <std::uint32_t N>
auto BuildArrayDefault(void* out) -> void* {
  ArrayOfElements<N>().BuildEach(out, [](std::size_t, void* element) {
    std::construct_at(static_cast<Element*>(element));
  });
  return out;
}

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
    -> RuntimeFixedArray {
  return RuntimeFixedArray::Built(type, [&](std::size_t i, void* element) {
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
class CountedElementType final : public ValueType {
 public:
  explicit CountedElementType(Lifecycle& counts)
      : ValueType(
            static_cast<std::uint32_t>(Inner().Size()),
            static_cast<std::uint32_t>(Inner().Align())),
        counts_(&counts) {
  }

  void Copy(const void* value, void* out) const override {
    ++counts_->copies;
    if (counts_->copies == counts_->failing_copy) {
      throw InternalError("CountedElementType: the copy the test fails");
    }
    Inner().Copy(value, out);
  }
  void Move(void* value, void* out) const noexcept override {
    Inner().Move(value, out);
  }
  void Destroy(void* value) const noexcept override {
    ++counts_->destroyed;
    Inner().Destroy(value);
  }
  void Assign(void* storage, const void* value) const override {
    Inner().Assign(storage, value);
  }

  [[nodiscard]] auto Equal(const void* lhs, const void* rhs) const
      -> FourStateBit override {
    return Inner().Equal(lhs, rhs);
  }
  [[nodiscard]] auto CaseEqual(const void* lhs, const void* rhs) const
      -> bool override {
    return Inner().CaseEqual(lhs, rhs);
  }
  [[nodiscard]] auto BitIdentical(const void* lhs, const void* rhs) const
      -> bool override {
    return Inner().BitIdentical(lhs, rhs);
  }
  [[nodiscard]] auto HasUnknown(const void* value) const -> bool override {
    return Inner().HasUnknown(value);
  }

  void BuildDefault(void* out) const override {
    Inner().BuildDefault(out);
  }

  void BitstreamWidth(const void* value, void* out) const override {
    Inner().BitstreamWidth(value, out);
  }
  void CountBits(
      const void* value, const ConstIntegralView& control_bits,
      void* out) const override {
    Inner().CountBits(value, control_bits, out);
  }
  auto WriteToStream(
      const void* value, const IntegralView& stream, std::uint64_t filled) const
      -> std::uint64_t override {
    return Inner().WriteToStream(value, stream, filled);
  }
  auto ReadFromStream(
      const ConstIntegralView& stream, std::uint64_t taken, void* out) const
      -> std::uint64_t override {
    return Inner().ReadFromStream(stream, taken, out);
  }

  void ResolveTriState(
      const void* lhs, const void* rhs, void* out) const override {
    Inner().ResolveTriState(lhs, rhs, out);
  }
  void ResolveWiredAnd(
      const void* lhs, const void* rhs, void* out) const override {
    Inner().ResolveWiredAnd(lhs, rhs, out);
  }
  void ResolveWiredOr(
      const void* lhs, const void* rhs, void* out) const override {
    Inner().ResolveWiredOr(lhs, rhs, out);
  }
  void Dominating(
      const void* stronger, const void* weaker, void* out) const override {
    Inner().Dominating(stronger, weaker, out);
  }
  void FilledLike(
      const void* prototype, const void* fill, void* out) const override {
    Inner().FilledLike(prototype, fill, out);
  }

  [[nodiscard]] auto OrderBefore(const void* lhs, const void* rhs) const
      -> bool override {
    return Inner().OrderBefore(lhs, rhs);
  }
  [[nodiscard]] auto IsTrue(const void* value) const -> bool override {
    return Inner().IsTrue(value);
  }
  void Reduce(Reduction reduction, const void* lhs, const void* rhs, void* out)
      const override {
    Inner().Reduce(reduction, lhs, rhs, out);
  }

  [[nodiscard]] auto Parts() const -> const PartsByPosition* override {
    return Inner().Parts();
  }
  [[nodiscard]] auto AsIntegral() const -> const IntegralValueType* override {
    return Inner().AsIntegral();
  }
  [[nodiscard]] auto AsFixedArray() const -> const FixedArrayType* override {
    return Inner().AsFixedArray();
  }

 private:
  [[nodiscard]] static auto Inner() -> const ValueType& {
    return IntegralValueType::Of<Element>();
  }

  Lifecycle* counts_;
};

TEST(FixedArrayType, ASmallArrayLiesWhereItIs) {
  const FixedArrayType type(ElementTypeOf<Element>(), 4);
  EXPECT_TRUE(type.LiesInPlace());
  EXPECT_EQ(type.Size(), 4 * sizeof(Element));
  EXPECT_EQ(type.Align(), alignof(Element));

  const Storage array(type);
  BuildCounting(type, array.Bytes(), 10);
  EXPECT_EQ(type.ElementAt(array.Bytes(), 0), array.Bytes());
  EXPECT_EQ(NumberAt(type, array.Bytes(), 3), 13);
  type.Destroy(array.Bytes());
}

TEST(FixedArrayType, ALargeArrayIsOneAddress) {
  const FixedArrayType type(ElementTypeOf<Element>(), 4000);
  EXPECT_FALSE(type.LiesInPlace());
  EXPECT_EQ(type.Size(), sizeof(void*));

  const Storage array(type);
  BuildCounting(type, array.Bytes(), 0);
  const Storage copy(type);
  type.Copy(array.Bytes(), copy.Bytes());
  EXPECT_NE(type.ElementAt(copy.Bytes(), 0), type.ElementAt(array.Bytes(), 0));
  EXPECT_EQ(NumberAt(type, copy.Bytes(), 100), 100);

  const void* run = type.ElementAt(copy.Bytes(), 0);
  const Storage moved(type);
  type.Move(copy.Bytes(), moved.Bytes());
  EXPECT_EQ(type.ElementAt(moved.Bytes(), 0), run);
  EXPECT_EQ(NumberAt(type, moved.Bytes(), 100), 100);
  EXPECT_EQ(*static_cast<void* const*>(copy.Bytes()), nullptr);

  // What was moved from holds no run, and is assigned and ended like any
  // other: the assignment obtains a run of its own.
  type.Assign(copy.Bytes(), array.Bytes());
  EXPECT_NE(type.ElementAt(copy.Bytes(), 0), type.ElementAt(moved.Bytes(), 0));
  EXPECT_EQ(NumberAt(type, copy.Bytes(), 7), 7);

  const Storage ended(type);
  type.Move(copy.Bytes(), ended.Bytes());
  type.Destroy(copy.Bytes());

  type.Destroy(ended.Bytes());
  type.Destroy(moved.Bytes());
  type.Destroy(array.Bytes());
}

TEST(FixedArrayType, AnArrayLyingInPlaceMovesElementByElement) {
  const FixedArrayType type(ElementTypeOf<String>(), 3);
  ASSERT_TRUE(type.LiesInPlace());
  // Long enough that each element owns storage of its own, which the move has
  // to hand over.
  const std::string text(200, 'm');
  const Storage array(type);
  type.BuildEach(array.Bytes(), [&](std::size_t, void* element) {
    std::construct_at(static_cast<String*>(element), text);
  });

  const Storage moved(type);
  type.Move(array.Bytes(), moved.Bytes());
  for (std::size_t i = 0; i < type.Count(); ++i) {
    EXPECT_EQ(
        static_cast<const String*>(type.ElementAt(moved.Bytes(), i))->View(),
        text);
  }
  EXPECT_EQ(type.ElementAt(moved.Bytes(), 0), moved.Bytes());

  // The elements moved from are still values, which their holder ends.
  type.Destroy(array.Bytes());
  type.Destroy(moved.Bytes());
}

TEST(FixedArrayType, AnAssignmentLeavesEveryElementWhereItLies) {
  for (const std::uint32_t count : {4U, 4000U}) {
    const FixedArrayType type(ElementTypeOf<Element>(), count);
    const Storage target(type);
    const Storage source(type);
    BuildCounting(type, target.Bytes(), 0);
    BuildCounting(type, source.Bytes(), 50);
    const void* before = type.ElementAt(target.Bytes(), 2);
    type.Assign(target.Bytes(), source.Bytes());
    EXPECT_EQ(type.ElementAt(target.Bytes(), 2), before);
    EXPECT_EQ(NumberAt(type, target.Bytes(), 2), 52);
    type.Destroy(target.Bytes());
    type.Destroy(source.Bytes());
  }
}

TEST(FixedArrayType, ABuildThatFailsEndsWhatItBuilt) {
  for (const std::uint32_t count : {5U, 4000U}) {
    static ElementScratch scratch;
    Lifecycle counts;
    const CountedElementType element(counts);
    const ElementType type_element(element, BuildDefault<Element>, scratch);
    const FixedArrayType type(type_element, count);
    const Storage source(type);
    BuildCounting(type, source.Bytes(), 0);

    counts.failing_copy = 3;
    const Storage target(type);
    EXPECT_THROW(type.Copy(source.Bytes(), target.Bytes()), InternalError);
    EXPECT_EQ(counts.copies, 3U);
    EXPECT_EQ(counts.destroyed, 2U);

    // A holder building its own copy ends the same two and keeps nothing.
    counts = Lifecycle{.copies = 0, .destroyed = 0, .failing_copy = 3};
    EXPECT_THROW(
        {
          const RuntimeFixedArray kept =
              RuntimeFixedArray::CopyOf(type, source.Bytes());
        },
        InternalError);
    EXPECT_EQ(counts.destroyed, 2U);

    counts = Lifecycle{};
    type.Destroy(source.Bytes());
    EXPECT_EQ(counts.destroyed, count);
  }
}

TEST(FixedArrayType, ComparesElementByElement) {
  const FixedArrayType type(ElementTypeOf<Element>(), 3);
  const Storage a(type);
  const Storage b(type);
  const Storage unknown(type);
  BuildCounting(type, a.Bytes(), 1);
  BuildCounting(type, b.Bytes(), 1);
  // An element at its default is all x.
  type.BuildEach(unknown.Bytes(), [](std::size_t, void* element) {
    std::construct_at(static_cast<Element*>(element));
  });

  EXPECT_EQ(type.Equal(a.Bytes(), b.Bytes()), FourStateBit::kOne);
  EXPECT_TRUE(type.CaseEqual(a.Bytes(), b.Bytes()));
  EXPECT_TRUE(type.BitIdentical(a.Bytes(), b.Bytes()));
  EXPECT_FALSE(type.HasUnknown(a.Bytes()));

  EXPECT_EQ(type.Equal(a.Bytes(), unknown.Bytes()), FourStateBit::kUnknown);
  EXPECT_FALSE(type.CaseEqual(a.Bytes(), unknown.Bytes()));
  EXPECT_TRUE(type.HasUnknown(unknown.Bytes()));

  *static_cast<Element*>(type.ElementAt(b.Bytes(), 2)) = Element::FromInt(9);
  EXPECT_EQ(type.Equal(a.Bytes(), b.Bytes()), FourStateBit::kZero);
  EXPECT_FALSE(type.BitIdentical(a.Bytes(), b.Bytes()));

  type.Destroy(a.Bytes());
  type.Destroy(b.Bytes());
  type.Destroy(unknown.Bytes());
}

TEST(FixedArrayType, TwoStatementsOfOneArrayTypeAreTheSameType) {
  const FixedArrayType four(ElementTypeOf<Element>(), 4);
  const FixedArrayType four_again(ElementTypeOf<Element>(), 4);
  const FixedArrayType five(ElementTypeOf<Element>(), 5);
  const FixedArrayType four_wider(ElementTypeOf<LogicVector<16>>(), 4);

  EXPECT_TRUE(SameType(four, four));
  EXPECT_TRUE(SameType(four, four_again));
  EXPECT_FALSE(SameType(four, five));
  EXPECT_FALSE(SameType(four, four_wider));
  EXPECT_FALSE(SameType(four, IntegralValueType::Of<Element>()));
  EXPECT_FALSE(SameType(IntegralValueType::Of<Element>(), four));

  // An array of arrays is the same type where its element arrays are, each
  // element array type stated with a scratch of its own.
  ElementScratch outer_scratch;
  ElementScratch outer_again_scratch;
  ElementScratch outer_of_five_scratch;
  const ElementType outer_element(four, BuildArrayDefault<4>, outer_scratch);
  const FixedArrayType outer(outer_element, 2);
  const ElementType outer_again_element(
      four_again, BuildArrayDefault<4>, outer_again_scratch);
  const FixedArrayType outer_again(outer_again_element, 2);
  const ElementType outer_of_five_element(
      five, BuildArrayDefault<5>, outer_of_five_scratch);
  const FixedArrayType outer_of_five(outer_of_five_element, 2);
  EXPECT_TRUE(SameType(outer, outer_again));
  EXPECT_FALSE(SameType(outer, outer_of_five));
}

TEST(FixedArrayType, AnIndexNamingNoElementLandsInTheDiscardedWrite) {
  const FixedArrayType type(ElementTypeOf<Element>(), 3);
  const Storage array(type);
  BuildCounting(type, array.Bytes(), 0);
  Formation formed = Formation::kMade;
  EXPECT_EQ(
      type.ElementRef(array.Bytes(), 2, formed),
      type.ElementAt(array.Bytes(), 2));
  EXPECT_EQ(formed, Formation::kExisting);
  for (const std::optional<std::int64_t> position :
       {std::optional<std::int64_t>{3}, std::optional<std::int64_t>{-1},
        std::optional<std::int64_t>{}}) {
    formed = Formation::kMade;
    EXPECT_EQ(
        type.ElementRef(array.Bytes(), position, formed),
        type.ElementType().DiscardedWrite());
    EXPECT_EQ(formed, Formation::kNowhere);
  }
  type.Destroy(array.Bytes());
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
      IntegralValueType::Of<Element>(), BuildCountedDefault, scratch);
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
  const FixedArrayType type(ElementTypeOf<Element>(), 5);
  const Storage array(type);
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

  type.Destroy(array.Bytes());
}

TEST(FixedArrayType, StreamsItsElementsFirstMostSignificant) {
  const FixedArrayType type(ElementTypeOf<Element>(), 3);
  const Storage array(type);
  BuildCounting(type, array.Bytes(), 1);

  LogicVector<24> stream = LogicVector<24>::Zero();
  EXPECT_EQ(
      type.WriteToStream(
          array.Bytes(), stream.AsMutableWords().MutableView(), 0),
      24U);
  EXPECT_EQ(stream.ToInt64(), 0x010203);

  // The read is told nothing but the stream, the type fixing every element's
  // width.
  const Storage read(type);
  EXPECT_EQ(type.ReadFromStream(stream.AsWords().View(), 0, read.Bytes()), 24U);
  EXPECT_TRUE(type.BitIdentical(array.Bytes(), read.Bytes()));

  Int width = Int::Zero();
  type.BitstreamWidth(array.Bytes(), &width);
  EXPECT_EQ(width.ToInt64(), 24);

  type.Destroy(array.Bytes());
  type.Destroy(read.Bytes());
}

TEST(FixedArrayType, HoldsElementsThatOwnStorage) {
  const FixedArrayType type(ElementTypeOf<String>(), 3);
  const std::string text(200, 'a');
  const Storage array(type);
  type.BuildEach(array.Bytes(), [&](std::size_t, void* element) {
    std::construct_at(static_cast<String*>(element), text);
  });
  const Storage copy(type);
  type.Copy(array.Bytes(), copy.Bytes());
  EXPECT_EQ(type.Equal(array.Bytes(), copy.Bytes()), FourStateBit::kOne);
  EXPECT_EQ(
      static_cast<const String*>(type.ElementAt(copy.Bytes(), 2))->View(),
      text);

  const PartsByPosition* parts = type.Parts();
  ASSERT_NE(parts, nullptr);
  EXPECT_EQ(parts->Count(array.Bytes()), 3U);
  EXPECT_EQ(parts->At(array.Bytes(), 1), type.ElementAt(array.Bytes(), 1));

  type.Destroy(array.Bytes());
  type.Destroy(copy.Bytes());
}

TEST(RuntimeFixedArray, HoldsACopyInStorageOfItsOwn) {
  for (const std::uint32_t count : {4U, 4000U}) {
    const FixedArrayType type(ElementTypeOf<Element>(), count);
    const Storage laid_out(type);
    BuildCounting(type, laid_out.Bytes(), 30);

    const RuntimeFixedArray copy =
        RuntimeFixedArray::CopyOf(type, laid_out.Bytes());
    EXPECT_EQ(&copy.Type(), &type);
    EXPECT_NE(copy.Bytes(), laid_out.Bytes());
    EXPECT_NE(
        type.ElementAt(copy.Bytes(), 0), type.ElementAt(laid_out.Bytes(), 0));
    EXPECT_EQ(NumberAt(type, copy.Bytes(), 3), 33);
    EXPECT_TRUE(copy.IsBitIdenticalTo(laid_out.Bytes()));

    const RuntimeFixedArray built = CountingArray(type, 30);
    EXPECT_EQ(&built.Type(), &type);
    EXPECT_TRUE(built.IsBitIdentical(copy));
    EXPECT_EQ(built == copy, FourStateBit::kOne);

    type.Destroy(laid_out.Bytes());
  }
}

TEST(RuntimeFixedArray, IsBuiltFromElementsTextAndBits) {
  const FixedArrayType type(ElementTypeOf<Element>(), 4);
  const std::array<Element, 4> values{
      Element::FromInt(7), Element::FromInt(8), Element::FromInt(9),
      Element::FromInt(10)};
  const std::array<const void*, 4> items{
      values.data(), &values[1], &values[2], &values[3]};

  const RuntimeFixedArray from_elements =
      RuntimeFixedArray::FromElements(type, items);
  EXPECT_EQ(NumberAt(type, from_elements.Bytes(), 0), 7);
  EXPECT_EQ(NumberAt(type, from_elements.Bytes(), 3), 10);
  // LRM 7.6: a source of another size is a run-time error.
  const std::array<const void*, 3> too_few{
      values.data(), &values[1], &values[2]};
  EXPECT_THROW(
      {
        const RuntimeFixedArray built =
            RuntimeFixedArray::FromElements(type, too_few);
      },
      SimulationError);

  // LRM 5.9: the text is left-justified and the elements past its end keep
  // the element default.
  const RuntimeFixedArray from_string =
      RuntimeFixedArray::FromString(type, String("AB"));
  EXPECT_EQ(NumberAt(type, from_string.Bytes(), 0), 'A');
  EXPECT_EQ(NumberAt(type, from_string.Bytes(), 1), 'B');
  EXPECT_TRUE(
      static_cast<const Element*>(type.ElementAt(from_string.Bytes(), 2))
          ->HasUnknown());
  EXPECT_TRUE(
      static_cast<const Element*>(type.ElementAt(from_string.Bytes(), 3))
          ->HasUnknown());

  const LogicVector<16> literal = LogicVector<16>::FromInt(0x4142);
  const RuntimeFixedArray from_integral =
      RuntimeFixedArray::FromIntegral(type, literal.AsWords().View());
  EXPECT_TRUE(from_integral.IsBitIdentical(from_string));

  // Text longer than the array loses what does not fit.
  const RuntimeFixedArray truncated =
      RuntimeFixedArray::FromString(type, String("ABCDEF"));
  EXPECT_EQ(NumberAt(type, truncated.Bytes(), 3), 'D');
}

TEST(RuntimeFixedArray, AnAssignmentKeepsEveryElementWhereItLies) {
  for (const std::uint32_t count : {4U, 4000U}) {
    const FixedArrayType type(ElementTypeOf<Element>(), count);
    // A second statement of the same type, as another unit would make.
    const FixedArrayType restated(ElementTypeOf<Element>(), count);
    RuntimeFixedArray held = CountingArray(type, 0);
    const void* bytes = held.Bytes();
    const void* third = type.ElementAt(held.Bytes(), 2);

    const RuntimeFixedArray source = CountingArray(restated, 50);
    held = source;
    EXPECT_EQ(held.Bytes(), bytes);
    EXPECT_EQ(type.ElementAt(held.Bytes(), 2), third);
    EXPECT_EQ(NumberAt(type, held.Bytes(), 2), 52);
    EXPECT_EQ(&held.Type(), &type);

    const Storage laid_out(type);
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

    type.Destroy(laid_out.Bytes());
  }
}

TEST(RuntimeFixedArray, LaysItsArrayOutWhereItIsAsked) {
  for (const std::uint32_t count : {4U, 4000U}) {
    const FixedArrayType type(ElementTypeOf<Element>(), count);
    RuntimeFixedArray held = CountingArray(type, 60);

    const Storage copied(type);
    EXPECT_EQ(held.CopyInto(copied.Bytes()), copied.Bytes());
    EXPECT_NE(
        type.ElementAt(copied.Bytes(), 0), type.ElementAt(held.Bytes(), 0));
    EXPECT_EQ(NumberAt(type, copied.Bytes(), 3), 63);
    EXPECT_EQ(NumberAt(type, held.Bytes(), 3), 63);

    const Storage moved(type);
    EXPECT_EQ(std::move(held).MoveInto(moved.Bytes()), moved.Bytes());
    EXPECT_EQ(NumberAt(type, moved.Bytes(), 3), 63);
    EXPECT_NE(
        type.ElementAt(moved.Bytes(), 0), type.ElementAt(copied.Bytes(), 0));

    type.Destroy(copied.Bytes());
    type.Destroy(moved.Bytes());
  }
}

TEST(ValueType, EveryTypeBuildsWhatItsStorageHoldsBeforeAnyWrite) {
  // LRM Table 6-7: x in every position of a four-state integral, 0 in every
  // position of a two-state one, and the empty string; an array with every
  // element at its element type's, and a queue holding nothing.
  Element logic = Element::FromInt(5);
  IntegralValueType::Of<Element>().BuildDefault(&logic);
  EXPECT_TRUE(logic.HasUnknown());
  EXPECT_TRUE(logic.CaseEqual(Element{}).IsTruthy());

  BitVector<8> bits = BitVector<8>::FromInt(5);
  IntegralValueType::Of<BitVector<8>>().BuildDefault(&bits);
  EXPECT_EQ(bits.ToInt64(), 0);

  const Storage text(LibraryTypeOf<String>());
  LibraryTypeOf<String>().BuildDefault(text.Bytes());
  EXPECT_TRUE(static_cast<const String*>(text.Bytes())->View().empty());
  LibraryTypeOf<String>().Destroy(text.Bytes());

  const FixedArrayType array(ElementTypeOf<Element>(), 2);
  const Storage pair(array);
  array.BuildDefault(pair.Bytes());
  for (std::size_t i = 0; i < 2; ++i) {
    const auto* element =
        static_cast<const Element*>(array.ElementAt(pair.Bytes(), i));
    EXPECT_TRUE(element->CaseEqual(Element{}).IsTruthy());
  }
  array.Destroy(pair.Bytes());

  const Storage queue(LibraryTypeOf<RuntimeQueue>());
  LibraryTypeOf<RuntimeQueue>().BuildDefault(queue.Bytes());
  EXPECT_EQ(static_cast<const RuntimeQueue*>(queue.Bytes())->Count(), 0U);
  LibraryTypeOf<RuntimeQueue>().Destroy(queue.Bytes());
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

  // A queue of another type keeps what that type's bound admits.
  const RuntimeQueue all = RuntimeQueue::FromElements(unbounded, items);
  EXPECT_EQ(all.Count(), 3U);
  const RuntimeQueue conformed = all.ConformBound(bounded);
  EXPECT_EQ(conformed.Count(), 2U);
  EXPECT_EQ(static_cast<const Element*>(conformed.ElementAt(1))->ToInt64(), 2);
  EXPECT_EQ(held.ConformBound(unbounded).Count(), 2U);
}

}  // namespace
}  // namespace lyra::value
