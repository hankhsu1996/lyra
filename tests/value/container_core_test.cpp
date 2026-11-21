#include "lyra/value/container_core.hpp"

#include <bit>
#include <cstddef>
#include <cstdint>
#include <gtest/gtest.h>
#include <memory>
#include <optional>
#include <span>
#include <vector>

#include "lyra/value/container_lease.hpp"
#include "lyra/value/integral.hpp"
#include "lyra/value/integral_value_type.hpp"
#include "lyra/value/layout.hpp"
#include "lyra/value/string.hpp"
#include "lyra/value/value_type.hpp"
#include "lyra/value/value_witnesses_of.hpp"

namespace {

using lyra::value::AssociativeArrayClear;
using lyra::value::AssociativeArrayCommit;
using lyra::value::AssociativeArrayConstruct;
using lyra::value::AssociativeArrayCore;
using lyra::value::AssociativeArrayEnd;
using lyra::value::AssociativeArrayFind;
using lyra::value::AssociativeArrayFirst;
using lyra::value::AssociativeArrayIndexOf;
using lyra::value::AssociativeArrayLease;
using lyra::value::AssociativeArrayNext;
using lyra::value::AssociativeArrayPrepare;
using lyra::value::AssociativeArrayPrepareDefault;
using lyra::value::AssociativeArrayRemove;
using lyra::value::AssociativeArraySetDefault;
using lyra::value::AssociativeArrayStatedDefault;
using lyra::value::ContainerLease;
using lyra::value::DynamicArrayAdopt;
using lyra::value::DynamicArrayConstruct;
using lyra::value::DynamicArrayCore;
using lyra::value::DynamicArrayFree;
using lyra::value::DynamicArrayGeneration;
using lyra::value::DynamicArrayLease;
using lyra::value::DynamicArrayLetGo;
using lyra::value::DynamicArrayPlace;
using lyra::value::IndexKind;
using lyra::value::IntegralValueType;
using lyra::value::LeaseCount;
using lyra::value::QueueCommit;
using lyra::value::QueueConstruct;
using lyra::value::QueueCore;
using lyra::value::QueueFreeRing;
using lyra::value::QueueLease;
using lyra::value::QueuePermute;
using lyra::value::QueueReverse;
using lyra::value::QueueUnlink;
using lyra::value::RecyclerAllocate;
using lyra::value::ValueType;
using lyra::value::ValueTypeKind;
using lyra::value::ValueWitnessTable;

// An element whose type counts how many times a value of it is ended, so that
// each is seen to be ended exactly once.
struct Counted {
  int value;
};

// The type's record, opening with what every type states, then the count.
struct CountedType {
  ValueType type;
  int* ended;
};

constexpr ValueWitnessTable kCountedWitnesses{
    .copy =
        [](const ValueType&, const void* value, void* out) {
          std::construct_at(
              static_cast<Counted*>(out), *static_cast<const Counted*>(value));
        },
    .relocate =
        [](const ValueType&, void* value, void* out) noexcept {
          std::construct_at(
              static_cast<Counted*>(out), *static_cast<Counted*>(value));
        },
    .destroy =
        [](const ValueType& type, void*) noexcept {
          ++*std::bit_cast<const CountedType*>(&type)->ended;
        },
    .assign =
        [](const ValueType&, void* storage, const void* value) {
          *static_cast<Counted*>(storage) = *static_cast<const Counted*>(value);
        },
    .bit_identical =
        [](const ValueType&, const void* lhs, const void* rhs) {
          return static_cast<const Counted*>(lhs)->value ==
                 static_cast<const Counted*>(rhs)->value;
        },
    .steps = nullptr};

auto ValueAt(const void* slot) -> int {
  return static_cast<const Counted*>(slot)->value;
}

// The storage of the element of `queue` at `position`, found as generated
// code finds it.
auto SlotAt(const QueueCore& queue, std::uint64_t position) -> void* {
  const std::span<void* const> ring(queue.ring, queue.mask + 1);
  return ring[(queue.head + position) & queue.mask];
}

// The element of `queue` at `position`.
auto QueueAt(const QueueCore& queue, std::uint64_t position) -> int {
  return ValueAt(SlotAt(queue, position));
}

// An element of `value` built in storage the queue's recycler hands out, its
// lease count started, as a queue's own code takes and builds one, and put at
// `position`.
void Commit(QueueCore& queue, std::uint64_t position, int value) {
  void* storage = RecyclerAllocate(queue.recycler);
  std::construct_at(static_cast<Counted*>(storage), Counted{.value = value});
  std::construct_at(
      static_cast<LeaseCount*>(lyra::value::Offset(
          storage,
          lyra::value::AlignTo(sizeof(Counted), alignof(LeaseCount)))));
  QueueCommit(queue, position, storage);
}

class ContainerCoreTest : public ::testing::Test {
 protected:
  int ended = 0;
  CountedType counted{
      .type = ValueType(
          kCountedWitnesses, sizeof(Counted), alignof(Counted),
          ValueTypeKind::kOther),
      .ended = &ended};
  const ValueType& counted_type = counted.type;
};

// Committing at either end and in the middle keeps the order however the ring
// wraps and grows, and taking an element out, reordering and reversing move
// only the addresses in the ring.
TEST_F(ContainerCoreTest, QueueKeepsOrderAcrossItsRing) {
  QueueCore queue = QueueConstruct(&counted_type);
  for (int i = 0; i < 10; ++i) {
    Commit(queue, queue.count, i);
    Commit(queue, 0, -i - 1);
  }
  Commit(queue, 10, 100);
  EXPECT_EQ(queue.count, 21U);
  EXPECT_EQ(QueueAt(queue, 0), -10);
  EXPECT_EQ(QueueAt(queue, 9), -1);
  EXPECT_EQ(QueueAt(queue, 10), 100);
  EXPECT_EQ(QueueAt(queue, 11), 0);
  EXPECT_EQ(QueueAt(queue, 20), 9);

  EXPECT_EQ(ValueAt(QueueUnlink(queue, 10)), 100);
  EXPECT_EQ(QueueAt(queue, 10), 0);

  const std::vector<std::uint64_t> reversed = [&] {
    std::vector<std::uint64_t> order;
    for (std::uint64_t i = queue.count; i-- > 0;) {
      order.push_back(i);
    }
    return order;
  }();
  QueuePermute(queue, reversed.data());
  EXPECT_EQ(QueueAt(queue, 0), 9);
  EXPECT_EQ(QueueAt(queue, 19), -10);
  QueueReverse(queue);
  EXPECT_EQ(QueueAt(queue, 0), -10);
  EXPECT_EQ(ended, 0);
  QueueFreeRing(queue);
}

// An element its queue let go of while a call leases it is left to the last
// lease, which then ends it once (LRM 13.5.2).
TEST_F(ContainerCoreTest, QueueLeavesALeasedElementToTheLastLease) {
  QueueCore queue = QueueConstruct(&counted_type);
  Commit(queue, 0, 8);
  void* last = SlotAt(queue, 0);
  std::optional<ContainerLease> lease(QueueLease(counted_type, last));
  EXPECT_EQ(QueueUnlink(queue, 0), last);
  static_cast<LeaseCount*>(
      lyra::value::Offset(
          last, lyra::value::AlignTo(sizeof(Counted), alignof(LeaseCount))))
      ->detached = true;
  EXPECT_EQ(queue.count, 0U);
  EXPECT_EQ(ended, 0);
  EXPECT_EQ(ValueAt(last), 8);
  lease.reset();
  EXPECT_EQ(ended, 1);
  QueueFreeRing(queue);
}

auto Generation(const DynamicArrayCore& array, const std::vector<int>& values)
    -> void* {
  void* data = DynamicArrayGeneration(array, values.size());
  for (std::size_t i = 0; i < values.size(); ++i) {
    std::construct_at(
        static_cast<Counted*>(DynamicArrayPlace(array, data, i)),
        Counted{.value = values[i]});
  }
  return data;
}

// What a dynamic array's own code does to let go of its elements: ends each,
// then has the core give their storage back, unless a call leases one of
// them, where the core keeps the generation for the last lease.
void LetGoOf(DynamicArrayCore& array) {
  if (!DynamicArrayLetGo(array)) {
    return;
  }
  for (std::uint64_t i = 0; i < array.count; ++i) {
    array.element->Destroy(DynamicArrayPlace(array, array.data, i));
  }
  DynamicArrayFree(array);
}

// A dynamic array's elements lie one after another; letting go of them while
// a call leases one of them keeps every element of that generation until the
// lease lets go (LRM 13.5.2).
TEST_F(ContainerCoreTest, DynamicArrayGenerationOutlivesItsLease) {
  DynamicArrayCore array = DynamicArrayConstruct(&counted_type);
  DynamicArrayAdopt(array, Generation(array, {1, 2, 3}), 3);
  EXPECT_EQ(array.count, 3U);
  EXPECT_EQ(ValueAt(lyra::value::Offset(array.data, 2 * sizeof(Counted))), 3);

  void* second = lyra::value::Offset(array.data, sizeof(Counted));
  std::optional<ContainerLease> lease(DynamicArrayLease(array, second));
  LetGoOf(array);
  EXPECT_EQ(array.count, 0U);
  DynamicArrayAdopt(array, Generation(array, {4}), 1);
  EXPECT_EQ(ended, 0);
  EXPECT_EQ(ValueAt(second), 2);
  lease.reset();
  EXPECT_EQ(ended, 3);

  LetGoOf(array);
  EXPECT_EQ(ended, 4);
  EXPECT_EQ(array.count, 0U);
  EXPECT_FALSE(DynamicArrayLetGo(array));
  DynamicArrayFree(array);
  EXPECT_EQ(ended, 4);
}

// The number an array's own code hands the core for the index kind `kind`.
constexpr auto KindNumber(IndexKind kind) -> std::int64_t {
  return static_cast<std::int64_t>(kind);
}

void Insert(AssociativeArrayCore& array, const void* index, int value) {
  void* element = AssociativeArrayPrepare(array);
  array.index->Copy(index, AssociativeArrayIndexOf(array, element));
  std::construct_at(static_cast<Counted*>(element), Counted{.value = value});
  AssociativeArrayCommit(array, element);
}

auto InOrder(const AssociativeArrayCore& array) -> std::vector<int> {
  std::vector<int> out;
  for (void* at = AssociativeArrayFirst(array); at != nullptr;
       at = AssociativeArrayNext(array, at)) {
    out.push_back(ValueAt(at));
  }
  return out;
}

// Integral indices order by their value at the index type's signedness (LRM
// 7.8.4), and an equal index finds the entry another one made.
TEST_F(ContainerCoreTest, AssociativeArrayOrdersSignedIndices) {
  using lyra::value::Int;
  const ValueType& int_type = IntegralValueType::Of<Int>().Type();
  AssociativeArrayCore array = AssociativeArrayConstruct(
      KindNumber(IndexKind::kIntegral), &int_type, &counted_type);
  for (const int key : {5, -3, 0, -100, 7}) {
    const Int index = Int::FromInt(key);
    Insert(array, &index, key);
  }
  EXPECT_EQ(InOrder(array), (std::vector<int>{-100, -3, 0, 5, 7}));
  const Int probe = Int::FromInt(-3);
  void* found = AssociativeArrayFind(array, &probe);
  ASSERT_NE(found, nullptr);
  EXPECT_EQ(ValueAt(found), -3);
  AssociativeArrayRemove(array, found);
  EXPECT_EQ(array.count, 4U);
  EXPECT_EQ(AssociativeArrayFind(array, &probe), nullptr);
  EXPECT_EQ(ended, 1);
  AssociativeArrayEnd(array);
  EXPECT_EQ(ended, 5);
}

// Strings order lexicographically (LRM 7.8.2), and an index of any other type
// keeps the order its entries were made in (LRM 7.8.5), each walked to and
// removed by where its element lies.
TEST_F(ContainerCoreTest, AssociativeArrayOrdersStringsAndKeepsOtherIndices) {
  using lyra::value::String;
  const ValueType& string_type = lyra::value::kValueTypeOf<String>;
  AssociativeArrayCore strings = AssociativeArrayConstruct(
      KindNumber(IndexKind::kString), &string_type, &counted_type);
  const String b("banana, long enough to own its storage");
  const String a("apple, long enough to own its storage");
  Insert(strings, &b, 2);
  Insert(strings, &a, 1);
  EXPECT_EQ(InOrder(strings), (std::vector<int>{1, 2}));
  AssociativeArrayEnd(strings);
  EXPECT_EQ(ended, 2);

  ended = 0;
  AssociativeArrayCore other = AssociativeArrayConstruct(
      KindNumber(IndexKind::kOther), &counted_type, &counted_type);
  for (const int key : {30, 10, 20}) {
    const Counted index{.value = key};
    Insert(other, &index, key + 1);
  }
  EXPECT_EQ(InOrder(other), (std::vector<int>{31, 11, 21}));
  void* middle = AssociativeArrayNext(other, AssociativeArrayFirst(other));
  EXPECT_EQ(ValueAt(AssociativeArrayIndexOf(other, middle)), 10);
  AssociativeArrayRemove(other, middle);
  EXPECT_EQ(InOrder(other), (std::vector<int>{31, 21}));
  EXPECT_EQ(ended, 2);
  AssociativeArrayClear(other);
  EXPECT_EQ(other.count, 0U);
  EXPECT_EQ(ended, 6);
  AssociativeArrayEnd(other);
  EXPECT_EQ(ended, 6);
}

// A deleted entry a call leases goes on existing until the lease lets go;
// the value a `default:` clause gives the array is its own, ended with it.
TEST_F(ContainerCoreTest, AssociativeArrayLeaseAndStatedDefault) {
  using lyra::value::Int;
  const ValueType& int_type = IntegralValueType::Of<Int>().Type();
  AssociativeArrayCore array = AssociativeArrayConstruct(
      KindNumber(IndexKind::kIntegral), &int_type, &counted_type);
  const Int index = Int::FromInt(4);
  Insert(array, &index, 40);
  void* entry = AssociativeArrayFind(array, &index);
  std::optional<ContainerLease> lease(AssociativeArrayLease(array, entry));
  AssociativeArrayRemove(array, entry);
  EXPECT_EQ(ended, 0);
  EXPECT_EQ(ValueAt(entry), 40);
  lease.reset();
  EXPECT_EQ(ended, 1);

  EXPECT_EQ(AssociativeArrayStatedDefault(array), nullptr);
  void* stated = AssociativeArrayPrepareDefault(array);
  std::construct_at(static_cast<Counted*>(stated), Counted{.value = 9});
  AssociativeArraySetDefault(array, stated);
  EXPECT_EQ(ValueAt(AssociativeArrayStatedDefault(array)), 9);
  AssociativeArrayEnd(array);
  EXPECT_EQ(ended, 2);
}

}  // namespace
