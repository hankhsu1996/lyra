#include "lyra/value/container_core.hpp"

#include <algorithm>
#include <bit>
#include <cstddef>
#include <cstdint>
#include <functional>
#include <iterator>
#include <list>
#include <map>
#include <memory>
#include <new>
#include <span>
#include <unordered_map>
#include <utility>
#include <vector>

#include "lyra/base/internal_error.hpp"
#include "lyra/value/any_value.hpp"
#include "lyra/value/container_lease.hpp"
#include "lyra/value/integral.hpp"
#include "lyra/value/integral_value_type.hpp"
#include "lyra/value/integral_words.hpp"
#include "lyra/value/layout.hpp"
#include "lyra/value/object_ref.hpp"
#include "lyra/value/string.hpp"
#include "lyra/value/unmanaged_ref.hpp"
#include "lyra/value/value_type.hpp"
#include "lyra/value/wildcard_index.hpp"

namespace lyra::value {

namespace {

// Storage of `bytes` at `align`, from the allocation function that meets that
// alignment at least cost: the ordinary one where its own alignment does, so
// an element of an ordinary type costs one ordinary allocation.
auto Allocate(std::size_t bytes, std::size_t align) -> void* {
  if (align <= __STDCPP_DEFAULT_NEW_ALIGNMENT__) {
    return ::operator new(bytes);
  }
  return ::operator new(bytes, std::align_val_t{align});
}

void Deallocate(void* storage, std::size_t align) noexcept {
  if (align <= __STDCPP_DEFAULT_NEW_ALIGNMENT__) {
    ::operator delete(storage);
    return;
  }
  ::operator delete(storage, std::align_val_t{align});
}

// How an element a container keeps in storage of its own lies: the element
// first, so its address is the storage's, then the count of leases on it, then
// whatever the container keeps beside it.
struct Slot {
  std::size_t count_at;
  std::size_t after;
  std::size_t align;

  explicit Slot(const ValueType& element)
      : count_at(AlignTo(element.Size(), alignof(LeaseCount))),
        after(count_at + sizeof(LeaseCount)),
        align(std::max(element.Align(), alignof(LeaseCount))) {
  }

  [[nodiscard]] auto CountOf(void* slot) const -> LeaseCount& {
    return *static_cast<LeaseCount*>(Offset(slot, count_at));
  }
  [[nodiscard]] auto CountOf(const void* slot) const -> const LeaseCount& {
    return *static_cast<const LeaseCount*>(Offset(slot, count_at));
  }
};

auto NewSlot(const Slot& shape, std::size_t bytes) -> void* {
  void* slot = Allocate(bytes, shape.align);
  std::construct_at(&shape.CountOf(slot));
  return slot;
}

void DeleteSlot(const Slot& shape, void* slot) noexcept {
  std::destroy_at(&shape.CountOf(slot));
  Deallocate(slot, shape.align);
}

// How many pieces of storage a recycler cuts from one page.
constexpr std::size_t kStoragesPerPage = 64;

// The distance between two pieces of storage of `size` bytes at `align` in a
// page, and the alignment a page keeps: room and alignment for the link a free
// piece holds in its first word, whatever the piece's own.
struct StorageStride {
  std::size_t align;
  std::size_t bytes;

  StorageStride(std::uint64_t size, std::uint64_t alignment)
      : align(std::max<std::size_t>(alignment, alignof(void*))),
        bytes(AlignTo(std::max<std::size_t>(size, sizeof(void*)), align)) {
  }

  auto operator<=>(const StorageStride&) const = default;
};

// The run's recycler for storage of one stride, made where it is first asked
// for and kept, with the pages it took, until the program ends. A value with
// static storage may end after anything else does and give its queue's
// storage back then, so the recyclers are never destroyed, as a static whose
// end would do work is kept in Google's C++ style.
auto RecyclerOf(StorageStride stride) -> Recycler* {
  static auto& recyclers = *new std::map<StorageStride, Recycler>();
  return &recyclers
              .try_emplace(
                  stride,
                  Recycler{
                      .free_list = nullptr,
                      .size = stride.bytes,
                      .align = stride.align,
                      .next = nullptr,
                      .end = nullptr})
              .first->second;
}

// The recycler a queue's storage for elements of `element` comes from and goes
// back to.
auto RecyclerFor(const ValueType& element) -> Recycler* {
  const Slot shape(element);
  return RecyclerOf(StorageStride(shape.after, shape.align));
}

// A piece of storage given back to `recycler`, as LLVM's `Recycler` pushes it
// on its free list.
void PushFree(Recycler& recycler, void* storage) noexcept {
  *static_cast<void**>(storage) = recycler.free_list;
  recycler.free_list = storage;
}

// Gives back a queue's element storage holding no value; its lease count is
// started again where the storage is next taken.
void DeallocateQueueStorage(
    const ValueType& element, Recycler& recycler, void* storage) noexcept {
  std::destroy_at(&Slot(element).CountOf(storage));
  PushFree(recycler, storage);
}

// Ends an element taken out of a queue that a call leased, once the last
// lease lets it go, and gives its storage back to the recycler it came from.
void EndDetachedElement(void* storage, const void* element) {
  const auto& type = *static_cast<const ValueType*>(element);
  type.Destroy(storage);
  DeallocateQueueStorage(type, *RecyclerFor(type), storage);
}

// Lets go of an element its container holds no more: ends it, or detaches it
// where a call leases it, leaving that to the last lease (LRM 13.5.2).
void ReleaseSlot(const ValueType& element, void* slot) noexcept {
  const Slot shape(element);
  LeaseCount& count = shape.CountOf(slot);
  if (count.held > 0) {
    count.detached = true;
    return;
  }
  element.Destroy(slot);
  DeleteSlot(shape, slot);
}

auto RingSlots(const QueueCore& queue) -> std::span<void*> {
  return {queue.ring, queue.ring == nullptr ? 0 : queue.mask + 1};
}

auto RingAt(const QueueCore& queue, std::uint64_t position) -> void*& {
  return RingSlots(queue)[(queue.head + position) & queue.mask];
}

void FreeRing(QueueCore& queue) noexcept {
  if (queue.ring != nullptr) {
    Deallocate(static_cast<void*>(queue.ring), alignof(void*));
  }
}

}  // namespace

auto RecyclerAllocate(Recycler* recycler) -> void* {
  if (recycler->next == recycler->end) {
    const std::size_t bytes = recycler->size * kStoragesPerPage;
    void* page = Allocate(bytes, recycler->align);
    recycler->next = page;
    recycler->end = Offset(page, bytes);
  }
  void* storage = recycler->next;
  recycler->next = Offset(storage, recycler->size);
  return storage;
}

auto QueueConstruct(const ValueType* element) -> QueueCore {
  return QueueCore{
      .ring = nullptr,
      .mask = 0,
      .head = 0,
      .count = 0,
      .recycler = RecyclerFor(*element)};
}

void TakeCore(QueueCore& from, QueueCore& out) noexcept {
  out = from;
  from.ring = nullptr;
  from.mask = 0;
  from.head = 0;
  from.count = 0;
}

void QueueAdopt(QueueCore& queue, QueueCore& from) noexcept {
  QueueFreeRing(queue);
  TakeCore(from, queue);
}

void QueueFreeRing(QueueCore& queue) noexcept {
  FreeRing(queue);
  queue.ring = nullptr;
  queue.mask = 0;
  queue.head = 0;
}

void QueueReserve(QueueCore& queue, std::uint64_t count) {
  if (queue.ring != nullptr && count <= queue.mask + 1) {
    return;
  }
  const std::uint64_t length = std::bit_ceil(std::max<std::uint64_t>(count, 4));
  auto* grown =
      static_cast<void**>(Allocate(length * sizeof(void*), alignof(void*)));
  const std::span<void*> into(grown, length);
  for (std::uint64_t i = 0; i < queue.count; ++i) {
    into[i] = RingAt(queue, i);
  }
  FreeRing(queue);
  queue.ring = grown;
  queue.mask = length - 1;
  queue.head = 0;
}

// Places the storage, moving whichever side of `position` is shorter.
void QueueCommit(QueueCore& queue, std::uint64_t position, void* storage) {
  if (position > queue.count) {
    throw InternalError(
        "an element is committed past the end of a queue -- please report "
        "this as a bug");
  }
  QueueReserve(queue, queue.count + 1);
  if (position < queue.count - position) {
    queue.head = (queue.head + queue.mask) & queue.mask;
    for (std::uint64_t i = 0; i < position; ++i) {
      RingAt(queue, i) = RingAt(queue, i + 1);
    }
  } else {
    for (std::uint64_t i = queue.count; i > position; --i) {
      RingAt(queue, i) = RingAt(queue, i - 1);
    }
  }
  RingAt(queue, position) = storage;
  ++queue.count;
}

// Closes the gap moving whichever side of `position` is shorter.
auto QueueUnlink(QueueCore& queue, std::uint64_t position) noexcept -> void* {
  void* storage = RingAt(queue, position);
  if (position < queue.count - 1 - position) {
    for (std::uint64_t i = position; i > 0; --i) {
      RingAt(queue, i) = RingAt(queue, i - 1);
    }
    queue.head = (queue.head + 1) & queue.mask;
  } else {
    for (std::uint64_t i = position; i + 1 < queue.count; ++i) {
      RingAt(queue, i) = RingAt(queue, i + 1);
    }
  }
  --queue.count;
  return storage;
}

void QueuePermute(QueueCore& queue, const std::uint64_t* order) {
  const std::span<const std::uint64_t> from(order, queue.count);
  std::vector<void*> slots;
  slots.reserve(from.size());
  for (const std::uint64_t at : from) {
    slots.push_back(RingAt(queue, at));
  }
  for (std::uint64_t k = 0; k < slots.size(); ++k) {
    RingAt(queue, k) = slots[k];
  }
}

void QueueApplyOrder(QueueCore& queue, const DynamicArrayCore& order) {
  QueuePermute(queue, static_cast<const std::uint64_t*>(order.data));
}

auto QueueGetUnchecked(const QueueCore& queue, std::uint64_t index) -> void* {
  return RingAt(queue, index);
}

void QueueReverse(QueueCore& queue) noexcept {
  for (std::uint64_t low = 0, high = queue.count; low + 1 < high;
       ++low, --high) {
    std::swap(RingAt(queue, low), RingAt(queue, high - 1));
  }
}

auto QueueLease(const ValueType& element, void* storage) -> ContainerLease {
  return {
      Slot(element).CountOf(storage), storage, &EndDetachedElement, &element};
}

namespace {

// What a dynamic array's elements are followed by: the leases on them, and,
// for whoever ends them, where they start and how many there are.
struct Generation {
  LeaseCount leases;
  std::byte* first;
  std::uint64_t count;
};

auto GenerationAt(const ValueType& element, std::uint64_t count)
    -> std::size_t {
  return AlignTo(element.Size() * count, alignof(Generation));
}

auto GenerationAlign(const ValueType& element) -> std::size_t {
  return std::max(element.Align(), alignof(Generation));
}

auto GenerationOf(const ValueType& element, void* data, std::uint64_t count)
    -> Generation& {
  return *static_cast<Generation*>(Offset(data, GenerationAt(element, count)));
}

void EndGeneration(const ValueType& element, Generation& generation) noexcept {
  std::byte* first = generation.first;
  for (std::uint64_t i = generation.count; i-- > 0;) {
    element.Destroy(Offset(static_cast<void*>(first), i * element.Size()));
  }
  std::destroy_at(&generation);
  Deallocate(first, GenerationAlign(element));
}

void EndDetachedGeneration(void* generation, const void* element) {
  EndGeneration(
      *static_cast<const ValueType*>(element),
      *static_cast<Generation*>(generation));
}

}  // namespace

auto DynamicArrayConstruct(const ValueType* element) -> DynamicArrayCore {
  return DynamicArrayCore{.data = nullptr, .count = 0, .element = element};
}

void TakeCore(DynamicArrayCore& from, DynamicArrayCore& out) noexcept {
  out = from;
  from.data = nullptr;
  from.count = 0;
}

auto DynamicArrayLetGo(DynamicArrayCore& array) noexcept -> bool {
  if (array.data == nullptr) {
    return false;
  }
  Generation& generation =
      GenerationOf(*array.element, array.data, array.count);
  if (generation.leases.held == 0) {
    return true;
  }
  generation.leases.detached = true;
  array.data = nullptr;
  array.count = 0;
  return false;
}

void DynamicArrayFree(DynamicArrayCore& array) noexcept {
  if (array.data == nullptr) {
    return;
  }
  Generation& generation =
      GenerationOf(*array.element, array.data, array.count);
  std::byte* first = generation.first;
  std::destroy_at(&generation);
  Deallocate(first, GenerationAlign(*array.element));
  array.data = nullptr;
  array.count = 0;
}

auto DynamicArrayGeneration(const DynamicArrayCore& array, std::uint64_t count)
    -> void* {
  if (count == 0) {
    return nullptr;
  }
  const ValueType& element = *array.element;
  auto* data = static_cast<std::byte*>(Allocate(
      GenerationAt(element, count) + sizeof(Generation),
      GenerationAlign(element)));
  std::construct_at(
      &GenerationOf(element, data, count),
      Generation{.leases = {}, .first = data, .count = count});
  return data;
}

auto DynamicArrayPlace(
    const DynamicArrayCore& array, void* data, std::uint64_t index) -> void* {
  return Offset(data, index * array.element->Size());
}

void DynamicArrayAdopt(
    DynamicArrayCore& array, void* data, std::uint64_t count) noexcept {
  array.data = data;
  array.count = data == nullptr ? 0 : count;
}

auto DynamicArrayLease(const DynamicArrayCore& array, void* element)
    -> ContainerLease {
  const void* first = array.data;
  const void* end = first == nullptr
                        ? nullptr
                        : Offset(first, array.count * array.element->Size());
  if (first == nullptr || std::less{}(element, first) ||
      !std::less{}(element, end)) {
    throw InternalError(
        "a lease is taken on an element a dynamic array does not hold -- "
        "please report this as a bug");
  }
  Generation& generation =
      GenerationOf(*array.element, array.data, array.count);
  return {
      generation.leases, &generation, &EndDetachedGeneration, array.element};
}

namespace {

// How an entry of an associative array lies: its element first, so the
// entry's address is the element's, then the leases on it, then its index's
// type, then the index at that type's alignment.
struct EntryShape {
  Slot slot;
  std::size_t type_at;
  std::size_t index_at;
  std::size_t size;
  std::size_t align;

  EntryShape(const ValueType& element, const ValueType& index)
      : slot(element),
        type_at(AlignTo(slot.after, alignof(const ValueType*))),
        index_at(AlignTo(type_at + sizeof(const ValueType*), index.Align())),
        size(index_at + index.Size()),
        align(std::max(slot.align, index.Align())) {
  }
};

auto IndexTypeOf(const ValueType& element, const void* entry)
    -> const ValueType& {
  const Slot slot(element);
  return **static_cast<const ValueType* const*>(
      Offset(entry, AlignTo(slot.after, alignof(const ValueType*))));
}

auto IndexIn(const ValueType& element, void* entry) -> void* {
  return Offset(
      entry, EntryShape(element, IndexTypeOf(element, entry)).index_at);
}
auto IndexIn(const ValueType& element, const void* entry) -> const void* {
  return Offset(
      entry, EntryShape(element, IndexTypeOf(element, entry)).index_at);
}

// Ends an entry: its index, its element, then its storage.
void EndEntry(const ValueType& element, void* entry) noexcept {
  const ValueType& index = IndexTypeOf(element, entry);
  const EntryShape shape(element, index);
  index.Destroy(IndexIn(element, entry));
  element.Destroy(entry);
  std::destroy_at(&shape.slot.CountOf(entry));
  Deallocate(entry, shape.align);
}

void EndDetachedEntry(void* entry, const void* element) {
  EndEntry(*static_cast<const ValueType*>(element), entry);
}

void ReleaseEntry(const ValueType& element, void* entry) noexcept {
  LeaseCount& count = Slot(element).CountOf(entry);
  if (count.held > 0) {
    count.detached = true;
    return;
  }
  EndEntry(element, entry);
}

// An index where it lies, with the type it is a value of: what an entry holds
// and what a lookup is handed.
struct Index {
  const void* bytes;
  const ValueType* type;
};

auto IndexOfEntry(const ValueType& element, const void* entry) -> Index {
  return {
      .bytes = IndexIn(element, entry), .type = &IndexTypeOf(element, entry)};
}

// An index read as the number the order compares (LRM 7.8.4): its value at
// its type's signedness, a signed one offset so that it orders as an unsigned
// word does.
auto NarrowKey(Index index) -> std::uint64_t {
  const IntegralValueType& type = *index.type->AsIntegral();
  std::uint64_t value = WordAt(type.Load(index.bytes).View().value, 0);
  if (type.Shape().signedness == Signedness::kSigned) {
    const auto unused = static_cast<unsigned>(64 - type.Shape().width);
    value = std::bit_cast<std::uint64_t>(
        std::bit_cast<std::int64_t>(value << unused) >> unused);
    value ^= std::uint64_t{1} << 63U;
  }
  return value;
}

auto WideKey(Index index) -> LoadedWords {
  return index.type->AsIntegral()->Load(index.bytes);
}
struct WideLess {
  auto operator()(const LoadedWords& a, const LoadedWords& b) const -> bool {
    return Less(a.View(), b.View()) == FourStateBit::kOne;
  }
};

auto StringKey(Index index) -> const String* {
  return static_cast<const String*>(index.bytes);
}
struct StringLess {
  auto operator()(const String* a, const String* b) const -> bool {
    return static_cast<bool>(*a < *b);
  }
};

// LRM 7.8.3, 6.14: a class handle and a chandle order by which object or
// address they name, which no operator of the language states; the order is
// the core's and stays put within a run.
auto HandleKey(Index index) -> const void* {
  return static_cast<const ObjectRef*>(index.bytes)->Handle().Share().get();
}
auto ChandleKey(Index index) -> const void* {
  return static_cast<const UnmanagedRef*>(index.bytes)->Address();
}

// LRM 7.8.1: a wildcard index is a value of the integral type it was written
// in, read as the unsigned number it names.
auto WildcardKey(Index index) -> std::vector<std::uint64_t> {
  const auto& written = *static_cast<const AnyValue*>(index.bytes);
  return WildcardIndexWords(
      written.Type().AsIntegral()->Load(written.Bytes()).View());
}
struct WildcardLess {
  auto operator()(
      const std::vector<std::uint64_t>& a,
      const std::vector<std::uint64_t>& b) const -> bool {
    return WildcardIndexBefore(a, b);
  }
};

// The entries of an associative array, in its index order, or for an index of
// a kind whose order the core does not know, in the order they were made.
class Entries {
 public:
  Entries() = default;
  Entries(const Entries&) = delete;
  auto operator=(const Entries&) -> Entries& = delete;
  Entries(Entries&&) = delete;
  auto operator=(Entries&&) -> Entries& = delete;
  virtual ~Entries() = default;

  virtual void Insert(void* entry) = 0;
  virtual void Remove(const void* entry) = 0;
  // Forgets every entry, answering them.
  virtual auto TakeAll() -> std::vector<void*> = 0;
  [[nodiscard]] virtual auto First() const -> void* = 0;
  [[nodiscard]] virtual auto Last() const -> void* = 0;
  [[nodiscard]] virtual auto Next(const void* entry) const -> void* = 0;
  [[nodiscard]] virtual auto Previous(const void* entry) const -> void* = 0;
  // The same entries as an index kind whose order the core knows keeps them,
  // and none where the core knows no order of the index type.
  [[nodiscard]] virtual auto Ordered() const -> const class OrderedIndices* {
    return nullptr;
  }
};

// The entries of an index kind whose order the core knows, which also finds
// the entry an index names and the ones on either side of it.
class OrderedIndices : public Entries {
 public:
  [[nodiscard]] auto Ordered() const -> const OrderedIndices* final {
    return this;
  }
  [[nodiscard]] virtual auto Find(Index index) const -> void* = 0;
  [[nodiscard]] virtual auto After(Index index) const -> void* = 0;
  [[nodiscard]] virtual auto Before(Index index) const -> void* = 0;
};

// The entries of an index kind whose order is fixed, kept in that order by
// the key `KeyOf` reads off each index.
template <typename Key, typename Less, Key (*KeyOf)(Index)>
class OrderedEntries final : public OrderedIndices {
 public:
  explicit OrderedEntries(const ValueType& element) : element_(&element) {
  }

  [[nodiscard]] auto Find(Index index) const -> void* override {
    const auto found = map_.find(KeyOf(index));
    return found == map_.end() ? nullptr : found->second;
  }
  void Insert(void* entry) override {
    where_.emplace(
        entry,
        map_.emplace(KeyOf(IndexOfEntry(*element_, entry)), entry).first);
  }
  void Remove(const void* entry) override {
    const auto found = Where(entry);
    map_.erase(found->second);
    where_.erase(found);
  }
  auto TakeAll() -> std::vector<void*> override {
    std::vector<void*> all;
    all.reserve(map_.size());
    for (const auto& [key, entry] : map_) {
      all.push_back(entry);
    }
    map_.clear();
    where_.clear();
    return all;
  }
  [[nodiscard]] auto First() const -> void* override {
    return map_.empty() ? nullptr : map_.begin()->second;
  }
  [[nodiscard]] auto Last() const -> void* override {
    return map_.empty() ? nullptr : map_.rbegin()->second;
  }
  [[nodiscard]] auto Next(const void* entry) const -> void* override {
    const auto after = std::next(Where(entry)->second);
    return after == map_.end() ? nullptr : after->second;
  }
  [[nodiscard]] auto Previous(const void* entry) const -> void* override {
    const auto at = Where(entry)->second;
    return at == map_.begin() ? nullptr : std::prev(at)->second;
  }
  [[nodiscard]] auto After(Index index) const -> void* override {
    const auto found = map_.upper_bound(KeyOf(index));
    return found == map_.end() ? nullptr : found->second;
  }
  [[nodiscard]] auto Before(Index index) const -> void* override {
    const auto found = map_.lower_bound(KeyOf(index));
    return found == map_.begin() ? nullptr : std::prev(found)->second;
  }

 private:
  using Map = std::map<Key, void*, Less>;

  // Where the entry `entry` lies in the order, which a step from it reads
  // without building its key again.
  [[nodiscard]] auto Where(const void* entry) const ->
      typename std::unordered_map<
          const void*, typename Map::iterator>::const_iterator {
    const auto found = where_.find(entry);
    if (found == where_.end()) {
      throw InternalError(
          "an associative array is asked the entry beside one it does not "
          "hold -- please report this as a bug");
    }
    return found;
  }

  const ValueType* element_;
  Map map_;
  std::unordered_map<const void*, typename Map::iterator> where_;
};

// The entries of an index type with no order the core knows, in the order
// they were made (LRM 7.8.5). The array type's own code finds an entry by its
// index, comparing each by the index type's equality, so nothing here reads an
// index.
class EntriesInOrderMade final : public Entries {
 public:
  void Insert(void* entry) override {
    entries_.push_back(entry);
    where_.emplace(entry, std::prev(entries_.end()));
  }
  void Remove(const void* entry) override {
    const auto found = Where(entry);
    entries_.erase(found->second);
    where_.erase(found);
  }
  auto TakeAll() -> std::vector<void*> override {
    std::vector<void*> all(entries_.begin(), entries_.end());
    entries_.clear();
    where_.clear();
    return all;
  }
  [[nodiscard]] auto First() const -> void* override {
    return entries_.empty() ? nullptr : entries_.front();
  }
  [[nodiscard]] auto Last() const -> void* override {
    return entries_.empty() ? nullptr : entries_.back();
  }
  [[nodiscard]] auto Next(const void* entry) const -> void* override {
    const auto after = std::next(Where(entry)->second);
    return after == entries_.end() ? nullptr : *after;
  }
  [[nodiscard]] auto Previous(const void* entry) const -> void* override {
    const auto at = Where(entry)->second;
    return at == entries_.begin() ? nullptr : *std::prev(at);
  }

 private:
  using Order = std::list<void*>;

  // Where the entry `entry` lies in the order, which a step from it reads in
  // constant time.
  [[nodiscard]] auto Where(const void* entry) const
      -> std::unordered_map<const void*, Order::iterator>::const_iterator {
    const auto found = where_.find(entry);
    if (found == where_.end()) {
      throw InternalError(
          "an associative array is asked the entry beside one it does not "
          "hold -- please report this as a bug");
    }
    return found;
  }

  Order entries_;
  std::unordered_map<const void*, Order::iterator> where_;
};

auto MakeEntries(
    IndexKind kind, const ValueType& index, const ValueType& element)
    -> std::unique_ptr<Entries> {
  switch (kind) {
    case IndexKind::kIntegral:
      if (index.AsIntegral()->Shape().width <= 64) {
        return std::make_unique<
            OrderedEntries<std::uint64_t, std::less<>, &NarrowKey>>(element);
      }
      return std::make_unique<OrderedEntries<LoadedWords, WideLess, &WideKey>>(
          element);
    case IndexKind::kString:
      return std::make_unique<
          OrderedEntries<const String*, StringLess, &StringKey>>(element);
    case IndexKind::kClassHandle:
      return std::make_unique<
          OrderedEntries<const void*, std::less<>, &HandleKey>>(element);
    case IndexKind::kChandle:
      return std::make_unique<
          OrderedEntries<const void*, std::less<>, &ChandleKey>>(element);
    case IndexKind::kWildcard:
      return std::make_unique<OrderedEntries<
          std::vector<std::uint64_t>, WildcardLess, &WildcardKey>>(element);
    case IndexKind::kOther:
      return std::make_unique<EntriesInOrderMade>();
  }
  std::unreachable();
}

auto EntriesOf(const AssociativeArrayCore& array) -> Entries& {
  return *static_cast<Entries*>(array.entries);
}

// The entries of an array whose index kind has an order the core knows, which
// alone is handed an index to find.
auto OrderedIndicesOf(const AssociativeArrayCore& array)
    -> const OrderedIndices& {
  const OrderedIndices* ordered = EntriesOf(array).Ordered();
  if (ordered == nullptr) {
    throw InternalError(
        "the core of an associative array is handed an index of a type whose "
        "order it does not know -- please report this as a bug");
  }
  return *ordered;
}

}  // namespace

auto AssociativeArrayConstruct(
    std::int64_t kind, const ValueType* index, const ValueType* element)
    -> AssociativeArrayCore {
  if (kind < 0 || kind > static_cast<std::int64_t>(IndexKind::kOther)) {
    throw InternalError(
        "an associative array is built over a kind of index no kind names -- "
        "please report this as a bug");
  }
  return AssociativeArrayCore{
      .entries =
          MakeEntries(static_cast<IndexKind>(kind), *index, *element).release(),
      .count = 0,
      .element = element,
      .index = index,
      .stated_default = nullptr,
      .kind = static_cast<IndexKind>(kind)};
}

void TakeCore(AssociativeArrayCore& from, AssociativeArrayCore& out) noexcept {
  out = from;
  from.entries = nullptr;
  from.count = 0;
  from.stated_default = nullptr;
}

void AssociativeArrayEnd(AssociativeArrayCore& array) noexcept {
  if (array.entries != nullptr) {
    AssociativeArrayClear(array);
    const std::unique_ptr<Entries> owned(static_cast<Entries*>(array.entries));
    array.entries = nullptr;
  }
  if (array.stated_default != nullptr) {
    ReleaseSlot(*array.element, array.stated_default);
    array.stated_default = nullptr;
  }
}

void AssociativeArrayAdopt(
    AssociativeArrayCore& array, AssociativeArrayCore& from) noexcept {
  AssociativeArrayEnd(array);
  TakeCore(from, array);
}

auto AssociativeArrayFind(const AssociativeArrayCore& array, const void* index)
    -> void* {
  return OrderedIndicesOf(array).Find({.bytes = index, .type = array.index});
}

// The entry holds the index type beside its index, so that a lease ending it
// after the array that made it has ended knows how (LRM 13.5.2).
auto AssociativeArrayPrepare(const AssociativeArrayCore& array) -> void* {
  const EntryShape shape(*array.element, *array.index);
  void* entry = NewSlot(shape.slot, shape.size);
  *static_cast<const ValueType**>(Offset(entry, shape.type_at)) = array.index;
  return entry;
}

auto AssociativeArrayIndexOf(const AssociativeArrayCore& array, void* element)
    -> void* {
  return IndexIn(*array.element, element);
}

void AssociativeArrayCommit(AssociativeArrayCore& array, void* element) {
  EntriesOf(array).Insert(element);
  ++array.count;
}

void AssociativeArrayRemove(
    AssociativeArrayCore& array, void* element) noexcept {
  EntriesOf(array).Remove(element);
  --array.count;
  ReleaseEntry(*array.element, element);
}

void AssociativeArrayClear(AssociativeArrayCore& array) noexcept {
  for (void* entry : EntriesOf(array).TakeAll()) {
    ReleaseEntry(*array.element, entry);
  }
  array.count = 0;
}

auto AssociativeArrayFirst(const AssociativeArrayCore& array) -> void* {
  return EntriesOf(array).First();
}

auto AssociativeArrayLast(const AssociativeArrayCore& array) -> void* {
  return EntriesOf(array).Last();
}

auto AssociativeArrayNext(
    const AssociativeArrayCore& array, const void* element) -> void* {
  return EntriesOf(array).Next(element);
}

auto AssociativeArrayPrevious(
    const AssociativeArrayCore& array, const void* element) -> void* {
  return EntriesOf(array).Previous(element);
}

auto AssociativeArrayAfter(const AssociativeArrayCore& array, const void* index)
    -> void* {
  return OrderedIndicesOf(array).After({.bytes = index, .type = array.index});
}

auto AssociativeArrayBefore(
    const AssociativeArrayCore& array, const void* index) -> void* {
  return OrderedIndicesOf(array).Before({.bytes = index, .type = array.index});
}

auto AssociativeArrayPrepareDefault(const AssociativeArrayCore& array)
    -> void* {
  const Slot shape(*array.element);
  return NewSlot(shape, shape.after);
}

void AssociativeArraySetDefault(AssociativeArrayCore& array, void* element) {
  if (array.stated_default != nullptr) {
    ReleaseSlot(*array.element, array.stated_default);
  }
  array.stated_default = element;
}

auto AssociativeArrayStatedDefault(const AssociativeArrayCore& array) -> const
    void* {
  return array.stated_default;
}

auto AssociativeArrayLease(const AssociativeArrayCore& array, void* element)
    -> ContainerLease {
  return {
      Slot(*array.element).CountOf(element), element, &EndDetachedEntry,
      array.element};
}

}  // namespace lyra::value
