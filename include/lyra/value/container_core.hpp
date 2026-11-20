#pragma once

#include <cstddef>
#include <cstdint>
#include <tuple>
#include <type_traits>

#include "lyra/value/container_lease.hpp"
#include "lyra/value/value_type.hpp"
#include "lyra/value/value_type_kind.hpp"

// What a run-time-sized container holds of its elements (LRM 7.5, 7.8, 7.10),
// compiled once in the library: the storage they lie in, which of them it
// holds in which order, and the leases calls take on them. Every operation on
// an element's value -- building, copying, comparing, streaming, ending one,
// moving one out of its storage -- is compiled with the element's type where
// the operation is. A core is handed the element's type only for what it does
// to an element itself: its size and alignment, and ending it where the last
// lease on an element its container let go of ends, or, for an associative
// array, wherever the core removes one.
//
// Each core is a C record. The fields that say where the elements lie are read
// by the bodies the compiler builds for a container type, on both backends, as
// well as by the core, so the two agree on them for as long as one release of
// the compiler and its library is used together; no code outside that release
// reads them. Each is a pointer or a 64-bit word, so the platform's C layout
// fixes where it lies for every compiler.
//
// Storage a body takes for an element -- a queue's from its recycler, a
// dynamic array's new generation -- is the body's until it builds the
// elements there and commits or adopts it. An associative array's entry passes
// through the states its core's entries name: it is prepared -- storage of the
// core's that holds no value -- and the caller builds the element's value in
// it and commits it, from which point the core ends it. Between taking storage
// and committing it nothing the program wrote runs, and storage that cannot be
// had ends the run, so taken storage is always committed.
namespace lyra::value {

// Storage of `size` bytes each at `align` that the run hands out and takes
// back, as LLVM's `RecyclingAllocator` does: a free list of what was given
// back, each free one holding the address of the next in its first word, over
// fresh storage cut from a page from `next` up to `end`. Storage a recycler
// hands out goes back to the same recycler whoever gives it back, so one may
// outlive the container it was taken for while a call still refers to its
// element (LRM 13.5.2).
struct Recycler {
  void* free_list;
  std::uint64_t size;
  std::uint64_t align;
  void* next;
  void* end;

  // Each member, in the order it is declared, and the one at position `N`,
  // which generated code reaches a member by.
  template <typename Self>
  static auto Tie(Self& self) noexcept {
    return std::tie(self.free_list, self.size, self.align, self.next, self.end);
  }
  template <std::size_t N>
  auto Component() noexcept -> auto& {
    return std::get<N>(Tie(*this));
  }
  template <std::size_t N>
  [[nodiscard]] auto Component() const noexcept -> const auto& {
    return std::get<N>(Tie(*this));
  }
};

// A queue (LRM 7.10): each element in storage of its own that never moves
// while it is in the queue, and the order held as a ring of their addresses,
// so the element at position `i` lies at `ring[(head + i) & mask]`. The ring's
// length is `mask + 1`, a power of two, and an empty queue may hold no ring at
// all. Its elements' storage comes from `recycler`, the run's recycler for
// storage of its element's size. Nothing in it depends on the element type.
struct QueueCore {
  void** ring;
  std::uint64_t mask;
  std::uint64_t head;
  std::uint64_t count;
  Recycler* recycler;

  // The kind of value a queue is, and how many elements it holds, which a
  // library template holding a value of any type asks of it.
  static constexpr ValueTypeKind kKind = ValueTypeKind::kQueue;
  [[nodiscard]] auto Count() const noexcept -> std::size_t {
    return count;
  }

  // Each member, in the order it is declared, and the one at position `N`,
  // which generated code reaches a member by.
  template <typename Self>
  static auto Tie(Self& self) noexcept {
    return std::tie(self.ring, self.mask, self.head, self.count, self.recycler);
  }
  template <std::size_t N>
  auto Component() noexcept -> auto& {
    return std::get<N>(Tie(*this));
  }
  template <std::size_t N>
  [[nodiscard]] auto Component() const noexcept -> const auto& {
    return std::get<N>(Tie(*this));
  }
};

// A dynamic array (LRM 7.5): its elements one after another, the one at index
// `i` at `data + i * size`, where `size` is the element type's. An array of no
// elements may hold no storage.
struct DynamicArrayCore {
  void* data;
  std::uint64_t count;
  const ValueType* element;

  // The kind of value a dynamic array is, and how many elements it holds,
  // which a library template holding a value of any type asks of it.
  static constexpr ValueTypeKind kKind = ValueTypeKind::kDynamicArray;
  [[nodiscard]] auto Count() const noexcept -> std::size_t {
    return count;
  }

  // Each member, in the order it is declared, and the one at position `N`,
  // which generated code reaches a member by.
  template <typename Self>
  static auto Tie(Self& self) noexcept {
    return std::tie(self.data, self.count, self.element);
  }
  template <std::size_t N>
  auto Component() noexcept -> auto& {
    return std::get<N>(Tie(*this));
  }
  template <std::size_t N>
  [[nodiscard]] auto Component() const noexcept -> const auto& {
    return std::get<N>(Tie(*this));
  }
};

// The kinds of index whose order the language fixes, which the core compares
// itself (LRM 7.8.1 -- 7.8.4), and the index of any other type, whose entries
// it keeps in the order they were made (LRM 7.8.5) and which only the array
// type's own code compares.
enum class IndexKind : std::uint32_t {
  kIntegral,
  kString,
  kClassHandle,
  kChandle,
  kWildcard,
  kOther,
};

// An associative array (LRM 7.8): each entry its element and its index in
// storage of its own that never moves while the entry is in the array, and
// how many there are. What a read of an absent index answers is the value a
// `default:` clause gave the array (LRM 7.9.11), where one did, and the
// element type's default otherwise.
struct AssociativeArrayCore {
  void* entries;
  std::uint64_t count;
  const ValueType* element;
  const ValueType* index;
  void* stated_default;
  IndexKind kind;

  // The kind of value an associative array is, and how many entries it
  // holds, which a library template holding a value of any type asks of it.
  static constexpr ValueTypeKind kKind = ValueTypeKind::kAssociativeArray;
  [[nodiscard]] auto Count() const noexcept -> std::size_t {
    return count;
  }

  // Each member, in the order it is declared, and the one at position `N`,
  // which generated code reaches a member by.
  template <typename Self>
  static auto Tie(Self& self) noexcept {
    return std::tie(
        self.entries, self.count, self.element, self.index, self.stated_default,
        self.kind);
  }
  template <std::size_t N>
  auto Component() noexcept -> auto& {
    return std::get<N>(Tie(*this));
  }
  template <std::size_t N>
  [[nodiscard]] auto Component() const noexcept -> const auto& {
    return std::get<N>(Tie(*this));
  }
};

static_assert(std::is_standard_layout_v<QueueCore>);
static_assert(std::is_trivially_copyable_v<QueueCore>);
static_assert(std::is_standard_layout_v<Recycler>);
static_assert(std::is_standard_layout_v<DynamicArrayCore>);
static_assert(std::is_trivially_copyable_v<DynamicArrayCore>);
static_assert(std::is_standard_layout_v<AssociativeArrayCore>);
static_assert(std::is_trivially_copyable_v<AssociativeArrayCore>);

// Fresh storage from `recycler`, whose free list is empty: the next piece of
// its page, a new page taken first where that one is used up. A free piece
// holds the link to the next one in its first word, over whatever it holds in
// use, so storage handed out holds nothing its taker may read before setting
// it.
[[nodiscard]] auto RecyclerAllocate(Recycler* recycler) -> void*;

// A queue of no elements of `element`, its storage from the run's recycler
// for that element. What a queue does with its elements -- taking storage for
// one, building it, letting it go, holding the queue to its bound -- is the
// queue type's own code; the functions below move the addresses in its ring
// and nothing else, and the lease, which ends an element its queue let go of,
// is handed the element's type, which the queue does not keep.
[[nodiscard]] auto QueueConstruct(const ValueType* element) -> QueueCore;
// Takes what `from` holds into `out`, which holds nothing, leaving `from`
// holding nothing, which ending does nothing with.
void TakeCore(QueueCore& from, QueueCore& out) noexcept;
// Makes what `from` holds the elements of `queue`, which has let go of the
// ones it held, `from` left empty.
void QueueAdopt(QueueCore& queue, QueueCore& from) noexcept;
// Gives back the ring of a queue holding no element.
void QueueFreeRing(QueueCore& queue) noexcept;
// A ring long enough for `count` elements, so that committing as many does
// not grow it.
void QueueReserve(QueueCore& queue, std::uint64_t count);
// Puts the element built at `storage` at `position`, which is at most the
// count; the elements from there move one position on.
void QueueCommit(QueueCore& queue, std::uint64_t position, void* storage);
// Where the element at `index`, one the queue holds, lies, as Rust's
// `get_unchecked` reaches one.
[[nodiscard]] auto QueueGetUnchecked(
    const QueueCore& queue, std::uint64_t index) -> void*;
// Takes the element at `position` out of the order and answers its storage,
// which still holds its value; the elements after it move one position back.
[[nodiscard]] auto QueueUnlink(
    QueueCore& queue, std::uint64_t position) noexcept -> void*;
// Puts the element that was at position `order[k]` at position `k`, or every
// element in the opposite order; no element moves (LRM 7.12.2).
void QueuePermute(QueueCore& queue, const std::uint64_t* order);
// The same, the order a dynamic array of 64-bit two-state positions holds, as
// a sort answers it (LRM 7.12.2).
void QueueApplyOrder(QueueCore& queue, const DynamicArrayCore& order);
void QueueReverse(QueueCore& queue) noexcept;
// A lease a call takes on the element of `element` at `storage`.
[[nodiscard]] auto QueueLease(const ValueType& element, void* storage)
    -> ContainerLease;

// A dynamic array of no elements of `element`.
[[nodiscard]] auto DynamicArrayConstruct(const ValueType* element)
    -> DynamicArrayCore;
// The same for a dynamic array.
void TakeCore(DynamicArrayCore& from, DynamicArrayCore& out) noexcept;
// The array letting go of its elements, answering whether its own code ends
// them: not where it holds none, nor where a call leases them, which keeps
// them to the last lease (LRM 13.5.2) and leaves the array holding none; and
// the storage of elements its own code ended given back, the array then
// holding none.
[[nodiscard]] auto DynamicArrayLetGo(DynamicArrayCore& array) noexcept -> bool;
void DynamicArrayFree(DynamicArrayCore& array) noexcept;
// Storage for `count` elements, holding no value, which the array adopts once
// each holds one; none where `count` is zero.
[[nodiscard]] auto DynamicArrayGeneration(
    const DynamicArrayCore& array, std::uint64_t count) -> void*;
// Where the element at index `index` of a new generation's storage lies,
// which holds no value until one is built there.
[[nodiscard]] auto DynamicArrayPlace(
    const DynamicArrayCore& array, void* data, std::uint64_t index) -> void*;
// Makes `data`, holding `count` elements, the elements of the array, which
// holds none.
void DynamicArrayAdopt(
    DynamicArrayCore& array, void* data, std::uint64_t count) noexcept;
// A lease a call takes on the elements `element`, one of them, lies among.
[[nodiscard]] auto DynamicArrayLease(
    const DynamicArrayCore& array, void* element) -> ContainerLease;

// An associative array of no entries, of elements of `element` indexed by
// `index`, whose order is that of the index kind numbered `kind`, which the
// array type's own code hands over as the machine integer it can write.
[[nodiscard]] auto AssociativeArrayConstruct(
    std::int64_t kind, const ValueType* index, const ValueType* element)
    -> AssociativeArrayCore;
// The same for an associative array.
void TakeCore(AssociativeArrayCore& from, AssociativeArrayCore& out) noexcept;
// Ends every entry and the value a `default:` clause gave, leaving the array
// holding nothing.
void AssociativeArrayEnd(AssociativeArrayCore& array) noexcept;
// Makes what `from` holds the entries of `array`, which ends the ones it held,
// `from` left empty.
void AssociativeArrayAdopt(
    AssociativeArrayCore& array, AssociativeArrayCore& from) noexcept;
// The element of the entry `index` names, an index of the array's index type
// holding no x or z, or none; the index type is of a kind whose order the core
// knows.
[[nodiscard]] auto AssociativeArrayFind(
    const AssociativeArrayCore& array, const void* index) -> void*;
// Storage for one entry, answering where its element lies; where its index
// lies is asked of the element. Both hold no value.
[[nodiscard]] auto AssociativeArrayPrepare(const AssociativeArrayCore& array)
    -> void*;
// The index of the entry whose element lies at `element`.
[[nodiscard]] auto AssociativeArrayIndexOf(
    const AssociativeArrayCore& array, void* element) -> void*;
// Makes the entry built at `element` one of the array's; no entry holds an
// equal index.
void AssociativeArrayCommit(AssociativeArrayCore& array, void* element);
// Takes the entry whose element lies at `element` out of the array and ends
// it, or leaves it to a lease a call holds on it (LRM 13.5.2); and every
// entry.
void AssociativeArrayRemove(
    AssociativeArrayCore& array, void* element) noexcept;
void AssociativeArrayClear(AssociativeArrayCore& array) noexcept;
// The element of the entry with the least index, and of the entry after the
// one whose element is at `element`, each none where there is no such entry:
// the array's entries in its index order (LRM 7.8), or for an index of any
// other kind in the order they were made (LRM 7.8.5).
[[nodiscard]] auto AssociativeArrayFirst(const AssociativeArrayCore& array)
    -> void*;
[[nodiscard]] auto AssociativeArrayLast(const AssociativeArrayCore& array)
    -> void*;
[[nodiscard]] auto AssociativeArrayNext(
    const AssociativeArrayCore& array, const void* element) -> void*;
[[nodiscard]] auto AssociativeArrayPrevious(
    const AssociativeArrayCore& array, const void* element) -> void*;
// The element of the entry with the least index after `index`, or with the
// greatest before it, an index of the array's index type the array need not
// hold (LRM 7.9.6, 7.9.7), of a kind whose order the core knows.
[[nodiscard]] auto AssociativeArrayAfter(
    const AssociativeArrayCore& array, const void* index) -> void*;
[[nodiscard]] auto AssociativeArrayBefore(
    const AssociativeArrayCore& array, const void* index) -> void*;
// Storage for the value a `default:` clause gives the array, holding no value,
// and that value, once built there, made the array's own, ending the one it
// had (LRM 7.9.11); and that value, none where no clause gave one.
[[nodiscard]] auto AssociativeArrayPrepareDefault(
    const AssociativeArrayCore& array) -> void*;
void AssociativeArraySetDefault(AssociativeArrayCore& array, void* element);
[[nodiscard]] auto AssociativeArrayStatedDefault(
    const AssociativeArrayCore& array) -> const void*;
// A lease a call takes on the entry whose element lies at `element`.
[[nodiscard]] auto AssociativeArrayLease(
    const AssociativeArrayCore& array, void* element) -> ContainerLease;

}  // namespace lyra::value
