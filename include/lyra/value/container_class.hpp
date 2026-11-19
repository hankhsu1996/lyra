#pragma once

#include <concepts>
#include <cstddef>
#include <cstdint>
#include <utility>
#include <vector>

#include "lyra/base/internal_error.hpp"
#include "lyra/base/source_location.hpp"
#include "lyra/value/array_ordering.hpp"
#include "lyra/value/concepts.hpp"
#include "lyra/value/container_core.hpp"
#include "lyra/value/container_lease.hpp"
#include "lyra/value/element_policy.hpp"
#include "lyra/value/formation.hpp"
#include "lyra/value/integral_fwd.hpp"
#include "lyra/value/value_witnesses_of.hpp"

// What the C++ realization of every dynamic array and queue type shares: one
// class over its core, every operation of the language on it a body the
// compiler builds for the type, and what is the same for every element type --
// what a library template holding a value of any type asks of any array,
// answered from the type's own bodies for these two, the order a sort answers,
// and the allocation function placing an element of a dynamic array's new
// storage.

// The allocation function a placement new-expression building the element at
// `index` of a dynamic array's new storage names ([expr.new]), for a value of
// `size` bytes. The storage is the core's, so no deallocation function matches
// it, and a construction that raises gives nothing back, which only a failed
// allocation can. The core lays its elements one after another, each as large
// as the type it was told says, so a value of another size is placed there
// only by a defect of the compiler's.
[[nodiscard]] inline auto operator new(
    std::size_t size, const lyra::value::DynamicArrayCore& array, void* data,
    std::uint64_t index) -> void* {
  if (size != array.element->Size()) {
    throw lyra::InternalError(
        "a dynamic array's element is built as a value of another size -- "
        "please report this as a bug");
  }
  return lyra::value::DynamicArrayPlace(array, data, index);
}

namespace lyra::value {

// The C++ class of a dynamic array type or a queue type, which derives from
// its core.
template <typename T>
concept RunTimeSizedArray =
    std::derived_from<T, DynamicArrayCore> || std::derived_from<T, QueueCore>;

// The element a write through `key` lands on in `array`, with what reaching it
// did, which a write in progress is told: storage no read reaches where the
// key names none (LRM 7.4.5, 7.10.1). The library's own arrays answer it
// themselves; a dynamic array's or a queue's is where the type's own body
// says the write lands, which is handed no location, since the write in
// progress asking is where the write was written and the one bound there
// stands.
template <typename Array, typename Key>
  requires requires(Array& array, const Key& key, Formation& formed) {
    array.ElementMut(key, formed);
  }
[[nodiscard]] auto ElementMut(Array& array, const Key& key, Formation& formed)
    -> auto& {
  return array.ElementMut(key, formed);
}
template <RunTimeSizedArray Array>
[[nodiscard]] auto ElementMut(
    Array& array, const Position& position, Formation& formed)
    -> ElementOf<Array>& {
  const auto found = array.ElementPlace(position, base::SourceLocation{});
  ElementOf<Array>* const element = found.template Component<0>();
  formed = FormationOf(element != nullptr, found.template Component<1>());
  return element == nullptr ? detail::DiscardedWriteOf<ElementOf<Array>>()
                            : *element;
}

// The lease a call takes on `element`, one `array` holds, which the call then
// goes on reaching however the array changes (LRM 13.5.2). The library's own
// arrays take it themselves; a dynamic array's generation keeps the count of
// its leases, and a queue's element keeps it in its own storage, so a queue
// holding the element holds at least one.
template <typename Array, typename Element>
  requires requires(const Array& array, Element& element) {
    array.Lease(&element);
  }
[[nodiscard]] auto ElementLease(const Array& array, Element& element)
    -> ContainerLease {
  return array.Lease(&element);
}
template <typename Element>
[[nodiscard]] auto ElementLease(const DynamicArrayCore& array, Element& element)
    -> ContainerLease {
  return DynamicArrayLease(array, &element);
}
template <typename Element>
[[nodiscard]] auto ElementLease(const QueueCore& queue, Element& element)
    -> ContainerLease {
  if (queue.count == 0) {
    throw InternalError(
        "a call leases an element of an empty queue -- please report this as "
        "a bug");
  }
  return QueueLease(kValueTypeOf<Element>, &element);
}

// The C++ class of a dynamic array type or a queue type whose core is `Core`:
// its core and nothing of its own. `Impl` is the struct the C++ backend writes
// for the type's implementation of the library's traits, each item a static
// function whose body is the type's own; this class is C++'s realization of
// them, the same text for every such type.
//
// Its default and copy constructors build the object from what the type's
// default and copy answer, which costs a move of the core, a few words, that
// C++ need not elide where a delegating constructor is handed a prvalue of its
// own class. Its move constructor takes over the core of the object it moves
// and leaves that core empty, which ending does nothing with, so C++
// destroying the object moved from ends nothing. Its assignments write a copy
// over the value, or end the value and take over another's. Its destructor
// ends the value in the destructor of the complete object, whose body runs
// while the object is still in use ([class.dtor]); a class declaring its own
// destructor or copy assignment would get no move constructor
// ([class.copy.ctor]), so the six are this class's. No operation can be
// unwound out of, so none leaves a value half built.
//
// Each question a library template asks of an array is asked of the type's
// operation for it, under the name the template asks by.
template <typename Core, typename Impl>
class Container final : public Core {
 public:
  explicit Container(Core core) noexcept : Core(core) {
  }
  Container() noexcept : Container(Impl::DeclaredDefault()) {
  }
  Container(const Container& from) noexcept : Container(Impl::Copy(from)) {
  }
  Container(Container&& from) noexcept : Core() {
    TakeCore(from, *this);
  }
  auto operator=(const Container& from) noexcept -> Container& {
    Impl::Assign(*this, from);
    return *this;
  }
  auto operator=(Container&& from) noexcept -> Container& {
    if (this != &from) {
      Impl::End(*this);
      TakeCore(from, *this);
    }
    return *this;
  }
  ~Container() noexcept {
    Impl::End(*this);
  }

  [[nodiscard]] static auto DeclaredDefault() noexcept -> Container {
    return Impl::DeclaredDefault();
  }
  [[nodiscard]] auto GetUnchecked(std::size_t index) const -> decltype(auto) {
    return Impl::GetUnchecked(*this, index);
  }
  [[nodiscard]] auto GetUnchecked(std::size_t index) -> decltype(auto) {
    return Impl::GetUnchecked(*this, index);
  }
  template <typename... Args>
  [[nodiscard]] auto ElementPlace(Args&&... args) -> decltype(auto) {
    return Impl::ElementPlace(*this, std::forward<Args>(args)...);
  }
  template <typename... Args>
  [[nodiscard]] auto Resized(Args&&... args) const -> decltype(auto) {
    return Impl::Resized(*this, std::forward<Args>(args)...);
  }
  [[nodiscard]] auto IsBitIdentical(const Container& other) const
      -> decltype(auto) {
    return Impl::IsBitIdentical(*this, other);
  }
  [[nodiscard]] auto operator==(const Container& other) const
      -> decltype(auto) {
    return Impl::Equal(*this, other);
  }
  [[nodiscard]] auto operator!=(const Container& other) const
      -> decltype(auto) {
    return Impl::Unequal(*this, other);
  }
  [[nodiscard]] auto CaseEqual(const Container& other) const -> decltype(auto) {
    return Impl::CaseEqual(*this, other);
  }
  [[nodiscard]] auto HasUnknown() const -> decltype(auto) {
    return Impl::HasUnknown(*this);
  }
  template <typename... Args>
  [[nodiscard]] auto MergeConditional(Args&&... args) const -> decltype(auto) {
    return Impl::MergeConditional(*this, std::forward<Args>(args)...);
  }
  [[nodiscard]] auto BitstreamWidth() const -> decltype(auto)
    requires requires(const Container& c) { Impl::BitstreamWidth(c); }
  {
    return Impl::BitstreamWidth(*this);
  }
  [[nodiscard]] auto CountBits() const -> decltype(auto)
    requires requires(const Container& c) { Impl::CountBits(c); }
  {
    return Impl::CountBits(*this);
  }
  template <typename... Args>
    requires requires(const Container& c, Args&&... args) {
      Impl::ToBitVec(c, std::forward<Args>(args)...);
    }
  [[nodiscard]] auto ToBitVec(Args&&... args) const -> decltype(auto) {
    return Impl::ToBitVec(*this, std::forward<Args>(args)...);
  }
  template <typename... Args>
    requires requires(Args&&... args) {
      Impl::FromBitVec(std::forward<Args>(args)...);
    }
  [[nodiscard]] static auto FromBitVec(Args&&... args) -> decltype(auto) {
    return Impl::FromBitVec(std::forward<Args>(args)...);
  }
};

// The empty array of the type `T`, a container class, told the record of its
// element type: the core the library builds, made the object of that class.
template <typename T>
[[nodiscard]] auto QueueConstruct(const ValueType* element) -> T {
  return T(QueueConstruct(element));
}
template <typename T>
[[nodiscard]] auto DynamicArrayConstruct(const ValueType* element) -> T {
  return T(DynamicArrayConstruct(element));
}

// LRM 7.12.2 `sort` and `rsort`: the positions each of the dynamic array
// `keys` comes from once they are put in ascending order, or descending where
// `descending` says, each pair compared by the keys' own `<`, equal keys and
// keys whose comparison is unknown keeping their order, as a dynamic array
// `Order` of longint positions.
template <typename Order, typename Keys>
  requires std::derived_from<Keys, DynamicArrayCore>
[[nodiscard]] auto SortOrder(const Keys& keys, bool descending) -> Order {
  using Key = ElementOf<Keys>;
  const std::vector<std::size_t> order = detail::SortedPositions(
      keys.Count(), [&](std::int64_t first, std::int64_t second) {
        const Key& a = keys.GetUnchecked(static_cast<std::size_t>(first));
        const Key& b = keys.GetUnchecked(static_cast<std::size_t>(second));
        return descending ? Holds(b < a) : Holds(a < b);
      });
  using Position = ElementOf<Order>;
  Order made;
  void* data = DynamicArrayGeneration(made, order.size());
  for (std::size_t k = 0; k < order.size(); ++k) {
    ::new (made, data, k)
        Position(Position::FromInt(static_cast<std::int64_t>(order[k])));
  }
  DynamicArrayAdopt(made, data, order.size());
  return made;
}

}  // namespace lyra::value
