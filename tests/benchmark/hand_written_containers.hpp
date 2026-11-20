// The containers a case's hand-written program uses: a queue, a dynamic array
// and an associative array, written as a careful C++ programmer would write
// them to keep every guarantee IEEE 1800 gives the language's own, whether or
// not the program reaching them needs it. A program that dropped one would be
// cheaper than any implementation of the language can be, since a compiler
// building one unit cannot know that no other unit binds a reference to an
// element.
//
// - A queue element never moves while it is a member (LRM 7.10.3): the queue
//   is a ring of the addresses of its elements, each in storage of its own,
//   taken from and given back to a free list of storage of its size.
// - An element let go while a call holds a reference to it goes on existing
//   until the reference ends (LRM 13.5.2): a queue element and an associative
//   entry each count the references held to it, and a dynamic array's block of
//   elements counts those held into it; what is let go while counted is kept
//   by the last reference instead of ended.
// - Reading through an index that names no element answers the element
//   default (LRM 7.4.5, 7.10.1); a queue write one past its last element
//   appends, and one anywhere else is ignored with a warning.
// - An associative array is walked in the order of its index (LRM 7.8.2,
//   7.8.4), and reading an index it has no entry for answers the default with
//   a warning (LRM 7.8.6).
//
// Nothing here is Lyra's: it is the floor Lyra's containers are measured
// against, so it shares no code with Lyra's library.

#pragma once

#include <algorithm>
#include <bit>
#include <charconv>
#include <cstddef>
#include <cstdint>
#include <cstdio>
#include <cstring>
#include <map>
#include <memory>
#include <new>
#include <span>
#include <string_view>
#include <unordered_map>
#include <utility>
#include <vector>

namespace hand_written {

// A line of the run's output, on the stream `$display` writes to.
inline void Print(std::string_view line) {
  std::fwrite(line.data(), 1, line.size(), stdout);
  std::fputc('\n', stdout);
}

// The run's own warnings, on the stream a simulator writes its notes to.
inline void Warn(std::string_view what) {
  constexpr std::string_view kLead = "warning: ";
  std::fwrite(kLead.data(), 1, kLead.size(), stderr);
  std::fwrite(what.data(), 1, what.size(), stderr);
  std::fputc('\n', stderr);
}

// SystemVerilog's `int` adds modulo 2^32 (LRM 11.4.3), where C++'s signed
// addition may not overflow.
[[nodiscard]] inline auto Add(std::int32_t a, std::int32_t b) -> std::int32_t {
  return static_cast<std::int32_t>(
      static_cast<std::uint32_t>(a) + static_cast<std::uint32_t>(b));
}

// The amount of work a case does, as `$value$plusargs("work=%d", n)` reads it
// (LRM 21.6), or the default where the run was given none.
[[nodiscard]] inline auto WorkFrom(std::span<char* const> args)
    -> std::int32_t {
  constexpr std::string_view kPrefix = "+work=";
  for (const std::string_view arg : args) {
    if (!arg.starts_with(kPrefix)) continue;
    const std::string_view digits = arg.substr(kPrefix.size());
    std::int32_t work = 0;
    std::from_chars(digits.data(), digits.data() + digits.size(), work);
    return work;
  }
  return 1;
}

// What reading an index that names no element answers: the element type's
// default, read where it lies like any element.
template <typename T>
inline const T kDefault{};

// A count of references held to something, and whether its owner has let it
// go; the last reference ends what was let go.
class Leases {
 public:
  [[nodiscard]] auto Held() const -> bool {
    return (word_ & kCount) != 0;
  }
  void Take() {
    ++word_;
  }
  // Answers whether this was the last reference to something let go.
  [[nodiscard]] auto Give() -> bool {
    --word_;
    return word_ == kLetGo;
  }
  void LetGo() {
    word_ |= kLetGo;
  }

 private:
  static constexpr std::uint32_t kLetGo = 1U << 31;
  static constexpr std::uint32_t kCount = kLetGo - 1;
  std::uint32_t word_ = 0;
};

// Storage for one `T`, given back and handed out again, as LLVM's `Recycler`
// links each freed piece through its first word.
template <typename T>
class Recycler {
 public:
  [[nodiscard]] static auto Take() -> T* {
    if (free_ == nullptr) return std::allocator<T>{}.allocate(1);
    T* piece = free_;
    std::memcpy(
        static_cast<void*>(&free_), static_cast<void*>(piece), sizeof(free_));
    return piece;
  }
  static void GiveBack(T* piece) {
    std::memcpy(
        static_cast<void*>(piece), static_cast<void*>(&free_), sizeof(free_));
    free_ = piece;
  }

 private:
  static_assert(sizeof(T) >= sizeof(T*));
  static inline T* free_ = nullptr;
};

template <typename T>
class Queue {
  struct Element {
    Leases leases;
    T value;
  };
  using Storage = Recycler<Element>;

 public:
  // A reference a call holds to an element, which keeps it past its removal.
  class Ref {
   public:
    explicit Ref(Element* element) : element_(element) {
      element_->leases.Take();
    }
    Ref(const Ref&) = delete;
    Ref(Ref&&) = delete;
    auto operator=(const Ref&) -> Ref& = delete;
    auto operator=(Ref&&) -> Ref& = delete;
    ~Ref() {
      if (element_->leases.Give()) End(element_);
    }
    [[nodiscard]] auto Value() const -> T& {
      return element_->value;
    }

   private:
    Element* element_;
  };

  Queue() = default;
  Queue(const Queue& other) {
    Reserve(other.count_);
    for (std::size_t i = 0; i < other.count_; ++i) {
      ring_[i] = Make(other.At(i)->value);
    }
    count_ = other.count_;
  }
  Queue(Queue&& other) noexcept
      : ring_(std::move(other.ring_)),
        head_(std::exchange(other.head_, 0)),
        count_(std::exchange(other.count_, 0)) {
  }
  // Assigning a whole queue outdates every reference to its elements
  // (LRM 7.10.3): an element a call holds is let go and replaced, and every
  // other one the queue keeps is written over, as `std::vector`'s assignment
  // writes over the elements it keeps.
  auto operator=(const Queue& other) -> Queue& {
    if (this == &other) return *this;
    const std::size_t kept = std::min(count_, other.count_);
    for (std::size_t i = 0; i < kept; ++i) {
      Element*& slot = Slot(i);
      if (slot->leases.Held()) {
        LetGo(slot);
        slot = Make(other.At(i)->value);
      } else {
        slot->value = other.At(i)->value;
      }
    }
    while (count_ > other.count_) {
      --count_;
      LetGo(At(count_));
    }
    for (std::size_t i = kept; i < other.count_; ++i) {
      PushBack(other.At(i)->value);
    }
    return *this;
  }
  auto operator=(Queue&& other) noexcept -> Queue& {
    Queue taken(std::move(other));
    Swap(taken);
    return *this;
  }
  ~Queue() {
    Clear();
  }

  [[nodiscard]] auto Size() const -> std::int32_t {
    return static_cast<std::int32_t>(count_);
  }

  [[nodiscard]] auto Read(std::int32_t index) const -> const T& {
    if (!Names(index)) return kDefault<T>;
    return At(static_cast<std::size_t>(index))->value;
  }
  void Write(std::int32_t index, T value) {
    if (Names(index)) {
      At(static_cast<std::size_t>(index))->value = std::move(value);
    } else if (index >= 0 && static_cast<std::size_t>(index) == count_) {
      PushBack(std::move(value));
    } else {
      Warn("a queue was written through an invalid index");
    }
  }
  [[nodiscard]] auto Bind(std::int32_t index) -> Ref {
    return Ref(At(static_cast<std::size_t>(index)));
  }

  void PushBack(T value) {
    Reserve(count_ + 1);
    Slot(count_) = Make(std::move(value));
    ++count_;
  }
  auto PopFront() -> T {
    if (count_ == 0) return T{};
    Element* element = ring_[head_];
    head_ = (head_ + 1) & Mask();
    --count_;
    return TakeOut(element);
  }
  void Insert(std::int32_t index, T value) {
    if (index < 0 || static_cast<std::size_t>(index) > count_) return;
    Reserve(count_ + 1);
    for (auto i = count_; i > static_cast<std::size_t>(index); --i) {
      Slot(i) = Slot(i - 1);
    }
    Slot(static_cast<std::size_t>(index)) = Make(std::move(value));
    ++count_;
  }
  void Delete(std::int32_t index) {
    if (!Names(index)) return;
    Element* element = At(static_cast<std::size_t>(index));
    for (auto i = static_cast<std::size_t>(index); i + 1 < count_; ++i) {
      Slot(i) = Slot(i + 1);
    }
    --count_;
    LetGo(element);
  }
  void Clear() {
    for (std::size_t i = 0; i < count_; ++i) LetGo(At(i));
    head_ = 0;
    count_ = 0;
  }

  [[nodiscard]] auto operator==(const Queue& other) const -> bool {
    if (count_ != other.count_) return false;
    for (std::size_t i = 0; i < count_; ++i) {
      if (!(At(i)->value == other.At(i)->value)) return false;
    }
    return true;
  }

 private:
  [[nodiscard]] auto Names(std::int32_t index) const -> bool {
    return index >= 0 && static_cast<std::size_t>(index) < count_;
  }
  // The ring's length is a power of two, so a position wraps by a mask.
  [[nodiscard]] auto Mask() const -> std::size_t {
    return ring_.size() - 1;
  }
  [[nodiscard]] auto Slot(std::size_t position) -> Element*& {
    return ring_[(head_ + position) & Mask()];
  }
  [[nodiscard]] auto At(std::size_t position) const -> Element* {
    return ring_[(head_ + position) & Mask()];
  }

  void Reserve(std::size_t count) {
    if (count <= ring_.size()) return;
    std::size_t grown = ring_.empty() ? 8 : ring_.size() * 2;
    while (grown < count) grown *= 2;
    std::vector<Element*> ring(grown);
    for (std::size_t i = 0; i < count_; ++i) ring[i] = At(i);
    ring_ = std::move(ring);
    head_ = 0;
  }

  [[nodiscard]] static auto Make(T value) -> Element* {
    return std::construct_at(Storage::Take(), Leases{}, std::move(value));
  }
  static void End(Element* element) {
    std::destroy_at(element);
    Storage::GiveBack(element);
  }
  // A popped element is moved out where nothing holds it, and copied out of
  // one a call still holds, which the call's reference then ends.
  [[nodiscard]] static auto TakeOut(Element* element) -> T {
    if (element->leases.Held()) {
      element->leases.LetGo();
      return element->value;
    }
    T value = std::move(element->value);
    End(element);
    return value;
  }
  static void LetGo(Element* element) {
    if (element->leases.Held()) {
      element->leases.LetGo();
      return;
    }
    End(element);
  }

  void Swap(Queue& other) noexcept {
    std::swap(ring_, other.ring_);
    std::swap(head_, other.head_);
    std::swap(count_, other.count_);
  }

  std::vector<Element*> ring_;
  std::size_t head_ = 0;
  std::size_t count_ = 0;
};

template <typename T>
class DynamicArray {
  // One block of elements, the array's current one or one a reference keeps
  // after the array has moved on to another: this header, and the elements
  // after it in the same allocation.
  struct Block {
    Leases leases;
    std::size_t count = 0;
  };
  static constexpr std::size_t kElementsAt =
      (sizeof(Block) + alignof(T) - 1) / alignof(T) * alignof(T);
  static constexpr std::align_val_t kAlign{
      std::max(alignof(T), alignof(Block))};

  [[nodiscard]] static auto BytesOf(std::size_t count) -> std::size_t {
    return kElementsAt + (count * sizeof(T));
  }
  [[nodiscard]] static auto Allocate(std::size_t count) -> Block* {
    return std::construct_at(
        static_cast<Block*>(::operator new(BytesOf(count), kAlign)), Leases{},
        count);
  }
  [[nodiscard]] static auto ElementsOf(Block* block) -> std::span<T> {
    const std::span<std::byte> bytes(
        std::bit_cast<std::byte*>(block), BytesOf(block->count));
    return {
        std::launder(std::bit_cast<T*>(bytes.subspan(kElementsAt).data())),
        block->count};
  }

 public:
  class Ref {
   public:
    Ref(Block* block, T* element) : block_(block), element_(element) {
      block_->leases.Take();
    }
    Ref(const Ref&) = delete;
    Ref(Ref&&) = delete;
    auto operator=(const Ref&) -> Ref& = delete;
    auto operator=(Ref&&) -> Ref& = delete;
    ~Ref() {
      if (block_->leases.Give()) End(block_);
    }
    [[nodiscard]] auto Value() const -> T& {
      return *element_;
    }

   private:
    Block* block_;
    T* element_;
  };

  DynamicArray() = default;
  DynamicArray(const DynamicArray& other) {
    if (other.elements_.empty()) return;
    Block* block = Allocate(other.elements_.size());
    std::ranges::uninitialized_copy(other.elements_, ElementsOf(block));
    Adopt(block);
  }
  DynamicArray(DynamicArray&& other) noexcept
      : block_(std::exchange(other.block_, nullptr)),
        elements_(std::exchange(other.elements_, {})) {
  }
  // Assigning outdates every reference into the array (LRM 13.5.2), so where
  // none is held and the size does not change, the block is written in place.
  auto operator=(const DynamicArray& other) -> DynamicArray& {
    if (this == &other) return *this;
    if (block_ != nullptr && !block_->leases.Held() &&
        elements_.size() == other.elements_.size()) {
      std::ranges::copy(other.elements_, elements_.begin());
      return *this;
    }
    DynamicArray copy(other);
    Swap(copy);
    return *this;
  }
  auto operator=(DynamicArray&& other) noexcept -> DynamicArray& {
    DynamicArray taken(std::move(other));
    Swap(taken);
    return *this;
  }
  ~DynamicArray() {
    Delete();
  }

  // `new[count]` (LRM 7.5.1): every element the default.
  void New(std::int32_t count) {
    Block* block = Allocate(Count(count));
    std::ranges::uninitialized_value_construct(ElementsOf(block));
    Replace(block);
  }
  // `new[count](from)`: the first elements copied from `from`, which may be
  // this array, the rest the default.
  void New(std::int32_t count, const DynamicArray& from) {
    Block* block = Allocate(Count(count));
    const std::span<T> elements = ElementsOf(block);
    const std::size_t kept = std::min(from.elements_.size(), elements.size());
    std::ranges::uninitialized_copy(
        from.elements_.first(kept), elements.first(kept));
    std::ranges::uninitialized_value_construct(elements.subspan(kept));
    Replace(block);
  }
  void Delete() {
    if (block_ != nullptr) LetGo(block_);
    block_ = nullptr;
    elements_ = {};
  }

  [[nodiscard]] auto Size() const -> std::int32_t {
    return static_cast<std::int32_t>(elements_.size());
  }
  [[nodiscard]] auto Read(std::int32_t index) const -> const T& {
    if (!Names(index)) return kDefault<T>;
    return elements_[static_cast<std::size_t>(index)];
  }
  void Write(std::int32_t index, T value) {
    if (Names(index)) {
      elements_[static_cast<std::size_t>(index)] = std::move(value);
    }
  }
  [[nodiscard]] auto Bind(std::int32_t index) -> Ref {
    return Ref(block_, &elements_[static_cast<std::size_t>(index)]);
  }

  // `std::equal` compares the elements' bytes at once where equality is
  // theirs, as it is for an integral element.
  [[nodiscard]] auto operator==(const DynamicArray& other) const -> bool {
    return std::equal(
        elements_.begin(), elements_.end(), other.elements_.begin(),
        other.elements_.end());
  }

 private:
  [[nodiscard]] auto Names(std::int32_t index) const -> bool {
    return index >= 0 && static_cast<std::size_t>(index) < elements_.size();
  }
  [[nodiscard]] static auto Count(std::int32_t count) -> std::size_t {
    return count < 0 ? 0 : static_cast<std::size_t>(count);
  }
  static void End(Block* block) {
    const std::size_t bytes = BytesOf(block->count);
    std::ranges::destroy(ElementsOf(block));
    std::destroy_at(block);
    ::operator delete(static_cast<void*>(block), bytes, kAlign);
  }
  static void LetGo(Block* block) {
    if (block->leases.Held()) {
      block->leases.LetGo();
      return;
    }
    End(block);
  }
  void Adopt(Block* block) {
    block_ = block;
    elements_ = ElementsOf(block);
  }
  void Replace(Block* block) {
    if (block_ != nullptr) LetGo(block_);
    Adopt(block);
  }
  void Swap(DynamicArray& other) noexcept {
    std::swap(block_, other.block_);
    std::swap(elements_, other.elements_);
  }

  Block* block_ = nullptr;
  std::span<T> elements_;
};

// An associative array whose index orders its entries, as an integral or a
// string index does (LRM 7.8.2, 7.8.4). A `std::map` node never moves while it
// is a member, so the node is each entry's storage.
template <typename Index, typename T>
class AssociativeArray {
  struct Entry {
    Leases leases;
    T value;
  };
  using Map = std::map<Index, Entry>;
  using Node = typename Map::node_type;

 public:
  class Ref {
   public:
    explicit Ref(Entry* entry) : entry_(entry) {
      entry_->leases.Take();
    }
    Ref(const Ref&) = delete;
    Ref(Ref&&) = delete;
    auto operator=(const Ref&) -> Ref& = delete;
    auto operator=(Ref&&) -> Ref& = delete;
    ~Ref() {
      if (entry_->leases.Give()) Detached().erase(entry_);
    }
    [[nodiscard]] auto Value() const -> T& {
      return entry_->value;
    }

   private:
    Entry* entry_;
  };

  [[nodiscard]] auto Exists(const Index& index) const -> bool {
    return entries_.contains(index);
  }
  [[nodiscard]] auto Read(const Index& index) const -> const T& {
    const auto found = entries_.find(index);
    if (found == entries_.end()) {
      Warn("an associative array was read at an index it has no entry for");
      return kDefault<T>;
    }
    return found->second.value;
  }
  void Write(const Index& index, T value) {
    entries_[index].value = std::move(value);
  }
  // Binding a reference to an index makes its entry first (LRM 7.8.7).
  [[nodiscard]] auto Bind(const Index& index) -> Ref {
    return Ref(&entries_[index]);
  }

  void Delete(const Index& index) {
    const auto found = entries_.find(index);
    if (found == entries_.end()) return;
    if (found->second.leases.Held()) {
      found->second.leases.LetGo();
      Entry* entry = &found->second;
      Detached().emplace(entry, entries_.extract(found));
      return;
    }
    entries_.erase(found);
  }
  void Delete() {
    while (!entries_.empty()) Delete(entries_.begin()->first);
  }

  // `first`, `next` (LRM 7.9.4, 7.9.6): whether there is one, and where
  // there is, the index written to `index`.
  [[nodiscard]] auto First(Index& index) const -> bool {
    if (entries_.empty()) return false;
    index = entries_.begin()->first;
    return true;
  }
  [[nodiscard]] auto Next(Index& index) const -> bool {
    const auto after = entries_.upper_bound(index);
    if (after == entries_.end()) return false;
    index = after->first;
    return true;
  }

 private:
  // Entries let go while a reference held them, each kept by its node until
  // the last reference ends.
  [[nodiscard]] static auto Detached() -> std::unordered_map<Entry*, Node>& {
    static std::unordered_map<Entry*, Node> detached;
    return detached;
  }

  Map entries_;
};

}  // namespace hand_written
