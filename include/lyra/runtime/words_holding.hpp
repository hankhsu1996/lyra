#pragma once

#include <bit>
#include <cstddef>
#include <cstdint>
#include <cstring>
#include <memory>
#include <new>
#include <utility>

#include "lyra/value/integral.hpp"
#include "lyra/value/wide.hpp"
#include "lyra/value/word_recycler.hpp"

namespace lyra::runtime {

// How a holder keeps an integral value wider than a word, whose width the
// library is told where the holder is built: the width, the address of the
// value's words, and the words themselves where the holder ends. A value past
// the size a value may be and still lie in what holds it has its words owned
// apart instead, as a frame keeps one, allocated where the holder is built. So
// the value has one address for as long as the holder lives -- a binding made
// before the declaration's value arrives names it already -- and generated
// code reads it through the address the holding keeps, at the same place
// whatever the width.
//
// The holding is its holder's last part, and its holder is laid out with room
// for what follows it. The address is kept even where it is that room's own, as
// `SmallVector` keeps where its elements begin while they are inline, so where
// the words lie is decided once, where the holding is built.
template <value::StateDomain kDomain>
class WordsHolding {
 public:
  // What the holder keeps for itself of a value of its type.
  using Owned = value::WideVector<kDomain>;

  // A value `width` bits wide, every position clear in both planes until the
  // declaration's value arrives.
  explicit WordsHolding(std::uint64_t width)
      : width_(width),
        words_(Apart() ? value::AllocateWords(WordCount()) : Trailing()) {
    std::memset(Bytes(), 0, ByteSize());
  }
  WordsHolding(const WordsHolding&) = delete;
  auto operator=(const WordsHolding&) -> WordsHolding& = delete;
  WordsHolding(WordsHolding&&) = delete;
  auto operator=(WordsHolding&&) -> WordsHolding& = delete;
  ~WordsHolding() {
    if (Apart()) {
      value::DeallocateWords(static_cast<std::uint64_t*>(words_), WordCount());
    }
  }

  // Whether the holder may take a write, which one built at its width always
  // may.
  [[nodiscard]] auto IsInstalled() const -> bool {
    return width_ != 0;
  }
  [[nodiscard]] auto Width() const -> std::uint64_t {
    return width_;
  }
  [[nodiscard]] auto ByteSize() const -> std::size_t {
    return value::IntegralBytesFor(width_, kDomain);
  }

  // Where the value's words lie, and where the address of them is kept, which
  // is what generated code reads them through.
  [[nodiscard]] auto Bytes() const -> void* {
    return words_;
  }
  [[nodiscard]] auto BytesThrough() const -> void* const* {
    return &words_;
  }

  // The value where it lies.
  [[nodiscard]] auto Get() const -> value::WideAt<kDomain> {
    return {.bytes = Bytes(), .width = width_};
  }

  // What a holder's protocol asks: whether it holds a value's bits, taking
  // them, a copy of its own of what it holds, and one of a value handed as its
  // bytes.
  [[nodiscard]] auto Holds(const Owned& value) const -> bool {
    return HoldsBytes(value.Bytes());
  }
  void Take(const Owned& value) {
    TakeBytes(value.Bytes());
  }
  [[nodiscard]] auto Holds(const WordsHolding& other) const -> bool {
    return HoldsBytes(other.Bytes());
  }
  void Take(const WordsHolding& other) {
    TakeBytes(other.Bytes());
  }
  [[nodiscard]] auto HoldsBytes(const void* bytes) const -> bool {
    return std::memcmp(Bytes(), bytes, ByteSize()) == 0;
  }
  void TakeBytes(const void* bytes) {
    std::memmove(Bytes(), bytes, ByteSize());
  }
  [[nodiscard]] auto Copy() const -> Owned {
    return CopyOf(Bytes());
  }
  [[nodiscard]] auto CopyOf(const void* bytes) const -> Owned {
    return Owned(width_, bytes);
  }

 private:
  [[nodiscard]] auto Apart() const -> bool {
    return value::UsesHeapWords(width_, kDomain);
  }
  [[nodiscard]] auto WordCount() const -> std::size_t {
    return ByteSize() / sizeof(std::uint64_t);
  }
  // Where the holder ends: the holding is its last part.
  [[nodiscard]] auto Trailing() const -> void* {
    return std::bit_cast<void*>(
        std::bit_cast<std::uintptr_t>(this) + sizeof(WordsHolding));
  }

  std::uint64_t width_ = 0;
  void* words_;
};

// How much storage a `Holder` whose last part is a holding of words takes, for
// a value `bytes` bytes long as its type lays it out: the holder, then the
// value itself, or nothing past the size a value may be and still lie in what
// holds it.
template <class Holder>
[[nodiscard]] constexpr auto HolderSizeFor(std::size_t bytes) -> std::size_t {
  return sizeof(Holder) + (bytes > value::kMaxInlineBytes ? 0 : bytes);
}

// A `Holder` built with room after it for a value `bytes` bytes long, held by
// a count of its owners.
template <class Holder, class... Args>
auto MakeWithRoomAfter(std::size_t bytes, Args&&... args)
    -> std::shared_ptr<Holder> {
  constexpr std::align_val_t kAlign{alignof(Holder)};
  void* room = ::operator new(HolderSizeFor<Holder>(bytes), kAlign);
  return std::shared_ptr<Holder>(
      std::construct_at(
          static_cast<Holder*>(room), std::forward<Args>(args)...),
      [](Holder* made) {
        std::destroy_at(made);
        ::operator delete(made, kAlign);
      });
}

}  // namespace lyra::runtime
