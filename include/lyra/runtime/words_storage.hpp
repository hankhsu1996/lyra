#pragma once

#include <cstddef>
#include <cstdint>
#include <cstring>

#include "lyra/runtime/tail.hpp"
#include "lyra/value/integral.hpp"
#include "lyra/value/integral_words.hpp"
#include "lyra/value/wide.hpp"
#include "lyra/value/word_recycler.hpp"

namespace lyra::runtime {

// The storage of an integral value wider than a word, whose width the library
// is told where the holder is built: the width, then the value's words, inline
// at the holder's tail or spilled into storage allocated where the holder is
// built. So the value has one address for as long as the holder lives -- a
// binding made before the declaration's value arrives names it already. The
// type fixes the value a declaration holds before anything writes it (LRM
// Table 6-7), so the storage holds that from the start.
template <value::StateDomain kDomain>
class WordsStorage : public TailValue<WordsStorage<kDomain>> {
 public:
  using Owned = value::WideVector<kDomain>;

  explicit WordsStorage(std::uint64_t width)
      : TailValue<WordsStorage>(
            IsInline(ValueLayoutFor(width))
                ? nullptr
                : value::AllocateWords(
                      BytesFor(width) / sizeof(std::uint64_t))),
        width_(width) {
    std::memset(this->Bytes(), 0, ByteSize());
    value::FillDefault(value::WideMutableViewAt(this->Bytes(), width, kDomain));
  }
  WordsStorage(const WordsStorage&) = delete;
  auto operator=(const WordsStorage&) -> WordsStorage& = delete;
  WordsStorage(WordsStorage&&) = delete;
  auto operator=(WordsStorage&&) -> WordsStorage& = delete;
  ~WordsStorage() {
    if (this->IsSpilled()) {
      value::DeallocateWords(
          static_cast<std::uint64_t*>(this->Bytes()),
          ByteSize() / sizeof(std::uint64_t));
    }
  }

  // Whether the storage holds a value a write may replace, which it does from
  // the moment it is built.
  [[nodiscard]] static auto IsInstalled() -> bool {
    return true;
  }
  [[nodiscard]] auto Width() const -> std::uint64_t {
    return width_;
  }
  [[nodiscard]] auto ByteSize() const -> std::size_t {
    return BytesFor(width_);
  }
  [[nodiscard]] auto ValueLayout() const -> Layout {
    return ValueLayoutFor(width_);
  }

  // What a value `width` bits wide takes.
  [[nodiscard]] static auto ValueLayoutFor(std::uint64_t width) -> Layout {
    return {.size = BytesFor(width), .align = alignof(std::uint64_t)};
  }

  // The value where it lies.
  [[nodiscard]] auto Get() const -> value::WideAt<kDomain> {
    return {.bytes = this->Bytes(), .width = width_};
  }

  // Whether the value at `bytes` is the one held, taking it, a copy of the
  // value held, and a copy of the one at `bytes`.
  [[nodiscard]] auto IsBitIdenticalTo(const void* bytes) const -> bool {
    return std::memcmp(this->Bytes(), bytes, ByteSize()) == 0;
  }
  void AssignBytes(const void* bytes) {
    std::memmove(this->Bytes(), bytes, ByteSize());
  }
  [[nodiscard]] auto Copy() const -> Owned {
    return ToOwned(this->Bytes());
  }
  [[nodiscard]] auto ToOwned(const void* bytes) const -> Owned {
    return Owned(width_, bytes);
  }

 private:
  [[nodiscard]] static auto BytesFor(std::uint64_t width) -> std::size_t {
    return value::IntegralBytesFor(width, kDomain);
  }

  std::uint64_t width_;
};

}  // namespace lyra::runtime
