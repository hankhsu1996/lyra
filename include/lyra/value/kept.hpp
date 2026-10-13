#pragma once

#include <cstddef>
#include <cstdint>
#include <memory>
#include <new>
#include <utility>

#include "lyra/value/integral_words.hpp"
#include "lyra/value/word_recycler.hpp"

namespace lyra::value {

// Where a value is kept when what keeps it would otherwise be a frame and the
// value is too large for one (`kMaxInlineBytes`): storage of its own, aligned
// as the value asks. A design makes and ends such values many times in a time
// step, at the sizes its types fix, so storage a word's alignment serves is a
// buffer of words, and a freed one is reused for the next as long.
struct KeptStorage {
  std::size_t align;

  [[nodiscard]] auto Obtain(std::size_t bytes) const -> void* {
    if (align <= alignof(std::uint64_t)) {
      return AllocateWords(WordCountForBits(bytes * 8U));
    }
    return ::operator new(bytes, std::align_val_t{align});
  }
  void GiveBack(void* storage, std::size_t bytes) const noexcept {
    if (align <= alignof(std::uint64_t)) {
      DeallocateWords(
          static_cast<std::uint64_t*>(storage), WordCountForBits(bytes * 8U));
      return;
    }
    ::operator delete(storage, std::align_val_t{align});
  }
};

// The storage a value of `T` is kept in.
template <typename T>
inline constexpr KeptStorage kKeptStorageFor{.align = alignof(T)};

}  // namespace lyra::value

// `new (lyra::value::kKeptStorageFor<T>) T(value)` builds `value` where it is
// kept, straight from the expression that makes it: a value an expression
// answers initializes the object it is built in, so no copy of it is made on
// the way and none lies in a frame.
[[nodiscard]] inline auto operator new(
    std::size_t bytes, const lyra::value::KeptStorage& storage) -> void* {
  return storage.Obtain(bytes);
}
inline void operator delete(
    void* built, std::size_t bytes,
    const lyra::value::KeptStorage& storage) noexcept {
  storage.GiveBack(built, bytes);
}

namespace lyra::value {

// A value kept apart from what keeps it, which holds its address: what a
// local, a parameter, an answer and a value an expression makes are where the
// value is too large for a frame. It owns one `T`, read and written as that
// `T`, and copying it copies the value.
template <typename T>
class Kept {
 public:
  // Takes the value a `new` in kept storage built.
  explicit Kept(T* built) noexcept : held_(built) {
  }
  explicit Kept(const T& value) : held_(new(kKeptStorageFor<T>) T(value)) {
  }
  Kept(const Kept& other) : Kept(*other) {
  }
  Kept(Kept&& other) noexcept = default;
  // What was moved from holds nothing, and takes a value of its own.
  auto operator=(const Kept& other) -> Kept& {
    if (this == &other) {
      return *this;
    }
    if (held_ == nullptr) {
      held_.reset(new (kKeptStorageFor<T>) T(*other));
    } else {
      *held_ = *other;
    }
    return *this;
  }
  auto operator=(Kept&& other) noexcept -> Kept& = default;
  ~Kept() = default;

  [[nodiscard]] auto operator*() const -> const T& {
    return *held_;
  }
  [[nodiscard]] auto operator*() -> T& {
    return *held_;
  }
  [[nodiscard]] auto operator->() const -> const T* {
    return held_.get();
  }
  [[nodiscard]] auto operator->() -> T* {
    return held_.get();
  }

 private:
  // Ends the value and gives its storage back.
  struct GiveBack {
    void operator()(T* built) const noexcept {
      std::destroy_at(built);
      kKeptStorageFor<T>.GiveBack(built, sizeof(T));
    }
  };

  std::unique_ptr<T, GiveBack> held_;
};

}  // namespace lyra::value
