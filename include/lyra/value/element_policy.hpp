#pragma once

#include <concepts>
#include <cstddef>
#include <cstdint>
#include <new>
#include <utility>

#include "lyra/value/bit_vec.hpp"
#include "lyra/value/integral_words.hpp"

namespace lyra::value {

// A type whose default is the one its declaration states (LRM 7.2.2, Table
// 7-1) rather than what building one from nothing makes: a structure or a
// union the source declared, and an array of either. C++ builds that default
// only from the function stating it, since a constructor handing on to another
// builds a copy first.
template <typename T>
concept HasDeclaredDefault = requires { T::DeclaredDefault(); };

// What a value of `T` starts as before anything is written to it (LRM Table
// 6-7, Table 7-1): the default a type's declaration states, and otherwise what
// its own default construction builds, as a C++ class's value-initialized
// object is. It is of the type alone, so every array of one element type and
// every variable of one type share it.
template <typename T>
[[nodiscard]] auto DefaultOf() -> const T& {
  static const T element_default = [] {
    if constexpr (HasDeclaredDefault<T>) {
      return T::DeclaredDefault();
    } else {
      return T{};
    }
  }();
  return element_default;
}

namespace detail {

// Where a write the language discards lands, which is of the element type
// alone, so every array of one element type shares it.
template <typename T>
[[nodiscard]] auto DiscardedWriteOf() -> T& {
  static T discarded;
  discarded = DefaultOf<T>();
  return discarded;
}

}  // namespace detail

// Where a read through a position naming no element of an array of `T` finds
// the element default, and where a write through one lands (LRM 7.4.5,
// 7.10.1): the addresses an array's own code answers such a position with.
template <typename T>
[[nodiscard]] auto ElementDefault() -> const T* {
  return &DefaultOf<T>();
}
template <typename T>
[[nodiscard]] auto DiscardedWrite() -> T* {
  return &detail::DiscardedWriteOf<T>();
}

// A value whose stream is a run of bits as long as the value makes it (LRM
// 6.24.3): `run`, the run it answers, written below the stream's `filled` most
// significant positions, answering how many are filled after it.
inline auto WriteRunToStream(
    const BitVec& run, const IntegralView& stream, std::uint64_t filled)
    -> std::uint64_t {
  Insert(
      stream, run.View(),
      static_cast<std::int64_t>(stream.width - filled - run.View().width));
  return filled + run.View().width;
}

// Storage for one value of a type that states its size and alignment, given
// back unless it is released: building the value may fail, and the storage is
// then nobody's.
class StorageFor {
 public:
  template <typename Type>
    requires requires(const Type& type) {
      { type.Size() } -> std::same_as<std::size_t>;
      { type.Align() } -> std::same_as<std::size_t>;
    }
  explicit StorageFor(const Type& type)
      : align_(std::align_val_t{type.Align()}),
        storage_(::operator new(type.Size(), align_)) {
  }
  StorageFor(const StorageFor&) = delete;
  auto operator=(const StorageFor&) -> StorageFor& = delete;
  StorageFor(StorageFor&&) = delete;
  auto operator=(StorageFor&&) -> StorageFor& = delete;
  ~StorageFor() {
    if (storage_ != nullptr) {
      ::operator delete(storage_, align_);
    }
  }

  [[nodiscard]] auto Get() const -> void* {
    return storage_;
  }
  [[nodiscard]] auto Release() -> void* {
    return std::exchange(storage_, nullptr);
  }

 private:
  std::align_val_t align_;
  void* storage_;
};

}  // namespace lyra::value
