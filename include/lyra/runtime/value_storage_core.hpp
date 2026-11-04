#pragma once

#include <concepts>
#include <utility>

#include "lyra/runtime/value_handle.hpp"
#include "lyra/value/concepts.hpp"
#include "lyra/value/small_box.hpp"

namespace lyra::runtime {

// A value whose representation is a run-time fact rather than its type's: it
// starts with none, takes one at its first write, and can be asked whether
// another value has the same one.
template <typename T>
concept RepresentedAtRunTime = requires(const T& value, const T& other) {
  { value.IsUninitialized() } -> std::same_as<bool>;
  { value.SameRepresentation(other) } -> std::same_as<bool>;
};

// The storage half of a value cell, with no observation attached: it owns one
// value of the declared representation and preserves that representation across
// writes (LRM 4.3). It is the shared substrate under an observable signal cell,
// which adds subscriber wakeup, and a cell nothing observes. Non-copyable and
// non-movable: a cell is addressed, so its identity is its address.
//
// It is how a cell holds a value whose type is known where it is compiled, so
// what the cell keeps for itself is a value of that type too. A value that does
// not lie inline by the rule every holder reads is kept in storage the cell
// owns, so a cell keeps at most that bound of a value wherever it is declared.
template <value::LyraValue T>
class ValueStorageCore {
 public:
  using Owned = T;

  // Holds the value `T` builds from `args`, so the storage holds a value of
  // its type from the moment it is built: from nothing where the library is
  // compiled with the type, from a container's type for a container it holds
  // for every element type alike, or as a copy of a value.
  template <class... Args>
    requires std::constructible_from<T, const Args&...>
  explicit ValueStorageCore(const Args&... args) : held_(args...) {
  }
  ValueStorageCore(const ValueStorageCore&) = delete;
  auto operator=(const ValueStorageCore&) -> ValueStorageCore& = delete;
  ValueStorageCore(ValueStorageCore&&) = delete;
  auto operator=(ValueStorageCore&&) -> ValueStorageCore& = delete;
  ~ValueStorageCore() = default;

  [[nodiscard]] auto Get() const noexcept -> const T& {
    return *held_;
  }

  // The contents as storage a caller descends into, for a write that reaches
  // part of them. Taking this reference changes nothing and tells nobody: what
  // decides an update event is the part written, which only whoever performs
  // that write knows, so the write is bracketed by the handle the cell hands
  // out for it rather than by this.
  [[nodiscard]] auto Storage() noexcept -> T& {
    return *held_;
  }

  // What a holder's protocol asks of how it holds its value: whether it holds
  // `value`'s bits, assigning them, and what it holds as its owned type, which
  // this holder keeps and so answers where it lies. Whether an assignment
  // moved the contents is not answered here: only whoever is armed to read
  // that answer should pay for it, and this is the one path a write takes
  // whether or not anything is.
  [[nodiscard]] auto IsBitIdentical(const T& value) const -> bool {
    return held_->IsBitIdentical(value);
  }
  void Assign(const T& value) {
    *held_ = value;
  }
  void Assign(T&& value) {
    *held_ = std::move(value);
  }
  [[nodiscard]] auto IsBitIdentical(const ValueStorageCore& other) const
      -> bool {
    return IsBitIdentical(*other.held_);
  }
  void Assign(const ValueStorageCore& other) {
    Assign(*other.held_);
  }
  [[nodiscard]] auto AsOwned() const -> const T& {
    return *held_;
  }

  // Where the held value lies as a reference names it.
  [[nodiscard]] auto Bytes() -> void* {
    return HandleTo(*held_);
  }

  // The same of a value lying at `bytes` as a reference names one.
  [[nodiscard]] auto IsBitIdenticalTo(const void* bytes) const -> bool {
    return held_->IsBitIdentical(Read<T>(bytes));
  }
  void AssignBytes(const void* bytes) {
    *held_ = Read<T>(bytes);
  }

 private:
  value::SmallBox<T> held_;
};

}  // namespace lyra::runtime
