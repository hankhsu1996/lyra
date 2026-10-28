#pragma once

#include <concepts>
#include <utility>

#include "lyra/base/internal_error.hpp"
#include "lyra/runtime/value_handle.hpp"
#include "lyra/value/concepts.hpp"
#include "lyra/value/small_box.hpp"

namespace lyra::runtime {

// A value whose representation is a run-time fact rather than its type's: it
// starts with none, takes one at its first write, and can be asked whether
// another value has the same one. A value of any other type has its
// representation from its type, so a cell holding one is installed from the
// start and every value of the type fits it.
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

  ValueStorageCore() = default;
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

  // True once the cell carries its declared representation, which a value
  // represented at run time takes at its first write.
  [[nodiscard]] auto IsInstalled() const noexcept -> bool {
    if constexpr (RepresentedAtRunTime<T>) {
      return !held_->IsUninitialized();
    } else {
      return true;
    }
  }

  // Installs the declared representation and initial contents from a prototype
  // -- the one-time write that fixes the cell's type. Only the prototype's
  // representation is load-bearing; its contents become the initial contents.
  void Install(const T& prototype) {
    *held_ = prototype;
  }
  void Install(T&& prototype) {
    *held_ = std::move(prototype);
  }

  // Overwrites the contents with a value already at the declared
  // representation. A value represented at run time whose representation does
  // not match the cell's is a missing upstream conversion, surfaced as a
  // compiler bug rather than silently reshaping the cell. Whether the contents
  // moved is not answered here: only whoever is armed to read that answer
  // should pay for it, and this is the one path a write takes whether or not
  // anything is.
  void Overwrite(const T& value) {
    if constexpr (RepresentedAtRunTime<T>) {
      if (!held_->SameRepresentation(value)) {
        throw InternalError(
            "ValueStorageCore::Overwrite: stored value's representation does "
            "not match the cell's declared type; a required conversion was not "
            "emitted");
      }
    }
    *held_ = value;
  }

  // What a holder's protocol asks of how it holds its value: whether it holds
  // `value`'s bits, assigning them, and what it holds as its owned type, which
  // this holder keeps and so answers where it lies. The first value assigned
  // installs the representation.
  [[nodiscard]] auto IsBitIdentical(const T& value) const -> bool {
    return held_->IsBitIdentical(value);
  }
  void Assign(const T& value) {
    if (!IsInstalled()) {
      Install(value);
      return;
    }
    Overwrite(value);
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
