#pragma once

#include "lyra/runtime/storage_for.hpp"
#include "lyra/runtime/value_storage_core.hpp"
#include "lyra/value/concepts.hpp"

namespace lyra::runtime {

// Storage holding a value nothing observes: a property of a class's object
// (LRM 8.5), and what a call that suspends answers into, which its caller
// reads once the call completes. A write to either is never an update event
// and wakes no subscriber, so the cell is its value's storage and nothing
// more, which is the whole difference from a variable's cell.
template <value::LyraValue T>
class ValueCell : public ValueStorageCore<T> {};

// The same of a value lying after the cell, held as `HeldAs` holds it -- the
// words of an integral value wider than a word, or a tuple, a union or a
// fixed-size unpacked array -- told its width or type where the cell is built,
// and holding what that type builds before anything writes it from then.
template <class HeldAs>
class TailLocal {
 public:
  template <class Told>
  explicit TailLocal(const Told& told) : held_(told) {
  }

  [[nodiscard]] auto BytesThrough() const -> void* const* {
    return held_.BytesThrough();
  }

 private:
  HeldAs held_;
};

// The cell a procedural local of `T` is.
template <class T>
struct LocalOf {
  using Type = ValueCell<T>;
};
template <DynamicallySized T>
struct LocalOf<T> {
  using Type = TailLocal<typename StorageFor<T>::Type>;
};

}  // namespace lyra::runtime
