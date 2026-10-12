#pragma once

#include <concepts>
#include <memory>
#include <type_traits>
#include <utility>

#include "lyra/value/runtime_fixed_array.hpp"
#include "lyra/value/runtime_tuple.hpp"
#include "lyra/value/wide.hpp"

// A value and the handle generated code holds it by. A handle is the address
// of storage laid out as the value's type states: for a value of most domains
// that is the runtime object itself, and for an integral value, a tuple and a
// fixed-size unpacked array it is the value's own bytes, wherever they lie --
// in what holds the value, or inside an object the runtime keeps a copy in.
// These state that correspondence once, in both directions, so no entry spells
// it.
namespace lyra::runtime {

// The handle a value the runtime holds crosses as: its bytes, for a value laid
// out by its type, and the object itself for every other. The value has to
// outlive whoever reads through it.
template <typename T>
auto HandleTo(const T& value) -> const void* {
  if constexpr (requires { value.Bytes(); }) {
    return value.Bytes();
  } else {
    return &value;
  }
}

template <typename T>
auto HandleTo(T& value) -> void* {
  if constexpr (requires { value.Bytes(); }) {
    return value.Bytes();
  } else {
    return &value;
  }
}

// The value a handle names, as the runtime takes it: the object itself, read
// where it lies. An integral value of a type the library states is its bytes,
// so it is read where it lies too. An integral value wider than a word is
// bytes whose extent only what holds one knows, so it is read by its holder,
// and a tuple and a fixed-size unpacked array are bytes whose type only what
// hands them over knows, so each is read with that type.
template <typename T>
auto Read(const void* handle) -> const T& {
  static_assert(
      !value::HeldAsWords<T>,
      "an integral value wider than a word is read at the width its holder "
      "was told");
  static_assert(
      !std::same_as<T, value::RuntimeFixedArray> &&
          !std::same_as<T, value::RuntimeTuple>,
      "a tuple and a fixed-size unpacked array are read with the type their "
      "bytes do not state");
  return *static_cast<const T*>(handle);
}

// A value the runtime holds in a form of its own, which lays the bytes
// generated code holds one by out in storage it is handed: a tuple, the words
// of an integral value wider than a word, and the elements of a fixed-size
// unpacked array.
template <typename T>
concept LaysItsBytesOut = requires(const T& kept, T given, void* out) {
  { kept.CopyInto(out) } -> std::convertible_to<void*>;
  { std::move(given).MoveInto(out) } -> std::convertible_to<void*>;
};

// Builds what an entry answers with in the storage the call handed it,
// answering with that storage: the value itself where the entry gives it up,
// and a copy where the runtime goes on holding it. The body that gave the
// storage is the one that ends what is built there, at the end of the
// evaluation that asked for it. A value held as its bytes leaves those bytes
// there.
template <typename T>
auto Emplace(void* out, T&& value) -> void* {
  using Value = std::remove_cvref_t<T>;
  if constexpr (!LaysItsBytesOut<Value>) {
    return std::construct_at(static_cast<Value*>(out), std::forward<T>(value));
  } else if constexpr (
      std::is_rvalue_reference_v<T&&> &&
      !std::is_const_v<std::remove_reference_t<T>>) {
    return std::forward<T>(value).MoveInto(out);
  } else {
    return value.CopyInto(out);
  }
}

}  // namespace lyra::runtime
