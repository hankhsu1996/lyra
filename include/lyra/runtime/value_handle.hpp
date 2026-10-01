#pragma once

#include <concepts>
#include <memory>
#include <type_traits>
#include <utility>

#include "lyra/value/runtime_tuple.hpp"

// A value and the handle generated code holds it by. A handle is the address
// of storage laid out as the value's type states: for a value of every domain
// that is the runtime object itself, and for a tuple it is the tuple's own
// bytes, which the runtime holds inside the tuple domain's object. These
// state that correspondence once, in both directions, so no entry spells it.
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
// where it lies, and for a tuple a copy of it in the one form the runtime
// holds every tuple in.
template <typename T>
auto Read(const void* handle) -> decltype(auto) {
  if constexpr (std::same_as<T, value::RuntimeTuple>) {
    return value::RuntimeTuple::CopyOf(handle);
  } else {
    return *static_cast<const T*>(handle);
  }
}

// Builds what an entry answers with in the storage the call handed it,
// answering with that storage: the value itself where the entry gives it up,
// and a copy where the runtime goes on holding it. The body that gave the
// storage is the one that ends what is built there, at the end of the
// evaluation that asked for it.
template <typename T>
auto Emplace(void* out, T&& value) -> void* {
  using Value = std::remove_cvref_t<T>;
  if constexpr (!std::same_as<Value, value::RuntimeTuple>) {
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
