#pragma once

#include "lyra/value/integral_fwd.hpp"
#include "lyra/value/integral_words.hpp"

namespace lyra::value {

// Every position of `value`'s representation that its type keeps clear made
// clear: what lies above each integral's width, in both planes. A value read
// out of storage another member of an untagged union may have written holds
// whatever that member put there, and nothing that compares or tests storage
// would see the value as its own type holds it (LRM 7.3.2). A value made of
// parts clears each part, which its own type says how to reach; a real's bits
// are all its own and an untagged union's are whatever its members' are, so
// neither keeps any clear.
template <typename T>
void ClearAboveWidths(T& value) {
  if constexpr (IntegralValue<T>) {
    const auto view = value.MutableView();
    ClearAboveWidth(view.value, view.width);
    ClearAboveWidth(view.unknown, view.width);
  } else if constexpr (requires { value.ClearAboveWidths(); }) {
    value.ClearAboveWidths();
  }
}

}  // namespace lyra::value
