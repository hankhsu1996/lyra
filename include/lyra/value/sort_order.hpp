#pragma once

#include <cstddef>
#include <vector>

#include "lyra/value/container_core.hpp"

namespace lyra::value {

// LRM 7.12.2 `sort` and `rsort`: the position each key the dynamic array
// `keys` holds comes from once they are put in ascending order, or descending
// where `descending` says. The keys are of a type the relational operators
// order -- integral, real, shortreal or string (LRM 11.4.4, 6.16) -- which the
// core's record of their type says, so each kind is compared as itself and no
// comparison is a call. Equal keys keep their order, and so do two whose
// comparison is unknown, a key holding x or z coming before none.
[[nodiscard]] auto SortOrder(const DynamicArrayCore& keys, bool descending)
    -> std::vector<std::size_t>;

}  // namespace lyra::value
