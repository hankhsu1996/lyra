#pragma once

// A guard the language requires to run as part of evaluating an access, rather
// than as a check ahead of it: LRM 11.3.5 forbids a short-circuited operand
// from raising the run-time errors its evaluation would have, so a check that
// belongs to an access cannot be hoisted out of the expression. Yielding the
// guarded value is what lets the access compose onto the guard instead of
// duplicating it, and is why this is a guard over any value rather than an
// operation of one value family.

#include <string_view>

#include "lyra/value/packed_array.hpp"

namespace lyra::value {

// Raises `message` as a simulation error unless `condition` is a definite one.
// An unknown answer decides nothing, so it cannot be what lets an access
// through.
void RequireCondition(const PackedArray& condition, std::string_view message);

template <typename T>
[[nodiscard]] auto Require(
    T& value, const PackedArray& condition, std::string_view message) -> T& {
  RequireCondition(condition, message);
  return value;
}

template <typename T>
[[nodiscard]] auto Require(
    const T& value, const PackedArray& condition, std::string_view message)
    -> const T& {
  RequireCondition(condition, message);
  return value;
}

}  // namespace lyra::value
