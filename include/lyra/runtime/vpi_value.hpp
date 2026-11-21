#pragma once

// The formats of LRM Table 38-3, in which foreign code reads a value and
// hands one to be written: what a value of each shape is in each format that
// applies to it, and what a value written in one is as that shape.

#include <string>
#include <vector>

#include "lyra/runtime/vpi_object.hpp"
#include "lyra/runtime/vpi_variable.hpp"
#include "vpi_user.h"

namespace lyra::runtime {

// What a value handed to foreign code points at -- its text, its words, its
// strengths, its time -- kept for as long as the value is to be read.
struct VpiValueStore {
  std::string text;
  std::vector<s_vpi_vecval> vector;
  std::vector<s_vpi_strengthval> strength;
  s_vpi_time time{};
};

// Where a value `vpi_get_value` hands is kept: until the next read (LRM
// 38.15).
[[nodiscard]] auto ReadValueStore() -> VpiValueStore&;

// Fills `value` with `held`, a value of `shape`, in the format it names, the
// shape's own where it names `vpiObjTypeVal`, which it then names instead
// (LRM 38.15). What the value points at is kept in `store`.
[[nodiscard]] auto FillValue(
    const ValueShape& shape, const HeldValue& held, s_vpi_value& value,
    VpiValueStore& store) -> VpiAnswer<void>;

// The value of `shape` that `value` writes (LRM 38.34).
[[nodiscard]] auto ValueToWrite(
    const ValueShape& shape, const s_vpi_value& value) -> VpiAnswer<HeldValue>;

}  // namespace lyra::runtime
