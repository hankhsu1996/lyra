#pragma once

// The formats of LRM Table 38-3, in which foreign code reads a value and
// hands one to be written: what a value of each shape is in each format that
// applies to it, and what a value written in one is as that shape.

#include "lyra/runtime/vpi_object.hpp"
#include "lyra/runtime/vpi_variable.hpp"
#include "vpi_user.h"

namespace lyra::runtime {

// Fills `value` with `held`, a value of `shape`, in the format it names, the
// shape's own where it names `vpiObjTypeVal`, which it then names instead
// (LRM 38.15). What the value points at is kept until the next read.
[[nodiscard]] auto FillValue(
    const ValueShape& shape, const HeldValue& held, s_vpi_value& value)
    -> VpiAnswer<void>;

// The value of `shape` that `value` writes (LRM 38.34).
[[nodiscard]] auto ValueToWrite(
    const ValueShape& shape, const s_vpi_value& value) -> VpiAnswer<HeldValue>;

}  // namespace lyra::runtime
