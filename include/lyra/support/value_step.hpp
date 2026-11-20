#pragma once

#include <cstdint>
#include <string_view>

namespace lyra::support {

// One step of a value's life that its type says how to carry out: a second
// value equal to one only read, built in storage holding nothing; a value
// relocated into such storage, its ownership taken from where it lay, which is
// owed nothing afterwards; one value written over the one storage already
// holds; and the end of a value.
enum class ValueStep : std::uint8_t { kCopy, kRelocate, kAssign, kEnd };

// The step's stable spelling, which names the function a unit states for it.
auto ValueStepName(ValueStep step) -> std::string_view;

}  // namespace lyra::support
