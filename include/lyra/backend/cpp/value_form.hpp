#pragma once

#include <cstdint>
#include <string>
#include <string_view>
#include <vector>

#include "lyra/mir/type.hpp"

namespace lyra::backend::cpp {

// How the rendered operands of a value-build primitive reach the operation:
// as its argument list, or as one sequence it takes whole.
enum class OperandGrouping : std::uint8_t {
  kArgumentList,
  kSequenceArgument,
};

// How one MIR value-build primitive is realized in the target. The operation is
// a member of the type its result has, so the form carries the member alone --
// what that type is called is type mapping's answer, and naming it here would
// be a second place the target's spelling lives.
struct ValueForm {
  std::string_view member;
  OperandGrouping grouping;
};

// The value-axis peer of the type-mapping dispatch. A runtime library operation
// is named here and nowhere else, so a value-emission entry states which
// primitive it is rendering and never what the target calls it, the same way it
// asks type mapping how to spell a type rather than composing one.
//
// A build primitive is a value that is its own parts, which for these two means
// an integral: a concatenation or replication of anything else comes into
// existence through its library type's own entry and reaches render as an
// ordinary call.
[[nodiscard]] auto ConcatValueForm(const mir::Type& result) -> ValueForm;
[[nodiscard]] auto ReplicationValueForm(const mir::Type& result) -> ValueForm;

// Composes one form around operands that are already rendered. `result_type` is
// the target's spelling of the result's type, which qualifies the member.
[[nodiscard]] auto RenderValueForm(
    const ValueForm& form, std::string_view result_type,
    const std::vector<std::string>& operands) -> std::string;

}  // namespace lyra::backend::cpp
