#pragma once

#include <optional>

#include "lyra/mir/callable_code.hpp"
#include "lyra/mir_transform/pass.hpp"

namespace lyra::mir_transform {

// Makes each operator on an integral value wider than the widest integer a
// machine operates on at once a call of the operation its type answers it
// with, a body built at the type's width over the words the value lies in, as
// a legalization expands an operation on a type the machine has no instruction
// for into ones it has. A conversion to a type no wider than a machine's
// integers is the operand's low part, as LLVM legalizes a truncation, so it
// makes no call of its own.
auto ExpandWideIntegralsOnBody(
    const PassContext& context, const mir::CallableCode& code)
    -> std::optional<mir::CallableCode>;

}  // namespace lyra::mir_transform
