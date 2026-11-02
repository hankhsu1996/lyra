#pragma once

#include <optional>

#include "lyra/mir/callable_code.hpp"
#include "lyra/mir/code_source.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir_transform/pass.hpp"

namespace lyra::mir_transform {

// Replaces an operation whose answer is one of its operands with that operand,
// as rustc's and LLVM's InstSimplify do. Reading every part of a value from
// its first, at the value's own kind of part, is reading the value, and
// writing them is writing it, so a select of the whole is the whole wherever
// it stands.
auto InstSimplifyOnBody(
    const PassContext& context, const mir::CodeSource& at,
    const mir::CallableCode& code) -> std::optional<mir::CallableCode>;

}  // namespace lyra::mir_transform
