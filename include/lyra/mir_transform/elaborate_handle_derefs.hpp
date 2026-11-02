#pragma once

#include <optional>

#include "lyra/mir/callable_code.hpp"
#include "lyra/mir/code_source.hpp"
#include "lyra/mir_transform/pass.hpp"

namespace lyra::mir_transform {

// States every dereference of a class handle as the library's opening of the
// handle followed by an ordinary dereference of the pointer it answers, as
// rustc's `ElaborateBoxDerefs` turns `*b` on a `Box` into a dereference of the
// pointer inside it, and as clang states `p->f` on a smart pointer as a call of
// its `operator->`. A handle naming no object fails the design (LRM 8.4), so
// opening one is an operation of the library like any other that can fail, and
// is handed where it was written by the rewrite that follows; every target then
// dereferences only pointers.
auto ElaborateHandleDerefsOnBody(
    const PassContext& context, const mir::CodeSource& at,
    const mir::CallableCode& code) -> std::optional<mir::CallableCode>;

}  // namespace lyra::mir_transform
