#pragma once

#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/type_id.hpp"

namespace lyra::mir {

// Whether `call`, answering a value of `result`, hands over where the call
// was written, as its last argument: a runtime entry its declaration does not
// excuse, a body of the tool's own text -- a constructor included -- which
// reports at its caller rather than at its own lines (rustc's `track_caller`),
// and a record's or a type's own operation, which answers such an entry. The
// fact is the callee's, read off the entry's declaration or the body's code, so
// every call of it and the body itself agree.
[[nodiscard]] auto TakesCallerLocation(
    const CompilationUnit& unit, const CallExpr& call, TypeId result) -> bool;

}  // namespace lyra::mir
