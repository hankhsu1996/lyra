#pragma once

#include <vector>

#include "lyra/hir/param_direction.hpp"
#include "lyra/hir/subroutine_kind.hpp"
#include "lyra/hir/type_id.hpp"

namespace lyra::hir {

// One formal of a callee in another compilation unit. The direction classifies
// how the actual is marshalled at the boundary (LRM 13.5); the type is the
// formal's type interned in the referring unit, needed to shape the completion
// payload an output / inout rides back in and the writeback assignment.
struct ExternalCalleeParam {
  ParamDirection direction = ParamDirection::kInput;
  TypeId type{};

  auto operator==(const ExternalCalleeParam&) const -> bool = default;
};

// What a call needs to know about a callee in another compilation unit beyond
// its name. A callee its unit published states it on that unit's signature,
// where the call reads it; one reached only by a name resolved while the design
// elaborates has no signature, so the call takes it from the declaration the
// front end resolved the name to, which is also what the callee is lowered
// from.
//
// `kind` is the call protocol: a task enable suspends the caller until
// completion (LRM 13.3), so the call site awaits it. `params` is the argument
// marshalling: an output or inout argument rides back in the callee's
// completion, so the call site reproduces that completion's shape from the
// formals' directions and types. The result component of that completion is the
// call's own result type -- the enclosing expression's -- so it is not recorded
// here.
struct ExternalCalleeInterface {
  SubroutineKind kind = SubroutineKind::kFunction;
  std::vector<ExternalCalleeParam> params;

  auto operator==(const ExternalCalleeInterface&) const -> bool = default;
};

}  // namespace lyra::hir
