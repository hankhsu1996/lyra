#pragma once

#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/integral_constant_id.hpp"
#include "lyra/mir/value_build.hpp"

namespace lyra::mir {

// How `constant` is built, as the expression that builds it. A constant is a
// function of its own bits and type alone: it reads no body and no other entry,
// so the answer does not depend on when it is asked for.
//
// It does name the description of its own type, which is what puts that
// description in the unit -- so a unit that has not been through here yet holds
// fewer descriptions than its program refers to.
[[nodiscard]] auto MaterializeIntegralConstant(
    const CompilationUnit& unit, IntegralConstantId constant) -> ValueBuild;

}  // namespace lyra::mir
