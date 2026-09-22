#pragma once

#include "lyra/mir/compilation_unit.hpp"

namespace lyra::mir {

// Settles how every value `unit` holds is brought into existence, once its
// bodies are lowered and before any consumer reads it.
//
// The constants are settled first: building one names the description of its
// own type, so the set of descriptions is not complete until every constant has
// been through here. The descriptions follow in one pass and need no second,
// because a description is made of records and machine integers and names no
// value the unit holds.
//
// Doing this once is what makes the two pools a fact rather than a question. A
// consumer that settled them as it walked would be reading pools it was still
// adding to, and two consumers doing it separately would each see a different
// set -- which is the same fact answered twice, held in step by nothing.
void SettleUnitValues(CompilationUnit& unit);

}  // namespace lyra::mir
