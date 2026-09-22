#pragma once

#include "lyra/base/translation.hpp"
#include "lyra/mir/expr_id.hpp"
#include "lyra/mir/integral_constant_id.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/mir/type_descriptor_id.hpp"

namespace lyra::mir {

// How one value the unit holds is brought into existence, as the expression
// that builds it (the root of the tree `body` owns; its `stmts` are empty, only
// `exprs` is used). It names neither what it builds nor the type of it:
// whoever asks for one already holds the identity it asked about.
//
// A type's run-time description and a constant the source wrote are both such a
// value -- settled before the program runs, named by every use, and built once
// for the artifact rather than wherever control reaches a use.
struct ValueBuild {
  Block body;
  ExprId value{};
};

// How every value one unit holds is brought into existence, one entry per
// constant and one per description, each keyed by the pool that holds it.
//
// They are settled once, when the unit is finished, which is what makes the two
// pools something the unit states rather than something a reader arrives at:
// building a constant names the description of its own type, so anyone building
// them while walking would be walking a pool it was still adding to.
struct UnitValueBuilds {
  base::Translation<IntegralConstantId, ValueBuild> constants;
  base::Translation<TypeDescriptorId, ValueBuild> descriptors;
};

}  // namespace lyra::mir
