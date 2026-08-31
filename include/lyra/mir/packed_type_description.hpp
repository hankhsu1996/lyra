#pragma once

#include "lyra/mir/expr_id.hpp"
#include "lyra/mir/stmt.hpp"

namespace lyra::mir {

// How one integral type is described at run time, as the expression that builds
// the description (the root of the tree `body` owns; its `stmts` are empty,
// only `exprs` is used). It names neither the type it describes nor its own
// type: whoever asks for one already holds the type it asked about, and every
// description is a value of the one runtime type that states a shape.
struct PackedTypeDescription {
  Block body;
  ExprId value{};
};

}  // namespace lyra::mir
