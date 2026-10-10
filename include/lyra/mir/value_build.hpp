#pragma once

#include "lyra/mir/expr_id.hpp"
#include "lyra/mir/stmt.hpp"

namespace lyra::mir {

// How one value settled before the program runs is brought into existence, as
// the expression that builds it (the root of the tree `body` owns; its `stmts`
// are empty, only `exprs` is used). It names neither what it builds nor the
// type of it: whoever asks for one already holds the identity it asked about.
struct ValueBuild {
  Block body;
  ExprId value{};
};

}  // namespace lyra::mir
