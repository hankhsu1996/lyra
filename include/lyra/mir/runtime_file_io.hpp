#pragma once

#include <optional>

#include "lyra/mir/expr_id.hpp"

namespace lyra::mir {

// LRM 21.3.1 $fopen. `mode` is absent for MCD form, present for FD form.
// Both operands are string-typed expressions; mode parses at runtime so a
// non-literal SV variable can flow through unchanged.
struct RuntimeFileOpenCall {
  ExprId name{};
  std::optional<ExprId> mode = std::nullopt;
};

// LRM 21.3.1 $fclose. `descriptor` is an int-typed expression; the runtime
// decides whether the value is an MCD (iterates set bits) or an FD (single
// descriptor) per the LRM bit-31 convention.
struct RuntimeFileCloseCall {
  ExprId descriptor{};
};

}  // namespace lyra::mir
