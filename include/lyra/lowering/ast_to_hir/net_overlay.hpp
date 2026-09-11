#pragma once

#include <cstdint>
#include <vector>

#include <slang/ast/Expression.h>
#include <slang/ast/Symbol.h>

#include "lyra/diag/diag_code.hpp"
#include "lyra/diag/diagnostic.hpp"
#include "lyra/hir/structural_scope.hpp"
#include "lyra/lowering/ast_to_hir/walk_frame.hpp"

namespace lyra::lowering::ast_to_hir {

class StructuralScopeLowerer;

// A run of one net's positions: the net, and where among its positions the run
// starts and how many it covers. Positions rather than the coordinate the
// source wrote, because what connectivity composes is expressible only as a
// position: joining a run of `a` to one of `b` and a run of `b` to one of `c`
// puts runs of `a` and `c` in one resolution, and the two may be declared in
// opposite directions, so where they meet is a select of neither.
struct NetRun {
  hir::ExprId net;
  std::uint32_t offset{};
  std::uint32_t width{};
};

// One side of an overlay: the runs it names, most significant first. Their
// widths sum to the width of the side, which is what LRM 10.11 requires of
// every side of one statement.
using NetSide = std::vector<NetRun>;

// The runs a net lvalue names (LRM 10.11 `net_lvalue`). A name with a constant
// select is one run; a concatenation is the runs of its operands, in the order
// written. `eval_scope` is the symbol the selects are folded against, and
// `code` is the construct's own diagnostic code, since what reaches this is a
// port connection or an alias and a refusal belongs to whichever one it is.
auto NetRunsOfLvalue(
    StructuralScopeLowerer& scope, const slang::ast::Symbol& eval_scope,
    const slang::ast::Expression& expr, diag::SourceSpan span,
    diag::DiagCode code, WalkFrame frame) -> diag::Result<NetSide>;

// What two sides of one statement say about each other. LRM 10.11 gives an
// overlay the bit overlay rules of a packed union with the same member types,
// so correspondence runs position-wise from the most significant end. The two
// sides' runs need not fall at the same boundaries, so each coupling is as wide
// as the shorter of the two runs it stands between, and whichever side it
// exhausts advances.
auto CoupleSides(
    const NetSide& left, const NetSide& right, diag::SourceSpan span)
    -> std::vector<hir::NetJoin>;

}  // namespace lyra::lowering::ast_to_hir
