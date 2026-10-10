#pragma once

#include "lyra/diag/diagnostic.hpp"
#include "lyra/hir/expr_id.hpp"
#include "lyra/lowering/hir_to_mir/expression/expr_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/walk_frame.hpp"
#include "lyra/mir/expr_id.hpp"
#include "lyra/mir/type_id.hpp"

namespace lyra::lowering::hir_to_mir {

// Whether `left`, already evaluated in the frame's block, matches any single
// value the set member `member` stands for (LRM 11.4.13), read as
// `result_type`: a value stands for itself, a range for what lies within it,
// and an unpacked array for every single value it holds at any depth. The set
// operator and a `case ... inside` item (LRM 12.5.4) ask it of each member and
// combine the answers. One template over the pass class serves both contexts;
// explicit instantiations live in the implementation file.
template <ExprLowerer Lowerer>
auto BuildSetMemberTest(
    Lowerer& lowerer, WalkFrame frame, mir::ExprId left, hir::ExprId member,
    mir::TypeId result_type) -> diag::Result<mir::ExprId>;

}  // namespace lyra::lowering::hir_to_mir
