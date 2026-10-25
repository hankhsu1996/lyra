#pragma once

#include <optional>
#include <string>
#include <string_view>

#include "lyra/diag/diagnostic.hpp"
#include "lyra/hir/class_ref.hpp"
#include "lyra/hir/compilation_unit.hpp"
#include "lyra/hir/expr.hpp"
#include "lyra/hir/stmt.hpp"
#include "lyra/hir/type.hpp"
#include "lyra/hir/type_id.hpp"
#include "lyra/lowering/hir_to_mir/walk_frame.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/expr_id.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/mir/type_id.hpp"

namespace lyra::lowering::hir_to_mir {

class ProcessLowerer;
class UnitLowerer;

// What a random variable of type `type` is made of, each of which the call
// chooses: the variable itself, or each element of an array, at any depth
// (LRM 18.4).
[[nodiscard]] auto ChosenValueType(const hir::TypePool& types, hir::TypeId type)
    -> hir::TypeId;

// The members of the enumeration a variable of type `type` is made of, where
// it is made of one, which are all a chosen one takes (LRM 18.5); nothing for
// any other type.
[[nodiscard]] auto ChosenMembers(
    UnitLowerer& unit_lowerer, mir::Block& block, hir::TypeId type)
    -> std::optional<mir::ExprId>;

// The name the mode of the static constraint block `block` is kept under, which
// is the mode of `introducer`, the class that introduced it (LRM 18.5.10): the
// unit declaring the class, the class's path in it, and the block's name, so
// every unit naming the block names the one mode.
[[nodiscard]] auto StaticConstraintModeKey(
    const hir::CompilationUnit& unit, hir::ClassId introducer,
    std::string_view block) -> std::string;

// What a constraint block states (LRM 18.5), in the body of the method it is.
[[nodiscard]] auto LowerConstraintStmt(
    ProcessLowerer& lowerer, const WalkFrame& frame,
    std::optional<std::string> label, const hir::ConstraintStmt& stmt)
    -> diag::Result<mir::Stmt>;

// A call of `randomize()` (LRM 18.6.1, 18.7, 18.11, 18.12): the steps of the
// call, in a block answering its result.
[[nodiscard]] auto LowerRandomizeExpr(
    ProcessLowerer& lowerer, const WalkFrame& frame,
    const hir::RandomizeExpr& call, mir::TypeId result_type)
    -> diag::Result<mir::Expr>;

// `rand_mode()` and `constraint_mode()` (LRM 18.8, 18.9).
[[nodiscard]] auto LowerRandModeSetExpr(
    ProcessLowerer& lowerer, const WalkFrame& frame,
    const hir::RandModeSetExpr& call) -> diag::Result<mir::Expr>;
[[nodiscard]] auto LowerRandModeGetExpr(
    ProcessLowerer& lowerer, const WalkFrame& frame,
    const hir::RandModeGetExpr& call, mir::TypeId result_type)
    -> diag::Result<mir::Expr>;
[[nodiscard]] auto LowerConstraintModeSetExpr(
    ProcessLowerer& lowerer, const WalkFrame& frame,
    const hir::ConstraintModeSetExpr& call) -> diag::Result<mir::Expr>;
[[nodiscard]] auto LowerConstraintModeGetExpr(
    ProcessLowerer& lowerer, const WalkFrame& frame,
    const hir::ConstraintModeGetExpr& call, mir::TypeId result_type)
    -> diag::Result<mir::Expr>;

}  // namespace lyra::lowering::hir_to_mir
