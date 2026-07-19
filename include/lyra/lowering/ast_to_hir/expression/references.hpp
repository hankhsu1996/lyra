#pragma once

// Lowering of name resolution expressions: NamedValue (LRM 6.6 names) and
// HierarchicalValue (LRM 23.6 hierarchical references). Routed reference
// construction lives on UnitLowerer; this file handles the expression-level
// resolution into DirectMemberRef / ProceduralVarRef / RoutedRef per context.

#include "lyra/diag/diagnostic.hpp"
#include "lyra/hir/expr.hpp"
#include "lyra/lowering/ast_to_hir/process_lowerer.hpp"
#include "lyra/lowering/ast_to_hir/unit_lowerer.hpp"
#include "lyra/lowering/ast_to_hir/walk_frame.hpp"

namespace slang::ast {
class HierarchicalValueExpression;
class NamedValueExpression;
class PackageSymbol;
class ValueSymbol;
}  // namespace slang::ast

namespace lyra::lowering::ast_to_hir {

// A variable declared directly in a package scope (LRM 26.2), or nullptr
// otherwise. A package variable is reached by name across the unit boundary,
// the way a package subroutine call is; both the value-reference path and the
// sensitivity path detect it here so a package variable read and a wait on its
// change agree on the by-name form.
auto EnclosingPackageOfValue(const slang::ast::ValueSymbol& value)
    -> const slang::ast::PackageSymbol*;

auto LowerNamedValueProc(
    ProcessLowerer& proc, WalkFrame frame,
    const slang::ast::NamedValueExpression& named) -> diag::Result<hir::Expr>;

// A hierarchical reference has the same shape across procedural and
// structural contexts (it always resolves into a cross-unit member binding on
// the referrer's structural scope), so this entry is generic over the calling
// context rather than split into Proc / Structural variants.
auto LowerHierarchicalValue(
    UnitLowerer& unit_lowerer, WalkFrame frame,
    const slang::ast::HierarchicalValueExpression& hve)
    -> diag::Result<hir::Expr>;

auto LowerNamedValueStructural(
    UnitLowerer& unit_lowerer, WalkFrame frame,
    const slang::ast::NamedValueExpression& named) -> diag::Result<hir::Expr>;

}  // namespace lyra::lowering::ast_to_hir
