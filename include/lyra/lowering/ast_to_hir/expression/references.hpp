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
class Symbol;
class ValueSymbol;
}  // namespace slang::ast

namespace lyra::lowering::ast_to_hir {

// The compilation unit a value is declared directly in when that unit is
// reached by name across the boundary -- a package (LRM 26.2) or the anonymous
// `$unit` scope (LRM 3.12.1) -- or nullptr when the value belongs to this unit
// and routes locally. Such a value is one program-global cell reached by name;
// both the value-reference path and the sensitivity path detect it here so a
// read and a wait on its change agree on the by-name form. The returned symbol
// is the declaring unit, from which the caller computes its published name.
auto DeclaringUnitOfValue(const slang::ast::ValueSymbol& value)
    -> const slang::ast::Symbol*;

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
