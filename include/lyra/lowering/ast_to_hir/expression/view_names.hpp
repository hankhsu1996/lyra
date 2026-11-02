#pragma once

// Lowering of a name a modport defines by an expression rather than by naming
// one of its interface's items (LRM 25.5.4) -- `.low(data[3:0])` or
// `.plus(data + 1)`. The front end holds a use of one as it holds a use of any
// other name, so the use is stated as such, and what the name stands for is
// read out of the view the interface published where the use is translated.

#include <string_view>

#include "lyra/diag/diagnostic.hpp"
#include "lyra/hir/expr.hpp"
#include "lyra/hir/published_member.hpp"
#include "lyra/hir/scope_class.hpp"
#include "lyra/hir/type_id.hpp"
#include "lyra/lowering/ast_to_hir/unit_lowerer.hpp"
#include "lyra/lowering/ast_to_hir/walk_frame.hpp"

namespace slang::ast {
class HierarchicalValueExpression;
}  // namespace slang::ast

namespace lyra::lowering::ast_to_hir {

// Where the interface class `scope_class` records published, in its view
// `modport`, the name `name`.
auto PublishedViewNameOf(
    const UnitLowerer& unit_lowerer, hir::ScopeClassId scope_class,
    std::string_view modport, std::string_view name)
    -> hir::PublishedViewNameId;

// The type a use of the name `name` has: the expression's self-determined one
// where it is storage (LRM 25.5.4), and otherwise what the subroutine computing
// it answers.
auto PublishedViewNameType(
    const UnitLowerer& unit_lowerer, hir::ScopeClassId scope_class,
    hir::PublishedViewNameId name) -> hir::TypeId;

// A name a view defines, reached by a hierarchical path: through the interface
// port that selected the view, or down to an instance the design declares.
auto LowerRoutedViewName(
    UnitLowerer& unit_lowerer, WalkFrame frame,
    const slang::ast::HierarchicalValueExpression& hve, diag::SourceSpan span)
    -> diag::Result<hir::Expr>;

}  // namespace lyra::lowering::ast_to_hir
