#pragma once

// Lowering of a name a modport defines by an expression rather than by naming
// one of its interface's items (LRM 25.5.4) -- `.low(data[3:0])` or
// `.plus(data + 1)`. The identifier belongs to the view rather than to the
// interface's declarations, so what it means is read out of the view the
// interface published, and it is reached on whichever instance the name was
// reached through: one a route reaches, or one a virtual interface holds.

#include <string_view>
#include <variant>

#include "lyra/diag/diagnostic.hpp"
#include "lyra/hir/expr.hpp"
#include "lyra/hir/external_scope_class.hpp"
#include "lyra/hir/interface_member_access.hpp"
#include "lyra/hir/published_modport.hpp"
#include "lyra/lowering/ast_to_hir/unit_lowerer.hpp"
#include "lyra/lowering/ast_to_hir/walk_frame.hpp"

namespace slang::ast {
class HierarchicalValueExpression;
}  // namespace slang::ast

namespace lyra::lowering::ast_to_hir {

// A name a view defines, on one instance of the interface that publishes it.
// How the instance is reached is kept beside the meaning because the two
// meanings need different things from it: a place is reached member by member,
// while a computed value is a call made on the instance itself. The instance
// is either one a route reaches, sealed where the design elaborates, or the one
// a virtual interface holds when the name is evaluated (LRM 25.9). The meaning
// is copied rather than pointed at, since reaching the object may grow the
// arena it lives in.
struct ViewNameOnInstance {
  std::variant<ScopeRoute, hir::InterfaceInstanceAccessExpr> instance;
  hir::ExternalScopeClassId scope_class;
  hir::ViewDefinedName meaning;
};

// What the interface class `scope_class` records published, in its view
// `modport`, as the meaning of the name `name`.
auto PublishedViewNameMeaning(
    const UnitLowerer& unit_lowerer, hir::ExternalScopeClassId scope_class,
    std::string_view modport, std::string_view name) -> hir::ViewDefinedName;

// LRM 25.5.4: a name a view defines is either the storage its expression
// designates or, where the view offers it only for reading, a value the
// interface computes. The first is reached as a place, which is what makes
// every form of write to it an ordinary write; the second is a call on the
// instance, because the expression names declarations this unit never sees.
auto LowerViewNameOnInstance(
    UnitLowerer& unit_lowerer, WalkFrame frame,
    const ViewNameOnInstance& view_name, diag::SourceSpan span) -> hir::Expr;

// A name a view defines, reached by a hierarchical path: through the interface
// port that selected the view, or down to an instance the design declares.
auto LowerRoutedViewName(
    UnitLowerer& unit_lowerer, WalkFrame frame,
    const slang::ast::HierarchicalValueExpression& hve, diag::SourceSpan span)
    -> diag::Result<hir::Expr>;

}  // namespace lyra::lowering::ast_to_hir
