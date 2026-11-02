#pragma once

// Lowering of a use of a name a modport defines (LRM 25.5.4). HIR states the
// use as the front end holds it, and what the name stands for is read here, out
// of the view the interface published. A name the view admits a write to is
// storage: one place, or several joined as a concatenation of places is (LRM
// 11.4.12). A name it offers only for reading is a value the interface
// computes, which is a call on the instance.

#include <optional>
#include <variant>
#include <vector>

#include "lyra/diag/diagnostic.hpp"
#include "lyra/hir/expr.hpp"
#include "lyra/hir/interface_member_access.hpp"
#include "lyra/hir/published_member.hpp"
#include "lyra/hir/scope_class.hpp"
#include "lyra/hir/structural_scope.hpp"
#include "lyra/hir/type_id.hpp"
#include "lyra/lowering/hir_to_mir/access_path.hpp"
#include "lyra/lowering/hir_to_mir/expression/expr_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/walk_frame.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/type_id.hpp"

namespace lyra::lowering::hir_to_mir {

// A use of a name a view defines: which name, and how the instance is reached,
// over routes a port or a hierarchical name sealed -- one per part of a name
// that is storage, one to the object of a name that is a value -- or through
// the virtual interface the use evaluates (LRM 25.9).
struct ViewNameUse {
  std::variant<
      const hir::ViewPlaceRoute*, const hir::ViewValueRoute*,
      const hir::InterfaceInstanceAccessExpr*>
      reached;
  hir::ScopeClassId scope_class;
  hir::PublishedViewNameId name;
};

// The use of a name a view defines that `expr` is, or nothing where it is
// another expression.
template <ExprLowerer L>
[[nodiscard]] auto ViewNameUseOf(const L& lowerer, const hir::Expr& expr)
    -> std::optional<ViewNameUse>;

template <ExprLowerer L>
[[nodiscard]] auto ViewNameUseOf(
    const L& lowerer, const hir::RoutedViewPlaceRef& reference) -> ViewNameUse;

template <ExprLowerer L>
[[nodiscard]] auto ViewNameUseOf(
    const L& lowerer, const hir::RoutedViewValueRef& reference) -> ViewNameUse;

// Whether `use` names one place, where the name stands for one part of its own
// type. Any other name is several places joined, or a value.
[[nodiscard]] auto IsOneViewPlace(
    const UnitLowerer& unit_lowerer, const ViewNameUse& use) -> bool;

// Whether `use` names several places joined.
[[nodiscard]] auto IsViewJoin(
    const UnitLowerer& unit_lowerer, const ViewNameUse& use) -> bool;

// The value `use` stands for. The handle is evaluated once, whatever the name
// goes on to do with the instance.
template <ExprLowerer L>
auto LowerViewNameValue(
    L& lowerer, WalkFrame frame, const ViewNameUse& use,
    mir::TypeId result_type) -> diag::Result<mir::Expr>;

// A part of what `use` stands for, and the type the part has.
struct ViewPart {
  AccessPath place;
  hir::TypeId type;
};

// Each place `use` joins, the first most significant, with the handle evaluated
// here, once.
template <ExprLowerer L>
auto LowerViewNameParts(L& lowerer, WalkFrame frame, const ViewNameUse& use)
    -> diag::Result<std::vector<ViewPart>>;

// The one place `use` names. A join is several, and the front end lets a select
// of one be written, which is not yet carried out; a value is none.
template <ExprLowerer L>
auto LowerViewNamePlace(L& lowerer, WalkFrame frame, const ViewNameUse& use)
    -> diag::Result<AccessPath>;

}  // namespace lyra::lowering::hir_to_mir
