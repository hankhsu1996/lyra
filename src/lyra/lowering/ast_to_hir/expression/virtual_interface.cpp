#include "lyra/lowering/ast_to_hir/expression/virtual_interface.hpp"

#include <expected>
#include <optional>
#include <string>
#include <utility>
#include <variant>

#include <slang/ast/Scope.h>
#include <slang/ast/Symbol.h>
#include <slang/ast/symbols/InstanceSymbols.h>
#include <slang/ast/symbols/MemberSymbols.h>
#include <slang/ast/types/AllTypes.h>

#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/diag/diag_code.hpp"
#include "lyra/hir/expr_builders.hpp"
#include "lyra/hir/published_modport.hpp"
#include "lyra/lowering/ast_to_hir/expression/view_names.hpp"
#include "lyra/lowering/ast_to_hir/generate_construct.hpp"
#include "lyra/lowering/ast_to_hir/unit_identity.hpp"
#include "lyra/support/def_path.hpp"

namespace lyra::lowering::ast_to_hir {

namespace {

// An interface the held instance instantiates, reached as a whole.
struct HeldInstance {
  hir::InterfaceInstanceAccessExpr access;
  std::string unit_name;
};

// A variable or net of the held instance, or of one it instantiates.
struct HeldMember {
  hir::InterfaceMemberAccessExpr access;
  hir::TypeId type;
};

// A name the view the handle's type selects defines for itself, on the held
// instance.
struct HeldViewName {
  hir::InterfaceInstanceAccessExpr instance;
  hir::ViewDefinedName meaning;
};

// What a name reaches on the instance a virtual interface holds.
using ReachedThroughHandle =
    std::variant<HeldViewName, HeldInstance, HeldMember>;

auto ReachThroughHandle(
    UnitLowerer& unit_lowerer, const WalkFrame& frame, hir::ExprId handle,
    const slang::ast::VirtualInterfaceType& handle_type,
    const slang::ast::Symbol& member, diag::SourceSpan span)
    -> diag::Result<ReachedThroughHandle> {
  // A port identifier a modport names without an expression stands for the
  // interface item of the same name, which is what is published; one it
  // defines by an expression is the view's own name (LRM 25.5.4), reached on
  // the instance the handle holds the way a port reaches it on the one it is
  // bound to.
  const slang::ast::Symbol* item = &member;
  if (const auto* port = member.as_if<slang::ast::ModportPortSymbol>()) {
    if (port->internalSymbol == nullptr) {
      auto held = DescendThroughHandle(
          unit_lowerer, frame, handle, handle_type, handle_type.iface.body,
          span);
      if (!held) return std::unexpected(std::move(held.error()));
      return HeldViewName{
          .instance = std::move(held->instance),
          .meaning = PublishedViewNameMeaning(
              unit_lowerer, held->place.scope_class,
              port->getParentScope()->asSymbol().name, port->name)};
    }
    item = port->internalSymbol;
  }

  // A name that is itself an interface the instance instantiates stands for
  // that instance, and the descent to it ends by stepping onto it.
  const auto* instance = item->as_if<slang::ast::InstanceSymbol>();
  auto descent = DescendThroughHandle(
      unit_lowerer, frame, handle, handle_type,
      instance != nullptr ? instance->body : *item->getParentScope(), span);
  if (!descent) return std::unexpected(std::move(descent.error()));
  if (instance != nullptr) {
    return HeldInstance{
        .access = std::move(descent->instance),
        .unit_name = unit_lowerer.Specialization().NameOf(*instance)};
  }

  const hir::ScopeClassId scope_class = descent->place.scope_class;
  const hir::ScopeClassSignature& scope =
      unit_lowerer.Unit().scope_classes.Get(scope_class);
  const auto published = scope.FindMember(
      item->name, support::Extended(scope.class_path, descent->place.within));
  if (!published.has_value()) {
    // LRM 25.9.
    return diag::Fail(
        span, diag::DiagCode::kUnsupportedExpressionForm,
        "reaching anything but a variable or a net of an interface through a "
        "virtual interface is not yet supported");
  }
  return HeldMember{
      .access =
          hir::InterfaceMemberAccessExpr{
              .instance = std::move(descent->instance),
              .scope_class = scope_class,
              .member = *published},
      .type = scope.members.Get(*published).type};
}

}  // namespace

auto DescendThroughHandle(
    UnitLowerer& unit_lowerer, const WalkFrame& frame, hir::ExprId handle,
    const slang::ast::VirtualInterfaceType& handle_type,
    const slang::ast::Scope& scope, diag::SourceSpan span)
    -> diag::Result<HeldDescent> {
  // Every scope between the instance the handle holds and `scope` -- an
  // interface it instantiates, a generate block, a named block or subroutine --
  // is one that instance's interface published (LRM 23.6), stepped through the
  // way a hierarchical name steps through the scope classes it reaches.
  const hir::ScopeClassId held =
      unit_lowerer.ScopeClassOfInstance(handle_type.iface);
  // The front end resolves a name reached through a virtual interface to the
  // member it lands on and keeps no select it was written with, so a loop's
  // block on the way is selected by the value its index stood at.
  const LoopSelectOf select_of =
      [&](const slang::ast::GenerateBlockSymbol& block)
      -> std::optional<hir::ExprId> {
    return frame.Exprs().Add(
        hir::MakeIntLiteral(
            LoopIndexOf(block), unit_lowerer.Unit().builtins.int_type, span));
  };
  std::optional<PublishedDescent> descended = unit_lowerer.DescendPublished(
      held, handle_type.iface.body, scope, select_of);
  if (!descended.has_value()) {
    // LRM 25.9.
    return diag::Fail(
        span, diag::DiagCode::kUnsupportedExpressionForm,
        "reaching this name through a virtual interface is not yet "
        "supported");
  }
  // A descent to a scope selects one object at every step, since a scope is
  // inside one of them.
  auto* place = std::get_if<InPublishedScope>(&descended->place);
  if (place == nullptr) {
    throw InternalError(
        "DescendThroughHandle: a descent to a scope stands on one object");
  }
  return HeldDescent{
      .instance =
          {.handle = handle,
           .scope_class = held,
           .steps = std::move(descended->steps)},
      .place = std::move(*place)};
}

auto LowerVirtualInterfaceMember(
    UnitLowerer& unit_lowerer, WalkFrame frame, hir::ExprId handle,
    const slang::ast::VirtualInterfaceType& handle_type,
    const slang::ast::Symbol& member, diag::SourceSpan span)
    -> diag::Result<hir::Expr> {
  auto reached = ReachThroughHandle(
      unit_lowerer, frame, handle, handle_type, member, span);
  if (!reached) return std::unexpected(std::move(reached.error()));
  return std::visit(
      Overloaded{
          [&](HeldViewName& view) {
            const hir::ScopeClassId scope_class = view.instance.scope_class;
            return LowerViewNameOnInstance(
                unit_lowerer, frame,
                ViewNameOnInstance{
                    .instance = std::move(view.instance),
                    .scope_class = scope_class,
                    .meaning = std::move(view.meaning)},
                span);
          },
          [&](HeldInstance& held) {
            return hir::Expr{
                .type = unit_lowerer.Unit().types.Intern(
                    hir::Type{hir::VirtualInterfaceType{
                        .unit_name = std::move(held.unit_name)}}),
                .data = std::move(held.access),
                .span = span};
          },
          [&](HeldMember& held) {
            return hir::Expr{
                .type = held.type,
                .data = std::move(held.access),
                .span = span};
          }},
      *reached);
}

}  // namespace lyra::lowering::ast_to_hir
