#include "lyra/lowering/ast_to_hir/expression/virtual_interface.hpp"

#include <algorithm>
#include <expected>
#include <string>
#include <utility>
#include <variant>
#include <vector>

#include <slang/ast/Scope.h>
#include <slang/ast/Symbol.h>
#include <slang/ast/symbols/InstanceSymbols.h>
#include <slang/ast/symbols/MemberSymbols.h>
#include <slang/ast/types/AllTypes.h>

#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/diag/diag_code.hpp"
#include "lyra/hir/published_modport.hpp"
#include "lyra/lowering/ast_to_hir/expression/view_names.hpp"
#include "lyra/lowering/ast_to_hir/instance_array_shape.hpp"
#include "lyra/lowering/ast_to_hir/unit_identity.hpp"

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
    UnitLowerer& unit_lowerer, hir::ExprId handle,
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
          unit_lowerer, handle, handle_type, handle_type.iface.body, span);
      if (!held) return std::unexpected(std::move(held.error()));
      return HeldViewName{
          .instance = std::move(held->instance),
          .meaning = PublishedViewNameMeaning(
              unit_lowerer, held->landed,
              port->getParentScope()->asSymbol().name, port->name)};
    }
    item = port->internalSymbol;
  }

  // A name that is itself an interface the instance instantiates stands for
  // that instance, and the descent to it ends by stepping onto it.
  const auto* instance = item->as_if<slang::ast::InstanceSymbol>();
  auto descent = DescendThroughHandle(
      unit_lowerer, handle, handle_type,
      instance != nullptr ? instance->body : *item->getParentScope(), span);
  if (!descent) return std::unexpected(std::move(descent.error()));
  if (instance != nullptr) {
    return HeldInstance{
        .access = std::move(descent->instance),
        .unit_name =
            SpecializationName(*instance, unit_lowerer.Specialization())};
  }

  const hir::ExternalUnitObject& promised =
      unit_lowerer.Unit().external_unit_objects.Get(descent->landed);
  const auto published = promised.FindMember(item->name);
  if (!published.has_value()) {
    return diag::Fail(
        span, diag::DiagCode::kUnsupportedExpressionForm,
        "only a variable or a net of an interface is reachable through a "
        "virtual interface yet (LRM 25.9)");
  }
  return HeldMember{
      .access =
          hir::InterfaceMemberAccessExpr{
              .instance = std::move(descent->instance), .member = *published},
      .type = promised.members.Get(*published).type};
}

}  // namespace

auto InterfaceObjectOf(
    UnitLowerer& unit_lowerer, const slang::ast::InstanceSymbol& instance)
    -> hir::ExternalUnitObjectId {
  const std::string unit_name =
      SpecializationName(instance, unit_lowerer.Specialization());
  if (unit_lowerer.Signatures().Find(unit_name) == nullptr) {
    throw InternalError(
        "InterfaceObjectOf: every interface a declared type names or an "
        "instance declares is collected as a unit, so its promise exists");
  }
  return unit_lowerer.ExternalUnitObjectOf(unit_name);
}

auto DescendThroughHandle(
    UnitLowerer& unit_lowerer, hir::ExprId handle,
    const slang::ast::VirtualInterfaceType& handle_type,
    const slang::ast::Scope& scope, diag::SourceSpan span)
    -> diag::Result<HeldDescent> {
  // The interfaces between the instance the handle holds and `scope`,
  // outermost first. Each is an instance its enclosing interface declares, and
  // so a member that interface published.
  std::vector<const slang::ast::InstanceSymbol*> nested;
  for (const slang::ast::Scope* at = &scope; at != &handle_type.iface.body;) {
    const auto* body =
        at == nullptr ? nullptr
                      : at->asSymbol().as_if<slang::ast::InstanceBodySymbol>();
    if (body == nullptr || body->parentInstance == nullptr) {
      return diag::Fail(
          span, diag::DiagCode::kUnsupportedExpressionForm,
          "a name declared inside a generate block of an interface is not yet "
          "reachable through a virtual interface (LRM 25.9)");
    }
    nested.push_back(body->parentInstance);
    at = OwnerOfInstance(*body->parentInstance).getParentScope();
  }
  std::ranges::reverse(nested);

  const hir::ExternalUnitObjectId held =
      InterfaceObjectOf(unit_lowerer, handle_type.iface);
  HeldDescent descent{
      .instance = {.handle = handle, .object = held, .steps = {}},
      .landed = held};
  descent.instance.steps.reserve(nested.size());
  for (const slang::ast::InstanceSymbol* instance : nested) {
    const auto member_id = unit_lowerer.Unit()
                               .external_unit_objects.Get(descent.landed)
                               .FindMember(OwnerOfInstance(*instance).name);
    if (!member_id.has_value()) {
      throw InternalError(
          "DescendThroughHandle: an interface promises every instance it "
          "declares");
    }
    descent.instance.steps.push_back(
        hir::SignatureMemberStep{
            .object = descent.landed,
            .member = *member_id,
            .indices = {
                instance->arrayPath.begin(), instance->arrayPath.end()}});
    descent.landed = InterfaceObjectOf(unit_lowerer, *instance);
  }
  return descent;
}

auto LowerVirtualInterfaceMember(
    UnitLowerer& unit_lowerer, WalkFrame frame, hir::ExprId handle,
    const slang::ast::VirtualInterfaceType& handle_type,
    const slang::ast::Symbol& member, diag::SourceSpan span)
    -> diag::Result<hir::Expr> {
  auto reached =
      ReachThroughHandle(unit_lowerer, handle, handle_type, member, span);
  if (!reached) return std::unexpected(std::move(reached.error()));
  return std::visit(
      Overloaded{
          [&](HeldViewName& view) {
            const hir::ExternalUnitObjectId object = view.instance.object;
            return LowerViewNameOnInstance(
                unit_lowerer, frame,
                ViewNameOnInstance{
                    .instance = std::move(view.instance),
                    .object = object,
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

auto WatchedThroughHandle(
    UnitLowerer& unit_lowerer, hir::ExprId handle,
    const slang::ast::VirtualInterfaceType& handle_type,
    const slang::ast::Symbol& member, diag::SourceSpan span)
    -> diag::Result<std::vector<hir::InterfaceMemberAccessExpr>> {
  auto reached =
      ReachThroughHandle(unit_lowerer, handle, handle_type, member, span);
  if (!reached) return std::unexpected(std::move(reached.error()));
  return std::visit(
      Overloaded{
          [](const HeldViewName& view)
              -> diag::Result<std::vector<hir::InterfaceMemberAccessExpr>> {
            std::vector<hir::InterfaceMemberAccessExpr> watched;
            for (const hir::PublishedMemberId id :
                 hir::WatchedMembers(view.meaning)) {
              watched.push_back(
                  hir::InterfaceMemberAccessExpr{
                      .instance = view.instance, .member = id});
            }
            return watched;
          },
          [](const HeldInstance&)
              -> diag::Result<std::vector<hir::InterfaceMemberAccessExpr>> {
            throw InternalError(
                "WatchedThroughHandle: a wait reading which instance a virtual "
                "interface holds is refused before its leaves are asked for");
          },
          [](HeldMember& held)
              -> diag::Result<std::vector<hir::InterfaceMemberAccessExpr>> {
            return std::vector{std::move(held.access)};
          }},
      *reached);
}

}  // namespace lyra::lowering::ast_to_hir
