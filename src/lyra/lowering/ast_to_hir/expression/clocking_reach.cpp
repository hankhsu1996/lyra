#include "lyra/lowering/ast_to_hir/expression/clocking_reach.hpp"

#include <optional>
#include <string>
#include <utility>
#include <variant>

#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/diag/diag_code.hpp"
#include "lyra/hir/interface_member_access.hpp"
#include "lyra/lowering/ast_to_hir/clocking_block.hpp"
#include "lyra/lowering/ast_to_hir/expression/virtual_interface.hpp"
#include "lyra/lowering/ast_to_hir/process_lowerer.hpp"
#include "lyra/lowering/ast_to_hir/structural_scope_lowerer.hpp"
#include "lyra/lowering/ast_to_hir/subroutine_decl.hpp"
#include "lyra/support/def_path.hpp"
#include "slang/ast/HierarchicalReference.h"
#include "slang/ast/expressions/MiscExpressions.h"
#include "slang/ast/expressions/SelectExpressions.h"
#include "slang/ast/symbols/MemberSymbols.h"
#include "slang/ast/symbols/VariableSymbols.h"
#include "slang/ast/types/AllTypes.h"

namespace lyra::lowering::ast_to_hir {

namespace {

auto BlockHolding(const slang::ast::Symbol& symbol)
    -> const slang::ast::ClockingBlockSymbol* {
  if (const auto* block = symbol.as_if<slang::ast::ClockingBlockSymbol>()) {
    return block;
  }
  if (symbol.kind != slang::ast::SymbolKind::ClockVar) return nullptr;
  return &symbol.getParentScope()
              ->asSymbol()
              .as<slang::ast::ClockingBlockSymbol>();
}

// What `part` is published as, and whether the block holds it or the scope
// declaring the block does.
auto PartNaming(const ClockingPart& part) -> DrivingVariableNaming {
  return std::visit(
      Overloaded{
          [](const ClockingBlockEvent&) {
            return DrivingVariableNaming{
                .name = ClockingBlockEventName(), .held_by_scope = false};
          },
          [](const ClockingDrivingVariable& driving) {
            return DrivingVariableNamingOf(*driving.clockvar);
          }},
      part);
}

// The path `part` is published under, given `block`, the path of its block.
auto HolderPath(const DrivingVariableNaming& naming, support::DefPath block)
    -> support::DefPath {
  return naming.held_by_scope ? support::EnclosingPath(block) : block;
}

auto OwnPart(const DeclaredClocking& declared, const ClockingPart& part)
    -> hir::StructuralDataObjectId {
  return std::visit(
      Overloaded{
          [&](const ClockingBlockEvent&) { return declared.block_event; },
          [&](const ClockingDrivingVariable& driving) {
            const auto found = declared.clockvars.find(driving.clockvar);
            if (found == declared.clockvars.end() ||
                !found->second.driving.has_value()) {
              throw InternalError(
                  "OwnPart: an output holds the variable it drives its signal "
                  "from");
            }
            return *found->second.driving;
          }},
      part);
}

auto NotReached(diag::SourceSpan span) -> std::unexpected<diag::Diagnostic> {
  return diag::Fail(
      span, diag::DiagCode::kUnsupportedExpressionForm,
      "reaching a clocking block through this name is not yet supported");
}

// Whether `origin` reaches `block` where this unit's own scope declares it,
// with no step out of the reader's scope.
auto ReachesOwnBlock(
    const UnitLowerer& unit_lowerer, const WalkFrame& frame,
    const slang::ast::ClockingBlockSymbol& block, const RouteOrigin& origin)
    -> const DeclaredClocking* {
  if (!NamesOwnDeclaration(origin)) return nullptr;
  const DeclaredClocking* declared = unit_lowerer.FindDeclaredClocking(block);
  if (declared == nullptr) return nullptr;
  const std::optional<hir::StructuralHops> hops =
      frame.HopsTo(declared->home_frame);
  if (!hops.has_value() || hops->value != 0) return nullptr;
  return declared;
}

}  // namespace

auto NamesClockingBlock(const slang::ast::Expression& expr) -> bool {
  if (const auto* named = expr.as_if<slang::ast::ArbitrarySymbolExpression>()) {
    return named->symbol->kind == slang::ast::SymbolKind::ClockingBlock;
  }
  if (const auto* access = expr.as_if<slang::ast::MemberAccessExpression>()) {
    return access->member.kind == slang::ast::SymbolKind::ClockingBlock;
  }
  return false;
}

auto ClockingNamingOf(
    UnitLowerer& unit_lowerer, const WalkFrame& frame,
    const slang::ast::Expression& naming) -> std::optional<ClockingNaming> {
  if (const auto* sel = naming.as_if<slang::ast::ElementSelectExpression>()) {
    return ClockingNamingOf(unit_lowerer, frame, sel->value());
  }
  if (const auto* sel = naming.as_if<slang::ast::RangeSelectExpression>()) {
    return ClockingNamingOf(unit_lowerer, frame, sel->value());
  }
  if (const auto* named =
          naming.as_if<slang::ast::ArbitrarySymbolExpression>()) {
    const auto* block = BlockHolding(*named->symbol);
    if (block == nullptr) return std::nullopt;
    return ClockingNaming{
        .block = block,
        .reach = unit_lowerer.StartOf(frame, *named->symbol, named->hierRef)};
  }
  if (const auto* named = naming.as_if<slang::ast::NamedValueExpression>()) {
    const auto* block = BlockHolding(named->symbol);
    if (block == nullptr) return std::nullopt;
    return ClockingNaming{
        .block = block,
        .reach = unit_lowerer.StartOf(
            frame, named->symbol, slang::ast::HierarchicalReference{})};
  }
  if (const auto* named =
          naming.as_if<slang::ast::HierarchicalValueExpression>()) {
    const auto* block = BlockHolding(named->symbol);
    if (block == nullptr) return std::nullopt;
    return ClockingNaming{
        .block = block,
        .reach = unit_lowerer.StartOf(frame, named->symbol, named->ref)};
  }
  if (const auto* access = naming.as_if<slang::ast::MemberAccessExpression>()) {
    const auto* block = BlockHolding(access->member);
    if (block == nullptr || !access->value().type->isVirtualInterface()) {
      return std::nullopt;
    }
    return ClockingNaming{
        .block = block, .reach = ThroughHandle{.handle = &access->value()}};
  }
  return std::nullopt;
}

template <ExprLowerer Lowerer>
auto LowerClockingPart(
    Lowerer& lowerer, WalkFrame frame, const ClockingNaming& naming,
    const ClockingPart& part, hir::TypeId type, diag::SourceSpan span)
    -> diag::Result<hir::Expr> {
  UnitLowerer& unit_lowerer = lowerer.Owner();
  const DrivingVariableNaming published_as = PartNaming(part);
  const auto own_leaf = [&](const DeclaredClocking& declared) {
    return hir::StructuralDataObjectLeaf{
        .object = OwnPart(declared, part),
        .storage = hir::VariableStorage{},
        .type = type};
  };
  const auto through_handle =
      [&](const ThroughHandle& through) -> diag::Result<hir::Expr> {
    auto handle = lowerer.LowerExpr(*through.handle, frame);
    if (!handle) return std::unexpected(std::move(handle.error()));
    const hir::ExprId handle_id = frame.Exprs().Add(*std::move(handle));
    auto descent = DescendThroughHandle(
        unit_lowerer, frame, handle_id,
        through.handle->type->getCanonicalType()
            .as<slang::ast::VirtualInterfaceType>(),
        *naming.block, span);
    if (!descent) return std::unexpected(std::move(descent.error()));
    const hir::ScopeClassSignature& published =
        unit_lowerer.Unit().scope_classes.Get(descent->place.scope_class);
    const auto member = published.FindMember(
        published_as.name,
        HolderPath(
            published_as,
            support::Extended(published.class_path, descent->place.within)));
    if (!member.has_value()) return NotReached(span);
    return hir::Expr{
        .type = type,
        .data =
            hir::InterfaceMemberAccessExpr{
                .instance = std::move(descent->instance),
                .scope_class = descent->place.scope_class,
                .component = *member},
        .span = span};
  };
  const auto by_name =
      [&](const RouteOrigin& origin) -> diag::Result<hir::Expr> {
    if (const DeclaredClocking* own =
            ReachesOwnBlock(unit_lowerer, frame, *naming.block, origin)) {
      return unit_lowerer.MakeRoutedMemberRef(
          frame.Current(),
          hir::ValueRoute{
              .base = hir::InUnitBase{.hops = {}},
              .steps = {},
              .leaf = own_leaf(*own)},
          span);
    }
    auto route = unit_lowerer.RouteToScope(frame, *naming.block, origin);
    if (!route) return std::unexpected(std::move(route.error()));
    if (!route->has_value()) return NotReached(span);
    ScopeRoute& reached = **route;
    auto leaf = std::visit(
        Overloaded{
            [&](const InPublishedScope& on) -> std::optional<hir::DataLeaf> {
              const hir::ScopeClassSignature& published =
                  unit_lowerer.Unit().scope_classes.Get(on.scope_class);
              const auto member = published.FindMember(
                  published_as.name,
                  HolderPath(
                      published_as,
                      support::Extended(published.class_path, on.within)));
              if (!member.has_value()) return std::nullopt;
              return unit_lowerer.PublishedMemberLeafOf(
                  on.scope_class, *member);
            },
            [&](const InOwnScope&) -> std::optional<hir::DataLeaf> {
              const DeclaredClocking* declared =
                  unit_lowerer.FindDeclaredClocking(*naming.block);
              if (declared == nullptr) return std::nullopt;
              return own_leaf(*declared);
            },
            [](const OnSeveralObjects&) -> std::optional<hir::DataLeaf> {
              return std::nullopt;
            }},
        reached.place);
    if (!leaf.has_value()) return NotReached(span);
    return unit_lowerer.MakeRoutedMemberRef(
        frame.Current(),
        hir::ValueRoute{
            .base = std::move(reached.base),
            .steps = std::move(reached.steps),
            .leaf = *std::move(leaf)},
        span);
  };
  return std::visit(Overloaded{through_handle, by_name}, naming.reach);
}

auto LandingOf(
    UnitLowerer& unit_lowerer, const WalkFrame& frame,
    const ClockingNaming& naming, const slang::ast::ClockVarSymbol& clockvar,
    hir::ExprId written, diag::SourceSpan span)
    -> diag::Result<hir::ClockingLanding> {
  const std::string name =
      ClockingLandingName(naming.block->name, clockvar.name);
  const auto published =
      [&](hir::ScopeClassId scope_class,
          hir::LandingReceiver receiver) -> diag::Result<hir::ClockingLanding> {
    const auto callable =
        unit_lowerer.Unit().scope_classes.Get(scope_class).FindCallable(name);
    if (!callable.has_value()) return NotReached(span);
    return hir::PublishedLanding{
        .scope_class = scope_class,
        .callable = *callable,
        .receiver = std::move(receiver)};
  };
  return std::visit(
      Overloaded{
          // The instance is the one the target's own access reaches through
          // the handle, which the landing names, so the handle is held and
          // evaluated once (LRM 14.16).
          [&](const ThroughHandle&) -> diag::Result<hir::ClockingLanding> {
            const auto* access = std::get_if<hir::InterfaceMemberAccessExpr>(
                &frame.Exprs().Get(written).data);
            if (access == nullptr) {
              throw InternalError(
                  "LandingOf: a drive's target reached through a virtual "
                  "interface is a member of the instance it holds");
            }
            return published(
                access->scope_class, hir::InstanceOfTarget{.access = written});
          },
          [&](const RouteOrigin& origin) -> diag::Result<hir::ClockingLanding> {
            if (const DeclaredClocking* own = ReachesOwnBlock(
                    unit_lowerer, frame, *naming.block, origin)) {
              return hir::ClockingOutputLanding{
                  .block = own->id,
                  .clockvar = own->clockvars.at(&clockvar).position};
            }
            auto route = unit_lowerer.RouteToScope(
                frame, *naming.block->getParentScope(), origin);
            if (!route) return std::unexpected(std::move(route.error()));
            if (!route->has_value()) return NotReached(span);
            const auto* on = std::get_if<InPublishedScope>(&(*route)->place);
            if (on == nullptr) {
              return diag::Fail(
                  span, diag::DiagCode::kUnsupportedExpressionForm,
                  "a drive of a clocking block another scope of the same "
                  "design element declares is not yet supported");
            }
            const hir::ScopeClassId scope_class = on->scope_class;
            return published(
                scope_class, unit_lowerer.MakeRoutedObjectRef(
                                 frame.Current(), **std::move(route),
                                 unit_lowerer.ScopeClassTypeOf(scope_class)));
          }},
      naming.reach);
}

template <ExprLowerer Lowerer>
auto LowerClockingBlockEvent(
    Lowerer& lowerer, WalkFrame frame, const ClockingNaming& naming,
    diag::SourceSpan span) -> diag::Result<hir::Expr> {
  return LowerClockingPart(
      lowerer, frame, naming, ClockingBlockEvent{},
      lowerer.Owner().Unit().types.Intern(hir::Type{hir::EventType{}}), span);
}

template auto LowerClockingBlockEvent(
    ProcessLowerer&, WalkFrame, const ClockingNaming&, diag::SourceSpan)
    -> diag::Result<hir::Expr>;
template auto LowerClockingBlockEvent(
    StructuralScopeLowerer&, WalkFrame, const ClockingNaming&, diag::SourceSpan)
    -> diag::Result<hir::Expr>;
template auto LowerClockingPart(
    ProcessLowerer&, WalkFrame, const ClockingNaming&, const ClockingPart&,
    hir::TypeId, diag::SourceSpan) -> diag::Result<hir::Expr>;
template auto LowerClockingPart(
    StructuralScopeLowerer&, WalkFrame, const ClockingNaming&,
    const ClockingPart&, hir::TypeId, diag::SourceSpan)
    -> diag::Result<hir::Expr>;

}  // namespace lyra::lowering::ast_to_hir
