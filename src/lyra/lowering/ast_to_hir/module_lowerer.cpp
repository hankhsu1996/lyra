#include "lyra/lowering/ast_to_hir/module_lowerer.hpp"

#include <cstdint>
#include <expected>
#include <map>
#include <optional>
#include <string>
#include <unordered_map>
#include <utility>
#include <vector>

#include <slang/ast/Expression.h>
#include <slang/ast/Symbol.h>
#include <slang/ast/symbols/InstanceSymbols.h>
#include <slang/ast/symbols/SubroutineSymbols.h>
#include <slang/ast/symbols/ValueSymbol.h>
#include <slang/ast/symbols/VariableSymbols.h>
#include <slang/ast/types/Type.h>

#include "lyra/base/internal_error.hpp"
#include "lyra/diag/diagnostic.hpp"
#include "lyra/diag/source_span.hpp"
#include "lyra/hir/expr.hpp"
#include "lyra/hir/module_unit.hpp"
#include "lyra/hir/value_ref.hpp"
#include "lyra/lowering/ast_to_hir/sensitivity.hpp"
#include "lyra/lowering/ast_to_hir/specialization_name.hpp"
#include "lyra/lowering/ast_to_hir/structural_scope_lowerer.hpp"
#include "lyra/lowering/ast_to_hir/walk_frame.hpp"

namespace lyra::lowering::ast_to_hir {

ModuleLowerer::ModuleLowerer(
    const LoweringFacts& facts, const slang::ast::InstanceBodySymbol& body)
    : facts_(facts), body_(&body), unit_{SpecializationName(body)} {
}

auto ModuleLowerer::Run() -> diag::Result<hir::ModuleUnit> {
  WalkFrame frame;
  StructuralScopeLowerer root(*this, *body_);
  auto root_scope_or = root.Run(frame);
  if (!root_scope_or) {
    return std::unexpected(std::move(root_scope_or.error()));
  }
  unit_.root_scope = *std::move(root_scope_or);
  return std::move(unit_);
}

auto ModuleLowerer::NextScopeFrameId() -> ScopeFrameId {
  return ScopeFrameId{.value = next_scope_frame_++};
}

auto ModuleLowerer::NextWithClauseId() -> hir::WithClauseId {
  return hir::WithClauseId{.value = next_with_clause_++};
}

void ModuleLowerer::MapStructuralDataObjectBinding(
    const slang::ast::ValueSymbol& var, ScopeFrameId home_frame,
    hir::StructuralDataObjectId local, hir::TypeId type) {
  const auto [_, inserted] = structural_data_object_bindings_.emplace(
      &var, StructuralDataObjectBinding{
                .home_frame = home_frame, .var_id = local, .type = type});
  if (!inserted) {
    throw InternalError(
        "ModuleLowerer::MapStructuralDataObjectBinding: structural data object "
        "already mapped");
  }
}

auto ModuleLowerer::LookupStructuralDataObjectBinding(
    const slang::ast::ValueSymbol& var) const
    -> std::optional<StructuralDataObjectBinding> {
  const auto it = structural_data_object_bindings_.find(&var);
  if (it == structural_data_object_bindings_.end()) {
    return std::nullopt;
  }
  return it->second;
}

void ModuleLowerer::MapSubroutineBinding(
    const slang::ast::SubroutineSymbol& sym, ScopeFrameId owner_frame,
    hir::StructuralSubroutineId local) {
  const auto [_, inserted] = subroutine_bindings_.emplace(
      &sym,
      SubroutineBinding{.owner_frame = owner_frame, .subroutine_id = local});
  if (!inserted) {
    throw InternalError(
        "ModuleLowerer::MapSubroutineBinding: subroutine symbol already "
        "mapped");
  }
}

auto ModuleLowerer::LookupSubroutineBinding(
    const slang::ast::SubroutineSymbol& sym) const
    -> std::optional<SubroutineBinding> {
  const auto it = subroutine_bindings_.find(&sym);
  if (it == subroutine_bindings_.end()) {
    return std::nullopt;
  }
  return it->second;
}

void ModuleLowerer::MapLoopVarBinding(
    const slang::ast::ValueSymbol& sym, ScopeFrameId home_frame,
    hir::LoopVarDeclId id, hir::TypeId type) {
  const auto [_, inserted] = loop_var_bindings_.emplace(
      &sym, LoopVarBinding{
                .home_frame = home_frame, .loop_var_id = id, .type = type});
  if (!inserted) {
    throw InternalError(
        "ModuleLowerer::MapLoopVarBinding: loop variable symbol already "
        "mapped");
  }
}

auto ModuleLowerer::LookupLoopVarBinding(
    const slang::ast::ValueSymbol& sym) const -> std::optional<LoopVarBinding> {
  const auto it = loop_var_bindings_.find(&sym);
  if (it == loop_var_bindings_.end()) {
    return std::nullopt;
  }
  return it->second;
}

void ModuleLowerer::MapOwnedChildBinding(
    const slang::ast::Symbol& child, ScopeFrameId home_frame,
    hir::DownwardHead head) {
  const auto [_, inserted] = owned_child_bindings_.emplace(
      &child,
      OwnedChildBinding{.home_frame = home_frame, .head = std::move(head)});
  if (!inserted) {
    throw InternalError(
        "ModuleLowerer::MapOwnedChildBinding: owned child already mapped");
  }
}

auto ModuleLowerer::LookupOwnedChildBinding(
    const slang::ast::Symbol& child) const -> std::optional<OwnedChildBinding> {
  const auto it = owned_child_bindings_.find(&child);
  if (it == owned_child_bindings_.end()) {
    return std::nullopt;
  }
  return it->second;
}

auto ModuleLowerer::MapOrGetCrossUnitRef(
    const slang::ast::ValueSymbol& target, ScopeFrameId slot_owner_frame,
    hir::CrossUnitRefHead head, std::vector<hir::PathStep> path,
    hir::TypeId type) -> hir::CrossUnitRefId {
  auto& frame_dedup = cross_unit_ref_dedup_[slot_owner_frame];
  if (const auto it = frame_dedup.find(&target); it != frame_dedup.end()) {
    return it->second;
  }
  auto& slots = cross_unit_refs_by_frame_[slot_owner_frame];
  const hir::CrossUnitRefId id{static_cast<std::uint32_t>(slots.size())};
  slots.push_back(
      hir::CrossUnitRefDecl{
          .head = std::move(head), .path = std::move(path), .type = type});
  frame_dedup.emplace(&target, id);
  return id;
}

auto ModuleLowerer::LookupCrossUnitRef(
    ScopeFrameId frame, const slang::ast::ValueSymbol& target) const
    -> std::optional<hir::CrossUnitRefId> {
  const auto fit = cross_unit_ref_dedup_.find(frame);
  if (fit == cross_unit_ref_dedup_.end()) {
    return std::nullopt;
  }
  const auto it = fit->second.find(&target);
  if (it == fit->second.end()) {
    return std::nullopt;
  }
  return it->second;
}

auto ModuleLowerer::TakeCrossUnitRefsForFrame(ScopeFrameId slot_owner_frame)
    -> std::vector<hir::CrossUnitRefDecl> {
  const auto it = cross_unit_refs_by_frame_.find(slot_owner_frame);
  if (it == cross_unit_refs_by_frame_.end()) {
    return {};
  }
  auto out = std::move(it->second);
  cross_unit_refs_by_frame_.erase(it);
  return out;
}

auto ModuleLowerer::MakeCrossUnitMemberRef(
    const slang::ast::ValueSymbol& target, ScopeFrameId slot_owner_frame,
    hir::CrossUnitRefHead head, std::vector<hir::PathStep> path,
    hir::TypeId type, diag::SourceSpan span) -> hir::Expr {
  const hir::CrossUnitRefId slot = MapOrGetCrossUnitRef(
      target, slot_owner_frame, std::move(head), std::move(path), type);
  return hir::Expr{
      .type = type,
      .data = hir::PrimaryExpr{.data = hir::CrossUnitVarRef{.id = slot}},
      .span = span,
  };
}

auto ModuleLowerer::TranslateSensitivityReads(
    const std::vector<SensitivityRead>& reads, const WalkFrame& frame) const
    -> std::vector<hir::SensitivityEntry> {
  std::vector<hir::SensitivityEntry> out;
  out.reserve(reads.size());
  for (const auto& read : reads) {
    const auto* var = read.symbol->as_if<slang::ast::VariableSymbol>();
    if (var == nullptr) continue;
    // A footprint is meaningful only for a signal the runtime bit-addresses: a
    // packed bit vector, which renders to one observable cell whose change set
    // is read per bit. For an enum, unpacked aggregate, string, or real the
    // runtime observes the whole signal on any change, so the read carries no
    // footprint regardless of the flat-bit view the DFA computed over its own
    // encoding.
    const auto& read_type = var->getType();
    const std::optional<std::pair<std::uint64_t, std::uint64_t>> footprint =
        read_type.isIntegral() && !read_type.isEnum() ? read.footprint
                                                      : std::nullopt;
    if (const auto binding = LookupStructuralDataObjectBinding(*var)) {
      const auto hops = frame.HopsTo(binding->home_frame);
      if (!hops.has_value()) continue;
      out.push_back(
          hir::SensitivityEntry{
              .ref =
                  hir::StructuralDataObjectRef{
                      .hops = *hops, .var = binding->var_id},
              .footprint = footprint});
      continue;
    }
    // A read of a cross-unit member resolves to the slot that body lowering
    // created for it; subscribing through the slot is what makes always_comb
    // re-trigger on a child's signal.
    if (const auto slot = LookupCrossUnitRef(frame.Current(), *var)) {
      out.push_back(
          hir::SensitivityEntry{
              .ref = hir::CrossUnitVarRef{.id = *slot},
              .footprint = footprint});
    }
  }
  return out;
}

}  // namespace lyra::lowering::ast_to_hir
