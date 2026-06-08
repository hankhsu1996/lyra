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
#include "lyra/hir/type.hpp"
#include "lyra/hir/value_ref.hpp"
#include "lyra/lowering/ast_to_hir/scope_lowerer.hpp"
#include "lyra/lowering/ast_to_hir/sensitivity.hpp"
#include "lyra/lowering/ast_to_hir/walk_frame.hpp"

namespace lyra::lowering::ast_to_hir {

ModuleLowerer::ModuleLowerer(
    const LoweringFacts& facts, const slang::ast::InstanceBodySymbol& body)
    : facts_(facts), body_(&body) {
  hir_unit_.name = std::string{body.name};
  // Intrinsic types are pre-registered so callers reference them by a stable
  // id rather than re-adding entries on demand. Extend this section when more
  // synthesized types accumulate (1-bit logic, etc.).
  builtins_.void_type = AddType(hir::TypeData{hir::VoidType{}});
  builtins_.int32 = AddType(
      hir::TypeData{hir::PackedArrayType{
          .atom = hir::BitAtom::kBit,
          .signedness = hir::Signedness::kSigned,
          .dims = {hir::PackedRange{.left = 31, .right = 0}},
          .form = hir::PackedArrayForm::kInt}});
  builtins_.integer = AddType(
      hir::TypeData{hir::PackedArrayType{
          .atom = hir::BitAtom::kLogic,
          .signedness = hir::Signedness::kSigned,
          .dims = {hir::PackedRange{.left = 31, .right = 0}},
          .form = hir::PackedArrayForm::kInteger}});
  builtins_.string = AddType(hir::TypeData{hir::StringType{}});
  builtins_.time = AddType(
      hir::TypeData{hir::PackedArrayType{
          .atom = hir::BitAtom::kLogic,
          .signedness = hir::Signedness::kUnsigned,
          .dims = {hir::PackedRange{.left = 63, .right = 0}},
          .form = hir::PackedArrayForm::kTime}});
  builtins_.realtime = AddType(hir::TypeData{hir::RealTimeType{}});
}

auto ModuleLowerer::Run() -> diag::Result<hir::ModuleUnit> {
  ScopeLowerer root(*this, hir_unit_.root_scope, *body_);
  auto r = root.Run(WalkFrame{});
  if (!r) return std::unexpected(std::move(r.error()));
  return std::move(hir_unit_);
}

auto ModuleLowerer::AddType(hir::TypeData data) -> hir::TypeId {
  const hir::TypeId id{static_cast<std::uint32_t>(hir_unit_.types.size())};
  hir_unit_.types.push_back(hir::Type{.data = std::move(data)});
  return id;
}

auto ModuleLowerer::GetType(hir::TypeId id) const -> const hir::Type& {
  return hir_unit_.GetType(id);
}

auto ModuleLowerer::GetTypeId(
    const slang::ast::Type& type, diag::SourceSpan span)
    -> diag::Result<hir::TypeId> {
  const auto* canonical = &type.getCanonicalType();
  const auto it = type_cache_.find(canonical);
  if (it != type_cache_.end()) {
    return it->second;
  }
  auto data_or = LowerType(type, span);
  if (!data_or) return std::unexpected(std::move(data_or.error()));
  const hir::TypeId id = AddType(*std::move(data_or));
  type_cache_.emplace(canonical, id);
  return id;
}

auto ModuleLowerer::GetTypeIdOf(const slang::ast::Expression& e)
    -> diag::Result<hir::TypeId> {
  return GetTypeId(*e.type, SourceMapper().SpanOf(e.sourceRange));
}

auto ModuleLowerer::NextScopeFrameId() -> ScopeFrameId {
  return ScopeFrameId{.value = next_scope_frame_++};
}

void ModuleLowerer::MapStructuralVarBinding(
    const slang::ast::VariableSymbol& var, ScopeFrameId home_frame,
    hir::StructuralVarId local, hir::TypeId type) {
  const auto [_, inserted] = structural_var_bindings_.emplace(
      &var, StructuralVarBinding{
                .home_frame = home_frame, .var_id = local, .type = type});
  if (!inserted) {
    throw InternalError(
        "ModuleLowerer::MapStructuralVarBinding: structural variable symbol "
        "already mapped");
  }
}

auto ModuleLowerer::LookupStructuralVarBinding(
    const slang::ast::VariableSymbol& var) const
    -> std::optional<StructuralVarBinding> {
  const auto it = structural_var_bindings_.find(&var);
  if (it == structural_var_bindings_.end()) {
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
    const slang::ast::ValueSymbol& target, ScopeFrameId home_frame,
    hir::CrossUnitRefHead head, std::vector<hir::PathStep> path,
    hir::TypeId type) -> hir::CrossUnitRefId {
  auto& frame_dedup = cross_unit_ref_dedup_[home_frame];
  if (const auto it = frame_dedup.find(&target); it != frame_dedup.end()) {
    return it->second;
  }
  auto& slots = cross_unit_refs_by_frame_[home_frame];
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

auto ModuleLowerer::TakeCrossUnitRefsForFrame(ScopeFrameId frame)
    -> std::vector<hir::CrossUnitRefDecl> {
  const auto it = cross_unit_refs_by_frame_.find(frame);
  if (it == cross_unit_refs_by_frame_.end()) {
    return {};
  }
  auto out = std::move(it->second);
  cross_unit_refs_by_frame_.erase(it);
  return out;
}

auto ModuleLowerer::MakeCrossUnitMemberRef(
    const slang::ast::ValueSymbol& target, ScopeFrameId home_frame,
    hir::CrossUnitRefHead head, std::vector<hir::PathStep> path,
    hir::TypeId type, diag::SourceSpan span) -> hir::Expr {
  const hir::CrossUnitRefId slot = MapOrGetCrossUnitRef(
      target, home_frame, std::move(head), std::move(path), type);
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
    if (const auto binding = LookupStructuralVarBinding(*var)) {
      const auto hops = frame.HopsTo(binding->home_frame);
      if (!hops.has_value()) continue;
      out.push_back(
          hir::SensitivityEntry{
              .ref =
                  hir::StructuralVarRef{.hops = *hops, .var = binding->var_id},
              .bit_range = read.bit_range});
      continue;
    }
    // A read of a cross-unit member resolves to the slot that body lowering
    // created for it; subscribing through the slot is what makes always_comb
    // re-trigger on a child's signal.
    if (const auto slot = LookupCrossUnitRef(frame.Current(), *var)) {
      out.push_back(
          hir::SensitivityEntry{
              .ref = hir::CrossUnitVarRef{.id = *slot},
              .bit_range = read.bit_range});
    }
  }
  return out;
}

}  // namespace lyra::lowering::ast_to_hir
