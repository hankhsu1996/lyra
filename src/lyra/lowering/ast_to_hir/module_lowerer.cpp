#include "lyra/lowering/ast_to_hir/module_lowerer.hpp"

#include <algorithm>
#include <cstdint>
#include <expected>
#include <map>
#include <optional>
#include <string>
#include <unordered_map>
#include <utility>
#include <vector>

#include <slang/ast/Expression.h>
#include <slang/ast/Scope.h>
#include <slang/ast/Symbol.h>
#include <slang/ast/symbols/BlockSymbols.h>
#include <slang/ast/symbols/InstanceSymbols.h>
#include <slang/ast/symbols/SubroutineSymbols.h>
#include <slang/ast/symbols/ValueSymbol.h>
#include <slang/ast/symbols/VariableSymbols.h>
#include <slang/ast/types/NetType.h>
#include <slang/ast/types/Type.h>
#include <slang/numeric/SVInt.h>

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
  DeclareStructuralIdentities(*body_);
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

void ModuleLowerer::DeclareStructuralIdentities(
    const slang::ast::Scope& scope) {
  const ScopeFrameId frame = NextScopeFrameId();
  scope_frames_.emplace(&scope, frame);
  // A generate id is the source-order position of the generate among this
  // scope's instantiated generates, matching the arena index the body pass
  // assigns; an uninstantiated `if` / `case` arm carries no runtime object
  // (LRM 27.5) and consumes no id.
  std::uint32_t generate_count = 0;
  for (const auto& member : scope.members()) {
    if (member.kind == slang::ast::SymbolKind::GenerateBlock) {
      const auto& block = member.as<slang::ast::GenerateBlockSymbol>();
      if (block.isUninstantiated) continue;
      MapOwnedChildBinding(
          block, frame,
          hir::DownwardHead{
              .child = hir::GenerateChildRef{
                  .generate = hir::GenerateId{generate_count++},
                  .scope = hir::StructuralScopeId{0}}});
      DeclareStructuralIdentities(block);
    } else if (member.kind == slang::ast::SymbolKind::GenerateBlockArray) {
      const auto& array = member.as<slang::ast::GenerateBlockArraySymbol>();
      if (array.entries.empty()) continue;
      MapOwnedChildBinding(
          array, frame,
          hir::DownwardHead{
              .child = hir::GenerateChildRef{
                  .generate = hir::GenerateId{generate_count++},
                  .scope = hir::StructuralScopeId{0}}});
      for (const auto* entry : array.entries) {
        DeclareStructuralIdentities(*entry);
      }
    }
  }
}

auto ModuleLowerer::LookupScopeFrame(const slang::ast::Scope& scope) const
    -> ScopeFrameId {
  const auto it = scope_frames_.find(&scope);
  if (it == scope_frames_.end()) {
    throw InternalError(
        "ModuleLowerer::LookupScopeFrame: scope frame was not declared before "
        "body lowering");
  }
  return it->second;
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

namespace {

// The HIR net type of a cross-unit reference's target when the target is a net
// (LRM 6.7), or empty when it is a variable. The net type, not a plain net
// flag, is what determines the target's resolved cell -- its resolver and
// undriven value. Only `wire` / `tri` are supported; other net types are
// rejected at the net's own declaration, so encountering one here is a lowering
// invariant violation, not a user-facing case.
auto TargetNetType(const slang::ast::ValueSymbol& target)
    -> std::optional<hir::NetType> {
  if (target.kind != slang::ast::SymbolKind::Net) {
    return std::nullopt;
  }
  switch (target.as<slang::ast::NetSymbol>().netType.netKind) {
    case slang::ast::NetType::Wire:
      return hir::NetType::kWire;
    case slang::ast::NetType::Tri:
      return hir::NetType::kTri;
    default:
      throw InternalError(
          "TargetNetType: cross-unit reference to an unsupported net type; the "
          "net's own declaration validates the supported net types");
  }
}

}  // namespace

auto ModuleLowerer::MapOrGetCrossUnitRef(
    const slang::ast::ValueSymbol& target, ScopeFrameId slot_owner_frame,
    hir::CrossUnitRefHead head, std::vector<hir::PathSegment> path,
    hir::TypeId type) -> hir::CrossUnitRefId {
  auto& frame_dedup = cross_unit_ref_dedup_[slot_owner_frame];
  if (const auto it = frame_dedup.find(&target); it != frame_dedup.end()) {
    return it->second;
  }
  auto& slots = cross_unit_refs_by_frame_[slot_owner_frame];
  const hir::CrossUnitRefId id{static_cast<std::uint32_t>(slots.size())};
  slots.push_back(
      hir::CrossUnitRefDecl{
          .head = std::move(head),
          .path = std::move(path),
          .type = type,
          .target_net_type = TargetNetType(target)});
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
    hir::CrossUnitRefHead head, std::vector<hir::PathSegment> path,
    hir::TypeId type, diag::SourceSpan span) -> hir::Expr {
  const hir::CrossUnitRefId slot = MapOrGetCrossUnitRef(
      target, slot_owner_frame, std::move(head), std::move(path), type);
  return hir::Expr{
      .type = type,
      .data = hir::PrimaryExpr{.data = hir::CrossUnitVarRef{.id = slot}},
      .span = span,
  };
}

auto ModuleLowerer::TranslateReadTarget(
    const WalkFrame& frame, const slang::ast::ValueSymbol& value)
    -> std::optional<hir::SensitivityRef> {
  // Only an observable structural signal -- a variable or a net -- is a
  // sensitivity target. A genvar or parameter read folds to an elaboration
  // constant that cannot change, so it is never observed.
  if (value.kind != slang::ast::SymbolKind::Variable &&
      value.kind != slang::ast::SymbolKind::Net) {
    return std::nullopt;
  }
  // Pure enclosing lexical access: the target sits directly on the reader's own
  // scope or an ancestor, reached by a typed climb of `hops` parent edges.
  if (const auto binding = LookupStructuralDataObjectBinding(value)) {
    if (const auto hops = frame.HopsTo(binding->home_frame)) {
      return hir::SensitivityRef{
          hir::StructuralDataObjectRef{.hops = *hops, .var = binding->var_id}};
    }
  }

  // Otherwise reconstruct the reader-relative route from the target's
  // elaborated position. Walking up its owner chain, the head is the first
  // scope owned by an ancestor of the reader (the child of the deepest common
  // scope) and the scopes below it form the descent. A generate-loop iteration
  // or an instance-array element is addressed as an index on its array member,
  // which is the registered owned child.
  const auto type =
      InternType(value.getType(), SourceMapper().PointSpanOf(value.location));
  if (!type) return std::nullopt;

  std::vector<hir::PathSegment> descent;
  descent.push_back(
      hir::PathSegment{.name = std::string{value.name}, .indices = {}});
  const slang::ast::Scope* scope = value.getHierarchicalParent();
  while (scope != nullptr) {
    const slang::ast::Symbol* owned = &scope->asSymbol();
    const slang::ast::Scope* next = owned->getHierarchicalParent();
    std::vector<std::uint32_t> indices;
    // Resolve the addressable owned child this hierarchy step corresponds to,
    // and its per-dimension indices when it is an array element. Crossing a
    // unit boundary, the instance member -- not its body -- is the owned child;
    // a generate-loop iteration or instance-array element is indexed on its
    // enclosing array member, which is the registered child.
    if (owned->kind == slang::ast::SymbolKind::InstanceBody) {
      const auto* inst =
          owned->as<slang::ast::InstanceBodySymbol>().parentInstance;
      if (inst == nullptr) return std::nullopt;
      if (inst->arrayPath.empty()) {
        owned = inst;
      } else {
        indices.assign(inst->arrayPath.begin(), inst->arrayPath.end());
        owned = &inst->getParentScope()->asSymbol();
      }
      next = owned->getHierarchicalParent();
    } else if (const auto* gb = owned->as_if<slang::ast::GenerateBlockSymbol>();
               gb != nullptr && gb->getArrayIndex() != nullptr) {
      indices.push_back(
          static_cast<std::uint32_t>(
              gb->getArrayIndex()->as<std::int64_t>().value_or(0)));
      owned = &owned->getHierarchicalParent()->asSymbol();
      next = owned->getHierarchicalParent();
    }
    if (const auto obinding = LookupOwnedChildBinding(*owned)) {
      if (const auto hops = frame.HopsTo(obinding->home_frame)) {
        hir::DownwardHead head = obinding->head;
        head.hops = *hops;
        head.head_indices = std::move(indices);
        std::ranges::reverse(descent);
        const ScopeFrameId slot_owner =
            hops->value == 0 ? obinding->home_frame : frame.Current();
        const hir::CrossUnitRefId id = MapOrGetCrossUnitRef(
            value, slot_owner, hir::CrossUnitRefHead{std::move(head)},
            std::move(descent), *type);
        return hir::SensitivityRef{hir::CrossUnitVarRef{.id = id}};
      }
    }
    descent.push_back(
        hir::PathSegment{
            .name = std::string{owned->name}, .indices = std::move(indices)});
    scope = next;
  }
  // No reader-relative downward route: an upward reference climbs out to an
  // ancestor unit, reached by name across the boundary and already resolved by
  // the body.
  if (const auto slot = LookupCrossUnitRef(frame.Current(), value)) {
    return hir::SensitivityRef{hir::CrossUnitVarRef{.id = *slot}};
  }
  return std::nullopt;
}

auto ModuleLowerer::TranslateSensitivityReads(
    const std::vector<SensitivityRead>& reads, const WalkFrame& frame)
    -> std::vector<hir::SensitivityEntry> {
  std::vector<hir::SensitivityEntry> out;
  out.reserve(reads.size());
  for (const auto& read : reads) {
    // Both a variable and a net are observable value symbols a process can wait
    // on; sensitivity subscribes to either (a net's resolved value changing is
    // an update event just like a variable write, LRM 9.4.2).
    const auto* value = read.symbol->as_if<slang::ast::ValueSymbol>();
    if (value == nullptr) continue;
    // A footprint is meaningful only for a signal the runtime bit-addresses: a
    // packed bit vector, which renders to one observable cell whose change set
    // is read per bit. For an enum, unpacked aggregate, string, or real the
    // runtime observes the whole signal on any change, so the read carries no
    // footprint regardless of the flat-bit view the DFA computed over its own
    // encoding.
    const auto& read_type = value->getType();
    const std::optional<std::pair<std::uint64_t, std::uint64_t>> footprint =
        read_type.isIntegral() && !read_type.isEnum() ? read.footprint
                                                      : std::nullopt;
    if (auto ref = TranslateReadTarget(frame, *value)) {
      out.push_back(
          hir::SensitivityEntry{
              .ref = *std::move(ref), .footprint = footprint});
    }
  }
  return out;
}

}  // namespace lyra::lowering::ast_to_hir
