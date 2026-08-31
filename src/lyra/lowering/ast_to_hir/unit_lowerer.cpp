#include "lyra/lowering/ast_to_hir/unit_lowerer.hpp"

#include <algorithm>
#include <cstdint>
#include <expected>
#include <map>
#include <optional>
#include <span>
#include <string>
#include <unordered_map>
#include <unordered_set>
#include <utility>
#include <variant>
#include <vector>

#include <slang/ast/Expression.h>
#include <slang/ast/Scope.h>
#include <slang/ast/Symbol.h>
#include <slang/ast/symbols/BlockSymbols.h>
#include <slang/ast/symbols/ClassSymbols.h>
#include <slang/ast/symbols/CompilationUnitSymbols.h>
#include <slang/ast/symbols/InstanceSymbols.h>
#include <slang/ast/symbols/PortSymbols.h>
#include <slang/ast/symbols/SubroutineSymbols.h>
#include <slang/ast/symbols/ValueSymbol.h>
#include <slang/ast/symbols/VariableSymbols.h>
#include <slang/ast/types/NetType.h>
#include <slang/ast/types/Type.h>
#include <slang/numeric/SVInt.h>

#include "lyra/base/internal_error.hpp"
#include "lyra/diag/diagnostic.hpp"
#include "lyra/diag/source_span.hpp"
#include "lyra/hir/compilation_unit.hpp"
#include "lyra/hir/expr.hpp"
#include "lyra/hir/value_ref.hpp"
#include "lyra/lowering/ast_to_hir/expression/references.hpp"
#include "lyra/lowering/ast_to_hir/instance_array_shape.hpp"
#include "lyra/lowering/ast_to_hir/sensitivity.hpp"
#include "lyra/lowering/ast_to_hir/structural_scope_lowerer.hpp"
#include "lyra/lowering/ast_to_hir/subroutine_decl.hpp"
#include "lyra/lowering/ast_to_hir/unit_identity.hpp"
#include "lyra/lowering/ast_to_hir/walk_frame.hpp"

namespace lyra::lowering::ast_to_hir {

UnitLowerer::UnitLowerer(
    const LoweringFacts& facts, const slang::ast::Scope& scope,
    std::string name, hir::UnitRole role)
    : facts_(facts), scope_(&scope), unit_{std::move(name)} {
  unit_.role = role;
  signature_.unit_name = unit_.name;
}

auto UnitLowerer::Declare() -> diag::Result<void> {
  if (auto r = DeclareStructuralIdentities(*scope_); !r) {
    return std::unexpected(std::move(r.error()));
  }
  if (auto r = InternOwnClassDeclarations(); !r) {
    return std::unexpected(std::move(r.error()));
  }
  return PublishSignature();
}

auto UnitLowerer::TakeSignature() -> hir::UnitSignature {
  return std::move(signature_);
}

void UnitLowerer::RecordReferencedUnit(std::string unit_name) {
  if (!std::ranges::contains(referenced_units_, unit_name)) {
    referenced_units_.push_back(std::move(unit_name));
  }
}

auto UnitLowerer::LowerBodies(hir::ConsumedSignatures signatures)
    -> diag::Result<hir::CompilationUnit> {
  consumed_signatures_ = std::move(signatures);
  WalkFrame frame;
  StructuralScopeLowerer root(*this, *scope_);
  auto root_scope_or = root.Run(frame);
  if (!root_scope_or) {
    return std::unexpected(std::move(root_scope_or.error()));
  }
  unit_.root_scope = *std::move(root_scope_or);
  unit_.root_scope.published_members.reserve(published_members_.size());
  for (const auto& decl : published_members_) {
    if (!decl.has_value()) {
      throw InternalError(
          "UnitLowerer::LowerBodies: a member this unit published stands on a "
          "declaration of its own, so the declaration walk reached it");
    }
    unit_.root_scope.published_members.push_back(*decl);
  }
  return std::move(unit_);
}

auto UnitLowerer::InternOwnClassDeclarations() -> diag::Result<void> {
  // A class is owned by the compilation unit that declares it. Slang exposes
  // this unit's class declarations at scope level as one of two kinds: a
  // `ClassType` for a non-parameterized declaration (LRM 8.3), or a
  // `GenericClassDefSymbol` for a parameterized one (LRM 8.25), which carries
  // one `ClassType` per live specialization slang deduplicated during
  // elaboration. Minting them here before any body lowers keeps class
  // identity queryable through the unit's registry from the moment any body
  // resolves a reference, and gives a specialization reached only from
  // another unit its home in the declaring unit.
  for (const auto& member : scope_->members()) {
    if (member.kind == slang::ast::SymbolKind::ClassType) {
      const auto& cls = member.as<slang::ast::ClassType>();
      const diag::SourceSpan span = SourceMapper().PointSpanOf(cls.location);
      if (auto r = InternLocalClass(cls, span); !r) {
        return std::unexpected(std::move(r.error()));
      }
    } else if (member.kind == slang::ast::SymbolKind::GenericClassDef) {
      const auto& def = member.as<slang::ast::GenericClassDefSymbol>();
      const diag::SourceSpan span = SourceMapper().PointSpanOf(def.location);
      for (const auto& spec : def.specializations()) {
        const auto& cls = spec.getCanonicalType().as<slang::ast::ClassType>();
        if (auto r = InternLocalClass(cls, span); !r) {
          return std::unexpected(std::move(r.error()));
        }
      }
    }
  }
  return {};
}

auto UnitLowerer::NextScopeFrameId() -> ScopeFrameId {
  return ScopeFrameId{.value = next_scope_frame_++};
}

auto UnitLowerer::NextWithClauseId() -> hir::WithClauseId {
  return hir::WithClauseId{.value = next_with_clause_++};
}

auto UnitLowerer::DeclareStructuralIdentities(const slang::ast::Scope& scope)
    -> diag::Result<void> {
  const ScopeFrameId frame = NextScopeFrameId();
  scope_frames_.emplace(&scope, frame);
  // A generate or instance owned-child id is the source-order position of that
  // child among its own kind in this scope, matching the arena index the body
  // pass assigns. A generate id counts instantiated generates -- an
  // uninstantiated `if` / `case` arm carries no runtime object (LRM 27.5) and
  // consumes no id. An instance-member id counts instances and non-empty
  // instance arrays -- a zero-element array (LRM 23.3.2) constructs nothing and
  // consumes no id. A subroutine id counts body-bearing subroutines, so a
  // bodyless DPI-C import consumes none; a process id counts procedural
  // blocks. Both match the arena index the body pass assigns, and both are
  // minted here so a call or a hierarchical reference resolves regardless of
  // source order (LRM 13.4.2 / 23.9).
  ScopeDeclarations& decls = scope_declarations_[&scope];
  for (const auto& member : scope.members()) {
    if (member.kind == slang::ast::SymbolKind::GenerateBlock) {
      const auto& block = member.as<slang::ast::GenerateBlockSymbol>();
      if (block.isUninstantiated) continue;
      MapOwnedChildBinding(
          block, frame,
          hir::GenerateChildRef{
              .generate = decls.generates.Declare(),
              .scope = hir::StructuralScopeId{0}});
      if (auto r = DeclareStructuralIdentities(block); !r) {
        return std::unexpected(std::move(r.error()));
      }
    } else if (member.kind == slang::ast::SymbolKind::GenerateBlockArray) {
      const auto& array = member.as<slang::ast::GenerateBlockArraySymbol>();
      if (array.entries.empty()) continue;
      // A loop generate elaborates each iteration into a block of its own
      // (LRM 27.4), so every iteration is a distinct child of this scope and
      // gets its own id rather than sharing the array's. Which iteration a
      // hierarchical reference names is then part of the child's identity,
      // not a coordinate carried alongside it.
      const hir::GenerateId generate = decls.generates.Declare();
      std::uint32_t block_index = 0;
      for (const auto* entry : array.entries) {
        MapOwnedChildBinding(
            *entry, frame,
            hir::GenerateChildRef{
                .generate = generate,
                .scope = hir::StructuralScopeId{block_index++}});
        if (auto r = DeclareStructuralIdentities(*entry); !r) {
          return std::unexpected(std::move(r.error()));
        }
      }
    } else if (member.kind == slang::ast::SymbolKind::Instance) {
      RecordReferencedUnit(
          SpecializationName(member.as<slang::ast::InstanceSymbol>()));
      MapOwnedChildBinding(member, frame, decls.instance_members.Declare());
    } else if (member.kind == slang::ast::SymbolKind::InstanceArray) {
      const auto shape = ResolveInstanceArrayShape(
          member.as<slang::ast::InstanceArraySymbol>());
      if (!shape.has_value()) {
        continue;
      }
      RecordReferencedUnit(SpecializationName(*shape->leaf));
      MapOwnedChildBinding(member, frame, decls.instance_members.Declare());
    } else if (member.kind == slang::ast::SymbolKind::Subroutine) {
      const auto& sub = member.as<slang::ast::SubroutineSymbol>();
      // A DPI-C import declares no body and reserves no subroutine id; the
      // unit interns its record on first sight from either side. What it does
      // record here is the scope it is declared in, which a `context` import
      // observes during its foreign call (LRM 35.5.3).
      if (sub.flags.has(slang::ast::MethodFlags::DPIImport)) {
        MapForeignImportScope(sub, frame);
        continue;
      }
      const hir::StructuralSubroutineId id =
          decls.structural_subroutines.Declare();
      MapSubroutineBinding(sub, frame, id);
      DeclareProceduralStatics(sub, sub, hir::ProceduralBodyRef{id}, frame);
    } else if (member.kind == slang::ast::SymbolKind::ProceduralBlock) {
      const auto& proc = member.as<slang::ast::ProceduralBlockSymbol>();
      if (!Contains(proc)) continue;
      const hir::ProcessId id = decls.processes.Declare();
      MapProcessBinding(proc, id);
      // The frontend hoists a process's outermost block into this scope's
      // member list, so the process is the only place that says which of those
      // blocks is its body.
      const auto* body_block =
          proc.getBody().as_if<slang::ast::BlockStatement>();
      if (body_block == nullptr || body_block->blockSymbol == nullptr) {
        continue;
      }
      DeclareProceduralStatics(
          *body_block->blockSymbol, proc, hir::ProceduralBodyRef{id}, frame);
    }
  }
  return {};
}

void UnitLowerer::DeclareProceduralStatics(
    const slang::ast::Scope& block, const slang::ast::Symbol& body_symbol,
    hir::ProceduralBodyRef body, ScopeFrameId frame) {
  for (const auto& member : block.members()) {
    if (member.kind == slang::ast::SymbolKind::StatementBlock) {
      DeclareProceduralStatics(
          member.as<slang::ast::StatementBlockSymbol>(), body_symbol, body,
          frame);
      continue;
    }
    if (member.kind != slang::ast::SymbolKind::Variable) continue;
    const auto& var = member.as<slang::ast::VariableSymbol>();
    if (var.lifetime != slang::ast::VariableLifetime::Static) continue;

    const hir::ProceduralVarId id =
        procedural_static_vars_[&body_symbol].Declare();
    const auto [_, inserted] = procedural_static_bindings_.emplace(
        &var,
        ProceduralStaticBinding{.home_frame = frame, .body = body, .var = id});
    if (!inserted) {
      throw InternalError(
          "UnitLowerer::DeclareProceduralStatics: procedural static already "
          "mapped");
    }
  }
}

auto UnitLowerer::MakeProceduralBody(const slang::ast::Symbol& body_symbol)
    -> hir::ProceduralBody {
  hir::ProceduralBody body;
  const auto it = procedural_static_vars_.find(&body_symbol);
  if (it == procedural_static_vars_.end()) return body;
  body.procedural_vars = std::move(it->second);
  return body;
}

auto UnitLowerer::TakeScopeDeclarations(const slang::ast::Scope& scope)
    -> ScopeDeclarations {
  const auto it = scope_declarations_.find(&scope);
  if (it == scope_declarations_.end()) return {};
  return std::move(it->second);
}

auto UnitLowerer::LookupScopeFrame(const slang::ast::Scope& scope) const
    -> ScopeFrameId {
  const auto it = scope_frames_.find(&scope);
  if (it == scope_frames_.end()) {
    throw InternalError(
        "UnitLowerer::LookupScopeFrame: scope frame was not declared before "
        "body lowering");
  }
  return it->second;
}

void UnitLowerer::MapStructuralDataObjectBinding(
    const slang::ast::ValueSymbol& var, ScopeFrameId home_frame,
    hir::StructuralDataObjectId local, hir::TypeId type) {
  const auto [_, inserted] = structural_data_object_bindings_.emplace(
      &var, StructuralDataObjectBinding{
                .home_frame = home_frame, .var_id = local, .type = type});
  if (!inserted) {
    throw InternalError(
        "UnitLowerer::MapStructuralDataObjectBinding: structural data object "
        "already mapped");
  }
  // A declaration this unit published takes the position its signature gave it,
  // so the object the unit builds and the object it promised are one shape.
  if (const auto it = published_member_ids_.find(&var);
      it != published_member_ids_.end()) {
    published_members_[it->second.value] = local;
  }
}

void UnitLowerer::MapInterfacePortBinding(
    const slang::ast::InterfacePortSymbol& port, ScopeFrameId home_frame,
    hir::InterfacePortId local, hir::ExternalUnitObjectId object) {
  const auto [_, inserted] = interface_port_bindings_.emplace(
      &port, InterfacePortBinding{
                 .home_frame = home_frame, .port = local, .object = object});
  if (!inserted) {
    throw InternalError(
        "UnitLowerer::MapInterfacePortBinding: interface port already mapped");
  }
  // A port this unit published takes the position its signature gave it, so the
  // object the unit builds and the object it promised are one shape.
  if (const auto it = published_member_ids_.find(&port);
      it != published_member_ids_.end()) {
    published_members_[it->second.value] = local;
  }
}

auto UnitLowerer::LookupInterfacePortBinding(const slang::ast::Symbol& port)
    const -> std::optional<InterfacePortBinding> {
  const auto it = interface_port_bindings_.find(&port);
  if (it == interface_port_bindings_.end()) {
    return std::nullopt;
  }
  return it->second;
}

auto UnitLowerer::LookupStructuralDataObjectBinding(
    const slang::ast::ValueSymbol& var) const
    -> std::optional<StructuralDataObjectBinding> {
  const auto it = structural_data_object_bindings_.find(&var);
  if (it == structural_data_object_bindings_.end()) {
    return std::nullopt;
  }
  return it->second;
}

void UnitLowerer::MapSubroutineBinding(
    const slang::ast::SubroutineSymbol& sym, ScopeFrameId owner_frame,
    hir::StructuralSubroutineId local) {
  const auto [_, inserted] = subroutine_bindings_.emplace(
      &sym,
      SubroutineBinding{.owner_frame = owner_frame, .subroutine_id = local});
  if (!inserted) {
    throw InternalError(
        "UnitLowerer::MapSubroutineBinding: subroutine symbol already "
        "mapped");
  }
}

auto UnitLowerer::LookupSubroutineBinding(
    const slang::ast::SubroutineSymbol& sym) const
    -> std::optional<SubroutineBinding> {
  const auto it = subroutine_bindings_.find(&sym);
  if (it == subroutine_bindings_.end()) {
    return std::nullopt;
  }
  return it->second;
}

void UnitLowerer::MapForeignImportScope(
    const slang::ast::SubroutineSymbol& sym, ScopeFrameId declaring_frame) {
  const auto [_, inserted] =
      foreign_import_scopes_.emplace(&sym, declaring_frame);
  if (!inserted) {
    throw InternalError(
        "UnitLowerer::MapForeignImportScope: DPI import symbol already mapped");
  }
}

auto UnitLowerer::LookupForeignImportScope(
    const slang::ast::SubroutineSymbol& sym) const
    -> std::optional<ScopeFrameId> {
  const auto it = foreign_import_scopes_.find(&sym);
  if (it == foreign_import_scopes_.end()) {
    return std::nullopt;
  }
  return it->second;
}

auto UnitLowerer::EnsureForeignImport(const slang::ast::SubroutineSymbol& sym)
    -> diag::Result<hir::ForeignImportId> {
  if (const auto it = foreign_import_bindings_.find(&sym);
      it != foreign_import_bindings_.end()) {
    return it->second;
  }
  auto decl_or = LowerForeignImport(*this, sym);
  if (!decl_or) return std::unexpected(std::move(decl_or.error()));
  const hir::ForeignImportId id =
      unit_.foreign_imports.Add(*std::move(decl_or));
  foreign_import_bindings_.emplace(&sym, id);
  return id;
}

void UnitLowerer::MapPatternVar(
    const slang::ast::PatternVarSymbol& sym, hir::PatternId pattern) {
  const auto [_, inserted] = pattern_var_bindings_.emplace(&sym, pattern);
  if (!inserted) {
    throw InternalError(
        "UnitLowerer::MapPatternVar: pattern-bound identifier already mapped; "
        "its id indexes one body's arena, so a second mapping would silently "
        "redirect the first body's references");
  }
}

auto UnitLowerer::LookupPatternVar(const slang::ast::PatternVarSymbol& sym)
    const -> std::optional<hir::PatternId> {
  const auto it = pattern_var_bindings_.find(&sym);
  if (it == pattern_var_bindings_.end()) return std::nullopt;
  return it->second;
}

void UnitLowerer::MapOwnedChildBinding(
    const slang::ast::Symbol& child, ScopeFrameId home_frame,
    hir::OwnedChildRef child_ref) {
  const auto [_, inserted] = owned_child_bindings_.emplace(
      &child, OwnedChildBinding{.home_frame = home_frame, .child = child_ref});
  if (!inserted) {
    throw InternalError(
        "UnitLowerer::MapOwnedChildBinding: owned child already mapped");
  }
}

auto UnitLowerer::LookupOwnedChildBinding(const slang::ast::Symbol& child) const
    -> std::optional<OwnedChildBinding> {
  const auto it = owned_child_bindings_.find(&child);
  if (it == owned_child_bindings_.end()) {
    return std::nullopt;
  }
  return it->second;
}

void UnitLowerer::MapProcessBinding(
    const slang::ast::ProceduralBlockSymbol& proc, hir::ProcessId id) {
  const auto [_, inserted] = process_bindings_.emplace(&proc, id);
  if (!inserted) {
    throw InternalError(
        "UnitLowerer::MapProcessBinding: process symbol already mapped");
  }
}

auto UnitLowerer::LookupProcessBinding(
    const slang::ast::ProceduralBlockSymbol& proc) const
    -> std::optional<hir::ProcessId> {
  const auto it = process_bindings_.find(&proc);
  if (it == process_bindings_.end()) {
    return std::nullopt;
  }
  return it->second;
}

auto UnitLowerer::LookupProceduralStatic(const slang::ast::Symbol& var) const
    -> std::optional<ProceduralStaticBinding> {
  const auto it = procedural_static_bindings_.find(&var);
  if (it == procedural_static_bindings_.end()) {
    return std::nullopt;
  }
  return it->second;
}

namespace {

// The compilation unit a value is declared directly in when that unit is a
// namespace -- a package (LRM 26.2) or the anonymous `$unit` scope (LRM
// 3.12.1) -- or nullptr when the value belongs to an instantiated scope and is
// reached by a route. A namespace unit has no instance, so its declarations are
// one program-global cell each, named rather than routed to.
auto DeclaringUnitOfValue(const slang::ast::ValueSymbol& value)
    -> const slang::ast::Symbol* {
  const slang::ast::Scope* scope = value.getParentScope();
  if (scope == nullptr) return nullptr;
  const slang::ast::Symbol& owner = scope->asSymbol();
  if (owner.kind != slang::ast::SymbolKind::Package &&
      owner.kind != slang::ast::SymbolKind::CompilationUnit) {
    return nullptr;
  }
  return &owner;
}

}  // namespace

auto UnitLowerer::MapOrGetRoutedRef(
    const slang::ast::ValueSymbol& target, ScopeFrameId slot_owner_frame,
    hir::RoutedRefDecl decl) -> hir::RoutedRefId {
  auto& frame_dedup = routed_ref_dedup_[slot_owner_frame];
  if (const auto it = frame_dedup.find(&target); it != frame_dedup.end()) {
    return it->second;
  }
  auto& slots = routed_refs_by_frame_[slot_owner_frame];
  const hir::RoutedRefId id{static_cast<std::uint32_t>(slots.size())};
  slots.push_back(std::move(decl));
  frame_dedup.emplace(&target, id);
  return id;
}

auto UnitLowerer::TakeRoutedRefsForFrame(ScopeFrameId slot_owner_frame)
    -> std::vector<hir::RoutedRefDecl> {
  const auto it = routed_refs_by_frame_.find(slot_owner_frame);
  if (it == routed_refs_by_frame_.end()) {
    return {};
  }
  auto out = std::move(it->second);
  routed_refs_by_frame_.erase(it);
  return out;
}

auto UnitLowerer::MakeRoutedMemberRef(
    const slang::ast::ValueSymbol& target, ScopeFrameId slot_owner_frame,
    hir::RoutedRefDecl decl, diag::SourceSpan span) -> hir::Expr {
  const hir::TypeId type = decl.recipe.type;
  const hir::RoutedRefId slot =
      MapOrGetRoutedRef(target, slot_owner_frame, std::move(decl));
  return hir::Expr{
      .type = type,
      .data = hir::PrimaryExpr{.data = hir::RoutedRef{.id = slot}},
      .span = span,
  };
}

auto UnitLowerer::PublishedRouteTarget(
    const slang::ast::ValueSymbol& value, std::span<const hir::PathStep> steps)
    -> std::optional<RouteTarget> {
  // Reaching a member of another unit by name needs a pointer to that unit's
  // own object, which two steps produce: onto an instance this unit declares,
  // and through an interface port, which stands for an instance some other
  // scope declares (LRM 25.3). A step into a generate scope stays inside this
  // unit, and one resolved by name yields a scope with no declaration behind
  // it.
  if (steps.empty()) return std::nullopt;
  const bool lands_on_unit_object = std::visit(
      Overloaded{
          [](const hir::OwnedChildStep& owned) {
            return std::holds_alternative<hir::InstanceMemberId>(owned.child);
          },
          [](const hir::InterfacePortStep&) { return true; },
          [](const hir::OpaqueStep&) { return false; }},
      steps.back());
  if (!lands_on_unit_object) return std::nullopt;

  const slang::ast::Scope* owner = value.getHierarchicalParent();
  if (owner == nullptr) return std::nullopt;
  const auto* body = owner->asSymbol().as_if<slang::ast::InstanceBodySymbol>();
  if (body == nullptr) return std::nullopt;

  const hir::UnitSignature* signature =
      Signatures().Find(SpecializationName(*body));
  if (signature == nullptr || !signature->instance_class.has_value()) {
    return std::nullopt;
  }
  const auto member_id = signature->instance_class->Find(value.name);
  if (!member_id.has_value()) return std::nullopt;

  // The name resolved against the signature; from here the route carries the
  // position, and the member's type is already in this unit's pool because the
  // record brought it there.
  const hir::ExternalUnitObjectId object =
      ExternalUnitObjectOf(signature->unit_name);
  const hir::PublishedMember& member =
      unit_.external_unit_objects.Get(object).members.Get(*member_id);
  return RouteTarget{
      .leaf = hir::SignatureMemberLeaf{.object = object, .member = *member_id},
      .type = member.type,
      .storage = member.storage};
}

auto UnitLowerer::ResolveRouteTarget(
    const slang::ast::ValueSymbol& value, std::span<const hir::PathStep> steps)
    -> diag::Result<RouteTarget> {
  // A member the owning unit published is named against the signature this
  // unit consumed, which also states what storage the name reaches -- so
  // nothing about it is read off the unit that declared it.
  if (auto published = PublishedRouteTarget(value, steps)) {
    return *std::move(published);
  }

  auto type =
      InternType(value.getType(), SourceMapper().PointSpanOf(value.location));
  if (!type) return std::unexpected(std::move(type.error()));

  auto storage =
      DeclarationStorage(value, SourceMapper().PointSpanOf(value.location));
  if (!storage) return std::unexpected(std::move(storage.error()));

  // This unit's own identity for the target when it declares it -- and for a
  // static a named block puts on the hierarchical path (LRM 23.9), that
  // identity also says the blocks between the static and its structural scope
  // describe where the storage sits rather than steps the route takes.
  if (const auto data_object = LookupStructuralDataObjectBinding(value)) {
    return RouteTarget{
        .leaf = hir::StructuralDataObjectLeaf{.object = data_object->var_id},
        .type = *type,
        .storage = *std::move(storage)};
  }
  if (const auto procedural_static = LookupProceduralStatic(value)) {
    return RouteTarget{
        .leaf =
            hir::ProceduralStaticLeaf{
                .body = procedural_static->body, .var = procedural_static->var},
        .type = *type,
        .storage = *std::move(storage)};
  }
  // Nothing was published to compile against, so the name is all that crosses
  // and the runtime answers it during elaboration (LRM 23.6). What storage it
  // reaches has no statement either, which is why it is read off the frontend.
  return RouteTarget{
      .leaf = hir::OpaqueLeaf{.name = std::string{value.name}},
      .type = *type,
      .storage = *std::move(storage)};
}

auto UnitLowerer::MakeRoutedRef(
    const slang::ast::ValueSymbol& value, ScopeFrameId slot_owner,
    hir::RouteHead head, std::vector<hir::PathStep> steps)
    -> diag::Result<hir::ReferenceRoute> {
  auto target = ResolveRouteTarget(value, steps);
  if (!target) return std::unexpected(std::move(target.error()));
  const hir::RoutedRefId id = MapOrGetRoutedRef(
      value, slot_owner,
      hir::RoutedRefDecl{
          .recipe =
              hir::RoutedPathRecipe{
                  .head = std::move(head),
                  .steps = std::move(steps),
                  .leaf = std::move(target->leaf),
                  .type = target->type},
          .target_storage = target->storage});
  return hir::ReferenceRoute{hir::RoutedRef{.id = id}};
}

auto UnitLowerer::TranslateReferenceRoute(
    const WalkFrame& frame, const slang::ast::ValueSymbol& value)
    -> diag::Result<std::optional<hir::ReferenceRoute>> {
  // This unit's own identity for the target, if it declares it.
  const auto data_object = LookupStructuralDataObjectBinding(value);
  const auto procedural_static = LookupProceduralStatic(value);
  const std::optional<hir::StructuralHops> data_object_hops =
      data_object ? frame.HopsTo(data_object->home_frame) : std::nullopt;

  // A data object of the reader's own scope is a direct member of `self`: the
  // one shape that is no route at all, and so has no leaf and no sealed
  // endpoint.
  if (data_object_hops.has_value() && data_object_hops->value == 0) {
    return hir::ReferenceRoute{
        hir::DirectMemberRef{.var = data_object->var_id}};
  }

  const auto routed_ref = [&](ScopeFrameId slot_owner, hir::RouteHead head,
                              std::vector<hir::PathStep> steps)
      -> diag::Result<std::optional<hir::ReferenceRoute>> {
    auto route =
        MakeRoutedRef(value, slot_owner, std::move(head), std::move(steps));
    if (!route) return std::unexpected(std::move(route.error()));
    return *route;
  };

  // The target's storage hangs under an ancestor scope of the same unit, so
  // the whole route is a typed climb to it: a routed reference sealed once in
  // the resolve phase rather than re-walked on each access.
  const auto in_unit_route = [&](hir::StructuralHops hops) {
    return routed_ref(frame.Current(), hir::InUnitHead{.hops = hops}, {});
  };
  if (data_object_hops.has_value()) {
    return in_unit_route(*data_object_hops);
  }
  if (procedural_static) {
    if (const auto hops = frame.HopsTo(procedural_static->home_frame)) {
      return in_unit_route(*hops);
    }
  }

  // The reader's elaborated ancestor scopes, across unit boundaries (slang's
  // `getHierarchicalParent` crosses the boundary at the instance-body
  // transition). The route meets the target at the deepest scope shared with
  // the reader; a target-side hop whose parent is one of these scopes is the
  // named child that shared ancestor exposes -- the head an out-of-unit or
  // sibling-subtree reference climbs to and descends from.
  std::unordered_set<const slang::ast::Scope*> reader_ancestors;
  for (const slang::ast::Scope* s = frame.reader_scope; s != nullptr;
       s = s->asSymbol().getHierarchicalParent()) {
    reader_ancestors.insert(s);
  }

  // Walk the target's owner chain, building the descent bottom-up. Each hop's
  // addressable owned child is resolved: the instance member (not its body)
  // across a unit boundary, the array member for a generate-loop iteration or
  // instance-array element with the elaborated index attached.
  std::vector<hir::PathStep> descent;
  const slang::ast::Scope* scope = value.getHierarchicalParent();
  while (scope != nullptr) {
    const slang::ast::Symbol* owned = &scope->asSymbol();
    const slang::ast::Scope* next = owned->getHierarchicalParent();
    std::vector<std::uint32_t> indices;
    // A target this unit declares reaches its storage through the scope that
    // owns it, so the named blocks in between are part of where the storage
    // sits rather than steps of their own.
    if (owned->kind == slang::ast::SymbolKind::StatementBlock &&
        (data_object || procedural_static)) {
      scope = next;
      continue;
    }
    if (owned->kind == slang::ast::SymbolKind::InstanceBody) {
      const auto* inst =
          owned->as<slang::ast::InstanceBodySymbol>().parentInstance;
      if (inst == nullptr) return std::nullopt;
      if (inst->arrayPath.empty()) {
        owned = inst;
      } else {
        // A multi-dimensional instance array nests one InstanceArray symbol per
        // dimension, but the unit registers a single array member spanning all
        // dimensions and `arrayPath` already carries every index. Climb to the
        // outermost array symbol so the head is that registered member.
        indices.assign(inst->arrayPath.begin(), inst->arrayPath.end());
        owned = &inst->getParentScope()->asSymbol();
        while (owned->getParentScope() != nullptr &&
               owned->getParentScope()->asSymbol().kind ==
                   slang::ast::SymbolKind::InstanceArray) {
          owned = &owned->getParentScope()->asSymbol();
        }
      }
      next = owned->getHierarchicalParent();
    } else if (const auto* gb = owned->as_if<slang::ast::GenerateBlockSymbol>();
               gb != nullptr && gb->getArrayIndex() != nullptr) {
      const slang::ast::Symbol& array =
          owned->getHierarchicalParent()->asSymbol();
      // The unit that declares a loop iteration declares it as a child in its
      // own right, so the iteration is the step and its elaborated position is
      // already part of that identity. Across the artifact boundary only the
      // array's source name travels, and the position picks the iteration out
      // of it (LRM 27.4).
      if (!LookupOwnedChildBinding(*owned).has_value()) {
        indices.push_back(
            static_cast<std::uint32_t>(
                gb->getArrayIndex()->as<std::int64_t>().value_or(0)));
        owned = &array;
      }
      next = array.getHierarchicalParent();
    }

    // The head is the child of the deepest scope shared with the reader: the
    // first hop whose parent scope is a reader ancestor. Everything already
    // accumulated is the descent below it. Stopping at the shared-scope child
    // rather than the first bound owned child is what makes a procedurally
    // nested head (a named block inside another) head at the block the shared
    // scope directly exposes, not the inner one.
    if (next != nullptr && reader_ancestors.contains(next)) {
      // A head whose owning scope this unit emits stays inside this unit's
      // layout: the climb to that scope is typed, and the head becomes the
      // route's first typed step.
      if (const auto obinding = LookupOwnedChildBinding(*owned)) {
        if (const auto hops = frame.HopsTo(obinding->home_frame)) {
          descent.emplace_back(
              hir::OwnedChildStep{
                  .child = obinding->child, .indices = std::move(indices)});
          std::ranges::reverse(descent);
          const ScopeFrameId slot_owner =
              hops->value == 0 ? obinding->home_frame : frame.Current();
          return routed_ref(
              slot_owner, hir::InUnitHead{.hops = *hops}, std::move(descent));
        }
      }
      // No owned-child binding: this unit does not declare the head, so it
      // lives in an ancestor compilation unit (an upward reference climbs out
      // through the reader's own instance to reach it). A generate block or a
      // named block in that other unit is reached by name across the boundary,
      // the same as an instance head. When this unit does own the head the
      // typed branch above always takes it, so reaching here is exactly the
      // cross-unit case and never a silent fallback for a local one.
      std::ranges::reverse(descent);
      return routed_ref(
          frame.Current(),
          hir::VisibleChildHead{
              .head_name = std::string{owned->name},
              .head_indices = std::move(indices)},
          std::move(descent));
    }

    // A step this unit declares stays inside its layout and carries the
    // declaring scope's identity; one it does not is past the artifact
    // boundary, where the canonical name is the only identity that travels.
    if (const auto obinding = LookupOwnedChildBinding(*owned)) {
      descent.emplace_back(
          hir::OwnedChildStep{
              .child = obinding->child, .indices = std::move(indices)});
    } else {
      descent.emplace_back(
          hir::OpaqueStep{
              .name = std::string{owned->name}, .indices = std::move(indices)});
    }
    scope = next;
  }
  return std::nullopt;
}

auto UnitLowerer::ResolveValueTarget(
    const WalkFrame& frame, const slang::ast::ValueSymbol& value)
    -> diag::Result<std::optional<hir::ValueTarget>> {
  // Only a variable or a net has a cell. A parameter, genvar, or enum value is
  // a compile-time constant that folds where it is used, so there is nothing to
  // reach: no route to seal and nothing to observe. Deciding this once, ahead
  // of both ways of reaching a cell, is what stops a constant declared in a
  // namespace unit from being mistaken for that unit's program-global cell.
  if (value.kind != slang::ast::SymbolKind::Variable &&
      value.kind != slang::ast::SymbolKind::Net) {
    return std::nullopt;
  }

  // A namespace unit has no instance, so its cell is reached by name rather
  // than by a route out of the reader's own storage (LRM 26.2, 3.12.1). The
  // same by-name form serves a referrer in another unit and the owning unit's
  // own body, neither of which has a receiver to route through.
  if (const auto* unit = DeclaringUnitOfValue(value)) {
    auto value_type =
        InternType(value.getType(), SourceMapper().PointSpanOf(value.location));
    if (!value_type) return std::unexpected(std::move(value_type.error()));
    return hir::ValueTarget{hir::ExternalUnitValueRef{
        .unit_name = CompilationUnitName(*unit),
        .variable_name = std::string{value.name},
        .value_type = *value_type}};
  }

  auto route = TranslateReferenceRoute(frame, value);
  if (!route) return std::unexpected(std::move(route.error()));
  if (!route->has_value()) return std::nullopt;
  return hir::ValueTarget{*std::move(*route)};
}

auto UnitLowerer::TranslateSensitivityReads(
    const std::vector<SensitivityRead>& reads, const WalkFrame& frame,
    support::EventEdge edge)
    -> diag::Result<std::vector<hir::SensitivityEntry>> {
  std::vector<hir::SensitivityEntry> out;
  out.reserve(reads.size());
  for (const auto& read : reads) {
    auto target = ResolveValueTarget(frame, *read.symbol);
    if (!target) return std::unexpected(std::move(target.error()));
    if (!target->has_value()) continue;
    // A footprint is meaningful only for a signal the runtime bit-addresses: a
    // packed bit vector, which renders to one observable cell whose change set
    // is read per bit. For an enum, unpacked aggregate, string, or real the
    // runtime observes the whole signal on any change, so the read carries no
    // footprint regardless of the flat-bit view the DFA computed over its own
    // encoding.
    const auto& read_type = read.symbol->getType();
    out.push_back(
        hir::SensitivityEntry{
            .ref = *std::move(*target),
            .footprint = read_type.isIntegral() && !read_type.isEnum()
                             ? read.footprint
                             : std::nullopt,
            .edge_kind = edge});
  }
  return out;
}

namespace {

// The subroutine body a scope member declares, or nothing when it declares
// none. A scope holds the subroutine itself where the source wrote the body
// inline, and holds a prototype where the source put the body outside the class
// (LRM 8.24) -- one declaration, reached two ways. Three members declare no
// body at all: a pure virtual method (LRM 8.21) is a signature and nothing
// else, and a DPI-C import (LRM 35.4) and a compiler-generated class built-in
// (the randomize family, LRM 18.6) are provided rather than lowered from
// source.
auto DeclaredSubroutineBody(const slang::ast::Symbol& member)
    -> const slang::ast::SubroutineSymbol* {
  if (member.kind == slang::ast::SymbolKind::MethodPrototype) {
    const auto& proto = member.as<slang::ast::MethodPrototypeSymbol>();
    return proto.flags.has(slang::ast::MethodFlags::Pure)
               ? nullptr
               : proto.getSubroutine();
  }
  if (member.kind != slang::ast::SymbolKind::Subroutine) {
    return nullptr;
  }
  const auto& sub = member.as<slang::ast::SubroutineSymbol>();
  const bool provided = sub.flags.has(slang::ast::MethodFlags::DPIImport) ||
                        sub.flags.has(slang::ast::MethodFlags::BuiltIn);
  return provided ? nullptr : &sub;
}

// What one member of a declaration scope gives this pass: the symbol whose
// procedural-scope identity is minted now, and the scope the walk continues
// into. The two are independent questions and all four answers occur -- a
// subroutine gives both, an unnamed block only a scope to walk, a procedural
// block only an identity, and a variable or a type neither -- so they are
// answered as data rather than decided inside a branch that then acts.
struct ScopeContribution {
  const slang::ast::Symbol* minted = nullptr;
  const slang::ast::Scope* walked = nullptr;
};

auto ContributionOf(const slang::ast::Symbol& member, const UnitLowerer& owner)
    -> ScopeContribution {
  if (const auto* body = DeclaredSubroutineBody(member); body != nullptr) {
    return {.minted = body, .walked = body};
  }
  if (member.kind == slang::ast::SymbolKind::ProceduralBlock) {
    const auto& proc = member.as<slang::ast::ProceduralBlockSymbol>();
    return {.minted = owner.Contains(proc) ? &member : nullptr};
  }
  if (member.kind == slang::ast::SymbolKind::StatementBlock) {
    const auto& block = member.as<slang::ast::StatementBlockSymbol>();
    // Only a block the source named can be named from elsewhere, so only one
    // needs an identity before the bodies lower. A block slang recorded for its
    // own reasons -- the implicit scope a pattern arm's bindings live in, the
    // one a loop's control variables live in -- is reached only by the walk
    // that lowers it, which mints its identity there.
    return {.minted = block.name.empty() ? nullptr : &member, .walked = &block};
  }
  return {};
}

}  // namespace

void DeclareProceduralScopes(
    const slang::ast::Scope& slang_scope, UnitLowerer& owner,
    base::Registry<hir::ProceduralScopeDecl, hir::ProceduralScopeId>& scopes) {
  for (const auto& member : slang_scope.members()) {
    // Slang lists a base class's members in the derived class's member list
    // too (LRM 8.13 inheritance), and this pass mints for one declaration
    // scope: a member declared elsewhere is that declaration's own to mint,
    // and minting a second identity for it would leave one of them unfilled.
    if (member.getParentScope() != &slang_scope) {
      continue;
    }
    const ScopeContribution contribution = ContributionOf(member, owner);
    if (contribution.minted != nullptr) {
      owner.DeclareProceduralScope(*contribution.minted, scopes.Declare());
    }
    if (contribution.walked != nullptr) {
      DeclareProceduralScopes(*contribution.walked, owner, scopes);
    }
  }
}

}  // namespace lyra::lowering::ast_to_hir
