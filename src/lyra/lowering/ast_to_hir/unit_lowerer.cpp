#include "lyra/lowering/ast_to_hir/unit_lowerer.hpp"

#include <algorithm>
#include <cstdint>
#include <expected>
#include <optional>
#include <string>
#include <unordered_set>
#include <utility>
#include <vector>

#include <slang/ast/Expression.h>
#include <slang/ast/Scope.h>
#include <slang/ast/Symbol.h>
#include <slang/ast/statements/MiscStatements.h>
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
#include "lyra/lowering/ast_to_hir/instance_array_shape.hpp"
#include "lyra/lowering/ast_to_hir/statement/assertions.hpp"
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
  if (auto r = InternOwnClassDeclarations(*scope_); !r) {
    return std::unexpected(std::move(r.error()));
  }
  return PublishSignature();
}

auto UnitLowerer::TakeSignature() -> hir::UnitSignature {
  return std::move(signature_);
}

auto UnitLowerer::LowerBodies(const hir::UnitSignatures& signatures)
    -> diag::Result<hir::CompilationUnit> {
  signatures_ = &signatures;
  WalkFrame frame;
  StructuralScopeLowerer root(*this, *scope_);
  auto root_scope_or = root.Run(frame);
  if (!root_scope_or) {
    return std::unexpected(std::move(root_scope_or.error()));
  }
  unit_.root_scope = *std::move(root_scope_or);
  RequireEveryClassBodyLowered();
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

auto UnitLowerer::InternOwnClassDeclarations(const slang::ast::Scope& scope)
    -> diag::Result<void> {
  // A class is owned by the compilation unit that declares it. Slang exposes a
  // class declaration at scope level as one of two kinds: a `ClassType` for a
  // non-parameterized declaration (LRM 8.3), or a `GenericClassDefSymbol` for a
  // parameterized one (LRM 8.25), which carries one `ClassType` per live
  // specialization slang deduplicated during elaboration. Minting them before
  // any body lowers keeps class identity queryable through the unit's registry
  // from the moment any body resolves a reference, and gives a specialization
  // reached only from another unit its home in the declaring unit.
  //
  // The walk descends every structural scope, so which scope declares a class
  // is settled before any body lowers rather than by whichever reference
  // reaches the class first (LRM 23.9 makes a class a scope of the name tree,
  // and a generate block declares its own).
  for (const auto& member : scope.members()) {
    if (member.kind == slang::ast::SymbolKind::ClassType) {
      const auto& cls = member.as<slang::ast::ClassType>();
      const diag::SourceSpan span = SourceMapper().PointSpanOf(cls.location);
      if (auto r = InternLocalClass(cls, span); !r) {
        return std::unexpected(std::move(r.error()));
      }
      // A class is itself a scope, so a class it declares is reached by the
      // same walk (LRM 8.3 admits a class declaration as a class item).
      if (auto r = InternOwnClassDeclarations(cls); !r) {
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
        if (auto r = InternOwnClassDeclarations(cls); !r) {
          return std::unexpected(std::move(r.error()));
        }
      }
    } else if (member.kind == slang::ast::SymbolKind::GenerateBlock) {
      const auto& block = member.as<slang::ast::GenerateBlockSymbol>();
      if (block.isUninstantiated) continue;
      if (auto r = InternOwnClassDeclarations(block); !r) {
        return std::unexpected(std::move(r.error()));
      }
    } else if (member.kind == slang::ast::SymbolKind::GenerateBlockArray) {
      const auto& array = member.as<slang::ast::GenerateBlockArraySymbol>();
      for (const auto* entry : array.entries) {
        if (auto r = InternOwnClassDeclarations(*entry); !r) {
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
      MapOwnedChildBinding(member, frame, decls.instance_members.Declare());
    } else if (member.kind == slang::ast::SymbolKind::InstanceArray) {
      const auto shape = ResolveInstanceArrayShape(
          member.as<slang::ast::InstanceArraySymbol>());
      if (!shape.has_value()) {
        continue;
      }
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
    } else if (member.kind == slang::ast::SymbolKind::Modport) {
      // A name a view defines and offers only for reading stands for an
      // expression this unit evaluates (LRM 25.5.4), which is a subroutine of
      // this scope and takes its identity here with every other, so the
      // signature can name it before any body is lowered. Every other name a
      // view offers designates storage -- an item the view wrote no expression
      // for is the interface's own, and every direction but `input` bounds the
      // expression to an lvalue -- and storage is reached rather than asked
      // for.
      for (const auto& item : member.as<slang::ast::Scope>().members()) {
        const auto* port = item.as_if<slang::ast::ModportPortSymbol>();
        if (port == nullptr || !ViewDefinesTheName(*port)) continue;
        if (port->direction != slang::ast::ArgumentDirection::In) continue;
        MapModportEvaluator(*port, decls.structural_subroutines.Declare());
      }
    } else if (member.kind == slang::ast::SymbolKind::ProceduralBlock) {
      const auto& proc = member.as<slang::ast::ProceduralBlockSymbol>();
      if (!Contains(proc)) continue;
      // An assertion whose enabling condition is 1 is not a procedure the
      // design runs (LRM 16.14.5): what starts its attempts is the clock. So it
      // takes no process identity and nothing reaches into it by a hierarchical
      // name.
      if (StaticConcurrentAssertionOf(proc).assertion != nullptr) continue;
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

auto UnitLowerer::DeclaringStructuralScope(
    const slang::ast::ClassType& cls) const -> const slang::ast::Scope& {
  // A class nested in another class adds no level: SystemVerilog gives the
  // inner one no access to the outer object, so what its bodies reach is the
  // enclosing structural scope's instance and nothing between. Walking to the
  // nearest scope the declaration pass assigned a frame is what states that.
  for (const slang::ast::Scope* level = cls.getParentScope(); level != nullptr;
       level = level->asSymbol().getParentScope()) {
    if (scope_frames_.contains(level)) return *level;
  }
  throw InternalError(
      "UnitLowerer::DeclaringStructuralScope: a class of this unit is declared "
      "inside a structural scope of it");
}

auto UnitLowerer::DeclaringScopeChain(const slang::ast::Scope& scope) const
    -> std::vector<ScopeFrameId> {
  // Walking outward and reversing, rather than descending, because the walk
  // starts from the declaration and the enclosing chain is what slang already
  // holds. A scope the declaration pass assigned no frame is not a structural
  // scope and contributes no level, which is the same reading the pass itself
  // applied when it chose which members to descend into.
  std::vector<ScopeFrameId> chain;
  for (const slang::ast::Scope* level = &scope; level != nullptr;
       level = level->asSymbol().getParentScope()) {
    if (const auto it = scope_frames_.find(level); it != scope_frames_.end()) {
      chain.push_back(it->second);
    }
  }
  std::ranges::reverse(chain);
  return chain;
}

auto UnitLowerer::DeclaringScopeHopsFrom(
    const slang::ast::ClassType& cls, const WalkFrame& frame,
    diag::SourceSpan span) -> diag::Result<std::optional<hir::StructuralHops>> {
  // A namespace unit -- a package or the `$unit` scope (LRM 26.2, 3.12.1) --
  // replicates nothing, so an object of a class it declares belongs to no
  // instance and construction supplies none. Which unit declares the class
  // decides this, not which unit is being lowered: a package class reached
  // from a module needs no instance either.
  const slang::ast::Symbol& decl_unit = DeclaringCompilationUnit(cls);
  if (!IsDesignElement(decl_unit)) {
    return std::nullopt;
  }
  if (&decl_unit != &scope_->asSymbol()) {
    // A module or interface declares the class, so an object of it belongs to
    // one instance -- and what crosses a unit boundary is that unit's
    // signature, which carries no instance of a scope inside it.
    return diag::Fail(
        span, diag::DiagCode::kUnsupportedClassFeature,
        "constructing a class another compilation unit declares inside one of "
        "its scopes is not yet supported");
  }
  const slang::ast::Scope& declaring = DeclaringStructuralScope(cls);
  const auto hops = frame.HopsTo(LookupScopeFrame(declaring));
  if (!hops.has_value()) {
    return diag::Fail(
        span, diag::DiagCode::kUnsupportedClassFeature,
        "constructing a class declared in a scope this body does not stand "
        "inside is not yet supported");
  }
  return *hops;
}

auto UnitLowerer::TakeDeclaredClasses(const slang::ast::Scope& scope)
    -> std::vector<hir::ClassId> {
  const auto it = classes_by_scope_.find(&scope);
  if (it == classes_by_scope_.end()) return {};
  return std::move(it->second);
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

void UnitLowerer::MapModportEvaluator(
    const slang::ast::Symbol& port, hir::StructuralSubroutineId evaluator) {
  const auto [_, inserted] = modport_evaluators_.emplace(&port, evaluator);
  if (!inserted) {
    throw InternalError(
        "UnitLowerer::MapModportEvaluator: a name a modport offers is declared "
        "once");
  }
}

auto UnitLowerer::ModportEvaluatorOf(const slang::ast::Symbol& port) const
    -> hir::StructuralSubroutineId {
  const auto it = modport_evaluators_.find(&port);
  if (it == modport_evaluators_.end()) {
    throw InternalError(
        "UnitLowerer::ModportEvaluatorOf: every name a view offers only for "
        "reading takes its identity with the unit's other structural "
        "declarations");
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

auto UnitLowerer::InferredProcedureClock(const slang::ast::Symbol& containing)
    const -> const slang::ast::TimingControl* {
  const auto* proc = containing.as_if<slang::ast::ProceduralBlockSymbol>();
  if (proc == nullptr) {
    return nullptr;
  }
  return Sensitivity().AnalyzeProcedureClock(*proc);
}

auto UnitLowerer::Contains(const slang::ast::ProceduralBlockSymbol& proc) const
    -> bool {
  const StaticConcurrentAssertion found = StaticConcurrentAssertionOf(proc);
  if (found.assertion == nullptr) {
    return true;
  }
  return EvaluatedInSimulation(*found.assertion, AssertionPolicy());
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
  std::span<const slang::ast::StatementBlockSymbol* const> blocks = {};
};

auto ContributionOf(const slang::ast::Symbol& member, const UnitLowerer& owner)
    -> ScopeContribution {
  if (const auto* body = DeclaredSubroutineBody(member); body != nullptr) {
    return {.minted = body, .walked = body};
  }
  if (member.kind == slang::ast::SymbolKind::ProceduralBlock) {
    const auto& proc = member.as<slang::ast::ProceduralBlockSymbol>();
    // A statement label creates a named block around the statement it labels
    // (LRM 16.3), and the front end lists that block beside the process rather
    // than inside it. Whether the design has one is the process's answer, so
    // the process contributes its blocks and a process the design does not
    // contain carries them out of reach with it.
    if (!owner.Contains(proc)) {
      return {};
    }
    return {.minted = &member, .blocks = proc.getBlocks()};
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
    const slang::ast::Scope& declaring, const slang::ast::Scope& walked,
    UnitLowerer& owner,
    base::Registry<hir::ProceduralScopeDecl, hir::ProceduralScopeId>& scopes) {
  // A process's own blocks are listed beside it here as well, so the pass
  // gathers them first and then leaves them to the process that answers for
  // them; reaching one from this loop would mint an identity for a block whose
  // process the design may not contain, and nothing would go on to fill it.
  std::unordered_set<const slang::ast::Symbol*> process_blocks;
  for (const auto& member : walked.members()) {
    if (member.kind != slang::ast::SymbolKind::ProceduralBlock) {
      continue;
    }
    for (const auto* block :
         member.as<slang::ast::ProceduralBlockSymbol>().getBlocks()) {
      process_blocks.insert(block);
    }
  }
  for (const auto& member : walked.members()) {
    // Slang lists a base class's members in the derived class's member list
    // too (LRM 8.13 inheritance), and this pass mints for one declaration
    // scope: a member declared elsewhere is that declaration's own to mint,
    // and minting a second identity for it would leave one of them unfilled.
    if (member.getParentScope() != &walked ||
        process_blocks.contains(&member)) {
      continue;
    }
    const ScopeContribution contribution = ContributionOf(member, owner);
    if (contribution.minted != nullptr) {
      owner.DeclareProceduralScope(
          *contribution.minted, declaring, scopes.Declare());
    }
    for (const auto* block : contribution.blocks) {
      const ScopeContribution owned = ContributionOf(*block, owner);
      if (owned.minted != nullptr) {
        owner.DeclareProceduralScope(
            *owned.minted, declaring, scopes.Declare());
      }
      DeclareProceduralScopes(declaring, *block, owner, scopes);
    }
    if (contribution.walked != nullptr) {
      DeclareProceduralScopes(declaring, *contribution.walked, owner, scopes);
    }
  }
}

}  // namespace lyra::lowering::ast_to_hir
