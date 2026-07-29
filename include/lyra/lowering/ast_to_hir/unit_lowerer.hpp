#pragma once

#include <cstdint>
#include <map>
#include <optional>
#include <unordered_map>
#include <vector>

#include <slang/ast/symbols/ClassSymbols.h>
#include <slang/ast/symbols/InstanceSymbols.h>
#include <slang/ast/symbols/SubroutineSymbols.h>
#include <slang/ast/symbols/ValueSymbol.h>
#include <slang/ast/symbols/VariableSymbols.h>
#include <slang/ast/types/Type.h>

#include "lyra/base/internal_error.hpp"
#include "lyra/diag/diagnostic.hpp"
#include "lyra/diag/source_span.hpp"
#include "lyra/frontend/slang_source_mapper.hpp"
#include "lyra/hir/class_ref.hpp"
#include "lyra/hir/compilation_unit.hpp"
#include "lyra/hir/expr.hpp"
#include "lyra/hir/field_id.hpp"
#include "lyra/hir/method_id.hpp"
#include "lyra/hir/pattern_id.hpp"
#include "lyra/hir/structural_data_object.hpp"
#include "lyra/hir/structural_scope.hpp"
#include "lyra/hir/value_ref.hpp"
#include "lyra/lowering/ast_to_hir/sensitivity.hpp"
#include "lyra/lowering/ast_to_hir/walk_frame.hpp"
#include "lyra/support/event_edge.hpp"

namespace slang::ast {
class Expression;
class ClassType;
class Scope;
}  // namespace slang::ast

namespace lyra::lowering::ast_to_hir {

struct StructuralDataObjectBinding {
  ScopeFrameId home_frame{};
  hir::StructuralDataObjectId var_id{};
  hir::TypeId type{};
};

// Keyed by `ValueSymbol`, the common base of a variable and a net: both are
// named structural signals a reference or a continuous-assignment target binds
// to the same way.
using StructuralDataObjectBindings = std::unordered_map<
    const slang::ast::ValueSymbol*, StructuralDataObjectBinding>;

struct SubroutineBinding {
  ScopeFrameId owner_frame{};
  hir::StructuralSubroutineId subroutine_id{};
};

using SubroutineBindings =
    std::unordered_map<const slang::ast::SubroutineSymbol*, SubroutineBinding>;

// A DPI-C import call resolves to the unit's own record of the import. An
// import is a bodyless external callable, not a body-bearing structural
// subroutine, so it binds in a space of its own. The map is also what makes one
// entry per declaration: a unit that both declares an import and calls it, or
// calls one declaration from several places, interns it once.
using ForeignImportBindings = std::unordered_map<
    const slang::ast::SubroutineSymbol*, hir::ForeignImportId>;

// The instantiated scope a DPI-C import declaration sits in, for the imports
// written inside this unit's own scopes. A `context` import observes that scope
// for the duration of its foreign call (LRM 35.5.3). An import declared in a
// package or at `$unit` scope has no entry, because such a namespace is never
// instantiated and the import therefore observes no scope.
using ForeignImportScopes =
    std::unordered_map<const slang::ast::SubroutineSymbol*, ScopeFrameId>;

// A downward reference's leading component names an owned child this scope
// declares: an instance / instance-array member (`c.x`, `c[1].x`), or a
// generate block (`g[1].x`, LRM 27). The child's slang symbol maps to the
// head the reference navigates from, so the reference resolves regardless of
// whether it precedes the child in source.
struct OwnedChildBinding {
  ScopeFrameId home_frame{};
  hir::DownwardHead head;
};

using OwnedChildBindings =
    std::unordered_map<const slang::ast::Symbol*, OwnedChildBinding>;

// Shared lowering-pass facts threaded into every UnitLowerer. SourceMapper
// translates slang source locations; SensitivityAnalyzer is shared across
// every unit's analysis (caches reads); disable_assertions is the policy
// that elides assertion constructs instead of rejecting them. Subset of
// `LowerCompilationFacts` that excludes the slang Compilation handle (which
// only the driver-level CompilationLowerer needs to walk top instances).
class LoweringFacts {
 public:
  LoweringFacts(
      const frontend::SlangSourceMapper& source_mapper,
      SensitivityAnalyzer& sensitivity_analyzer, bool disable_assertions)
      : source_mapper_(&source_mapper),
        sensitivity_analyzer_(&sensitivity_analyzer),
        disable_assertions_(disable_assertions) {
  }

  [[nodiscard]] auto SourceMapper() const
      -> const frontend::SlangSourceMapper& {
    return *source_mapper_;
  }

  [[nodiscard]] auto Sensitivity() const -> SensitivityAnalyzer& {
    return *sensitivity_analyzer_;
  }

  [[nodiscard]] auto DisableAssertions() const -> bool {
    return disable_assertions_;
  }

 private:
  const frontend::SlangSourceMapper* source_mapper_;
  SensitivityAnalyzer* sensitivity_analyzer_;
  bool disable_assertions_;
};

// Per-unit lowerer, over a module instance body or a package. It holds the
// in-progress compilation unit together with the registries that record which
// HIR identity this unit gave each slang symbol, so a reference resolves
// against what the unit already decided rather than by re-reading the frontend.
//
// The unit is constructed with its name and an initial builtins table,
// populated by `Run`, and moved out by `Run`'s return; afterwards the lowerer
// holds no IR.
class UnitLowerer {
 public:
  UnitLowerer(
      const LoweringFacts& facts, const slang::ast::Scope& scope,
      std::string name);

  auto Run() -> diag::Result<hir::CompilationUnit>;

  // Read access to the in-progress unit. Handlers reach the unit's type vocab
  // and builtins through this accessor; downstream consumers post-Run use the
  // same `hir::CompilationUnit` interface.
  [[nodiscard]] auto Unit() const -> const hir::CompilationUnit& {
    return unit_;
  }

  // The slang scope this unit is lowered from -- a module instance body or a
  // package. A constant expression the lowering must evaluate itself (an LRM
  // 20.7 dimension index) is evaluated against its symbol.
  [[nodiscard]] auto SourceScope() const -> const slang::ast::Scope& {
    return *scope_;
  }

  // Lowers a slang type to a HIR TypeId, memoizing by slang canonical pointer.
  // The slang-keyed cache and the unit's type table are coordinated together:
  // the dedup invariant (same slang canonical -> same HIR TypeId) is enforced
  // structurally here, so callers cannot bypass it by writing to the unit
  // directly.
  auto InternType(const slang::ast::Type& type, diag::SourceSpan span)
      -> diag::Result<hir::TypeId>;

  // Adds a type the lowering itself composes rather than reads off the
  // frontend -- the array of a type's per-dimension query results that an LRM
  // 20.7 query selects from when its dimension is named at run time. There is
  // no frontend type to key the cache on, so it is added directly.
  auto AddComposedType(hir::TypeData data) -> hir::TypeId;

  // Mints a class of this unit into the unit's class registry: allocates the
  // `ClassId`, populates the shape, and returns the id. The caller carries the
  // proof that the class belongs to this unit -- the pre-pass walks this
  // unit's own scope, and a same-unit base link resolves through the reference
  // translator before flowing back here -- so no parent-chain query runs. A
  // repeat call for the same class is idempotent: the cache entry the first
  // mint installed returns without re-work, which admits mutual reference
  // during body population.
  auto InternLocalClass(const slang::ast::ClassType& cls, diag::SourceSpan span)
      -> diag::Result<hir::ClassId>;

  // Translates a slang class pointer -- reached from an expression site or a
  // base-class link -- into HIR's owner-qualified reference form. The result
  // is a `LocalClassRef` when the class is declared by this unit, an
  // `ExternalClassRef{unit_name, class_name}` when it is declared by another.
  // Classification runs at most once per class per unit: the first encounter
  // walks `cls.getParentScope()` up to the enclosing compilation unit and
  // caches the answer; every later encounter reads it. A local class not yet
  // interned is minted lazily on this path, so a body reads the same identity
  // regardless of which route saw the class first. This is the sole
  // AST-to-HIR site that walks slang's parent chain to answer "which CU
  // declares this class?" -- the top-down mint path is walk-free by design.
  auto ResolveClassRef(const slang::ast::ClassType& cls, diag::SourceSpan span)
      -> diag::Result<hir::ClassRef>;

  // Builds a class-method target from a class reference and the resolved
  // callee symbol. Local when the class was interned by this unit -- the
  // method's arena position is queryable through the `SubroutineSymbol`-keyed
  // cache the interning populated. External when the class lives in another
  // compilation unit -- the method is named by (declaring unit, class
  // canonical name, method name), the same by-name form a cross-unit
  // reference to any other class member uses.
  [[nodiscard]] auto MakeClassMethodTarget(
      const hir::ClassRef& class_ref,
      const slang::ast::SubroutineSymbol& method) const
      -> hir::ClassMethodTarget;

  // The instance-property peer of `MakeClassMethodTarget`. Local when the
  // class was interned by this unit; external when the class lives in another
  // compilation unit, in which case the property is named by its source name.
  [[nodiscard]] auto MakeClassPropertyTarget(
      const hir::ClassRef& class_ref,
      const slang::ast::ClassPropertySymbol& prop) const
      -> hir::ClassPropertyTarget;

  // The static-property peer of `MakeClassMethodTarget`. Local when the class
  // was interned by this unit; external when the class lives in another
  // compilation unit, in which case the property is named by its source name.
  [[nodiscard]] auto MakeStaticPropertyTarget(
      const hir::ClassRef& class_ref,
      const slang::ast::ClassPropertySymbol& prop) const
      -> hir::StaticPropertyTarget;

  // Records a frontend method symbol's HIR arena identity as the class
  // interning that owns it adds the method. Downstream consumers translate a
  // frontend symbol into the HIR-side identity through this cache, so the
  // slang enumeration order is walked once (at class interning) and never
  // again at a resolution site.
  void RegisterMethodId(
      const slang::ast::SubroutineSymbol& method, hir::MethodId id) {
    method_cache_.emplace(&method, id);
  }

  [[nodiscard]] auto LookupMethodId(
      const slang::ast::SubroutineSymbol& method) const -> hir::MethodId {
    if (const auto it = method_cache_.find(&method);
        it != method_cache_.end()) {
      return it->second;
    }
    throw InternalError(
        "UnitLowerer::LookupMethodId: method has no HIR identity; the "
        "owning class was not interned before this lookup");
  }

  // Records the HIR `FieldId` a class property received when the owning
  // class was minted. A downstream `handle.field` access reads the id in
  // O(1) through this lookup instead of re-walking the property list at
  // every reference site.
  void RegisterClassPropertyFieldId(
      const slang::ast::ClassPropertySymbol& prop, hir::FieldId id) {
    class_property_field_ids_.emplace(&prop, id);
  }

  [[nodiscard]] auto LookupClassPropertyFieldId(
      const slang::ast::ClassPropertySymbol& prop) const -> hir::FieldId {
    if (const auto it = class_property_field_ids_.find(&prop);
        it != class_property_field_ids_.end()) {
      return it->second;
    }
    throw InternalError(
        "UnitLowerer::LookupClassPropertyFieldId: property has no recorded "
        "id; the owning class was not interned before this lookup");
  }

  // Records the HIR `StaticPropertyId` a static class property (LRM 8.9)
  // received when the owning class was minted, so a downstream `Cls::prop`
  // or `handle.prop` (static-lifetime) access reads the id in O(1) rather
  // than re-walking the arena. The type-associated storage counterpart to
  // the instance-property registry above.
  void RegisterClassPropertyStaticId(
      const slang::ast::ClassPropertySymbol& prop, hir::StaticPropertyId id) {
    class_property_static_ids_.emplace(&prop, id);
  }

  [[nodiscard]] auto LookupClassPropertyStaticId(
      const slang::ast::ClassPropertySymbol& prop) const
      -> hir::StaticPropertyId {
    if (const auto it = class_property_static_ids_.find(&prop);
        it != class_property_static_ids_.end()) {
      return it->second;
    }
    throw InternalError(
        "UnitLowerer::LookupClassPropertyStaticId: static property has no "
        "recorded id; the owning class was not interned before this lookup");
  }

  [[nodiscard]] auto SourceMapper() const
      -> const frontend::SlangSourceMapper& {
    return facts_.SourceMapper();
  }
  [[nodiscard]] auto Sensitivity() const -> SensitivityAnalyzer& {
    return facts_.Sensitivity();
  }
  [[nodiscard]] auto DisableAssertions() const -> bool {
    return facts_.DisableAssertions();
  }

  void MapStructuralDataObjectBinding(
      const slang::ast::ValueSymbol& var, ScopeFrameId home_frame,
      hir::StructuralDataObjectId local, hir::TypeId type);
  [[nodiscard]] auto LookupStructuralDataObjectBinding(
      const slang::ast::ValueSymbol& var) const
      -> std::optional<StructuralDataObjectBinding>;

  void MapSubroutineBinding(
      const slang::ast::SubroutineSymbol& sym, ScopeFrameId owner_frame,
      hir::StructuralSubroutineId local);
  [[nodiscard]] auto LookupSubroutineBinding(
      const slang::ast::SubroutineSymbol& sym) const
      -> std::optional<SubroutineBinding>;

  // Interns this unit's record of a DPI-C import (LRM 35.4), classifying its
  // ABI projection on first sight and answering with the same id every later
  // time. Both the declaration walk and a call site reach an import through
  // here, so a unit that calls an import declared elsewhere holds an entry
  // identical to the declaring unit's, classified from the same declaration.
  auto EnsureForeignImport(const slang::ast::SubroutineSymbol& sym)
      -> diag::Result<hir::ForeignImportId>;

  void MapForeignImportScope(
      const slang::ast::SubroutineSymbol& sym, ScopeFrameId declaring_frame);
  [[nodiscard]] auto LookupForeignImportScope(
      const slang::ast::SubroutineSymbol& sym) const
      -> std::optional<ScopeFrameId>;

  // Pattern-bound identifiers (LRM 12.6). The declaration is the
  // `VariablePattern` node itself, so a reference resolves to that node's
  // `PatternId`. The map lives on the unit rather than on either pass class
  // because a pattern reads the same in a procedural body and in a structural
  // expression, and neither owns a declaration arena for it.
  void MapPatternVar(
      const slang::ast::PatternVarSymbol& sym, hir::PatternId pattern);
  [[nodiscard]] auto LookupPatternVar(const slang::ast::PatternVarSymbol& sym)
      const -> std::optional<hir::PatternId>;

  void MapOwnedChildBinding(
      const slang::ast::Symbol& child, ScopeFrameId home_frame,
      hir::DownwardHead head);
  [[nodiscard]] auto LookupOwnedChildBinding(const slang::ast::Symbol& child)
      const -> std::optional<OwnedChildBinding>;

  // Routed reference dedup. `slot_owner_frame` is the frame whose `routed_refs`
  // arena holds the slot -- the scope whose MIR class receives the endpoint
  // member and whose resolve body installs it. For an intra-unit reference that
  // reaches an enclosing ancestor or a sibling-of-ancestor head the slot owner
  // is the referrer's frame while the head lives in an enclosing frame; for a
  // downward head in the referrer's own scope the slot owner is also the head's
  // owner.
  auto MapOrGetRoutedRef(
      const slang::ast::ValueSymbol& target, ScopeFrameId slot_owner_frame,
      hir::RoutedRefHead head, std::vector<hir::PathSegment> path,
      hir::TypeId type) -> hir::RoutedRefId;
  auto TakeRoutedRefsForFrame(ScopeFrameId slot_owner_frame)
      -> std::vector<hir::RoutedRefDecl>;

  // The compilation-unit structural declaration pass (LRM 27, 23.6): before any
  // executable body lowers, walk the whole unit's scope tree, assign each
  // addressable scope its frame, and register every owned-child head (instance,
  // generate block, generate array). A body or sensitivity read resolves an
  // owned child regardless of which sibling scope lowered first. Registers no
  // executable HIR.
  void DeclareStructuralIdentities(const slang::ast::Scope& scope);

  // The frame assigned to `scope` by the declaration pass. Every scope a
  // structural lowerer is built for was assigned one, so absence is a
  // compiler-bug invariant.
  [[nodiscard]] auto LookupScopeFrame(const slang::ast::Scope& scope) const
      -> ScopeFrameId;

  // Frame minting for scope entry.
  [[nodiscard]] auto NextScopeFrameId() -> ScopeFrameId;

  // Identity minting for an array-method `with` clause (LRM 7.12). Unique
  // within the unit, so HIR-to-MIR can key its iteration-binding registry on
  // it.
  [[nodiscard]] auto NextWithClauseId() -> hir::WithClauseId;

  // Interns every class this unit declares -- a non-parameterized class as a
  // single entry, and a parameterized class as one entry per live
  // specialization slang deduplicated during elaboration. Runs before any
  // body lowering so the unit's class registry is complete before any
  // reference resolves; a specialization reached only from another unit
  // still lands here, in its declaring unit.
  auto InternOwnClassDeclarations() -> diag::Result<void>;

  // Builds a HIR Expr referring to a leaf reached by navigating `path` down
  // from `head`. `target` is the leaf value symbol, which is also the key two
  // references to one target dedup on; `slot_owner_frame` is the frame whose
  // routed-reference arena holds the slot.
  auto MakeRoutedMemberRef(
      const slang::ast::ValueSymbol& target, ScopeFrameId slot_owner_frame,
      hir::RoutedRefHead head, std::vector<hir::PathSegment> path,
      hir::TypeId type, diag::SourceSpan span) -> hir::Expr;

  // Where a named value lives, as this unit reaches it. One answer serves
  // every consumer of a reference -- reading it, writing it, and waiting on it
  // changing. A variable or net resolves to a route from the reader to its
  // cell, sealed once at elaboration; a variable of a namespace unit, which has
  // no instance to route through, resolves to its name across the boundary. A
  // constant resolves to nothing at all -- it names a value, not a cell, so
  // there is nothing to read through and nothing to wait on -- as does a target
  // whose route form is not yet supported. Asking here, once, is what stops one
  // symbol being a value to one consumer and a signal to another.
  [[nodiscard]] auto ResolveValueTarget(
      const WalkFrame& frame, const slang::ast::ValueSymbol& value)
      -> diag::Result<std::optional<hir::ValueTarget>>;

  // The reads of a dependency set that name a cell, as the entries that wake on
  // it under `edge`. A read of anything else contributes none, so a constant
  // read alongside a signal leaves only the signal subscribed (LRM 9.2.2.2.1).
  // An inferred sensitivity wakes on any change; only an explicit event control
  // qualifies its terms with a polarity (LRM 9.4.2), and every leaf of one term
  // carries that term's own.
  [[nodiscard]] auto TranslateSensitivityReads(
      const std::vector<SensitivityRead>& reads, const WalkFrame& frame,
      support::EventEdge edge)
      -> diag::Result<std::vector<hir::SensitivityEntry>>;

 private:
  // The reader-relative route to a cell in an instantiated scope: a direct
  // member when the target sits on the reader's own scope, a routed reference
  // otherwise -- a typed enclosing climb to a this-unit ancestor member, a
  // typed downward head when this unit emits the head's class, or a by-name
  // head where the route crosses into another instance's unit.
  [[nodiscard]] auto TranslateReferenceRoute(
      const WalkFrame& frame, const slang::ast::ValueSymbol& value)
      -> std::optional<hir::ReferenceRoute>;

  LoweringFacts facts_;
  const slang::ast::Scope* scope_;

  hir::CompilationUnit unit_;

  std::unordered_map<const slang::ast::Type*, hir::TypeId> type_cache_;
  // The classification of every class this unit's lowering has resolved: the
  // Local / External arm chosen by walking slang's parent chain to find the
  // class's declaring compilation unit. Caching the classification -- not just
  // the local id -- lets an external class's parent-chain walk run once per
  // class per unit; a second reference to the same slang `ClassType` reads
  // the answer, whether it is Local or External.
  std::unordered_map<const slang::ast::ClassType*, hir::ClassRef> class_cache_;
  std::unordered_map<const slang::ast::SubroutineSymbol*, hir::MethodId>
      method_cache_;
  std::unordered_map<const slang::ast::ClassPropertySymbol*, hir::FieldId>
      class_property_field_ids_;
  std::unordered_map<
      const slang::ast::ClassPropertySymbol*, hir::StaticPropertyId>
      class_property_static_ids_;
  StructuralDataObjectBindings structural_data_object_bindings_;
  SubroutineBindings subroutine_bindings_;
  ForeignImportBindings foreign_import_bindings_;
  ForeignImportScopes foreign_import_scopes_;
  OwnedChildBindings owned_child_bindings_;
  std::unordered_map<const slang::ast::PatternVarSymbol*, hir::PatternId>
      pattern_var_bindings_;
  std::unordered_map<const slang::ast::Scope*, ScopeFrameId> scope_frames_;
  // Dedup by (home_frame, target): the slot id is an index within a scope's
  // own `routed_refs`, so two scopes referencing the same member each need
  // their own slot.
  std::map<
      ScopeFrameId,
      std::unordered_map<const slang::ast::ValueSymbol*, hir::RoutedRefId>>
      routed_ref_dedup_;
  std::map<ScopeFrameId, std::vector<hir::RoutedRefDecl>> routed_refs_by_frame_;
  std::uint32_t next_scope_frame_ = 0;
  std::uint32_t next_with_clause_ = 0;
};

}  // namespace lyra::lowering::ast_to_hir
