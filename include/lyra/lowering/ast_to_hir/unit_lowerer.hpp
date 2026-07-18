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
#include "lyra/hir/compilation_unit.hpp"
#include "lyra/hir/expr.hpp"
#include "lyra/hir/field_id.hpp"
#include "lyra/hir/method_id.hpp"
#include "lyra/hir/structural_data_object.hpp"
#include "lyra/hir/structural_scope.hpp"
#include "lyra/lowering/ast_to_hir/sensitivity.hpp"
#include "lyra/lowering/ast_to_hir/walk_frame.hpp"

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

// A DPI-C import call resolves to the scope that declares the import, reached
// through `owner_frame`, and to the import's `foreign_imports` id. A separate
// binding space from SubroutineBinding: an import is a bodyless external
// callable, not a body-bearing structural subroutine.
struct ForeignImportBinding {
  ScopeFrameId owner_frame{};
  hir::ForeignImportId import_id{};
};

using ForeignImportBindings = std::unordered_map<
    const slang::ast::SubroutineSymbol*, ForeignImportBinding>;

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

// Per-unit lowerer, over a module instance body or a package.
//
// Facts: the shared lowering facts (source mapper + sensitivity analyzer) and
// a reference to the slang scope being walked.
// Registries: type cache (slang canonical type -> hir TypeId), structural-var
// / subroutine / loop-var / owned-child binding tables, per-frame cross-unit
// ref slot tables, and a scope-frame counter.
// Root output: the in-progress `hir::CompilationUnit`. Constructed in the ctor
// with the unit's name and an initial builtins table; populated by `Run`; moved
// out by `Run`'s return. After `Run` returns the class holds no IR pointer.
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

  // Registers a class declaration on first encounter and returns its id,
  // memoizing by slang class pointer. The id is declared before the body is
  // built so a property whose type names the same class (a self-reference)
  // resolves against a stable identity.
  auto InternClass(const slang::ast::ClassType& cls, diag::SourceSpan span)
      -> diag::Result<hir::ClassId>;

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

  // Records the HIR `FieldId` a class property received when its owning
  // class's `InternClass` added it to the field arena. A downstream
  // `handle.field` access reads the id in O(1) through this lookup instead
  // of re-walking the property list at every reference site.
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

  // Peer of the field-id registry on the type-associated axis. Records the
  // HIR `StaticPropertyId` a static class property (LRM 8.9) received when
  // its owning class's `InternClass` added it to the static-property arena,
  // so a downstream `Cls::prop` or `handle.prop` (static-lifetime) access
  // reads the id in O(1) rather than re-walking the arena.
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

  // Facts.
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

  // Binding registries. Each Map* asserts no double-mapping for the same
  // slang symbol.
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

  void MapForeignImportBinding(
      const slang::ast::SubroutineSymbol& sym, ScopeFrameId owner_frame,
      hir::ForeignImportId import_id);
  [[nodiscard]] auto LookupForeignImportBinding(
      const slang::ast::SubroutineSymbol& sym) const
      -> std::optional<ForeignImportBinding>;

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

  // Builds a HIR Expr referring to a leaf reached by navigating `path` down
  // from `head`. `target` is the leaf value symbol (routed-ref dedup key);
  // `slot_owner_frame` is the frame whose `routed_refs` arena holds the slot
  // (see `MapOrGetRoutedRef`).
  auto MakeRoutedMemberRef(
      const slang::ast::ValueSymbol& target, ScopeFrameId slot_owner_frame,
      hir::RoutedRefHead head, std::vector<hir::PathSegment> path,
      hir::TypeId type, diag::SourceSpan span) -> hir::Expr;

  // Translates slang-side reads to HIR SensitivityEntries. Each read resolves
  // to the same reader-relative route the reader would use to reach the target,
  // derived from the target's elaborated position -- not reclassified from a
  // global table lookup.
  [[nodiscard]] auto TranslateSensitivityReads(
      const std::vector<SensitivityRead>& reads, const WalkFrame& frame)
      -> std::vector<hir::SensitivityEntry>;

  // The one route translator for every reference consumer -- value read, value
  // write, and change observation. From the reader's elaborated position
  // (`frame.reader_scope` and its unit-local chain) and the target symbol it
  // computes the reader-relative route and classifies each segment by layout
  // visibility: a direct member when the target sits on the reader's own scope,
  // a routed reference otherwise -- a typed enclosing climb to a this-unit
  // ancestor member, a typed downward head when the head's class this unit
  // emits, or a by-name head where the route crosses the compilation-unit
  // boundary. Returns nullopt when the target is not an addressable signal or
  // its form is not yet supported.
  [[nodiscard]] auto TranslateReferenceRoute(
      const WalkFrame& frame, const slang::ast::ValueSymbol& value)
      -> std::optional<hir::ReferenceRoute>;

 private:
  // Facts.
  LoweringFacts facts_;
  const slang::ast::Scope* scope_;

  // Root output.
  hir::CompilationUnit unit_;

  // Registries.
  std::unordered_map<const slang::ast::Type*, hir::TypeId> type_cache_;
  std::unordered_map<const slang::ast::ClassType*, hir::ClassId> class_cache_;
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
  OwnedChildBindings owned_child_bindings_;
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
