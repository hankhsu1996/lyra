#pragma once

#include <cstdint>
#include <map>
#include <optional>
#include <span>
#include <string>
#include <string_view>
#include <unordered_map>
#include <vector>

#include <slang/ast/symbols/BlockSymbols.h>
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
#include "lyra/hir/type_import.hpp"
#include "lyra/hir/unit_signature.hpp"
#include "lyra/hir/unit_signatures.hpp"
#include "lyra/hir/value_ref.hpp"
#include "lyra/lowering/ast_to_hir/sensitivity.hpp"
#include "lyra/lowering/ast_to_hir/walk_frame.hpp"
#include "lyra/support/event_edge.hpp"

namespace slang::ast {
class Expression;
class ClassType;
class InterfacePortSymbol;
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

// Where an interface port stands, as its own unit reaches it: the scope that
// declares it, its identity there, and this unit's record of the object bound
// to it. A name reached through the port is counted out of that record.
struct InterfacePortBinding {
  ScopeFrameId home_frame{};
  hir::InterfacePortId port{};
  hir::ExternalUnitObjectId object{};
};

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

// The program-global C name each exported subroutine is reached by (LRM 35.5).
// An `export "DPI-C"` is a directive rather than a member symbol, so a scope
// walk never encounters one and the subroutine it names carries no mark of its
// own; the frontend resolves each directive against the scope declaring it, and
// this is that resolution keyed by the subroutine it resolved to. It spans the
// design because directives resolve once, after every scope has elaborated.
using ForeignExportNames =
    std::unordered_map<const slang::ast::SubroutineSymbol*, std::string_view>;

// A component of a hierarchical path names an owned child of some scope this
// unit declares: an instance / instance-array member (`c.x`, `c[1].x`), or a
// generate block (`g[1].x`, LRM 27). The child's slang symbol maps to the
// declaring scope's identity for it, so the reference resolves regardless of
// whether it precedes the child in source.
struct OwnedChildBinding {
  ScopeFrameId home_frame{};
  hir::OwnedChildRef child;
};

using OwnedChildBindings =
    std::unordered_map<const slang::ast::Symbol*, OwnedChildBinding>;

// A static-lifetime body local that a hierarchical path can name (LRM 23.9).
// The declaration pass mints it, so a reference from a peer body resolves it
// whichever body lowers first; `home_frame` is the structural scope whose
// object tree carries the storage. Where inside that scope the storage sits --
// which named blocks stand between -- is settled when the scope's classes are
// built, so the reference names only the declaration.
struct ProceduralStaticBinding {
  ScopeFrameId home_frame{};
  hir::ProceduralBodyRef body;
  hir::ProceduralVarId var{};
};

using ProceduralStaticBindings =
    std::unordered_map<const slang::ast::Symbol*, ProceduralStaticBinding>;

// What a route ends at, and what storage that is. A published member states its
// own type and its own storage on the signature that carries it; every other
// target is described the way the referrer already knows it.
struct RouteTarget {
  hir::RouteLeaf leaf;
  hir::TypeId type;
  hir::PublishedStorage storage;
};

// How a reader reaches a scope elsewhere on the elaborated hierarchy: where
// navigation starts, and the descent from there. What the route ends at is not
// part of it, so one walk serves both a reference to storage some scope holds
// and a connection naming the scope itself.
struct ScopeRoute {
  hir::RouteHead head;
  std::vector<hir::PathStep> steps;
};

// The declarations of one structural scope that a peer may name before the
// scope is built, minted here and handed to the scope when it is.
struct ScopeDeclarations {
  base::Registry<hir::SubroutineDecl, hir::StructuralSubroutineId>
      structural_subroutines;
  base::Registry<hir::Process, hir::ProcessId> processes;
  base::Registry<hir::Generate, hir::GenerateId> generates;
  base::Registry<hir::InstanceMemberDecl, hir::InstanceMemberId>
      instance_members;
};

// What every unit's lowering reads and none of them changes: where a
// construct was written, the shared sensitivity analysis (one cache across the
// design), whether assertions are elided rather than rejected, and the foreign
// export names, which are resolved design-wide before any unit is walked.
// The slang compilation itself is deliberately absent: walking top instances is
// the driver's job, and a unit lowering that could reach it would be able to
// read another unit.
class LoweringFacts {
 public:
  LoweringFacts(
      const frontend::SlangSourceMapper& source_mapper,
      SensitivityAnalyzer& sensitivity_analyzer,
      const ForeignExportNames& foreign_export_names, bool disable_assertions)
      : source_mapper_(&source_mapper),
        sensitivity_analyzer_(&sensitivity_analyzer),
        foreign_export_names_(&foreign_export_names),
        disable_assertions_(disable_assertions) {
  }

  [[nodiscard]] auto SourceMapper() const
      -> const frontend::SlangSourceMapper& {
    return *source_mapper_;
  }

  [[nodiscard]] auto Sensitivity() const -> SensitivityAnalyzer& {
    return *sensitivity_analyzer_;
  }

  // The C name `sub` is exported under (LRM 35.5), or nullopt when no `export
  // "DPI-C"` names it.
  [[nodiscard]] auto ForeignExportName(const slang::ast::SubroutineSymbol& sub)
      const -> std::optional<std::string_view> {
    const auto it = foreign_export_names_->find(&sub);
    if (it == foreign_export_names_->end()) {
      return std::nullopt;
    }
    return it->second;
  }

  [[nodiscard]] auto DisableAssertions() const -> bool {
    return disable_assertions_;
  }

 private:
  const frontend::SlangSourceMapper* source_mapper_;
  SensitivityAnalyzer* sensitivity_analyzer_;
  const ForeignExportNames* foreign_export_names_;
  bool disable_assertions_;
};

// Per-unit lowerer, over a module instance body or a package. It holds the
// in-progress compilation unit together with the registries that record which
// HIR identity this unit gave each slang symbol, so a reference resolves
// against what the unit already decided rather than by re-reading the frontend.
//
// A unit lowers in two phases, and every unit completes the first before any
// unit begins the second. `Declare` reads this unit's own declarations and
// nothing else, which is what lets the design run it for every unit in any
// order; `LowerBodies` lowers what executes, against the signatures the first
// phase produced. The compilation unit is populated across both and moved out
// by the second's return; afterwards the lowerer holds no IR.
class UnitLowerer {
 public:
  UnitLowerer(
      const LoweringFacts& facts, const slang::ast::Scope& scope,
      std::string name, hir::UnitRole role);

  // The declaration phase: everything this unit states about itself, including
  // the signature it publishes. Reads no other unit, so a body that later
  // references one cannot observe whether it had been declared yet.
  auto Declare() -> diag::Result<void>;

  // What this unit publishes, moved out once its declaration phase has run.
  // The design collects these before any body lowers and hands each unit the
  // ones it may read.
  [[nodiscard]] auto TakeSignature() -> hir::UnitSignature;

  // The units this one's declarations name, each once. The design narrows the
  // signatures it hands this unit to these, so a unit's own declarations fix
  // what its bodies can read about any other.
  [[nodiscard]] auto ReferencedUnits() const -> std::span<const std::string> {
    return referenced_units_;
  }

  // The body phase: everything this unit executes, resolved against what the
  // units it references published.
  auto LowerBodies(hir::ConsumedSignatures signatures)
      -> diag::Result<hir::CompilationUnit>;

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

  // Lowers a slang type to a HIR TypeId. Identity is the pool's and is
  // structural, so what the frontend-keyed memo here adds is only a shortcut
  // past the translation work for a type already translated; two frontend
  // spellings of one type reach the same id whether or not either took it.
  auto InternType(const slang::ast::Type& type, diag::SourceSpan span)
      -> diag::Result<hir::TypeId>;

  // Adds a type the lowering itself composes rather than reads off the
  // frontend -- the array of a type's per-dimension query results that an LRM
  // 20.7 query selects from when its dimension is named at run time. There is
  // no frontend type to take the shortcut on, so it goes straight to the pool.
  auto AddComposedType(hir::Type type) const -> hir::TypeId;

  // Takes a type another unit published into this unit's own pool, and answers
  // with the identity this unit knows it by. The published pool's identities
  // index storage that unit carries, so what crosses is the type's structure,
  // re-identified here; a type taken twice out of one signature is taken once.
  auto ImportSignatureType(
      const hir::UnitSignature& signature, hir::TypeId published)
      -> hir::TypeId;

  // What the units this one references published, for a lowering that reaches
  // across the unit boundary. Reachable only once bodies lower: the declaration
  // phase reads this unit alone, so it has none.
  [[nodiscard]] auto Signatures() const -> const hir::ConsumedSignatures& {
    if (!consumed_signatures_.has_value()) {
      throw InternalError(
          "UnitLowerer::Signatures: another unit's signature is reachable only "
          "while bodies lower; the declaration phase reads this unit alone");
    }
    return *consumed_signatures_;
  }

  // This unit's record of the object an instance of `unit_name` is, taken from
  // that unit's signature the first time one is reached and answered with the
  // same identity every later time. Reaching another unit's object is what
  // declares the dependency on it, so its signature is in hand here.
  auto ExternalUnitObjectOf(const std::string& unit_name)
      -> hir::ExternalUnitObjectId;

  // Which storage the declaration `value` holds. One answer, so what this unit
  // publishes about a declaration and what a route to it reaches cannot differ.
  [[nodiscard]] auto DeclarationStorage(
      const slang::ast::ValueSymbol& value, diag::SourceSpan span) const
      -> diag::Result<hir::PublishedStorage>;

  // Which unit's instances the interface port `port` carries. The connection
  // decides it and the connection is read where this unit's ports are, so the
  // answer is taken there and read back here rather than reached for a second
  // time -- what the unit published about the port and the member it builds for
  // it then cannot describe different interfaces.
  [[nodiscard]] auto InterfaceUnitOf(const slang::ast::Symbol& port) const
      -> const std::string& {
    const auto it = interface_port_units_.find(&port);
    if (it == interface_port_units_.end()) {
      throw InternalError(
          "UnitLowerer::InterfaceUnitOf: a unit publishes every interface port "
          "it declares before any of its bodies lower");
    }
    return it->second;
  }

  // Whether `internal` is the declaration a `ref` / `const ref` port reaches,
  // and under which binding (LRM 23.3.3.2). The port's direction decides it, so
  // this is answered where this unit's ports are read.
  [[nodiscard]] auto ReferenceBindingOf(const slang::ast::Symbol& internal)
      const -> std::optional<hir::ReferenceBinding> {
    const auto it = ref_port_internals_.find(&internal);
    return it == ref_port_internals_.end() ? std::nullopt
                                           : std::optional{it->second};
  }

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

  // The method a call reaches. Intra-unit that is the slot alone, since the
  // declaration it resolves to answers everything else about the callee;
  // cross-unit there is no such declaration to reach, so the callee also
  // carries its dispatch role (LRM 8.20) and the interface a call marshals
  // against (LRM 13.5).
  auto MakeMethodCallee(
      const hir::ClassRef& class_ref,
      const slang::ast::SubroutineSymbol& method, diag::SourceSpan span)
      -> diag::Result<hir::MethodCallee>;

  // The interface a call recomputes for a callee in another compilation unit:
  // its call protocol and each formal's direction and type (LRM 13.5). Both
  // sides derive it from the callee's own declaration, so no table is shared
  // and neither can state an interface the other does not have.
  auto MakeExternalCalleeInterface(
      const slang::ast::SubroutineSymbol& sym, diag::SourceSpan span)
      -> diag::Result<hir::ExternalCalleeInterface>;

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
  [[nodiscard]] auto ForeignExportName(const slang::ast::SubroutineSymbol& sub)
      const -> std::optional<std::string_view> {
    return facts_.ForeignExportName(sub);
  }
  // Whether the design being built contains this procedural block. A concurrent
  // assertion is a process whose whole body is the assertion, so disabling
  // assertions removes it rather than emptying it -- an always block with no
  // body and no timing control would be a zero-delay infinite loop. What a
  // design contains is one answer, so every pass that enumerates processes
  // reads it here rather than restating the condition.
  [[nodiscard]] auto Contains(
      const slang::ast::ProceduralBlockSymbol& proc) const -> bool {
    return !DisableAssertions() || !proc.isFromAssertion;
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

  void MapInterfacePortBinding(
      const slang::ast::InterfacePortSymbol& port, ScopeFrameId home_frame,
      hir::InterfacePortId local, hir::ExternalUnitObjectId object);
  [[nodiscard]] auto LookupInterfacePortBinding(const slang::ast::Symbol& port)
      const -> std::optional<InterfacePortBinding>;

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
      hir::OwnedChildRef child_ref);
  [[nodiscard]] auto LookupOwnedChildBinding(const slang::ast::Symbol& child)
      const -> std::optional<OwnedChildBinding>;

  void MapProcessBinding(
      const slang::ast::ProceduralBlockSymbol& proc, hir::ProcessId id);
  [[nodiscard]] auto LookupProcessBinding(
      const slang::ast::ProceduralBlockSymbol& proc) const
      -> std::optional<hir::ProcessId>;

  [[nodiscard]] auto LookupProceduralStatic(const slang::ast::Symbol& var) const
      -> std::optional<ProceduralStaticBinding>;

  // Opens the body of a process or subroutine with the static-lifetime locals
  // the compilation unit's declaration pass minted for it already in place. A
  // peer body that lowered earlier may already name one of those ids in a
  // hierarchical reference, so this body cannot assign its own: the minted ids
  // have to occupy the leading arena slots. Producing the body already holding
  // them, rather than filling one afterwards, is what leaves no window in
  // which anything else could take those slots.
  [[nodiscard]] auto MakeProceduralBody(const slang::ast::Symbol& body_symbol)
      -> hir::ProceduralBody;

  // Hands a structural scope the declarations minted for it, once.
  [[nodiscard]] auto TakeScopeDeclarations(const slang::ast::Scope& scope)
      -> ScopeDeclarations;

  // Routed reference dedup. `slot_owner_frame` is the frame whose `routed_refs`
  // arena holds the slot -- the scope whose MIR class receives the endpoint
  // member and whose resolve body installs it. For an intra-unit reference that
  // reaches an enclosing ancestor or a sibling-of-ancestor head the slot owner
  // is the referrer's frame while the head lives in an enclosing frame; for a
  // downward head in the referrer's own scope the slot owner is also the head's
  // owner.
  auto MapOrGetRoutedRef(
      const slang::ast::ValueSymbol& target, ScopeFrameId slot_owner_frame,
      hir::RoutedRefDecl decl) -> hir::RoutedRefId;
  auto TakeRoutedRefsForFrame(ScopeFrameId slot_owner_frame)
      -> std::vector<hir::RoutedRefDecl>;

  // The compilation-unit declaration pass (LRM 23.6 / 23.9 / 27): before any
  // executable body lowers, walk the whole unit's scope tree and mint every
  // declaration a peer body may reference -- owned children (instance,
  // generate block, generate array), subroutines, and the static-lifetime body
  // locals a named block puts on the hierarchical path -- assigning each scope
  // its frame along the way. A body or sensitivity read then resolves any of
  // them regardless of which sibling scope or body lowered first. Registers no
  // executable HIR.
  auto DeclareStructuralIdentities(const slang::ast::Scope& scope)
      -> diag::Result<void>;

  // Records that this unit's declarations name `unit_name`. Several instances
  // may be built from one unit, and the dependency is on the unit rather than
  // on any one of them, so a repeat contributes nothing.
  void RecordReferencedUnit(std::string unit_name);

  // The frame assigned to `scope` by the declaration pass. Every scope a
  // structural lowerer is built for was assigned one, so absence is a
  // compiler-bug invariant.
  [[nodiscard]] auto LookupScopeFrame(const slang::ast::Scope& scope) const
      -> ScopeFrameId;

  // Records the identity a declaration scope minted for one of its procedural
  // scopes, keyed by the symbol slang records the scope as.
  void DeclareProceduralScope(
      const slang::ast::Symbol& symbol, hir::ProceduralScopeId scope) {
    procedural_scopes_.emplace(&symbol, scope);
  }

  // The identity minted for `symbol`'s procedural scope. Every procedural scope
  // the source wrote is minted before any of its declaration scope's bodies
  // lower, so absence is a compiler-bug invariant -- which makes this a lookup
  // for a symbol the declaration scope being lowered is known to own, never a
  // test of whether it owns one.
  [[nodiscard]] auto LookupProceduralScope(
      const slang::ast::Symbol& symbol) const -> hir::ProceduralScopeId {
    const auto it = procedural_scopes_.find(&symbol);
    if (it == procedural_scopes_.end()) {
      throw InternalError(
          "UnitLowerer::LookupProceduralScope: a procedural scope was not "
          "declared before its body lowered");
    }
    return it->second;
  }

  // The structural scope `symbol` belongs to -- the nearest enclosing slang
  // scope the declaration pass minted a frame for -- or nullopt when the symbol
  // sits outside any (a class member). A `disable` compares this against its
  // own frame to tell a same-scope target from a cross-scope one, which needs
  // the hierarchical addressing that is not yet built.
  [[nodiscard]] auto OwningScopeFrame(const slang::ast::Symbol& symbol) const
      -> std::optional<ScopeFrameId> {
    for (const slang::ast::Scope* s = symbol.getParentScope(); s != nullptr;
         s = s->asSymbol().getParentScope()) {
      if (const auto it = scope_frames_.find(s); it != scope_frames_.end()) {
        return it->second;
      }
    }
    return std::nullopt;
  }

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

  // Builds a HIR Expr referring to the leaf `decl` navigates to. `target` is
  // the leaf value symbol, which is also the key two references to one target
  // dedup on; `slot_owner_frame` is the frame whose routed-reference arena
  // holds the slot.
  auto MakeRoutedMemberRef(
      const slang::ast::ValueSymbol& target, ScopeFrameId slot_owner_frame,
      hir::RoutedRefDecl decl, diag::SourceSpan span) -> hir::Expr;

  // The reference to `value` over a route the caller derived: `head` and
  // `steps` say how the reader reaches it, and what the route ends at follows
  // from the steps alone. A reader that can locate the target on the object
  // tree derives the route from there; one reached through an interface port
  // has no such position to read -- the port is the only reach -- so that step
  // is derived at the reference site and handed here.
  [[nodiscard]] auto MakeRoutedRef(
      const slang::ast::ValueSymbol& value, ScopeFrameId slot_owner,
      hir::RouteHead head, std::vector<hir::PathStep> steps)
      -> diag::Result<hir::ReferenceRoute>;

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

  // How this reader reaches `target`, a scope elsewhere on the elaborated
  // hierarchy: the head it anchors at and the descent from there, with each
  // step typed where this unit declares what it lands on and by name where it
  // does not. Empty when no route reaches the scope, which is a target form
  // this unit cannot yet express rather than a compiler-bug invariant. Port
  // connections and hierarchical references share this one walk, so neither
  // reaches across an instance boundary a way the other cannot.
  [[nodiscard]] auto RouteToScope(
      const WalkFrame& frame, const slang::ast::Scope& target) const
      -> std::optional<ScopeRoute>;

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
  // Derives what this unit publishes from its own declarations: the object an
  // instance of it is, with a member per declaration another unit may name, and
  // one entry per port, whose parts the instantiating unit's connections are
  // consumed in step with. Every type is interned by this unit and then taken
  // into the signature's own pool, so what leaves stands on its own.
  auto PublishSignature() -> diag::Result<void>;

  // The reader-relative route to a cell in an instantiated scope: a direct
  // member when the target sits on the reader's own scope, a routed reference
  // otherwise -- a typed enclosing climb to a this-unit ancestor member, a
  // typed downward head when this unit emits the head's class, or a by-name
  // head where the route crosses into another instance's unit.
  [[nodiscard]] auto TranslateReferenceRoute(
      const WalkFrame& frame, const slang::ast::ValueSymbol& value)
      -> diag::Result<std::optional<hir::ReferenceRoute>>;

  // What the route ending in `steps` reaches, and what storage that is.
  [[nodiscard]] auto ResolveRouteTarget(
      const slang::ast::ValueSymbol& value,
      std::span<const hir::PathStep> steps) -> diag::Result<RouteTarget>;

  // The same, when the unit owning `value` published its name and the route's
  // own last step lands on an object of that unit. Empty otherwise, which is
  // every case where no declaration stands behind the name at the point the
  // reference is compiled.
  [[nodiscard]] auto PublishedRouteTarget(
      const slang::ast::ValueSymbol& value,
      std::span<const hir::PathStep> steps) -> std::optional<RouteTarget>;

  // Reserves an identity for each static-lifetime local one procedural block
  // subtree of `body` declares, and recurses into the blocks nested in it.
  // Every scope is walked the same way and contributes however many statics it
  // holds, none excluded: whether a hierarchical path can reach a given one is
  // the frontend's question, already answered before any reference gets here,
  // so nothing is held back on the chance that nothing will name it. Only the
  // identity is minted -- what is declared, including the initializer, is an
  // expression of the body and is filled when that body lowers.
  void DeclareProceduralStatics(
      const slang::ast::Scope& block, const slang::ast::Symbol& body_symbol,
      hir::ProceduralBodyRef body, ScopeFrameId frame);

  LoweringFacts facts_;
  const slang::ast::Scope* scope_;

  hir::CompilationUnit unit_;

  // What this unit publishes, built by the declaration phase and moved out
  // before any body lowers.
  hir::UnitSignature signature_;
  // The units this unit's own declarations name, recorded as the declaration
  // phase walks them.
  std::vector<std::string> referenced_units_;
  // What the units this one references publish, for the body phase alone. The
  // declaration phase has none, which is what makes "a declaration reads only
  // its own unit" a property of the code rather than a discipline.
  std::optional<hir::ConsumedSignatures> consumed_signatures_;
  // What each signature this unit has read out of became in this unit's pool,
  // one entry per signature, so a type published once is taken once however
  // many connections name it.
  std::unordered_map<const hir::UnitSignature*, hir::TypeImportMemo>
      signature_type_memos_;
  // Which record this unit made of each referenced unit's object, so every
  // reference into one names the same entry.
  std::unordered_map<std::string, hir::ExternalUnitObjectId>
      external_unit_objects_;
  // Which published position this unit gave each of its own declarations,
  // taken while the signature is derived and read back while bodies lower.
  std::unordered_map<const slang::ast::Symbol*, hir::PublishedMemberId>
      published_member_ids_;
  // Which unit each of this unit's interface ports carries.
  std::unordered_map<const slang::ast::Symbol*, std::string>
      interface_port_units_;
  // The declarations this unit's `ref` ports reach, under the binding each
  // port's direction states.
  std::unordered_map<const slang::ast::Symbol*, hir::ReferenceBinding>
      ref_port_internals_;
  // The declaration standing at each published position. A slot is filled when
  // its declaration takes its identity, and every one is filled before the unit
  // is handed on.
  std::vector<std::optional<hir::PublishedDecl>> published_members_;

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
  std::unordered_map<const slang::ast::Symbol*, InterfacePortBinding>
      interface_port_bindings_;
  SubroutineBindings subroutine_bindings_;
  ForeignImportBindings foreign_import_bindings_;
  ForeignImportScopes foreign_import_scopes_;
  OwnedChildBindings owned_child_bindings_;
  std::unordered_map<const slang::ast::ProceduralBlockSymbol*, hir::ProcessId>
      process_bindings_;
  ProceduralStaticBindings procedural_static_bindings_;
  // The local pool of each body that declares a static, keyed by the body's
  // frontend symbol; the body receives it when it is built.
  std::unordered_map<
      const slang::ast::Symbol*,
      base::Registry<hir::ProceduralVarDecl, hir::ProceduralVarId>>
      procedural_static_vars_;
  std::unordered_map<const slang::ast::Scope*, ScopeDeclarations>
      scope_declarations_;
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
  std::unordered_map<const slang::ast::Symbol*, hir::ProceduralScopeId>
      procedural_scopes_;
};

// Mints the identity of every procedural scope the bodies of `slang_scope`
// declare, into the registry of the declaration scope that owns them -- a
// structural scope or a class. A procedural scope is what slang records as
// one: a process body, a subroutine body, and each block that introduces
// declarations or carries a name (LRM 9.3.4 / 9.3.5); a construct that
// introduces neither is transparent and is no scope at all, so nothing is
// minted for it. Instance and generate members are not crossed, being their
// own declaration scopes with their own registries.
//
// Identity precedes bodies because a `disable` names a block or task by static
// declaration identity (LRM 9.6.2) and so can name one whose body lowers later
// or lives in another body entirely. Only what a name can reach from elsewhere
// is minted here, so nothing is minted that no body goes on to fill: the walk
// that lowers a body mints and fills every other scope it opens, and the
// lexical nesting -- which slang's member list does not follow -- is recorded
// there, where it is known.
void DeclareProceduralScopes(
    const slang::ast::Scope& slang_scope, UnitLowerer& owner,
    base::Registry<hir::ProceduralScopeDecl, hir::ProceduralScopeId>& scopes);

}  // namespace lyra::lowering::ast_to_hir
