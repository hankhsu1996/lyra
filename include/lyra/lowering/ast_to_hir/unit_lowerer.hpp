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
#include "lyra/support/assertion_policy.hpp"

namespace slang::ast {
class Expression;
class ClassType;
class HierarchicalReference;
class InterfacePortSymbol;
class ModportPortSymbol;
class Scope;
class TimingControl;
}  // namespace slang::ast

namespace lyra::lowering::ast_to_hir {

struct StructuralDataObjectBinding {
  ScopeFrameId home_frame{};
  hir::StructuralDataObjectId var_id{};
  hir::TypeId type{};
};

// Keyed by `ValueSymbol`, the common base of a variable and a net: both are
// named structural signals a reference binds to the same way, whatever the
// reference goes on to do with what it reaches.
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

// One procedural scope's minted identity, together with the declaration scope
// whose registry it indexes -- the one owning the body it was written in. The
// pair is what makes the identity meaningful to a reader: an id alone is a
// position in whichever registry the reader happens to hold.
//
// The owner is named by the frontend's own scope, which lives as long as the
// elaborated design. Naming it by the address of the registry instead makes the
// pair unsound: registries sit inside a scope arena that reallocates as sibling
// scopes are added, so one scope's recorded address can come to name a later
// scope's registry, and two unrelated scopes then compare equal.
struct MintedProceduralScope {
  const slang::ast::Scope* owner;
  hir::ProceduralScopeId scope;
};

// A contiguous run of positions kept out of one dimension, counted the way that
// dimension's declared range counts positions. It names no dimension of its
// own: a part select is written before the walk that reads it knows what the
// path lands on, and keeping every position is the run spanning the whole of
// whatever that turns out to be.
struct KeptPositions {
  std::uint32_t first;
  std::uint32_t count;
};

// One dimension of a declaration standing for objects, and the run of its
// positions in play (LRM 23.3.3.4, 23.3.3.5). So a port that binds every object
// it stands for and a name that selected a part of one are the same statement
// at two widths.
//
// LRM 23.3.3.5 pairs two such dimensions left index to left index, which is a
// statement about ends rather than about coordinates. So the conversion between
// a position and the offset of that position from the left end is the whole of
// the pairing, and both directions of it live here: one side of a connection
// converts to an offset, the other converts back.
struct OpenDimension {
  hir::UnpackedRange declared;
  KeptPositions kept;

  // How far in from the left end the element at `position` sits.
  [[nodiscard]] auto OffsetFromLeft(std::uint32_t position) const
      -> std::uint32_t {
    return Ascending() ? position - kept.first
                       : kept.first + kept.count - 1 - position;
  }

  // The position of the element sitting `offset` places in from the left end,
  // which is `OffsetFromLeft` read the other way.
  [[nodiscard]] auto PositionFromLeft(std::uint32_t offset) const
      -> std::uint32_t {
    return Ascending() ? kept.first + offset
                       : kept.first + kept.count - 1 - offset;
  }

 private:
  // Whether position zero is the left end. A position is counted from the lower
  // end of the declared range, so the two ends coincide exactly when the range
  // ascends.
  [[nodiscard]] auto Ascending() const -> bool {
    return declared.left <= declared.right;
  }
};

// Every position of every dimension, which is what a declaration nothing has
// selected out of leaves in play. A declaration standing for one object has no
// dimension, so it leaves nothing, which is what makes a scalar port the empty
// case of a ranged one everywhere below.
[[nodiscard]] inline auto WholeDimensions(
    std::span<const hir::UnpackedRange> dims) -> std::vector<OpenDimension> {
  std::vector<OpenDimension> open;
  open.reserve(dims.size());
  for (const hir::UnpackedRange& dim : dims) {
    open.push_back(
        OpenDimension{
            .declared = dim,
            .kept = KeptPositions{
                .first = 0,
                .count = static_cast<std::uint32_t>(dim.ElementCount())}});
  }
  return open;
}

// How a reader reaches a scope elsewhere on the elaborated hierarchy: where
// navigation starts, and the descent from there. What the route ends at is not
// part of it, so one walk serves both a reference to storage some scope holds
// and a connection naming the scope itself.
struct ScopeRoute {
  hir::RouteHead head;
  std::vector<hir::PathStep> steps;
  // The unit whose object the last step lands on, where it lands on one. What
  // the route reaches from there is counted out of that unit's signature, so
  // the walk states which unit it stopped in; a route that stops on no object
  // ended at a scope, and nothing past it was promised to anyone.
  std::optional<std::string> unit_name;
  // The coordinates of that landing the name left open, outermost first. A
  // name reaching one object leaves none, so this is empty for every reference
  // to storage; a connection may leave some, because a port is handed on whole
  // or in part and both are several objects rather than one.
  std::vector<OpenDimension> open;
};

// A reach that stays inside one unit's layout: out `hops` enclosing edges to
// the nearest scope enclosing both reader and target, then down through
// children this unit declares. The two are separate because out and in are
// separate axes -- an ancestor is the empty descent, and a sibling's child is
// both halves at once.
struct InUnitReach {
  hir::StructuralHops hops;
  std::vector<hir::OwnedChildRef> descent;
};

// What a hop of a descent could be, where the unit standing above it published
// the name: that unit's promise, and the position it gave the member. It
// becomes a step exactly where the hops above it kept a typed pointer, which is
// known only once the whole descent is in hand.
struct PublishedHop {
  const hir::UnitSignature* signature;
  hir::PublishedMemberId member;
  std::string unit_name;
  // How many objects the member stands for, as the promise declares them.
  // Empty where it stands for one.
  std::vector<hir::UnpackedRange> dims;
};

// One hop of a descent: the step it stands as, and the unit whose object it
// lands on where this unit's own declaration says which -- a child it declares,
// or the interface a port of it carries. A hop it declares nothing about lands
// on whatever the unit above it promised, which is read off that promise rather
// than recorded here -- and which unit stands above it is known only once the
// whole descent is in hand.
struct DescentHop {
  hir::PathStep step;
  std::optional<std::string> declared_unit;
  // How many objects this unit's own declaration says the hop stands over,
  // outermost first. Empty where it declares nothing about the hop, and empty
  // where what it declares stands for one object.
  std::vector<hir::UnpackedRange> declared_dims;
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
      const ForeignExportNames& foreign_export_names,
      support::AssertionPolicy assertion_policy)
      : source_mapper_(&source_mapper),
        sensitivity_analyzer_(&sensitivity_analyzer),
        foreign_export_names_(&foreign_export_names),
        assertion_policy_(assertion_policy) {
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

  [[nodiscard]] auto AssertionPolicy() const -> support::AssertionPolicy {
    return assertion_policy_;
  }

 private:
  const frontend::SlangSourceMapper* source_mapper_;
  SensitivityAnalyzer* sensitivity_analyzer_;
  const ForeignExportNames* foreign_export_names_;
  support::AssertionPolicy assertion_policy_;
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

  // The body phase: everything this unit executes, resolved against what the
  // design's units published. Which of those promises this unit ends up
  // depending on is the set it reads, so nothing decides that set in advance.
  auto LowerBodies(const hir::UnitSignatures& signatures)
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
  [[nodiscard]] auto Signatures() const -> const hir::UnitSignatures& {
    if (signatures_ == nullptr) {
      throw InternalError(
          "UnitLowerer::Signatures: another unit's signature is reachable only "
          "while bodies lower; the declaration phase reads this unit alone");
    }
    return *signatures_;
  }

  // This unit's record of the object an instance of `unit_name` is, taken from
  // that unit's signature the first time one is reached and answered with the
  // same identity every later time. Reaching another unit's object is what
  // declares the dependency on it, so its signature is in hand here.
  auto ExternalUnitObjectOf(const std::string& unit_name)
      -> hir::ExternalUnitObjectId;

  // This unit's record of what `unit_name` promised about its class
  // `class_name`, taken from that unit's signature the first time a property or
  // a behavior on it is reached. Nothing where that unit published no such
  // class, which is what leaves such a reference with nothing to compile
  // against.
  auto ExternalClassOf(
      const std::string& unit_name, const std::string& class_name)
      -> const hir::ExternalClass*;

  // Which storage the declaration `value` holds. One answer, so what this unit
  // publishes about a declaration and what a route to it reaches cannot differ.
  [[nodiscard]] auto DeclarationStorage(
      const slang::ast::ValueSymbol& value) const -> hir::PublishedStorage;

  // What the interface port `port` stands for: instances of the unit the
  // connection named, as many as the range it declares. The connection is read
  // where this unit's ports are published, so the answer is taken there and
  // read back here rather than reached for a second time -- what the unit
  // published about the port and the member it builds for it then cannot
  // describe different interfaces. Everything a name reached through the port
  // asks -- which unit, how many coordinates reach one instance -- is a
  // question about this one type.
  [[nodiscard]] auto InterfacePortType(const slang::ast::Symbol& port) const
      -> hir::TypeId {
    const auto it = interface_port_types_.find(&port);
    if (it == interface_port_types_.end()) {
      throw InternalError(
          "UnitLowerer::InterfacePortType: a unit publishes every interface "
          "port it declares before any of its bodies lower");
    }
    return it->second;
  }

  // The objects that port stands for, as its own type states them: which unit
  // they belong to, and the declared range of each dimension.
  [[nodiscard]] auto InterfacePortObjects(const slang::ast::Symbol& port) const
      -> hir::ObjectsBehindType {
    auto behind = hir::ObjectsBehind(unit_.types, InterfacePortType(port));
    if (!behind.has_value()) {
      throw InternalError(
          "UnitLowerer::InterfacePortObjects: an interface port stands for "
          "instances of the unit its connection named");
    }
    return *std::move(behind);
  }

  // How many instances that port stands for, as the declared range of each
  // dimension, outermost first. Empty where it stands for one.
  [[nodiscard]] auto InterfacePortDimensions(
      const slang::ast::Symbol& port) const -> std::vector<hir::UnpackedRange> {
    return InterfacePortObjects(port).shape.dims;
  }

  // Which unit's instances that port carries.
  [[nodiscard]] auto InterfaceUnitOf(const slang::ast::Symbol& port) const
      -> std::string {
    return std::string{InterfacePortObjects(port).unit_name};
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
  // Which behavior a method overriding `overridden` takes over, with the
  // introducing class named the way the boundary it sits on names one.
  auto MakeOverriddenBehavior(
      const hir::ClassRef& class_ref,
      const slang::ast::SubroutineSymbol& overridden, diag::SourceSpan span)
      -> diag::Result<hir::OverriddenBehavior>;

  // Which of the behaviors `cls` published the one named `method_name` is.
  // Refused where that class introduces no such behavior, since what a class
  // publishes is what it adds and this unit cannot count through a lineage of
  // another unit.
  auto MakeExternalDispatchSlot(
      const hir::ExternalClassRef& cls, std::string_view method_name,
      diag::SourceSpan span) -> diag::Result<hir::ExternalDispatchSlot>;

  auto MakeExternalCalleeInterface(
      const slang::ast::SubroutineSymbol& sym, diag::SourceSpan span)
      -> diag::Result<hir::ExternalCalleeInterface>;

  // The instance-property peer of `MakeClassMethodTarget`. Local when the class
  // was interned by this unit; external when the class lives in another
  // compilation unit, in which case the property is named by its position in
  // what that class published. A property that class kept to itself has no such
  // position, and the access has nothing to compile against.
  [[nodiscard]] auto MakeClassPropertyTarget(
      const hir::ClassRef& class_ref,
      const slang::ast::ClassPropertySymbol& prop, diag::SourceSpan span)
      -> diag::Result<hir::ClassPropertyTarget>;

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

  // The clocking event settled where `containing` is written, for a construct
  // that names none of its own -- the clock the enclosing procedure settles
  // (LRM 16.14.6), and otherwise that scope's default clocking (LRM 14.12).
  // Only a procedure settles one, so a construct written anywhere else has
  // none, which is what a sampled value function and a concurrent assertion
  // both read before reporting the error the standard requires.
  [[nodiscard]] auto InferredProcedureClock(
      const slang::ast::Symbol& containing) const
      -> const slang::ast::TimingControl*;

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
      const slang::ast::ProceduralBlockSymbol& proc) const -> bool;

  [[nodiscard]] auto AssertionPolicy() const -> support::AssertionPolicy {
    return facts_.AssertionPolicy();
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

  // The subroutine this interface evaluates one name a view offers only for
  // reading in (LRM 25.5.4). Only such a name has one: every other direction
  // designates storage, which a referrer reaches rather than asks for. The
  // identity is minted with every other structural identity so the signature
  // can name it before any body is lowered.
  void MapModportEvaluator(
      const slang::ast::Symbol& port, hir::StructuralSubroutineId evaluator);
  [[nodiscard]] auto ModportEvaluatorOf(const slang::ast::Symbol& port) const
      -> hir::StructuralSubroutineId;

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
  auto MapOrGetRoutedRef(ScopeFrameId slot_owner_frame, hir::RoutedRefDecl decl)
      -> hir::RoutedRefId;
  auto TakeRoutedRefsForFrame(ScopeFrameId slot_owner_frame)
      -> base::Arena<hir::RoutedRefDecl, hir::RoutedRefId>;

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

  // The frame assigned to `scope` by the declaration pass. Every scope a
  // structural lowerer is built for was assigned one, so absence is a
  // compiler-bug invariant.
  [[nodiscard]] auto LookupScopeFrame(const slang::ast::Scope& scope) const
      -> ScopeFrameId;

  // The structural nesting a body declared in `scope` resolves outward names
  // against: every enclosing structural frame, outermost first. A class is a
  // scope of the name tree (LRM 23.9), so a body it declares reaches an
  // enclosing declaration the same way a process of the declaring scope does,
  // and this is what puts that scope at the start of the walk.
  [[nodiscard]] auto DeclaringScopeChain(const slang::ast::Scope& scope) const
      -> std::vector<ScopeFrameId>;

  // The structural scope whose instance `cls` is a type of (LRM 6.22). A class
  // nested inside another class is a type of the same instance the outer one
  // is, since SystemVerilog gives it no reach into the outer object.
  [[nodiscard]] auto DeclaringStructuralScope(
      const slang::ast::ClassType& cls) const -> const slang::ast::Scope&;

  // How far out of `frame`'s own structural scope the instance an object of
  // `cls` would belong to sits, for a construction written there. Absent where
  // the class is a namespace unit's, which no instance replicates. A class this
  // unit does not declare is reached through its unit's signature, which
  // carries no such instance, and is refused rather than answered wrongly.
  [[nodiscard]] auto DeclaringScopeHopsFrom(
      const slang::ast::ClassType& cls, const WalkFrame& frame,
      diag::SourceSpan span)
      -> diag::Result<std::optional<hir::StructuralHops>>;

  // Hands a structural scope the classes it declares, once. Every class the
  // unit declares is named by exactly one scope, so a scope that declares none
  // takes the empty list rather than being absent from the relation.
  [[nodiscard]] auto TakeDeclaredClasses(const slang::ast::Scope& scope)
      -> std::vector<hir::ClassId>;

  // Records the identity a declaration scope minted for one of its procedural
  // scopes, keyed by the symbol slang records the scope as.
  void DeclareProceduralScope(
      const slang::ast::Symbol& symbol, const slang::ast::Scope& owner,
      hir::ProceduralScopeId scope) {
    procedural_scopes_.emplace(
        &symbol, MintedProceduralScope{.owner = &owner, .scope = scope});
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
    return it->second.scope;
  }

  // The identity minted for `symbol` together with the declaration scope that
  // minted it, or nothing where this unit minted none. A scope's identity
  // indexes its declaration scope's registry, so it means nothing without that
  // scope -- which is why a body naming a scope reads the two together: the
  // owner says whether the name stays inside this artifact and, if it does not
  // reach it directly, which scope a route to it lands on.
  [[nodiscard]] auto LookupMintedProceduralScope(
      const slang::ast::Symbol& symbol) const
      -> std::optional<MintedProceduralScope> {
    const auto it = procedural_scopes_.find(&symbol);
    if (it == procedural_scopes_.end()) return std::nullopt;
    return it->second;
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
  auto InternOwnClassDeclarations(const slang::ast::Scope& scope)
      -> diag::Result<void>;

  // A class whose declarations are settled and whose bodies are not. A class
  // body names what encloses the class the way any other body does, so it
  // lowers once every structural scope has bound its own declarations; what is
  // carried here is what the declaration half already settled.
  struct PendingClassBody {
    const slang::ast::ClassType* cls;
    hir::ClassId id;
    diag::SourceSpan span;
    const slang::ast::Scope* declaring_scope;
    // Held by pointer because its identity is its address: the procedural
    // scopes a body may name are minted into `decl->procedural_scopes` in the
    // declaration half and looked up by the registry they belong to, so the
    // registry may not move between the two halves.
    std::unique_ptr<hir::ClassDecl> decl;
    std::vector<const slang::ast::SubroutineSymbol*> defined_methods;
    std::vector<const slang::ast::MethodPrototypeSymbol*> pure_prototypes;
    const slang::ast::SubroutineSymbol* constructor_sym;
  };

  // Lowers the body half of every class `scope` declares: each method, the
  // constructor, and every property initializer. Called while that scope is
  // lowered, so a class body has the reach a process of the scope has.
  auto PopulateClassBodiesDeclaredIn(const slang::ast::Scope& scope)
      -> diag::Result<void>;
  auto PopulateClassBody(PendingClassBody& pending) -> diag::Result<void>;

  // Every class a unit declares is declared by one of its structural scopes,
  // and every such scope lowers what it declares, so nothing is left over.
  void RequireEveryClassBodyLowered() const;

  // Builds a HIR Expr referring to the leaf `decl` navigates to.
  // `slot_owner_frame` is the frame whose routed-reference arena holds the
  // slot.
  auto MakeRoutedMemberRef(
      ScopeFrameId slot_owner_frame, hir::RoutedRefDecl decl,
      diag::SourceSpan span) -> hir::Expr;

  // The reference to `value` over a route the caller derived: the route says
  // how the reader reaches it, and what the route ends at follows from the
  // route alone. A reader that can locate the target on the object tree derives
  // the route from there; one reached through an interface port has no such
  // position to read -- the port is the only reach -- so that route is derived
  // at the reference site and handed here.
  [[nodiscard]] auto MakeRoutedRef(
      const slang::ast::ValueSymbol& value, ScopeFrameId slot_owner,
      ScopeRoute route) -> diag::Result<hir::ReferenceRoute>;

  // The reference to the object `route` reaches. Reaching an object across an
  // instance boundary seals like reaching a cell there: the route runs once at
  // elaboration and what it lands on is read directly after, so a caller
  // holding this reaches the object with no traversal of its own.
  [[nodiscard]] auto MakeRoutedObjectRef(
      ScopeFrameId slot_owner, ScopeRoute route, hir::TypeId object_type)
      -> hir::RoutedRef;

  // The reference to the callable `route` reaches by name, for a callable no
  // unit published. It seals on the same terms the object does and over the
  // same walk: the scope answers the name once at elaboration and the call
  // reads the entry directly after.
  [[nodiscard]] auto MakeRoutedCallableRef(
      ScopeFrameId slot_owner, ScopeRoute route, std::string name,
      hir::ExternalCalleeInterface interface) -> hir::RoutedRef;

  // What a `disable` naming `target` terminates, reached over a route (LRM
  // 9.6.2, 23.6). Where this unit lays out the scope that declares `target` the
  // route runs to that scope and carries its own identity for the block;
  // otherwise it runs to the block's own node on the object tree, which answers
  // for what it carries. Both seal in the resolve phase, so the statement
  // itself walks nothing.
  [[nodiscard]] auto MakeRoutedDisableTargetRef(
      const WalkFrame& frame, const slang::ast::Symbol& target,
      diag::SourceSpan span) -> diag::Result<hir::RoutedRef>;

  // How this reader reaches the object an instance of another unit is, given
  // how the name reached it. A port is the answer where the name went through
  // one, since what stands behind a port is reached no other way; any other
  // name reaches the same object in every instantiation, and the walk on the
  // elaborated hierarchy states that. Empty when no route reaches it.
  [[nodiscard]] auto RouteToUnitObject(
      const WalkFrame& frame, const slang::ast::InstanceBodySymbol& body,
      const slang::ast::HierarchicalReference& reference, diag::SourceSpan span)
      -> diag::Result<ScopeRoute>;

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

  // The route to the scope that declares a callee, for a callee no unit's
  // signature mentions. It is the walk below, with the one reason that walk can
  // fail written where it is known rather than at whichever caller met it.
  [[nodiscard]] auto RouteToDeclaringScope(
      const WalkFrame& frame, const slang::ast::Scope& target,
      diag::SourceSpan span) -> diag::Result<ScopeRoute>;

  // How this reader reaches `target`, a scope elsewhere on the elaborated
  // hierarchy: the head it anchors at and the descent from there, with each
  // step typed where this unit declares what it lands on and by name where it
  // does not. Empty when no route reaches the scope, never a compiler-bug
  // invariant -- either the walk found a target form this unit cannot yet
  // express, or the scope sits in a namespace unit, which has no instance and
  // so nothing on the object tree a route could walk to at all. Port
  // connections and hierarchical references share this one walk, so neither
  // reaches across an instance boundary a way the other cannot.
  [[nodiscard]] auto RouteToScope(
      const WalkFrame& frame, const slang::ast::Scope& target)
      -> std::optional<ScopeRoute>;

  // How this reader reaches `target` when `target` is a scope this unit itself
  // lays out: the climb to the nearest scope enclosing both, then the descent
  // back down. Every part is typed, so an identity the target scope minted --
  // a position in one of its own registries -- means what it says once the
  // reach lands. Refuses where any part leaves this unit's layout, which is
  // the same reach and a different arm of it.
  [[nodiscard]] auto ReachOwnScope(
      const WalkFrame& frame, const slang::ast::Scope& target,
      diag::SourceSpan span) -> diag::Result<InUnitReach>;

  // How this reader reaches the object that owns what a name reached through an
  // interface port names (LRM 25.3). The port is the first step and says which
  // unit the descent starts in; each hop after it either carries a coordinate
  // on the hop before it -- already the position the select resolved to, since
  // spending the declared range is what resolving it does -- or names one more
  // step down. Which of those steps are typed and which are answered by name is
  // decided the way it is for every other descent, so a name may continue past
  // what the interface published (LRM 25.10) rather than stopping there.
  // A coordinate the path did not write stays open, because a port is handed on
  // whole or in part and both name several objects at once (LRM 23.3.3.4).
  // Nothing when the path is of a shape the walk does not take.
  [[nodiscard]] auto ReachThroughInterfacePort(
      const WalkFrame& frame,
      const slang::ast::HierarchicalReference& reference)
      -> std::optional<ScopeRoute>;

  // The same, for a name, which reaches exactly one object: a reach that left a
  // coordinate open is several and so is not one.
  [[nodiscard]] auto ReachOneThroughInterfacePort(
      const WalkFrame& frame,
      const slang::ast::HierarchicalReference& reference)
      -> std::optional<ScopeRoute> {
    auto reach = ReachThroughInterfacePort(frame, reference);
    if (!reach.has_value() || !reach->open.empty()) {
      return std::nullopt;
    }
    return reach;
  }

  // The reads of a dependency set that name a cell, as the entries watching it.
  // A read of anything else contributes none, so a constant read alongside a
  // signal leaves only the signal watched (LRM 9.2.2.2.1).
  [[nodiscard]] auto TranslateSensitivityReads(
      const std::vector<SensitivityRead>& reads, const WalkFrame& frame)
      -> diag::Result<std::vector<hir::SensitivityEntry>>;

 private:
  // Derives what this unit publishes from its own declarations: the object an
  // instance of it is, with a member per declaration another unit may name, and
  // one entry per port, whose parts the instantiating unit's connections are
  // consumed in step with. Every type is interned by this unit and then taken
  // into the signature's own pool, so what leaves stands on its own.
  auto PublishSignature() -> diag::Result<void>;

  // Derives what this unit promises about each class of the source language it
  // declares: the properties another unit may name, in the order that fixes
  // their slots, and the behaviors the class introduces, in the order that
  // fixes their ordinals. Every class is already minted when this runs, so this
  // reads the unit's own declarations rather than the frontend's tree.
  auto PublishClassSignatures() -> void;

  // How this reader reaches the interface an enclosing scope's `port` carries
  // (LRM 25.3). The port is the whole of this unit's reach to it: what stands
  // behind it belongs to a unit this one reaches no other way, so any other
  // route to the same object would describe a different design -- which is why
  // the frontend's own resolution of the port to that object is not what this
  // reads. Where the route goes from there is the caller's, so one derivation
  // serves a name read through the port and a connection handing the port's
  // interface on.
  [[nodiscard]] auto RouteThroughInterfacePort(
      const WalkFrame& frame, const slang::ast::Symbol& port) const
      -> ScopeRoute;

  // What the unit named `unit_name` promised under `name`, where what it
  // promised is an object of a unit of its own (LRM 25.10). Nothing when it
  // promised no such name, which leaves the hop one the runtime answers.
  [[nodiscard]] auto PromisedObjectMember(
      const std::string& unit_name, std::string_view name) const
      -> std::optional<PublishedHop>;

  // Fills `route` with the descent `hops` state, turning each hop the unit
  // standing above it promised into a step through that promise, and stating
  // which unit the whole route lands on. Both follow the descent forward, since
  // which unit stands at a hop is what every hop before it decided.
  void ClassifyDescent(ScopeRoute& route, std::span<DescentHop> hops);

  // What a process waiting on a name a modport offers observes: every member
  // the expression behind that name reads (LRM 25.5.4), each reached through
  // the interface the reader's own port carries. The name is no storage of its
  // own, so nothing waits on it directly.
  auto ObservedThroughModport(
      const slang::ast::ModportPortSymbol& offered, const WalkFrame& frame)
      -> diag::Result<std::vector<hir::SensitivityEntry>>;

  // The reader-relative route to a cell in an instantiated scope: a direct
  // member when the target sits on the reader's own scope, a routed reference
  // otherwise -- a typed enclosing climb to a this-unit ancestor member, a
  // typed downward head when this unit emits the head's class, or a by-name
  // head where the route crosses into another instance's unit.
  [[nodiscard]] auto TranslateReferenceRoute(
      const WalkFrame& frame, const slang::ast::ValueSymbol& value)
      -> diag::Result<std::optional<hir::ReferenceRoute>>;

  // What `route` reaches.
  [[nodiscard]] auto ResolveRouteTarget(
      const slang::ast::ValueSymbol& value, const ScopeRoute& route)
      -> diag::Result<hir::RouteLeaf>;

  // The same, when the route lands on an object of a unit that published the
  // name. Empty otherwise, which is every case where no declaration stands
  // behind the name at the point the reference is compiled.
  [[nodiscard]] auto LookupPublishedRouteTarget(
      const slang::ast::ValueSymbol& value, const ScopeRoute& route)
      -> std::optional<hir::RouteLeaf>;

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
  // What the design's units publish, for the body phase alone. The declaration
  // phase has none, which is what makes "a declaration reads only its own unit"
  // a property of the code rather than a discipline.
  const hir::UnitSignatures* signatures_ = nullptr;
  // What each signature this unit has read out of became in this unit's pool,
  // one entry per signature, so a type published once is taken once however
  // many connections name it.
  std::unordered_map<const hir::UnitSignature*, hir::TypeImportMemo>
      signature_type_memos_;
  // What each class this unit declares would promise another unit, taken where
  // that class's own arenas are built so the two count the same positions. Only
  // the classes the unit's namespace declares reach its signature.
  std::unordered_map<const slang::ast::ClassType*, hir::ClassSignature>
      own_class_promises_;
  // Which record this unit made of each referenced unit's object, so every
  // reference into one names the same entry.
  std::unordered_map<std::string, hir::ExternalUnitObjectId>
      external_unit_objects_;
  // Which published position this unit gave each of its own declarations,
  // taken while the signature is derived and read back while bodies lower.
  std::unordered_map<const slang::ast::Symbol*, hir::PublishedMemberId>
      published_member_ids_;
  // What each of this unit's interface ports stands for.
  std::unordered_map<const slang::ast::Symbol*, hir::TypeId>
      interface_port_types_;
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
  std::unordered_map<const slang::ast::Scope*, std::vector<PendingClassBody>>
      pending_class_bodies_;
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
  std::unordered_map<const slang::ast::Symbol*, hir::StructuralSubroutineId>
      modport_evaluators_;
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
  std::unordered_map<const slang::ast::Scope*, std::vector<hir::ClassId>>
      classes_by_scope_;
  std::map<ScopeFrameId, base::Arena<hir::RoutedRefDecl, hir::RoutedRefId>>
      routed_refs_by_frame_;
  std::uint32_t next_scope_frame_ = 0;
  std::uint32_t next_with_clause_ = 0;
  std::unordered_map<const slang::ast::Symbol*, MintedProceduralScope>
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
// `declaring` is the scope whose registry the minted identities index, and
// `walked` the scope being scanned for them; the two part company as the walk
// descends into blocks the declaration scope still owns.
void DeclareProceduralScopes(
    const slang::ast::Scope& declaring, const slang::ast::Scope& walked,
    UnitLowerer& owner,
    base::Registry<hir::ProceduralScopeDecl, hir::ProceduralScopeId>& scopes);

}  // namespace lyra::lowering::ast_to_hir
