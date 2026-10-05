#pragma once

#include <cstdint>
#include <map>
#include <optional>
#include <span>
#include <string>
#include <string_view>
#include <unordered_map>
#include <utility>
#include <variant>
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
#include "lyra/hir/published_callable.hpp"
#include "lyra/hir/stmt.hpp"
#include "lyra/hir/structural_data_object.hpp"
#include "lyra/hir/structural_scope.hpp"
#include "lyra/hir/type_import.hpp"
#include "lyra/hir/unit_signature.hpp"
#include "lyra/hir/unit_signatures.hpp"
#include "lyra/hir/value_ref.hpp"
#include "lyra/lowering/ast_to_hir/climb.hpp"
#include "lyra/lowering/ast_to_hir/instance_array_shape.hpp"
#include "lyra/lowering/ast_to_hir/sensitivity.hpp"
#include "lyra/lowering/ast_to_hir/unit_identity.hpp"
#include "lyra/lowering/ast_to_hir/walk_frame.hpp"
#include "lyra/support/assertion_policy.hpp"

namespace slang::ast {
class Expression;
class ClassPropertySymbol;
class ClassType;
class HierarchicalReference;
class GenerateBlockArraySymbol;
class InterfacePortSymbol;
class ModportPortSymbol;
class ModportSymbol;
class PortSymbol;
class Scope;
class TimingControl;
class ValueSymbol;
class VariableSymbol;
}  // namespace slang::ast

namespace lyra::lowering::ast_to_hir {

struct StructuralDataObjectBinding {
  ScopeFrameId home_frame{};
  hir::StructuralDataObjectId var_id{};
};

// Keyed by `ValueSymbol`, the common base of a variable and a net: both are
// named structural signals a reference binds to the same way, whatever the
// reference goes on to do with what it reaches.
using StructuralDataObjectBindings = std::unordered_map<
    const slang::ast::ValueSymbol*, StructuralDataObjectBinding>;

// Where an interface port stands, as its own unit reaches it: the scope that
// declares it, its identity there, and this unit's record of the class of the
// object bound to it. A name reached through the port is counted out of that
// record.
struct InterfacePortBinding {
  ScopeFrameId home_frame{};
  hir::InterfacePortId port{};
  hir::ExternalScopeClassId scope_class{};
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
// declaring scope's identity for it, as a path element reaches it -- a loop's
// block being the loop with the select that picks it -- so the reference
// resolves regardless of whether it precedes the child in source.
struct OwnedChildBinding {
  ScopeFrameId home_frame{};
  hir::OwnedChildStep step;
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

// What one scope of this unit makes reachable to other units, as the walk
// minting the unit's identities found it: which declarations, in which order,
// and under which named blocks and subroutines (LRM 23.6, 23.9). Each list is
// in the order that walk reached its entries, and that order is the one the
// signature states and the scope's published class is laid out in. The
// signature adds names, types and storage to these entries, and the walk
// lowering the scope reads its identities off them.
//
// An entry carries the identity minted for it where the walk minted one. What
// takes its identity later -- an interface port, once the object it carries is
// known; a class's static property, once the class is interned; a named block,
// once its body's scopes are -- carries the front end's symbol until then.
struct ScopePublicationRecord {
  // A variable or net of the scope (LRM 6.5).
  struct DataObject {
    const slang::ast::ValueSymbol* symbol = nullptr;
    hir::StructuralDataObjectId id;
  };
  // A static-lifetime variable of one of the scope's bodies (LRM 6.21),
  // published under the named blocks and subroutine `within` it.
  struct LocalStatic {
    const slang::ast::VariableSymbol* symbol = nullptr;
    hir::ProceduralBodyRef body;
    hir::ProceduralVarId var;
    std::vector<std::string> within;
  };
  // A static property of a class the scope declares (LRM 6.22, 8.9).
  struct ClassStatic {
    const slang::ast::ClassPropertySymbol* symbol = nullptr;
    const slang::ast::ClassType* owner = nullptr;
  };
  // An instance the scope builds, or an array of them, with the unit each is
  // an instance of and the range of each dimension; a single instance has
  // none.
  struct Instance {
    const slang::ast::Symbol* symbol = nullptr;
    hir::InstanceMemberId id;
    InstanceArrayShape shape;
  };
  // An interface port (LRM 25.3).
  struct InterfacePort {
    const slang::ast::InterfacePortSymbol* symbol = nullptr;
  };
  using Member = std::variant<
      DataObject, LocalStatic, ClassStatic, Instance, InterfacePort>;

  // A subroutine the source declared on the scope (LRM 13).
  struct Subroutine {
    const slang::ast::SubroutineSymbol* symbol = nullptr;
    hir::StructuralSubroutineId id;
  };
  // The subroutine evaluating a port's default (LRM 23.2.2.4).
  struct PortDefault {
    const slang::ast::PortSymbol* port = nullptr;
    hir::StructuralSubroutineId id;
  };
  // The subroutine evaluating a name a view offers only for reading (LRM
  // 25.5.4).
  struct ViewRead {
    const slang::ast::ModportSymbol* modport = nullptr;
    const slang::ast::ModportPortSymbol* port = nullptr;
    hir::StructuralSubroutineId id;
  };
  using Callable = std::variant<Subroutine, PortDefault, ViewRead>;

  // A loop generate that counted out at least one block (LRM 27.4).
  struct Loop {
    hir::GenerateId id;
    const slang::ast::GenerateBlockArraySymbol* loop = nullptr;
  };
  // A conditional generate (LRM 27.5), with the alternatives this elaboration
  // built a scope for.
  struct Choice {
    hir::GenerateId id;
    std::vector<const slang::ast::GenerateBlockSymbol*> built;
  };
  using Construct = std::variant<Loop, Choice>;

  // A named block or task a `disable` ends (LRM 9.6.2), under the path of
  // named scopes a name spells to reach it.
  struct DisableTarget {
    const slang::ast::Symbol* symbol = nullptr;
    std::vector<std::string> path;
  };

  // A name a view defines with an expression of its own (LRM 25.5.4): one
  // designating storage, or one offered only for reading, which the scope's
  // callable for it computes.
  struct ViewPlace {
    const slang::ast::ModportPortSymbol* port = nullptr;
  };
  struct ViewComputed {
    const slang::ast::ModportPortSymbol* port = nullptr;
  };
  using ViewName = std::variant<ViewPlace, ViewComputed>;
  struct View {
    const slang::ast::ModportSymbol* modport = nullptr;
    std::vector<ViewName> names;
  };

  std::string class_name;
  std::vector<Member> members;
  std::vector<Callable> callables;
  std::vector<Construct> generates;
  std::vector<DisableTarget> disable_targets;
  std::vector<View> views;

  // Where the member standing for `declared` sits among the members, or
  // nothing where the scope publishes no member for it.
  [[nodiscard]] auto MemberOf(const slang::ast::Symbol& declared) const
      -> std::optional<hir::PublishedMemberId>;

  // Where the callable evaluating the expression `holder` holds sits among
  // the callables, or nothing where the scope publishes none for it.
  [[nodiscard]] auto CallableOf(const slang::ast::Symbol& holder) const
      -> std::optional<hir::PublishedCallableId>;
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

// Where a walk stands once its steps are taken: still in this unit's layout,
// where what it reaches is this unit's own declaration, or on a scope of
// another unit, where what it reaches is counted out of what that scope
// published.
struct InOwnScope {};

// The scope of another unit a walk stands on: this unit's record of what that
// scope published, and the named blocks and subroutines of it the name went on
// through, outermost first (LRM 23.9). Those are no objects, so they take no
// step; a declaration the name reaches there is published under this path.
struct InExternalScope {
  hir::ExternalScopeClassId scope_class;
  std::vector<std::string> within;
};

using RoutePlace = std::variant<InOwnScope, InExternalScope>;

// How a reader reaches a scope elsewhere on the elaborated hierarchy: where
// navigation starts, the descent from there, and where that leaves it. What
// the route ends at is not part of it, so one walk serves both a reference to
// storage some scope holds and a connection naming the scope itself.
struct ScopeRoute {
  hir::RouteBase base;
  std::vector<hir::PathStep> steps;
  RoutePlace place;
  // The coordinates of that landing the name left open, outermost first. A
  // name reaching one object leaves none, so this is empty for every reference
  // to storage; a connection may leave some, because a port is handed on whole
  // or in part and both are several objects rather than one.
  std::vector<OpenDimension> open;

  // The route out `hops` enclosing edges of the reader's own layout, which
  // descends nowhere -- zero of them for the reader's own scope.
  static auto Enclosing(hir::StructuralHops hops) -> ScopeRoute {
    return ScopeRoute{
        .base = hir::InUnitBase{.hops = hops},
        .steps = {},
        .place = InOwnScope{},
        .open = {}};
  }
};

// A reach that stays inside one unit's layout: out `hops` enclosing edges to
// the nearest scope enclosing both reader and target, then down through
// children this unit declares. The two are separate because out and in are
// separate axes -- an ancestor is the empty descent, and a sibling's child is
// both halves at once.
struct InUnitReach {
  hir::StructuralHops hops;
  std::vector<hir::OwnedChildStep> descent;
};

// A hop of a descent this unit declares nothing about, as the front end says
// the name reached it: an instance, with the coordinates the name wrote for
// it; a block of a loop generate, by the label of the loop and the value its
// index stood at (LRM 27.4); a block that stands alone or that a conditional
// chose, by its own label (LRM 27.5); or a named block or subroutine, which is
// no object (LRM 23.9). It is resolved against what the scope standing above
// it published, which is known only once the whole descent is in hand.
struct InstanceHop {
  std::string name;
  std::vector<std::uint32_t> indices;
};
struct LoopBlockHop {
  std::string loop;
  std::int64_t index;
};
struct LabeledBlockHop {
  std::string name;
};
struct ProceduralHop {
  std::string name;
};
using NamedHop =
    std::variant<InstanceHop, LoopBlockHop, LabeledBlockHop, ProceduralHop>;

// A hop of a descent this unit declares: the step it stands as, where that
// leaves the walk -- on the scope of another unit where this unit's
// declaration says which, a child it declares or the interface a port of it
// carries, and in its own layout otherwise -- and how many objects that
// declaration says it stands over, outermost first, empty where it stands for
// one.
struct DeclaredHop {
  hir::PathStep step;
  RoutePlace place;
  std::vector<hir::UnpackedRange> dims;
};

// One hop of a descent: one this unit declares, or one resolved against what
// the scope above it published.
using DescentHop = std::variant<DeclaredHop, NamedHop>;

// Where a descent ends up: its steps, where they leave a route taking them,
// and the coordinates of that landing left open.
struct Descent {
  std::vector<hir::PathStep> steps;
  RoutePlace place;
  std::vector<OpenDimension> open;
};

// The same for a descent through published scope classes alone, which steps
// only through what each published and so always stands on one of them.
struct PublishedDescent {
  std::vector<hir::ExternalStep> steps;
  InExternalScope place;
  std::vector<OpenDimension> open;
};

// The reach an interface port of this unit is (LRM 25.3): the base of a route
// to the scope that carries it, and the port as the descent's first hop,
// standing for every object it carries until a coordinate picks one.
struct PortReach {
  hir::RouteBase base;
  DeclaredHop hop;
};

// Where a name that leaves the reader's instance starts: the base the route
// takes, the hop that brings it to stand in `below` where one does, and
// `below`, the scope the rest of the descent starts under. A name searched
// upward starts at the enclosing instance it landed in (LRM 23.8) and needs no
// hop; one written through an interface port starts at the port and stands in
// the instance bound there (LRM 25.3).
struct RouteStart {
  const slang::ast::Scope* below = nullptr;
  hir::RouteBase base;
  std::optional<DeclaredHop> leading;
};

// Where a route to a scope starts: at the reader, for a name that stays inside
// the reader's instance and where no name is involved at all, or where a name
// that left it starts.
using RouteOrigin = std::variant<FromReader, RouteStart>;

// The declarations of one structural scope that a peer may name before the
// scope is built, minted here and handed to the scope when it is.
struct ScopeDeclarations {
  base::Registry<hir::StructuralDataObjectDecl, hir::StructuralDataObjectId>
      structural_data_objects;
  base::Registry<hir::SubroutineDecl, hir::StructuralSubroutineId>
      structural_subroutines;
  base::Registry<hir::Process, hir::ProcessId> processes;
  base::Registry<hir::Generate, hir::GenerateId> generates;
  base::Registry<hir::InstanceMemberDecl, hir::InstanceMemberId>
      instance_members;
};

// What every unit's lowering reads and none of them changes: where a
// construct was written, the shared sensitivity analysis (one cache across the
// design), whether assertions are elided rather than rejected, the foreign
// export names, which are resolved design-wide before any unit is walked, and
// what decides the name of every unit, which every party naming one reads.
// The slang compilation itself is deliberately absent: walking top instances is
// the driver's job, and a unit lowering that could reach it would be able to
// read another unit.
class LoweringFacts {
 public:
  LoweringFacts(
      const frontend::SlangSourceMapper& source_mapper,
      SensitivityAnalyzer& sensitivity_analyzer,
      const ForeignExportNames& foreign_export_names,
      support::AssertionPolicy assertion_policy,
      const SpecializationPolicy& specialization)
      : source_mapper_(&source_mapper),
        sensitivity_analyzer_(&sensitivity_analyzer),
        foreign_export_names_(&foreign_export_names),
        assertion_policy_(assertion_policy),
        specialization_(&specialization) {
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

  [[nodiscard]] auto Specialization() const -> const SpecializationPolicy& {
    return *specialization_;
  }

 private:
  const frontend::SlangSourceMapper* source_mapper_;
  SensitivityAnalyzer* sensitivity_analyzer_;
  const ForeignExportNames* foreign_export_names_;
  support::AssertionPolicy assertion_policy_;
  const SpecializationPolicy* specialization_;
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
  // design's units published. Which of those publications this unit ends up
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

  // This unit's record of the class `instance` is an object of: the class of
  // the unit its specialization compiled to.
  auto ScopeClassOfInstance(const slang::ast::InstanceSymbol& instance)
      -> hir::ExternalScopeClassId;

  // This unit's record of the class an instance of `unit_name` is an object
  // of, taken from that unit's signature the first time one is reached and
  // answered with the same identity every later time. Reaching another unit's
  // object is what declares the dependency on it, so its signature is in hand
  // here.
  auto ExternalScopeClassOf(const std::string& unit_name)
      -> hir::ExternalScopeClassId;

  // The same record of the scope class `class_name` that unit published: the
  // class of an instance of it, or of a generate block inside one.
  auto ExternalScopeClassOf(
      const std::string& unit_name, const std::string& class_name)
      -> hir::ExternalScopeClassId;

  // The type of an object of the scope class `scope_class` records.
  [[nodiscard]] auto ScopeClassTypeOf(
      hir::ExternalScopeClassId scope_class) const -> hir::TypeId;

  // Where a route ends that reaches the member the scope class `scope_class`
  // records published at `member`: what storage that member is and its type,
  // as that class stated them.
  [[nodiscard]] auto ExternalMemberLeafOf(
      hir::ExternalScopeClassId scope_class,
      hir::PublishedMemberId member) const -> hir::ExternalMemberLeaf;

  // This unit's record of what `unit_name` published about its class
  // `class_name`, taken from that unit's signature the first time this unit
  // asks and answered from the record every later time. Every class a unit
  // declares is published, so the record always exists. Reading another
  // signature may move the records, so a caller takes what it needs from one
  // before asking for the next.
  auto ExternalClassOf(
      const std::string& unit_name, const std::string& class_name)
      -> const hir::ExternalClass&;

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
  // regardless of which route saw the class first.
  auto ResolveClassRef(const slang::ast::ClassType& cls, diag::SourceSpan span)
      -> diag::Result<hir::ClassRef>;

  // Builds a class-method target from a class reference and the resolved
  // callee symbol. Local when the class was interned by this unit -- the
  // method's arena position is queryable through the `SubroutineSymbol`-keyed
  // cache the interning populated. External when the class lives in another
  // compilation unit -- the method is named by (declaring unit, class
  // canonical name, method name).
  [[nodiscard]] auto MakeClassMethodTarget(
      const hir::ClassRef& class_ref,
      const slang::ast::SubroutineSymbol& method) const
      -> hir::ClassMethodTarget;

  // The method a call reaches. Intra-unit that is the slot alone, since the
  // declaration it resolves to answers everything else about the callee;
  // cross-unit there is no such declaration to reach, so the callee also
  // carries its dispatch role (LRM 8.20) and the interface a call marshals
  // against (LRM 13.5). `owner` declares the method.
  auto MakeMethodCallee(
      const slang::ast::ClassType& owner,
      const slang::ast::SubroutineSymbol& method, diag::SourceSpan span)
      -> diag::Result<hir::MethodCallee>;

  // A class as this unit reads it: one it declares, read off its own
  // declaration, or one another unit declares, read off that unit's signature.
  using LocalOrPublishedClass =
      std::variant<const slang::ast::ClassType*, hir::ExternalClassRef>;

  // Which of the two the class `type` names is.
  auto ReadAsLocalOrPublished(
      const slang::ast::Type& type, diag::SourceSpan span)
      -> diag::Result<LocalOrPublishedClass>;

  // What a class's declaration names beside itself: the class it extends, and
  // the interface classes it implements or, for an interface class, extends
  // (LRM 8.26.2), in the order written.
  struct ClassParents {
    std::optional<LocalOrPublishedClass> base;
    std::vector<LocalOrPublishedClass> implements;
  };
  auto ParentsOf(const LocalOrPublishedClass& cls, diag::SourceSpan span)
      -> diag::Result<ClassParents>;

  // The declaring unit and canonical name of `cls`, which identify it whichever
  // way it was read.
  [[nodiscard]] auto NameOf(const LocalOrPublishedClass& cls) const
      -> hir::ExternalClassRef;

  // The names of the methods the interface class `iface` declares.
  auto MethodNamesOf(const LocalOrPublishedClass& iface)
      -> std::vector<std::string>;

  // Appends the interface class `iface`, then every interface class it extends
  // (LRM 8.26.2), skipping one already in `reached`: an interface class is one
  // however many ways it is arrived at (LRM 8.26.6.3). What each extends is
  // read where that class states it, so one of another unit is its signature
  // consumed.
  auto ReachInterface(
      const LocalOrPublishedClass& iface, diag::SourceSpan span,
      std::vector<LocalOrPublishedClass>& reached) -> diag::Result<void>;

  // Every interface class a value of `cls` is also a value of (LRM 8.26.5):
  // the ones the class it extends is (LRM 8.26), then the ones `cls` names and
  // the ones those extend.
  auto AllInterfacesOf(const LocalOrPublishedClass& cls, diag::SourceSpan span)
      -> diag::Result<std::vector<LocalOrPublishedClass>>;

  // What the unit declaring the class of `method` published about it, or
  // nothing where the class is this unit's own, which leaves the declaration as
  // all there is to read.
  auto PublishedMethodOf(
      const slang::ast::SubroutineSymbol& method, diag::SourceSpan span)
      -> diag::Result<std::optional<hir::PublishedMethod>>;

  // Whether the class method `method` is one of the class rather than of an
  // object of it (LRM 8.10), so a call hands it no object.
  auto IsTypeAssociatedMethod(
      const slang::ast::SubroutineSymbol& method, diag::SourceSpan span)
      -> diag::Result<bool>;

  // Whether the constructor of `cls` declares any formal: what the declaring
  // unit published where another unit declares the class, and the class's own
  // declaration otherwise. Asked where the front end resolved no base call,
  // which it does exactly when every formal has a default value, so this is
  // whether a construction of `cls` as a base owes any default.
  auto ConstructorDeclaresFormals(
      const slang::ast::ClassType& cls, diag::SourceSpan span)
      -> diag::Result<bool>;

  // What a call to the class method `method` passes and awaits and what it
  // yields: what the declaring unit published where there is such a thing, and
  // the method's own declaration otherwise.
  auto ClassMethodPrototype(
      const slang::ast::SubroutineSymbol& method, diag::SourceSpan span)
      -> diag::Result<hir::PublishedCallable>;

  // For a class that is not an interface class, states which behavior of its
  // lineage answers each behavior of every interface class it is also a value
  // of. The answer is found by looking the behavior's name up in the class,
  // which is how the language finds it (LRM 8.26.2).
  auto StateConformance(
      const slang::ast::ClassType& cls, diag::SourceSpan span,
      hir::ClassDecl& decl) -> diag::Result<void>;

  // The behavior `method` states, with the class declaring it named the way
  // the boundary it sits on names one: what a method overriding it overrides,
  // and what an interface class's behavior is answered by.
  auto MakeOverriddenBehavior(
      const slang::ast::SubroutineSymbol& method, diag::SourceSpan span)
      -> diag::Result<hir::OverriddenBehavior>;

  // The behavior named `method_name` that `cls` answers, named by the class of
  // its lineage that introduced it. Refused where no signature up that lineage
  // publishes one, which leaves nothing to name the behavior by.
  auto MakeExternalDispatchSlot(
      const hir::ExternalClassRef& cls, std::string_view method_name,
      diag::SourceSpan span) -> diag::Result<hir::ExternalDispatchSlot>;

  // The same walk, answering nothing where no signature up the lineage
  // publishes the behavior.
  auto IntroducerOf(
      const hir::ExternalClassRef& cls, std::string_view method_name)
      -> std::optional<hir::ExternalDispatchSlot>;

  // The interface a subroutine's own declaration states: its call protocol and
  // each formal's direction and type (LRM 13.5), in this unit's types. It is
  // what a unit publishes about a subroutine it declares, and what a call to
  // one of this unit's own namespace subroutines takes.
  auto MakeExternalCalleeInterface(
      const slang::ast::SubroutineSymbol& sym, diag::SourceSpan span)
      -> diag::Result<hir::ExternalCalleeInterface>;

  // The interface a call to `sym` is made through, where `sym` is declared in
  // the namespace of the unit named `unit_name` (LRM 26.3). Another unit's is
  // read off that unit's signature; this unit's own is its own declaration.
  auto NamespaceCalleeInterface(
      const std::string& unit_name, const slang::ast::SubroutineSymbol& sym,
      diag::SourceSpan span) -> diag::Result<hir::ExternalCalleeInterface>;

  // The instance-property peer of `MakeClassMethodTarget`. Local when the class
  // was interned by this unit; external when the class lives in another
  // compilation unit, in which case the property is named by its position in
  // what that class published.
  [[nodiscard]] auto MakeClassPropertyTarget(
      const slang::ast::ClassType& owner,
      const slang::ast::ClassPropertySymbol& prop, diag::SourceSpan span)
      -> diag::Result<hir::ClassPropertyTarget>;

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

  [[nodiscard]] auto Specialization() const -> const SpecializationPolicy& {
    return facts_.Specialization();
  }

  // Where the value of a parameter this unit's scopes declare comes from, for
  // the objects built from the scope declaring it. Only a design element is
  // built once per instance, so a package's parameters are all fixed by its
  // specialization.
  [[nodiscard]] auto ValueSourceOf(
      const slang::ast::ParameterSymbol& param) const -> ParameterValueSource;

  void MapStructuralDataObjectBinding(
      const slang::ast::ValueSymbol& var, ScopeFrameId home_frame,
      hir::StructuralDataObjectId local);
  // The identity the declaration pass reserved for a variable or net this
  // unit declares, which the walk building that declaration defines.
  [[nodiscard]] auto ReservedDataObject(const slang::ast::ValueSymbol& declared)
      const -> hir::StructuralDataObjectId;
  [[nodiscard]] auto LookupStructuralDataObjectBinding(
      const slang::ast::ValueSymbol& var) const
      -> std::optional<StructuralDataObjectBinding>;

  void MapInterfacePortBinding(
      const slang::ast::InterfacePortSymbol& port, ScopeFrameId home_frame,
      hir::InterfacePortId local, hir::ExternalScopeClassId scope_class);
  [[nodiscard]] auto LookupInterfacePortBinding(const slang::ast::Symbol& port)
      const -> std::optional<InterfacePortBinding>;

  void MapSubroutineBinding(
      const slang::ast::SubroutineSymbol& sym, ScopeFrameId owner_frame,
      hir::StructuralSubroutineId local);
  [[nodiscard]] auto LookupSubroutineBinding(
      const slang::ast::SubroutineSymbol& sym) const
      -> std::optional<SubroutineBinding>;

  // The subroutine this unit evaluates a declaration's expression in, for
  // another unit that asks for the value because the expression names
  // declarations it cannot see: a name a view offers only for reading (LRM
  // 25.5.4), and a port's default (LRM 23.2.2.4). The identity is minted with
  // every other structural identity so the signature can name it before any
  // body is lowered.
  void MapEvaluator(
      const slang::ast::Symbol& holder, hir::StructuralSubroutineId evaluator);
  [[nodiscard]] auto EvaluatorOf(const slang::ast::Symbol& holder) const
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
      hir::OwnedChildStep step);
  [[nodiscard]] auto LookupOwnedChildBinding(const slang::ast::Symbol& child)
      const -> std::optional<OwnedChildBinding>;
  // The identity the declaration pass gave a generate construct, or an
  // instance, of this unit; that pass gives one to every such child.
  [[nodiscard]] auto GenerateIdOf(const slang::ast::Symbol& construct) const
      -> hir::GenerateId;
  [[nodiscard]] auto InstanceMemberIdOf(
      const slang::ast::Symbol& instance) const -> hir::InstanceMemberId;

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

  // The routes gathered for `owner_frame`, the scope whose names take them and
  // which each of them is counted from, wherever its base lies. Handed over
  // once, when the scope is finished.
  auto RoutesOf(ScopeFrameId owner_frame) -> hir::ScopeRoutes&;
  auto TakeRoutesForFrame(ScopeFrameId owner_frame) -> hir::ScopeRoutes;

  // The compilation-unit declaration pass (LRM 23.6 / 23.9 / 27): before any
  // executable body lowers, walk the whole unit's scope tree and mint every
  // declaration a peer body may reference -- owned children (instance,
  // generate block, generate array), subroutines, and the static-lifetime body
  // locals a named block puts on the hierarchical path -- assigning each scope
  // its frame along the way. A body or sensitivity read then resolves any of
  // them regardless of which sibling scope or body lowered first. Registers no
  // executable HIR. What each scope publishes is recorded in the same walk,
  // under `class_name`, the name of the class an object of the scope is.
  void DeclareStructuralIdentities(
      const slang::ast::Scope& scope, std::string class_name);

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

  // Whether a design element other than this unit declares `cls`, so that an
  // object of it belongs to an instance of that element, which no count of
  // this unit's own scopes reaches.
  [[nodiscard]] auto DeclaredByAnotherDesignElement(
      const slang::ast::ClassType& cls) const -> bool;

  // Whether a construction of `cls` and a call of a method of the class itself
  // are handed the instance it belongs to, as the class states it: its own
  // declaration where this unit declares it, and its signature otherwise.
  auto TakesDeclaringInstance(
      const slang::ast::ClassType& cls, diag::SourceSpan span)
      -> diag::Result<bool>;

  // How far out of `frame`'s own structural scope the instance an object of
  // `cls` belongs to sits, for a class this unit declares in one of its
  // structural scopes.
  [[nodiscard]] auto DeclaringScopeHopsFrom(
      const slang::ast::ClassType& cls, const WalkFrame& frame,
      diag::SourceSpan span) const -> diag::Result<hir::StructuralHops>;

  // How a body at `frame` reaches the instance an object of `cls` belongs to,
  // for a construction or a call of a method of the class itself written
  // there, or nothing where the class takes no such instance: as above where
  // this unit declares the class, and by a route to the declaring scope's
  // object where another design element does (LRM 6.22).
  [[nodiscard]] auto DeclaringInstanceFrom(
      const slang::ast::ClassType& cls, const WalkFrame& frame,
      diag::SourceSpan span)
      -> diag::Result<std::optional<hir::DeclaringInstanceReach>>;

  // What `scope` published, in this unit's own identities: asked once the walk
  // lowering the scope has given every declaration it published its identity.
  // A scope this unit published nothing of -- a namespace unit's -- takes the
  // empty publication.
  [[nodiscard]] auto TakePublication(const slang::ast::Scope& scope)
      -> hir::ScopePublication;

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

  // Interns every unpacked structure a typedef of this unit declares outside a
  // body, in the same scopes classes are declared in, so the structure's type
  // -- and with it the operations its declaration brings -- is in the unit that
  // declares it whatever the unit's bodies use.
  auto InternOwnStructureDeclarations(const slang::ast::Scope& scope)
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

  // Reads the signature of every class of another unit this one named, once
  // every body has lowered and so every name is in: a value of such a class is
  // converted to the views it has wherever it is held, which takes its layout
  // whether or not a member of it is ever reached -- as a C++ translation unit
  // includes the header of every class it names.
  void ReadSignaturesOfNamedClasses();

  // Builds a HIR Expr referring to the data `route` navigates to.
  // `owner_frame` is the frame whose routes hold it.
  auto MakeRoutedMemberRef(
      ScopeFrameId owner_frame, hir::ValueRoute route, diag::SourceSpan span)
      -> hir::Expr;

  // The reference to `value` over a route the caller derived: the route says
  // how the reader reaches it, and what the route ends at follows from the
  // route alone.
  [[nodiscard]] auto MakeRoutedValueRef(
      const slang::ast::ValueSymbol& value, ScopeFrameId owner_frame,
      ScopeRoute route) -> diag::Result<hir::RoutedValueRef>;

  // The reference to the object `route` reaches. Reaching an object across an
  // instance boundary seals like reaching a cell there: the route runs once at
  // elaboration and what it lands on is read directly after, so a caller
  // holding this reaches the object with no traversal of its own.
  [[nodiscard]] auto MakeRoutedObjectRef(
      ScopeFrameId owner_frame, ScopeRoute route, hir::TypeId object_type)
      -> hir::RoutedObjectRef;

  // What a `disable` naming `target` by `reference` terminates (LRM 9.6.2,
  // 23.6). A target the body's own declaration scope declares is named by its
  // identity there. Any other is reached over a route: where this unit lays
  // out the scope that declares `target`, to that scope, carrying its identity
  // for the block; otherwise to the scope of another unit declaring it, naming
  // the position that scope published it at.
  [[nodiscard]] auto DisableTargetOf(
      const WalkFrame& frame, const slang::ast::Symbol& target,
      const slang::ast::HierarchicalReference& reference, diag::SourceSpan span)
      -> diag::Result<hir::DisableTarget>;

  // Where a declaration's cell lives, as this unit reaches it. One answer
  // serves every consumer of a reference -- reading it, writing it, and waiting
  // on it changing -- so no consumer can make one symbol a value to itself and
  // a signal to the next. A cell on an instance resolves to a route from the
  // reader to it; one a namespace unit owns, having
  // no instance to route through, resolves to its name across the boundary.
  //
  // The caller states that the name denotes storage, by having classified it,
  // so there is no answer here for a name that denotes something else. A route
  // this compiler does not yet build is a refusal rather than an answer: it is
  // a property of the compiler rather than of the name.
  [[nodiscard]] auto ResolveValueTarget(
      const WalkFrame& frame, const slang::ast::ValueSymbol& value,
      RouteOrigin origin, diag::SourceSpan span)
      -> diag::Result<hir::ValueTarget>;

  // The value a name denotes, where elaboration fills that value rather than
  // having folded it: a loop generate's index (LRM 27.4), and a parameter
  // whose value differs between the objects built from one scope -- a
  // generate block's, or a unit's that its instance is handed or works out
  // from what it is handed (LRM 23.10.2). The front end spells each as a
  // constant, so what separates them from an ordinary one is that this unit
  // declared somewhere to put it. Absent for every other name, including one of
  // another unit, whose own lowering answered this for itself.
  [[nodiscard]] auto NameFilledDuringElaboration(
      const slang::ast::Symbol& named) const -> const slang::ast::ValueSymbol*;

  // Where a static class property's cell lives (LRM 8.9). It belongs to the
  // type rather than to any object of it, so it is reached without a receiver,
  // and how many such cells exist follows from what replicates the class
  // declaration: one a design element's scope replicates is a cell of that
  // scope's instance, reached by a route like any of its declarations. Separate
  // from the resolution above because the caller knows which of the two it is
  // asking about, having classified the name.
  [[nodiscard]] auto ResolveStaticPropertyTarget(
      const WalkFrame& frame, const slang::ast::ClassPropertySymbol& prop,
      diag::SourceSpan span) -> diag::Result<hir::ValueTarget>;

  // The walk below, with the one reason that walk can fail written where it is
  // known rather than at whichever caller met it.
  [[nodiscard]] auto RouteToScopeOrRefuse(
      const WalkFrame& frame, const slang::ast::Scope& target,
      RouteOrigin origin, diag::SourceSpan span) -> diag::Result<ScopeRoute>;

  // How this reader reaches `target`, a scope elsewhere on the elaborated
  // hierarchy, from `origin`: the base it anchors at and the descent from
  // there, with each step typed where this unit declares what it lands on and
  // resolved against what the scope above it published where it does not.
  // Empty when no route reaches the scope, never a compiler-bug invariant --
  // either the walk found a target form this unit cannot yet express, or the
  // scope sits in a namespace unit, which has no instance and so nothing on the
  // object tree a route could walk to at all.
  [[nodiscard]] auto RouteToScope(
      const WalkFrame& frame, const slang::ast::Scope& target,
      RouteOrigin origin) -> std::optional<ScopeRoute>;

  // Where `reference`, written in this unit, starts: through an interface port
  // or upward at the enclosing instance it landed in, where it leaves the
  // unit's instance, and at the reader for a name that never leaves and for
  // every name a namespace unit writes, since a namespace has no instance to
  // leave. A name through a port that leaves several of the instances behind
  // it in play has no one place to start, and is refused at `span`.
  [[nodiscard]] auto StartOf(
      const WalkFrame& frame,
      const slang::ast::HierarchicalReference& reference, diag::SourceSpan span)
      -> diag::Result<RouteOrigin>;

  // Where a route to `target` starts when no name says so -- a scope reached
  // because a type belongs to it: at the reader where `target` stands in this
  // unit's instance or below it, and otherwise at the instance one of this
  // unit's names lands in once it leaves, or at the enclosing instance a type
  // handed down came from. The anchor is the same in every instance this unit
  // serves, since what tells the unit apart is where those names land. Nothing
  // where none of these applies, which leaves no route to the scope.
  [[nodiscard]] auto StartReaching(const slang::ast::Scope& target)
      -> std::optional<RouteOrigin>;

  // A route starting at the enclosing instance `body` is, which stands for the
  // object of whichever unit that instance is.
  [[nodiscard]] auto StartInEnclosing(
      const slang::ast::InstanceBodySymbol& body) -> RouteStart;

  // How this reader reaches the scope whose instance `cls` belongs to (LRM
  // 6.22), where another design element declares it.
  [[nodiscard]] auto RouteToClassScope(
      const WalkFrame& frame, const slang::ast::ClassType& cls)
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

  // How this reader reaches the objects a port connection's actual names when
  // that actual goes through one of the reader's own interface ports (LRM
  // 25.3, 23.3.3.4): the port is the first step and says which unit the
  // descent starts in; each hop after it either carries a coordinate on the
  // hop before it -- already the position the select resolved to, since
  // spending the declared range is what resolving it does -- or names one more
  // step down, resolved against what the scope standing above it published. A
  // coordinate the path did not write stays open, and a part it selected
  // narrows the outermost one, because a port is handed on whole or in part
  // and both name several objects at once; a single name reaches one object and
  // is routed as any name is. Nothing when the path is of a shape the walk does
  // not take.
  [[nodiscard]] auto ReachThroughInterfacePort(
      const WalkFrame& frame,
      const slang::ast::HierarchicalReference& reference)
      -> std::optional<ScopeRoute>;

  // A set of accesses as the entries naming what each reaches -- the reads a
  // wait watches, or the writes an implicit list leaves out. What each access
  // contributes follows from what its name denotes: a value fixed before
  // simulation starts contributes nothing, so a constant read alongside a
  // signal leaves only the signal watched (LRM 9.2.2.2.1). An access this
  // compiler cannot name is refused, because a process that does not wake
  // answers wrongly and shows nothing.
  //
  // An access to part of a bit vector names that part as the source selected
  // it, lowered by `lowerer` into the arena `frame` adds to, so an index that
  // is a value each construction is given stays that value.
  template <typename Lowerer>
  [[nodiscard]] auto SensitivityEntriesOf(
      Lowerer& lowerer, const std::vector<AccessedPart>& reads,
      const WalkFrame& frame)
      -> diag::Result<std::vector<hir::SensitivityEntry>>;

  // The cells a set of reads names, whatever part of each it reads: what a
  // sampled value arms is the whole cell (LRM 16.5.1). A read naming no cell
  // this scope can reach contributes none, which the caller counts.
  [[nodiscard]] auto CellsRead(
      const std::vector<AccessedPart>& reads, const WalkFrame& frame)
      -> diag::Result<std::vector<hir::ValueTarget>>;

  // The descent from the scope class `from` records, which stands for the scope
  // `from_scope` of another unit, down to `to`, which stands in it: every hop
  // resolved against what the scope above it published. Empty where `to` does
  // not stand in `from_scope`, or a scope on the way published nothing the
  // descent names.
  [[nodiscard]] auto DescendPublished(
      hir::ExternalScopeClassId from, const slang::ast::Scope& from_scope,
      const slang::ast::Scope& to) -> std::optional<PublishedDescent>;

 private:
  // The instance this unit lowers and where each name it writes lands once it
  // leaves that instance, or nothing for a namespace unit, which has no
  // instance for a name to leave.
  struct ReaderClimbs {
    const slang::ast::InstanceBodySymbol* body = nullptr;
    std::span<const ClimbAnchor> climbs;
  };
  [[nodiscard]] auto ReaderInstance() const -> std::optional<ReaderClimbs>;

  // Derives what this unit publishes from its own declarations: the object an
  // instance of it is and the class each generate block it elaborates is, each
  // with a member per declaration another unit may name, and one entry per
  // port, whose parts the instantiating unit's connections are consumed in step
  // with. Every type is interned by this unit and then taken
  // into the signature's own pool, so what leaves stands on its own.
  auto PublishSignature() -> diag::Result<void>;

  // Derives what this unit publishes about each class of the source language it
  // declares: the properties another unit may name, in the order that fixes
  // their slots, and the behaviors the class introduces, in the order that
  // fixes their ordinals. Every class is already minted when this runs, so this
  // reads the unit's own declarations rather than the frontend's tree.
  void PublishClassSignatures();

  // Derives what this unit publishes about each subroutine its namespace
  // declares (LRM 26.3). A design element declares none another unit calls by
  // name alone, so it publishes none here.
  auto PublishNamespaceSubroutines() -> diag::Result<void>;

  // How this reader reaches the interface an enclosing scope's `port` carries
  // (LRM 25.3). The port is the whole of this unit's reach to it: what stands
  // behind it belongs to a unit this one reaches no other way, so any other
  // route to the same object would describe a different design -- which is why
  // the frontend's own resolution of the port to that object is not what this
  // reads. Where the route goes from there is the caller's, so one derivation
  // serves a name read through the port and a connection handing the port's
  // interface on.
  [[nodiscard]] auto ReachOfPort(
      const WalkFrame& frame, const slang::ast::InterfacePortSymbol& port) const
      -> PortReach;

  // Fills `route` with the descent `hops` state, resolving each hop this unit
  // declares nothing about against what the scope standing above it published,
  // and stating which scope the whole route lands on. Both follow the descent
  // forward, since which scope stands at a hop is what every hop before it
  // decided. False where a hop names nothing that scope published, which a
  // name the front end resolved reaches only where the design publishes less
  // than it declares.
  [[nodiscard]] auto ClassifyDescent(
      ScopeRoute& route, std::span<DescentHop> hops) -> bool;

  // The same walk from wherever it starts, `standing`. The hops this unit
  // declares come first, and the rest step through published scope classes.
  [[nodiscard]] auto DescendFrom(
      RoutePlace standing, std::span<DescentHop> hops)
      -> std::optional<Descent>;

  // The part of a descent that steps through published scope classes, from the
  // one `standing` records: each hop resolved against what the class standing
  // above it published.
  [[nodiscard]] auto DescendPublishedFrom(
      hir::ExternalScopeClassId standing, std::span<NamedHop> hops)
      -> std::optional<PublishedDescent>;

  // What a process waiting on a name a modport offers observes: every member
  // the expression behind that name reads (LRM 25.5.4), each reached the way
  // the interface was, from `origin`. The name is no storage of its own, so
  // nothing waits on it directly.
  auto ObservedThroughModport(
      const slang::ast::ModportPortSymbol& offered, const WalkFrame& frame,
      RouteOrigin origin) -> diag::Result<std::vector<hir::SensitivityEntry>>;

  // Where each of `names`, the names a read was reached by, starts: once per
  // name leaving the reader's instance, and at the reader, once, for every
  // name that stays inside it, as for a read put together with no names at
  // all.
  [[nodiscard]] auto StartsOfNames(
      const WalkFrame& frame,
      std::span<const slang::ast::Expression* const> names,
      diag::SourceSpan span) -> diag::Result<std::vector<RouteOrigin>>;

  // The entries a set of reads watches, with `parts_of` saying which parts of
  // a bit vector a read of one watches and `declared_by` which variable of the
  // reading body a name binds, where it binds one.
  template <typename PartsOf, typename DeclaredBy>
  auto WatchedEntriesOf(
      const std::vector<AccessedPart>& reads, const WalkFrame& frame,
      PartsOf parts_of, DeclaredBy declared_by)
      -> diag::Result<std::vector<hir::SensitivityEntry>>;

  // The reader-relative route to a cell in an instantiated scope: a count of
  // parent edges when the target sits on the reader's own scope or one
  // enclosing it in this unit, then a descent through children this unit
  // declares and through what each scope of another unit published.
  [[nodiscard]] auto TranslateReferenceRoute(
      const WalkFrame& frame, const slang::ast::ValueSymbol& value,
      RouteOrigin origin) -> diag::Result<std::optional<hir::RoutedValueRef>>;

  // What `route` reaches: a member the scope it landed on published, or this
  // unit's own declaration where it stays in this unit's layout.
  [[nodiscard]] auto ResolveRouteTarget(
      const slang::ast::ValueSymbol& value, const ScopeRoute& route)
      -> diag::Result<hir::DataLeaf>;

  // Reserves an identity for each static-lifetime local one procedural block
  // subtree of `body` declares, and for each constant it declares whose value
  // differs between the objects built from this unit, and recurses into the
  // blocks nested in it.
  // Every scope is walked the same way and contributes however many statics it
  // holds, none excluded: whether a hierarchical path can reach a given one is
  // the frontend's question, already answered before any reference gets here,
  // so nothing is held back on the chance that nothing will name it. Only the
  // identity is minted -- what is declared, including the initializer, is an
  // expression of the body and is filled when that body lowers.
  //
  // What a name reaches here from elsewhere is published under the path of
  // named scopes `within` spells (LRM 23.9): each static-lifetime variable,
  // and each named block as what a `disable` of it ends (LRM 9.6.2). A block
  // the source left unnamed puts nothing below it on any path, which is what
  // an absent path says.
  void DeclareProceduralStatics(
      const slang::ast::Scope& block, const slang::ast::Symbol& body_symbol,
      hir::ProceduralBodyRef body, ScopeFrameId frame,
      ScopePublicationRecord& published,
      const std::optional<std::vector<std::string>>& within);

  // The identities one member of a scope brings, minted into `decls`, the
  // scope's own, at `frame`, the scope's frame, and what of it the scope
  // publishes, appended to `published`.
  void DeclareMemberIdentities(
      const slang::ast::Symbol& member, ScopeDeclarations& decls,
      ScopePublicationRecord& published, ScopeFrameId frame);
  void DeclareConditionalGenerate(
      const slang::ast::GenerateBlockSymbol& block, ScopeDeclarations& decls,
      ScopePublicationRecord& published, ScopeFrameId frame);
  void DeclareLoopGenerate(
      const slang::ast::GenerateBlockArraySymbol& array,
      ScopeDeclarations& decls, ScopePublicationRecord& published,
      ScopeFrameId frame);
  void DeclareSubroutine(
      const slang::ast::SubroutineSymbol& sub, ScopeDeclarations& decls,
      ScopePublicationRecord& published, ScopeFrameId frame);
  void DeclareModportEvaluators(
      const slang::ast::ModportSymbol& modport, ScopeDeclarations& decls,
      ScopePublicationRecord& published);
  void DeclareProcess(
      const slang::ast::ProceduralBlockSymbol& proc, ScopeDeclarations& decls,
      ScopePublicationRecord& published, ScopeFrameId frame);
  // A class declared in a scope is a type of each instance of it (LRM 6.22),
  // so what the class `declared` keeps for itself (LRM 8.9) is a cell the
  // scope publishes, and so is what each class declared inside it keeps.
  // Anything else declares no class and keeps nothing.
  static void DeclareClassStatics(
      const slang::ast::Symbol& declared, ScopePublicationRecord& published);

  // The class of the scope `published` records, in this unit's own types.
  auto PublishScopeClass(const ScopePublicationRecord& published)
      -> diag::Result<hir::ScopeClassSignature>;

  // A published callable as this unit's own types state it.
  auto PublishedCallableOf(const ScopePublicationRecord::Callable& callable)
      -> diag::Result<hir::PublishedCallable>;

  // What the identity walk recorded for `scope`.
  [[nodiscard]] auto PublicationOf(const slang::ast::Scope& scope) const
      -> const ScopePublicationRecord&;

  // Whether a parameter's value differs between the objects built from the
  // scope declaring it: supplied or computed at construction, rather than fixed
  // by the specialization.
  [[nodiscard]] auto DiffersPerObject(
      const slang::ast::ParameterSymbol& param) const -> bool;

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
  // What each class this unit declares would publish to another unit, taken
  // where that class's own arenas are built so the two count the same
  // positions. Only the classes the unit's namespace declares reach its
  // signature.
  std::unordered_map<const slang::ast::ClassType*, hir::ClassSignature>
      own_class_signatures_;
  // Which record this unit made of each referenced scope class, by unit and
  // class, so every reference into one names the same entry.
  std::map<std::pair<std::string, std::string>, hir::ExternalScopeClassId>
      external_scope_classes_;
  // What each of this unit's interface ports stands for.
  std::unordered_map<const slang::ast::Symbol*, hir::TypeId>
      interface_port_types_;
  // The declarations this unit's `ref` ports reach, under the binding each
  // port's direction states.
  std::unordered_map<const slang::ast::Symbol*, hir::ReferenceBinding>
      ref_port_internals_;
  // What each scope of this unit publishes, recorded by the walk minting the
  // unit's identities and read by the signature and by the walk lowering that
  // scope; and the scopes in the order that walk reached them, the unit's own
  // first, so the signature states them in an order that does not depend on
  // where the front end allocated them.
  std::unordered_map<const slang::ast::Scope*, ScopePublicationRecord>
      scope_publications_;
  std::vector<const slang::ast::Scope*> publishing_scopes_;
  // The class each of those scopes published, in this unit's own types, which
  // the signature states in its own and the walk lowering the scope hands on.
  std::unordered_map<const slang::ast::Scope*, hir::ScopeClassSignature>
      scope_classes_;

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
      evaluators_;
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
  std::map<ScopeFrameId, hir::ScopeRoutes> routes_by_frame_;
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
