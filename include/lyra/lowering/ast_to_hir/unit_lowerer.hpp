#pragma once

#include <cstdint>
#include <functional>
#include <map>
#include <optional>
#include <span>
#include <string>
#include <string_view>
#include <type_traits>
#include <unordered_map>
#include <utility>
#include <variant>
#include <vector>

#include <slang/ast/HierarchicalReference.h>
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
#include "lyra/hir/class_ref.hpp"
#include "lyra/hir/compilation_unit.hpp"
#include "lyra/hir/expr.hpp"
#include "lyra/hir/field_id.hpp"
#include "lyra/hir/method_id.hpp"
#include "lyra/hir/pattern_id.hpp"
#include "lyra/hir/published_callable.hpp"
#include "lyra/hir/specialization.hpp"
#include "lyra/hir/specialization_ref.hpp"
#include "lyra/hir/stmt.hpp"
#include "lyra/hir/structural_data_object.hpp"
#include "lyra/hir/structural_scope.hpp"
#include "lyra/hir/subroutine_prototype.hpp"
#include "lyra/hir/type_import.hpp"
#include "lyra/hir/unit_signature.hpp"
#include "lyra/hir/unit_signatures.hpp"
#include "lyra/hir/value_ref.hpp"
#include "lyra/lowering/ast_to_hir/built_as.hpp"
#include "lyra/lowering/ast_to_hir/climb.hpp"
#include "lyra/lowering/ast_to_hir/coverage_library.hpp"
#include "lyra/lowering/ast_to_hir/declaration_scopes.hpp"
#include "lyra/lowering/ast_to_hir/implementations.hpp"
#include "lyra/lowering/ast_to_hir/instance_array_shape.hpp"
#include "lyra/lowering/ast_to_hir/sensitivity.hpp"
#include "lyra/lowering/ast_to_hir/std_classes.hpp"
#include "lyra/lowering/ast_to_hir/unit_identity.hpp"
#include "lyra/lowering/ast_to_hir/walk_frame.hpp"
#include "lyra/stable_hash/fingerprint.hpp"
#include "lyra/support/assertion_policy.hpp"
#include "lyra/support/def_path.hpp"

namespace slang::ast {
class ClockingBlockSymbol;
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
class SignalEventControl;
class TimingControl;
class ValueSymbol;
class VariableSymbol;
}  // namespace slang::ast

namespace lyra::lowering::ast_to_hir {

class StructuralScopeLowerer;
struct BuiltScope;

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
// declares it, and its identity there. Which kind of instance is bound at each
// of its positions is what the port's type states.
struct InterfacePortBinding {
  ScopeFrameId home_frame{};
  hir::InterfacePortId port{};
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

// A block of a generate construct this unit declares, by its construct (LRM
// 27). Which body the block is, is its object's construction data, so a path
// reaches it through what the scope holding the construct published.
struct DeclaredGenerateBlock {
  hir::GenerateId generate;
};

// What a child of a scope this unit declares is, to a path element reaching
// it: an instance or instance-array member (`c.x`, `c[1].x`), or a block of a
// generate construct (`g[1].x`, `b.x`).
using DeclaredOwnedChild =
    std::variant<hir::InstanceMemberId, DeclaredGenerateBlock>;

// A component of a hierarchical path names an owned child of some scope this
// unit declares. The child's slang symbol maps to the declaring scope's
// identity for it, so the reference resolves regardless of whether it
// precedes the child in source.
struct OwnedChildBinding {
  ScopeFrameId home_frame{};
  DeclaredOwnedChild child;
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
// elaborated design and is what a reader means by the scope.
struct MintedProceduralScope {
  const slang::ast::Scope* owner = nullptr;
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
  // published as a declaration of `within`: the subroutine or named block of
  // the unit declaring it.
  struct LocalStatic {
    const slang::ast::VariableSymbol* symbol = nullptr;
    hir::ProceduralBodyRef body;
    hir::ProceduralVarId var;
    support::DefPath within;
  };
  // A static property of a class the scope declares (LRM 6.22, 8.9).
  struct ClassStatic {
    const slang::ast::ClassPropertySymbol* symbol = nullptr;
    const slang::ast::ClassType* owner = nullptr;
  };
  // The block event of a covergroup the scope declares (LRM 19.3), whose
  // identity the covergroup is minted with.
  struct CovergroupEvent {
    const slang::ast::CovergroupType* symbol = nullptr;
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
      DataObject, LocalStatic, ClassStatic, CovergroupEvent, Instance,
      InterfacePort>;

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
  // The subroutine evaluating a formal's default (LRM 13.5.3), for a call that
  // omits the formal.
  struct ArgumentDefault {
    const slang::ast::SubroutineSymbol* subroutine = nullptr;
    const slang::ast::FormalArgumentSymbol* formal = nullptr;
    hir::StructuralSubroutineId id;
  };
  // The subroutine evaluating the operand or the `iff` qualifier of one event
  // of a clocking block's clocking event (LRM 14.3, 9.4.2.3), for a unit
  // clocking a sequence or property the block declares (LRM 14.13, 16.16).
  struct ClockingExpression {
    const slang::ast::ClockingBlockSymbol* block = nullptr;
    const slang::ast::Expression* expression = nullptr;
    std::string name;
    hir::StructuralSubroutineId id;
  };
  using Callable = std::variant<
      Subroutine, PortDefault, ViewRead, ArgumentDefault, ClockingExpression>;

  // A loop generate, with whatever blocks it counted out (LRM 27.4).
  struct Loop {
    hir::GenerateId id;
    const slang::ast::GenerateBlockArraySymbol* loop = nullptr;
  };
  // A conditional generate (LRM 27.5): where the source wrote it among the
  // generate constructs of its scope, and the alternatives this elaboration
  // built a scope for.
  struct Choice {
    hir::GenerateId id;
    std::uint32_t construct = 0;
    std::vector<const slang::ast::GenerateBlockSymbol*> built;
  };
  using Construct = std::variant<Loop, Choice>;

  // A named block or task a `disable` ends (LRM 9.6.2), and which declaration
  // of the unit it is.
  struct DisableTarget {
    const slang::ast::Symbol* symbol = nullptr;
    support::DefPath path;
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

  // A named clocking block of the scope (LRM 14.3): each event of its clocking
  // event, in the order its event list wrote them, with the callables
  // evaluating its operand and its qualifier.
  struct ClockingEntry {
    const slang::ast::SignalEventControl* event = nullptr;
    hir::PublishedCallableId operand;
    std::optional<hir::PublishedCallableId> condition;
  };
  struct Clocking {
    const slang::ast::ClockingBlockSymbol* block = nullptr;
    std::vector<ClockingEntry> events;
  };

  // Which class of the unit an object of the scope is: each generate block
  // down to this scope as the application of its definition it is, and no step
  // for the unit's own scope.
  support::DefPath class_path;
  std::vector<Member> members;
  std::vector<Callable> callables;
  std::vector<Construct> generates;
  std::vector<DisableTarget> disable_targets;
  std::vector<View> views;
  std::vector<Clocking> clockings;

  // Where the member standing for `declared` sits among the members, or
  // nothing where the scope publishes no member for it.
  [[nodiscard]] auto MemberOf(const slang::ast::Symbol& declared) const
      -> std::optional<hir::PublishedMemberId>;

  // Where the callable evaluating the expression `holder` holds sits among
  // the callables, or nothing where the scope publishes none for it.
  [[nodiscard]] auto CallableOf(const slang::ast::Symbol& holder) const
      -> std::optional<hir::PublishedCallableId>;
};

// Adjacent positions kept out of one dimension, counted the way that
// dimension's declared range counts positions. It names no dimension of its
// own: a part select is written before the walk that reads it knows what the
// path lands on, and keeping every position is the positions spanning the
// whole of whatever that turns out to be.
struct KeptPositions {
  std::uint32_t first;
  std::uint32_t count;
};

// One dimension of a declaration standing for objects, and which of its
// positions are in play (LRM 23.3.3.4, 23.3.3.5). So a port that binds every
// object it stands for and a name that selected a part of one are the same
// statement at two widths.
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
// where what it reaches is this unit's own declaration, or on a scope reached
// through what it published -- one of another unit, or a generate block of
// this one -- where what it reaches is counted out of that publication.
struct InOwnScope {};

// A scope a walk stands on through what it published: this unit's record of
// what that scope published, and the named blocks and subroutines of it the
// name went on through, outermost first (LRM 23.9). Those are no objects, so
// they take no step; a declaration the name reaches there is one the scope
// published as held by the scope those lead to.
struct InPublishedScope {
  hir::ScopeClassId scope_class;
  std::vector<support::DefPathData> within;
};

// The walk stands on several objects of another unit: a set it stepped onto
// with a dimension still unselected. It is no one scope, so no name resolves
// past it until a select picks one object out (LRM 23.6); what it reaches is
// the set, as a connection binding a port to several instances does.
struct OnSeveralObjects {};

using RoutePlace = std::variant<InOwnScope, InPublishedScope, OnSeveralObjects>;

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

// A hop of a descent this unit declares nothing about, as the front end says
// the name reached it: an instance, with the coordinates the name wrote for
// it; a block of a loop generate, by the label of the loop, the value the
// front end resolved its select to, which says what the block was published
// as, and the select itself, evaluated in each object the name is read in
// (LRM 27.4); a block that stands alone or that a conditional chose, by where
// the source wrote its construct among those of its scope and the block among
// that construct's alternatives, since a label does not tell the alternatives
// of a conditional apart (LRM 27.5); or a named block or subroutine, which is
// no object (LRM 23.9). It is resolved against what the scope standing above
// it published, which is known only once the whole descent is in hand.
struct InstanceHop {
  std::string name;
  std::vector<std::uint32_t> indices;
};
struct LoopBlockHop {
  std::string loop;
  std::int64_t resolved_index;
  hir::ExprId position;
};
struct ChosenBlockHop {
  std::uint32_t construct;
  std::uint32_t position;
};
struct ProceduralHop {
  support::DefPathData step;
};
using NamedHop =
    std::variant<InstanceHop, LoopBlockHop, ChosenBlockHop, ProceduralHop>;

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
// only through what each published: it stands on one of them, or on several
// objects of a set it left a dimension of unselected.
struct PublishedDescent {
  std::vector<hir::PublishedStep> steps;
  RoutePlace place;
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
// upward starts at the enclosing scope it landed in (LRM 23.8) and needs no
// hop; one written through an interface port starts at the port and stands in
// the instance bound there (LRM 25.3).
struct RouteStart {
  const slang::ast::Scope* below = nullptr;
  hir::RouteBase base;
  std::optional<DeclaredHop> leading;
};

// The select a name wrote for a loop's block its route passes, lowered beside
// the route. Nothing where the name wrote none.
using LoopSelectOf = std::function<std::optional<hir::ExprId>(
    const slang::ast::GenerateBlockSymbol&)>;

// A scope while it is built: the lowerer building it and what it builds.
struct ScopeUnderConstruction {
  StructuralScopeLowerer* lowerer = nullptr;
  hir::StructuralScope* scope = nullptr;
};

// Where a route to a scope starts -- at the reader, for a name that stays
// inside the reader's instance and where no name is involved at all, or where
// a name that left it starts -- and the path the source wrote for it. Each
// select that path writes for a loop generate picks the block the route
// passes, in each object the route is walked from (LRM 23.6, 27.4); a route
// no name writes, to the scope a class belongs to, passes no loop it would
// have to pick a block of.
struct RouteOrigin {
  std::variant<FromReader, RouteStart> start;
  std::span<const slang::ast::HierarchicalReference::Element> written;
};

// Whether the declaration the front end resolved a name to is what the name
// reaches in every object of the scope being lowered: only for a name that
// stays inside the reader's instance and passes no generate block. One that
// left the instance reaches what stands where it lands in each instance (LRM
// 23.8), and which body a generate block is, is each object's own (LRM 27.4,
// 27.5), so for either the resolved declaration is the target only in the
// elaboration the front end resolved it in.
[[nodiscard]] auto NamesOwnDeclaration(const RouteOrigin& origin) -> bool;

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
  base::Registry<hir::ProceduralScopeDecl, hir::ProceduralScopeId>
      procedural_scopes;
};

// The specializations of a generic class a namespace declares that a design
// element holds, by the body of that element: each is specialized on a type an
// instance of it replicates (LRM 6.22, 8.25), so the element's own walk, which
// never reaches the namespace, is not where it finds them.
using PlacedSpecializations = std::unordered_map<
    const slang::ast::Symbol*, std::vector<const slang::ast::ClassType*>>;

// What every unit's lowering reads and none of them changes: the shared
// sensitivity analysis (one cache across the design), whether assertions are
// elided rather than rejected, what decides the name of every unit, which
// every party naming one reads, the specializations placed in a unit from a
// generic declared elsewhere, which only the design as a whole lists, and the
// class defining each class of `std` a design names and the coverage library's
// classes where the design asks for coverage, which only the design as a whole
// can make. The slang compilation itself is deliberately absent:
// walking top instances is the driver's job, and a unit lowering that could
// reach it would be able to read another unit.
class LoweringFacts {
 public:
  LoweringFacts(
      SensitivityAnalyzer& sensitivity_analyzer,
      support::AssertionPolicy assertion_policy, const UnitNamingPolicy& naming,
      const PlacedSpecializations& placed, const StdDefinitions& std_classes,
      const std::optional<CoverageLibrary>& coverage)
      : sensitivity_analyzer_(&sensitivity_analyzer),
        assertion_policy_(assertion_policy),
        naming_(&naming),
        placed_(&placed),
        std_classes_(&std_classes),
        coverage_(&coverage) {
  }

  // The specializations placed in the unit `home` is the body of.
  [[nodiscard]] auto PlacedIn(const slang::ast::Symbol& home) const
      -> std::span<const slang::ast::ClassType* const> {
    const auto it = placed_->find(&home);
    if (it == placed_->end()) return {};
    return it->second;
  }

  // The class Lyra defines for `cls`, a class `std` declares, or nothing where
  // Lyra defines none.
  [[nodiscard]] auto StdDefinitionOf(const slang::ast::ClassType& cls) const
      -> const slang::ast::ClassType* {
    const auto it = std_classes_->find(&cls);
    return it == std_classes_->end() ? nullptr : it->second;
  }

  // What every covergroup's class is written over (LRM 19). The compilation
  // reads it wherever a covergroup or a coverage function is written, and only
  // those ask for it.
  [[nodiscard]] auto Coverage() const -> const CoverageLibrary& {
    if (!coverage_->has_value()) {
      throw InternalError(
          "a design asking for coverage was compiled without the coverage "
          "library -- please report this as a bug");
    }
    return **coverage_;
  }

  [[nodiscard]] auto Sensitivity() const -> SensitivityAnalyzer& {
    return *sensitivity_analyzer_;
  }

  [[nodiscard]] auto AssertionPolicy() const -> support::AssertionPolicy {
    return assertion_policy_;
  }

  [[nodiscard]] auto Naming() const -> const UnitNamingPolicy& {
    return *naming_;
  }

 private:
  SensitivityAnalyzer* sensitivity_analyzer_;
  support::AssertionPolicy assertion_policy_;
  const UnitNamingPolicy* naming_;
  const PlacedSpecializations* placed_;
  const StdDefinitions* std_classes_;
  const std::optional<CoverageLibrary>* coverage_;
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
  // The unit holding the declarations whose own source `home` is: a design
  // element's body, a package, a `$unit` scope, or a specialization that is a
  // unit of its own, which is lowered from the scope declaring its generic and
  // holds nothing of that scope but itself.
  //
  // A design element's body is one text every application of it shares, and
  // `home` is the body of one of them. `checked_against` holds the bodies of
  // others, which the body is lowered from as well and has to come out the
  // same from: what is compiled is a function of the body's name, so an
  // application lowering to anything else is a defect of how the name was
  // worked out. A build asks for none; the check of the compile against itself
  // asks for every instance.
  UnitLowerer(
      const LoweringFacts& facts, const slang::ast::Symbol& home,
      hir::SpecializationRef names, hir::UnitRole role,
      std::span<const slang::ast::InstanceBodySymbol* const> checked_against =
          {});

  // The declaration phase: everything this unit states about itself, including
  // the signature it publishes. Reads no other unit, so a body that later
  // references one cannot observe whether it had been declared yet.
  auto Declare() -> diag::Result<void>;

  // The same declaration, made to compare what one specialization publishes
  // with what another does before any unit is named, which declares no unit.
  auto DescribeForGrouping() -> diag::Result<void>;

  // What this unit publishes, moved out once its declaration phase has run.
  // The design collects these before any body lowers and hands each unit the
  // ones it may read.
  [[nodiscard]] auto TakeSignature() -> hir::UnitSignature;

  // What this specialization holds that its unit does not, under its name,
  // moved out with the signature.
  [[nodiscard]] auto SpecializationName() const -> const std::string& {
    return specialization_;
  }
  [[nodiscard]] auto TakeValues() -> hir::SpecializationValues;

  // The body phase: everything this unit executes, resolved against what the
  // design's units published and what `built_as` says each instance is built
  // as. Which of those publications this unit ends up depending on is the set
  // it reads, so nothing decides that set in advance. The unit's own scope is
  // lowered from the instance it is named for, and comes back as the
  // implementation it is, named as `implementations` names it, beside how its
  // objects are built.
  auto LowerBodies(
      const hir::UnitSignatures& signatures, const BuiltAsQuery& built_as,
      Implementations& implementations) -> diag::Result<hir::Specialization>;

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

  // Whether this unit holds `decl`. Every walk of the scope this unit is
  // lowered from asks it, since that scope may declare what another unit
  // holds: a generic's specializations, or everything but the one
  // specialization a unit of its own holds.
  [[nodiscard]] auto Owns(const slang::ast::Symbol& decl) const -> bool;

  // Lowers a slang type to a HIR TypeId. Identity is the pool's and is
  // structural, so what the frontend-keyed memo here adds is only a shortcut
  // past the translation work for a type already translated; two frontend
  // spellings of one type reach the same id whether or not either took it.
  auto InternType(const slang::ast::Type& type, diag::SourceSpan span)
      -> diag::Result<hir::TypeId>;

  // Records what a declared type of this unit's own declares, as `define`
  // works it out from the unit's declaration, the first time the type is
  // named. Another unit's is what that unit published, taken when it is read,
  // so nothing of another unit's declaration is read.
  template <typename Define>
  auto DefineType(const hir::TypeDeclarationRef& declaration, Define define)
      -> diag::Result<void> {
    if (!unit_.IsItself(declaration.unit_name) ||
        unit_.type_definitions.Find(declaration) != nullptr) {
      return {};
    }
    diag::Result<hir::TypeDefinition> definition = define();
    if (!definition) return std::unexpected(std::move(definition.error()));
    unit_.type_definitions.Define(declaration, *std::move(definition));
    return {};
  }

  // What a declared type this unit names declares: its own, as it defined it,
  // and another unit's, as that unit published it, taken the first time it is
  // asked for, which is after every unit has published.
  auto TypeDefinitionOf(const hir::TypeDeclarationRef& declaration)
      -> const hir::TypeDefinition&;
  auto DefinitionOf(const hir::EnumType& type) -> const hir::EnumDefinition& {
    return std::get<hir::EnumDefinition>(TypeDefinitionOf(type.declaration));
  }
  auto DefinitionOf(const hir::UnpackedStructType& type)
      -> const hir::UnpackedStructDefinition& {
    return std::get<hir::UnpackedStructDefinition>(
        TypeDefinitionOf(type.declaration));
  }
  auto DefinitionOf(const hir::UnpackedUnionType& type)
      -> const hir::UnpackedUnionDefinition& {
    return std::get<hir::UnpackedUnionDefinition>(
        TypeDefinitionOf(type.declaration));
  }

  // The value the value parameter `param` holds where a name reaching it is
  // folded: one this unit declares, as the front end evaluated it, and
  // another unit's, as that one published it.
  [[nodiscard]] auto ParameterValueOf(
      const slang::ast::ParameterSymbol& param, diag::SourceSpan span) const
      -> diag::Result<hir::ConstantValue>;

  // The type of `expr`, which is the one way an expression's type is interned.
  // What a declaration types -- a name, a member, an element, a user
  // subroutine's result, a pattern or a `new` typed by what it is assigned to
  // -- and what its own text types, a literal, keep that type, since a select
  // or a key is read against its range. What an operation yields -- an
  // operator, a conversion, an assignment, a part-select, a concatenation and
  // its kin, a system subroutine's result -- and an unbased unsized literal,
  // which is as wide as its context (LRM 5.7.1), have no range of their own
  // and are typed as what an operation yields, below.
  auto InternExpressionType(
      const slang::ast::Expression& expr, diag::SourceSpan span)
      -> diag::Result<hir::TypeId>;

  // The type of a value no select or key is ever read against -- what an
  // operation yields -- numbered from zero in the direction its outermost
  // dimension runs. No clause fixes that range (LRM 11.6.1 and 11.8.1 fix a
  // width and a signedness, 11.4.11 and 7.4.5 an element count), and the front
  // end hands up one it took from an operand or settled from a select's base,
  // which differs between two instances whose identity is one. A query that
  // reads the range reads the front end's own type, whose reads the front end
  // records where the query is written.
  auto InternResultType(const slang::ast::Type& type, diag::SourceSpan span)
      -> diag::Result<hir::TypeId>;

  // Adds a type the lowering itself composes rather than reads off the
  // frontend -- the array of a type's per-dimension query results that an LRM
  // 20.7 query selects from when its dimension is named at run time. There is
  // no frontend type to take the shortcut on, so it goes straight to the pool.
  auto AddComposedType(hir::Type type) const -> hir::TypeId;

  // The type a class property is held as: the type it was declared with, or a
  // weak handle of it where Lyra's definitions hold it weakly (LRM 8.30).
  auto InternPropertyType(
      const slang::ast::ClassPropertySymbol& prop, diag::SourceSpan span)
      -> diag::Result<hir::TypeId>;

  // A type importer taking this unit's types into its own signature, naming
  // every class the way the signature does.
  auto PublishingImporter(hir::TypeImportMemo& memo) -> hir::TypeImporter;

  // A type importer taking `signature`'s types into this unit's pool, naming
  // every class the way this unit does.
  auto ReadingImporter(const hir::UnitSignature& signature)
      -> hir::TypeImporter;

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
      -> hir::ScopeClassId;

  // This unit's record of the class an instance of `unit_name` is an object
  // of, taken from that unit's signature the first time one is reached and
  // answered with the same identity every later time. Reaching another unit's
  // object is what declares the dependency on it, so its signature is in hand
  // here.
  auto ScopeClassOf(const std::string& unit_name) -> hir::ScopeClassId;

  // The same record of the scope class `class_path` that unit published: the
  // class of an instance of it, or of a generate block inside one.
  auto ScopeClassOf(
      const std::string& unit_name, const support::DefPath& class_path)
      -> hir::ScopeClassId;

  // The scope class `scope_class` names, with what its unit published of it
  // recorded, which a name reaching into it reads.
  auto RecordedScopeClass(hir::ScopeClassId scope_class) -> hir::ScopeClassId;

  // The type of an object of the scope class `scope_class` records.
  [[nodiscard]] auto ScopeClassTypeOf(hir::ScopeClassId scope_class) const
      -> hir::TypeId;

  // Where a route ends that reaches the member the scope class `scope_class`
  // records published at `member`: what storage that member is and its type,
  // as that class stated them.
  [[nodiscard]] auto PublishedMemberLeafOf(
      hir::ScopeClassId scope_class, hir::PublishedMemberId member) const
      -> hir::PublishedMemberLeaf;

  // What the class `cls` declares: for a class of this unit, what interning it
  // computed; for one of another unit, what that unit published, taken from
  // its signature the first time this unit asks. Every class a unit declares
  // is published, so the answer always exists.
  auto DeclarationOf(hir::ClassId cls) -> const hir::ClassDeclaration&;

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

  // The objects that port stands for, as its own type states them: the
  // declared range of each dimension, and which kind of instance is bound at
  // each position.
  [[nodiscard]] auto InterfacePortObjects(const slang::ast::Symbol& port) const
      -> hir::UnitObjectsType {
    const auto* objects =
        unit_.types.Get(InterfacePortType(port)).As<hir::UnitObjectsType>();
    if (objects == nullptr) {
      throw InternalError(
          "UnitLowerer::InterfacePortObjects: an interface port stands for "
          "the instances its connection named");
    }
    return *objects;
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
  // proof that the class belongs to this unit -- the pre-pass walks only what
  // this unit holds, and a same-unit base link resolves through the reference
  // translator before flowing back here -- so no parent-chain query runs here.
  // A repeat call for the same class is idempotent: the cache entry the first
  // mint installed returns without re-work, which admits mutual reference
  // during body population.
  auto InternOwnClass(const slang::ast::ClassType& cls, diag::SourceSpan span)
      -> diag::Result<hir::ClassId>;

  // The identity a slang class -- reached from an expression site or a
  // base-class link -- has in this unit, whichever unit declares it.
  // Classification runs at most once per class per unit: the first encounter
  // walks `cls.getParentScope()` to find the unit holding it and caches the
  // answer; every later encounter reads it. A class of this unit not yet
  // interned is minted lazily on this path, so a body reads the same identity
  // regardless of which route saw the class first.
  auto ResolveClass(const slang::ast::ClassType& cls, diag::SourceSpan span)
      -> diag::Result<hir::ClassId>;

  // The same for a covergroup (LRM 19.3), which is identified as a class is.
  auto ResolveCovergroup(
      const slang::ast::CovergroupType& covergroup, diag::SourceSpan span)
      -> diag::Result<hir::CovergroupId>;
  auto InternOwnCovergroup(
      const slang::ast::CovergroupType& covergroup, diag::SourceSpan span)
      -> diag::Result<hir::CovergroupId>;

  // What the covergroup `covergroup` declares, whichever unit declares it: this
  // unit's own as it publishes it, or another unit's as that unit published
  // it, taken from its signature the first time this unit asks.
  auto CovergroupDeclarationOf(hir::CovergroupId covergroup)
      -> const hir::PublishedCovergroup&;

  // The method a class embedding `covergroup` publishes for making it (LRM
  // 19.4.1).
  auto PublishedFactoryOf(
      const slang::ast::CovergroupType& covergroup, diag::SourceSpan span)
      -> diag::Result<hir::DeclaredMethod>;

  // Records what each other unit published about every covergroup of its that
  // a type of this unit names, once the bodies have lowered.
  void ReadSignaturesOfNamedCovergroups();

  // What this unit publishes about its own covergroup `covergroup`.
  auto PublishedCovergroupOf(
      const slang::ast::CovergroupType& covergroup, diag::SourceSpan span)
      -> diag::Result<hir::PublishedCovergroup>;

  // Hands a structural scope the covergroups it replicates, once.
  [[nodiscard]] auto TakeReplicatedCovergroups(const slang::ast::Scope& scope)
      -> std::vector<hir::CovergroupId>;

  // Lowers every covergroup `scope` replicates, where a process of the scope
  // is lowered, so its expressions reach what a body of the scope reaches.
  auto PopulateCovergroupsReplicatedBy(const slang::ast::Scope& scope)
      -> diag::Result<void>;

  // The classes of the coverage library this unit names, recorded once
  // anything of this unit is a covergroup or reads one's coverage (LRM 19).
  auto NameCoverageLibrary(diag::SourceSpan span) -> diag::Result<void>;

  // What a class `std` declares, or one of its methods, stands for: Lyra's
  // definition of it. Any other class or method stands for itself.
  [[nodiscard]] auto Defining(const slang::ast::ClassType& cls) const
      -> const slang::ast::ClassType*;
  [[nodiscard]] auto Defining(const slang::ast::SubroutineSymbol& method) const
      -> const slang::ast::SubroutineSymbol*;

  // The method `method` of the class `owner` declares, at its position among
  // that class's methods: for a class of this unit the one the interning
  // registered for the symbol, and for another unit's the one of that name its
  // signature published.
  [[nodiscard]] auto MakeClassMethodTarget(
      hir::ClassId owner, const slang::ast::SubroutineSymbol& method)
      -> hir::ClassMethodTarget;

  // The same for a method a call reaches, `owner` being the slang class
  // declaring it.
  auto MakeClassMethodTarget(
      const slang::ast::ClassType& owner,
      const slang::ast::SubroutineSymbol& method, diag::SourceSpan span)
      -> diag::Result<hir::ClassMethodTarget>;

  // The identity of the class the type `type` names.
  auto ResolveClassType(const slang::ast::Type& type, diag::SourceSpan span)
      -> diag::Result<hir::ClassId>;

  // What a class's declaration names beside itself: the class it extends, and
  // the interface classes it implements or, for an interface class, extends
  // (LRM 8.26.2), in the order written.
  struct ClassParents {
    std::optional<hir::ClassId> base;
    std::vector<hir::ClassId> implements;
  };
  auto ParentsOf(hir::ClassId cls) -> ClassParents;

  // Appends the interface class `iface`, then every interface class it extends
  // (LRM 8.26.2), skipping one already in `reached`: an interface class is one
  // however many ways it is arrived at (LRM 8.26.6.3). What each extends is
  // read where that class states it, so one of another unit is its signature
  // consumed.
  void ReachInterface(hir::ClassId iface, std::vector<hir::ClassId>& reached);

  // Every interface class a value of `cls` is also a value of (LRM 8.26.5):
  // the ones the class it extends is (LRM 8.26), then the ones `cls` names and
  // the ones those extend.
  auto AllInterfacesOf(hir::ClassId cls) -> std::vector<hir::ClassId>;

  // What the class of `method` declares of it, or nothing for a method the
  // front end supplies every class (LRM 18.6, 18.8), which no declaration
  // lists.
  auto DeclaredMethodOf(
      const slang::ast::SubroutineSymbol& method, diag::SourceSpan span)
      -> diag::Result<std::optional<hir::DeclaredMethod>>;

  // Whether the class method `method` is one of the class rather than of an
  // object of it (LRM 8.10), so a call hands it no object.
  auto IsTypeAssociatedMethod(
      const slang::ast::SubroutineSymbol& method, diag::SourceSpan span)
      -> diag::Result<bool>;

  // For a class that is not an interface class, states which behavior of its
  // lineage answers each behavior of every interface class it is also a value
  // of. The answer is found by looking the behavior's name up in the class,
  // which is how the language finds it (LRM 8.26.2).
  auto StateConformance(
      const slang::ast::ClassType& cls, diag::SourceSpan span,
      hir::ClassDecl& decl) -> diag::Result<void>;

  // The behavior `method` is part of -- a method, or a constraint block, which
  // is overridden as one (LRM 18.5.2) -- named by the member that introduced
  // it, whichever unit declares that member's class: what a member overriding
  // it overrides, and what an interface class's behavior is answered by.
  auto IntroducingMethodTarget(
      const slang::ast::Symbol& method, diag::SourceSpan span)
      -> diag::Result<hir::ClassMethodTarget>;

  // The interface a subroutine's own declaration states: its call protocol and
  // each formal's direction and type (LRM 13.5), in this unit's types. It is
  // what a unit publishes about a subroutine it declares, and what a call to
  // one of this unit's own namespace subroutines takes.
  auto MakeCalleeInterface(
      const slang::ast::SubroutineSymbol& sym, diag::SourceSpan span)
      -> diag::Result<hir::CalleeInterface>;

  // The interface a call to `sym` is made through, where `sym` is declared in
  // the namespace of the unit named `unit_name` (LRM 26.3). Another unit's is
  // read off that unit's signature; this unit's own is its own declaration.
  auto NamespaceCalleeInterface(
      const std::string& unit_name, const slang::ast::SubroutineSymbol& sym,
      diag::SourceSpan span) -> diag::Result<hir::CalleeInterface>;

  // The property `prop` of the class `owner`, at the position the property of
  // that name has in the class's declaration, whichever unit declares it.
  [[nodiscard]] auto MakeClassPropertyTarget(
      const slang::ast::ClassType& owner,
      const slang::ast::ClassPropertySymbol& prop, diag::SourceSpan span)
      -> diag::Result<hir::ClassPropertyTarget>;

  // The position the member named `name` has in the declaration of `cls`,
  // whichever unit declares it.
  auto MethodPositionOf(hir::ClassId cls, std::string_view name)
      -> hir::MethodId;
  auto PropertyPositionOf(hir::ClassId cls, std::string_view name)
      -> hir::FieldId;
  auto StaticPropertyPositionOf(hir::ClassId cls, std::string_view name)
      -> hir::StaticPropertyId;

  // The position `found` names, which a class the front end resolved a name in
  // always has.
  template <typename Id>
  auto Positioned(
      hir::ClassId cls, std::string_view name, std::optional<Id> found) const
      -> Id {
    if (found.has_value()) return *found;
    const hir::ClassKey& key = unit_.classes.KeyOf(cls);
    throw InternalError(
        std::format(
            "UnitLowerer: class '{}' of unit '{}' declares no member '{}' a "
            "name resolved to -- please report this as a bug",
            support::DisplayOf(key.class_path), key.unit_name, name));
  }

  [[nodiscard]] auto Sensitivity() const -> SensitivityAnalyzer& {
    return facts_.Sensitivity();
  }

  // The clocking event the procedure `containing` settles for a construct that
  // names none of its own -- its inferred clock (LRM 16.14.6), and otherwise
  // its scope's default clocking (LRM 14.12). Nothing where `containing` is no
  // procedure.
  [[nodiscard]] auto InferredProcedureClock(
      const slang::ast::Symbol& containing) const
      -> const slang::ast::TimingControl*;

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

  [[nodiscard]] auto Coverage() const -> const CoverageLibrary& {
    return facts_.Coverage();
  }

  [[nodiscard]] auto Naming() const -> const UnitNamingPolicy& {
    return facts_.Naming();
  }

  // What each instance is built as, which a body's construction and its reads
  // of another unit's values ask. Reachable only once bodies lower, as the
  // classes it is worked out over are not known while units are compared.
  [[nodiscard]] auto BuiltAs() const -> const BuiltAsQuery& {
    if (built_as_ == nullptr) {
      throw InternalError(
          "UnitLowerer::BuiltAs: what an instance is built as is reachable "
          "only while bodies lower");
    }
    return *built_as_;
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
      hir::InterfacePortId local);
  [[nodiscard]] auto LookupInterfacePortBinding(const slang::ast::Symbol& port)
      const -> std::optional<InterfacePortBinding>;

  void MapSubroutineBinding(
      const slang::ast::SubroutineSymbol& sym, ScopeFrameId owner_frame,
      hir::StructuralSubroutineId local);
  [[nodiscard]] auto LookupSubroutineBinding(
      const slang::ast::SubroutineSymbol& sym) const
      -> std::optional<SubroutineBinding>;

  // The subroutine this unit evaluates an expression of its own in, for
  // another unit that asks for the value because the expression names
  // declarations it cannot see: a name a view offers only for reading (LRM
  // 25.5.4), a port's default (LRM 23.2.2.4), a formal's default (LRM 13.5.3),
  // and an event of a clocking block's clocking event with its qualifier (LRM
  // 14.3). The identity is minted with every other structural identity so the
  // signature can name it before any body is lowered.
  void MapEvaluator(
      const slang::ast::Expression& evaluated,
      hir::StructuralSubroutineId evaluator);
  [[nodiscard]] auto EvaluatorOf(const slang::ast::Expression& evaluated) const
      -> hir::StructuralSubroutineId;
  // The same, or nothing where this unit evaluates `evaluated` in no
  // subroutine of its own -- a formal's default of a method, whose class
  // evaluates it.
  [[nodiscard]] auto FindEvaluator(const slang::ast::Expression& evaluated)
      const -> std::optional<hir::StructuralSubroutineId>;

  // The type a port (LRM 23.2.2), a name a view offers or a formal declares,
  // which for the name is the self-determined type of the expression it was
  // written with (LRM 25.5.4). A unit reading the name places its selects by
  // that range, so what crosses for it -- the value an evaluator returns, or
  // the place a view names -- has this type, whatever its expression yields;
  // a formal's default is a value of the formal's type (LRM 13.5.3).
  auto InternDeclaredType(const slang::ast::Symbol& holder)
      -> diag::Result<hir::TypeId>;

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
      DeclaredOwnedChild declared);
  [[nodiscard]] auto LookupOwnedChildBinding(const slang::ast::Symbol& child)
      const -> std::optional<OwnedChildBinding>;
  // The identity the declaration pass gave a generate construct, or an
  // instance, of this unit; that pass gives one to every such child.
  [[nodiscard]] auto GenerateIdOf(const slang::ast::Symbol& construct) const
      -> hir::GenerateId;
  // The same for a loop generate, which is one construct however many blocks
  // it counted out, none included.
  [[nodiscard]] auto GenerateIdOf(
      const slang::ast::GenerateBlockArraySymbol& loop) const
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

  // `scope`, which `lowerer` builds as `frame`, holds the routes the names
  // read in it take, and so the selects those names write; until its routes
  // are taken, a select is lowered into it by its own lowerer.
  void BeginScope(
      ScopeFrameId frame, StructuralScopeLowerer& lowerer,
      hir::StructuralScope& scope);

  // The body a generate block stating `scope` is: one found before it where the
  // two state the same thing, and otherwise a new one. What a block compiles
  // to is what it states, so blocks stating alike are one body whichever
  // objects build them, and a copy found equal is dropped as soon as it is
  // found. The text of each body sorts the bodies so that a block is held
  // against only those that could equal it.
  auto InternBlockBody(hir::StructuralScope scope) -> hir::BlockBodyId;

  // The selects `written`, a path a name read where `frame` stands wrote,
  // gives each loop's block it passes, each lowered once and stored beside the
  // routes it belongs to.
  [[nodiscard]] auto SelectsWritten(
      const WalkFrame& frame,
      std::span<const slang::ast::HierarchicalReference::Element> written) const
      -> diag::Result<LoopSelectOf>;

  // The scope holding the routes of what is read where `frame` stands: the
  // structural scope a body -- a process, a subroutine, a class's method -- is
  // read from, as for the scope's own expressions.
  [[nodiscard]] auto RouteHolderOf(const WalkFrame& frame) const
      -> const ScopeUnderConstruction&;

  // The compilation-unit declaration pass (LRM 23.6 / 23.9 / 27): before any
  // executable body lowers, walk the whole unit's scope tree and mint every
  // declaration a peer body may reference -- owned children (instance,
  // generate block, generate array), subroutines, and the static-lifetime body
  // locals a named block puts on the hierarchical path -- assigning each scope
  // its frame along the way. A body or sensitivity read then resolves any of
  // them regardless of which sibling scope or body lowered first. Registers no
  // executable HIR. What each scope publishes is recorded in the same walk,
  // under `class_path`, the class an object of the scope is.
  void DeclareStructuralIdentities(
      const slang::ast::Scope& scope, support::DefPath class_path);

  // The same for the generate block `block`, a declaration nested in the scope
  // that published `holder`.
  void DeclareBlockIdentities(
      const slang::ast::GenerateBlockSymbol& block,
      const ScopePublicationRecord& holder);

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

  // `built`, scopes that each built one part of what is compiled alike, in
  // the order the lowering reads them: the part is lowered from the first, and
  // where they disagree on something only all of them together state, the
  // first to state it decides. That is the order the source wrote them in,
  // and the reverse where the compile checks itself, so that the lowering it
  // is checked against reads a second scope of every part wherever one built
  // it.
  [[nodiscard]] auto InReadingOrder(std::vector<const slang::ast::Scope*> built)
      const -> std::vector<const slang::ast::Scope*>;

  // Whether a design element other than this unit holds `type`, a class or a
  // covergroup, so that an object of it belongs to an instance of that
  // element, which no count of this unit's own scopes reaches.
  [[nodiscard]] auto HeldByAnotherDesignElement(
      const slang::ast::Symbol& type) const -> bool;

  // Whether a construction of `cls` and a call of a method of the class itself
  // are handed the instance it belongs to, as the class's declaration states
  // it.
  auto TakesDeclaringInstance(
      const slang::ast::ClassType& cls, diag::SourceSpan span)
      -> diag::Result<bool>;

  // How far out of `frame`'s own structural scope the instance an object of
  // `type` belongs to sits, for a class or a covergroup one of this unit's
  // structural scopes replicates.
  [[nodiscard]] auto DeclaringScopeHopsFrom(
      const slang::ast::Symbol& type, const WalkFrame& frame,
      diag::SourceSpan span) const -> diag::Result<hir::StructuralHops>;

  // How a body at `frame` reaches the instance an object of `cls` belongs to,
  // for a construction or a call of a method of the class itself written
  // there, or nothing where the class takes no such instance: as above where
  // this unit holds the class, and by a route to the replicating scope's
  // object where another design element does (LRM 6.22).
  [[nodiscard]] auto DeclaringInstanceFrom(
      const slang::ast::ClassType& cls, const WalkFrame& frame,
      diag::SourceSpan span)
      -> diag::Result<std::optional<hir::DeclaringInstanceReach>>;

  // How a body at `frame` reaches the instance an object of `type` belongs to,
  // for a class or a covergroup an instance replicates (LRM 6.22).
  [[nodiscard]] auto ReachDeclaringInstance(
      const slang::ast::Symbol& type, const WalkFrame& frame,
      diag::SourceSpan span) -> diag::Result<hir::DeclaringInstanceReach>;

  // What `scope` published, in this unit's own identities: asked once the walk
  // lowering the scope has given every declaration it published its identity.
  // A scope this unit published nothing of -- a namespace unit's -- takes the
  // empty publication.
  [[nodiscard]] auto TakePublication(const slang::ast::Scope& scope)
      -> hir::ScopePublication;

  // Hands a structural scope the classes it replicates, once. Every class the
  // unit holds is named by exactly one scope, so a scope that replicates none
  // takes the empty list rather than being absent from the relation.
  [[nodiscard]] auto TakeReplicatedClasses(const slang::ast::Scope& scope)
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

  // Interns every class this unit holds -- a non-parameterized class as a
  // single entry, and each live specialization of a parameterized one the unit
  // holds as one. Runs before any body lowering so the unit's class registry
  // is complete before any reference resolves.
  auto InternOwnClassDeclarations() -> diag::Result<void>;

  // Interns every enumeration, unpacked structure and unpacked union a
  // declaration of this unit states outside a body, in the same scopes classes
  // are declared in, so the type -- and with it what it declares and the
  // operations its declaration brings -- is in the unit that declares it
  // whatever the unit's bodies use.
  auto InternOwnTypeDeclarations() -> diag::Result<void>;

  // A class whose declarations are settled and whose bodies are not. A class
  // body names what encloses the class the way any other body does, so it
  // lowers once every structural scope has bound its own declarations; what is
  // carried here is what the declaration half already settled.
  struct PendingClassBody {
    const slang::ast::ClassType* cls = nullptr;
    hir::ClassId id;
    diag::SourceSpan span;
    const slang::ast::Scope* replicating_scope = nullptr;
    // Held by pointer because its identity is its address: the procedural
    // scopes a body may name are minted into `decl->procedural_scopes` in the
    // declaration half and looked up by the registry they belong to, so the
    // registry may not move between the two halves.
    std::unique_ptr<hir::ClassDecl> decl;
    std::vector<const slang::ast::SubroutineSymbol*> defined_methods;
    std::vector<const slang::ast::MethodPrototypeSymbol*> pure_prototypes;
    std::vector<const slang::ast::ConstraintBlockSymbol*> constraint_blocks;
    // Every formal of a method with a default the class evaluates (LRM
    // 13.5.3), beside the method declaring it.
    std::vector<std::pair<
        const slang::ast::SubroutineSymbol*,
        const slang::ast::FormalArgumentSymbol*>>
        defaulted_formals;
    const slang::ast::SubroutineSymbol* constructor_sym = nullptr;
  };

  // Lowers the body half of every class `scope` replicates: each method, the
  // constructor, and every property initializer. Called while that scope is
  // lowered, so a class body has the reach a process of the scope has.
  auto PopulateClassBodiesReplicatedBy(const slang::ast::Scope& scope)
      -> diag::Result<void>;
  auto PopulateClassBody(PendingClassBody& pending) -> diag::Result<void>;

  // Requires every class the unit holds to be defined once its scopes are
  // lowered. Each class is replicated by a structural scope the unit lowers,
  // which lowers the class's body; a block instance no body was lowered from
  // replicates only classes the block it shares a body with defined.
  void RequireEveryClassBodyLowered() const;

  // Marks the class `cls` as one this unit reads, not only names: the
  // declaration phase meets classes whose objects the unit makes and whose
  // methods it calls -- the coverage library's -- while other units'
  // signatures are not yet reachable, so their declarations are read once every
  // body has lowered.
  void ReadDeclarationLater(hir::ClassId cls);
  void ReadPendingDeclarations();
  // Takes what each declared type this unit names declares, which a later
  // layer reads off the unit rather than off the declaring one.
  void ReadTypeDefinitions();

  // Builds a HIR Expr referring to the data `route` navigates to.
  // `owner_frame` is the frame whose routes hold it.
  auto MakeRoutedMemberRef(
      ScopeFrameId owner_frame, hir::ValueRoute route, diag::SourceSpan span)
      -> hir::Expr;
  // The storage `route` reaches, as what a wait watches.
  auto RoutedMemberCell(ScopeFrameId owner_frame, hir::ValueRoute route)
      -> hir::RoutedValueRef;

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

  // Whether a read of `param` reads a cell rather than a folded value: its
  // value differs between the objects built from the scope declaring it, so
  // each of them holds it, and a name reaches it there as it reaches a
  // variable. A parameter the specialization of its unit fixes folds instead.
  [[nodiscard]] auto ReadsParameterFromCell(
      const slang::ast::ParameterSymbol& param) const -> bool;

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
  // object tree a route could walk to at all. An error is a select the name
  // wrote that did not lower.
  [[nodiscard]] auto RouteToScope(
      const WalkFrame& frame, const slang::ast::Scope& target,
      RouteOrigin origin) -> diag::Result<std::optional<ScopeRoute>>;

  // Where a name written in this unit starts: through an interface port or
  // upward at the enclosing scope it landed in, where it leaves the unit's
  // instance, and at the reader for a name that never leaves and for every
  // name a namespace unit writes, since a namespace has no instance to leave.
  // The name is `found`, reached by `written`, the path the source wrote to it,
  // empty for a name written plainly; how it was spelled decides nothing.
  [[nodiscard]] auto StartOf(
      const WalkFrame& frame, const slang::ast::Symbol& found,
      const slang::ast::HierarchicalReference& written) -> RouteOrigin;

  // The events the clocking event of another instance's clocking block stands
  // for (LRM 14.3, 9.4.2.1), as that instance published them: each a call of
  // what it evaluates for the block, on the object the block's instance is,
  // waited on at the members it reads, and qualified by a call where the block
  // wrote a qualifier.
  [[nodiscard]] auto PublishedClockingEntries(
      const WalkFrame& frame, const ClockOfAnotherInstance& clock,
      diag::SourceSpan span) -> diag::Result<std::vector<hir::EventEntry>>;

  // Where a route to `target` starts when no name says so -- a scope reached
  // because a type belongs to it: at the reader where `target` stands in this
  // unit's instance or below it, and otherwise at `target` where this unit's
  // instance stands in it, at the scope one of this unit's names lands in once
  // it leaves, or at the enclosing scope a type handed down came from. The
  // anchor is the same in every instance this unit serves, since what tells
  // the unit apart is where those names land. Nothing where none of these
  // applies, which leaves no route to the scope.
  [[nodiscard]] auto StartReaching(const slang::ast::Scope& target)
      -> std::optional<RouteOrigin>;

  // The class the unit of the instance holding `scope` published that scope
  // as, recorded in this unit -- this unit's own included.
  [[nodiscard]] auto PublishedClassOf(const slang::ast::Scope& scope)
      -> hir::ScopeClassId;

  // A route starting where `scope` stands for this reader: `scope` is an
  // instance's body or a generate block outside the reader, and the route
  // starts at the nearest scope enclosing the reader that is an object of the
  // same class, or past the topmost of them at a top-level instance of it.
  [[nodiscard]] auto StartInEnclosing(const slang::ast::Scope& scope)
      -> RouteStart;

  // How this reader reaches the scope whose instance `type` belongs to (LRM
  // 6.22), where another design element declares it.
  [[nodiscard]] auto RouteToReplicatingScope(
      const WalkFrame& frame, const slang::ast::Symbol& type)
      -> diag::Result<std::optional<ScopeRoute>>;

  // Whether `target`, a scope enclosing this reader that declares a subroutine
  // it calls, is the reader's own scope, whose identity for the subroutine the
  // reader may name; any other is reached through what it published.
  [[nodiscard]] auto IsOwnScope(
      const WalkFrame& frame, const slang::ast::Scope& target) const -> bool;

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
      -> diag::Result<std::optional<ScopeRoute>>;

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
  // not stand in `from_scope`, a scope on the way published nothing the
  // descent names, or `select_of` gives no select for a loop's block on it.
  [[nodiscard]] auto DescendPublished(
      hir::ScopeClassId from, const slang::ast::Scope& from_scope,
      const slang::ast::Scope& to, const LoopSelectOf& select_of)
      -> std::optional<PublishedDescent>;

 private:
  // What `Declare` and `DescribeForGrouping` both do.
  auto DeclareOwn() -> diag::Result<void>;

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
  // reads the unit's own declarations rather than the frontend's tree. Each
  // covergroup it declares is published beside them, as what the step to MIR
  // makes its class from (LRM 19.3).
  auto PublishTypeSignatures() -> diag::Result<void>;
  // The definition of each enumeration, unpacked structure and unpacked union
  // this unit declares, into the signature, for a unit naming one to take.
  void PublishTypeDefinitions();
  // The value of each value parameter this specialization declares, for a unit
  // reading one by name to fold. One whose value no constant of this compiler
  // states is left out, and a reader of it refuses it as a reader of this
  // unit's own would.
  void PublishParameterValues();

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
      const WalkFrame& frame, const slang::ast::InterfacePortSymbol& port)
      -> PortReach;

  // Where a walk through `port` stands once `selects` are written after it:
  // on the instance bound at the position they pick out, or on several where
  // they leave a dimension of the port unselected.
  [[nodiscard]] auto PlaceThroughPort(
      const slang::ast::InterfacePortSymbol& port,
      std::span<const std::uint32_t> selects) -> RoutePlace;

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
      hir::ScopeClassId standing, std::span<NamedHop> hops)
      -> std::optional<PublishedDescent>;

  // What a process waiting on a name a modport offers observes: every member
  // the expression behind that name reads (LRM 25.5.4), each reached the way
  // the interface was, from `origin`. The name is no storage of its own, so
  // nothing waits on it directly.
  auto ObservedThroughModport(
      const slang::ast::ModportPortSymbol& offered, const WalkFrame& frame,
      RouteOrigin origin) -> diag::Result<std::vector<hir::SensitivityEntry>>;

  // A watch of the whole of each of `members`, which the scope of class
  // `scope_class` published, on the object `route` reaches.
  auto WholeMembersAlong(
      const WalkFrame& frame, const ScopeRoute& route,
      hir::ScopeClassId scope_class,
      std::span<const hir::PublishedMemberId> members)
      -> std::vector<hir::SensitivityEntry>;

  // Where each of `names`, the names a read was reached by, starts: once per
  // name leaving the reader's instance or selecting a loop's block, which
  // reaches a different block from each object, and at the reader, once, for
  // every other name that stays inside it, as for a read put together with no
  // names at all.
  [[nodiscard]] auto StartsOfNames(
      const WalkFrame& frame,
      std::span<const slang::ast::Expression* const> names)
      -> std::vector<RouteOrigin>;

  // The cells a read of `target` reaches, once each: one per place the names
  // it was reached by start from.
  [[nodiscard]] auto CellsReachedBy(
      const WalkFrame& frame, const slang::ast::ValueSymbol& target,
      std::span<const slang::ast::Expression* const> names,
      diag::SourceSpan span) -> diag::Result<std::vector<hir::ValueTarget>>;

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
  // What a name reaches here from elsewhere is published as a declaration of
  // `within`, which scope of the unit `block` is (LRM 23.9): each
  // static-lifetime variable, and each named block as what a `disable` of it
  // ends (LRM 9.6.2). A block the source left unnamed puts nothing below it on
  // any path a name spells, which is what an absent `within` says.
  void DeclareProceduralStatics(
      const slang::ast::Scope& block, const slang::ast::Symbol& body_symbol,
      hir::ProceduralBodyRef body, ScopeFrameId frame,
      ScopePublicationRecord& published,
      const std::optional<support::DefPath>& within);

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
  void DeclareClockingEvaluators(
      const slang::ast::ClockingBlockSymbol& block, ScopeDeclarations& decls,
      ScopePublicationRecord& published);
  void DeclareProcess(
      const slang::ast::ProceduralBlockSymbol& proc, ScopeDeclarations& decls,
      ScopePublicationRecord& published, ScopeFrameId frame);
  // A class is a type of each instance of the scope replicating it (LRM 6.22,
  // 8.25), so what `cls` keeps for itself (LRM 8.9) is a cell that scope
  // publishes.
  void DeclareClassStatics(
      const slang::ast::ClassType& cls, const slang::ast::Scope& replicating);

  // The class `scopes` are objects of, in this unit's own types, the scopes in
  // the order the lowering reads them: what it declares as the first records
  // it, and every block any of them built.
  auto PublishScopeClass(std::span<const slang::ast::Scope* const> scopes)
      -> diag::Result<hir::ScopeClassSignature>;

  // A published callable as this unit's own types state it.
  auto PublishedCallableOf(const ScopePublicationRecord::Callable& callable)
      -> diag::Result<hir::SubroutinePrototype>;

  // What the identity walk recorded for `scope`.
  [[nodiscard]] auto PublicationOf(const slang::ast::Scope& scope) const
      -> const ScopePublicationRecord&;

  // Whether a parameter's value differs between the objects built from the
  // scope declaring it: supplied or computed at construction, rather than fixed
  // by the specialization.
  [[nodiscard]] auto DiffersPerObject(
      const slang::ast::ParameterSymbol& param) const -> bool;

  // Hands `visit` every declaration outside a body this unit holds, each before
  // anything inside it, in every application the body is lowered from: those
  // its scope declares, then each specialization placed in it from a generic
  // another unit declares. Each application holds its own copy of a
  // declaration, so one declaration may be handed over once per application.
  template <typename Visit>
  auto WalkOwnDeclarations(Visit& visit)
      -> std::invoke_result_t<Visit&, const slang::ast::Symbol&>;

  // The application of this unit that `scope` is or sits inside, or nothing
  // where it is in none of them.
  [[nodiscard]] auto ApplicationHolding(const slang::ast::Scope& scope) const
      -> const slang::ast::Scope*;

  // The application of this unit that declares `param`, or the one the unit is
  // named for where none of them does.
  [[nodiscard]] auto ApplicationDeclaring(
      const slang::ast::ParameterSymbol& param) const
      -> const slang::ast::Scope&;

  // The unit's own scope as `application` built it, and how that object
  // builds its children.
  auto LowerScopeOf(const slang::ast::Scope& application)
      -> diag::Result<BuiltScope>;

  LoweringFacts facts_;
  // The homes whose declarations this unit holds, the one it is named for
  // first.
  std::vector<const slang::ast::Symbol*> homes_;
  // The scope each home is lowered and declared from, in the same order.
  std::vector<const slang::ast::Scope*> applications_;
  const slang::ast::Scope* scope_;
  // The application being read, which is what a name leaving the instance is
  // resolved from.
  const slang::ast::Scope* lowering_;

  hir::CompilationUnit unit_;
  // Each block body of `unit_`, by a digest of what it states, which sorts
  // them so a block is held against only those that could equal it.
  std::map<stable_hash::Fingerprint, std::vector<hir::BlockBodyId>>
      block_bodies_by_text_;
  // The specialization this unit is lowered as.
  std::string specialization_;

  // What this unit publishes, built by the declaration phase and moved out
  // before any body lowers.
  hir::UnitSignature signature_;
  hir::SpecializationValues values_;
  // What the design's units publish, for the body phase alone. The declaration
  // phase has none, which is what makes "a declaration reads only its own unit"
  // a property of the code rather than a discipline.
  const hir::UnitSignatures* signatures_ = nullptr;
  const BuiltAsQuery* built_as_ = nullptr;
  // What each signature this unit has read out of became in this unit's pool,
  // one entry per signature, so a type published once is taken once however
  // many connections name it.
  std::unordered_map<const hir::UnitSignature*, hir::TypeImportMemo>
      signature_type_memos_;
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
  // The identity of every class this unit's lowering has resolved, so the walk
  // up slang's parent chain finding the unit declaring a class runs once per
  // class.
  std::unordered_map<const slang::ast::ClassType*, hir::ClassId> class_cache_;
  // The classes of other units whose declarations this unit reads once every
  // body has lowered.
  std::vector<hir::ClassId> pending_declaration_reads_;
  // The same for each covergroup this unit's lowering has resolved.
  std::unordered_map<const slang::ast::CovergroupType*, hir::CovergroupId>
      covergroup_cache_;
  // A covergroup whose identity is minted and whose declaration is not: its
  // expressions name what encloses it, so it lowers with the scope
  // replicating it.
  struct PendingCovergroup {
    const slang::ast::CovergroupType* covergroup = nullptr;
    hir::CovergroupId id;
    diag::SourceSpan span;
    const slang::ast::Scope* replicating_scope = nullptr;
  };
  std::unordered_map<const slang::ast::Scope*, std::vector<PendingCovergroup>>
      pending_covergroups_;
  std::unordered_map<const slang::ast::Scope*, std::vector<hir::CovergroupId>>
      covergroups_by_scope_;
  std::unordered_map<const slang::ast::Scope*, std::vector<PendingClassBody>>
      pending_class_bodies_;
  StructuralDataObjectBindings structural_data_object_bindings_;
  std::unordered_map<const slang::ast::Symbol*, InterfacePortBinding>
      interface_port_bindings_;
  SubroutineBindings subroutine_bindings_;
  std::unordered_map<const slang::ast::Expression*, hir::StructuralSubroutineId>
      evaluators_;
  ForeignImportBindings foreign_import_bindings_;
  ForeignImportScopes foreign_import_scopes_;
  OwnedChildBindings owned_child_bindings_;
  std::unordered_map<
      const slang::ast::GenerateBlockArraySymbol*, hir::GenerateId>
      loop_generates_;
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
  std::map<ScopeFrameId, ScopeUnderConstruction> scopes_under_construction_;
  std::uint32_t next_scope_frame_ = 0;
  std::uint32_t next_with_clause_ = 0;
  std::unordered_map<const slang::ast::Symbol*, MintedProceduralScope>
      procedural_scopes_;
};

template <typename Visit>
auto UnitLowerer::WalkOwnDeclarations(Visit& visit)
    -> std::invoke_result_t<Visit&, const slang::ast::Symbol&> {
  using Result = std::invoke_result_t<Visit&, const slang::ast::Symbol&>;
  const auto owned = [&](const slang::ast::Symbol& member) -> Result {
    if (Owns(member)) return visit(member);
    return Result();
  };
  const auto name_of = [&](const slang::ast::ClassType& cls) {
    return UnitName(cls, Naming());
  };
  if constexpr (std::is_void_v<Result>) {
    for (const slang::ast::Scope* application : applications_) {
      WalkDeclarationScopes(*application, owned, name_of);
      for (const slang::ast::ClassType* spec :
           facts_.PlacedIn(application->asSymbol())) {
        owned(*spec);
        WalkDeclarationScopes(*spec, owned, name_of);
      }
    }
  } else {
    for (const slang::ast::Scope* application : applications_) {
      if (auto r = WalkDeclarationScopes(*application, owned, name_of); !r) {
        return r;
      }
      for (const slang::ast::ClassType* spec :
           facts_.PlacedIn(application->asSymbol())) {
        if (auto r = owned(*spec); !r) return r;
        if (auto r = WalkDeclarationScopes(*spec, owned, name_of); !r) {
          return r;
        }
      }
    }
    return {};
  }
}

// Mints the identity of every procedural scope the bodies of `walked`
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
// is minted here, so nothing is minted that no body goes on to fill -- a block
// instance its body was not lowered from mints the identities the one it was
// lowered from fills, since the source orders both alike. The walk
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
