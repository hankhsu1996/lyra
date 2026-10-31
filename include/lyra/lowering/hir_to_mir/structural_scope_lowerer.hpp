#pragma once

#include <cstdint>
#include <deque>
#include <memory>
#include <optional>
#include <span>
#include <string>
#include <utility>
#include <variant>
#include <vector>

#include "lyra/base/arena.hpp"
#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/base/translation.hpp"
#include "lyra/diag/diagnostic.hpp"
#include "lyra/hir/expr.hpp"
#include "lyra/hir/structural_data_object.hpp"
#include "lyra/hir/structural_hops.hpp"
#include "lyra/hir/structural_scope.hpp"
#include "lyra/lowering/hir_to_mir/access_path.hpp"
#include "lyra/lowering/hir_to_mir/class_decl_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/covergroup_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/declared_callable.hpp"
#include "lyra/lowering/hir_to_mir/declared_scope.hpp"
#include "lyra/lowering/hir_to_mir/design_root_inputs.hpp"
#include "lyra/lowering/hir_to_mir/object_change.hpp"
#include "lyra/lowering/hir_to_mir/self_ref.hpp"
#include "lyra/lowering/hir_to_mir/sensitivity_wait.hpp"
#include "lyra/lowering/hir_to_mir/static_var_binding.hpp"
#include "lyra/lowering/hir_to_mir/unit_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/walk_frame.hpp"
#include "lyra/mir/builder.hpp"
#include "lyra/mir/callable_id.hpp"
#include "lyra/mir/class_id.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/field.hpp"
#include "lyra/mir/type_id.hpp"
#include "lyra/support/builtin_fn.hpp"

namespace lyra::lowering::hir_to_mir {

class StructuralScopeLowerer;

// Where the object an entry makes takes its construction table from: supplied
// by whoever asks for the object, or a constant the unit states of itself.
struct ConstructionTableSupplied {};
using ConstructionTableSource =
    std::variant<ConstructionTableSupplied, mir::ClassConstantRef>;

// A value whoever builds a scope hands it: the declaration it fills, and the
// type the constructor takes it as.
struct ConstructionValue {
  hir::StructuralDataObjectId declared;
  mir::TypeId type;
};

// A route made only of parent edges within this unit, `hops` of them. Every
// scope it passes encloses the reader and so exists whenever the reader does,
// which leaves nothing about it to settle: it is walked where it is used.
struct ClimbedRoute {
  hir::StructuralHops hops;
};

// A route that descends into a scope or crosses into another unit, which may
// reach something not built yet, not selected, or answered only by the
// runtime. It is walked once the tree is whole, and what it reached is kept in
// `slot`, a member of the reader's own class.
struct StoredRoute {
  mir::FieldId slot;
};

// How a route is reached from its reader.
using RouteReach = std::variant<ClimbedRoute, StoredRoute>;

// Lowers one HIR structural scope into one MIR class, in two passes over the
// unit's scopes: the first settles what every scope declares, the second lowers
// every body against those. Two, because a body may name any peer's
// declaration while no declaration names a body. A block body is lowered once
// whichever objects build it, and reaches the scopes around it only through
// what each published, so its lowering stands on those published classes. The
// class being built is not held here -- a body
// reaches it down the walk, like everything else that changes as the walk
// moves.
class StructuralScopeLowerer {
 public:
  StructuralScopeLowerer(
      UnitLowerer& unit_lowerer, const hir::StructuralScope& hir_scope,
      std::optional<DesignRootInputs> design_root = std::nullopt)
      : owner_(&unit_lowerer),
        hir_scope_(&hir_scope),
        design_root_(std::move(design_root)) {
  }

  // The lowering of a unit's scopes: the root scope's, and each block body's,
  // by the body's id.
  struct Tree {
    std::unique_ptr<StructuralScopeLowerer> root;
    std::vector<std::unique_ptr<StructuralScopeLowerer>> blocks;
  };

  // The lowering of the unit's root scope and of every block body the unit
  // holds, each taking the identity of the class its unit published of it as
  // it is built -- which is before any type translates, since a type may name
  // one.
  [[nodiscard]] static auto ForRoot(
      UnitLowerer& unit_lowerer, const hir::StructuralScope& root_scope,
      std::optional<DesignRootInputs> design_root) -> Tree;

  // Mints this class's identity, builds its structural shape, and publishes
  // the shape so peer body lowering can query it.
  auto DeclareShape() -> diag::Result<mir::ClassId>;

  // The class the unit published of the object this scope is.
  [[nodiscard]] auto PublishedClassId() const -> mir::ClassId {
    return published_class_id_;
  }

  // Settles every class `unit_lowerer`'s unit published of its scopes: its
  // published members, and a virtual method per published subroutine, which
  // each class realizing it overrides.
  static void DefinePublishedClasses(UnitLowerer& unit_lowerer);

  // The member the generate `generate` holds what it built in.
  [[nodiscard]] auto GenerateHandle(hir::GenerateId generate) const
      -> const mir::ClassFieldTarget& {
    return generate_handles_.Get(generate);
  }

  // Publishes the body that makes a block of the block body this scope is: it
  // takes where the block hangs, the construction table it is built with and
  // the index it is handed, if a loop's, at the type every block entry of the
  // generate has, and builds the block under its own label, at the coordinate
  // that index names (LRM 23.6, 27.4).
  [[nodiscard]] auto PublishBlockEntry() const -> mir::CallableId;

  // The values whoever builds this scope hands it, in the order they are
  // handed.
  [[nodiscard]] auto ConstructionValues() const
      -> std::span<const ConstructionValue> {
    return construction_values_;
  }

  // Lowers every body and every install statement against the already-
  // published shape, and commits the composed class to the compilation unit.
  auto PopulateBodies() -> diag::Result<void>;

  // Central scope-level expression dispatcher. One switch over `hir::Expr::
  // data` routing each kind to its per-family handler.
  [[nodiscard]] auto LowerExpr(const hir::Expr& expr, WalkFrame frame) const
      -> diag::Result<mir::Expr>;

  // The same expression where a part of it is reached: storage it names is
  // that place, unread, and any other expression is its value.
  [[nodiscard]] auto LowerAsPlace(const hir::Expr& expr, WalkFrame frame) const
      -> diag::Result<mir::Expr>;

  // The same expression where nothing reads its value: a loop generate's step.
  // A write is then the write alone.
  [[nodiscard]] auto LowerIgnoredExpr(
      const hir::Expr& expr, WalkFrame frame) const -> diag::Result<mir::Expr>;

  // Dispatcher for an expression named as a part rather than read: addressable
  // kinds only, no auto-Get wrap, peeled into the place that owns the value and
  // the descent above it. Nothing is appended, so a construct that only names
  // the part -- a wait, a join of nets -- asks this.
  [[nodiscard]] auto LowerAccessPath(
      const hir::Expr& expr, WalkFrame frame) const -> diag::Result<AccessPath>;

  // The same part as the target of a write, with the checks the write owes
  // before it lands appended where the statement is reached.
  [[nodiscard]] auto LowerLhsExpr(const hir::Expr& expr, WalkFrame frame) const
      -> diag::Result<AccessPath>;

  [[nodiscard]] auto Owner() const -> UnitLowerer& {
    return *owner_;
  }

  [[nodiscard]] auto HirScope() const -> const hir::StructuralScope& {
    return *hir_scope_;
  }

  // Where the object the unit's object entry makes takes its construction
  // table from: handed in by whoever asks for the object, or, for the design
  // root, which nothing instantiates (LRM 23.3.1), the table the root states
  // of itself. Settled once the bodies are populated.
  [[nodiscard]] auto ConstructionTable() const
      -> const ConstructionTableSource& {
    return construction_table_;
  }

  // The scope's time unit (LRM 3.14.2), which a time query scales its result
  // to. Named as the procedural pass names it, so a handler shared by both
  // passes reads it the same way.
  [[nodiscard]] auto Resolution() const -> TimeResolution {
    return hir_scope_->time_resolution;
  }

  // The expression arena of the scope being lowered. The uniform sub-expression
  // accessor the context-free expression handler templates reach through; both
  // lowering pass classes expose it with the same shape so those templates bind
  // to either.
  [[nodiscard]] auto HirExprs() const
      -> const base::Arena<hir::Expr, hir::ExprId>& {
    return hir_scope_->exprs;
  }

  // The pattern arena of the scope being lowered, exposed with the same shape
  // on both pass classes for the same reason the expression arena is.
  [[nodiscard]] auto HirPatterns() const
      -> const base::Arena<hir::Pattern, hir::PatternId>& {
    return hir_scope_->patterns;
  }

  // The HIR declaration of a subroutine this scope declares. It is complete
  // before any body is lowered, so a call can read a peer's formals even when
  // the peer's MIR declaration is not yet built (forward / mutual reference,
  // LRM 13.7). The desugar reads the formals' directions and types from here.
  [[nodiscard]] auto LookupHirSubroutine(hir::StructuralSubroutineId id) const
      -> const hir::SubroutineDecl& {
    return hir_scope_->structural_subroutines.Get(id);
  }

  // How each of this scope's routes, of every use, is reached from it.
  [[nodiscard]] auto ReachOf(hir::RoutedValueRefId hir_id) const
      -> const RouteReach& {
    return value_reaches_.Get(hir_id);
  }
  [[nodiscard]] auto ReachOf(hir::RoutedObjectRefId hir_id) const
      -> const RouteReach& {
    return object_reaches_.Get(hir_id);
  }
  [[nodiscard]] auto ReachOf(hir::RoutedDisableTargetRefId hir_id) const
      -> const RouteReach& {
    return disable_target_reaches_.Get(hir_id);
  }

  // What a route ends at, as the reader at `frame` reaches it -- a borrowed
  // pointer to it: the route walked there, or the slot it was kept in. Appends
  // to `frame.current_block`. A value is read through an endpoint instead,
  // which also knows what kind of member the route ends at.
  [[nodiscard]] auto RouteEnd(
      const WalkFrame& frame, hir::RoutedObjectRefId id) const -> mir::ExprId;
  [[nodiscard]] auto RouteEnd(
      const WalkFrame& frame, hir::RoutedDisableTargetRefId id) const
      -> mir::ExprId;

  // The MIR field the history of one sampled expression became (LRM 16.9.3).
  // It takes no hop count: a history is recorded on the scope whose process
  // reads it, so the reader and the storage are always the same scope.
  [[nodiscard]] auto TranslateSampledHistory(hir::SampledHistoryId hir_id) const
      -> mir::FieldId {
    return sampled_history_fields_.Get(hir_id);
  }

  // The MIR field holding one concurrent assertion's attempts (LRM 16.14.1).
  // It takes no hop count for the same reason: an assertion is evaluated on the
  // scope that declares it.
  [[nodiscard]] auto TranslateConcurrentAssertion(
      hir::ConcurrentAssertionId hir_id) const -> mir::FieldId {
    return concurrent_assertion_fields_.Get(hir_id);
  }

  // The MIR field a structural data object of this scope became: a field of
  // the published class where the unit published it, and of this scope's own
  // class otherwise.
  [[nodiscard]] auto TranslateStructuralDataObject(
      hir::StructuralDataObjectId hir_id) const -> mir::ClassFieldTarget {
    return data_object_fields_.Get(hir_id);
  }

  // The MIR field an interface port of this scope became.
  [[nodiscard]] auto TranslateInterfacePort(hir::InterfacePortId hir_id) const
      -> mir::ClassFieldTarget {
    return interface_port_fields_.Get(hir_id);
  }

  // Registry identity of the class this scope lowers to.
  [[nodiscard]] auto ClassId() const -> mir::ClassId {
    return class_id_;
  }

  // The field one of this scope's static-lifetime body variables was given. A
  // reference names the declaration rather than the procedural scopes around
  // it, and the storage is a field of this scope's object, so a referrer
  // standing on this object is already standing on the cell.
  [[nodiscard]] auto ProceduralStaticField(
      const hir::ProceduralBodyRef& body, hir::ProceduralVarId var) const
      -> mir::ClassFieldTarget {
    const StaticVarBindings& statics = std::visit(
        Overloaded{
            [&](hir::ProcessId id) -> const StaticVarBindings& {
              return process_static_bindings_.Get(id);
            },
            [&](hir::StructuralSubroutineId id) -> const StaticVarBindings& {
              return declared_subroutines_.Get(id).statics;
            }},
        body);
    for (const StaticVarBinding& binding : statics) {
      if (binding.var == var) return InstanceFieldOf(binding);
    }
    throw InternalError(
        "StructuralScopeLowerer::ProceduralStaticField: the var was given no "
        "persistent storage, so it is not a static-lifetime variable of that "
        "body");
  }

  // What each of this scope's procedural scopes owns at run time.
  [[nodiscard]] auto Scopes() const -> const DeclaredScopes& {
    return scopes_;
  }

  // The field carrying what a `disable` naming one of this scope's procedural
  // scopes terminates (LRM 9.6.2). A scope of the design hierarchy is
  // replicated with its instance, so the cell is a field of this scope's
  // object and a referrer standing on this object is already standing on it.
  [[nodiscard]] auto DisableTargetField(hir::ProceduralScopeId scope) const
      -> mir::ClassFieldTarget {
    const std::optional<StaticStorageHome>& home =
        scopes_.Get(scope).disable_target;
    if (!home.has_value()) {
      throw InternalError(
          "StructuralScopeLowerer::DisableTargetField: the scope owns no "
          "disable target, so the source named it nothing and no name could "
          "have reached it -- please report this as a bug");
    }
    return std::get<InstanceFieldHome>(*home).field;
  }

  // The call target a subroutine this scope declares resolves to: this
  // scope's class, and the callable's identity within it.
  [[nodiscard]] auto TranslateStructuralSubroutine(
      hir::StructuralSubroutineId hir_id) const -> mir::Direct {
    return mir::Direct{
        .target = mir::CallableTarget{
            .owner = class_id_,
            .slot = declared_subroutines_.Get(hir_id).callable}};
  }

  // The MIR field one instance member became. A declaration is one field
  // whatever its multiplicity: what it holds is a handle for a single instance
  // and a sequence of them for an array, which is what the field's type states.
  [[nodiscard]] auto InstanceMemberField(hir::InstanceMemberId hir_id) const
      -> mir::ClassFieldTarget {
    return instance_member_fields_.Get(hir_id);
  }

 private:
  // The lowering of `hir_scope`, taking the identity of the class its unit
  // published of it.
  [[nodiscard]] static auto LoweringOf(
      UnitLowerer& unit_lowerer, const hir::StructuralScope& hir_scope,
      std::optional<DesignRootInputs> design_root)
      -> std::unique_ptr<StructuralScopeLowerer>;

  UnitLowerer* owner_;
  const hir::StructuralScope* hir_scope_;
  // The classes the unit published of the scopes standing around this one,
  // outermost first: none for the unit's own scope.
  std::vector<mir::ClassId> published_around_;
  // Present only on the design root's own scope, the sole scope whose
  // elaboration spans the whole design: it brings up every namespace and
  // states every specialization's construction table as constants of its
  // class.
  std::optional<DesignRootInputs> design_root_;
  ConstructionTableSource construction_table_ = ConstructionTableSupplied{};
  base::Translation<hir::StructuralDataObjectId, mir::ClassFieldTarget>
      data_object_fields_;
  base::Translation<hir::SampledHistoryId, mir::FieldId>
      sampled_history_fields_;
  base::Translation<hir::ConcurrentAssertionId, mir::FieldId>
      concurrent_assertion_fields_;
  base::Translation<hir::InterfacePortId, mir::ClassFieldTarget>
      interface_port_fields_;
  base::Translation<hir::RoutedValueRefId, RouteReach> value_reaches_;
  base::Translation<hir::RoutedObjectRefId, RouteReach> object_reaches_;
  base::Translation<hir::RoutedDisableTargetRefId, RouteReach>
      disable_target_reaches_;
  // The member each generate construct holds what it built in: the block it
  // built, or for a loop its blocks by the value each one's index stood at.
  // Either holds the base every scope extends, because which body a block was
  // built as is the object's, and a step reaching one views it as the class
  // its scope published.
  base::Translation<hir::GenerateId, mir::ClassFieldTarget> generate_handles_;
  base::Translation<hir::InstanceMemberId, mir::ClassFieldTarget>
      instance_member_fields_;
  DeclaredScopes scopes_;
  base::Translation<hir::StructuralSubroutineId, DeclaredCallable>
      declared_subroutines_;
  // A process is anonymous (LRM 9.2), so nothing can name it early and it takes
  // no callable identity: its answer is storage alone, where a subroutine's is
  // a whole declared callable.
  base::Translation<hir::ProcessId, StaticVarBindings> process_static_bindings_;
  // The same for a concurrent assertion's action block, whose block-local
  // variables are static by default as any procedure's are (LRM 6.21).
  base::Translation<hir::ConcurrentAssertionId, StaticVarBindings>
      assertion_static_bindings_;
  mir::ClassId class_id_{};
  // The member this scope's object keeps the object of the scope around it in,
  // for a block of a generate construct: the scope around it builds it and
  // hands it itself, which never changes after, as an inner class keeps its
  // enclosing instance. So a name declared out there is reached by reading this
  // member, once per scope climbed. Absent for a unit's own scope, which no
  // scope of its unit encloses.
  std::optional<mir::ClassFieldTarget> enclosing_;
  // Each is a constructor parameter after the prefix every scope takes, and is
  // filled from it before anything the construction does can read it.
  std::vector<ConstructionValue> construction_values_;
  // What the unit published of the object this scope is -- an instance of the
  // unit, or a generate block inside one: what it published as its first
  // fields, in the order the signature states them, and a virtual method per
  // published subroutine, which the class above overrides. The class above
  // extends it with everything the lowering adds. A referrer compiles against
  // this one and holds nothing else, so what the unit adds while lowering its
  // bodies moves no field a referrer reads.
  mir::ClassId published_class_id_{};
  // The classes this scope replicates: those it declares (LRM 23.9), and the
  // specializations of a generic on one (LRM 8.25), each a type of this scope's
  // instance (LRM 6.22). So the scope both settles its shape and lowers its
  // bodies -- which is what gives a class body the reach a process of the scope
  // has, and the instance to record.
  std::vector<ClassDeclLowerer> class_lowerers_;
  std::deque<CovergroupLowerer> covergroup_lowerers_;
};

// Which property a class property access names (LRM 8.4): the member at the
// position the class declaring it gave it, this unit's or the one another
// unit published.
template <typename Lowerer>
auto PropertyNameOf(Lowerer& lowerer, const hir::ClassPropertyTarget& target)
    -> mir::ClassFieldTarget {
  return lowerer.Owner().TranslateClassPropertyTarget(target);
}

// The storage one class property access reaches, over `receiver` (LRM 8.4).
template <typename Lowerer>
auto BuildClassPropertyAccess(
    Lowerer& lowerer, const WalkFrame& frame, mir::ExprId receiver,
    const hir::ClassPropertyTarget& target, mir::TypeId reached) -> mir::Expr {
  mir::CompilationUnit& unit = lowerer.Owner().Unit();
  return PropertyStorage(
      unit, frame.Builder(), ReportedObject(unit, frame, receiver),
      PropertyNameOf(lowerer, target), reached);
}

// The type `field` was declared with: read off its class's shape, which is
// settled before any body lowers, for a class this unit defines, and off the
// declaration of one another unit defines.
[[nodiscard]] inline auto FieldTypeOf(
    const UnitLowerer& unit_lowerer, const mir::ClassFieldTarget& field)
    -> mir::TypeId {
  if (mir::IsDefinedElsewhere(unit_lowerer.Unit(), field.owner)) {
    return unit_lowerer.Unit()
        .GetClass(field.owner)
        .fields.Get(field.slot)
        .type;
  }
  return unit_lowerer.GetClassShape(field.owner).fields.Get(field.slot).type;
}

// A value the walk has reached, and the type it has there.
struct ReachedObject {
  mir::ExprId expr;
  mir::TypeId type;
};

// Picks one object out of a value standing for several: one index per instance
// select (LRM 23.6), each taking a dimension off what the value holds. A select
// is resolved where the name is, so an index crosses as a constant rather than
// as a value the design computes, and a value standing for one object takes no
// select and comes back as it went in.
auto ApplyInstanceSelects(
    UnitLowerer& unit_lowerer, mir::Builder block, ReachedObject reached,
    std::span<const std::uint32_t> selects) -> ReachedObject;

// The call `start` makes of `body`, a callable of the scope `frame` stands on
// whose result is the execution it runs as, written into `frame`'s block,
// whether it is a procedure the scope registers or an assertion's action it
// runs later. The call names the scope; the instance whose text creates the
// execution, which holds the seeds LRM 18.14.1 splits its generator from and
// whose code it evaluates; and where the source writes it. A continuous driver
// is not started here, because it may evaluate another instance's code and so
// also names the instance it runs as.
[[nodiscard]] auto BuildExecutionStart(
    UnitLowerer& unit_lowerer, const WalkFrame& frame, mir::CallableId body,
    support::BuiltinFn start, diag::SourceSpan written_at) -> mir::ExprId;

}  // namespace lyra::lowering::hir_to_mir
