#include "lyra/lowering/hir_to_mir/structural_scope_lowerer.hpp"

#include <algorithm>
#include <cstdint>
#include <expected>
#include <format>
#include <optional>
#include <span>
#include <string>
#include <string_view>
#include <unordered_map>
#include <utility>
#include <variant>
#include <vector>

#include "lyra/base/id_allocator.hpp"
#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/diag/diagnostic.hpp"
#include "lyra/hir/procedural_body.hpp"
#include "lyra/hir/procedural_scope.hpp"
#include "lyra/hir/procedural_var.hpp"
#include "lyra/hir/structural_scope.hpp"
#include "lyra/lowering/hir_to_mir/binding_origin.hpp"
#include "lyra/lowering/hir_to_mir/block_builder.hpp"
#include "lyra/lowering/hir_to_mir/callable_bindings.hpp"
#include "lyra/lowering/hir_to_mir/class_shape.hpp"
#include "lyra/lowering/hir_to_mir/concurrent_assertion.hpp"
#include "lyra/lowering/hir_to_mir/condition.hpp"
#include "lyra/lowering/hir_to_mir/continuous_assign.hpp"
#include "lyra/lowering/hir_to_mir/declaration_initializer.hpp"
#include "lyra/lowering/hir_to_mir/default_value.hpp"
#include "lyra/lowering/hir_to_mir/design_namespaces.hpp"
#include "lyra/lowering/hir_to_mir/expression/dpi_call.hpp"
#include "lyra/lowering/hir_to_mir/forwarding_entry.hpp"
#include "lyra/lowering/hir_to_mir/integral_literal.hpp"
#include "lyra/lowering/hir_to_mir/lhs_store.hpp"
#include "lyra/lowering/hir_to_mir/net_declaration.hpp"
#include "lyra/lowering/hir_to_mir/process_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/runtime_call.hpp"
#include "lyra/lowering/hir_to_mir/sampled_history.hpp"
#include "lyra/lowering/hir_to_mir/self_ref.hpp"
#include "lyra/lowering/hir_to_mir/sensitivity_wait.hpp"
#include "lyra/lowering/hir_to_mir/statement/loops.hpp"
#include "lyra/lowering/hir_to_mir/static_var_binding.hpp"
#include "lyra/lowering/hir_to_mir/walk_frame.hpp"
#include "lyra/mir/behavior_ordinal.hpp"
#include "lyra/mir/class.hpp"
#include "lyra/mir/class_ref.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/field.hpp"
#include "lyra/mir/integral_constant.hpp"
#include "lyra/mir/local.hpp"
#include "lyra/mir/runtime_record.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/mir/type.hpp"
#include "lyra/mir/type_builders.hpp"
#include "lyra/support/builtin_fn.hpp"
#include "lyra/support/strength_level.hpp"

namespace lyra::lowering::hir_to_mir {

namespace {

// Adds the runtime scope base's construction prefix (parent, hierarchy
// segment) as ordinary ctor params, in the order the base
// constructor consumes them.
void AttachRuntimeScopeCtorPrefix(
    const mir::CompilationUnit& unit, ClassShape& shape) {
  const auto& builtins = unit.builtins;
  shape.ctor_prefix_params.Add(mir::ParamDecl{.type = builtins.scope_ptr});
  shape.ctor_prefix_params.Add(
      mir::ParamDecl{.type = builtins.hierarchy_segment});
}

// The declaration of a value whoever constructs the scope supplies, if the
// scope has one. A scope has at most one, because the only thing supplied that
// way is the index a loop generate builds it at (LRM 27.4).
auto ConstructionValueOf(const hir::StructuralScope& scope)
    -> std::optional<hir::StructuralDataObjectId> {
  for (const hir::StructuralDataObjectId id :
       scope.structural_data_objects.Ids()) {
    if (std::holds_alternative<hir::StructuralConstructionValueDecl>(
            scope.structural_data_objects.Get(id).kind)) {
      return id;
    }
  }
  return std::nullopt;
}

// A class while it is being built: the class, the constructor a value of it is
// entered through, and the locals that constructor hands its base. The three
// are settled together and travel together -- whoever installs the runtime's
// record and whoever finishes the constructor want all of them -- so which
// class is meant is decided once rather than once per part.
struct ClassUnderConstruction {
  mir::Class* cls = nullptr;
  mir::CallableCode* ctor = nullptr;
  const std::vector<mir::LocalId>* prefix = nullptr;
};

// What a unit promised of its object, before its construction protocol is
// settled: the class under construction, and the behavior stated for each
// published member, which the realizing class takes over once it has one.
struct BuiltPromise {
  mir::Class cls;
  mir::CallableCode ctor;
  std::vector<mir::LocalId> ctor_prefix;
  std::vector<PromisedAccessor> accessors;
};

// Builds what a unit promised of its object: a behavior per published member,
// each answering with the storage behind that member, over the base that roots
// an object in the runtime's tree. It declares no storage and defines no body
// -- what each behavior answers with is the realizing class's, that being the
// only thing that ever builds one -- so the constructor here forwards what it
// is handed and stops.
//
// It is the promise rather than the realization that stands directly in the
// tree, so whatever a target's tree class has to be entered with is entered
// here. Nothing of it reaches the realization's own signature as a result,
// which is what keeps a record only one target reads out of every signature.
auto BuildPromise(
    mir::CompilationUnit& unit, mir::ClassId promise, std::string name,
    std::span<const PromisedMember> members,
    std::span<const PromisedSubroutine> subroutines,
    const mir::Class& realization) -> BuiltPromise {
  const mir::TypeId self_pointer = unit.types.Intern(
      mir::Type{mir::PointerType{
          .pointee = unit.types.Intern(
              mir::Type{mir::ObjectType{.class_id = promise}}),
          .ownership = mir::PointerOwnership::kBorrowed}});

  mir::CallableCode ctor = mir::CallableCode::Defined();
  const mir::LocalId ctor_self = ctor.AddLocal(self_pointer);
  std::vector<mir::LocalId> prefix;
  for (const mir::TypeId type :
       {unit.builtins.scope_ptr, unit.builtins.hierarchy_segment}) {
    prefix.push_back(ctor.AddLocal(type));
  }
  ctor.params = {ctor_self, prefix[0], prefix[1]};
  ctor.result_type = unit.builtins.void_type;

  mir::Class cls;
  cls.name = std::move(name);
  cls.base = mir::ClassRef{
      mir::RuntimeClassRef{.symbol = std::string{mir::kObjectTreeClassSymbol}}};
  cls.self_pointer_type = self_pointer;

  std::vector<PromisedAccessor> stated;
  stated.reserve(members.size());
  for (const PromisedMember& member : members) {
    mir::CallableCode code{};
    const mir::LocalId self = code.AddLocal(self_pointer);
    code.params = {self};
    code.result_type = unit.types.Intern(
        mir::Type{mir::PointerType{
            .pointee = member.cell_type,
            .ownership = mir::PointerOwnership::kBorrowed}});
    const mir::CallableId id = cls.callables.Add(
        mir::CallableDecl{
            .code = std::move(code),
            .foreign = std::nullopt,
            .virtual_dispatch =
                mir::VirtualDispatchRole{mir::IntroducesVirtualSlot{}}});
    cls.named_callables.push_back(
        mir::NamedCallable{.name = member.name, .body = id});
    stated.push_back(PromisedAccessor{.behavior = id, .cell = member.cell});
  }

  // A published subroutine is a behavior like any other, stated with the
  // signature the realizing body already has -- its own formals unchanged, and
  // the object it is entered on retyped to what a referrer holds.
  for (const PromisedSubroutine& subroutine : subroutines) {
    const mir::CallableCode& body =
        realization.callables.Get(subroutine.body).code;
    mir::CallableCode code{};
    code.params.reserve(body.params.size());
    code.params.push_back(code.AddLocal(self_pointer));
    for (std::size_t at = 1; at < body.params.size(); ++at) {
      code.params.push_back(
          code.AddLocal(body.locals.Get(body.params[at]).type));
    }
    code.result_type = body.result_type;
    const mir::CallableId id = cls.callables.Add(
        mir::CallableDecl{
            .code = std::move(code),
            .foreign = std::nullopt,
            .virtual_dispatch =
                mir::VirtualDispatchRole{mir::IntroducesVirtualSlot{}}});
    cls.named_callables.push_back(
        mir::NamedCallable{.name = subroutine.name, .body = id});
  }

  return BuiltPromise{
      .cls = std::move(cls),
      .ctor = std::move(ctor),
      .ctor_prefix = std::move(prefix),
      .accessors = std::move(stated)};
}

auto MakeUniqueObjectPointer(UnitLowerer& unit_lowerer, mir::ClassId class_id)
    -> mir::TypeId {
  const mir::TypeId object_type = unit_lowerer.Unit().types.Intern(
      mir::Type{mir::ObjectType{.class_id = class_id}});
  return unit_lowerer.Unit().types.Intern(
      mir::Type{mir::PointerType{
          .pointee = object_type,
          .ownership = mir::PointerOwnership::kUnique}});
}

// The pointer type a handle to one of `member`'s objects has. The object is one
// the declaring unit publishes, so the type names this unit's record of it.
auto MakeExternalUnitPointer(
    UnitLowerer& unit_lowerer, const hir::InstanceMemberDecl& member,
    mir::PointerOwnership ownership) -> mir::TypeId {
  const mir::TypeId object_type = unit_lowerer.Unit().types.Intern(
      mir::Type{mir::ExternalUnitObjectType{
          .object = unit_lowerer.TranslateExternalUnitObject(member.object)}});
  return unit_lowerer.Unit().types.Intern(
      mir::Type{
          mir::PointerType{.pointee = object_type, .ownership = ownership}});
}

// A type wrapped once per dimension still to be fixed: what a declaration
// standing for several objects covers where `depth` of its coordinates are
// open. At depth zero it is the type itself, which is why one object needs no
// case of its own.
auto SequenceOver(
    UnitLowerer& unit_lowerer, mir::TypeId type, std::size_t depth)
    -> mir::TypeId {
  for (std::size_t i = 0; i < depth; ++i) {
    type = unit_lowerer.Unit().types.Intern(
        mir::Type{mir::VectorType{.element = type}});
  }
  return type;
}

// The type of the member one instance declaration becomes. Multiplicity is this
// type and nothing else, so an array and a single instance are one declaration
// shape differing in how many wrappers stand over the handle (LRM 23.3.2).
auto MakeInstanceMemberType(
    UnitLowerer& unit_lowerer, const hir::InstanceMemberDecl& member,
    mir::PointerOwnership ownership) -> mir::TypeId {
  return SequenceOver(
      unit_lowerer, MakeExternalUnitPointer(unit_lowerer, member, ownership),
      member.array_dims.size());
}

// The position a coordinate names, as the machine integer a sequence is indexed
// by. Which element a route reaches is settled during elaboration, so it
// crosses as a constant rather than as a value the design computes.
auto BuildSequenceIndex(
    UnitLowerer& unit_lowerer, mir::Block& block, std::uint32_t coord)
    -> mir::ExprId {
  return block.exprs.Add(
      mir::Expr{
          .data =
              mir::MachineIntLiteral{.value = static_cast<std::int64_t>(coord)},
          .type = unit_lowerer.Unit().builtins.machine_int64});
}

// Builds one object an external-unit instance member declares, at the positions
// `coords` names, and hands back the borrowed pointer the runtime tree returns.
// The object is built and given to the tree to own; its Segment -- the label
// plus those positions -- is the key a by-name descent matches it on. A
// position is a value the construction counts out rather than a constant, so a
// declaration covering many objects builds them in a loop; a scalar instance is
// the position-free case, built by the same expression.
auto BuildOwnedInstance(
    UnitLowerer& unit_lowerer, const WalkFrame& frame, mir::ExprId parent_self,
    const std::string& runtime_label, std::string_view declaring_unit,
    mir::TypeId owning_pointer_type, mir::TypeId borrowed_pointer_type,
    std::span<const mir::LocalId> coords) -> mir::ExprId {
  mir::Block& block = *frame.current_block;
  const auto& builtins = unit_lowerer.Unit().builtins;

  std::vector<mir::ExprId> indices;
  indices.reserve(coords.size());
  for (const mir::LocalId coord : coords) {
    indices.push_back(
        block.exprs.Add(mir::MakeLocalRefExpr(coord, builtins.int_type)));
  }
  const mir::TypeId indices_type = mir::MachineArrayOf(
      unit_lowerer.Unit().types, builtins.int_type, indices.size());
  const mir::ExprId indices_id = block.exprs.Add(
      mir::Expr{
          .data = mir::CompositeExpr{.parts = std::move(indices)},
          .type = indices_type});
  const mir::ExprId segment_id = block.exprs.Add(
      mir::Expr{
          .data =
              mir::CallExpr{
                  .callee = mir::Construct{},
                  .arguments =
                      {block.exprs.Add(
                           mir::MakeStringLiteral(
                               builtins.string, runtime_label)),
                       indices_id}},
          .type = builtins.hierarchy_segment});

  // This unit consumed what the instantiated one promised, and a promise states
  // what may be reached and never how much storage an object takes, so the
  // object is asked for rather than made here.
  const mir::ExprId ctor_call_id = block.exprs.Add(
      mir::Expr{
          .data =
              mir::CallExpr{
                  .callee =
                      mir::Direct{
                          .target =
                              mir::ExternalUnitMintedEntryTarget{
                                  .unit_name = std::string{declaring_unit},
                                  .entry = mir::MintedEntry::kMakeObject}},
                  .arguments = {parent_self, segment_id}},
          .type = owning_pointer_type});

  // The runtime tree owns the instance (AddOwnedChild consumes the freshly
  // built owning pointer) and hands back a borrowed handle, which is what
  // the parent keeps and what a layout-visible route step projects through.
  const mir::ExprId add_id = block.exprs.Add(
      mir::Expr{
          .data =
              mir::CallExpr{
                  .callee =
                      mir::Direct{
                          .target = support::BuiltinFn::kAddOwnedChild,
                          .receiver = parent_self},
                  .arguments = {ctor_call_id}},
          .type = builtins.scope_ptr});
  return block.exprs.Add(
      mir::Expr{
          .data = mir::CastExpr{.operand = add_id},
          .type = borrowed_pointer_type});
}

// Builds what an instance declaration's member holds once `coords` are fixed as
// far as they go: the handle to the object those positions name when they are
// complete, and the sequence the next dimension counts out while they are not.
// Counting a dimension out is what the object graph does at construction, so
// the work reaches the target as a loop over one body rather than as one
// expression per element, and how many objects a declaration covers stops being
// something the artifact grows with. The sequence a member holds is complete
// when the member receives it, which is why the one that grows is a local the
// steps below own and nothing else can name.
auto BuildInstanceMemberValue(
    UnitLowerer& unit_lowerer, const WalkFrame& frame,
    const hir::InstanceMemberDecl& member, std::string_view declaring_unit,
    mir::TypeId owning, mir::TypeId borrowed, std::vector<mir::LocalId>& coords)
    -> mir::ExprId {
  mir::Block& block = *frame.current_block;
  if (coords.size() == member.array_dims.size()) {
    const mir::ExprId parent_self = block.exprs.Add(
        MakeSelfRefExpr(frame, frame.current_class->self_pointer_type));
    return BuildOwnedInstance(
        unit_lowerer, frame, parent_self, member.instance_name, declaring_unit,
        owning, borrowed, coords);
  }

  const mir::CompilationUnit& unit = unit_lowerer.Unit();
  const std::uint32_t count = member.array_dims[coords.size()];
  const mir::TypeId sequence_type = SequenceOver(
      unit_lowerer, borrowed, member.array_dims.size() - coords.size());

  BlockBuilder steps(frame);
  mir::Block& body = steps.Body();
  const mir::LocalId sequence =
      steps.Bindings().DeclareAnonymous(sequence_type);
  body.AppendStmt(
      mir::LocalDeclStmt{
          .target = sequence,
          .init = body.exprs.Add(
              BuildSequenceConstructionCall(unit, body, sequence_type, {}))});

  const mir::LocalId position =
      steps.Bindings().DeclareAnonymous(unit.builtins.int_type);
  mir::Block element_block;
  const WalkFrame element_frame = steps.Frame().WithBlock(&element_block);
  coords.push_back(position);
  const mir::ExprId element = BuildInstanceMemberValue(
      unit_lowerer, element_frame, member, declaring_unit, owning, borrowed,
      coords);
  coords.pop_back();

  const mir::ExprId grown = element_block.exprs.Add(
      mir::Expr{
          .data =
              mir::CallExpr{
                  .callee =
                      mir::Direct{
                          .target = support::BuiltinFn::kExtendSequence},
                  .arguments =
                      {element_block.exprs.Add(
                           mir::MakeLocalRefExpr(sequence, sequence_type)),
                       element}},
          .type = sequence_type});
  element_block.AppendStmt(
      mir::ExprStmt{
          .expr = element_block.exprs.Add(
              mir::MakeAssignExpr(
                  element_block.exprs.Add(
                      mir::MakeLocalRefExpr(sequence, sequence_type)),
                  grown, sequence_type))});

  const mir::BlockId element_scope =
      body.child_scopes.Add(std::move(element_block));
  body.AppendStmt(BuildCountingLoopStmt(
      unit, steps.Frame(), body,
      BuildIntLiteral(unit, body, static_cast<std::int64_t>(count)), position,
      element_scope));

  return block.exprs.Add(steps.Build(
      body.exprs.Add(mir::MakeLocalRefExpr(sequence, sequence_type))));
}

// Emits the constructor-body construction for every object the scope's instance
// members declare: one value per declaration, holding every object that
// declaration builds, stored into the one field it was given. The runtime tree
// owns every built instance; the field keeps the borrowed handles a
// layout-visible route projects through.
void EmitInstanceMemberConstruction(
    StructuralScopeLowerer& lowerer, WalkFrame frame) {
  UnitLowerer& unit_lowerer = lowerer.Owner();
  mir::Block& block = *frame.current_block;
  const hir::StructuralScope& hir_scope = lowerer.HirScope();
  for (const hir::InstanceMemberId id : hir_scope.instance_members.Ids()) {
    const hir::InstanceMemberDecl& im = hir_scope.instance_members.Get(id);
    const mir::TypeId owning = MakeExternalUnitPointer(
        unit_lowerer, im, mir::PointerOwnership::kUnique);
    const mir::TypeId borrowed = MakeExternalUnitPointer(
        unit_lowerer, im, mir::PointerOwnership::kBorrowed);
    const std::string& declaring_unit =
        unit_lowerer.Unit()
            .external_unit_objects
            .Get(unit_lowerer.TranslateExternalUnitObject(im.object))
            .unit_name;
    std::vector<mir::LocalId> coords;
    const mir::ExprId value = BuildInstanceMemberValue(
        unit_lowerer, frame, im, declaring_unit, owning, borrowed, coords);
    const mir::TypeId member_type =
        SequenceOver(unit_lowerer, borrowed, im.array_dims.size());
    const mir::ExprId member = block.exprs.Add(
        mir::MakeFieldAccessExpr(
            block.exprs.Add(
                MakeSelfRefExpr(frame, frame.current_class->self_pointer_type)),
            mir::ClassFieldTarget{
                .owner = frame.current_class_id,
                .slot = lowerer.InstanceMemberField(id)},
            member_type));
    block.AppendStmt(
        mir::ExprStmt{
            .expr = block.exprs.Add(
                mir::MakeAssignExpr(member, value, member_type))});
  }
}

// What a route of each use is reached by: a pointer to the cell or the object
// it ends at, which depends on where it ends; a pointer to a disable target,
// and an entry's code address, which is one already, each the same for every
// route of their use.
auto PointerTypeOf(UnitLowerer& unit_lowerer, const hir::DataLeaf& leaf)
    -> mir::TypeId {
  const hir::DataCell cell = hir::CellOf(leaf);
  return unit_lowerer.Unit().types.Intern(
      mir::Type{mir::PointerType{
          .pointee = unit_lowerer.MemberCellType(
              unit_lowerer.TranslateType(cell.type), cell.storage),
          .ownership = mir::PointerOwnership::kBorrowed}});
}

auto PointerTypeOf(UnitLowerer& unit_lowerer, const hir::ScopeLeaf& leaf)
    -> mir::TypeId {
  return unit_lowerer.Unit().types.Intern(
      mir::Type{mir::PointerType{
          .pointee = unit_lowerer.TranslateType(leaf.type),
          .ownership = mir::PointerOwnership::kBorrowed}});
}

auto DisableTargetPointerType(mir::TypePool& types) -> mir::TypeId {
  return types.Intern(
      mir::Type{mir::PointerType{
          .pointee = types.Intern(
              mir::Type{mir::RuntimeLibraryType{
                  .kind = mir::RuntimeLibraryKind::kCancellationTarget}}),
          .ownership = mir::PointerOwnership::kBorrowed}});
}

// A member name answered with where it lands rather than with what runs: a
// borrowed pointer to the record stating the position. That record lives as
// long as the class does, which is as long as any reference settled against
// it, so nothing is copied anywhere to outlive the lookup. A name answered with
// the body itself is a code address instead, the same shape an entry reached
// by name holds, and the call restores the prototype it was generated with.
auto CoordinateType(mir::TypePool& types, mir::RuntimeLibraryKind kind)
    -> mir::TypeId {
  return types.Intern(
      mir::Type{mir::PointerType{
          .pointee =
              types.Intern(mir::Type{mir::RuntimeLibraryType{.kind = kind}}),
          .ownership = mir::PointerOwnership::kBorrowed,
          .mutability = mir::Mutability::kReadOnly}});
}

// Settles how each route of one use is reached. A route made only of parent
// edges within this unit is walked where it is used and takes no member. Any
// other -- downward, sideways, `$root`-anchored or named -- takes one slot,
// typed by `slot_type` from what the route ends at, so a body reaching through
// it meets the target's own access protocol and no other. The type is interned
// per slot rather than once per use, so a unit that keeps no route of a use
// carries none of the types it would be kept in -- which is what lets a backend
// read off its own types whether it meets the form at all.
template <typename Leaf, typename Id, typename SlotType>
auto DeclareReaches(
    ClassShape& shape, const base::Arena<hir::Route<Leaf>, Id>& routes,
    SlotType slot_type) -> base::Translation<Id, RouteReach> {
  std::vector<RouteReach> reaches;
  reaches.reserve(routes.size());
  for (const hir::Route<Leaf>& route : routes) {
    const auto* in_unit = std::get_if<hir::InUnitHead>(&route.head);
    if (in_unit != nullptr && route.steps.empty()) {
      reaches.emplace_back(ClimbedRoute{.hops = in_unit->hops});
    } else {
      reaches.emplace_back(
          StoredRoute{.slot = shape.AddField(slot_type(route.leaf))});
    }
  }
  return {routes.size(), std::move(reaches)};
}

// Builds one `PackedArray[]` value carrying every per-axis index for a
// single hop; the runtime SDK's `GetChild` / `ResolveVisibleChild` accept
// it as a `std::span<PackedArray>`.
auto BuildIndicesLiteral(
    UnitLowerer& unit_lowerer, mir::Block& block,
    std::span<const std::uint32_t> indices) -> mir::ExprId {
  const auto& builtins = unit_lowerer.Unit().builtins;
  std::vector<mir::ExprId> ids;
  ids.reserve(indices.size());
  for (const std::uint32_t idx : indices) {
    ids.push_back(BuildIntLiteral(
        unit_lowerer.Unit(), block, static_cast<std::int64_t>(idx)));
  }
  const mir::TypeId indices_type = mir::MachineArrayOf(
      unit_lowerer.Unit().types, builtins.int_type, indices.size());
  return block.exprs.Add(
      mir::Expr{
          .data = mir::CompositeExpr{.parts = std::move(ids)},
          .type = indices_type});
}

auto BuildStringLiteral(
    UnitLowerer& unit_lowerer, mir::Block& block, const std::string& s)
    -> mir::ExprId {
  return block.exprs.Add(
      mir::MakeStringLiteral(unit_lowerer.Unit().builtins.string, s));
}

// A route runs from its origin (the referrer's `self`) to the referenced leaf,
// and each step reaches through whatever the step before it landed on. What
// that is decides how the next one may reach: a scope this artifact lowers
// admits a typed member access onto anything it declares, an object another
// unit defines is a typed pointer whose names resolve against the signature
// that unit published, and the base every object on the tree is one of admits
// only a name the runtime answers.
struct OwnScope {
  const StructuralScopeLowerer* scope;
};
struct ExternalObject {};
struct ScopeBase {};

using ReceiverTarget = std::variant<OwnScope, ExternalObject, ScopeBase>;

struct RouteReceiver {
  mir::ExprId expr{};
  ReceiverTarget target;
};

// The scope a step or leaf naming one of this artifact's own declarations is
// standing on. Reaching one is only possible while the route is still inside
// the artifact, so a receiver that has left it is a route built against a
// different design than the one it reached.
auto OwnScopeOf(const RouteReceiver& receiver, std::string_view site)
    -> const StructuralScopeLowerer& {
  const auto* own = std::get_if<OwnScope>(&receiver.target);
  if (own == nullptr) {
    throw InternalError(
        std::format(
            "{}: the route names a declaration of a scope this artifact "
            "lowers, so it cannot have left the artifact before reaching it",
            site));
  }
  return *own->scope;
}

// Reaches a child by name+indices as an opaque `Scope*` -- the realization of
// an opaque step (one crossing into another unit's body).
auto StepToChildByName(
    UnitLowerer& unit_lowerer, mir::Block& block, mir::ExprId receiver,
    const std::string& name, std::span<const std::uint32_t> indices)
    -> RouteReceiver {
  const mir::ExprId step = block.exprs.Add(
      mir::Expr{
          .data =
              mir::CallExpr{
                  .callee =
                      mir::Direct{
                          .target = support::BuiltinFn::kFindChild,
                          .receiver = receiver},
                  .arguments =
                      {BuildStringLiteral(unit_lowerer, block, name),
                       BuildIndicesLiteral(unit_lowerer, block, indices)}},
          .type = unit_lowerer.Unit().builtins.scope_ptr});
  return RouteReceiver{.expr = step, .target = ScopeBase{}};
}

// Establishes the route's starting receiver from the head. An in-unit head
// climbs `hops` typed parent edges to an ancestor scope of this unit, which
// keeps the receiver typed. `$root` and the visible-child climb name a scope
// this unit does not declare, so both are opaque runtime-SDK reaches.
auto BuildRouteAnchor(
    const StructuralScopeLowerer& lowerer, const WalkFrame& frame,
    const hir::RouteHead& head) -> RouteReceiver {
  UnitLowerer& unit_lowerer = lowerer.Owner();
  auto& unit = unit_lowerer.Unit();
  mir::Block& block = *frame.current_block;
  const mir::TypeId scope_ptr_type = unit.builtins.scope_ptr;

  if (const auto* ih = std::get_if<hir::InUnitHead>(&head)) {
    return RouteReceiver{
        .expr = BuildEnclosingScopeReceiver(
            frame, unit, mir::EnclosingHops{.value = ih->hops.value}),
        .target = OwnScope{&lowerer.EnclosingScopeAtHops(ih->hops)}};
  }

  const mir::ExprId self_ref = block.exprs.Add(
      MakeSelfRefExpr(frame, frame.current_class->self_pointer_type));

  if (std::holds_alternative<hir::RootHead>(head)) {
    const mir::ExprId root = block.exprs.Add(
        mir::Expr{
            .data =
                mir::CallExpr{
                    .callee =
                        mir::Direct{
                            .target = support::BuiltinFn::kResolveRoot,
                            .receiver = self_ref},
                    .arguments = {}},
            .type = scope_ptr_type});
    return RouteReceiver{.expr = root, .target = ScopeBase{}};
  }

  // The visible-child climb walks the parent chain by name (LRM 23.8).
  const auto& vc = std::get<hir::VisibleChildHead>(head);
  const mir::ExprId matched = block.exprs.Add(
      mir::Expr{
          .data =
              mir::CallExpr{
                  .callee =
                      mir::Direct{
                          .target = support::BuiltinFn::kResolveVisibleChild,
                          .receiver = self_ref},
                  .arguments =
                      {BuildStringLiteral(unit_lowerer, block, vc.head_name),
                       BuildIndicesLiteral(
                           unit_lowerer, block, vc.head_indices)}},
          .type = scope_ptr_type});
  return RouteReceiver{.expr = matched, .target = ScopeBase{}};
}

// Descends one step into a child the receiver's scope declares: the typed
// member access that projects the parent's handle on that child, then one index
// per coordinate the step names. The coordinates are settled during
// elaboration, and each takes one dimension off what the member holds, so a
// child with no declared dimensions indexes nothing and is the same step with
// no coordinates. A child whose body is another compilation unit is still
// reached by a typed pointer, but what it declares is that unit's to state, so
// the route stops resolving names against a scope of this one.
auto StepToOwnedChild(
    UnitLowerer& unit_lowerer, mir::Block& block, const RouteReceiver& receiver,
    const hir::OwnedChildStep& step) -> RouteReceiver {
  const StructuralScopeLowerer& scope =
      OwnScopeOf(receiver, "StepToOwnedChild");
  const OwnedChildAnchor anchor =
      scope.TranslateOwnedChild(hir::StructuralHops{0}, step.child);
  const mir::ClassId receiver_class = scope.ClassId();
  const mir::TypeId reached = unit_lowerer.GetClassShape(receiver_class)
                                  .fields.Get(anchor.borrowed_handle)
                                  .type;
  const ReachedObject object = IndexCoordinates(
      unit_lowerer, block,
      ReachedObject{
          .expr = block.exprs.Add(
              mir::MakeFieldAccessExpr(
                  receiver.expr,
                  mir::ClassFieldTarget{
                      .owner = receiver_class, .slot = anchor.borrowed_handle},
                  reached)),
          .type = reached},
      CoordinatesAt(anchor, step.indices));
  // A child whose body is another compilation unit leaves the artifact here;
  // one this artifact lowers keeps the route inside it.
  return RouteReceiver{
      .expr = object.expr,
      .target = anchor.target_scope == nullptr
                    ? ReceiverTarget{ExternalObject{}}
                    : ReceiverTarget{OwnScope{anchor.target_scope}}};
}

// Descends one step through an interface port of the receiver's scope: the
// typed member access that projects the borrowed reference the parent bound
// there (LRM 25.3), then one index per coordinate the step names, since a port
// carrying a range is one member standing for every instance bound to it.
// Everything past the step belongs to the unit the port names, which this
// artifact does not lower, so the receiver stops being one of its own scopes --
// the same place an owned child whose body is another unit leaves it.
auto StepThroughInterfacePort(
    UnitLowerer& unit_lowerer, mir::Block& block, const RouteReceiver& receiver,
    const hir::InterfacePortStep& step) -> RouteReceiver {
  const StructuralScopeLowerer& scope =
      OwnScopeOf(receiver, "StepThroughInterfacePort");
  const mir::ClassId receiver_class = scope.ClassId();
  const mir::FieldId field =
      scope.TranslateInterfacePort(hir::StructuralHops{0}, step.port);
  const mir::TypeId reached =
      unit_lowerer.GetClassShape(receiver_class).fields.Get(field).type;
  const ReachedObject object = IndexCoordinates(
      unit_lowerer, block,
      ReachedObject{
          .expr = block.exprs.Add(
              mir::MakeFieldAccessExpr(
                  receiver.expr,
                  mir::ClassFieldTarget{.owner = receiver_class, .slot = field},
                  reached)),
          .type = reached},
      step.indices);
  return RouteReceiver{.expr = object.expr, .target = ExternalObject{}};
}

// A member another unit published, reached the way that unit offers it: the
// behavior of the promise that answers with the storage behind the member,
// dispatched on the object the route has descended to. Which behavior it is is
// counted out of the order the promise published its members in, and the
// storage it answers with is the only thing this side learns about the object.
auto ReadPublishedMember(
    mir::CompilationUnit& unit, mir::Block& block,
    const RouteReceiver& receiver, hir::PublishedMemberId member)
    -> mir::ExprId {
  const mir::TypeId pointee =
      unit.types.Get(block.exprs.Get(receiver.expr).type)
          .Get<mir::PointerType>()
          .pointee;
  const mir::ExternalUnitObject& promised = unit.external_unit_objects.Get(
      unit.types.Get(pointee).Get<mir::ExternalUnitObjectType>().object);
  const mir::FieldId slot = UnitLowerer::TranslatePublishedMember(member);
  const mir::PromisedField& field = promised.fields.Get(slot);
  return block.exprs.Add(
      mir::Expr{
          .data =
              mir::CallExpr{
                  .callee =
                      mir::Virtual{
                          .receiver = receiver.expr,
                          .slot =
                              mir::ExternalVirtualSlot{
                                  .unit_name = promised.unit_name,
                                  .class_name = promised.class_name,
                                  .ordinal = mir::BehaviorOrdinal{slot.value}}},
                  .arguments = {}},
          .type = unit.types.Intern(
              mir::Type{mir::PointerType{
                  .pointee = field.type,
                  .ownership = mir::PointerOwnership::kBorrowed}})});
}

// Descends one step onto a member another unit published whose type makes it an
// object of a third unit (LRM 25.3, 25.10): the member access at the position
// that unit's signature gave it, then one index per coordinate the step names.
auto StepToSignatureMember(
    UnitLowerer& unit_lowerer, mir::Block& block, const RouteReceiver& receiver,
    const hir::SignatureMemberStep& step) -> RouteReceiver {
  const mir::ExprId storage =
      ReadPublishedMember(unit_lowerer.Unit(), block, receiver, step.member);
  const mir::TypeId reached = unit_lowerer.Unit()
                                  .types.Get(block.exprs.Get(storage).type)
                                  .Get<mir::PointerType>()
                                  .pointee;
  const ReachedObject object = IndexCoordinates(
      unit_lowerer, block,
      ReachedObject{
          .expr = block.exprs.Add(
              mir::Expr{
                  .data = mir::DerefExpr{.pointer = storage}, .type = reached}),
          .type = reached},
      step.indices);
  return RouteReceiver{.expr = object.expr, .target = ExternalObject{}};
}

// Projects the borrowed-pointer value the slot takes out of a typed receiver:
// the field access, addressed. Everything a scope's bodies declare with a
// lifetime longer than an activation is a field of the scope's own class, so
// the receiver is already standing where the field is.
auto AddressTypedLeaf(
    UnitLowerer& unit_lowerer, mir::Block& block, const RouteReceiver& receiver,
    mir::ClassId owner_class, mir::FieldId field, mir::TypeId slot_type)
    -> mir::ExprId {
  const mir::TypeId field_type =
      unit_lowerer.GetClassShape(owner_class).fields.Get(field).type;
  const mir::ExprId access = block.exprs.Add(
      mir::MakeFieldAccessExpr(
          receiver.expr,
          mir::ClassFieldTarget{.owner = owner_class, .slot = field},
          field_type));
  return block.exprs.Add(
      mir::Expr{
          .data = mir::AddressOfExpr{.operand = access}, .type = slot_type});
}

// Materializes where a route landed as the value its use reaches it by, one
// form per use. Data is the addressed member access when this artifact
// declares it or the target unit published it, or a cast of the untyped
// address a by-name signal query answers with when it reaches past a
// signature, where nothing was promised for this one to compile against.
auto MaterializeLeaf(
    UnitLowerer& unit_lowerer, mir::Block& block, const RouteReceiver& receiver,
    const hir::DataLeaf& leaf) -> mir::ExprId {
  const mir::TypeId pointer_type = PointerTypeOf(unit_lowerer, leaf);
  return std::visit(
      Overloaded{
          [&](const hir::StructuralDataObjectLeaf& l) {
            const StructuralScopeLowerer& scope =
                OwnScopeOf(receiver, "MaterializeLeaf");
            return AddressTypedLeaf(
                unit_lowerer, block, receiver, scope.ClassId(),
                scope.TranslateStructuralDataObject(
                    hir::StructuralHops{0}, l.object),
                pointer_type);
          },
          [&](const hir::ProceduralStaticLeaf& l) {
            const StructuralScopeLowerer& scope =
                OwnScopeOf(receiver, "MaterializeLeaf");
            return AddressTypedLeaf(
                unit_lowerer, block, receiver, scope.ClassId(),
                scope.ProceduralStaticField(l.body, l.var), pointer_type);
          },
          // A published member is reached through the target unit's own
          // object, whose pointer the step before it produced.
          [&](const hir::SignatureMemberLeaf& l) {
            return ReadPublishedMember(
                unit_lowerer.Unit(), block, receiver, l.member);
          },
          [&](const hir::OpaqueLeaf& l) {
            const mir::ExprId raw = block.exprs.Add(
                mir::Expr{
                    .data =
                        mir::CallExpr{
                            .callee =
                                mir::Direct{
                                    .target = support::BuiltinFn::kFindSignal,
                                    .receiver = receiver.expr},
                            .arguments = {BuildStringLiteral(
                                unit_lowerer, block, l.name)}},
                    .type = mir::ErasedPointer(unit_lowerer.Unit().types)});
            return block.exprs.Add(
                mir::Expr{
                    .data = mir::CastExpr{.operand = raw},
                    .type = pointer_type});
          }},
      leaf);
}

// An object is the one the steps landed on, which the last step already
// produced as a borrowed pointer. What the use holds that object as is a
// separate fact -- a receiver a call passes takes the scope every body is
// entered through, where a member read takes the object's own type -- so the
// value states the pointer's type rather than staying whatever the step
// happened to reach.
auto MaterializeLeaf(
    UnitLowerer& unit_lowerer, mir::Block& block, const RouteReceiver& receiver,
    const hir::ScopeLeaf& leaf) -> mir::ExprId {
  return block.exprs.Add(
      mir::Expr{
          .data = mir::CastExpr{.operand = receiver.expr},
          .type = PointerTypeOf(unit_lowerer, leaf)});
}

// A callable reached past a signature is answered the way a cell is, from the
// scope's own record of what it declares -- what differs is only which of the
// two namespaces the name is looked up in and that the answer is already a
// code address rather than something to cast.
auto MaterializeLeaf(
    UnitLowerer& unit_lowerer, mir::Block& block, const RouteReceiver& receiver,
    const hir::OpaqueCallableLeaf& leaf) -> mir::ExprId {
  return block.exprs.Add(
      mir::Expr{
          .data =
              mir::CallExpr{
                  .callee =
                      mir::Direct{
                          .target = support::BuiltinFn::kFindSubroutine,
                          .receiver = receiver.expr},
                  .arguments = {BuildStringLiteral(
                      unit_lowerer, block, leaf.name)}},
          .type = mir::ErasedFunction(unit_lowerer.Unit().types)});
}

// What a `disable` terminates: the target's own cell where this artifact lays
// out the scope declaring it, and otherwise what the scope the steps reached
// answers, unnamed (LRM 9.6.2, 23.9).
auto MaterializeLeaf(
    UnitLowerer& unit_lowerer, mir::Block& block, const RouteReceiver& receiver,
    const hir::DisableLeaf& leaf) -> mir::ExprId {
  const mir::TypeId pointer_type =
      DisableTargetPointerType(unit_lowerer.Unit().types);
  return std::visit(
      Overloaded{
          [&](const hir::DisableTargetLeaf& l) {
            const StructuralScopeLowerer& scope =
                OwnScopeOf(receiver, "MaterializeLeaf");
            return AddressTypedLeaf(
                unit_lowerer, block, receiver, scope.ClassId(),
                scope.DisableTargetField(l.scope), pointer_type);
          },
          [&](const hir::OpaqueDisableTargetLeaf&) {
            return block.exprs.Add(
                mir::Expr{
                    .data =
                        mir::CallExpr{
                            .callee =
                                mir::Direct{
                                    .target =
                                        support::BuiltinFn::kFindDisableTarget,
                                    .receiver = receiver.expr},
                            .arguments = {}},
                    .type = pointer_type});
          }},
      leaf);
}

// Where a member name lands on a class the steps' scope declares: that scope
// answers which class `class_name` means, and the class answers `name` the way
// `ask` counts. Which class the walk lands on belongs to the instance, so one
// artifact serving several instances asks each of them.
auto AskClassMember(
    UnitLowerer& unit_lowerer, mir::Block& block, const RouteReceiver& receiver,
    const hir::ClassMemberName& member, support::BuiltinFn ask,
    mir::TypeId answer_type) -> mir::ExprId {
  const mir::TypeId class_type = unit_lowerer.Unit().types.Intern(
      mir::Type{mir::PointerType{
          .pointee = unit_lowerer.Unit().builtins.void_type,
          .ownership = mir::PointerOwnership::kBorrowed}});
  const mir::ExprId cls = block.exprs.Add(
      mir::Expr{
          .data =
              mir::CallExpr{
                  .callee =
                      mir::Direct{
                          .target = support::BuiltinFn::kFindClass,
                          .receiver = receiver.expr},
                  .arguments = {BuildStringLiteral(
                      unit_lowerer, block, member.class_name)}},
          .type = class_type});
  return block.exprs.Add(
      mir::Expr{
          .data =
              mir::CallExpr{
                  .callee = mir::Direct{.target = ask},
                  .arguments =
                      {cls,
                       BuildStringLiteral(unit_lowerer, block, member.name)}},
          .type = answer_type});
}

auto MaterializeLeaf(
    UnitLowerer& unit_lowerer, mir::Block& block, const RouteReceiver& receiver,
    const hir::PropertyCoordinateLeaf& leaf) -> mir::ExprId {
  return AskClassMember(
      unit_lowerer, block, receiver, leaf.member,
      support::BuiltinFn::kClassFindProperty,
      CoordinateType(
          unit_lowerer.Unit().types,
          mir::RuntimeLibraryKind::kPropertyCoordinate));
}

auto MaterializeLeaf(
    UnitLowerer& unit_lowerer, mir::Block& block, const RouteReceiver& receiver,
    const hir::BehaviorCoordinateLeaf& leaf) -> mir::ExprId {
  return AskClassMember(
      unit_lowerer, block, receiver, leaf.member,
      support::BuiltinFn::kClassFindBehavior,
      CoordinateType(
          unit_lowerer.Unit().types,
          mir::RuntimeLibraryKind::kBehaviorCoordinate));
}

auto MaterializeLeaf(
    UnitLowerer& unit_lowerer, mir::Block& block, const RouteReceiver& receiver,
    const hir::BehaviorBodyLeaf& leaf) -> mir::ExprId {
  return AskClassMember(
      unit_lowerer, block, receiver, leaf.member,
      support::BuiltinFn::kClassFindBehaviorBody,
      mir::ErasedFunction(unit_lowerer.Unit().types));
}

// Walks from the head to whatever the last step lands on, which is a scope of
// the elaborated tree. What the walk is for -- reaching something the scope
// holds, or asking the scope a name -- is the caller's, so the walk ends here.
auto BuildRouteWalk(
    const StructuralScopeLowerer& lowerer, const WalkFrame& frame,
    const hir::RouteHead& head, std::span<const hir::PathStep> steps)
    -> RouteReceiver {
  UnitLowerer& unit_lowerer = lowerer.Owner();
  mir::Block& block = *frame.current_block;
  RouteReceiver receiver = BuildRouteAnchor(lowerer, frame, head);
  for (const auto& step : steps) {
    receiver = std::visit(
        Overloaded{
            [&](const hir::OwnedChildStep& owned) {
              return StepToOwnedChild(unit_lowerer, block, receiver, owned);
            },
            [&](const hir::InterfacePortStep& port) {
              return StepThroughInterfacePort(
                  unit_lowerer, block, receiver, port);
            },
            [&](const hir::SignatureMemberStep& member) {
              return StepToSignatureMember(
                  unit_lowerer, block, receiver, member);
            },
            [&](const hir::OpaqueStep& opaque) {
              return StepToChildByName(
                  unit_lowerer, block, receiver.expr, opaque.name,
                  opaque.indices);
            }},
        step);
  }
  return receiver;
}

// Composes what a route ends at: the walk above, then the leaf materialized
// where it landed. Appends to the frame's block and returns the value, for
// whoever uses it there -- a slot being filled, or an access.
template <typename Leaf>
auto BuildRouteValue(
    const StructuralScopeLowerer& lowerer, const WalkFrame& frame,
    const hir::Route<Leaf>& route) -> mir::ExprId {
  const RouteReceiver receiver =
      BuildRouteWalk(lowerer, frame, route.head, route.steps);
  return MaterializeLeaf(
      lowerer.Owner(), *frame.current_block, receiver, route.leaf);
}

// Stores `value` into the scope's own `slot`. Every slot a scope settles is
// filled this way, once, and read directly at every use afterwards.
void FillScopeSlot(
    const WalkFrame& frame, mir::FieldId slot, mir::ExprId value) {
  mir::Class& mir_class = *frame.current_class;
  mir::Block& block = *frame.current_block;
  const mir::TypeId slot_type = mir_class.fields.Get(slot).type;
  const mir::ExprId self =
      block.exprs.Add(MakeSelfRefExpr(frame, mir_class.self_pointer_type));
  const mir::ExprId target = block.exprs.Add(
      mir::Expr{
          .data =
              mir::FieldAccessExpr{
                  .receiver = self,
                  .field =
                      mir::ClassFieldTarget{
                          .owner = frame.current_class_id, .slot = slot}},
          .type = slot_type});
  const mir::ExprId assign = block.exprs.Add(
      mir::Expr{
          .data = mir::AssignExpr{.target = target, .value = value},
          .type = slot_type});
  block.AppendStmt(mir::ExprStmt{.expr = assign});
}

// Walks every stored route of one use where the object tree is whole, so what
// its slot holds is what the route ends at rather than anything it had to walk
// to reach it. A climbed route has no slot to fill.
template <typename Leaf, typename Id>
void InstallStoredRoutes(
    const StructuralScopeLowerer& lowerer, const WalkFrame& resolve_frame,
    const base::Arena<hir::Route<Leaf>, Id>& routes) {
  for (const Id hir_id : routes.Ids()) {
    std::visit(
        Overloaded{
            [](const ClimbedRoute&) {},
            [&](const StoredRoute& stored) {
              FillScopeSlot(
                  resolve_frame, stored.slot,
                  BuildRouteValue(lowerer, resolve_frame, routes.Get(hir_id)));
            }},
        lowerer.ReachOf(hir_id));
  }
}

// Fills every slot the scope's walks keep, once the object tree is whole.
void InstallScopeRoutes(
    const StructuralScopeLowerer& lowerer, const WalkFrame& resolve_frame) {
  const hir::ScopeRoutes& routes = lowerer.HirScope().routes;
  InstallStoredRoutes(lowerer, resolve_frame, routes.values);
  InstallStoredRoutes(lowerer, resolve_frame, routes.objects);
  InstallStoredRoutes(lowerer, resolve_frame, routes.callables);
  InstallStoredRoutes(lowerer, resolve_frame, routes.disable_targets);
  InstallStoredRoutes(lowerer, resolve_frame, routes.property_coordinates);
  InstallStoredRoutes(lowerer, resolve_frame, routes.behavior_coordinates);
  InstallStoredRoutes(lowerer, resolve_frame, routes.behavior_bodies);
}

// Appends one process activation registration to the scope's `activate` body:
// invokes `body` over the activate frame's `self` to produce the coroutine,
// then registers it for the scope's startup (`is_final == false`) or shutdown
// (`is_final == true`) lifecycle (LRM 9.2). Startup and shutdown are distinct
// registration callees, not one tagged call.
//
// The registration also names the unit instance the process belongs to, which
// is where LRM 18.14.1 keeps the seeds a static process starts from. That
// instance is the scope the artifact's own class tree is rooted at, a fixed
// number of steps out from wherever the process is declared, so the call
// reaches it by typed navigation over a distance this walk already knows.
void AppendProcessRegistration(
    UnitLowerer& unit_lowerer, const WalkFrame& activate_frame,
    mir::CallableId body, bool is_final) {
  mir::Block& block = *activate_frame.current_block;
  const mir::TypeId self_ptr_type =
      activate_frame.current_class->self_pointer_type;
  const mir::ExprId body_self =
      block.exprs.Add(MakeSelfRefExpr(activate_frame, self_ptr_type));
  const mir::ExprId body_call = block.exprs.Add(
      mir::Expr{
          .data =
              mir::CallExpr{
                  .callee =
                      mir::Direct{
                          .target =
                              mir::CallableTarget{
                                  .owner = activate_frame.current_class_id,
                                  .slot = body},
                          .receiver = body_self},
                  .arguments = {}},
          .type = unit_lowerer.Unit().builtins.coroutine_void});
  const mir::ExprId reg_self =
      block.exprs.Add(MakeSelfRefExpr(activate_frame, self_ptr_type));
  const mir::ExprId unit_instance = BuildEnclosingScopeReceiver(
      activate_frame, unit_lowerer.Unit(), activate_frame.HopsToUnitRoot());
  const mir::ExprId reg_call = block.exprs.Add(
      mir::Expr{
          .data =
              mir::CallExpr{
                  .callee =
                      mir::Direct{
                          .target = is_final
                                        ? support::BuiltinFn::kRegisterFinal
                                        : support::BuiltinFn::kRegisterInitial},
                  .arguments = {reg_self, unit_instance, body_call}},
          .type = unit_lowerer.Unit().builtins.void_type});
  block.AppendStmt(mir::ExprStmt{.expr = reg_call});
}

// A variable declaration assignment runs before any procedure starts (LRM
// 10.5), so a randomization call inside one draws from the initialization RNG
// of the instance the declaration sits in rather than from any process's (LRM
// 18.14.1). That instance is the scope itself unless this one is a generate
// scope, which keeps no seeds of its own, so it is reached over the distance
// this walk already knows.
void WrapInScopeStaticInitExtent(
    const UnitLowerer& unit_lowerer, const WalkFrame& init_frame,
    mir::CallableCode& code) {
  mir::Block extent;
  AppendRuntimeEffectStmt(
      unit_lowerer, extent, support::BuiltinFn::kEnterScopeStaticInit,
      {BuildEnclosingScopeReceiver(
          init_frame.WithBlock(&extent), unit_lowerer.Unit(),
          init_frame.HopsToUnitRoot())});

  mir::Block cleanup;
  AppendRuntimeEffectStmt(
      unit_lowerer, cleanup, support::BuiltinFn::kLeaveStaticInit, {});

  extent.AppendFinally(std::move(code.Body()), std::move(cleanup));
  code.Body() = std::move(extent);
}

// Composes the value a port member takes from the handles its connection
// supplies: the handle itself where the member stands for one object, and the
// sequence of what the dimension below holds where it stands for several. The
// shape is read off the child's own declared type, so how many the parent
// supplies per dimension is the child's promise rather than a second count.
// `next` walks the handles in the order the port's coordinates count them.
auto ComposeBoundObjects(
    UnitLowerer& unit_lowerer, mir::Block& block, hir::TypeId member_type,
    std::span<const mir::ExprId> handles, std::size_t& next) -> mir::ExprId {
  const auto* array =
      unit_lowerer.Hir().types.Get(member_type).As<hir::UnpackedArrayType>();
  if (array == nullptr) {
    if (next >= handles.size()) {
      throw InternalError(
          "ComposeBoundObjects: the connection supplies one instance per "
          "object the port stands for, which is checked where it is recorded");
    }
    return handles[next++];
  }
  const std::uint64_t count = array->dim.ElementCount();
  std::vector<mir::ExprId> elements;
  elements.reserve(count);
  for (std::uint64_t i = 0; i < count; ++i) {
    elements.push_back(ComposeBoundObjects(
        unit_lowerer, block, array->element_type, handles, next));
  }
  return block.exprs.Add(BuildSequenceConstructionCall(
      unit_lowerer.Unit(), block,
      unit_lowerer.MemberCellType(
          unit_lowerer.TranslateType(member_type),
          hir::BorrowedObjectStorage{}),
      std::move(elements)));
}

// Binds a child's interface port to the interface instances the connection
// names (LRM 25.3), in the resolve phase where the object tree is complete: the
// route to the child's member yields a pointer to the slot the child holds, the
// routes to the interfaces yield the objects, and one store fills the one with
// the other. The child owns no storage on either side, so nothing else happens
// here -- the same shape a `ref` port's alias bind takes, over objects rather
// than a cell.
void InstallInterfacePortConnection(
    StructuralScopeLowerer& lowerer, const WalkFrame& resolve_frame,
    const hir::InterfacePortConnection& conn) {
  UnitLowerer& unit_lowerer = lowerer.Owner();
  mir::Block& block = *resolve_frame.current_block;
  // What the member holds is a handle on each object it stands for, which is
  // the same function of its declared type the declaring unit built it from.
  const hir::DataCell port = hir::CellOf(conn.endpoint.leaf);
  const mir::TypeId member_type = unit_lowerer.MemberCellType(
      unit_lowerer.TranslateType(port.type), port.storage);
  const mir::ExprId nav =
      BuildRouteValue(lowerer, resolve_frame, conn.endpoint);
  const mir::ExprId target = block.exprs.Add(
      mir::Expr{.data = mir::DerefExpr{.pointer = nav}, .type = member_type});

  std::vector<mir::ExprId> handles;
  handles.reserve(conn.peers.size());
  for (const hir::ObjectRoute& peer : conn.peers) {
    handles.push_back(BuildRouteValue(lowerer, resolve_frame, peer));
  }
  std::size_t next = 0;
  const mir::ExprId value =
      ComposeBoundObjects(unit_lowerer, block, port.type, handles, next);
  block.AppendStmt(
      mir::ExprStmt{
          .expr = block.exprs.Add(
              mir::Expr{
                  .data = mir::AssignExpr{.target = target, .value = value},
                  .type = member_type})});
}

// Realizes the runs of nets this scope's constructs place in one resolution
// (LRM 23.3.3.7, 10.11). Each is one statement in the resolve body, beside the
// `ref` port's bind: no driver is attached and no process is registered,
// because what a join states is which contributions resolve together and not an
// edge anything travels along. Both nets are named whole and the run says which
// of their positions the connection reached, so what a backend meets is one
// call with every operand stated.
auto InstallNetJoins(StructuralScopeLowerer& lowerer, WalkFrame resolve_frame)
    -> diag::Result<void> {
  UnitLowerer& unit_lowerer = lowerer.Owner();
  mir::Block& block = *resolve_frame.current_block;
  const hir::StructuralScope& hir_scope = lowerer.HirScope();
  for (const hir::NetJoin& join : hir_scope.net_joins) {
    auto here_or =
        lowerer.LowerLhsExpr(hir_scope.exprs.Get(join.here), resolve_frame);
    if (!here_or) return std::unexpected(std::move(here_or.error()));
    auto there_or =
        lowerer.LowerLhsExpr(hir_scope.exprs.Get(join.there), resolve_frame);
    if (!there_or) return std::unexpected(std::move(there_or.error()));
    if (!here_or->descent.empty() || !there_or->descent.empty()) {
      throw InternalError(
          "InstallNetJoins: a join names a whole net on each side and says "
          "which of its positions the run covers, decided where it is read");
    }
    const mir::ExprId there = there_or->owner;
    const mir::TypeId net_ptr_type = unit_lowerer.Unit().types.Intern(
        mir::Type{mir::PointerType{
            .pointee = block.exprs.Get(there).type,
            .ownership = mir::PointerOwnership::kBorrowed}});
    block.AppendStmt(
        mir::ExprStmt{
            .expr = block.exprs.Add(
                mir::MakeNetJoinCallExpr(
                    here_or->owner,
                    block.exprs.Add(
                        mir::MakeAddressOfExpr(there, net_ptr_type)),
                    BuildIntLiteral(
                        unit_lowerer.Unit(), block, join.here_offset),
                    BuildIntLiteral(
                        unit_lowerer.Unit(), block, join.there_offset),
                    BuildIntLiteral(unit_lowerer.Unit(), block, join.width),
                    unit_lowerer.Unit().builtins.void_type))});
  }
  return {};
}

// Realizes each connection that carries data across the boundary (LRM 23.3.3).
// An input or output port is the implied continuous assignment between the two
// cells, materialized as the same synthesized process a scope-level `assign`
// produces, registered as a process; when the driven side is a net the edge
// attaches a driver rather than writing the cell. A `ref` port carries no edge
// and is emitted into the resolve block instead, binding the child's reference
// member -- navigated by name from the owned child -- to the connected
// variable's cell: one statement, with no second cell and no continuous
// assignment.
//
// A bidirectional port carries no data in either direction and states no
// direction at all, so it is not one of these; what it states is which
// positions resolve together, which is a join.
auto InstallPortConnections(
    StructuralScopeLowerer& lowerer, WalkFrame frame, WalkFrame resolve_frame,
    WalkFrame init_frame, WalkFrame activate_frame) -> diag::Result<void> {
  mir::Class& mir_class = *frame.current_class;
  mir::Block& resolve_block = *resolve_frame.current_block;
  const hir::StructuralScope& hir_scope = lowerer.HirScope();
  UnitLowerer& unit_lowerer = lowerer.Owner();
  for (const hir::PortConnectionId id : hir_scope.port_connections.Ids()) {
    const hir::PortConnection& pc = hir_scope.port_connections.Get(id);
    if (const auto* iface =
            std::get_if<hir::InterfacePortConnection>(&pc.kind)) {
      InstallInterfacePortConnection(lowerer, resolve_frame, *iface);
      continue;
    }
    const auto& data = std::get<hir::DataPortConnection>(pc.kind);
    // A `ref` port binds once and is done, and a bidirectional one joins once;
    // the two value directions share the reactive edge built below and differ
    // only in which end of it drives (LRM 23.3.3).
    switch (data.direction) {
      case hir::PortDirection::kInput:
      case hir::PortDirection::kOutput:
        break;
      case hir::PortDirection::kInOut:
        throw InternalError(
            "InstallPortConnections: a bidirectional connection carries no "
            "data across the boundary and is recorded as the join it is, so "
            "it never reaches the connection switch");
      case hir::PortDirection::kRef: {
        // A `ref` port reaches the child's reference member by the same route
        // navigation any value reference uses, then binds it to the peer's cell
        // through the one canonical reference-store primitive. It holds no
        // persistent slot -- a `ref` needs no simulation-time reach, so the
        // member is reached once here in the resolve phase (LRM 23.3.3.2).
        const auto& route = std::get<hir::ValueRoute>(data.endpoint);
        if (!std::holds_alternative<hir::InUnitHead>(route.head)) {
          throw InternalError(
              "InstallPortConnections: a ref port reaches its child downward");
        }
        const hir::DataCell member = hir::CellOf(route.leaf);
        const mir::TypeId ref_type = unit_lowerer.MemberCellType(
            unit_lowerer.TranslateType(member.type), member.storage);
        const mir::ExprId nav = BuildRouteValue(lowerer, resolve_frame, route);
        const mir::ExprId target = resolve_block.exprs.Add(
            mir::Expr{
                .data = mir::DerefExpr{.pointer = nav}, .type = ref_type});

        auto peer_or =
            lowerer.LowerLhsExpr(hir_scope.exprs.Get(data.peer), resolve_frame);
        if (!peer_or) return std::unexpected(std::move(peer_or.error()));
        const mir::ExprId peer_cell = peer_or->owner;

        const mir::ExprId bind = BindReferenceSlot(
            unit_lowerer.Unit(), resolve_block, target, peer_cell);
        resolve_block.AppendStmt(mir::ExprStmt{.expr = bind});
        continue;
      }
      // A unit publishes every direction the language admits, and AST-to-HIR
      // refuses this one, so a recorded connection never carries it.
      case hir::PortDirection::kConstRef:
        throw InternalError(
            "InstallPortConnections: a refused port direction reached the "
            "connection switch");
    }
    const auto& cell = std::get<hir::PortCellEndpoint>(data.endpoint);
    const bool is_input = data.direction == hir::PortDirection::kInput;
    // A port connection is a reactive edge: the source is read, the sink is
    // driven. An input port's source is the parent expression and its sink is
    // the child cell; an output port's source is the child cell and its sink
    // is the parent target. The edge is the same continuous assignment either
    // way, written with no strength of its own and so driving at strong like
    // any other (LRM 23.3.3, 10.3.1); the sink's own MIR type -- resolved-net
    // cell or observable cell -- picks the write protocol.
    const hir::ContinuousAssign assign{
        .span = pc.span,
        .lhs = is_input ? cell.cell : data.peer,
        .rhs = is_input ? data.peer : cell.cell,
        .strength = support::StrengthLevel::kStrong,
        .sensitivity_list = data.sensitivity};
    auto method_or = LowerContinuousAssign(
        lowerer, frame, resolve_frame, init_frame, assign);
    if (!method_or) return std::unexpected(std::move(method_or.error()));
    const mir::CallableId body = mir_class.callables.Add(std::move(*method_or));
    AppendProcessRegistration(unit_lowerer, activate_frame, body, false);
  }
  return {};
}

void ValidateOwnedChildConstruction(
    const mir::Class& owner_class, mir::ClassId child_scope_id) {
  if (std::ranges::find(owner_class.contained, child_scope_id) ==
      owner_class.contained.end()) {
    throw InternalError(
        "owned-child construction: child scope is not a direct child of the "
        "enclosing class");
  }
}

// Lowers an owned-child construction site to the MIR call shape
// `AddOwnedChild(parent, make_unique<Child>(parent, HierarchySegment{label,
// indices}, ctor_args...))`: the child instance is built carrying
// its complete hierarchy identity, then handed to the parent to own. The
// runtime tree owns the child and answers a by-name descent with it; what
// comes back is a borrowed pointer, which is what a route navigates through
// and what the caller stores. `runtime_label` is the
// SV-visible identifier; an anonymous scope gets an empty label, which the
// runtime treats as non-addressable so a peer by-name lookup walks past it to
// the addressable descendants underneath. `arm_frame` must point at the block
// where the stmts land and carry the constructor's bindings so a `self` read
// resolves to the receiver binding.
//
// Where the child hangs in the runtime tree and who keeps the borrowed handle
// to it are separate: `runtime_parent_handle` names an object this one already
// holds a handle to, and the handle to the new child lands in `handle_field` of
// this class regardless. So one object can build a whole nested tree and still
// reach every node of it in one step. Absent means the child hangs directly
// under this object.
auto BuildOwnedChildHandle(
    UnitLowerer& unit_lowerer, const WalkFrame& arm_frame,
    std::optional<mir::FieldId> runtime_parent_handle,
    const std::string& runtime_label, mir::ClassId child_scope_id,
    std::optional<mir::ExprId> array_index, mir::TypeId handle_type,
    std::optional<mir::ExprId> construction_value) -> mir::ExprId {
  mir::Block& arm_block = *arm_frame.current_block;
  const mir::Class& owner_class = *arm_frame.current_class;
  ValidateOwnedChildConstruction(owner_class, child_scope_id);

  const auto& builtins = unit_lowerer.Unit().builtins;
  const mir::TypeId self_ptr_type = owner_class.self_pointer_type;
  const mir::TypeId child_ptr_type =
      MakeUniqueObjectPointer(unit_lowerer, child_scope_id);

  const auto string_literal = [&](const std::string& s) -> mir::ExprId {
    return arm_block.exprs.Add(
        mir::Expr{
            .data = mir::StringLiteral{.value = s}, .type = builtins.string});
  };
  const auto self_read = [&]() -> mir::ExprId {
    return arm_block.exprs.Add(MakeSelfRefExpr(arm_frame, self_ptr_type));
  };
  const auto parent_read = [&]() -> mir::ExprId {
    if (!runtime_parent_handle.has_value()) {
      return self_read();
    }
    return arm_block.exprs.Add(
        mir::MakeFieldAccessExpr(
            self_read(),
            mir::ClassFieldTarget{
                .owner = arm_frame.current_class_id,
                .slot = *runtime_parent_handle},
            owner_class.fields.Get(*runtime_parent_handle).type));
  };

  // Build the child's structural identity once and pass it as the child's
  // own ctor argument. The child holds onto it from the moment its
  // constructor returns; %m, by-name lookup, and debug traces all read
  // from that single source. The index list carries the caller-provided
  // hierarchy index when there is one -- a generated scope's constant index --
  // and is empty otherwise.
  std::vector<mir::ExprId> index_elems;
  if (array_index.has_value()) {
    index_elems.push_back(*array_index);
  }
  const mir::TypeId indices_type = mir::MachineArrayOf(
      unit_lowerer.Unit().types, builtins.int_type, index_elems.size());
  const mir::ExprId indices_id = arm_block.exprs.Add(
      mir::Expr{
          .data = mir::CompositeExpr{.parts = std::move(index_elems)},
          .type = indices_type});
  const mir::ExprId segment_id = arm_block.exprs.Add(
      mir::Expr{
          .data =
              mir::CallExpr{
                  .callee = mir::Construct{},
                  .arguments = {string_literal(runtime_label), indices_id}},
          .type = builtins.hierarchy_segment});

  std::vector<mir::ExprId> ctor_call_args;
  ctor_call_args.reserve(3);
  ctor_call_args.push_back(parent_read());
  ctor_call_args.push_back(segment_id);
  if (construction_value.has_value()) {
    ctor_call_args.push_back(*construction_value);
  }
  const mir::ExprId ctor_call_id = arm_block.exprs.Add(
      mir::Expr{
          .data =
              mir::CallExpr{
                  .callee = mir::Construct{},
                  .arguments = std::move(ctor_call_args)},
          .type = child_ptr_type});

  // The runtime tree owns the child (AddOwnedChild consumes the freshly built
  // unique pointer); ownership transfers after the child's constructor commits,
  // so a thrown subobject ctor leaves no half-attached scope. The borrowed
  // pointer it hands back is downcast and stored in the parent's handle
  // member, which is what a typed intra-unit route navigates through.
  const mir::ExprId add_call_id = arm_block.exprs.Add(
      mir::Expr{
          .data =
              mir::CallExpr{
                  .callee =
                      mir::Direct{
                          .target = support::BuiltinFn::kAddOwnedChild,
                          .receiver = parent_read()},
                  .arguments = {ctor_call_id}},
          .type = builtins.scope_ptr});
  return arm_block.exprs.Add(
      mir::Expr{
          .data = mir::CastExpr{.operand = add_call_id}, .type = handle_type});
}

// The same construction for a child this scope keeps one handle to, stored into
// the member that names it.
void AppendOwnedChildConstruction(
    UnitLowerer& unit_lowerer, const WalkFrame& arm_frame,
    std::optional<mir::FieldId> runtime_parent_handle,
    const std::string& runtime_label, mir::ClassId child_scope_id,
    std::optional<mir::ExprId> array_index, mir::FieldId handle_field,
    std::optional<mir::ExprId> construction_value) {
  mir::Block& arm_block = *arm_frame.current_block;
  const mir::Class& owner_class = *arm_frame.current_class;
  const mir::TypeId handle_type = owner_class.fields.Get(handle_field).type;
  const mir::ExprId typed_handle = BuildOwnedChildHandle(
      unit_lowerer, arm_frame, runtime_parent_handle, runtime_label,
      child_scope_id, array_index, handle_type, construction_value);
  const mir::ExprId member = arm_block.exprs.Add(
      mir::MakeFieldAccessExpr(
          arm_block.exprs.Add(
              MakeSelfRefExpr(arm_frame, owner_class.self_pointer_type)),
          mir::ClassFieldTarget{
              .owner = arm_frame.current_class_id, .slot = handle_field},
          handle_type));
  arm_block.AppendStmt(
      mir::ExprStmt{
          .expr = arm_block.exprs.Add(
              mir::Expr{
                  .data =
                      mir::AssignExpr{.target = member, .value = typed_handle},
                  .type = handle_type})});
}

// A generate whose blocks are one body builds that body at every index the loop
// counts out (LRM 27.4). The index is a declaration of this scope, so the
// loop's own expressions read and write it the way any name reaches a
// declaration, and each block is built with the index it stands at. What the
// member receives is the sequence of what the loop built, complete: the one
// that grows is a local these steps own and nothing else can name.
auto LowerRepeatedGenerate(
    StructuralScopeLowerer& lowerer, WalkFrame frame,
    const hir::BlocksRepeat& repeat, const GenerateBindings& gen_bindings)
    -> diag::Result<mir::Stmt> {
  UnitLowerer& unit_lowerer = lowerer.Owner();
  const mir::CompilationUnit& unit = unit_lowerer.Unit();
  const hir::StructuralScope& hir_scope = lowerer.HirScope();
  const mir::Class& owner_class = *frame.current_class;

  // One body, so the scope the loop builds is the only one there is.
  const auto& binding = gen_bindings.Get(hir::StructuralScopeId{0});
  const mir::TypeId sequence_type =
      owner_class.fields.Get(binding.borrowed_handle).type;
  const mir::TypeId handle_type =
      unit.types.Get(sequence_type).Get<mir::VectorType>().element;
  const mir::TypeId index_type = unit_lowerer.TranslateType(
      hir_scope.structural_data_objects.Get(repeat.variable).type);
  const mir::FieldId index_field = lowerer.TranslateStructuralDataObject(
      hir::StructuralHops{0}, repeat.variable);

  BlockBuilder steps(frame);
  mir::Block& body = steps.Body();
  const WalkFrame body_frame = steps.Frame();
  const mir::LocalId sequence =
      steps.Bindings().DeclareAnonymous(sequence_type);
  body.AppendStmt(
      mir::LocalDeclStmt{
          .target = sequence,
          .init = body.exprs.Add(
              BuildSequenceConstructionCall(unit, body, sequence_type, {}))});

  const auto index_place = [&](mir::Block& in) -> mir::ExprId {
    return in.exprs.Add(
        mir::MakeFieldAccessExpr(
            in.exprs.Add(MakeSelfRefExpr(
                frame.WithBlock(&in), owner_class.self_pointer_type)),
            mir::ClassFieldTarget{
                .owner = frame.current_class_id, .slot = index_field},
            owner_class.fields.Get(index_field).type));
  };
  const auto index_read = [&](mir::Block& in) -> mir::ExprId {
    return in.exprs.Add(
        mir::Expr{
            .data =
                mir::CallExpr{
                    .callee =
                        mir::Direct{
                            .target = support::BuiltinFn::kLoad,
                            .receiver = index_place(in)},
                    .arguments = {}},
            .type = index_type});
  };

  // Where the loop starts is a value it is given: LRM 27.4 spells the
  // initialization as an assignment of one and admits no other form there, so
  // the write belongs to the loop rather than to anything the source wrote.
  auto initial_or =
      lowerer.LowerExpr(hir_scope.exprs.Get(repeat.initial), body_frame);
  if (!initial_or) return std::unexpected(std::move(initial_or.error()));
  body.AppendStmt(
      mir::ExprStmt{
          .expr = body.exprs.Add(BuildStoreExpr(
              unit_lowerer.Unit(), body,
              WriteTarget{.owner = index_place(body), .descent = {}},
              body.exprs.Add(*std::move(initial_or)), std::nullopt,
              index_type))});

  mir::Block loop_body;
  const WalkFrame loop_frame = body_frame.WithBlock(&loop_body);
  const mir::ExprId child = BuildOwnedChildHandle(
      unit_lowerer, loop_frame, std::nullopt, binding.label,
      binding.lowerer->ClassId(), index_read(loop_body), handle_type,
      index_read(loop_body));
  const mir::ExprId grown = loop_body.exprs.Add(
      mir::Expr{
          .data =
              mir::CallExpr{
                  .callee =
                      mir::Direct{
                          .target = support::BuiltinFn::kExtendSequence},
                  .arguments =
                      {loop_body.exprs.Add(
                           mir::MakeLocalRefExpr(sequence, sequence_type)),
                       child}},
          .type = sequence_type});
  loop_body.AppendStmt(
      mir::ExprStmt{
          .expr = loop_body.exprs.Add(
              mir::MakeAssignExpr(
                  loop_body.exprs.Add(
                      mir::MakeLocalRefExpr(sequence, sequence_type)),
                  grown, sequence_type))});
  // The step is the expression the source wrote, and it reaches the next index
  // by writing the loop's own, so it is placed for its effect and its value is
  // dropped -- every form LRM 27.4 admits for it says where the index goes in
  // exactly that way.
  auto step_or =
      lowerer.LowerExpr(hir_scope.exprs.Get(repeat.step), loop_frame);
  if (!step_or) return std::unexpected(std::move(step_or.error()));
  loop_body.AppendStmt(
      mir::ExprStmt{.expr = loop_body.exprs.Add(*std::move(step_or))});

  auto condition =
      lowerer.LowerExpr(hir_scope.exprs.Get(repeat.condition), body_frame);
  if (!condition) return std::unexpected(std::move(condition.error()));
  const mir::ExprId condition_id = ReduceToCondition(
      unit_lowerer.Unit(), body, body.exprs.Add(*std::move(condition)));
  const mir::BlockId loop_scope = body.child_scopes.Add(std::move(loop_body));
  body.AppendStmt(
      mir::WhileStmt{.condition = condition_id, .scope = loop_scope});

  const mir::ExprId member = body.exprs.Add(
      mir::MakeFieldAccessExpr(
          body.exprs.Add(
              MakeSelfRefExpr(body_frame, owner_class.self_pointer_type)),
          mir::ClassFieldTarget{
              .owner = frame.current_class_id, .slot = binding.borrowed_handle},
          sequence_type));
  body.AppendStmt(
      mir::ExprStmt{
          .expr = body.exprs.Add(
              mir::MakeAssignExpr(
                  member,
                  body.exprs.Add(
                      mir::MakeLocalRefExpr(sequence, sequence_type)),
                  sequence_type))});
  return steps.BuildStatement();
}

auto LowerSelectionChoiceInto(
    StructuralScopeLowerer& lowerer, WalkFrame frame,
    const hir::BlocksChoose& chosen, hir::SelectionChoiceId at,
    const GenerateBindings& gen_bindings) -> diag::Result<void>;

// What stands on one side of a choice, built into the block the side owns:
// nothing at all, the construction of one alternative's block, or a further
// choice the source wrote inside this side (LRM 27.5).
auto LowerSelectionBranchInto(
    StructuralScopeLowerer& lowerer, WalkFrame frame,
    const hir::BlocksChoose& chosen, const hir::SelectionBranch& branch,
    const GenerateBindings& gen_bindings) -> diag::Result<void> {
  return std::visit(
      Overloaded{
          [](const hir::NothingStands&) -> diag::Result<void> { return {}; },
          [&](const hir::AlternativeStands& stands) -> diag::Result<void> {
            // An alternative no elaboration of the construct selected has no
            // body, and the same expressions read against the same inputs
            // cannot reach it here either.
            const std::optional<hir::StructuralScopeId> block =
                chosen.alternatives[stands.position];
            if (!block.has_value()) return {};
            const auto& binding = gen_bindings.Get(*block);
            AppendOwnedChildConstruction(
                lowerer.Owner(), frame, std::nullopt, binding.label,
                binding.lowerer->ClassId(), std::nullopt,
                binding.borrowed_handle, std::nullopt);
            return {};
          },
          [&](hir::SelectionChoiceId nested) -> diag::Result<void> {
            return LowerSelectionChoiceInto(
                lowerer, frame, chosen, nested, gen_bindings);
          }},
      branch);
}

// One alternative of a `case` is reached where the selector matches one of its
// own labels. LRM 12.5 fixes that comparison: it succeeds only where every bit
// matches exactly, `x` and `z` included, so the selector is read against each
// label rather than reduced to a value first.
auto MatchesAnyLabel(
    StructuralScopeLowerer& lowerer, WalkFrame frame, mir::ExprId selector,
    const std::vector<hir::ExprId>& labels) -> diag::Result<mir::ExprId> {
  mir::Block& block = *frame.current_block;
  const mir::CompilationUnit& unit = lowerer.Owner().Unit();
  const hir::StructuralScope& hir_scope = lowerer.HirScope();

  std::optional<mir::ExprId> any;
  for (const hir::ExprId label : labels) {
    auto lowered = lowerer.LowerExpr(hir_scope.exprs.Get(label), frame);
    if (!lowered) return std::unexpected(std::move(lowered.error()));
    const mir::ExprId matched = block.exprs.Add(
        mir::Expr{
            .data =
                mir::CallExpr{
                    .callee =
                        mir::Direct{
                            .target = support::BuiltinFn::kCaseEqual,
                            .receiver = selector},
                    .arguments = {block.exprs.Add(*std::move(lowered))}},
            .type = unit.builtins.bit1});
    // Every operand of a logical operator here is a stated predicate, so a
    // comparison's own 1-bit answer is reduced where it is produced.
    const mir::ExprId here = ReduceToCondition(unit, block, matched);
    any = any.has_value() ? block.exprs.Add(
                                mir::Expr{
                                    .data =
                                        mir::BinaryExpr{
                                            .op = mir::BinaryOp::kLogicalOr,
                                            .lhs = *any,
                                            .rhs = here},
                                    .type = unit.builtins.machine_bool})
                          : here;
  }
  // An item the source gave no label matches nothing of its own, which is what
  // a `default` is; it is reached by the search running out instead.
  if (!any.has_value()) {
    return block.exprs.Add(
        mir::Expr{
            .data = mir::MachineBoolLiteral{.value = false},
            .type = unit.builtins.machine_bool});
  }
  return *any;
}

// A `case` searches its items in the order the source wrote them and stops at
// the first match, taking the `default` only once every one of them has failed
// (LRM 12.5). That search is a chain of conditions over one selector, built
// from what stands after every item has failed outwards, so an item states its
// own labels and nothing about the items before it.
auto LowerLabelledChoiceInto(
    StructuralScopeLowerer& lowerer, WalkFrame frame,
    const hir::BlocksChoose& chosen, const hir::ChoiceOnLabel& on,
    const GenerateBindings& gen_bindings) -> diag::Result<void> {
  mir::Block& block = *frame.current_block;
  const hir::StructuralScope& hir_scope = lowerer.HirScope();

  mir::Block tail;
  auto otherwise = LowerSelectionBranchInto(
      lowerer, frame.WithBlock(&tail), chosen, on.otherwise, gen_bindings);
  if (!otherwise) return std::unexpected(std::move(otherwise.error()));

  for (std::size_t back = on.items.size(); back > 0; --back) {
    const hir::LabeledItem& item = on.items[back - 1];
    mir::Block stands;
    auto body = LowerSelectionBranchInto(
        lowerer, frame.WithBlock(&stands), chosen, item.stands, gen_bindings);
    if (!body) return std::unexpected(std::move(body.error()));

    mir::Block step;
    const WalkFrame step_frame = frame.WithBlock(&step);
    auto selector =
        lowerer.LowerExpr(hir_scope.exprs.Get(on.selector), step_frame);
    if (!selector) return std::unexpected(std::move(selector.error()));
    auto test = MatchesAnyLabel(
        lowerer, step_frame, step.exprs.Add(*std::move(selector)), item.labels);
    if (!test) return std::unexpected(std::move(test.error()));
    step.AppendStmt(
        mir::IfStmt{
            .condition = *test,
            .then_scope = step.child_scopes.Add(std::move(stands)),
            .else_scope = step.child_scopes.Add(std::move(tail))});
    tail = std::move(step);
  }
  block.AppendStmt(
      mir::BlockStmt{.scope = block.child_scopes.Add(std::move(tail))});
  return {};
}

// One conditional generate construct, built as the source nested it: the
// condition is asked once and each side holds whatever the source wrote there.
auto LowerSelectionChoiceInto(
    StructuralScopeLowerer& lowerer, WalkFrame frame,
    const hir::BlocksChoose& chosen, hir::SelectionChoiceId at,
    const GenerateBindings& gen_bindings) -> diag::Result<void> {
  const hir::SelectionChoice& choice = chosen.choices.Get(at);
  if (const auto* on = std::get_if<hir::ChoiceOnLabel>(&choice)) {
    return LowerLabelledChoiceInto(lowerer, frame, chosen, *on, gen_bindings);
  }

  const auto& on = std::get<hir::ChoiceOnCondition>(choice);
  mir::Block& block = *frame.current_block;
  const mir::CompilationUnit& unit = lowerer.Owner().Unit();
  const hir::StructuralScope& hir_scope = lowerer.HirScope();

  auto condition = lowerer.LowerExpr(hir_scope.exprs.Get(on.condition), frame);
  if (!condition) return std::unexpected(std::move(condition.error()));
  const mir::ExprId test =
      ReduceToCondition(unit, block, block.exprs.Add(*std::move(condition)));

  mir::Block holds;
  auto taken = LowerSelectionBranchInto(
      lowerer, frame.WithBlock(&holds), chosen, on.holds, gen_bindings);
  if (!taken) return std::unexpected(std::move(taken.error()));

  std::optional<mir::BlockId> otherwise;
  if (!std::holds_alternative<hir::NothingStands>(on.fails)) {
    mir::Block fails;
    auto untaken = LowerSelectionBranchInto(
        lowerer, frame.WithBlock(&fails), chosen, on.fails, gen_bindings);
    if (!untaken) return std::unexpected(std::move(untaken.error()));
    otherwise = block.child_scopes.Add(std::move(fails));
  }

  block.AppendStmt(
      mir::IfStmt{
          .condition = test,
          .then_scope = block.child_scopes.Add(std::move(holds)),
          .else_scope = otherwise});
  return {};
}

auto LowerChosenGenerate(
    StructuralScopeLowerer& lowerer, WalkFrame frame,
    const hir::BlocksChoose& chosen, const GenerateBindings& gen_bindings)
    -> diag::Result<mir::Stmt> {
  mir::Block& block = *frame.current_block;
  mir::Block body;

  auto built = LowerSelectionChoiceInto(
      lowerer, frame.WithBlock(&body), chosen, chosen.root, gen_bindings);
  if (!built) return std::unexpected(std::move(built.error()));

  return mir::Stmt{
      .label = std::nullopt,
      .data = mir::BlockStmt{.scope = block.child_scopes.Add(std::move(body))}};
}

// A generate construct becomes the construction its compiled form calls for.
// What is left when it repeats nothing and chooses nothing is the correctness
// baseline every construct falls back to: each instantiated block's own
// concrete scalar child, built directly with no runtime branch or loop, each
// carrying any constant hierarchy index it has. A block whose index is a value
// its construction supplies is handed that index here, the same value its
// hierarchy segment carries.
auto LowerGenerateAsStmt(
    StructuralScopeLowerer& lowerer, WalkFrame frame, const hir::Generate& gen,
    const GenerateBindings& gen_bindings) -> diag::Result<mir::Stmt> {
  if (const auto* repeat = std::get_if<hir::BlocksRepeat>(&gen.counting)) {
    return LowerRepeatedGenerate(lowerer, frame, *repeat, gen_bindings);
  }
  if (const auto* chosen = std::get_if<hir::BlocksChoose>(&gen.counting)) {
    return LowerChosenGenerate(lowerer, frame, *chosen, gen_bindings);
  }
  mir::Block& block = *frame.current_block;

  mir::Block body;
  const WalkFrame body_frame = frame.WithBlock(&body);
  for (const hir::StructuralScopeId scope_id : gen.child_scopes.Ids()) {
    const auto& child_scope = gen.child_scopes.Get(scope_id);
    const auto& binding = gen_bindings.Get(scope_id);
    std::optional<mir::ExprId> index_id;
    std::optional<mir::ExprId> supplied_index;
    if (child_scope.index.has_value()) {
      index_id =
          BuildIntLiteral(lowerer.Owner().Unit(), body, *child_scope.index);
      // The value reaches a cell the block declared, so it is written in that
      // declaration's own representation rather than in the one an index is
      // spelled with: a genvar is `integer` (LRM 27.4), and a store whose
      // representation is not the cell's is refused at run time.
      if (const auto supplied = ConstructionValueOf(child_scope)) {
        supplied_index = BuildIntegralLiteral(
            lowerer.Owner().Unit(), body,
            lowerer.Owner().TranslateType(
                child_scope.structural_data_objects.Get(*supplied).type),
            mir::IntegralConstant{
                .value_words = {static_cast<std::uint64_t>(*child_scope.index)},
                .state_words = {}});
      }
    }
    AppendOwnedChildConstruction(
        lowerer.Owner(), body_frame, std::nullopt, binding.label,
        binding.lowerer->ClassId(), index_id, binding.borrowed_handle,
        supplied_index);
  }
  const mir::BlockId body_id = block.child_scopes.Add(std::move(body));
  return mir::Stmt{
      .label = std::nullopt, .data = mir::BlockStmt{.scope = body_id}};
}

}  // namespace

auto IndexCoordinates(
    UnitLowerer& unit_lowerer, mir::Block& block, ReachedObject reached,
    std::span<const std::uint32_t> indices) -> ReachedObject {
  for (const std::uint32_t coord : indices) {
    reached.type = unit_lowerer.Unit()
                       .types.Get(reached.type)
                       .Get<mir::VectorType>()
                       .element;
    reached.expr = block.exprs.Add(
        mir::Expr{
            .data =
                mir::VectorGetExpr{
                    .vector = reached.expr,
                    .index = BuildSequenceIndex(unit_lowerer, block, coord)},
            .type = reached.type});
  }
  return reached;
}

namespace {

// What a route ends at, walked where it is used or read out of the slot it was
// kept in.
template <typename Leaf>
auto EndAlong(
    const StructuralScopeLowerer& lowerer, const WalkFrame& frame,
    const hir::Route<Leaf>& route, const RouteReach& reach) -> mir::ExprId {
  return std::visit(
      Overloaded{
          [&](const ClimbedRoute&) {
            return BuildRouteValue(lowerer, frame, route);
          },
          [&](const StoredRoute& stored) {
            return frame.current_block->exprs.Add(
                BuildStructuralFieldAccessExpr(
                    frame, lowerer.Owner().Unit(), mir::EnclosingHops{0},
                    stored.slot));
          }},
      reach);
}

}  // namespace

auto StructuralScopeLowerer::RouteEnd(
    const WalkFrame& frame, hir::RoutedObjectRefId id) const -> mir::ExprId {
  return EndAlong(*this, frame, HirScope().routes.objects.Get(id), ReachOf(id));
}

auto StructuralScopeLowerer::RouteEnd(
    const WalkFrame& frame, hir::RoutedCallableRefId id) const -> mir::ExprId {
  return EndAlong(
      *this, frame, HirScope().routes.callables.Get(id), ReachOf(id));
}

auto StructuralScopeLowerer::RouteEnd(
    const WalkFrame& frame, hir::RoutedDisableTargetRefId id) const
    -> mir::ExprId {
  return EndAlong(
      *this, frame, HirScope().routes.disable_targets.Get(id), ReachOf(id));
}

auto StructuralScopeLowerer::RouteEnd(
    const WalkFrame& frame, hir::PropertyCoordinateId id) const -> mir::ExprId {
  return EndAlong(
      *this, frame, HirScope().routes.property_coordinates.Get(id),
      ReachOf(id));
}

auto StructuralScopeLowerer::RouteEnd(
    const WalkFrame& frame, hir::BehaviorCoordinateId id) const -> mir::ExprId {
  return EndAlong(
      *this, frame, HirScope().routes.behavior_coordinates.Get(id),
      ReachOf(id));
}

auto StructuralScopeLowerer::RouteEnd(
    const WalkFrame& frame, hir::BehaviorBodyId id) const -> mir::ExprId {
  return EndAlong(
      *this, frame, HirScope().routes.behavior_bodies.Get(id), ReachOf(id));
}

auto StructuralScopeLowerer::DeclareShape() -> diag::Result<mir::ClassId> {
  UnitLowerer& unit_lowerer = *owner_;
  const hir::StructuralScope& hir_scope = *hir_scope_;

  // The identity is minted before the shape is populated so the class's own
  // `self_pointer_type` can name it. A scope that is the object its unit
  // publishes mints a second one for what the unit promised of it: the name the
  // source wrote is what a referrer has and so is the promise's, and this
  // class, which realizes it, answers to none.
  if (name_.has_value()) {
    promise_id_ = unit_lowerer.Unit().DeclareClass();
  }
  class_id_ = unit_lowerer.Unit().DeclareClass();
  const mir::TypeId self_object_type = unit_lowerer.Unit().types.Intern(
      mir::Type{mir::ObjectType{.class_id = class_id_}});
  const mir::TypeId self_pointer_type = unit_lowerer.Unit().types.Intern(
      mir::Type{mir::PointerType{
          .pointee = self_object_type,
          .ownership = mir::PointerOwnership::kBorrowed}});

  ClassShape shape;
  shape.is_final = true;
  shape.self_pointer_type = self_pointer_type;
  shape.time_resolution = hir_scope.time_resolution;

  AttachRuntimeScopeCtorPrefix(unit_lowerer.Unit(), shape);
  // A value construction supplies arrives as a parameter of the constructor,
  // after the prefix every scope takes, because it is what distinguishes one
  // built scope from another built from the same declarations.
  construction_value_ = ConstructionValueOf(hir_scope);
  if (construction_value_.has_value()) {
    shape.ctor_prefix_params.Add(
        mir::ParamDecl{
            .type = unit_lowerer.TranslateType(
                hir_scope.structural_data_objects.Get(*construction_value_)
                    .type)});
  }

  // Every member this scope holds, in the order it was declared. Nothing counts
  // a position out of this order: what another unit reaches is a behavior the
  // promise states, so a member sits where it was declared whether or not the
  // unit published it -- which is also the order its initializer runs in (LRM
  // 10.5).
  std::vector<hir::PublishedDecl> member_order;
  member_order.reserve(
      hir_scope.structural_data_objects.size() +
      hir_scope.instance_members.size() + hir_scope.interface_ports.size());
  for (const hir::StructuralDataObjectId id :
       hir_scope.structural_data_objects.Ids()) {
    member_order.emplace_back(id);
  }
  for (const hir::InstanceMemberId id : hir_scope.instance_members.Ids()) {
    member_order.emplace_back(id);
  }
  for (const hir::InterfacePortId id : hir_scope.interface_ports.Ids()) {
    member_order.emplace_back(id);
  }

  std::vector<mir::FieldId> data_object_fields(
      hir_scope.structural_data_objects.size());
  std::vector<mir::FieldId> instance_fields(hir_scope.instance_members.size());
  std::vector<mir::FieldId> interface_port_fields(
      hir_scope.interface_ports.size());
  for (const hir::PublishedDecl& decl : member_order) {
    std::visit(
        Overloaded{
            [&](const hir::StructuralDataObjectId& id) {
              const auto& d = hir_scope.structural_data_objects.Get(id);
              const mir::TypeId cell = unit_lowerer.MemberCellType(
                  unit_lowerer.TranslateType(d.type), hir::StorageOf(d));
              data_object_fields[id.value] =
                  hir::AnsweredByName(d) ? shape.AddNamedField(d.name, cell)
                                         : shape.AddField(cell);
            },
            [&](const hir::InstanceMemberId& id) {
              // Every instance member keeps one borrowed typed handle on this
              // class, and that handle's type states the member's cardinality
              // -- the bare handle for a single instance, one sequence wrapper
              // per declared dimension for an array (LRM 23.3.2). A route step
              // projects the handle and indexes it once per dimension, so
              // reaching an element never has to name the member a second
              // time.
              const auto& im = hir_scope.instance_members.Get(id);
              instance_fields[id.value] = shape.AddNamedField(
                  im.instance_name,
                  MakeInstanceMemberType(
                      unit_lowerer, im, mir::PointerOwnership::kBorrowed));
            },
            [&](const hir::InterfacePortId& id) {
              const auto& port = hir_scope.interface_ports.Get(id);
              // The port stands for instances of the unit its record names, so
              // that record is the one source of both the object's type and the
              // positions a name reached through it is counted out of. How many
              // instances is the port's own multiplicity, which stands over
              // that type the way an instance member's stands over its handle.
              const mir::TypeId object_type = unit_lowerer.Unit().types.Intern(
                  mir::Type{mir::ExternalUnitObjectType{
                      .object = unit_lowerer.TranslateExternalUnitObject(
                          port.object)}});
              interface_port_fields[id.value] = shape.AddNamedField(
                  port.name,
                  unit_lowerer.MemberCellType(
                      SequenceOver(
                          unit_lowerer, object_type, port.array_dims.size()),
                      hir::BorrowedObjectStorage{}));
            }},
        decl);
  }
  // What the unit promised of this object, stated as the behaviors a referrer
  // reaches it through, in the order the signature published them -- which is
  // the order a referrer counts and the only thing both sides share about the
  // object.
  if (promise_id_.has_value()) {
    std::vector<PromisedMember> members;
    members.reserve(hir_scope.published_members.size());
    for (const hir::PublishedDecl& decl : hir_scope.published_members) {
      const mir::FieldId slot = std::visit(
          Overloaded{
              [&](const hir::StructuralDataObjectId& id) {
                return data_object_fields[id.value];
              },
              [&](const hir::InstanceMemberId& id) {
                return instance_fields[id.value];
              },
              [&](const hir::InterfacePortId& id) {
                return interface_port_fields[id.value];
              }},
          decl);
      const std::optional<std::string_view> named =
          mir::NameOf(shape.named_fields, slot);
      if (!named.has_value()) {
        throw InternalError(
            "hir_to_mir: a unit published a member the source never named");
      }
      members.push_back(
          PromisedMember{
              .name = std::string{*named},
              .cell = slot,
              .cell_type = shape.fields.Get(slot).type});
    }
    promised_members_ = std::move(members);
  }

  data_object_fields_ = {
      hir_scope.structural_data_objects.size(), std::move(data_object_fields)};

  // A history is storage nothing outside this scope names, so the unit promises
  // nothing about it: what reaches it is the scope's own
  // activation, its sampler, and the reads that asked for it (LRM 16.9.3). Its
  // value type is the subject's own, because what a tick keeps is what that
  // expression settled.
  std::vector<mir::FieldId> sampled_history_fields;
  sampled_history_fields.reserve(hir_scope.sampled_histories.size());
  for (const hir::SampledHistoryId id : hir_scope.sampled_histories.Ids()) {
    const hir::SampledHistoryDecl& history =
        hir_scope.sampled_histories.Get(id);
    const mir::TypeId value_type =
        unit_lowerer.TranslateType(hir_scope.exprs.Get(history.subject).type);
    sampled_history_fields.push_back(
        shape.AddField(unit_lowerer.Unit().types.Intern(
            mir::Type{mir::SampledHistoryType{.value = value_type}})));
  }
  sampled_history_fields_ = {
      hir_scope.sampled_histories.size(), std::move(sampled_history_fields)};

  // What an assertion has in flight is storage nothing outside this scope
  // names either: an attempt is started by the clock and read by the tick that
  // advances it, both of which are this scope's own (LRM 16.14.1). The type
  // carries nothing, because how wide a position set is and what a pending
  // attempt is owed are fixed by filling the storage rather than by naming it.
  std::vector<mir::FieldId> concurrent_assertion_fields;
  concurrent_assertion_fields.reserve(hir_scope.concurrent_assertions.size());
  for (std::size_t i = 0; i < hir_scope.concurrent_assertions.size(); ++i) {
    concurrent_assertion_fields.push_back(
        shape.AddField(unit_lowerer.Unit().types.Intern(
            mir::Type{mir::EvaluationAttemptsType{}})));
  }
  concurrent_assertion_fields_ = {
      hir_scope.concurrent_assertions.size(),
      std::move(concurrent_assertion_fields)};
  instance_member_fields_ = {
      hir_scope.instance_members.size(), std::move(instance_fields)};
  interface_port_fields_ = {
      hir_scope.interface_ports.size(), std::move(interface_port_fields)};

  mir::TypePool& types = unit_lowerer.Unit().types;
  const hir::ScopeRoutes& routes = hir_scope.routes;
  value_reaches_ =
      DeclareReaches(shape, routes.values, [&](const hir::DataLeaf& leaf) {
        return PointerTypeOf(unit_lowerer, leaf);
      });
  object_reaches_ =
      DeclareReaches(shape, routes.objects, [&](const hir::ScopeLeaf& leaf) {
        return PointerTypeOf(unit_lowerer, leaf);
      });
  callable_reaches_ = DeclareReaches(
      shape, routes.callables, [&](const hir::OpaqueCallableLeaf&) {
        return mir::ErasedFunction(types);
      });
  disable_target_reaches_ = DeclareReaches(
      shape, routes.disable_targets,
      [&](const hir::DisableLeaf&) { return DisableTargetPointerType(types); });
  property_coordinate_reaches_ = DeclareReaches(
      shape, routes.property_coordinates,
      [&](const hir::PropertyCoordinateLeaf&) {
        return CoordinateType(
            types, mir::RuntimeLibraryKind::kPropertyCoordinate);
      });
  behavior_coordinate_reaches_ = DeclareReaches(
      shape, routes.behavior_coordinates,
      [&](const hir::BehaviorCoordinateLeaf&) {
        return CoordinateType(
            types, mir::RuntimeLibraryKind::kBehaviorCoordinate);
      });
  behavior_body_reaches_ = DeclareReaches(
      shape, routes.behavior_bodies,
      [&](const hir::BehaviorBodyLeaf&) { return mir::ErasedFunction(types); });

  // Recursively declare every owned generate child's class shape; each child
  // lowerer is retained for the body sweep.
  std::vector<GenerateBindings> generates;
  generates.reserve(hir_scope.generates.size());
  for (const hir::GenerateId gen_id : hir_scope.generates.Ids()) {
    const auto& gen = hir_scope.generates.Get(gen_id);
    // Every compiled scope of a generate gets its own class and its own scalar
    // handle: blocks that are not one body are told apart on the hierarchy by
    // the index each carries, and alternatives of a conditional by at most one
    // of them ever being built. A repeated structure is the one that differs.
    // Its blocks are a single class the loop builds at every index, so the
    // handle it keeps states that multiplicity the way every other declaration
    // standing for several objects does -- a sequence of the handle -- and a
    // route step indexes it.
    const bool repeats = std::visit(
        Overloaded{
            [](const hir::BlocksStandAlone&) { return false; },
            [](const hir::BlocksChoose&) { return false; },
            [](const hir::BlocksRepeat&) { return true; },
        },
        gen.counting);
    std::vector<ChildStructuralScopeBinding> gen_bindings;
    gen_bindings.reserve(gen.child_scopes.size());
    for (const auto& child_scope : gen.child_scopes) {
      auto child = std::make_unique<StructuralScopeLowerer>(
          unit_lowerer, this, std::nullopt, child_scope);
      auto child_r = child->DeclareShape();
      if (!child_r) return std::unexpected(std::move(child_r.error()));

      const mir::ClassId child_id = *child_r;
      shape.contained.push_back(child_id);
      const mir::TypeId handle_type = unit_lowerer.Unit().types.Intern(
          mir::Type{mir::PointerType{
              .pointee = unit_lowerer.Unit().types.Intern(
                  mir::Type{mir::ObjectType{.class_id = child_id}}),
              .ownership = mir::PointerOwnership::kBorrowed}});
      const mir::FieldId borrowed_handle = shape.AddField(
          repeats ? SequenceOver(unit_lowerer, handle_type, 1) : handle_type);
      gen_bindings.push_back(
          ChildStructuralScopeBinding{
              .label = child_scope.source_name,
              .borrowed_handle = borrowed_handle,
              .lowerer = child.get()});
      children_.push_back(std::move(child));
    }
    generates.emplace_back(gen.child_scopes.size(), std::move(gen_bindings));
  }
  generate_bindings_ = {hir_scope.generates.size(), std::move(generates)};

  // Every procedural scope becomes a name node -- an object carrying the
  // identity a hierarchical path matches -- whatever the source called it and
  // whether or not anything was declared there, so one shape lowers every
  // scope. Whether a name reaches it decides only what it exposes: a scope the
  // source named carries its segment and one it did not carries none, which
  // keeps the latter off every hierarchical path (LRM 23.6) while it still
  // holds the nodes below it together.
  //
  // This scope keeps a borrowed handle to every one of them, however deeply
  // they nest, so a body reaches its own name node in one step and nothing has
  // to know what stands between. The nodes' own nesting is the HIR scope tree,
  // read where the objects are built.
  const mir::TypeId cancellation_target_type = unit_lowerer.Unit().types.Intern(
      mir::Type{mir::RuntimeLibraryType{
          .kind = mir::RuntimeLibraryKind::kCancellationTarget}});
  std::vector<DeclaredScope> scopes;
  scopes.reserve(hir_scope.procedural_scopes.size());
  for (const hir::ProceduralScopeId scope_id :
       hir_scope.procedural_scopes.Ids()) {
    const auto& scope = hir_scope.procedural_scopes.Get(scope_id);

    const mir::ClassId node_class = unit_lowerer.Unit().DeclareClass();
    ClassShape node_shape;
    node_shape.is_final = true;
    node_shape.self_pointer_type = unit_lowerer.Unit().types.Intern(
        mir::Type{mir::PointerType{
            .pointee = unit_lowerer.Unit().types.Intern(
                mir::Type{mir::ObjectType{.class_id = node_class}}),
            .ownership = mir::PointerOwnership::kBorrowed}});
    node_shape.time_resolution = hir_scope.time_resolution;
    AttachRuntimeScopeCtorPrefix(unit_lowerer.Unit(), node_shape);
    unit_lowerer.DefineClassShape(node_class, std::move(node_shape));
    shape.contained.push_back(node_class);

    // The handle is a borrowed pointer to the node's own class, the same shape
    // an owned child instance or generate block keeps. That every owned child
    // is reachable through a typed member is what makes a class's layout state
    // which objects the runtime builds under it, so the naming handle carries
    // the node's type although it only ever asks for a name.
    DeclaredScope node{
        .name_node =
            ScopeNameNode{
                .class_id = node_class,
                .borrowed_handle =
                    shape.AddField(unit_lowerer.Unit().types.Intern(
                        mir::Type{mir::PointerType{
                            .pointee = unit_lowerer.Unit().types.Intern(
                                mir::Type{
                                    mir::ObjectType{.class_id = node_class}}),
                            .ownership = mir::PointerOwnership::kBorrowed}}))},
        .disable_target = std::nullopt};

    // What a `disable` of this scope invalidates (LRM 9.6.2). Its targets are
    // the blocks and tasks a name reaches, so a scope the source named owns one
    // for that reason alone and one it did not owns none -- no pass has to
    // first find out which scopes some `disable` names. A scope of this
    // hierarchy is replicated with its instance, so the cell is one per
    // instance, shared by every activation of the scope.
    if (scope.source_name.has_value()) {
      node.disable_target = DeclareStaticCell(
          InstanceStorage{.shape = &shape}, cancellation_target_type);
    }
    scopes.push_back(node);
  }
  scopes_ = {hir_scope.procedural_scopes.size(), std::move(scopes)};

  // Everything a peer may need about a subroutine before its body exists is
  // settled here, in one pass over the subroutines. Its callable identity comes
  // from the shape's own pool, so a call in one body resolves a forward or
  // mutual reference to a peer (LRM 13.7) whatever order the two lower in; the
  // body pass fills the identity it was handed rather than working out where
  // the other side will put things.
  // A structural scope has no inheritance, so no declaration names another
  // scope's callable and the scope is the authority for its own identity space.
  // Which behavior of the promise each published subroutine answers, counted
  // out of the signature's own order and continuing where the members left off.
  // A subroutine the unit kept to itself answers none.
  std::unordered_map<std::string_view, std::size_t> published_at;
  promised_subroutines_.resize(hir_scope.published_callables.size());
  for (std::size_t at = 0; at < hir_scope.published_callables.size(); ++at) {
    published_at.emplace(hir_scope.published_callables[at], at);
  }
  base::IdAllocator<mir::CallableId> subroutine_ids;
  std::vector<DeclaredCallable> declared_subroutines;
  std::vector<CallableSignature> signatures;
  declared_subroutines.reserve(hir_scope.structural_subroutines.size());
  signatures.reserve(hir_scope.structural_subroutines.size());
  for (const auto& s : hir_scope.structural_subroutines) {
    const mir::CallableId body = subroutine_ids.Take();
    const auto published = published_at.find(s.name);
    std::optional<mir::VirtualDispatchRole> dispatch;
    if (published != published_at.end()) {
      const auto behavior = static_cast<std::uint32_t>(
          promised_members_.size() + published->second);
      dispatch = mir::VirtualDispatchRole{mir::OverridesIntraUnitSlot{
          .slot_owner = *promise_id_, .slot_id = mir::CallableId{behavior}}};
      promised_subroutines_[published->second] =
          PromisedSubroutine{.name = s.name, .body = body};
    }
    signatures.push_back(CallableSignature{.virtual_dispatch = dispatch});
    declared_subroutines.push_back(
        DeclaredCallable{
            .callable = body,
            .statics = BindBodyStatics(
                unit_lowerer, hir_scope.procedural_scopes,
                InstanceStorage{.shape = &shape}, s.body,
                SignatureBoundVars(s))});
  }
  shape.callable_signatures = {
      hir_scope.structural_subroutines.size(), std::move(signatures)};
  declared_subroutines_ = {
      hir_scope.structural_subroutines.size(), std::move(declared_subroutines)};

  std::vector<StaticVarBindings> process_statics;
  process_statics.reserve(hir_scope.processes.size());
  for (const hir::ProcessId id : hir_scope.processes.Ids()) {
    process_statics.push_back(BindBodyStatics(
        unit_lowerer, hir_scope.procedural_scopes,
        InstanceStorage{.shape = &shape}, hir_scope.processes.Get(id).body,
        {}));
  }
  process_static_bindings_ = {
      hir_scope.processes.size(), std::move(process_statics)};

  // The classes this scope declares settle their shapes against this one, and
  // before it is published: a class this scope replicates keeps its cells here,
  // as fields of the instance, so what it places has to land while the shape is
  // still open.
  class_lowerers_.reserve(hir_scope.declared_classes.size());
  for (const hir::ClassId hir_class : hir_scope.declared_classes) {
    const mir::ClassId declared = unit_lowerer.TranslateClass(hir_class);
    shape.declares.push_back(declared);
    class_lowerers_.emplace_back(
        unit_lowerer, hir_class, declared,
        unit_lowerer.ClassObjectType(hir_class),
        unit_lowerer.Hir().classes.Get(hir_class), this);
  }
  for (ClassDeclLowerer& class_lowerer : class_lowerers_) {
    if (auto r = class_lowerer.DeclareShape(&shape); !r) {
      return std::unexpected(std::move(r.error()));
    }
  }

  unit_lowerer.DefineClassShape(class_id_, std::move(shape));
  return class_id_;
}

namespace {

// Builds a runtime scope class's definition as an ordinary constructed value: a
// per-phase ABI adapter that downcasts the generic scope receiver to `cls` and
// forwards to the phase body (empty when the phase has none), wrapped in a
// ScopeProgram, wrapped in turn in the definition that adds the construct
// entry. Every scope class publishes the same record, so a site constructing an
// instance of one reads the definition the same way wherever the class came
// from. A class that is not a runtime tree node gets none.
//
// The adapters belong to `cls`, whose bodies they enter. The record belongs to
// `rooted`, which is the class standing in the runtime's tree and so the one
// entering it -- the same class where the source declares no promise, and the
// promise where it does. Answers with what `rooted`'s constructor hands its
// base, which is why that constructor's own code is what the record's address
// is formed in.
auto InstallGeneratedDefinition(
    mir::CompilationUnit& unit, mir::Class& cls, mir::ClassId cls_id,
    mir::ClassRef base, mir::Class& rooted, mir::CallableCode& rooted_ctor,
    mir::CallableId resolve_body, mir::CallableId init_body,
    mir::CallableId create_body) -> std::vector<mir::ExprId> {
  const auto make_adapter = [&](mir::CallableId body) -> mir::AbiAdapterId {
    mir::CallableCode code =
        BuildForwardingEntry(unit, cls, cls_id, body, unit.builtins.scope_ptr);
    return cls.abi_adapters.Add(
        mir::AbiAdapter{
            .code = std::move(code), .published = mir::UnpublishedEntry{}});
  };
  // What this class extends roots an object in the tree -- the runtime's scope,
  // or what its unit promised of this object, which is rooted there itself. The
  // three bodies are what this class supplies to be driven through either way.
  // The record below is a second reading of those bodies, for a target whose
  // runtime is entered through a function pointer; a target that reaches them
  // directly reads the statement and composes no record at all.
  cls.base = std::move(base);
  cls.tree_program = mir::ObjectTreeProgram{
      .resolve_state = resolve_body,
      .initialize_state = init_body,
      .create_processes = create_body};
  const mir::AbiAdapterId resolve_abi = make_adapter(resolve_body);
  const mir::AbiAdapterId init_abi = make_adapter(init_body);
  const mir::AbiAdapterId create_abi = make_adapter(create_body);

  // The names this scope answers, one constant per namespace: the table the
  // runtime holds is a pointer into contiguous storage, so the records must
  // outlive the definition that points at them rather than sit in its
  // initializer. A scope answering no name in a namespace contributes an empty
  // array, which the same construction covers.
  struct NameTable {
    mir::TypeId records_type;
    mir::StaticConstantId records;
    std::uint32_t count = 0;
  };
  const auto publish = [&](auto&& name_of) -> NameTable {
    mir::StaticConstantDecl decl;
    mir::RuntimeRecordBuilder records(unit, decl.body.exprs);
    std::vector<mir::ExprId> entries;
    for (const mir::AbiAdapterId adapter_id : cls.abi_adapters.Ids()) {
      const mir::AbiAdapter& adapter = cls.abi_adapters.Get(adapter_id);
      const std::optional<std::string> name = name_of(adapter.published);
      if (!name.has_value()) {
        continue;
      }
      entries.push_back(records.Construct(
          mir::RuntimeLibraryKind::kScopeCallable,
          {records.StringRef(*name),
           records.ErasedFunctionRef(cls_id, cls, adapter_id)}));
    }
    const auto count = static_cast<std::uint32_t>(entries.size());
    decl.value = records.MachineArray(
        records.Type(mir::RuntimeLibraryKind::kScopeCallable),
        std::move(entries));
    decl.type = records.TypeOf(decl.value);
    const mir::TypeId records_type = decl.type;
    return NameTable{
        .records_type = records_type,
        .records = rooted.static_constants.Add(std::move(decl)),
        .count = count};
  };
  const NameTable exports = publish(
      [](const mir::AbiAdapterPublication& p) -> std::optional<std::string> {
        const auto* linkage = std::get_if<mir::ForeignLinkage>(&p);
        return linkage == nullptr ? std::nullopt
                                  : std::optional{linkage->foreign_name};
      });
  const NameTable subroutines = publish(
      [](const mir::AbiAdapterPublication& p) -> std::optional<std::string> {
        const auto* entry = std::get_if<mir::SubroutineEntry>(&p);
        return entry == nullptr ? std::nullopt : std::optional{entry->name};
      });

  // The classes this scope declares (LRM 23.9), each under the name the source
  // gave it. A referrer outside has no name for such a class -- it is a type of
  // this scope's instance (LRM 6.22) -- so it walks to the scope and asks, and
  // what the scope answers with is the record every object of that class
  // carries.
  mir::StaticConstantDecl classes_decl;
  const auto class_count = static_cast<std::uint32_t>(cls.declares.size());
  {
    mir::RuntimeRecordBuilder records(unit, classes_decl.body.exprs);
    const mir::TypeId record_ptr = unit.types.Intern(
        mir::Type{mir::PointerType{
            .pointee = records.Type(mir::RuntimeLibraryKind::kObjectDefinition),
            .ownership = mir::PointerOwnership::kBorrowed,
            .mutability = mir::Mutability::kReadOnly}});
    std::vector<mir::ExprId> entries;
    entries.reserve(cls.declares.size());
    for (const mir::ClassId declared : cls.declares) {
      const mir::ExprId record = records.Add(
          mir::Expr{
              .data =
                  mir::ReferenceExpr{
                      .target =
                          mir::ObjectRecordRef{
                              .of =
                                  mir::IntraUnitClassRef{
                                      .class_id = declared}}},
              .type =
                  records.Type(mir::RuntimeLibraryKind::kObjectDefinition)});
      entries.push_back(records.Construct(
          mir::RuntimeLibraryKind::kScopeClass,
          {records.StringRef(*unit.GetClass(declared).name),
           records.Add(mir::MakeAddressOfExpr(record, record_ptr))}));
    }
    classes_decl.value = records.MachineArray(
        records.Type(mir::RuntimeLibraryKind::kScopeClass), std::move(entries));
    classes_decl.type = records.TypeOf(classes_decl.value);
  }
  const mir::TypeId classes_type = classes_decl.type;
  const mir::StaticConstantId classes_id =
      rooted.static_constants.Add(std::move(classes_decl));

  mir::StaticConstantDecl def;
  mir::RuntimeRecordBuilder definition(unit, def.body.exprs);
  const auto build_table = [&](const NameTable& table) -> mir::ExprId {
    const mir::ExprId records_ref = definition.Add(
        mir::Expr{
            .data =
                mir::ReferenceExpr{
                    .target =
                        mir::StaticConstantRef{.constant = table.records}},
            .type = table.records_type});
    const mir::ExprId data = definition.Add(
        mir::Expr{
            .data = mir::MachineArrayDataExpr{.array = records_ref},
            .type = unit.types.Intern(
                mir::Type{mir::PointerType{
                    .pointee = definition.Type(
                        mir::RuntimeLibraryKind::kScopeCallable),
                    .ownership = mir::PointerOwnership::kBorrowed,
                    .mutability = mir::Mutability::kReadOnly}})});
    return definition.Construct(
        mir::RuntimeLibraryKind::kScopeCallableTable,
        {data, definition.MachineInt(table.count)});
  };
  const mir::ExprId export_table = build_table(exports);
  const mir::ExprId subroutine_table = build_table(subroutines);

  const mir::ExprId metadata = definition.Construct(
      mir::RuntimeLibraryKind::kScopeMetadata,
      {definition.MachineInt(cls.time_resolution.unit_power),
       definition.MachineInt(cls.time_resolution.precision_power)});
  const mir::ExprId classes_ref = definition.Add(
      mir::Expr{
          .data =
              mir::ReferenceExpr{
                  .target = mir::StaticConstantRef{.constant = classes_id}},
          .type = classes_type});
  const mir::ExprId classes_data = definition.Add(
      mir::Expr{
          .data = mir::MachineArrayDataExpr{.array = classes_ref},
          .type = unit.types.Intern(
              mir::Type{mir::PointerType{
                  .pointee =
                      definition.Type(mir::RuntimeLibraryKind::kScopeClass),
                  .ownership = mir::PointerOwnership::kBorrowed,
                  .mutability = mir::Mutability::kReadOnly}})});
  const mir::ExprId class_table = definition.Construct(
      mir::RuntimeLibraryKind::kScopeClassTable,
      {classes_data, definition.MachineInt(class_count)});
  const mir::ExprId program = definition.Construct(
      mir::RuntimeLibraryKind::kScopeProgram,
      {metadata, definition.FunctionRef(cls_id, cls, resolve_abi),
       definition.FunctionRef(cls_id, cls, init_abi),
       definition.FunctionRef(cls_id, cls, create_abi), export_table,
       subroutine_table, class_table});
  def.value = definition.Construct(
      mir::RuntimeLibraryKind::kScopeDefinition, {program});
  def.type = definition.TypeOf(def.value);
  const mir::TypeId const_type = def.type;
  const mir::StaticConstantId def_id =
      rooted.static_constants.Add(std::move(def));

  // The constructor hands the base the address of the constant just installed.
  auto& cex = rooted_ctor.Body().exprs;
  const mir::ExprId ref = cex.Add(
      mir::Expr{
          .data =
              mir::ReferenceExpr{
                  .target = mir::StaticConstantRef{.constant = def_id}},
          .type = const_type});
  const mir::ExprId addr = cex.Add(
      mir::Expr{
          .data = mir::AddressOfExpr{.operand = ref},
          .type = unit.types.Intern(
              mir::Type{mir::PointerType{
                  .pointee = const_type,
                  .ownership = mir::PointerOwnership::kBorrowed,
                  .mutability = mir::Mutability::kReadOnly}})});
  return {addr};
}

// Settles the class's construction protocol: the constructor body, and the
// arguments its base is entered with -- each prefix forwarded as a consuming
// use, then the trailing ones. `ctor_code` arrives finalized, its params and
// result type set, and this is what installs it.
void FinalizeConstructor(
    mir::CompilationUnit& unit, mir::Class& cls, mir::CallableCode ctor_code,
    const std::vector<mir::LocalId>& prefix_local_ids,
    const std::vector<mir::ExprId>& base_trailing_args) {
  std::vector<mir::ExprId> base_args;
  if (cls.base.has_value()) {
    base_args.reserve(prefix_local_ids.size() + base_trailing_args.size());
    for (const mir::LocalId id : prefix_local_ids) {
      const mir::TypeId ty = ctor_code.locals.Get(id).type;
      const mir::ExprId local_ref =
          ctor_code.Body().exprs.Add(mir::MakeLocalRefExpr(id, ty));
      if (unit.types.Get(ty).IsAliasHandle()) {
        base_args.push_back(local_ref);
      } else {
        base_args.push_back(ctor_code.Body().exprs.Add(
            mir::Expr{
                .data = mir::MoveExpr{.operand = local_ref}, .type = ty}));
      }
    }
    for (const mir::ExprId e : base_trailing_args) {
      base_args.push_back(e);
    }
  }
  cls.constructor = mir::ConstructorDecl{
      .code = std::move(ctor_code), .base_args = std::move(base_args)};
}

}  // namespace

auto StructuralScopeLowerer::PopulateBodies(WalkFrame parent_frame)
    -> diag::Result<void> {
  UnitLowerer& unit_lowerer = *owner_;
  const hir::StructuralScope& hir_scope = *hir_scope_;

  const ClassShape& shape = unit_lowerer.GetClassShape(class_id_);
  mir::Class mir_class = shape.OpenClass();

  const mir::TypeId void_type = unit_lowerer.Unit().builtins.void_type;
  const mir::TypeId self_ptr_type = mir_class.self_pointer_type;
  ScopeChainNode outer_scope_link{};
  const auto seed_self = [&](CallableBindings& bindings) -> mir::LocalId {
    return bindings.Declare(BindingOriginId::Receiver(), self_ptr_type);
  };

  // Each lifecycle phase is a callable like any other: `self` is the receiver
  // binding seeded into its `locals`, and every nested block's `self` read
  // resolves through that one binding.
  mir::CallableCode ctor_code = mir::CallableCode::Defined();
  CallableBindings ctor_bindings(unit_lowerer.Unit(), ctor_code);
  const mir::LocalId self_id = seed_self(ctor_bindings);
  // Each prefix param the base contract demands lands as an ordinary local
  // after `self`, so a base call reads it as a plain LocalRef and the ctor
  // signature exposes it as a regular parameter.
  std::vector<mir::LocalId> ctor_prefix_local_ids;
  ctor_prefix_local_ids.reserve(shape.ctor_prefix_params.size());
  for (const mir::ParamId param : shape.ctor_prefix_params.Ids()) {
    const auto& p = shape.ctor_prefix_params.Get(param);
    ctor_prefix_local_ids.push_back(ctor_bindings.DeclareAnonymous(p.type));
  }
  // Every body of the scope is the instance its outward references count from.
  const WalkFrame scope_frame =
      parent_frame.WithClass(&mir_class, class_id_, outer_scope_link)
          .WithStructuralBase(ScopeIsSelf{});
  mir::Block& ctor_block = ctor_code.Body();
  const WalkFrame ctor_frame =
      scope_frame.WithBlock(&ctor_block).WithBindings(&ctor_bindings);

  mir::CallableCode initialize_code = mir::CallableCode::Defined();
  CallableBindings init_bindings(unit_lowerer.Unit(), initialize_code);
  const mir::LocalId init_self_id = seed_self(init_bindings);
  // Bringing a declaration up is two things, and they are separated because
  // they answer to different orders. Installing the declared representation
  // and default reads nothing but the declared type, so installs have no order
  // among themselves; running what the source assigned may read any other
  // declaration of this scope -- a handle assigned a new object whose
  // constructor names a static property, say -- so those keep the order the
  // source wrote and stand after every install.
  mir::Block& install_block = initialize_code.Body();
  const WalkFrame install_frame =
      scope_frame.WithBlock(&install_block).WithBindings(&init_bindings);

  mir::Block initialize_block;
  const WalkFrame init_frame =
      scope_frame.WithBlock(&initialize_block).WithBindings(&init_bindings);

  mir::CallableCode resolve_code = mir::CallableCode::Defined();
  CallableBindings resolve_bindings(unit_lowerer.Unit(), resolve_code);
  const mir::LocalId resolve_self_id = seed_self(resolve_bindings);
  mir::Block& resolve_block = resolve_code.Body();
  const WalkFrame resolve_frame =
      scope_frame.WithBlock(&resolve_block).WithBindings(&resolve_bindings);

  mir::CallableCode activate_code = mir::CallableCode::Defined();
  CallableBindings activate_bindings(unit_lowerer.Unit(), activate_code);
  const mir::LocalId activate_self_id = seed_self(activate_bindings);
  mir::Block& activate_block = activate_code.Body();
  const WalkFrame activate_frame =
      scope_frame.WithBlock(&activate_block).WithBindings(&activate_bindings);
  const auto self_read = [&]() -> mir::ExprId {
    return ctor_block.exprs.Add(MakeSelfRefExpr(ctor_frame, self_ptr_type));
  };
  const auto init_self_read = [&]() -> mir::ExprId {
    return initialize_block.exprs.Add(
        MakeSelfRefExpr(init_frame, self_ptr_type));
  };

  // What elaboration settles is read while the object is still being built -- a
  // block's own declarations and connections are written in terms of the index
  // it stands at, and the loop advances that index as it builds (LRM 27.4) --
  // so these cells exist from the constructor rather than from the initialize
  // phase every other declared value waits for.
  const auto install_in_constructor =
      [&](hir::StructuralDataObjectId id) -> mir::ExprId {
    const auto& d = hir_scope.structural_data_objects.Get(id);
    const mir::FieldId field =
        TranslateStructuralDataObject(hir::StructuralHops{0}, id);
    const mir::ExprId target = ctor_block.exprs.Add(
        mir::MakeFieldAccessExpr(
            self_read(),
            mir::ClassFieldTarget{.owner = class_id_, .slot = field},
            mir_class.fields.Get(field).type));
    ctor_block.AppendStmt(
        mir::ExprStmt{
            .expr = ctor_block.exprs.Add(
                mir::MakeCapabilityInstallCallExpr(
                    target,
                    ctor_block.exprs.Add(BuildDefaultValueFromHir(
                        unit_lowerer, ctor_block, d.type)),
                    support::BuiltinFn::kInitialize,
                    unit_lowerer.Unit().builtins.void_type))});
    return target;
  };
  for (const hir::StructuralDataObjectId id :
       hir_scope.structural_data_objects.Ids()) {
    if (std::holds_alternative<hir::StructuralGenvarDecl>(
            hir_scope.structural_data_objects.Get(id).kind)) {
      install_in_constructor(id);
    }
  }
  if (construction_value_.has_value()) {
    const auto& supplied =
        hir_scope.structural_data_objects.Get(*construction_value_);
    const mir::TypeId value_type = unit_lowerer.TranslateType(supplied.type);
    const mir::ExprId target = install_in_constructor(*construction_value_);
    ctor_block.AppendStmt(
        mir::ExprStmt{
            .expr = ctor_block.exprs.Add(BuildStoreExpr(
                unit_lowerer.Unit(), ctor_block,
                WriteTarget{.owner = target, .descent = {}},
                ctor_block.exprs.Add(
                    mir::MakeLocalRefExpr(
                        ctor_prefix_local_ids.back(), value_type)),
                std::nullopt, value_type))});
  }

  // A constant the scope settles for itself is settled after whatever
  // construction supplied is in its cell, because such a constant may be
  // written from it, and in declaration order, because one may be written from
  // an earlier one (LRM 6.20).
  for (const hir::StructuralDataObjectId id :
       hir_scope.structural_data_objects.Ids()) {
    const auto& settled_decl = hir_scope.structural_data_objects.Get(id);
    const auto* settled =
        std::get_if<hir::StructuralParameterDecl>(&settled_decl.kind);
    if (settled == nullptr) {
      continue;
    }
    const mir::TypeId settled_type =
        unit_lowerer.TranslateType(settled_decl.type);
    const mir::ExprId settled_target = install_in_constructor(id);
    auto value_or =
        LowerExpr(hir_scope.exprs.Get(settled->initializer), ctor_frame);
    if (!value_or) return std::unexpected(std::move(value_or.error()));
    ctor_block.AppendStmt(
        mir::ExprStmt{
            .expr = ctor_block.exprs.Add(BuildStoreExpr(
                unit_lowerer.Unit(), ctor_block,
                WriteTarget{.owner = settled_target, .descent = {}},
                ctor_block.exprs.Add(*std::move(value_or)), std::nullopt,
                settled_type))});
  }

  std::vector<mir::FieldId> data_object_fields;
  data_object_fields.reserve(hir_scope.structural_data_objects.size());
  for (const hir::StructuralDataObjectId hir_id :
       hir_scope.structural_data_objects.Ids()) {
    const auto& d = hir_scope.structural_data_objects.Get(hir_id);
    const mir::FieldId mir_id =
        TranslateStructuralDataObject(hir::StructuralHops{0}, hir_id);
    const mir::TypeId mir_field_type = mir_class.fields.Get(mir_id).type;
    const mir::TypeId mir_value_type = unit_lowerer.TranslateType(d.type);
    const auto* net = std::get_if<hir::StructuralNetDecl>(&d.kind);
    const auto* var = std::get_if<hir::StructuralVariableDecl>(&d.kind);
    const mir::Type& var_type = unit_lowerer.Unit().types.Get(mir_value_type);
    // Owned children (pointer / vector / object), cross-instance reference
    // slots (borrowed pointers filled in the resolve phase), and named events
    // have no "value assignment" -- their declaration shape itself fixes the
    // field at construction. A net takes none either: its value is produced by
    // its drivers, seeded when each driver updates in the initialize phase.
    // Value-typed variables (integral, string, real, unpacked / dynamic array)
    // receive an LRM 10.5 initialization statement, run in the initialize
    // phase after the tree's references resolve, not in the constructor.
    const bool is_assignable_value =
        var != nullptr && !var_type.Is<mir::PointerType>() &&
        !var_type.Is<mir::VectorType>() && !var_type.Is<mir::ObjectType>() &&
        !var_type.Is<mir::ExternalUnitObjectType>() &&
        !var_type.Is<mir::EventType>();
    if (is_assignable_value) {
      const mir::ExprId init_target = initialize_block.exprs.Add(
          mir::MakeFieldAccessExpr(
              init_self_read(),
              mir::ClassFieldTarget{.owner = class_id_, .slot = mir_id},
              mir_field_type));
      const auto append_stmt = [&](mir::Expr expr) {
        initialize_block.AppendStmt(
            mir::Stmt{
                .label = std::nullopt,
                .data = mir::ExprStmt{
                    .expr = initialize_block.exprs.Add(std::move(expr))}});
      };
      const auto emit_value_store = [&](mir::ExprId value_id) {
        append_stmt(BuildStoreExpr(
            unit_lowerer.Unit(), initialize_block,
            WriteTarget{.owner = init_target, .descent = {}}, value_id,
            std::nullopt, mir_value_type));
      };

      // Every observable value cell installs its declared representation and
      // default at construction (LRM 10.5), so its type is fixed by
      // construction and a later store -- including a user initializer -- is
      // verified against it rather than discovered from whichever store runs
      // first. A non-observable value member carries no cell wrapper, so it
      // installs its representation through an ordinary store of the default.
      if (unit_lowerer.Unit().types.Get(mir_field_type).IsCapabilityWrapper()) {
        const mir::ExprId install_target = install_block.exprs.Add(
            mir::MakeFieldAccessExpr(
                install_block.exprs.Add(
                    MakeSelfRefExpr(install_frame, self_ptr_type)),
                mir::ClassFieldTarget{.owner = class_id_, .slot = mir_id},
                mir_field_type));
        const mir::ExprId prototype = install_block.exprs.Add(
            BuildDefaultValueFromHir(unit_lowerer, install_block, d.type));
        install_block.AppendStmt(
            mir::ExprStmt{
                .expr = install_block.exprs.Add(
                    mir::MakeCapabilityInstallCallExpr(
                        install_target, prototype,
                        support::BuiltinFn::kInitialize,
                        unit_lowerer.Unit().builtins.void_type))});
        if (var->initializer.has_value()) {
          auto value_or =
              LowerExpr(hir_scope.exprs.Get(*var->initializer), init_frame);
          if (!value_or) return std::unexpected(std::move(value_or.error()));
          emit_value_store(initialize_block.exprs.Add(*std::move(value_or)));
        }
      } else {
        mir::ExprId value_id{};
        if (var->initializer.has_value()) {
          auto value_or =
              LowerExpr(hir_scope.exprs.Get(*var->initializer), init_frame);
          if (!value_or) return std::unexpected(std::move(value_or.error()));
          value_id = initialize_block.exprs.Add(*std::move(value_or));
        } else {
          value_id = initialize_block.exprs.Add(
              BuildDefaultValueFromHir(unit_lowerer, initialize_block, d.type));
        }
        emit_value_store(value_id);
      }
    }

    // A net cell fixes what its declaration gives it -- the declared type, and
    // what its net type states about how contributions resolve and what the net
    // shows where nothing drives them -- at construction (LRM 6.6.1, 6.7.1), in
    // the constructor rather than the initialize phase: a net is a readable,
    // well-typed observable before any driver attaches, and before a cross-unit
    // reader seeds from it during the parent-first initialize phase, so a read
    // that early sees what the net type contributes, never an uninitialized
    // cell. Drivers, attached at Resolve, update it from there.
    if (net != nullptr) {
      const mir::ExprId net_target = ctor_block.exprs.Add(
          mir::MakeFieldAccessExpr(
              self_read(),
              mir::ClassFieldTarget{.owner = class_id_, .slot = mir_id},
              mir_field_type));
      const mir::ExprId prototype = ctor_block.exprs.Add(
          BuildDefaultValueFromHir(unit_lowerer, ctor_block, d.type));
      const NetInstall install =
          BuildNetInstall(unit_lowerer.Unit(), ctor_block, *net);
      ctor_block.AppendStmt(
          mir::Stmt{
              .label = std::nullopt,
              .data = mir::ExprStmt{
                  .expr = ctor_block.exprs.Add(
                      mir::MakeNetInstallCallExpr(
                          net_target, prototype, install.fill, install.strength,
                          install.entry, void_type))}});
    }

    // A value signal, or a named event, records its address under its name so a
    // cross-unit referrer resolves it by name at construction. The excluded
    // members -- owned children and cross-unit reference slots -- are not
    // signals, and neither is a declaration nothing answers by name:
    // registering one would offer a name no reference can spell, and two loops
    // counting with the same genvar would offer it twice.
    const bool is_signal =
        hir::AnsweredByName(d) && !var_type.Is<mir::PointerType>() &&
        !var_type.Is<mir::VectorType>() && !var_type.Is<mir::ObjectType>() &&
        !var_type.Is<mir::ExternalUnitObjectType>();
    if (is_signal) {
      const mir::ExprId var_ref = ctor_block.exprs.Add(
          mir::MakeFieldAccessExpr(
              self_read(),
              mir::ClassFieldTarget{.owner = class_id_, .slot = mir_id},
              mir_field_type));
      const mir::TypeId var_ptr_type = unit_lowerer.Unit().types.Intern(
          mir::Type{mir::PointerType{
              .pointee = mir_field_type,
              .ownership = mir::PointerOwnership::kBorrowed}});
      const mir::ExprId addr_id =
          ctor_block.exprs.Add(mir::MakeAddressOfExpr(var_ref, var_ptr_type));
      const mir::ExprId name_id = ctor_block.exprs.Add(
          mir::Expr{
              .data = mir::StringLiteral{.value = d.name},
              .type = unit_lowerer.Unit().builtins.string});
      const mir::ExprId call = ctor_block.exprs.Add(
          mir::Expr{
              .data =
                  mir::CallExpr{
                      .callee =
                          mir::Direct{
                              .target = support::BuiltinFn::kRegisterSignal,
                              .receiver = self_read()},
                      .arguments = {name_id, addr_id}},
              .type = void_type});
      ctor_block.AppendStmt(mir::ExprStmt{.expr = call});
    }
  }

  // The design root's Initialize phase brings up what every namespace unit owns
  // (LRM 26.2 / 8.9 / 10.5). Such a unit owns no runtime tree node, so the root
  // calls its receiver-less callables here, before the top modules initialize
  // -- this scope's Initialize runs parent-first, and the design root is every
  // module's ancestor. It runs design-wide in two passes: install every unit's
  // cells (their declared type and default), then run every unit's value
  // initializers, so a value initializer that reads another unit's cell always
  // reaches installed storage. Which order the initializers run in is not
  // decided here: each entry claims its own bring-up and calls the namespaces
  // it reads, so calling all of them in any order runs each once and runs a
  // read namespace ahead of the one reading it. This scope carries the list
  // only for the design root, so a source unit's scope and a nested scope both
  // leave it empty.
  const auto call_namespace_unit = [&](const std::string& unit_name,
                                       mir::MintedEntry entry) {
    unit_lowerer.Unit().ConsumeNamespaceOf(unit_name);
    const mir::ExprId call = initialize_block.exprs.Add(
        mir::Expr{
            .data =
                mir::CallExpr{
                    .callee =
                        mir::Direct{
                            .target =
                                mir::ExternalUnitMintedEntryTarget{
                                    .unit_name = unit_name, .entry = entry}},
                    .arguments = {}},
            .type = void_type});
    initialize_block.AppendStmt(mir::ExprStmt{.expr = call});
  };
  for (const std::string& unit : namespaces_.units) {
    call_namespace_unit(unit, mir::MintedEntry::kInstallStorage);
  }
  for (const std::string& unit : namespaces_.units) {
    call_namespace_unit(unit, mir::MintedEntry::kInitializeStorage);
  }

  // Commit the class of every procedural scope's name node. A name node is
  // reached by name and answers with a name, so what it carries is what the
  // runtime scope base already gives it and its constructor takes only the
  // identity every scope is built with.
  for (const hir::ProceduralScopeId scope : scopes_.Ids()) {
    const ScopeNameNode& name_node = *scopes_.Get(scope).name_node;
    const ClassShape& node_shape =
        unit_lowerer.GetClassShape(name_node.class_id);
    mir::Class node_class;
    node_class.name = node_shape.name;
    node_class.is_final = node_shape.is_final;
    node_class.self_pointer_type = node_shape.self_pointer_type;
    node_class.time_resolution = node_shape.time_resolution;

    mir::CallableCode node_ctor_code = mir::CallableCode::Defined();
    CallableBindings node_ctor_bindings(unit_lowerer.Unit(), node_ctor_code);
    node_ctor_code.params.push_back(node_ctor_bindings.Declare(
        BindingOriginId::Receiver(), node_shape.self_pointer_type));
    std::vector<mir::LocalId> node_ctor_prefix_local_ids;
    node_ctor_prefix_local_ids.reserve(node_shape.ctor_prefix_params.size());
    for (const mir::ParamId param : node_shape.ctor_prefix_params.Ids()) {
      const auto& p = node_shape.ctor_prefix_params.Get(param);
      node_ctor_prefix_local_ids.push_back(
          node_ctor_bindings.DeclareAnonymous(p.type));
      node_ctor_code.params.push_back(node_ctor_prefix_local_ids.back());
    }
    node_ctor_code.result_type = void_type;
    // A name node has no work in any phase, which is three bodies with no
    // statements rather than three phases it does not have -- the same shape a
    // scope with work carries, so nothing downstream tells the two apart.
    const auto empty_phase = [&]() -> mir::CallableId {
      mir::CallableCode code = mir::CallableCode::Defined();
      CallableBindings bindings(unit_lowerer.Unit(), code);
      code.params = {bindings.Declare(
          BindingOriginId::Receiver(), node_shape.self_pointer_type)};
      code.result_type = void_type;
      return node_class.callables.Add(
          mir::CallableDecl{
              .code = std::move(code),
              .foreign = std::nullopt,
              .virtual_dispatch = std::nullopt});
    };
    const std::vector<mir::ExprId> node_base_trailing_args =
        InstallGeneratedDefinition(
            unit_lowerer.Unit(), node_class, name_node.class_id,
            mir::ClassRef{mir::RuntimeClassRef{
                .symbol = std::string{mir::kObjectTreeClassSymbol}}},
            node_class, node_ctor_code, empty_phase(), empty_phase(),
            empty_phase());
    FinalizeConstructor(
        unit_lowerer.Unit(), node_class, std::move(node_ctor_code),
        node_ctor_prefix_local_ids, node_base_trailing_args);
    unit_lowerer.Unit().DefineClass(name_node.class_id, std::move(node_class));
  }

  // What a `disable` naming a block or task terminates is a cell on this
  // object, placed by the rule that places every other piece of static-lifetime
  // state, while what a name reaches is the scope itself (LRM 9.6.2). So the
  // scope's node keeps the address, and a route that walked to that node asks
  // it for the target the way it asks for a static's cell.
  const mir::TypeId disable_target_ptr_type = unit_lowerer.Unit().types.Intern(
      mir::Type{mir::PointerType{
          .pointee = unit_lowerer.Unit().types.Intern(
              mir::Type{mir::RuntimeLibraryType{
                  .kind = mir::RuntimeLibraryKind::kCancellationTarget}}),
          .ownership = mir::PointerOwnership::kBorrowed}});

  // Build the whole name tree here, in this scope's own constructor: each node
  // hangs under the node of the scope around it, which is what the source
  // nesting means, while the borrowed handle to it lands on this class -- so
  // the objects nest and every one of them is still one step from a body.
  // Construction precedes every resolve, so the names registered below are in
  // place before anything asks for one.
  //
  // What each node answers for registers against the handle the construction
  // fills, so a node registers exactly when it is built and which scopes stand
  // at run time is one answer rather than two. A scope declared without being
  // built therefore registers nothing.
  const auto build_name_tree =
      [&](const auto& self_ref, hir::ProceduralScopeId scope_id,
          std::optional<mir::FieldId> parent_handle) -> void {
    const auto& scope = hir_scope.procedural_scopes.Get(scope_id);
    const DeclaredScope& declared = scopes_.Get(scope_id);
    const ScopeNameNode& name_node = *declared.name_node;
    AppendOwnedChildConstruction(
        unit_lowerer, ctor_frame, parent_handle, scope.source_name.value_or(""),
        name_node.class_id, std::nullopt, name_node.borrowed_handle,
        std::nullopt);
    if (declared.disable_target.has_value()) {
      const mir::FieldId field = DisableTargetField(scope_id);
      const mir::ExprId cell = ctor_block.exprs.Add(
          mir::MakeFieldAccessExpr(
              self_read(),
              mir::ClassFieldTarget{.owner = class_id_, .slot = field},
              mir_class.fields.Get(field).type));
      const mir::ExprId addr = ctor_block.exprs.Add(
          mir::MakeAddressOfExpr(cell, disable_target_ptr_type));
      const mir::ExprId node = ctor_block.exprs.Add(
          mir::MakeFieldAccessExpr(
              self_read(),
              mir::ClassFieldTarget{
                  .owner = class_id_, .slot = name_node.borrowed_handle},
              mir_class.fields.Get(name_node.borrowed_handle).type));
      ctor_block.AppendStmt(
          mir::ExprStmt{
              .expr = ctor_block.exprs.Add(
                  mir::Expr{
                      .data =
                          mir::CallExpr{
                              .callee =
                                  mir::Direct{
                                      .target = support::BuiltinFn::
                                          kRegisterDisableTarget,
                                      .receiver = node},
                              .arguments = {addr}},
                      .type = void_type})});
    }
    for (const hir::ProceduralScopeId child : scope.child_scopes) {
      self_ref(self_ref, child, name_node.borrowed_handle);
    }
  };
  for (const auto& s : hir_scope.structural_subroutines) {
    build_name_tree(build_name_tree, s.body.root_scope, std::nullopt);
  }
  for (const auto& p : hir_scope.processes) {
    build_name_tree(build_name_tree, p.body.root_scope, std::nullopt);
  }

  // A static-lifetime local is a cell on this object, but the name reaching it
  // belongs to the block that wrote it: LRM 6.21 lets a hierarchical reference
  // name any static variable except one declared inside an unnamed block. So it
  // registers under its source spelling on that block's node, and a descent
  // (`Top.outer.x`, intra- or cross-unit) walks the object tree to that node by
  // name and asks it for the cell's address.
  const auto register_named_statics = [&](const StaticVarBindings& statics,
                                          const hir::ProceduralBody& body) {
    for (const StaticVarBinding& binding : statics) {
      const auto& scope = hir_scope.procedural_scopes.Get(binding.scope);
      if (!scope.source_name.has_value()) continue;
      const mir::FieldId field = InstanceFieldOf(binding);
      const mir::ExprId cell = ctor_block.exprs.Add(
          mir::MakeFieldAccessExpr(
              self_read(),
              mir::ClassFieldTarget{.owner = class_id_, .slot = field},
              binding.cell_type));
      const mir::ExprId addr = ctor_block.exprs.Add(
          mir::MakeAddressOfExpr(
              cell, unit_lowerer.Unit().types.Intern(
                        mir::Type{mir::PointerType{
                            .pointee = binding.cell_type,
                            .ownership = mir::PointerOwnership::kBorrowed}})));
      const mir::FieldId borrowed_handle =
          scopes_.Get(binding.scope).name_node->borrowed_handle;
      const mir::ExprId node = ctor_block.exprs.Add(
          mir::MakeFieldAccessExpr(
              self_read(),
              mir::ClassFieldTarget{
                  .owner = class_id_, .slot = borrowed_handle},
              mir_class.fields.Get(borrowed_handle).type));
      // What registers is the spelling the source wrote, so a variable the
      // front end introduced has nothing to register under -- and none can
      // reach here, since a hierarchical name reaches static storage and every
      // such variable is automatic.
      const std::optional<std::string>& declared_as =
          body.procedural_vars.Get(binding.var).name;
      if (!declared_as.has_value()) {
        throw InternalError(
            "register_named_statics: a variable the source never declared "
            "took static storage a hierarchical name can reach");
      }
      const mir::ExprId name_lit = ctor_block.exprs.Add(
          mir::Expr{
              .data = mir::StringLiteral{.value = *declared_as},
              .type = unit_lowerer.Unit().builtins.string});
      ctor_block.AppendStmt(
          mir::ExprStmt{
              .expr = ctor_block.exprs.Add(
                  mir::Expr{
                      .data =
                          mir::CallExpr{
                              .callee =
                                  mir::Direct{
                                      .target =
                                          support::BuiltinFn::kRegisterSignal,
                                      .receiver = node},
                              .arguments = {name_lit, addr}},
                      .type = void_type})});
    }
  };
  for (const hir::StructuralSubroutineId id :
       hir_scope.structural_subroutines.Ids()) {
    register_named_statics(
        declared_subroutines_.Get(id).statics,
        hir_scope.structural_subroutines.Get(id).body);
  }
  for (const hir::ProcessId id : hir_scope.processes.Ids()) {
    register_named_statics(
        process_static_bindings_.Get(id), hir_scope.processes.Get(id).body);
  }

  // The callable each subroutine lowered to, recorded where it is created so an
  // export below names its own by identity, indexed by that subroutine's id.
  std::vector<mir::CallableId> subroutine_callables;
  subroutine_callables.reserve(hir_scope.structural_subroutines.size());
  for (const hir::StructuralSubroutineId sub_id :
       hir_scope.structural_subroutines.Ids()) {
    const auto& src = hir_scope.structural_subroutines.Get(sub_id);
    const DeclaredCallable& declared = declared_subroutines_.Get(sub_id);
    // A subroutine the source declared is what a hierarchical name and a call
    // from another unit both spell, so the scope's class records the name.
    mir_class.named_callables.push_back(
        mir::NamedCallable{.name = src.name, .body = declared.callable});
    ProcessLowerer subroutine_lowerer(
        unit_lowerer, this, hir_scope.time_resolution, src.body, src.root_stmt,
        ctor_frame, scopes_, declared.statics);
    auto code_or = subroutine_lowerer.Run(src);
    if (!code_or) return std::unexpected(std::move(code_or.error()));
    // Which behavior of the promise this body answers was settled where the
    // shape was declared, because that is where the promise's own order is; a
    // subroutine the unit kept to itself answers none.
    mir_class.callables.Define(
        declared.callable,
        mir::CallableDecl{
            .code = *std::move(code_or),
            .foreign = std::nullopt,
            .virtual_dispatch = shape.callable_signatures.Get(declared.callable)
                                    .virtual_dispatch});
    subroutine_callables.push_back(declared.callable);
    for (const StaticVarBinding& binding : declared.statics) {
      auto integ = IntegrateStaticInitializer(
          subroutine_lowerer, src.body,
          StorageBringUp{.install = install_frame, .value = init_frame},
          binding);
      if (!integ) return std::unexpected(std::move(integ.error()));
    }
  }

  // An exported subroutine's C entry point calls that method on the receiver
  // recovered from the current DPI scope (LRM 35.5.3) -- the instance the
  // foreign call chain targets, which svSetScope may have redirected -- and the
  // unit owns the entry point, since a DPI-C name is program-global and never a
  // class member (LRM 35.4, 35.7).
  for (const hir::ForeignExportDecl& export_decl : hir_scope.foreign_exports) {
    const mir::CallableId method_id =
        subroutine_callables[export_decl.subroutine.value];
    const mir::TypeId method_result_type =
        mir_class.callables.Get(method_id).code.result_type;
    // The subroutine is compiled once per specialization of this scope while
    // the DPI-C name is one program-global symbol, so the scope publishes the
    // entry and the symbol resolves against whichever scope the foreign call
    // chain established. The entry takes the scope receiver, so it is the same
    // species as a lifecycle entry, not a callable the unit's namespace owns.
    ForeignExportEntry entry = SynthesizeForeignExportEntry(
        unit_lowerer, ctor_frame,
        mir::CallableTarget{.owner = class_id_, .slot = method_id},
        method_result_type, export_decl);
    // Two scopes of one unit may export one name (LRM 35.4), and what the unit
    // states of that name is the same either way, so it is stated once. Each
    // scope still publishes an entry of its own, because which subroutine the
    // symbol reaches is the scope's and only the name is the unit's.
    PublishForeignScopeName(
        unit_lowerer.Unit(), entry.linkage, entry.signature);
    mir_class.abi_adapters.Add(
        mir::AbiAdapter{
            .code = std::move(entry.code),
            .published = std::move(entry.linkage)});
  }

  // Every subroutine this scope declares answers to its own SV name, so a
  // hierarchical enable that reaches the instance finds it there (LRM 23.6).
  // The declaring unit cannot know which of them anyone will name -- a module
  // promises its parameters and ports and nothing else -- so it publishes them
  // all rather than the ones some referrer happened to compile against.
  for (const hir::StructuralSubroutineId sub_id :
       hir_scope.structural_subroutines.Ids()) {
    const std::string& name = hir_scope.structural_subroutines.Get(sub_id).name;
    const mir::CallableId method_id = subroutine_callables[sub_id.value];
    mir_class.abi_adapters.Add(
        mir::AbiAdapter{
            .code = BuildForwardingEntry(
                unit_lowerer.Unit(), mir_class, class_id_, method_id,
                unit_lowerer.Unit().builtins.scope_ptr),
            .published = mir::SubroutineEntry{.name = name}});
  }

  for (const hir::ProcessId id : hir_scope.processes.Ids()) {
    const auto& p = hir_scope.processes.Get(id);
    const StaticVarBindings& statics = process_static_bindings_.Get(id);
    ProcessLowerer process_lowerer(
        unit_lowerer, this, hir_scope.time_resolution, p.body, p.root_stmt,
        ctor_frame, scopes_, statics);
    auto code_or = process_lowerer.Run(p);
    if (!code_or) return std::unexpected(std::move(code_or.error()));
    const mir::CallableId body = mir_class.callables.Add(
        mir::CallableDecl{
            .code = *std::move(code_or),
            .foreign = std::nullopt,
            .virtual_dispatch = std::nullopt});
    AppendProcessRegistration(
        unit_lowerer, activate_frame, body, p.kind == hir::ProcessKind::kFinal);
    for (const StaticVarBinding& binding : statics) {
      auto integ = IntegrateStaticInitializer(
          process_lowerer, p.body,
          StorageBringUp{.install = install_frame, .value = init_frame},
          binding);
      if (!integ) return std::unexpected(std::move(integ.error()));
    }
  }

  // Fill every slot first in the resolve phase, so a later resolve-phase
  // consumer that reaches a target through one -- a continuous-assign driver
  // attached to a cross-unit net, a port-cell connection -- dereferences a slot
  // that is already bound.
  InstallScopeRoutes(*this, resolve_frame);

  for (const hir::ContinuousAssignId id : hir_scope.continuous_assigns.Ids()) {
    auto method_or = LowerContinuousAssign(
        *this, ctor_frame, resolve_frame, init_frame,
        hir_scope.continuous_assigns.Get(id));
    if (!method_or) return std::unexpected(std::move(method_or.error()));
    const mir::CallableId body = mir_class.callables.Add(std::move(*method_or));
    AppendProcessRegistration(unit_lowerer, activate_frame, body, false);
  }

  // One sampler per history, not one per clocking event. Two histories under
  // one event could share the wait, but deciding that two clocks are the same
  // event means comparing lowered expressions for equality -- and a wrong
  // answer there records one expression's ticks against another's clock, which
  // no test would obviously catch. What sharing would save is a wait.
  for (const hir::SampledHistoryId id : hir_scope.sampled_histories.Ids()) {
    auto sampler_or = LowerSampledHistorySampler(
        *this, ctor_frame, id, hir_scope.sampled_histories.Get(id));
    if (!sampler_or) return std::unexpected(std::move(sampler_or.error()));
    const mir::CallableId body =
        mir_class.callables.Add(std::move(*sampler_or));
    AppendProcessRegistration(unit_lowerer, activate_frame, body, false);
  }

  // The classes this scope declares, lowered against it: their bodies reach
  // this scope's declarations the way a process of it does, and the frame they
  // stand on is this scope's own.
  for (ClassDeclLowerer& class_lowerer : class_lowerers_) {
    auto class_r = class_lowerer.PopulateBodies(
        ctor_frame,
        StorageBringUp{.install = install_frame, .value = init_frame});
    if (!class_r) return std::unexpected(std::move(class_r.error()));
  }

  // One process per assertion and clocking event, for the same reason a
  // sampler is one per history: two assertions under one event could share the
  // wait, and deciding that two clocks are the same event means comparing
  // lowered expressions for equality.
  std::vector<
      std::pair<hir::ConcurrentAssertionId, InstalledConcurrentAssertion>>
      installed_assertions;
  installed_assertions.reserve(hir_scope.concurrent_assertions.size());
  for (const hir::ConcurrentAssertionId id :
       hir_scope.concurrent_assertions.Ids()) {
    auto installed = LowerConcurrentAssertion(
        *this, mir_class, ctor_frame, scopes_, id,
        hir_scope.concurrent_assertions.Get(id));
    if (!installed) return std::unexpected(std::move(installed.error()));
    for (const mir::CallableId process : installed->processes) {
      AppendProcessRegistration(unit_lowerer, activate_frame, process, false);
    }
    installed_assertions.emplace_back(id, *installed);
  }

  // Recurse into descendants. Every class's shape is already published, so a
  // body that names a peer's member resolves through the existing identity
  // model regardless of which sibling lowers next.
  for (auto& child : children_) {
    auto child_r = child->PopulateBodies(ctor_frame);
    if (!child_r) return std::unexpected(std::move(child_r.error()));
  }

  for (const hir::GenerateId gen : hir_scope.generates.Ids()) {
    auto stmt = LowerGenerateAsStmt(
        *this, ctor_frame, hir_scope.generates.Get(gen),
        generate_bindings_.Get(gen));
    if (!stmt) return std::unexpected(std::move(stmt.error()));
    ctor_block.AppendStmt(*std::move(stmt));
  }

  EmitInstanceMemberConstruction(*this, ctor_frame);
  auto port_conn_r = InstallPortConnections(
      *this, ctor_frame, resolve_frame, init_frame, activate_frame);
  if (!port_conn_r) return std::unexpected(std::move(port_conn_r.error()));
  auto net_join_r = InstallNetJoins(*this, resolve_frame);
  if (!net_join_r) return std::unexpected(std::move(net_join_r.error()));

  // A cell answers for a sampled value only once armed, and what arming
  // installs is the value every read answers with until a later time slot
  // first changes the cell -- for a static variable, the value its declaration
  // assigns (LRM 16.5.1). That value is in the cell once every initializer in
  // the design has run, which is what puts this here rather than beside the
  // initializer itself: a cell reached across an instance boundary is one this
  // scope cannot order itself against.
  for (const hir::SensitivityEntry& sampled : hir_scope.sampled_cells) {
    const mir::ExprId cell = BuildObservableCellExpr(
        activate_block, activate_frame, unit_lowerer.Unit(), *this, sampled);
    activate_block.AppendStmt(
        mir::ExprStmt{
            .expr = activate_block.exprs.Add(
                mir::MakeCellArmSamplingCallExpr(cell, void_type))});
  }

  // Every history is filled here, once its subject's cells are armed above.
  // What the subject evaluates to now is the expression's default sampled value
  // (LRM 16.5.1), which is the answer the standard requires of a read that
  // reaches further back than the ticks that have happened -- so a history that
  // starts full has no empty case and keeps no count of them.
  for (const hir::SampledHistoryId id : hir_scope.sampled_histories.Ids()) {
    const hir::SampledHistoryDecl& history =
        hir_scope.sampled_histories.Get(id);
    auto subject_or = LowerExpr(
        hir_scope.exprs.Get(history.subject),
        activate_frame.WithReadsAsOf(ReadsAsOf::kPreponed));
    if (!subject_or) return std::unexpected(std::move(subject_or.error()));
    const mir::ExprId value = activate_block.exprs.Add(*std::move(subject_or));
    auto depth_or =
        LowerExpr(hir_scope.exprs.Get(history.depth), activate_frame);
    if (!depth_or) return std::unexpected(std::move(depth_or.error()));
    const mir::ExprId depth = activate_block.exprs.Add(*std::move(depth_or));
    activate_block.AppendStmt(
        mir::ExprStmt{
            .expr = activate_block.exprs.Add(
                mir::MakeSampledHistoryInstallCallExpr(
                    BuildSampledHistoryExpr(
                        activate_block, activate_frame, *this, id),
                    value, depth, void_type))});
  }

  // Every assertion's storage is filled here too. Nothing about one needs an
  // earlier phase -- what it reads it reaches through cells sealed since Seal
  // -- and the sampling those cells answer from is armed just above.
  for (const auto& [id, installed] : installed_assertions) {
    AppendConcurrentAssertionInstall(*this, activate_frame, id, installed);
  }

  ctor_code.params.clear();
  ctor_code.params.reserve(1 + ctor_prefix_local_ids.size());
  ctor_code.params.push_back(self_id);
  for (const mir::LocalId id : ctor_prefix_local_ids) {
    ctor_code.params.push_back(id);
  }
  ctor_code.result_type = void_type;
  // Ctor code stays local so subsequent lowering can still append exprs into
  // its body; once complete, it is moved into the class's method storage and
  // referenced by the construction protocol.

  auto& unit = unit_lowerer.Unit();

  // The three bodies the runtime drives this scope through after construction
  // (LRM 23.3.3.2 / 6.8 / 9.2). `self` is the body's own receiver, typed as
  // this class. Which body is which is said once, where the class states that
  // it stands on the object tree; none of them answers to a name, because a
  // name would sit in the same name space as the scope's own subroutines and a
  // method spelled the same way would then share it.
  // A phase with no work is a body with no statements, not an absent body:
  // whether emitting one is worth avoiding is a question for the layer that
  // removes dead work, and answering it here would cost every consumer a case.
  const auto add_body = [&](mir::CallableCode& code,
                            mir::LocalId self) -> mir::CallableId {
    code.params = {self};
    code.result_type = void_type;
    return mir_class.callables.Add(
        mir::CallableDecl{
            .code = std::move(code),
            .foreign = std::nullopt,
            .virtual_dispatch = std::nullopt});
  };
  const mir::CallableId resolve_body = add_body(resolve_code, resolve_self_id);
  install_block.AppendStmt(
      mir::BlockStmt{
          .scope =
              install_block.child_scopes.Add(std::move(initialize_block))});
  WrapInScopeStaticInitExtent(unit_lowerer, install_frame, initialize_code);
  const mir::CallableId init_body = add_body(initialize_code, init_self_id);
  const mir::CallableId create_body = add_body(activate_code, activate_self_id);

  // A scope that is its unit's object stands in the tree through what the unit
  // promised of it; every other scope stands there directly. Whichever of the
  // two it is is the class the runtime's record belongs to, because that class
  // is the one entering the tree.
  std::optional<BuiltPromise> promise;
  if (promise_id_.has_value()) {
    promise = BuildPromise(
        unit, *promise_id_, *name_, promised_members_, promised_subroutines_,
        mir_class);
  }
  const mir::ClassRef base =
      promise.has_value()
          ? mir::ClassRef{mir::IntraUnitClassRef{.class_id = *promise_id_}}
          : mir::ClassRef{mir::RuntimeClassRef{
                .symbol = std::string{mir::kObjectTreeClassSymbol}}};
  // The base is entered with what its own contract demands and nothing else, so
  // a value this scope alone is supplied stays out of that call while still
  // standing on the constructor's own signature.
  std::vector<mir::LocalId> base_prefix_local_ids = ctor_prefix_local_ids;
  if (construction_value_.has_value()) {
    base_prefix_local_ids.pop_back();
  }
  const ClassUnderConstruction rooted =
      promise.has_value()
          ? ClassUnderConstruction{
                .cls = &promise->cls,
                .ctor = &promise->ctor,
                .prefix = &promise->ctor_prefix}
          : ClassUnderConstruction{
                .cls = &mir_class,
                .ctor = &ctor_code,
                .prefix = &base_prefix_local_ids};
  const std::vector<mir::ExprId> record_args = InstallGeneratedDefinition(
      unit, mir_class, class_id_, base, *rooted.cls, *rooted.ctor, resolve_body,
      init_body, create_body);
  FinalizeConstructor(
      unit, *rooted.cls, std::move(*rooted.ctor), *rooted.prefix, record_args);

  // Where the two are different classes, the one below enters its base with
  // nothing: the record is the tree's and the class standing there took it.
  std::vector<PromisedAccessor> accessors;
  if (promise.has_value()) {
    accessors = std::move(promise->accessors);
    FinalizeConstructor(
        unit, mir_class, std::move(ctor_code), base_prefix_local_ids, {});
    unit.DefineClass(*promise_id_, std::move(promise->cls));
  }

  // Each behavior the promise stated is taken over here, answering with the
  // storage of the member it was stated for. Nothing names one -- a referrer
  // reaches it through the promise -- so it takes its identity where it is
  // built rather than being reserved with the bodies a peer may call.
  for (const PromisedAccessor& accessor : accessors) {
    const mir::FieldId slot = accessor.cell;
    mir::CallableCode code = mir::CallableCode::Defined();
    const mir::LocalId self = code.AddLocal(self_ptr_type);
    code.params = {self};
    const mir::TypeId cell_type = mir_class.fields.Get(slot).type;
    code.result_type = unit.types.Intern(
        mir::Type{mir::PointerType{
            .pointee = cell_type,
            .ownership = mir::PointerOwnership::kBorrowed}});
    mir::Block& body = code.Body();
    const mir::ExprId member = body.exprs.Add(
        mir::MakeFieldAccessExpr(
            body.exprs.Add(mir::MakeLocalRefExpr(self, self_ptr_type)),
            mir::ClassFieldTarget{.owner = class_id_, .slot = slot},
            cell_type));
    body.AppendStmt(
        mir::ReturnStmt{
            .value = body.exprs.Add(
                mir::Expr{
                    .data = mir::AddressOfExpr{.operand = member},
                    .type = code.result_type})});
    mir_class.callables.Add(
        mir::CallableDecl{
            .code = std::move(code),
            .foreign = std::nullopt,
            .virtual_dispatch =
                mir::VirtualDispatchRole{mir::OverridesIntraUnitSlot{
                    .slot_owner = *promise_id_,
                    .slot_id = accessor.behavior}}});
  }

  unit.DefineClass(class_id_, std::move(mir_class));
  return {};
}

}  // namespace lyra::lowering::hir_to_mir
