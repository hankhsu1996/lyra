#include "lyra/lowering/hir_to_mir/structural_scope_lowerer.hpp"

#include <algorithm>
#include <array>
#include <cstddef>
#include <cstdint>
#include <expected>
#include <format>
#include <functional>
#include <map>
#include <memory>
#include <optional>
#include <ranges>
#include <span>
#include <string>
#include <string_view>
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
#include "lyra/lowering/hir_to_mir/access_path.hpp"
#include "lyra/lowering/hir_to_mir/binding_origin.hpp"
#include "lyra/lowering/hir_to_mir/block_builder.hpp"
#include "lyra/lowering/hir_to_mir/callable_bindings.hpp"
#include "lyra/lowering/hir_to_mir/cast_lowering.hpp"
#include "lyra/lowering/hir_to_mir/class_definition.hpp"
#include "lyra/lowering/hir_to_mir/class_shape.hpp"
#include "lyra/lowering/hir_to_mir/concurrent_assertion.hpp"
#include "lyra/lowering/hir_to_mir/condition.hpp"
#include "lyra/lowering/hir_to_mir/continuous_assign.hpp"
#include "lyra/lowering/hir_to_mir/declaration_initializer.hpp"
#include "lyra/lowering/hir_to_mir/default_value.hpp"
#include "lyra/lowering/hir_to_mir/design_namespaces.hpp"
#include "lyra/lowering/hir_to_mir/expression/dpi_call.hpp"
#include "lyra/lowering/hir_to_mir/integral_literal.hpp"
#include "lyra/lowering/hir_to_mir/library_entry_body.hpp"
#include "lyra/lowering/hir_to_mir/net_declaration.hpp"
#include "lyra/lowering/hir_to_mir/process_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/runtime_call.hpp"
#include "lyra/lowering/hir_to_mir/sampled_history.hpp"
#include "lyra/lowering/hir_to_mir/select_position.hpp"
#include "lyra/lowering/hir_to_mir/self_ref.hpp"
#include "lyra/lowering/hir_to_mir/sensitivity_wait.hpp"
#include "lyra/lowering/hir_to_mir/snapshot_local.hpp"
#include "lyra/lowering/hir_to_mir/statement/loops.hpp"
#include "lyra/lowering/hir_to_mir/static_var_binding.hpp"
#include "lyra/lowering/hir_to_mir/struct_methods.hpp"
#include "lyra/lowering/hir_to_mir/unit_object_access.hpp"
#include "lyra/lowering/hir_to_mir/walk_frame.hpp"
#include "lyra/mir/class.hpp"
#include "lyra/mir/class_ref.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/field.hpp"
#include "lyra/mir/local.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/mir/type.hpp"
#include "lyra/mir/type_builders.hpp"
#include "lyra/support/builtin_fn.hpp"
#include "lyra/support/runtime_class.hpp"
#include "lyra/support/strength_level.hpp"

namespace lyra::lowering::hir_to_mir {

namespace {

// Adds what every scope is built against -- its parent and its hierarchy
// segment -- as ordinary ctor params, in the order the base constructor
// consumes them. The base also takes the definition of the class the instance
// is of, which comes after these: a class names its own definition, and what a
// unit published of its object is handed it by the class realizing it.
void AttachRuntimeScopeCtorPrefix(
    const mir::CompilationUnit& unit, ClassShape& shape) {
  const auto& builtins = unit.builtins;
  shape.ctor_prefix_params.Add(mir::ParamDecl{.type = builtins.scope_ptr});
  shape.ctor_prefix_params.Add(
      mir::ParamDecl{.type = builtins.hierarchy_segment});
}

// The declarations of the values whoever constructs the scope supplies, in the
// order they are supplied: the index a loop generate builds a block at (LRM
// 27.4), or each parameter a unit's instance is handed (LRM 23.10.2).
auto ConstructionValuesOf(const hir::StructuralScope& scope)
    -> std::vector<hir::StructuralDataObjectId> {
  std::vector<hir::StructuralDataObjectId> supplied;
  for (const hir::StructuralDataObjectId id :
       scope.structural_data_objects.Ids()) {
    if (std::holds_alternative<hir::StructuralConstructionValueDecl>(
            scope.structural_data_objects.Get(id).kind)) {
      supplied.push_back(id);
    }
  }
  return supplied;
}

// What a unit published of its object, before its construction protocol is
// settled: the class under construction, and its constructor.
struct BuiltPublishedClass {
  mir::Class cls;
  mir::CallableCode ctor;
  std::vector<mir::LocalId> ctor_prefix;
};

// Builds what a unit published of its object from the shape settled for it --
// the published members, its first fields -- over the base that roots an
// object in the runtime's tree, with a method per published subroutine that
// enters, on the object, the body of whichever of `realizations` the object
// is. Nothing is virtual: the class a name reaches is settled where a referrer
// compiles, so a call names its method outright, and which class realizes the
// object is a question only this unit can ask.
//
// It is this class rather than a realization that stands directly in the
// tree, so what the tree's class is entered with is entered here, and that
// includes the definition the object carries. The object is of a realizing
// class, which is the only one that knows it, so its constructor hands the
// definition in and this one passes it on.
auto BuildPublishedClass(
    const mir::CompilationUnit& unit, const ClassShape& shape,
    const PublishedScope& published) -> BuiltPublishedClass {
  mir::CallableCode ctor = mir::CallableCode::Defined();
  const mir::LocalId ctor_self = ctor.AddLocal(shape.self_pointer_type);
  std::vector<mir::LocalId> prefix;
  for (const mir::TypeId type :
       {unit.builtins.scope_ptr, unit.builtins.hierarchy_segment,
        mir::ClassDefinitionPointer(unit.types)}) {
    prefix.push_back(ctor.AddLocal(type));
  }
  ctor.params = {ctor_self};
  ctor.params.insert(ctor.params.end(), prefix.begin(), prefix.end());
  ctor.receiver = ctor_self;
  ctor.result_type = unit.builtins.void_type;

  // The shape reserved one method per published subroutine, each where the
  // subroutine's published position says, and every realization states its
  // body for that position.
  mir::Class cls = shape.OpenClass();
  for (std::uint32_t at = 0; at < published.subroutine_names.size(); ++at) {
    const hir::PublishedCallableId subroutine{at};
    std::vector<mir::CallableTarget> bodies;
    bodies.reserve(published.realizations.size());
    for (const ScopeRealization& realization : published.realizations) {
      bodies.push_back(
          mir::CallableTarget{
              .owner = realization.id,
              .slot = realization.subroutines.Get(subroutine)});
    }
    const mir::CallableId id = PublishedMethodOf(subroutine);
    cls.callables.Define(
        id, mir::CallableDecl{
                .code = ForwardingMethod(unit, bodies, shape.self_pointer_type),
                .foreign = std::nullopt,
                .virtual_dispatch = std::nullopt});
    cls.named_callables.push_back(
        mir::NamedCallable{.name = published.subroutine_names[at], .body = id});
  }

  return BuiltPublishedClass{
      .cls = std::move(cls),
      .ctor = std::move(ctor),
      .ctor_prefix = std::move(prefix)};
}

auto MakeUniqueObjectPointer(UnitLowerer& unit_lowerer, mir::ClassId class_id)
    -> mir::TypeId {
  const mir::TypeId object_type = unit_lowerer.Unit().types.Intern(
      mir::Type{mir::ObjectType{.of = mir::IntraUnitClassRef{class_id}}});
  return unit_lowerer.Unit().types.Intern(
      mir::Type{mir::PointerType{
          .pointee = object_type,
          .ownership = mir::PointerOwnership::kUnique}});
}

// The pointer type a handle to an object built as `alternative` has. The object
// is one the declaring unit publishes, so the type names that unit's class.
auto MakeExternalUnitPointer(
    UnitLowerer& unit_lowerer, const hir::InstanceAlternative& alternative,
    mir::PointerOwnership ownership) -> mir::TypeId {
  const mir::TypeId object_type =
      unit_lowerer.UnitObjectType(alternative.scope_class);
  return unit_lowerer.Unit().types.Intern(
      mir::Type{
          mir::PointerType{.pointee = object_type, .ownership = ownership}});
}

// A type wrapped once per dimension still to be fixed: what a declaration
// standing for several objects covers where `depth` of its dimensions are
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

// The position a select names, as the machine integer a sequence is indexed by.
auto BuildSequenceIndex(
    UnitLowerer& unit_lowerer, mir::Block& block, std::uint32_t position)
    -> mir::ExprId {
  return block.exprs.Add(
      mir::Expr{
          .data =
              mir::MachineIntLiteral{
                  .value = static_cast<std::int64_t>(position)},
          .type = unit_lowerer.Unit().builtins.machine_int64});
}

// Appends `element` to the sequence the local `sequence` holds, as a statement
// of `block`.
void AppendToSequence(
    const mir::CompilationUnit& unit, mir::Block& block, mir::LocalId sequence,
    mir::TypeId sequence_type, mir::ExprId element) {
  const mir::ExprId grown = block.exprs.Add(
      mir::Expr{
          .data =
              mir::CallExpr{
                  .callee =
                      mir::Direct{
                          .target = support::BuiltinFn::kExtendSequence},
                  .arguments =
                      {block.exprs.Add(
                           mir::MakeLocalRefExpr(sequence, sequence_type)),
                       element}},
          .type = sequence_type});
  block.AppendStmt(
      mir::ExprStmt{
          .expr = block.exprs.Add(
              mir::MakeAssignExpr(
                  unit.builtins,
                  block.exprs.Add(
                      mir::MakeLocalRefExpr(sequence, sequence_type)),
                  grown))});
}

// What a scope adds to the hierarchical name of everything inside it (LRM
// 23.6): `name`, which arrives as such a name writes it, and one index per
// dimension it is an element of. A scope standing alone has no index, and one
// the source gave no name has an empty name, which keeps it off every
// hierarchical name the run reports.
auto BuildHierarchySegment(
    const mir::CompilationUnit& unit, mir::Block& block,
    const std::string& name, std::span<const mir::ExprId> indices)
    -> mir::ExprId {
  const mir::ExprId indices_id = block.exprs.Add(
      mir::Expr{
          .data = mir::CompositeExpr{.parts = {indices.begin(), indices.end()}},
          .type = mir::MachineArrayOf(
              unit.types, unit.builtins.int_type, indices.size())});
  return block.exprs.Add(
      mir::Expr{
          .data =
              mir::CallExpr{
                  .callee = mir::Construct{},
                  .arguments =
                      {block.exprs.Add(
                           mir::MakeStringLiteral(unit.builtins.string, name)),
                       indices_id}},
          .type = unit.builtins.hierarchy_segment});
}

// Builds one object an external-unit instance member declares and hands back
// the borrowed pointer the runtime tree returns. The object is built and given
// to the tree to own, under `runtime_label` and the index `indices` give it in
// each dimension of its declaration (LRM 23.3.3.5). An index is a value the
// construction counts out rather than a constant, so a declaration covering
// many objects builds them in a loop; a scalar instance is the index-free
// case, built by the same expression. `arguments` are what the object's
// constructor is passed, one per parameter it takes at construction.
auto BuildOwnedInstance(
    UnitLowerer& unit_lowerer, const WalkFrame& frame, mir::ExprId parent_self,
    const std::string& runtime_label, std::string_view declaring_unit,
    mir::TypeId owning_pointer_type, mir::TypeId borrowed_pointer_type,
    std::span<const mir::ExprId> indices, std::vector<mir::Expr> arguments)
    -> mir::ExprId {
  mir::Block& block = *frame.current_block;
  const auto& builtins = unit_lowerer.Unit().builtins;
  const mir::ExprId segment_id =
      BuildHierarchySegment(unit_lowerer.Unit(), block, runtime_label, indices);

  // This unit read what the instantiated one published, which states what may
  // be reached and never how much storage an object takes, so the object is
  // asked for rather than made here.
  std::vector<mir::ExprId> entry_arguments{parent_self, segment_id};
  for (mir::Expr& value : arguments) {
    entry_arguments.push_back(block.exprs.Add(std::move(value)));
  }
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
                  .arguments = std::move(entry_arguments)},
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
                          .receiver = BuildObjectDeref(
                              unit_lowerer.Unit(), block, parent_self)},
                  .arguments = {ctor_call_id}},
          .type = builtins.scope_ptr});
  return ObjectAs(block, add_id, borrowed_pointer_type);
}

// The arguments a construction passes the constructor of the object it builds,
// evaluated where it is built, once per object, in the block that builds it.
auto LowerConstructorArguments(
    StructuralScopeLowerer& lowerer, const WalkFrame& frame,
    std::span<const hir::ExprId> arguments)
    -> diag::Result<std::vector<mir::Expr>> {
  std::vector<mir::Expr> lowered;
  lowered.reserve(arguments.size());
  for (const hir::ExprId value : arguments) {
    auto one = lowerer.LowerExpr(lowerer.HirScope().exprs.Get(value), frame);
    if (!one) return std::unexpected(std::move(one.error()));
    lowered.push_back(*std::move(one));
  }
  return lowered;
}

// Builds one object of `member` as its alternative `which` says, at the
// positions `coords` count out, and hands back the handle the member holds it
// by.
auto BuildAlternative(
    StructuralScopeLowerer& lowerer, const WalkFrame& frame,
    const hir::InstanceMemberDecl& member, std::uint32_t which,
    std::span<const mir::LocalId> coords, mir::TypeId held)
    -> diag::Result<mir::ExprId> {
  UnitLowerer& unit_lowerer = lowerer.Owner();
  mir::Block& block = *frame.current_block;
  const hir::InstanceAlternative& alternative = member.alternatives[which];
  const mir::TypeId borrowed = MakeExternalUnitPointer(
      unit_lowerer, alternative, mir::PointerOwnership::kBorrowed);
  const mir::ExprId parent_self = block.exprs.Add(
      MakeSelfRefExpr(frame, frame.current_class->self_pointer_type));
  auto arguments =
      LowerConstructorArguments(lowerer, frame, alternative.arguments);
  if (!arguments) return std::unexpected(std::move(arguments.error()));

  // An element is selected by an index of the range its dimension declares
  // (LRM 23.6), and the one at a position is that many above the lowest.
  const mir::CompilationUnit& unit = unit_lowerer.Unit();
  std::vector<mir::ExprId> indices;
  indices.reserve(coords.size());
  for (std::size_t d = 0; d < coords.size(); ++d) {
    indices.push_back(block.exprs.Add(
        mir::Expr{
            .data =
                mir::BinaryExpr{
                    .op = mir::BinaryOp::kAdd,
                    .lhs = BuildIntLiteral(
                        unit, block, member.array_dims[d].LowestIndex()),
                    .rhs = block.exprs.Add(
                        mir::MakeLocalRefExpr(
                            coords[d], unit.builtins.int_type))},
            .type = unit.builtins.int_type}));
  }
  const mir::ExprId built = BuildOwnedInstance(
      unit_lowerer, frame, parent_self, member.instance_name,
      unit_lowerer.Hir()
          .external_scope_classes.Get(alternative.scope_class)
          .unit_name,
      MakeExternalUnitPointer(
          unit_lowerer, alternative, mir::PointerOwnership::kUnique),
      borrowed, indices, *std::move(arguments));
  return ObjectAs(block, built, held);
}

// The object a construction builds at one position of several, as the
// alternative `taken` says that position takes. Equal neighbours form runs, so
// the choice is a search over the runs -- the position compared with where
// each ends, in order, the last being what remains -- and a construction whose
// objects are all built alike has one run and builds it with no test at all.
// `position` reads the position afresh at every call, and `build` builds one
// alternative as the handle `held` holds it by.
auto ChooseByPosition(
    const mir::CompilationUnit& unit, mir::Block& block,
    std::span<const std::uint32_t> taken,
    const std::function<mir::ExprId()>& position,
    const std::function<diag::Result<mir::ExprId>(std::uint32_t)>& build,
    mir::TypeId held) -> diag::Result<mir::ExprId> {
  struct Run {
    std::size_t end;
    std::uint32_t alternative;
  };
  std::vector<Run> runs;
  for (std::size_t at = 0; at < taken.size(); ++at) {
    if (runs.empty() || runs.back().alternative != taken[at]) {
      runs.push_back(Run{.end = at + 1, .alternative = taken[at]});
    } else {
      runs.back().end = at + 1;
    }
  }

  auto chosen = build(runs.back().alternative);
  if (!chosen) return std::unexpected(std::move(chosen.error()));
  for (std::size_t back = runs.size() - 1; back > 0; --back) {
    const Run& run = runs[back - 1];
    auto built = build(run.alternative);
    if (!built) return std::unexpected(std::move(built.error()));
    const mir::ExprId before_end = block.exprs.Add(
        mir::Expr{
            .data =
                mir::BinaryExpr{
                    .op = mir::BinaryOp::kLessThan,
                    .lhs = position(),
                    .rhs = BuildIntLiteral(
                        unit, block, static_cast<std::int64_t>(run.end))},
            .type = unit.builtins.bit1});
    chosen = block.exprs.Add(
        mir::Expr{
            .data =
                mir::ConditionalExpr{
                    .condition = ReduceToCondition(unit, block, before_end),
                    .then_value = *built,
                    .else_value = *chosen},
            .type = held});
  }
  return chosen;
}

// Builds the object of `member` that the positions `coords` name: the
// alternative its position takes, its position counted in row-major order.
auto BuildElement(
    StructuralScopeLowerer& lowerer, const WalkFrame& frame,
    const hir::InstanceMemberDecl& member, std::span<const mir::LocalId> coords,
    mir::TypeId held) -> diag::Result<mir::ExprId> {
  const mir::CompilationUnit& unit = lowerer.Owner().Unit();
  mir::Block& block = *frame.current_block;
  const auto position = [&] {
    mir::ExprId at = BuildIntLiteral(unit, block, 0);
    for (std::size_t d = 0; d < coords.size(); ++d) {
      const mir::ExprId scaled = block.exprs.Add(
          mir::Expr{
              .data =
                  mir::BinaryExpr{
                      .op = mir::BinaryOp::kMul,
                      .lhs = at,
                      .rhs = BuildIntLiteral(
                          unit, block,
                          static_cast<std::int64_t>(
                              member.array_dims[d].ElementCount()))},
              .type = unit.builtins.int_type});
      at = block.exprs.Add(
          mir::Expr{
              .data =
                  mir::BinaryExpr{
                      .op = mir::BinaryOp::kAdd,
                      .lhs = scaled,
                      .rhs = block.exprs.Add(
                          mir::MakeLocalRefExpr(
                              coords[d], unit.builtins.int_type))},
              .type = unit.builtins.int_type});
    }
    return at;
  };
  return ChooseByPosition(
      unit, block, member.taken, position,
      [&](std::uint32_t which) {
        return BuildAlternative(lowerer, frame, member, which, coords, held);
      },
      held);
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
    StructuralScopeLowerer& lowerer, const WalkFrame& frame,
    const hir::InstanceMemberDecl& member, mir::TypeId held,
    std::vector<mir::LocalId>& coords) -> diag::Result<mir::ExprId> {
  UnitLowerer& unit_lowerer = lowerer.Owner();
  mir::Block& block = *frame.current_block;
  if (coords.size() == member.array_dims.size()) {
    return BuildElement(lowerer, frame, member, coords, held);
  }

  const mir::CompilationUnit& unit = unit_lowerer.Unit();
  const std::uint64_t count = member.array_dims[coords.size()].ElementCount();
  const mir::TypeId sequence_type = SequenceOver(
      unit_lowerer, held, member.array_dims.size() - coords.size());

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
  auto element_or =
      BuildInstanceMemberValue(lowerer, element_frame, member, held, coords);
  coords.pop_back();
  if (!element_or) return std::unexpected(std::move(element_or.error()));
  AppendToSequence(unit, element_block, sequence, sequence_type, *element_or);

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
auto EmitInstanceMemberConstruction(
    StructuralScopeLowerer& lowerer, WalkFrame frame) -> diag::Result<void> {
  UnitLowerer& unit_lowerer = lowerer.Owner();
  mir::Block& block = *frame.current_block;
  const hir::StructuralScope& hir_scope = lowerer.HirScope();
  for (const hir::InstanceMemberId id : hir_scope.instance_members.Ids()) {
    const hir::InstanceMemberDecl& im = hir_scope.instance_members.Get(id);
    // The field is laid out from what the unit published, which states how
    // each object is held: the pointer under one sequence per dimension.
    const mir::ClassFieldTarget field = lowerer.InstanceMemberField(id);
    const mir::TypeId member_type = FieldTypeOf(unit_lowerer, field);
    mir::TypeId held = member_type;
    for (std::size_t d = 0; d < im.array_dims.size(); ++d) {
      held = unit_lowerer.Unit().types.Get(held).Get<mir::VectorType>().element;
    }
    std::vector<mir::LocalId> coords;
    auto value_or = BuildInstanceMemberValue(lowerer, frame, im, held, coords);
    if (!value_or) return std::unexpected(std::move(value_or.error()));
    const mir::ExprId value = *value_or;
    const mir::ExprId member = block.exprs.Add(
        mir::MakeFieldAccessExpr(
            BuildObjectDeref(
                unit_lowerer.Unit(), block,
                block.exprs.Add(MakeSelfRefExpr(
                    frame, frame.current_class->self_pointer_type))),
            field, member_type));
    block.AppendStmt(
        mir::ExprStmt{
            .expr = block.exprs.Add(
                mir::MakeAssignExpr(
                    unit_lowerer.Unit().builtins, member, value))});
  }
  return {};
}

// What a route of each use is reached by: a pointer to the cell or the object
// it ends at, which depends on where it ends, or a pointer to a disable target,
// the same for every route of that use.
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

// Settles how each route of one use is reached. A route made only of parent
// edges within this unit is walked where it is used and takes no member. Any
// other -- downward, sideways, or starting at an enclosing scope -- takes
// one slot, typed by `slot_type` from what the route ends at, so a body
// reaching through it meets the target's own access protocol and no other. The
// type is interned per slot rather than once per use, so a unit that keeps no
// route of a use carries none of the types it would be kept in -- which is
// what lets a backend read off its own types whether it meets the form at all.
template <typename Leaf, typename Id, typename SlotType>
auto DeclareReaches(
    ClassShape& shape, const base::Arena<hir::Route<Leaf>, Id>& routes,
    SlotType slot_type) -> base::Translation<Id, RouteReach> {
  std::vector<RouteReach> reaches;
  reaches.reserve(routes.size());
  for (const hir::Route<Leaf>& route : routes) {
    const auto* in_unit = std::get_if<hir::InUnitBase>(&route.base);
    if (in_unit != nullptr && route.steps.empty()) {
      reaches.emplace_back(ClimbedRoute{.hops = in_unit->hops});
    } else {
      reaches.emplace_back(
          StoredRoute{.slot = shape.AddField(slot_type(route.leaf))});
    }
  }
  return {routes.size(), std::move(reaches)};
}

// A route runs from its origin (the referrer's `self`) to the referenced leaf,
// and each step reaches through whatever the step before it landed on. What
// that is decides how the next one may reach: a scope this artifact lowers
// admits a typed member access onto anything it declares, and an object another
// unit defines is a typed pointer whose names resolve against what that unit
// published -- each step and leaf there naming the published class it reads.
struct InOwnScope {
  const StructuralScopeLowerer* scope;
};
struct InExternalScope {};

using RoutePlace = std::variant<InOwnScope, InExternalScope>;

struct ReachedPlace {
  mir::ExprId expr{};
  RoutePlace place;
};

// The scope a step or leaf naming one of this artifact's own declarations is
// standing on. Reaching one is only possible while the route is still inside
// the artifact, so a place that has left it is a route built against a
// different design than the one it reached.
auto OwnScopeOf(const ReachedPlace& from, std::string_view site)
    -> const StructuralScopeLowerer& {
  const auto* own = std::get_if<InOwnScope>(&from.place);
  if (own == nullptr) {
    throw InternalError(
        std::format(
            "{}: the route names a declaration of a scope this artifact "
            "lowers, so it cannot have left the artifact before reaching it",
            site));
  }
  return *own->scope;
}

// Establishes the place the route starts from its base. An in-unit base climbs
// `hops` typed parent edges to an ancestor scope of this unit, which keeps the
// place typed. A base outside this unit is the enclosing scope of a class,
// which the runtime finds above this unit's own object: starting there rather
// than at the reader is what keeps an instance of this same unit from answering
// for one enclosing it.
auto BuildRouteAnchor(
    const StructuralScopeLowerer& lowerer, const WalkFrame& frame,
    const hir::RouteBase& base) -> ReachedPlace {
  UnitLowerer& unit_lowerer = lowerer.Owner();
  auto& unit = unit_lowerer.Unit();
  mir::Block& block = *frame.current_block;

  return std::visit(
      Overloaded{
          [&](const hir::InUnitBase& ib) {
            return ReachedPlace{
                .expr = BuildEnclosingScopeReceiver(
                    frame, unit, mir::EnclosingHops{.value = ib.hops.value}),
                .place = InOwnScope{&lowerer.EnclosingScopeAtHops(ib.hops)}};
          },
          [&](const hir::EnclosingScopeBase& eb) {
            std::uint32_t to_unit_object = 0;
            for (const StructuralScopeLowerer* at = &lowerer;
                 at->Parent() != nullptr; at = at->Parent()) {
              ++to_unit_object;
            }
            const mir::ExprId found = block.exprs.Add(
                mir::Expr{
                    .data =
                        mir::CallExpr{
                            .callee =
                                mir::Direct{
                                    .target =
                                        support::BuiltinFn::kEnclosingScope,
                                    .receiver = BuildObjectDeref(
                                        unit, block,
                                        BuildEnclosingScopeReceiver(
                                            frame, unit,
                                            mir::EnclosingHops{
                                                .value = to_unit_object}))},
                            .arguments = {BuildDefinitionRead(
                                unit, block,
                                unit_lowerer.ScopeClassIdentity(
                                    eb.scope_class))}},
                    .type = unit.builtins.scope_ptr});
            // The query answered with a scope of that unit's class, so it is
            // read as one.
            const mir::TypeId object_pointer = unit.types.Intern(
                mir::Type{mir::PointerType{
                    .pointee = unit_lowerer.UnitObjectType(eb.scope_class),
                    .ownership = mir::PointerOwnership::kBorrowed}});
            return ReachedPlace{
                .expr = ObjectAs(block, found, object_pointer),
                .place = InExternalScope{}};
          }},
      base);
}

// The member of the class `from` stands in at `field`, with the path element's
// instance selects applied.
auto SelectedMember(
    UnitLowerer& unit_lowerer, mir::Block& block, const ReachedPlace& from,
    const mir::ClassFieldTarget& field, std::span<const std::uint32_t> selects)
    -> mir::ExprId {
  const mir::TypeId held = FieldTypeOf(unit_lowerer, field);
  return ApplyInstanceSelects(
             unit_lowerer, block,
             ReachedObject{
                 .expr = block.exprs.Add(
                     mir::MakeFieldAccessExpr(
                         BuildObjectDeref(
                             unit_lowerer.Unit(), block, from.expr),
                         field, held)),
                 .type = held},
             selects)
      .expr;
}

// Descends one element into a child the scope `from` stands in declares: the
// parent's handle on that child, selected. A child whose body is another
// compilation unit is still reached by a typed pointer, but what it declares
// is that unit's to state, so the route stops resolving names against a scope
// of this one. One this artifact lowers keeps the route inside it, and since
// the handle holds the base every block of a construct extends, what it
// reached is viewed as the class of the scope the element names.
auto StepToOwnedChild(
    UnitLowerer& unit_lowerer, mir::Block& block, const ReachedPlace& from,
    const hir::OwnedChildRef& names, std::span<const std::uint32_t> selects)
    -> ReachedPlace {
  const StructuralScopeLowerer& scope = OwnScopeOf(from, "StepToOwnedChild");
  const OwnedChildAnchor anchor = scope.TranslateOwnedChild(names, selects);
  const mir::ExprId reached = SelectedMember(
      unit_lowerer, block, from, anchor.borrowed_handle, selects);
  if (anchor.target_scope == nullptr) {
    return ReachedPlace{.expr = reached, .place = InExternalScope{}};
  }
  return ReachedPlace{
      .expr = ObjectAs(
          block, reached,
          unit_lowerer.GetClassShape(anchor.target_scope->ClassId())
              .self_pointer_type),
      .place = InOwnScope{anchor.target_scope}};
}

// Descends one element through an interface port of the scope `from` stands
// in: the borrowed reference the parent bound there (LRM 25.3), selected, since
// a port carrying a range is one member standing for every instance bound to
// it. Everything past it belongs to the unit the port names, which this
// artifact does not lower -- the same place an owned child whose body is
// another unit leaves it.
auto StepThroughInterfacePort(
    UnitLowerer& unit_lowerer, mir::Block& block, const ReachedPlace& from,
    hir::InterfacePortId port, std::span<const std::uint32_t> selects)
    -> ReachedPlace {
  const StructuralScopeLowerer& scope =
      OwnScopeOf(from, "StepThroughInterfacePort");
  return ReachedPlace{
      .expr = SelectedMember(
          unit_lowerer, block, from,
          scope.TranslateInterfacePort(hir::StructuralHops{0}, port), selects),
      .place = InExternalScope{}};
}

// Projects the borrowed-pointer value the slot takes out of a typed place: the
// field access, addressed. Everything a scope's bodies declare with a lifetime
// longer than an activation is a field of the scope's object, so the place is
// already standing where the field is.
auto AddressTypedLeaf(
    UnitLowerer& unit_lowerer, mir::Block& block, const ReachedPlace& from,
    const mir::ClassFieldTarget& field, mir::TypeId slot_type) -> mir::ExprId {
  const mir::ExprId access = block.exprs.Add(
      mir::MakeFieldAccessExpr(
          BuildObjectDeref(unit_lowerer.Unit(), block, from.expr), field,
          FieldTypeOf(unit_lowerer, field)));
  return block.exprs.Add(
      mir::Expr{
          .data = mir::AddressOfExpr{.operand = access}, .type = slot_type});
}

// Materializes where a route landed as the value its use reaches it by, one
// form per use. Data is the addressed member access, of a field this artifact
// declares or of one the target scope published.
auto MaterializeLeaf(
    UnitLowerer& unit_lowerer, mir::Block& block, const ReachedPlace& reached,
    const hir::DataLeaf& leaf) -> mir::ExprId {
  const mir::TypeId pointer_type = PointerTypeOf(unit_lowerer, leaf);
  return std::visit(
      Overloaded{
          [&](const hir::StructuralDataObjectLeaf& l) {
            const StructuralScopeLowerer& scope =
                OwnScopeOf(reached, "MaterializeLeaf");
            return AddressTypedLeaf(
                unit_lowerer, block, reached,
                scope.TranslateStructuralDataObject(
                    hir::StructuralHops{0}, l.object),
                pointer_type);
          },
          [&](const hir::ProceduralStaticLeaf& l) {
            const StructuralScopeLowerer& scope =
                OwnScopeOf(reached, "MaterializeLeaf");
            return AddressTypedLeaf(
                unit_lowerer, block, reached,
                scope.ProceduralStaticField(l.body, l.var), pointer_type);
          },
          // A published member is reached through the target unit's own
          // object, whose pointer the step before it produced.
          [&](const hir::ExternalMemberLeaf& l) {
            return ReadPublishedMember(
                unit_lowerer, block, reached.expr, l.scope_class, l.member);
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
    UnitLowerer& unit_lowerer, mir::Block& block, const ReachedPlace& reached,
    const hir::ScopeLeaf& leaf) -> mir::ExprId {
  return ObjectAs(block, reached.expr, PointerTypeOf(unit_lowerer, leaf));
}

// What a `disable` terminates: the target's own cell where this artifact lays
// out the scope declaring it, and otherwise the one the scope the steps reached
// published (LRM 9.6.2, 23.9).
auto MaterializeLeaf(
    UnitLowerer& unit_lowerer, mir::Block& block, const ReachedPlace& reached,
    const hir::DisableLeaf& leaf) -> mir::ExprId {
  const mir::TypeId pointer_type =
      DisableTargetPointerType(unit_lowerer.Unit().types);
  return std::visit(
      Overloaded{
          [&](const hir::DisableTargetLeaf& l) {
            const StructuralScopeLowerer& scope =
                OwnScopeOf(reached, "MaterializeLeaf");
            return AddressTypedLeaf(
                unit_lowerer, block, reached, scope.DisableTargetField(l.scope),
                pointer_type);
          },
          [&](const hir::ExternalDisableTargetLeaf& l) {
            return ReachPublishedDisableTarget(
                unit_lowerer, block, reached.expr, l);
          }},
      leaf);
}

// Walks from the base to whatever the last step lands on, which is a scope of
// the elaborated tree. What the walk is for -- reaching something the scope
// holds, or asking the scope a name -- is the caller's, so the walk ends here.
auto BuildRouteWalk(
    const StructuralScopeLowerer& lowerer, const WalkFrame& frame,
    const hir::RouteBase& base, std::span<const hir::PathStep> steps)
    -> ReachedPlace {
  UnitLowerer& unit_lowerer = lowerer.Owner();
  mir::Block& block = *frame.current_block;
  ReachedPlace reached = BuildRouteAnchor(lowerer, frame, base);
  for (const hir::PathStep& step : steps) {
    reached = std::visit(
        Overloaded{
            [&](const hir::OwnedChildRef& owned) {
              return StepToOwnedChild(
                  unit_lowerer, block, reached, owned, step.selects);
            },
            [&](const hir::InterfacePortId& port) {
              return StepThroughInterfacePort(
                  unit_lowerer, block, reached, port, step.selects);
            },
            // What another unit published belongs to that unit, so the route
            // has left this artifact.
            [&](const hir::ExternalScopeRef& published) {
              return ReachedPlace{
                  .expr = StepThroughPublished(
                      unit_lowerer, block, reached.expr, published,
                      step.selects),
                  .place = InExternalScope{}};
            }},
        step.names);
  }
  return reached;
}

// Composes what a route ends at: the walk above, then the leaf materialized
// where it landed. Appends to the frame's block and returns the value, for
// whoever uses it there -- a slot being filled, or an access.
template <typename Leaf>
auto BuildRouteValue(
    const StructuralScopeLowerer& lowerer, const WalkFrame& frame,
    const hir::Route<Leaf>& route) -> mir::ExprId {
  const ReachedPlace reached =
      BuildRouteWalk(lowerer, frame, route.base, route.steps);
  return MaterializeLeaf(
      lowerer.Owner(), *frame.current_block, reached, route.leaf);
}

// Stores `value` into the scope's own `slot`. Every slot a scope settles is
// filled this way, once, and read directly at every use afterwards.
void FillScopeSlot(
    const mir::CompilationUnit& unit, const WalkFrame& frame, mir::FieldId slot,
    mir::ExprId value) {
  mir::Class& mir_class = *frame.current_class;
  mir::Block& block = *frame.current_block;
  const mir::TypeId slot_type = mir_class.fields.Get(slot).type;
  const mir::ExprId self =
      block.exprs.Add(MakeSelfRefExpr(frame, mir_class.self_pointer_type));
  const mir::ExprId target = block.exprs.Add(
      mir::MakeFieldAccessExpr(
          BuildObjectDeref(unit, block, self),
          mir::ClassFieldTarget{
              .owner = mir::IntraUnitClassRef{frame.current_class_id},
              .slot = slot},
          slot_type));
  const mir::ExprId assign =
      block.exprs.Add(mir::MakeAssignExpr(unit.builtins, target, value));
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
                  lowerer.Owner().Unit(), resolve_frame, stored.slot,
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
  InstallStoredRoutes(lowerer, resolve_frame, routes.disable_targets);
}

// Appends one process activation registration to the scope's `activate` body:
// invokes `body` over the activate frame's `self` to produce the coroutine,
// then registers it through `registration`, which says whether it starts with
// the scope or runs at its shutdown (LRM 9.2). Startup and shutdown are
// distinct registration callees, not one tagged call.
//
// The registration also names the unit instance the process belongs to, which
// is where LRM 18.14.1 keeps the seeds a static process starts from. That
// instance is the scope the artifact's own class tree is rooted at, a fixed
// number of steps out from wherever the process is declared, so the call
// reaches it by typed navigation over a distance this walk already knows.
void AppendProcessRegistration(
    UnitLowerer& unit_lowerer, const WalkFrame& activate_frame,
    mir::CallableId body, support::BuiltinFn registration) {
  mir::Block& block = *activate_frame.current_block;
  const mir::TypeId self_ptr_type =
      activate_frame.current_class->self_pointer_type;
  const mir::ExprId body_self = BuildObjectDeref(
      unit_lowerer.Unit(), block,
      block.exprs.Add(MakeSelfRefExpr(activate_frame, self_ptr_type)));
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
                  .callee = mir::Direct{.target = registration},
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
// supplies: the handle itself where the member stands for one object, held the
// way the member holds it, and the sequence of what the dimension below holds
// where it stands for several. The ranges are the child's own published ones,
// so how many the parent supplies per dimension is what the child declares
// rather than a second count. `held` is what the member holds at this depth,
// and `next` walks the handles in the order the port's dimensions count them.
auto ComposeBoundObjects(
    UnitLowerer& unit_lowerer, mir::Block& block,
    std::span<const hir::UnpackedRange> ranges, mir::TypeId held,
    std::span<const mir::ExprId> handles, std::size_t& next) -> mir::ExprId {
  if (ranges.empty()) {
    if (next >= handles.size()) {
      throw InternalError(
          "ComposeBoundObjects: the connection supplies one instance per "
          "object the port stands for, which is checked where it is recorded");
    }
    return ObjectAs(block, handles[next++], held);
  }
  const mir::TypeId element =
      unit_lowerer.Unit().types.Get(held).Get<mir::VectorType>().element;
  const std::uint64_t count = ranges.front().ElementCount();
  std::vector<mir::ExprId> elements;
  elements.reserve(count);
  for (std::uint64_t i = 0; i < count; ++i) {
    elements.push_back(ComposeBoundObjects(
        unit_lowerer, block, ranges.subspan(1), element, handles, next));
  }
  return block.exprs.Add(BuildSequenceConstructionCall(
      unit_lowerer.Unit(), block, held, std::move(elements)));
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
  const auto* objects =
      unit_lowerer.Hir().types.Get(port.type).As<hir::UnitObjectsType>();
  if (objects == nullptr) {
    throw InternalError(
        "InstallInterfacePortConnection: an interface port's type is the set "
        "of instances it stands for");
  }
  std::size_t next = 0;
  const mir::ExprId value = ComposeBoundObjects(
      unit_lowerer, block, objects->ranges, member_type, handles, next);
  block.AppendStmt(
      mir::ExprStmt{
          .expr = block.exprs.Add(
              mir::MakeAssignExpr(
                  unit_lowerer.Unit().builtins, target, value))});
}

// The positions one operand of a join's side names: the part of a net the
// source wrote, as a path that evaluates nothing, and where among that part's
// positions they start and how many there are. Positions that meet several of
// the side beside them, or that stand between two sides, are named at each
// place they meet one, which a path that evaluates nothing may be.
struct JoinedPositions {
  AccessPath part;
  std::uint32_t offset = 0;
  std::uint32_t width = 0;
};

// The positions one side of a join names, operand by operand, each with what
// reaching its part computes evaluated in the frame's block, once.
auto JoinedSideOf(
    const StructuralScopeLowerer& lowerer, WalkFrame resolve_frame,
    const hir::NetSide& side) -> diag::Result<std::vector<JoinedPositions>> {
  std::vector<JoinedPositions> operands;
  operands.reserve(side.size());
  for (const hir::NetPositions& operand : side) {
    auto named = lowerer.LowerAccessPath(
        lowerer.HirScope().exprs.Get(operand.part), resolve_frame);
    if (!named) return std::unexpected(std::move(named.error()));
    operands.push_back(
        JoinedPositions{
            .part = Settled(lowerer.Owner(), resolve_frame, *std::move(named)),
            .offset = operand.offset,
            .width = operand.width});
  }
  return operands;
}

// The position within its net that lies `offset` positions into the part
// `operand` names.
auto PositionWithinNet(
    mir::CompilationUnit& unit, mir::Block& block,
    const JoinedPositions& operand, std::uint32_t offset) -> mir::ExprId {
  const PathBits within = BitsWithinOwner(unit, block, operand.part);
  return ConvertToType(
      unit, block,
      BuildPositionSum(
          unit, block, within.first,
          BuildConstantPosition(
              unit, block, static_cast<std::int64_t>(offset))),
      unit.builtins.int_type);
}

// Equally many positions of two operands that one construct places in the same
// resolution: where the shared positions start in the part each operand names,
// and how many there are.
struct Coupling {
  const JoinedPositions* here = nullptr;
  std::uint32_t here_offset = 0;
  const JoinedPositions* there = nullptr;
  std::uint32_t there_offset = 0;
  std::uint32_t width = 0;
};

// What two sides of one construct say about each other. LRM 10.11 gives an
// overlay the bit overlay rules of a packed union with the same member types,
// so correspondence goes position-wise from the most significant end. The two
// sides' operands need not fall at the same boundaries, so each coupling is as
// wide as the shorter of the two operands it stands between, and whichever side
// it exhausts advances.
auto CoupleSides(
    std::span<const JoinedPositions> left,
    std::span<const JoinedPositions> right) -> std::vector<Coupling> {
  std::vector<Coupling> couplings;
  std::size_t at_left = 0;
  std::size_t at_right = 0;
  std::uint32_t taken_left = 0;
  std::uint32_t taken_right = 0;
  while (at_left < left.size() && at_right < right.size()) {
    const JoinedPositions& here = left[at_left];
    const JoinedPositions& there = right[at_right];
    const std::uint32_t width =
        std::min(here.width - taken_left, there.width - taken_right);
    couplings.push_back(
        Coupling{
            .here = &here,
            .here_offset = here.offset + here.width - taken_left - width,
            .there = &there,
            .there_offset = there.offset + there.width - taken_right - width,
            .width = width});
    taken_left += width;
    taken_right += width;
    if (taken_left == here.width) {
      ++at_left;
      taken_left = 0;
    }
    if (taken_right == there.width) {
      ++at_right;
      taken_right = 0;
    }
  }
  if (at_left != left.size() || at_right != right.size()) {
    throw InternalError(
        "CoupleSides: the sides of one join cover the same number of "
        "positions, which the front end requires of every construct that "
        "forms an overlay");
  }
  return couplings;
}

// The statement of one coupling. Both nets are named whole and the positions
// say which of them the coupling reached, so what a backend meets is one call
// with every operand stated.
auto BuildNetJoinStmt(
    mir::CompilationUnit& unit, mir::Block& block, const Coupling& coupling)
    -> mir::Stmt {
  const mir::ExprId other = OwnerPlace(coupling.there->part);
  const mir::TypeId net_ptr_type = unit.types.Intern(
      mir::Type{mir::PointerType{
          .pointee = block.exprs.Get(other).type,
          .ownership = mir::PointerOwnership::kBorrowed}});
  return mir::Stmt{
      .label = std::nullopt,
      .data = mir::ExprStmt{
          .expr = block.exprs.Add(
              mir::MakeNetJoinCallExpr(
                  OwnerPlace(coupling.here->part),
                  block.exprs.Add(mir::MakeAddressOfExpr(other, net_ptr_type)),
                  PositionWithinNet(
                      unit, block, *coupling.here, coupling.here_offset),
                  PositionWithinNet(
                      unit, block, *coupling.there, coupling.there_offset),
                  BuildIntLiteral(unit, block, coupling.width),
                  unit.builtins.void_type))}};
}

// Realizes the positions of nets this scope's constructs place in one
// resolution (LRM 23.3.3.7, 10.11), as statements in the resolve body, beside
// the `ref` port's bind: no driver is attached and no process is registered,
// because what a join states is which contributions resolve together and not an
// edge anything travels along. Being the same physical net is transitive, so
// stating it between each side and the next states it among all of them.
auto InstallNetJoins(StructuralScopeLowerer& lowerer, WalkFrame resolve_frame)
    -> diag::Result<void> {
  mir::Block& block = *resolve_frame.current_block;
  mir::CompilationUnit& unit = lowerer.Owner().Unit();
  for (const hir::NetJoin& join : lowerer.HirScope().net_joins) {
    std::vector<std::vector<JoinedPositions>> sides;
    sides.reserve(join.sides.size());
    for (const hir::NetSide& side : join.sides) {
      auto operands = JoinedSideOf(lowerer, resolve_frame, side);
      if (!operands) return std::unexpected(std::move(operands.error()));
      sides.push_back(*std::move(operands));
    }
    for (std::size_t next = 1; next < sides.size(); ++next) {
      for (const Coupling& coupling :
           CoupleSides(sides[next - 1], sides[next])) {
        block.AppendStmt(BuildNetJoinStmt(unit, block, coupling));
      }
    }
  }
  return {};
}

// Realizes each connection that carries data across the boundary (LRM 23.3.3).
// An input or output port is the implied continuous assignment between the two
// cells, materialized as the same synthesized process a scope-level `assign`
// produces, registered as a process; when the driven side is a net the edge
// attaches a driver rather than writing the cell. A `ref` port carries no edge
// and is emitted into the resolve block instead, binding the child's reference
// member -- reached by the route to it -- to the connected variable's cell:
// one statement, with no second cell and no continuous assignment.
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
        const hir::DataCell member = hir::CellOf(route.leaf);
        const mir::TypeId ref_type = unit_lowerer.MemberCellType(
            unit_lowerer.TranslateType(member.type), member.storage);
        const mir::ExprId nav = BuildRouteValue(lowerer, resolve_frame, route);
        const mir::ExprId target = resolve_block.exprs.Add(
            mir::Expr{
                .data = mir::DerefExpr{.pointer = nav}, .type = ref_type});

        // The peer is lent as a `ref` actual is. A part of a variable is not
        // yet: the member is bound before the variable's declaration installs
        // what it holds, which moves the part the reference would name.
        auto peer_or =
            lowerer.LowerLhsExpr(hir_scope.exprs.Get(data.peer), resolve_frame);
        if (!peer_or) return std::unexpected(std::move(peer_or.error()));
        if (!peer_or->descent.empty()) {
          return diag::Fail(
              pc.span, diag::DiagCode::kUnsupportedExpressionForm,
              "a ref port connected to an element or a member of a variable "
              "is not yet supported");
        }
        const mir::ExprId bind = BindReferenceSlot(
            unit_lowerer.Unit(), resolve_block, target,
            PathReference(unit_lowerer.Unit(), resolve_block, *peer_or));
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
    AppendProcessRegistration(
        unit_lowerer, activate_frame, body,
        support::BuiltinFn::kRegisterInitial);
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
// its complete hierarchy identity, then handed to the parent to own. What
// comes back is a borrowed pointer, which is what a route navigates through
// and what the caller stores. `runtime_label` is the child's name as a
// hierarchical name writes it (LRM 23.6), empty for a scope the source gave
// none, and `indices` are the coordinates it stands at on that name -- a
// loop's block stands at its index. `arm_frame` must point at the block where
// the stmts land and carry the constructor's bindings so a `self` read
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
    std::span<const mir::ExprId> indices, mir::TypeId handle_type,
    std::vector<mir::Expr> arguments) -> mir::ExprId {
  mir::Block& arm_block = *arm_frame.current_block;
  const mir::Class& owner_class = *arm_frame.current_class;
  ValidateOwnedChildConstruction(owner_class, child_scope_id);

  const auto& builtins = unit_lowerer.Unit().builtins;
  const mir::TypeId self_ptr_type = owner_class.self_pointer_type;
  const mir::TypeId child_ptr_type =
      MakeUniqueObjectPointer(unit_lowerer, child_scope_id);

  const auto self_read = [&]() -> mir::ExprId {
    return arm_block.exprs.Add(MakeSelfRefExpr(arm_frame, self_ptr_type));
  };
  const auto parent_read = [&]() -> mir::ExprId {
    if (!runtime_parent_handle.has_value()) {
      return self_read();
    }
    return arm_block.exprs.Add(
        mir::MakeFieldAccessExpr(
            BuildObjectDeref(unit_lowerer.Unit(), arm_block, self_read()),
            mir::ClassFieldTarget{
                .owner = mir::IntraUnitClassRef{arm_frame.current_class_id},
                .slot = *runtime_parent_handle},
            owner_class.fields.Get(*runtime_parent_handle).type));
  };

  // Build the child's structural identity once and pass it as the child's
  // own ctor argument. The child holds onto it from the moment its
  // constructor returns; %m and debug traces read from that single source.
  const mir::ExprId segment_id = BuildHierarchySegment(
      unit_lowerer.Unit(), arm_block, runtime_label, indices);

  std::vector<mir::ExprId> ctor_call_args;
  ctor_call_args.reserve(2 + arguments.size());
  ctor_call_args.push_back(parent_read());
  ctor_call_args.push_back(segment_id);
  for (mir::Expr& value : arguments) {
    ctor_call_args.push_back(arm_block.exprs.Add(std::move(value)));
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
                          .receiver = BuildObjectDeref(
                              unit_lowerer.Unit(), arm_block, parent_read())},
                  .arguments = {ctor_call_id}},
          .type = builtins.scope_ptr});
  return ObjectAs(arm_block, add_call_id, handle_type);
}

// The same construction for a child this scope keeps one handle to, stored into
// the member that names it. Such a child stands at no coordinate, being the
// only one its member holds.
void AppendOwnedChildConstruction(
    UnitLowerer& unit_lowerer, const WalkFrame& arm_frame,
    std::optional<mir::FieldId> runtime_parent_handle,
    const std::string& runtime_label, mir::ClassId child_scope_id,
    const mir::ClassFieldTarget& handle_field,
    std::vector<mir::Expr> arguments) {
  mir::Block& arm_block = *arm_frame.current_block;
  const mir::Class& owner_class = *arm_frame.current_class;
  const mir::TypeId handle_type = FieldTypeOf(unit_lowerer, handle_field);
  const mir::ExprId typed_handle = BuildOwnedChildHandle(
      unit_lowerer, arm_frame, runtime_parent_handle, runtime_label,
      child_scope_id, {}, handle_type, std::move(arguments));
  const mir::ExprId member = arm_block.exprs.Add(
      mir::MakeFieldAccessExpr(
          BuildObjectDeref(
              unit_lowerer.Unit(), arm_block,
              arm_block.exprs.Add(
                  MakeSelfRefExpr(arm_frame, owner_class.self_pointer_type))),
          handle_field, handle_type));
  arm_block.AppendStmt(
      mir::ExprStmt{
          .expr = arm_block.exprs.Add(
              mir::MakeAssignExpr(
                  unit_lowerer.Unit().builtins, member, typed_handle))});
}

// A repeated generate builds, at every index the loop counts out (LRM 27.4),
// the body the block at that index is. The index is a declaration of this
// scope, so the loop's own expressions read and write it the way any name
// reaches a declaration, and each block is built with the index it stands at.
// Which body a block is follows the order the loop counts its blocks out, so
// the loop counts them as it builds them. What the member receives is the
// sequence of what the loop built, complete: the one that grows is a local
// these steps own and nothing else can name.
auto LowerRepeatedGenerate(
    StructuralScopeLowerer& lowerer, WalkFrame frame,
    const hir::BlocksRepeat& repeat, const GenerateBinding& gen_binding)
    -> diag::Result<mir::Stmt> {
  UnitLowerer& unit_lowerer = lowerer.Owner();
  const mir::CompilationUnit& unit = unit_lowerer.Unit();
  const hir::StructuralScope& hir_scope = lowerer.HirScope();
  const mir::Class& owner_class = *frame.current_class;

  const mir::TypeId sequence_type =
      FieldTypeOf(unit_lowerer, gen_binding.handle);
  const mir::TypeId handle_type =
      unit.types.Get(sequence_type).Get<mir::VectorType>().element;
  const mir::TypeId index_type = unit_lowerer.TranslateType(
      hir_scope.structural_data_objects.Get(repeat.variable).type);
  const mir::ClassFieldTarget index_field =
      lowerer.TranslateStructuralDataObject(
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
            BuildObjectDeref(
                unit, in,
                in.exprs.Add(MakeSelfRefExpr(
                    frame.WithBlock(&in), owner_class.self_pointer_type))),
            index_field, FieldTypeOf(unit_lowerer, index_field)));
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
              AccessPath{.owner = index_place(body), .descent = {}},
              body.exprs.Add(*std::move(initial_or))))});

  const mir::LocalId counted =
      steps.Bindings().DeclareAnonymous(unit.builtins.int_type);
  body.AppendStmt(
      mir::LocalDeclStmt{
          .target = counted, .init = BuildIntLiteral(unit, body, 0)});

  mir::Block loop_body;
  const WalkFrame loop_frame = body_frame.WithBlock(&loop_body);
  // Every body is held by the base each of them extends, so which body a block
  // is changes what is built and not how the sequence holds it.
  auto built = ChooseByPosition(
      unit, loop_body, repeat.taken,
      [&] {
        return loop_body.exprs.Add(
            mir::MakeLocalRefExpr(counted, unit.builtins.int_type));
      },
      [&](std::uint32_t which) -> diag::Result<mir::ExprId> {
        const auto& binding =
            gen_binding.blocks.Get(hir::StructuralScopeId{which});
        auto arguments =
            LowerConstructorArguments(lowerer, loop_frame, binding.arguments);
        if (!arguments) return std::unexpected(std::move(arguments.error()));
        const std::array index{index_read(loop_body)};
        return BuildOwnedChildHandle(
            unit_lowerer, loop_frame, std::nullopt, binding.label,
            binding.lowerer->ClassId(), index, handle_type,
            *std::move(arguments));
      },
      handle_type);
  if (!built) return std::unexpected(std::move(built.error()));
  AppendToSequence(unit, loop_body, sequence, sequence_type, *built);
  loop_body.AppendStmt(
      mir::ExprStmt{
          .expr = loop_body.exprs.Add(
              mir::MakeAssignExpr(
                  unit.builtins,
                  loop_body.exprs.Add(
                      mir::MakeLocalRefExpr(counted, unit.builtins.int_type)),
                  loop_body.exprs.Add(
                      mir::Expr{
                          .data =
                              mir::BinaryExpr{
                                  .op = mir::BinaryOp::kAdd,
                                  .lhs = loop_body.exprs.Add(
                                      mir::MakeLocalRefExpr(
                                          counted, unit.builtins.int_type)),
                                  .rhs = BuildIntLiteral(unit, loop_body, 1)},
                          .type = unit.builtins.int_type})))});
  // The step is the expression the source wrote, and it reaches the next index
  // by writing the loop's own, so it is placed for its effect and its value is
  // dropped -- every form LRM 27.4 admits for it says where the index goes in
  // exactly that way.
  auto step_or =
      lowerer.LowerIgnoredExpr(hir_scope.exprs.Get(repeat.step), loop_frame);
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
          BuildObjectDeref(
              unit, body,
              body.exprs.Add(
                  MakeSelfRefExpr(body_frame, owner_class.self_pointer_type))),
          gen_binding.handle, sequence_type));
  body.AppendStmt(
      mir::ExprStmt{
          .expr = body.exprs.Add(
              mir::MakeAssignExpr(
                  unit.builtins, member,
                  body.exprs.Add(
                      mir::MakeLocalRefExpr(sequence, sequence_type))))});
  return steps.BuildStatement();
}

auto LowerSelectionChoiceInto(
    StructuralScopeLowerer& lowerer, WalkFrame frame,
    const hir::BlocksChoose& chosen, hir::SelectionChoiceId at,
    const GenerateBinding& gen_binding) -> diag::Result<void>;

// What stands on one side of a choice, built into the block the side owns:
// nothing at all, the construction of one alternative's block, or a further
// choice the source wrote inside this side (LRM 27.5).
auto LowerSelectionBranchInto(
    StructuralScopeLowerer& lowerer, WalkFrame frame,
    const hir::BlocksChoose& chosen, const hir::SelectionBranch& branch,
    const GenerateBinding& gen_binding) -> diag::Result<void> {
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
            const auto& binding = gen_binding.blocks.Get(*block);
            auto arguments =
                LowerConstructorArguments(lowerer, frame, binding.arguments);
            if (!arguments)
              return std::unexpected(std::move(arguments.error()));
            AppendOwnedChildConstruction(
                lowerer.Owner(), frame, std::nullopt, binding.label,
                binding.lowerer->ClassId(), gen_binding.handle,
                *std::move(arguments));
            return {};
          },
          [&](hir::SelectionChoiceId nested) -> diag::Result<void> {
            return LowerSelectionChoiceInto(
                lowerer, frame, chosen, nested, gen_binding);
          }},
      branch);
}

// One alternative of a `case` is reached where the selector matches one of its
// own labels. LRM 12.5 fixes that comparison: it succeeds only where every bit
// matches exactly, `x` and `z` included, so the selector is read against each
// label rather than reduced to a value first. `selector` stands under every
// one of those comparisons, so it is a read that evaluates nothing.
auto MatchesAnyLabel(
    StructuralScopeLowerer& lowerer, WalkFrame frame, mir::ExprId selector,
    const std::vector<hir::ExprId>& labels) -> diag::Result<mir::ExprId> {
  mir::Block& block = *frame.current_block;
  const mir::CompilationUnit& unit = lowerer.Owner().Unit();
  const hir::StructuralScope& hir_scope = lowerer.HirScope();

  std::vector<mir::ExprId> matches;
  matches.reserve(labels.size());
  for (const hir::ExprId label : labels) {
    auto lowered = lowerer.LowerExpr(hir_scope.exprs.Get(label), frame);
    if (!lowered) return std::unexpected(std::move(lowered.error()));
    matches.push_back(BuildCaseEquality(
        unit, block, selector, block.exprs.Add(*std::move(lowered))));
  }
  // An item the source gave no label matches nothing of its own, which is what
  // a `default` is; it is reached by the search running out instead.
  return AnyHolds(unit, block, matches);
}

// A `case` searches its items in the order the source wrote them and stops at
// the first match, taking the `default` only once every one of them has failed
// (LRM 12.5). That search is a chain of conditions over one selector, built
// from what stands after every item has failed outwards, so an item states its
// own labels and nothing about the items before it.
auto LowerLabelledChoiceInto(
    StructuralScopeLowerer& lowerer, WalkFrame frame,
    const hir::BlocksChoose& chosen, const hir::ChoiceOnLabel& on,
    const GenerateBinding& gen_binding) -> diag::Result<void> {
  mir::Block& block = *frame.current_block;
  const hir::StructuralScope& hir_scope = lowerer.HirScope();

  // The source wrote the selector once and every item is read against it, so
  // it is evaluated once, ahead of the search, and each item names the result.
  auto selector = lowerer.LowerExpr(hir_scope.exprs.Get(on.selector), frame);
  if (!selector) return std::unexpected(std::move(selector.error()));
  const mir::TypeId selector_type = selector->type;
  const mir::LocalId held = SnapshotExprToLocal(
      lowerer.Owner(), frame, block, selector_type,
      block.exprs.Add(*std::move(selector)));

  mir::Block tail;
  auto otherwise = LowerSelectionBranchInto(
      lowerer, frame.WithBlock(&tail), chosen, on.otherwise, gen_binding);
  if (!otherwise) return std::unexpected(std::move(otherwise.error()));

  for (std::size_t back = on.items.size(); back > 0; --back) {
    const hir::LabeledItem& item = on.items[back - 1];
    mir::Block stands;
    auto body = LowerSelectionBranchInto(
        lowerer, frame.WithBlock(&stands), chosen, item.stands, gen_binding);
    if (!body) return std::unexpected(std::move(body.error()));

    mir::Block step;
    auto test = MatchesAnyLabel(
        lowerer, frame.WithBlock(&step),
        step.exprs.Add(mir::MakeLocalRefExpr(held, selector_type)),
        item.labels);
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
    const GenerateBinding& gen_binding) -> diag::Result<void> {
  const hir::SelectionChoice& choice = chosen.choices.Get(at);
  if (const auto* on = std::get_if<hir::ChoiceOnLabel>(&choice)) {
    return LowerLabelledChoiceInto(lowerer, frame, chosen, *on, gen_binding);
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
      lowerer, frame.WithBlock(&holds), chosen, on.holds, gen_binding);
  if (!taken) return std::unexpected(std::move(taken.error()));

  std::optional<mir::BlockId> otherwise;
  if (!std::holds_alternative<hir::NothingStands>(on.fails)) {
    mir::Block fails;
    auto untaken = LowerSelectionBranchInto(
        lowerer, frame.WithBlock(&fails), chosen, on.fails, gen_binding);
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
    const hir::BlocksChoose& chosen, const GenerateBinding& gen_binding)
    -> diag::Result<mir::Stmt> {
  mir::Block& block = *frame.current_block;
  mir::Block body;

  auto built = LowerSelectionChoiceInto(
      lowerer, frame.WithBlock(&body), chosen, chosen.root, gen_binding);
  if (!built) return std::unexpected(std::move(built.error()));

  return mir::Stmt{
      .label = std::nullopt,
      .data = mir::BlockStmt{.scope = block.child_scopes.Add(std::move(body))}};
}

// A loop whose blocks are each a scope of their own builds every one of them
// directly, with no loop at run time, each carrying the constant index it
// stands at and passed the constructor arguments the generate states for it.
// What the member receives is the sequence of them in the order the loop
// counted them out, complete: the one that grows is a local these steps own and
// nothing else can name.
auto LowerStandAloneGenerate(
    StructuralScopeLowerer& lowerer, WalkFrame frame, const hir::Generate& gen,
    const hir::BlocksStandAlone& stand_alone,
    const GenerateBinding& gen_binding) -> diag::Result<mir::Stmt> {
  UnitLowerer& unit_lowerer = lowerer.Owner();
  const mir::CompilationUnit& unit = unit_lowerer.Unit();
  const mir::TypeId sequence_type =
      FieldTypeOf(unit_lowerer, gen_binding.handle);
  const mir::TypeId handle_type =
      unit.types.Get(sequence_type).Get<mir::VectorType>().element;

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
  for (const hir::StructuralScopeId scope_id : gen.blocks.Ids()) {
    const auto& binding = gen_binding.blocks.Get(scope_id);
    const std::array index{
        BuildIntLiteral(unit, body, stand_alone.indices[scope_id.value])};
    auto arguments =
        LowerConstructorArguments(lowerer, body_frame, binding.arguments);
    if (!arguments) return std::unexpected(std::move(arguments.error()));
    const mir::ExprId child = BuildOwnedChildHandle(
        unit_lowerer, body_frame, std::nullopt, binding.label,
        binding.lowerer->ClassId(), index, handle_type, *std::move(arguments));
    AppendToSequence(unit, body, sequence, sequence_type, child);
  }
  const mir::ExprId member = body.exprs.Add(
      mir::MakeFieldAccessExpr(
          BuildObjectDeref(
              unit, body,
              body.exprs.Add(MakeSelfRefExpr(
                  body_frame, frame.current_class->self_pointer_type))),
          gen_binding.handle, sequence_type));
  body.AppendStmt(
      mir::ExprStmt{
          .expr = body.exprs.Add(
              mir::MakeAssignExpr(
                  unit.builtins, member,
                  body.exprs.Add(
                      mir::MakeLocalRefExpr(sequence, sequence_type))))});
  return steps.BuildStatement();
}

// A block no loop and no conditional produced is built once, directly.
auto LowerSingleBlockGenerate(
    StructuralScopeLowerer& lowerer, WalkFrame frame,
    const GenerateBinding& gen_binding) -> diag::Result<mir::Stmt> {
  mir::Block& block = *frame.current_block;
  mir::Block body;
  const WalkFrame body_frame = frame.WithBlock(&body);
  const auto& binding = gen_binding.blocks.Get(hir::StructuralScopeId{0});
  auto arguments =
      LowerConstructorArguments(lowerer, body_frame, binding.arguments);
  if (!arguments) return std::unexpected(std::move(arguments.error()));
  AppendOwnedChildConstruction(
      lowerer.Owner(), body_frame, std::nullopt, binding.label,
      binding.lowerer->ClassId(), gen_binding.handle, *std::move(arguments));
  return mir::Stmt{
      .label = std::nullopt,
      .data = mir::BlockStmt{.scope = block.child_scopes.Add(std::move(body))}};
}

// A generate construct becomes the construction its compiled form calls for.
auto LowerGenerateAsStmt(
    StructuralScopeLowerer& lowerer, WalkFrame frame, const hir::Generate& gen,
    const GenerateBinding& gen_binding) -> diag::Result<mir::Stmt> {
  return std::visit(
      Overloaded{
          [&](const hir::SingleBlock&) {
            return LowerSingleBlockGenerate(lowerer, frame, gen_binding);
          },
          [&](const hir::BlocksStandAlone& stand_alone) {
            return LowerStandAloneGenerate(
                lowerer, frame, gen, stand_alone, gen_binding);
          },
          [&](const hir::BlocksRepeat& repeat) {
            return LowerRepeatedGenerate(lowerer, frame, repeat, gen_binding);
          },
          [&](const hir::BlocksChoose& chosen) {
            return LowerChosenGenerate(lowerer, frame, chosen, gen_binding);
          }},
      gen.counting);
}

}  // namespace

auto ApplyInstanceSelects(
    UnitLowerer& unit_lowerer, mir::Block& block, ReachedObject reached,
    std::span<const std::uint32_t> selects) -> ReachedObject {
  for (const std::uint32_t select : selects) {
    reached.type = unit_lowerer.Unit()
                       .types.Get(reached.type)
                       .Get<mir::VectorType>()
                       .element;
    reached.expr = block.exprs.Add(
        mir::Expr{
            .data =
                mir::VectorGetExpr{
                    .vector = reached.expr,
                    .index = BuildSequenceIndex(unit_lowerer, block, select)},
            .type = reached.type});
  }
  return reached;
}

auto DescendOwnedChildren(
    const StructuralScopeLowerer& scope, mir::Block& block, mir::ExprId object,
    std::span<const hir::OwnedChildStep> descent) -> mir::ExprId {
  ReachedPlace reached{.expr = object, .place = InOwnScope{&scope}};
  for (const hir::OwnedChildStep& step : descent) {
    reached = StepToOwnedChild(
        scope.Owner(), block, reached, step.names, step.selects);
  }
  return reached.expr;
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
    const WalkFrame& frame, hir::RoutedDisableTargetRefId id) const
    -> mir::ExprId {
  return EndAlong(
      *this, frame, HirScope().routes.disable_targets.Get(id), ReachOf(id));
}

auto StructuralScopeLowerer::ForScope(
    UnitLowerer& unit_lowerer, const StructuralScopeLowerer* parent,
    const hir::StructuralScope& hir_scope, DesignNamespaces namespaces)
    -> std::unique_ptr<StructuralScopeLowerer> {
  auto lowerer = std::make_unique<StructuralScopeLowerer>(
      unit_lowerer, parent, hir_scope, std::move(namespaces));
  lowerer->published_class_id_ =
      unit_lowerer.TakePublishedScopeClass(hir_scope.published);
  lowerer->generate_children_.reserve(hir_scope.generates.size());
  for (const hir::Generate& generate : hir_scope.generates) {
    std::vector<std::unique_ptr<StructuralScopeLowerer>>& blocks =
        lowerer->generate_children_.emplace_back();
    blocks.reserve(generate.blocks.size());
    for (const hir::GenerateBlock& block : generate.blocks) {
      blocks.push_back(ForScope(unit_lowerer, lowerer.get(), block.scope));
    }
  }
  return lowerer;
}

auto StructuralScopeLowerer::DeclareShape() -> diag::Result<mir::ClassId> {
  UnitLowerer& unit_lowerer = *owner_;
  const hir::StructuralScope& hir_scope = *hir_scope_;

  // The identity is minted before the shape is populated so the class's own
  // `self_pointer_type` can name it. What the unit published of the scope took
  // one when this lowering was built: the name a referrer has is the published
  // class's, and this class, which extends it with what the lowering adds,
  // answers to none.
  class_id_ = unit_lowerer.Unit().DeclareClass();
  const mir::TypeId self_object_type = unit_lowerer.Unit().types.Intern(
      mir::Type{mir::ObjectType{.of = mir::IntraUnitClassRef{class_id_}}});
  const mir::TypeId self_pointer_type = unit_lowerer.Unit().types.Intern(
      mir::Type{mir::PointerType{
          .pointee = self_object_type,
          .ownership = mir::PointerOwnership::kBorrowed}});

  ClassShape shape;
  // The scope stands in the tree through what its unit published of it.
  shape.base =
      mir::ClassRef{mir::IntraUnitClassRef{.class_id = published_class_id_}};
  shape.is_final = true;
  shape.self_pointer_type = self_pointer_type;
  shape.time_resolution = hir_scope.time_resolution;

  AttachRuntimeScopeCtorPrefix(unit_lowerer.Unit(), shape);
  // What construction supplies is what distinguishes one built scope from
  // another built from the same declarations.
  for (const hir::StructuralDataObjectId declared :
       ConstructionValuesOf(hir_scope)) {
    construction_values_.push_back(
        ConstructionValue{
            .declared = declared,
            .type = unit_lowerer.TranslateType(
                hir_scope.structural_data_objects.Get(declared).type)});
  }

  // What the unit published of this object is laid out from the signature it
  // published, through the one layout every referrer's record of the object
  // uses too, and each published declaration of this scope takes the field its
  // publication was given. Everything else is placed in this scope's own
  // class, after them.
  const mir::ClassId owner = published_class_id_;
  const PublishedScopeLayout& layout =
      unit_lowerer.SettlePublishedScope(owner, hir_scope);
  const auto on_published = [&](mir::FieldId slot) {
    return mir::ClassFieldTarget{
        .owner = mir::IntraUnitClassRef{owner}, .slot = slot};
  };

  std::vector<std::optional<mir::ClassFieldTarget>> data_object_fields(
      hir_scope.structural_data_objects.size());
  std::vector<std::optional<mir::ClassFieldTarget>> instance_fields(
      hir_scope.instance_members.size());
  std::vector<std::optional<mir::ClassFieldTarget>> interface_port_fields(
      hir_scope.interface_ports.size());
  // The cells of the published static-lifetime locals, taken over by the walk
  // binding each body's locals, keyed by that body.
  std::vector<std::vector<PlacedStatic>> placed_process_statics(
      hir_scope.processes.size());
  std::vector<std::vector<PlacedStatic>> placed_subroutine_statics(
      hir_scope.structural_subroutines.size());
  // The cells of the published static properties of the classes this scope
  // declares, keyed by the class, which takes them over.
  std::map<hir::ClassId, std::vector<PlacedProperty>> placed_class_statics;
  // What each published generate construct built, and what each published
  // disable target ends.
  std::vector<std::optional<mir::ClassFieldTarget>> generate_handles(
      hir_scope.generates.size());
  std::vector<std::optional<mir::ClassFieldTarget>> disable_cells(
      hir_scope.procedural_scopes.size());

  std::uint32_t published_member = 0;
  for (const hir::PublishedDecl& decl : hir_scope.published.members) {
    const mir::ClassFieldTarget field = on_published(
        layout.members.Get(hir::PublishedMemberId{published_member++}));
    std::visit(
        Overloaded{
            [&](hir::StructuralDataObjectId id) {
              data_object_fields[id.value] = field;
            },
            [&](const hir::PublishedStatic& local) {
              const PlacedStatic placed{.var = local.var, .field = field};
              std::visit(
                  Overloaded{
                      [&](hir::ProcessId id) {
                        placed_process_statics[id.value].push_back(placed);
                      },
                      [&](hir::StructuralSubroutineId id) {
                        placed_subroutine_statics[id.value].push_back(placed);
                      }},
                  local.body);
            },
            [&](const hir::LocalStaticPropertyTarget& property) {
              placed_class_statics[property.owner].push_back(
                  PlacedProperty{.property = property.prop, .field = field});
            },
            [&](hir::InstanceMemberId id) {
              instance_fields[id.value] = field;
            },
            [&](hir::InterfacePortId id) {
              interface_port_fields[id.value] = field;
            }},
        decl);
  }
  std::uint32_t published_generate = 0;
  for (const hir::GenerateId id : hir_scope.published.generates) {
    generate_handles[id.value] = on_published(
        layout.generates.Get(hir::PublishedGenerateId{published_generate++}));
  }
  std::uint32_t published_disable_target = 0;
  for (const hir::ProceduralScopeId id : hir_scope.published.disable_targets) {
    disable_cells[id.value] = on_published(layout.disable_targets.Get(
        hir::PublishedDisableTargetId{published_disable_target++}));
  }

  // A data object nothing outside the scope may name is the one member the
  // scope publishes nothing of, so it alone is placed in this scope's own
  // class, in the order it was declared.
  for (const hir::StructuralDataObjectId id :
       hir_scope.structural_data_objects.Ids()) {
    if (data_object_fields[id.value].has_value()) continue;
    const auto& d = hir_scope.structural_data_objects.Get(id);
    const mir::TypeId cell = unit_lowerer.MemberCellType(
        unit_lowerer.TranslateType(d.type), hir::StorageOf(d));
    data_object_fields[id.value] = mir::ClassFieldTarget{
        .owner = mir::IntraUnitClassRef{class_id_},
        .slot = hir::AnsweredByName(d) ? shape.AddNamedField(d.name, cell)
                                       : shape.AddField(cell)};
  }
  // Every instance, interface port and generate construct the scope declares
  // is published, and every data object was placed by the loop above.
  const auto settled =
      [](std::vector<std::optional<mir::ClassFieldTarget>>& fields) {
        std::vector<mir::ClassFieldTarget> out;
        out.reserve(fields.size());
        for (const std::optional<mir::ClassFieldTarget>& field : fields) {
          if (!field.has_value()) {
            throw InternalError(
                "hir_to_mir: a scope publishes every instance, interface port "
                "and generate construct it declares, and places every data "
                "object it holds");
          }
          out.push_back(*field);
        }
        return out;
      };

  data_object_fields_ = {
      hir_scope.structural_data_objects.size(), settled(data_object_fields)};

  // A history is storage nothing outside this scope names, so the unit
  // publishes nothing about it: what reaches it is the scope's own activation,
  // its sampler, and the reads that asked for it (LRM 16.9.3). Its value type
  // is the subject's own, because what a tick keeps is what that expression
  // settled.
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
      hir_scope.instance_members.size(), settled(instance_fields)};
  interface_port_fields_ = {
      hir_scope.interface_ports.size(), settled(interface_port_fields)};

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
  disable_target_reaches_ = DeclareReaches(
      shape, routes.disable_targets,
      [&](const hir::DisableLeaf&) { return DisableTargetPointerType(types); });

  // Recursively declare every owned generate child's class shape; each child
  // lowerer is retained for the body sweep.
  // Every generate construct is published, so what it built is held in the
  // field its publication was given: the block it built, or for a loop one per
  // block in the order it counted them out. What that field holds is the base
  // every scope extends, and which class a block is, is what the step that
  // reaches it says.
  const std::vector<mir::ClassFieldTarget> handles = settled(generate_handles);
  std::vector<GenerateBinding> generates;
  generates.reserve(hir_scope.generates.size());
  for (const hir::GenerateId gen_id : hir_scope.generates.Ids()) {
    const auto& gen = hir_scope.generates.Get(gen_id);
    const mir::ClassFieldTarget handle = handles[gen_id.value];
    std::vector<ChildStructuralScopeBinding> blocks;
    blocks.reserve(gen.blocks.size());
    for (const hir::StructuralScopeId block_id : gen.blocks.Ids()) {
      const hir::GenerateBlock& block = gen.blocks.Get(block_id);
      StructuralScopeLowerer& child =
          *generate_children_[gen_id.value][block_id.value];
      auto child_r = child.DeclareShape();
      if (!child_r) return std::unexpected(std::move(child_r.error()));
      shape.contained.push_back(*child_r);
      blocks.push_back(
          ChildStructuralScopeBinding{
              .label = block.scope.source_name,
              .lowerer = &child,
              .arguments = block.arguments});
    }
    generates.push_back(
        GenerateBinding{
            .handle = handle,
            .blocks = {gen.blocks.size(), std::move(blocks)}});
  }
  generate_bindings_ = {hir_scope.generates.size(), std::move(generates)};

  // Every procedural scope becomes a name node -- an object carrying the
  // segment a construct inside it reports as its hierarchical name (LRM
  // 21.2.1.5) -- whatever the source called it and whether or not anything was
  // declared there, so one shape lowers every scope. A scope the source named
  // carries its segment and one it did not carries none, which keeps the latter
  // out of every reported name while it still holds the nodes below it
  // together.
  //
  // This scope keeps a borrowed handle to every one of them, however deeply
  // they nest, so a body reaches its own name node in one step and nothing has
  // to know what stands between. The nodes' own nesting is the HIR scope tree,
  // read where the objects are built.
  std::vector<DeclaredScope> scopes;
  scopes.reserve(hir_scope.procedural_scopes.size());
  for (const hir::ProceduralScopeId scope_id :
       hir_scope.procedural_scopes.Ids()) {
    const auto& scope = hir_scope.procedural_scopes.Get(scope_id);

    const mir::ClassId node_class = unit_lowerer.Unit().DeclareClass();
    ClassShape node_shape;
    node_shape.base = mir::ClassRef{mir::ObjectTreeRootRef{}};
    node_shape.is_final = true;
    node_shape.self_pointer_type = unit_lowerer.Unit().types.Intern(
        mir::Type{mir::PointerType{
            .pointee = unit_lowerer.Unit().types.Intern(
                mir::Type{
                    mir::ObjectType{.of = mir::IntraUnitClassRef{node_class}}}),
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
                                mir::Type{mir::ObjectType{
                                    .of = mir::IntraUnitClassRef{node_class}}}),
                            .ownership = mir::PointerOwnership::kBorrowed}}))},
        .disable_target = std::nullopt};

    // What a `disable` of this scope invalidates (LRM 9.6.2). Its targets are
    // the blocks and tasks a name reaches, so a scope the source named owns one
    // for that reason alone and one it did not owns none -- no pass has to
    // first find out which scopes some `disable` names. A scope of this
    // hierarchy is replicated with its instance, so the cell is one per
    // instance, shared by every activation of the scope -- the one placed
    // where the scope published it, where it did.
    if (disable_cells[scope_id.value].has_value()) {
      node.disable_target =
          InstanceFieldHome{.field = *disable_cells[scope_id.value]};
    } else if (scope.source_name.has_value()) {
      node.disable_target = DeclareStaticCell(
          InstanceStorage{.owner = class_id_, .shape = &shape, .placed = {}},
          unit_lowerer.Unit().types.Intern(
              mir::Type{mir::RuntimeLibraryType{
                  .kind = mir::RuntimeLibraryKind::kCancellationTarget}}));
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
  base::IdAllocator<mir::CallableId> subroutine_ids;
  std::vector<DeclaredCallable> declared_subroutines;
  std::vector<CallableSignature> signatures;
  declared_subroutines.reserve(hir_scope.structural_subroutines.size());
  signatures.reserve(hir_scope.structural_subroutines.size());
  for (const hir::StructuralSubroutineId sub_id :
       hir_scope.structural_subroutines.Ids()) {
    const hir::SubroutineDecl& s = hir_scope.structural_subroutines.Get(sub_id);
    const mir::CallableId body = subroutine_ids.Take();
    signatures.push_back(CallableSignature{.virtual_dispatch = std::nullopt});
    declared_subroutines.push_back(
        DeclaredCallable{
            .callable = body,
            .statics = BindBodyStatics(
                unit_lowerer, hir_scope.procedural_scopes,
                InstanceStorage{
                    .owner = class_id_,
                    .shape = &shape,
                    .placed = placed_subroutine_statics[sub_id.value]},
                s.body, SignatureBoundVars(s))});
  }
  shape.callable_signatures = {
      hir_scope.structural_subroutines.size(), std::move(signatures)};
  declared_subroutines_ = {
      hir_scope.structural_subroutines.size(), std::move(declared_subroutines)};
  // Which body each published subroutine enters; a subroutine the unit kept to
  // itself is none of them.
  published_subroutines_ =
      base::Translation<hir::PublishedCallableId, mir::CallableId>{
          hir_scope.published.callables.size()};
  for (const hir::StructuralSubroutineId id : hir_scope.published.callables) {
    published_subroutines_.Append(declared_subroutines_.Get(id).callable);
  }

  std::vector<StaticVarBindings> process_statics;
  process_statics.reserve(hir_scope.processes.size());
  for (const hir::ProcessId id : hir_scope.processes.Ids()) {
    process_statics.push_back(BindBodyStatics(
        unit_lowerer, hir_scope.procedural_scopes,
        InstanceStorage{
            .owner = class_id_,
            .shape = &shape,
            .placed = placed_process_statics[id.value]},
        hir_scope.processes.Get(id).body, {}));
  }
  process_static_bindings_ = {
      hir_scope.processes.size(), std::move(process_statics)};

  // The classes this scope replicates settle their shapes against this one, and
  // before it is published: such a class keeps its cells here, as fields of the
  // instance, so what it places has to land while the shape is still open.
  class_lowerers_.reserve(hir_scope.replicated_classes.size());
  for (const hir::ClassId hir_class : hir_scope.replicated_classes) {
    class_lowerers_.emplace_back(
        unit_lowerer, hir_class, unit_lowerer.TranslateClass(hir_class),
        unit_lowerer.ClassObjectType(hir_class),
        unit_lowerer.Hir().classes.Get(hir_class), this);
  }
  for (auto&& [class_lowerer, hir_class] :
       std::views::zip(class_lowerers_, hir_scope.replicated_classes)) {
    if (auto r =
            class_lowerer.DeclareShape(&shape, placed_class_statics[hir_class]);
        !r) {
      return std::unexpected(std::move(r.error()));
    }
  }

  unit_lowerer.DefineClassShape(class_id_, std::move(shape));
  return class_id_;
}

namespace {

// Settles the class's construction protocol: the constructor body, and the
// arguments its base is entered with -- each prefix forwarded as a consuming
// use, then the trailing ones. `ctor_code` arrives finalized, its params and
// result type set, and this is what installs it.
void FinalizeConstructor(
    mir::CompilationUnit& unit, mir::Class& cls, mir::CallableCode ctor_code,
    const std::vector<mir::LocalId>& prefix_local_ids,
    const std::vector<mir::ExprId>& base_trailing_args) {
  std::vector<mir::ExprId> base_args;
  base_args.reserve(prefix_local_ids.size() + base_trailing_args.size());
  for (const mir::LocalId id : prefix_local_ids) {
    const mir::TypeId ty = ctor_code.locals.Get(id).type;
    const mir::ExprId local_ref =
        ctor_code.Body().exprs.Add(mir::MakeLocalRefExpr(id, ty));
    if (unit.types.Get(ty).IsAliasHandle()) {
      base_args.push_back(local_ref);
    } else {
      base_args.push_back(ctor_code.Body().exprs.Add(
          mir::Expr{.data = mir::MoveExpr{.operand = local_ref}, .type = ty}));
    }
  }
  for (const mir::ExprId e : base_trailing_args) {
    base_args.push_back(e);
  }
  cls.constructor = mir::ConstructorDecl{
      .code = std::move(ctor_code), .base_args = std::move(base_args)};
}

// Whether a declared object of this type is an owned child (a pointer, a
// vector, an object) or a cross-instance reference slot. Its declaration shape
// alone fixes such a field, so it takes no value and is no signal a name
// answers for.
auto IsOwnedChildOrReferenceSlot(const mir::Type& type) -> bool {
  return type.Is<mir::PointerType>() || type.Is<mir::VectorType>() ||
         type.Is<mir::ObjectType>();
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
  // What construction supplies follows the prefix, and stays out of the base
  // call: the base contract demands the prefix and nothing this scope alone is
  // handed.
  struct SuppliedLocal {
    ConstructionValue value;
    mir::LocalId local;
  };
  std::vector<SuppliedLocal> supplied;
  supplied.reserve(construction_values_.size());
  for (const ConstructionValue& value : construction_values_) {
    supplied.push_back(
        SuppliedLocal{
            .value = value,
            .local = ctor_bindings.DeclareAnonymous(value.type)});
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
  const auto self_object = [&]() -> mir::ExprId {
    return BuildObjectDeref(
        unit_lowerer.Unit(), ctor_block,
        ctor_block.exprs.Add(MakeSelfRefExpr(ctor_frame, self_ptr_type)));
  };
  const auto init_self_object = [&]() -> mir::ExprId {
    return BuildObjectDeref(
        unit_lowerer.Unit(), initialize_block,
        initialize_block.exprs.Add(MakeSelfRefExpr(init_frame, self_ptr_type)));
  };

  // What elaboration settles is read while the object is still being built -- a
  // block's own declarations and connections are written in terms of the index
  // it stands at, and the loop advances that index as it builds (LRM 27.4) --
  // so these cells exist from the constructor rather than from the initialize
  // phase every other declared value waits for.
  const auto install_in_constructor =
      [&](hir::StructuralDataObjectId id) -> mir::ExprId {
    const auto& d = hir_scope.structural_data_objects.Get(id);
    const mir::ClassFieldTarget field =
        TranslateStructuralDataObject(hir::StructuralHops{0}, id);
    const mir::ExprId target = ctor_block.exprs.Add(
        mir::MakeFieldAccessExpr(
            self_object(), field, FieldTypeOf(unit_lowerer, field)));
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
  for (const SuppliedLocal& handed : supplied) {
    const mir::ExprId target = install_in_constructor(handed.value.declared);
    ctor_block.AppendStmt(
        mir::ExprStmt{
            .expr = ctor_block.exprs.Add(BuildStoreExpr(
                unit_lowerer.Unit(), ctor_block,
                AccessPath{.owner = target, .descent = {}},
                ctor_block.exprs.Add(
                    mir::MakeLocalRefExpr(handed.local, handed.value.type))))});
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
    const mir::ExprId settled_target = install_in_constructor(id);
    auto value_or =
        LowerExpr(hir_scope.exprs.Get(settled->initializer), ctor_frame);
    if (!value_or) return std::unexpected(std::move(value_or.error()));
    ctor_block.AppendStmt(
        mir::ExprStmt{
            .expr = ctor_block.exprs.Add(BuildStoreExpr(
                unit_lowerer.Unit(), ctor_block,
                AccessPath{.owner = settled_target, .descent = {}},
                ctor_block.exprs.Add(*std::move(value_or))))});
  }

  for (const hir::StructuralDataObjectId hir_id :
       hir_scope.structural_data_objects.Ids()) {
    const auto& d = hir_scope.structural_data_objects.Get(hir_id);
    const mir::ClassFieldTarget cell =
        TranslateStructuralDataObject(hir::StructuralHops{0}, hir_id);
    const mir::TypeId mir_field_type = FieldTypeOf(unit_lowerer, cell);
    const mir::TypeId mir_value_type = unit_lowerer.TranslateType(d.type);
    const auto* net = std::get_if<hir::StructuralNetDecl>(&d.kind);
    const auto* var = std::get_if<hir::StructuralVariableDecl>(&d.kind);
    // Both are asked of the type here, ahead of the statements built below,
    // because building one may add a type and the type table then moves.
    const bool is_child_or_reference_slot = IsOwnedChildOrReferenceSlot(
        unit_lowerer.Unit().types.Get(mir_value_type));
    const bool is_event =
        unit_lowerer.Unit().types.Get(mir_value_type).Is<mir::EventType>();
    // Owned children, cross-instance reference slots (borrowed pointers filled
    // in the resolve phase), and named events have no "value assignment" --
    // their declaration shape itself fixes the field at construction. A net
    // takes none either: its value is produced by its drivers, seeded when each
    // driver updates in the initialize phase. Value-typed variables (integral,
    // string, real, unpacked / dynamic array) receive an LRM 10.5
    // initialization statement, run in the initialize phase after the tree's
    // references resolve, not in the constructor.
    const bool is_assignable_value =
        var != nullptr && !is_child_or_reference_slot && !is_event;
    if (is_assignable_value) {
      const mir::ExprId init_target = initialize_block.exprs.Add(
          mir::MakeFieldAccessExpr(init_self_object(), cell, mir_field_type));
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
            AccessPath{.owner = init_target, .descent = {}}, value_id));
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
                BuildObjectDeref(
                    unit_lowerer.Unit(), install_block,
                    install_block.exprs.Add(
                        MakeSelfRefExpr(install_frame, self_ptr_type))),
                cell, mir_field_type));
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
          mir::MakeFieldAccessExpr(self_object(), cell, mir_field_type));
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

  // Commit the class of every procedural scope's name node. A name node carries
  // only the segment a construct inside it reports, which the runtime scope
  // base already gives it, so its constructor takes only the identity every
  // scope is built with.
  for (const hir::ProceduralScopeId scope : scopes_.Ids()) {
    const ScopeNameNode& name_node = *scopes_.Get(scope).name_node;
    const ClassShape& node_shape =
        unit_lowerer.GetClassShape(name_node.class_id);
    mir::Class node_class = node_shape.OpenClass();

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
    const auto empty_phase = [&](support::LibraryVirtual phase) {
      mir::CallableCode code = mir::CallableCode::Defined();
      CallableBindings bindings(unit_lowerer.Unit(), code);
      code.params = {bindings.Declare(
          BindingOriginId::Receiver(), node_shape.self_pointer_type)};
      code.result_type = void_type;
      node_class.callables.Add(
          mir::CallableDecl{
              .code = std::move(code),
              .foreign = std::nullopt,
              .virtual_dispatch =
                  mir::OverridesLibraryVirtual{.function = phase}});
    };
    empty_phase(support::LibraryVirtual::kScopeResolve);
    empty_phase(support::LibraryVirtual::kScopeInitialize);
    empty_phase(support::LibraryVirtual::kScopeCreateProcesses);
    StateScopeDefinition(
        unit_lowerer.Unit(), name_node.class_id, node_class, {});
    const mir::ExprId node_definition = BuildDefinitionRead(
        unit_lowerer.Unit(), node_ctor_code.Body(),
        mir::IntraUnitClassRef{.class_id = name_node.class_id});
    FinalizeConstructor(
        unit_lowerer.Unit(), node_class, std::move(node_ctor_code),
        node_ctor_prefix_local_ids, {node_definition});
    unit_lowerer.Unit().DefineClass(name_node.class_id, std::move(node_class));
  }

  // Build the whole name tree here, in this scope's own constructor: each node
  // hangs under the node of the scope around it, which is what the source
  // nesting means, while the borrowed handle to it lands on this class -- so
  // the objects nest and every one of them is still one step from a body.
  const auto build_name_tree =
      [&](const auto& self_ref, hir::ProceduralScopeId scope_id,
          std::optional<mir::FieldId> parent_handle) -> void {
    const auto& scope = hir_scope.procedural_scopes.Get(scope_id);
    const ScopeNameNode& name_node = *scopes_.Get(scope_id).name_node;
    AppendOwnedChildConstruction(
        unit_lowerer, ctor_frame, parent_handle, scope.source_name.value_or(""),
        name_node.class_id,
        mir::ClassFieldTarget{
            .owner = mir::IntraUnitClassRef{class_id_},
            .slot = name_node.borrowed_handle},
        {});
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

  // The callable each subroutine lowered to, recorded where it is created so an
  // export below names its own by identity, indexed by that subroutine's id.
  std::vector<mir::CallableId> subroutine_callables;
  subroutine_callables.reserve(hir_scope.structural_subroutines.size());
  for (const hir::StructuralSubroutineId sub_id :
       hir_scope.structural_subroutines.Ids()) {
    const auto& src = hir_scope.structural_subroutines.Get(sub_id);
    const DeclaredCallable& declared = declared_subroutines_.Get(sub_id);
    // A subroutine's body keeps the name the source declared it under, which is
    // what its symbol is spelled from; a referrer spells the same name on the
    // published method that enters it.
    mir_class.named_callables.push_back(
        mir::NamedCallable{.name = src.name, .body = declared.callable});
    ProcessLowerer subroutine_lowerer(
        unit_lowerer, this, hir_scope.time_resolution, src.body, src.root_stmt,
        ctor_frame, scopes_, declared.statics);
    auto code_or = subroutine_lowerer.Run(src);
    if (!code_or) return std::unexpected(std::move(code_or.error()));
    mir_class.callables.Define(
        declared.callable, mir::CallableDecl{
                               .code = *std::move(code_or),
                               .foreign = std::nullopt,
                               .virtual_dispatch = std::nullopt});
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
  // unit owns the C symbol, since a DPI-C name is program-global and never a
  // class member (LRM 35.4, 35.7). The scope answers the name with that entry.
  std::vector<mir::NamedCallable> exports;
  exports.reserve(hir_scope.foreign_exports.size());
  for (const hir::ForeignExportDecl& export_decl : hir_scope.foreign_exports) {
    const mir::CallableId method_id =
        subroutine_callables[export_decl.subroutine.value];
    const mir::TypeId method_result_type =
        mir_class.callables.Get(method_id).code.result_type;
    // The subroutine is compiled once per specialization of this scope while
    // the DPI-C name is one program-global symbol, so the scope publishes the
    // entry and the symbol resolves against whichever scope the foreign call
    // chain established. So the entry is the scope's own: it takes the scope
    // as its first argument and is reached only as a code address, while the
    // symbol dispatching to it stays the unit's.
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
    const mir::CallableId entry_id = mir_class.callables.Add(
        mir::CallableDecl{
            .code = std::move(entry.code),
            .foreign = std::nullopt,
            .virtual_dispatch = std::nullopt});
    exports.push_back(
        mir::NamedCallable{
            .name = std::move(entry.linkage.foreign_name), .body = entry_id});
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
    // A final procedure runs when the simulation ends, and every other kind
    // starts with the scope (LRM 9.2).
    constexpr support::BuiltinFn kStarts = support::BuiltinFn::kRegisterInitial;
    const support::BuiltinFn registration = std::visit(
        Overloaded{
            [](const hir::InitialProcess&) { return kStarts; },
            [](const hir::FinalProcess&) {
              return support::BuiltinFn::kRegisterFinal;
            },
            [](const hir::AlwaysProcess&) { return kStarts; },
            [](const hir::AlwaysFfProcess&) { return kStarts; },
            [](const hir::AlwaysCombProcess&) { return kStarts; },
            [](const hir::AlwaysLatchProcess&) { return kStarts; }},
        p.kind);
    AppendProcessRegistration(unit_lowerer, activate_frame, body, registration);
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
    AppendProcessRegistration(
        unit_lowerer, activate_frame, body,
        support::BuiltinFn::kRegisterInitial);
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
    AppendProcessRegistration(
        unit_lowerer, activate_frame, body,
        support::BuiltinFn::kRegisterInitial);
  }

  // The classes this scope replicates, lowered against it: their bodies reach
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
      AppendProcessRegistration(
          unit_lowerer, activate_frame, process,
          support::BuiltinFn::kRegisterInitial);
    }
    installed_assertions.emplace_back(id, *installed);
  }

  // Recurse into descendants. Every class's shape is already published, so a
  // body that names a peer's member resolves through the existing identity
  // model regardless of which sibling lowers next.
  for (const auto& blocks : generate_children_) {
    for (const auto& child : blocks) {
      auto child_r = child->PopulateBodies(ctor_frame);
      if (!child_r) return std::unexpected(std::move(child_r.error()));
    }
  }

  for (const hir::GenerateId gen : hir_scope.generates.Ids()) {
    auto stmt = LowerGenerateAsStmt(
        *this, ctor_frame, hir_scope.generates.Get(gen),
        generate_bindings_.Get(gen));
    if (!stmt) return std::unexpected(std::move(stmt.error()));
    ctor_block.AppendStmt(*std::move(stmt));
  }

  auto instances_r = EmitInstanceMemberConstruction(*this, ctor_frame);
  if (!instances_r) return std::unexpected(std::move(instances_r.error()));
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
  for (const hir::ValueTarget& sampled : hir_scope.sampled_cells) {
    const mir::ExprId cell = BuildObservableCellExpr(
        activate_block, activate_frame, unit_lowerer.Unit(),
        std::as_const(*this), sampled);
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
  ctor_code.params.reserve(1 + ctor_prefix_local_ids.size() + supplied.size());
  ctor_code.params.push_back(self_id);
  for (const mir::LocalId id : ctor_prefix_local_ids) {
    ctor_code.params.push_back(id);
  }
  for (const SuppliedLocal& handed : supplied) {
    ctor_code.params.push_back(handed.local);
  }
  ctor_code.result_type = void_type;
  // Ctor code stays local so subsequent lowering can still append exprs into
  // its body; once complete, it is moved into the class's method storage and
  // referenced by the construction protocol.

  auto& unit = unit_lowerer.Unit();

  // The three bodies the runtime drives this scope through after construction
  // (LRM 23.3.3.2 / 6.8 / 9.2), each overriding the virtual function of the
  // library's scope for its phase. `self` is the body's own receiver, typed as
  // this class. None of them answers to a name of the source, because such a
  // name would sit in the same name space as the scope's own subroutines and a
  // method spelled the same way would then share it. A phase with no work is a
  // body with no statements, not an absent body: whether emitting one is worth
  // avoiding is a question for the layer that removes dead work, and answering
  // it here would cost every consumer a case.
  const auto add_body = [&](mir::CallableCode& code, mir::LocalId self,
                            support::LibraryVirtual phase) {
    code.params = {self};
    code.result_type = void_type;
    mir_class.callables.Add(
        mir::CallableDecl{
            .code = std::move(code),
            .foreign = std::nullopt,
            .virtual_dispatch =
                mir::OverridesLibraryVirtual{.function = phase}});
  };
  add_body(
      resolve_code, resolve_self_id, support::LibraryVirtual::kScopeResolve);
  install_block.AppendStmt(
      mir::BlockStmt{
          .scope =
              install_block.child_scopes.Add(std::move(initialize_block))});
  WrapInScopeStaticInitExtent(unit_lowerer, install_frame, initialize_code);
  add_body(
      initialize_code, init_self_id, support::LibraryVirtual::kScopeInitialize);
  add_body(
      activate_code, activate_self_id,
      support::LibraryVirtual::kScopeCreateProcesses);

  // The object is of this class, which is the only one that knows it, so its
  // constructor hands the published class the definition the tree is told.
  const mir::ExprId definition = BuildDefinitionRead(
      unit, ctor_code.Body(), mir::IntraUnitClassRef{.class_id = class_id_});
  FinalizeConstructor(
      unit, mir_class, std::move(ctor_code), ctor_prefix_local_ids,
      {definition});

  StateScopeDefinition(unit, class_id_, mir_class, exports);
  unit.DefineClass(class_id_, std::move(mir_class));
  unit_lowerer.AddRealization(
      published_class_id_,
      ScopeRealization{
          .id = class_id_, .subroutines = std::move(published_subroutines_)});
  return {};
}

// What the unit published passes the definition it was handed on to the tree.
// Every referrer names that class through the unit's signature, and no instance
// is built of it alone, so it tells the library nothing of its own.
void StructuralScopeLowerer::DefinePublishedClasses(UnitLowerer& unit_lowerer) {
  mir::CompilationUnit& unit = unit_lowerer.Unit();
  for (const auto& [id, scope] : unit_lowerer.PublishedScopes()) {
    BuiltPublishedClass published =
        BuildPublishedClass(unit, unit_lowerer.GetClassShape(id), scope);
    FinalizeConstructor(
        unit, published.cls, std::move(published.ctor), published.ctor_prefix,
        {});
    StateNamedClassDefinition(unit, published.cls);
    unit.DefineClass(id, std::move(published.cls));
  }
}

}  // namespace lyra::lowering::hir_to_mir
