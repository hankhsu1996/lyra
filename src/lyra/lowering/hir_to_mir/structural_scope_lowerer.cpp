#include "lyra/lowering/hir_to_mir/structural_scope_lowerer.hpp"

#include <algorithm>
#include <array>
#include <cstddef>
#include <cstdint>
#include <expected>
#include <format>
#include <iterator>
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
#include "lyra/diag/source_manager.hpp"
#include "lyra/hir/procedural_scope.hpp"
#include "lyra/hir/procedural_var.hpp"
#include "lyra/hir/scope_instantiations.hpp"
#include "lyra/hir/structural_scope.hpp"
#include "lyra/hir/subroutine.hpp"
#include "lyra/lowering/hir_to_mir/access_path.hpp"
#include "lyra/lowering/hir_to_mir/binding_origin.hpp"
#include "lyra/lowering/hir_to_mir/block_builder.hpp"
#include "lyra/lowering/hir_to_mir/callable_bindings.hpp"
#include "lyra/lowering/hir_to_mir/cast_lowering.hpp"
#include "lyra/lowering/hir_to_mir/class_code.hpp"
#include "lyra/lowering/hir_to_mir/class_definition.hpp"
#include "lyra/lowering/hir_to_mir/class_shape.hpp"
#include "lyra/lowering/hir_to_mir/closure_builder.hpp"
#include "lyra/lowering/hir_to_mir/concurrent_assertion.hpp"
#include "lyra/lowering/hir_to_mir/condition.hpp"
#include "lyra/lowering/hir_to_mir/continuous_assign.hpp"
#include "lyra/lowering/hir_to_mir/covergroup_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/declaration_initializer.hpp"
#include "lyra/lowering/hir_to_mir/declared_initial_value.hpp"
#include "lyra/lowering/hir_to_mir/default_value.hpp"
#include "lyra/lowering/hir_to_mir/design_root_inputs.hpp"
#include "lyra/lowering/hir_to_mir/expression/dpi_call.hpp"
#include "lyra/lowering/hir_to_mir/library_entry_body.hpp"
#include "lyra/lowering/hir_to_mir/net_declaration.hpp"
#include "lyra/lowering/hir_to_mir/path_select.hpp"
#include "lyra/lowering/hir_to_mir/predicate.hpp"
#include "lyra/lowering/hir_to_mir/process_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/runtime_call.hpp"
#include "lyra/lowering/hir_to_mir/sampled_history.hpp"
#include "lyra/lowering/hir_to_mir/select_position.hpp"
#include "lyra/lowering/hir_to_mir/self_ref.hpp"
#include "lyra/lowering/hir_to_mir/sensitivity_wait.hpp"
#include "lyra/lowering/hir_to_mir/snapshot_local.hpp"
#include "lyra/lowering/hir_to_mir/statement/loops.hpp"
#include "lyra/lowering/hir_to_mir/static_var_binding.hpp"
#include "lyra/lowering/hir_to_mir/switch_stmt.hpp"
#include "lyra/lowering/hir_to_mir/unit_object_access.hpp"
#include "lyra/lowering/hir_to_mir/value_operations.hpp"
#include "lyra/lowering/hir_to_mir/walk_frame.hpp"
#include "lyra/mir/class.hpp"
#include "lyra/mir/class_ref.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/expr_builders.hpp"
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

// Adds what places every scope -- its parent and its hierarchy segment -- as
// ordinary ctor params, in the order its constructor's base call hands them
// on: to its base's constructor, or, for a class extending the root of the
// design hierarchy, to the scope's own part. The parent is taken as the base
// every scope extends: a block reaches the scope that built it only through
// what that scope published, whichever body either is.
void AttachRuntimeScopeCtorPrefix(
    const mir::CompilationUnit& unit, ClassShape& shape) {
  shape.ctor_prefix_params.Add(mir::ParamDecl{.type = unit.builtins.scope_ptr});
  shape.ctor_prefix_params.Add(
      mir::ParamDecl{.type = unit.builtins.hierarchy_segment});
}

// Where the parent stands among the prefix a scope's constructor takes.
constexpr mir::ParamId kParentPrefix{0};

// The borrowed pointer a body of a class this unit declares names its object
// by.
auto BorrowedPointerTo(mir::CompilationUnit& unit, mir::ClassId cls)
    -> mir::TypeId {
  const mir::TypeId object =
      unit.types.Intern(mir::Type{mir::ObjectType{.of = cls}});
  return unit.types.Intern(
      mir::Type{mir::PointerType{
          .pointee = object, .ownership = mir::PointerOwnership::kBorrowed}});
}

// A parameter an instantiation may leave to its default is handed as whether a
// value was assigned (LRM 23.10.2.2) beside the value, which is its type's
// default where none was: what C++'s `std::optional` holds, a flag and a slot
// for the value. A union of MIR's is a declaration the source states, which
// this is not, so the two cross as a product, the same type in every unit.
constexpr base::ComponentIndex kHasValue{0};
constexpr base::ComponentIndex kValue{1};

auto OptionalOf(mir::CompilationUnit& unit, mir::TypeId value) -> mir::TypeId {
  return unit.types.Intern(
      mir::Type{mir::TupleType{.elements = {unit.builtins.bit1, value}}});
}

// The values whoever constructs the scope supplies, in the order they are
// supplied, each with the type the constructor takes it as: the index a loop
// generate builds a block at (LRM 27.4), or each parameter a unit's instance
// is assigned or left to its default (LRM 23.10.2).
auto ConstructionValuesOf(
    UnitLowerer& unit_lowerer, const hir::StructuralScope& scope)
    -> std::vector<ConstructionValue> {
  std::vector<ConstructionValue> supplied;
  for (const hir::StructuralDataObjectId id :
       scope.structural_data_objects.Ids()) {
    const hir::StructuralDataObjectDecl& decl =
        scope.structural_data_objects.Get(id);
    std::visit(
        Overloaded{
            [&](const hir::StructuralConstructionValueDecl&) {
              supplied.push_back(
                  ConstructionValue{
                      .declared = id,
                      .type = unit_lowerer.TranslateType(decl.type)});
            },
            [&](const hir::StructuralParameterDecl&) {
              supplied.push_back(
                  ConstructionValue{
                      .declared = id,
                      .type = OptionalOf(
                          unit_lowerer.Unit(),
                          unit_lowerer.TranslateType(decl.type))});
            },
            [](const hir::StructuralVariableDecl&) {},
            [](const hir::StructuralNetDecl&) {},
            [](const hir::StructuralReferenceDecl&) {},
            [](const hir::StructuralGenvarDecl&) {},
            [](const hir::StructuralLocalparamDecl&) {}},
        decl.kind);
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
// object in the runtime's tree, with a virtual method per published subroutine
// that it leaves to the classes realizing the scope. A referrer calls one
// through the object, whose class says which body runs.
//
// It is this class rather than a realization that stands directly in the
// tree, so what the tree's class is entered with is entered here.
auto BuildPublishedClass(
    const mir::CompilationUnit& unit, const ClassShape& shape,
    const PublishedScope& published) -> BuiltPublishedClass {
  mir::CallableCode ctor = mir::CallableCode::Defined();
  const mir::LocalId ctor_self = ctor.AddLocal(shape.self_pointer_type);
  std::vector<mir::LocalId> prefix;
  for (const mir::TypeId type :
       {unit.builtins.scope_ptr, unit.builtins.hierarchy_segment}) {
    prefix.push_back(ctor.AddLocal(type));
  }
  ctor.params = {ctor_self};
  ctor.params.insert(ctor.params.end(), prefix.begin(), prefix.end());
  ctor.receiver = ctor_self;
  ctor.result_type = unit.builtins.void_type;

  mir::Class cls = shape.OpenClass();
  for (const hir::PublishedCallableId at : published.subroutines.Ids()) {
    const PublishedSubroutine& subroutine = published.subroutines.Get(at);
    const mir::CallableId id = PublishedMethodOf(at);
    cls.callables.Define(
        id, mir::CallableDecl{
                .code = subroutine.prototype,
                .foreign = std::nullopt,
                .virtual_dispatch = mir::IntroducesVirtualSlot{.is_pure = true},
                .defining_unit = std::nullopt});
    cls.named_callables.push_back(
        mir::NamedCallable{.name = subroutine.name, .body = id});
  }
  for (const ClassCodeSlot& code : published.class_code) {
    cls.callables.Define(
        code.slot,
        mir::CallableDecl{
            .code = SlotPrototype(code.operation, shape.self_pointer_type),
            .foreign = std::nullopt,
            .virtual_dispatch = mir::IntroducesVirtualSlot{.is_pure = true},
            .defining_unit = std::nullopt});
    cls.named_callables.push_back(
        mir::NamedCallable{.name = code.name, .body = code.slot});
  }

  return BuiltPublishedClass{
      .cls = std::move(cls),
      .ctor = std::move(ctor),
      .ctor_prefix = std::move(prefix)};
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
                      {block.ReadLocal(sequence, sequence_type), element}},
          .type = sequence_type});
  block.AppendStmt(
      mir::ExprStmt{
          .expr = block.exprs.Add(
              mir::MakeAssignExpr(
                  unit.builtins, block.LocalPlace(sequence, sequence_type),
                  grown))});
}

// The machine integer each of `indices`, values the design computes, names,
// which is how an index crosses to the library.
auto MachineIndices(
    const mir::CompilationUnit& unit, mir::Block& block,
    std::span<const mir::ExprId> indices) -> std::vector<mir::ExprId> {
  std::vector<mir::ExprId> numbers;
  numbers.reserve(indices.size());
  for (const mir::ExprId index : indices) {
    numbers.push_back(BuildToInt64Call(unit, block, index));
  }
  return numbers;
}

// What a scope adds to the hierarchical name of everything inside it (LRM
// 23.6): `name`, which arrives as such a name writes it, and one index per
// dimension it is an element of, each a machine integer. A scope standing
// alone has no index, and one the source gave no name has an empty name, which
// keeps it off every hierarchical name the run reports.
auto BuildHierarchySegment(
    const mir::CompilationUnit& unit, mir::Block& block,
    const std::string& name, std::vector<mir::ExprId> numbers) -> mir::ExprId {
  const mir::TypeId numbers_type = mir::MachineArrayOf(
      unit.types, unit.builtins.machine_int64, numbers.size());
  const mir::ExprId indices_id = block.exprs.Add(
      mir::Expr{
          .data = mir::CompositeExpr{.parts = std::move(numbers)},
          .type = numbers_type});
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

// The construction table the object under construction in `frame` was
// handed.
auto ReadConstructionTable(
    const mir::CompilationUnit& unit, const WalkFrame& frame) -> mir::Expr {
  const BodyBindingRef table =
      frame.bindings->EnsureCarrier(ConstructionTableOrigin{});
  mir::Block& block = *frame.current_block;
  const mir::TypeId type = mir::ConstructionTablePointer(unit.types);
  mir::Expr place = frame.bindings->MakeBindingPlace(table, block);
  place.type = type;
  return mir::MakeCopyExpr(block.exprs.Add(std::move(place)), type);
}

// What the instantiation at `at`, a machine integer, of the table the object
// under construction in `frame` was handed says, by the library entry `read`
// that answers it.
auto ReadInstantiation(
    const mir::CompilationUnit& unit, const WalkFrame& frame,
    support::BuiltinFn read, mir::ExprId at, mir::TypeId type) -> mir::ExprId {
  mir::Block& block = *frame.current_block;
  const mir::ExprId table = block.exprs.Add(ReadConstructionTable(unit, frame));
  return block.exprs.Add(
      mir::Expr{
          .data =
              mir::CallExpr{
                  .callee = mir::Direct{.target = read},
                  .arguments = {table, at}},
          .type = type});
}

// Where the object of the instance member `member` that `coords` name stands
// among the member's objects, counted in row-major order, as an `int`.
auto RowMajorPosition(
    const mir::CompilationUnit& unit, mir::Block& block,
    const hir::InstanceMemberDecl& member, std::span<const mir::LocalId> coords)
    -> mir::ExprId {
  mir::ExprId at = mir::BuildIntLiteral(unit, block, 0);
  for (std::size_t d = 0; d < coords.size(); ++d) {
    const mir::ExprId scaled = block.exprs.Add(
        mir::MakeBinary(
            unit, block, mir::BinaryOp::kMul, at,
            mir::BuildIntLiteral(
                unit, block,
                static_cast<std::int64_t>(member.array_dims[d].ElementCount())),
            unit.builtins.int_type));
    at = block.exprs.Add(
        mir::MakeBinary(
            unit, block, mir::BinaryOp::kAdd, scaled,
            block.ReadLocal(coords[d], unit.builtins.int_type),
            unit.builtins.int_type));
  }
  return at;
}

// The type of a unit's object entry: where the object hangs, its segment and
// the construction table it is built with, then each parameter it is handed,
// assigned or left to its default, at the type `handed` gives it, answering an
// owning pointer to the object, `made`. The holder building one and the design
// root listing it both state it from here, so the two cannot disagree.
auto ObjectEntryType(
    UnitLowerer& unit_lowerer, std::span<const hir::TypeId> handed,
    mir::TypeId made) -> mir::TypeId {
  mir::CompilationUnit& unit = unit_lowerer.Unit();
  std::vector<mir::TypeId> params{
      unit.builtins.scope_ptr, unit.builtins.hierarchy_segment,
      mir::ConstructionTablePointer(unit.types)};
  for (const hir::TypeId value : handed) {
    params.push_back(OptionalOf(unit, unit_lowerer.TranslateType(value)));
  }
  return unit.types.Intern(
      mir::Type{mir::MachineFunctionType{
          .params = std::move(params), .result = made}});
}

// The type of the object entry of each unit the design root's tables make,
// as that unit published it: every value it is handed, assigned or not, and an
// owning pointer to the class it published of its object.
auto ObjectEntryTypes(
    UnitLowerer& unit_lowerer,
    const std::map<std::string, PublishedObjectEntry, std::less<>>& entries)
    -> std::map<std::string, mir::TypeId, std::less<>> {
  mir::CompilationUnit& unit = unit_lowerer.Unit();
  std::map<std::string, mir::TypeId, std::less<>> types;
  for (const auto& [made, entry] : entries) {
    const mir::TypeId object =
        unit_lowerer.UnitObjectType(entry.published_class);
    const mir::TypeId owning = mir::BoxOf(unit.types, object);
    types.emplace(made, ObjectEntryType(unit_lowerer, entry.handed, owning));
  }
  return types;
}

// Builds one object an external-unit instance member declares and hands back
// the borrowed pointer the runtime tree returns. The object is built and given
// to the tree to own, under `runtime_label` and the index `indices` give it in
// each dimension of its declaration (LRM 23.3.3.5). An index is a value the
// construction counts out rather than a constant, so a declaration covering
// many objects builds them in a loop; a scalar instance is the index-free
// case, built by the same expression. `arguments` are what the object's
// constructor is passed, one per parameter it takes at construction, which
// `handed` types.
auto BuildOwnedInstance(
    UnitLowerer& unit_lowerer, const WalkFrame& frame, mir::ExprId parent_self,
    const std::string& runtime_label, mir::ExprId instantiation,
    mir::TypeId owning_pointer_type, mir::TypeId borrowed_pointer_type,
    std::span<const mir::ExprId> indices, std::span<const hir::TypeId> handed,
    std::vector<mir::Expr> arguments) -> mir::ExprId {
  mir::CompilationUnit& unit = unit_lowerer.Unit();
  mir::Block& block = *frame.current_block;
  const auto& builtins = unit.builtins;
  const mir::ExprId segment_id = BuildHierarchySegment(
      unit, block, runtime_label, MachineIndices(unit, block, indices));

  // This unit read what the instantiated one published, which states what may
  // be reached and never how much storage an object takes, so the object is
  // asked for rather than made here. Which body is made at the instantiation
  // is decided where the program is assembled, so the entry is read off the
  // table and converted back to the prototype its unit declares it with.
  // The entry takes its parent as the base every scope extends, being
  // compiled by a unit that knows no class of this one.
  const mir::TypeId entry_type =
      ObjectEntryType(unit_lowerer, handed, owning_pointer_type);
  const mir::TypeId table_type = mir::ConstructionTablePointer(unit.types);
  const mir::ExprId at = EvaluatedOnce(frame, instantiation);
  std::vector<mir::ExprId> entry_arguments{
      ObjectAs(block, parent_self, builtins.scope_ptr), segment_id,
      ReadInstantiation(
          unit, frame, support::BuiltinFn::kInstantiationTable, at,
          table_type)};
  for (mir::Expr& argument : arguments) {
    entry_arguments.push_back(block.exprs.Add(std::move(argument)));
  }
  const mir::ExprId erased = ReadInstantiation(
      unit, frame, support::BuiltinFn::kInstantiationEntry, at,
      mir::ErasedFunction(unit.types));
  const mir::ExprId entry = block.exprs.Add(
      mir::Expr{.data = mir::CastExpr{.operand = erased}, .type = entry_type});
  const mir::ExprId ctor_call_id = block.exprs.Add(
      mir::Expr{
          .data =
              mir::CallExpr{
                  .callee = mir::Indirect{.code = entry},
                  .arguments = std::move(entry_arguments)},
          .type = owning_pointer_type});

  // The runtime tree owns the instance, under the parent it was built with,
  // and hands back a borrowed handle, which is what the parent keeps and what
  // a layout-visible route step projects through.
  const mir::ExprId add_id = block.exprs.Add(
      mir::Expr{
          .data =
              mir::CallExpr{
                  .callee =
                      mir::Direct{.target = support::BuiltinFn::kAttachScope},
                  .arguments = {ctor_call_id}},
          .type = builtins.scope_ptr});
  return ObjectAs(block, add_id, borrowed_pointer_type);
}

// The arguments an instance's construction passes: for each parameter, the
// value assigned, or none, where the instantiation left it to its default,
// which the instance evaluates itself. Each is evaluated where the instance is
// built, once per object, in the block that builds it.
auto LowerConstructorArguments(
    StructuralScopeLowerer& lowerer, const WalkFrame& frame,
    std::span<const hir::ParameterValueAssignment> assignments)
    -> diag::Result<std::vector<mir::Expr>> {
  UnitLowerer& unit_lowerer = lowerer.Owner();
  mir::Block& block = *frame.current_block;
  std::vector<mir::Expr> lowered;
  lowered.reserve(assignments.size());
  for (const hir::ParameterValueAssignment& assigned : assignments) {
    const mir::TypeId value_type = unit_lowerer.TranslateType(assigned.type);
    const mir::TypeId passed = OptionalOf(unit_lowerer.Unit(), value_type);
    const auto handed = [&](bool was_assigned, mir::ExprId value) {
      return mir::Expr{
          .data =
              mir::CompositeExpr{
                  .parts =
                      {mir::BuildBit1Literal(
                           unit_lowerer.Unit(), block, was_assigned),
                       value}},
          .type = passed};
    };
    if (!assigned.value.has_value()) {
      lowered.push_back(handed(
          false, block.exprs.Add(BuildDefaultValueExpr(
                     unit_lowerer.Unit(), block, value_type))));
      continue;
    }
    auto value =
        lowerer.LowerExpr(lowerer.HirScope().exprs.Get(*assigned.value), frame);
    if (!value) return std::unexpected(std::move(value.error()));
    lowered.push_back(handed(true, block.exprs.Add(*std::move(value))));
  }
  return lowered;
}

// Builds one object of the instance member `id` as its alternative `which`
// says, at the positions `coords` count out, and hands back the handle the
// member holds it by. Which specialization the object is, is this object's
// construction, so its entry is the member's first in the table plus where it
// stands among the member's objects.
auto BuildAlternative(
    StructuralScopeLowerer& lowerer, const WalkFrame& frame,
    hir::InstanceMemberId id, std::uint32_t which,
    std::span<const mir::LocalId> coords, mir::TypeId held)
    -> diag::Result<mir::ExprId> {
  UnitLowerer& unit_lowerer = lowerer.Owner();
  mir::Block& block = *frame.current_block;
  const hir::InstanceMemberDecl& member =
      lowerer.HirScope().instance_members.Get(id);
  const hir::InstanceAlternative& alternative = member.alternatives[which];
  const mir::TypeId borrowed = MakeExternalUnitPointer(
      unit_lowerer, alternative, mir::PointerOwnership::kBorrowed);
  const mir::ExprId parent_self = block.exprs.Add(MakeSelfRefExpr(frame));
  auto arguments =
      LowerConstructorArguments(lowerer, frame, alternative.assignments);
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
                    .lhs = mir::BuildIntLiteral(
                        unit, block, member.array_dims[d].LowestIndex()),
                    .rhs = block.ReadLocal(coords[d], unit.builtins.int_type)},
            .type = unit.builtins.int_type}));
  }
  std::vector<hir::TypeId> handed;
  handed.reserve(alternative.assignments.size());
  for (const hir::ParameterValueAssignment& assigned :
       alternative.assignments) {
    handed.push_back(assigned.type);
  }
  const mir::ExprId lhs = mir::BuildIntLiteral(
      unit, block, hir::InstantiationIndexOf(lowerer.HirScope(), id).value);
  const mir::ExprId rhs = RowMajorPosition(unit, block, member, coords);
  const mir::ExprId entry_at = BuildToInt64Call(
      unit, block,
      block.exprs.Add(
          mir::MakeBinary(
              unit, block, mir::BinaryOp::kAdd, lhs, rhs,
              unit.builtins.int_type)));
  const mir::ExprId built = BuildOwnedInstance(
      unit_lowerer, frame, parent_self, member.instance_name, entry_at,
      MakeExternalUnitPointer(
          unit_lowerer, alternative, mir::PointerOwnership::kUnique),
      borrowed, indices, handed, *std::move(arguments));
  return ObjectAs(block, built, held);
}

// Appends to `frame`'s block the construction of the object of the instance
// array `id` that the positions `coords` name, and its append to the sequence
// the local `sequence` holds: the alternative its position takes, its position
// counted in row-major order. An array whose objects are all built alike builds
// that one with no test at all. Otherwise the choice is a switch over the
// position, each alternative taken at the positions that take it and the one
// the last position takes at every other, so it stands as flat however many
// times the alternatives change along the array:
//
//   switch (position) {
//     case p0, p1, ...: append(alternative a); ...
//     default: append(the last position's alternative)
//   }
auto AppendElement(
    StructuralScopeLowerer& lowerer, const WalkFrame& frame,
    hir::InstanceMemberId id, std::span<const mir::LocalId> coords,
    mir::TypeId held, mir::LocalId sequence, mir::TypeId sequence_type)
    -> diag::Result<void> {
  const mir::CompilationUnit& unit = lowerer.Owner().Unit();
  const hir::InstanceMemberDecl& member =
      lowerer.HirScope().instance_members.Get(id);
  mir::Block& block = *frame.current_block;
  const std::uint32_t otherwise = member.taken.back();
  // The positions each other alternative is taken at, in the order the
  // alternatives first appear along the array.
  std::vector<std::pair<std::uint32_t, std::vector<std::int64_t>>> taken_at;
  for (std::size_t at = 0; at < member.taken.size(); ++at) {
    const std::uint32_t which = member.taken[at];
    if (which == otherwise) {
      continue;
    }
    auto listed = std::ranges::find(
        taken_at, which,
        &std::pair<std::uint32_t, std::vector<std::int64_t>>::first);
    if (listed == taken_at.end()) {
      listed =
          taken_at.insert(taken_at.end(), {which, std::vector<std::int64_t>{}});
    }
    listed->second.push_back(static_cast<std::int64_t>(at));
  }
  // `in` with the object alternative `which` builds appended to the sequence.
  const auto kept_in = [&](mir::Block& in,
                           std::uint32_t which) -> diag::Result<void> {
    auto built = BuildAlternative(
        lowerer, frame.WithBlock(&in), id, which, coords, held);
    if (!built) return std::unexpected(std::move(built.error()));
    AppendToSequence(unit, in, sequence, sequence_type, *built);
    return {};
  };
  if (taken_at.empty()) {
    return kept_in(block, otherwise);
  }

  std::vector<ValueArm> arms;
  arms.reserve(taken_at.size());
  for (const auto& [which, positions] : taken_at) {
    arms.push_back(
        ValueArm{
            .values = positions,
            .body = [&kept_in,
                     which](const WalkFrame&) -> diag::Result<mir::Block> {
              mir::Block arm;
              if (auto kept = kept_in(arm, which); !kept) {
                return std::unexpected(std::move(kept.error()));
              }
              return arm;
            }});
  }
  mir::Block last;
  if (auto kept = kept_in(last, otherwise); !kept) {
    return std::unexpected(std::move(kept.error()));
  }
  const mir::ExprId position = BuildToInt64Call(
      unit, block, RowMajorPosition(unit, block, member, coords));
  auto choice = BuildValueSwitch(frame, position, arms, std::move(last));
  if (!choice) return std::unexpected(std::move(choice.error()));
  block.AppendStmt(*std::move(choice));
  return {};
}

// Builds what an instance declaration's member holds once `coords` are fixed as
// far as they go: the handle to its one object where it declares no dimension,
// and otherwise the sequence the next dimension counts out.
// Counting a dimension out is what the object graph does at construction, so
// the work reaches the target as a loop over one body rather than as one
// expression per element, and how many objects a declaration covers is not
// something the artifact grows with. The sequence a member holds is complete
// when the member receives it, which is why the one that grows is a local the
// steps below own and nothing else can name.
auto BuildInstanceMemberValue(
    StructuralScopeLowerer& lowerer, const WalkFrame& frame,
    hir::InstanceMemberId id, mir::TypeId held,
    std::vector<mir::LocalId>& coords) -> diag::Result<mir::ExprId> {
  UnitLowerer& unit_lowerer = lowerer.Owner();
  const hir::InstanceMemberDecl& member =
      lowerer.HirScope().instance_members.Get(id);
  mir::Block& block = *frame.current_block;
  // A declaration of no dimension builds one object, which is the one
  // alternative it takes.
  if (member.array_dims.empty()) {
    return BuildAlternative(
        lowerer, frame, id, member.taken.front(), coords, held);
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
  // The last dimension counts out objects, and any other the sequences of the
  // one inside it.
  const diag::Result<void> kept =
      coords.size() == member.array_dims.size()
          ? AppendElement(
                lowerer, element_frame, id, coords, held, sequence,
                sequence_type)
          : BuildInstanceMemberValue(lowerer, element_frame, id, held, coords)
                .transform([&](mir::ExprId inner) {
                  AppendToSequence(
                      unit, element_block, sequence, sequence_type, inner);
                });
  coords.pop_back();
  if (!kept) return std::unexpected(kept.error());

  const mir::BlockId element_scope =
      body.child_scopes.Add(std::move(element_block));
  body.AppendStmt(BuildCountingLoopStmt(
      unit, steps.Frame(), body,
      mir::BuildIntLiteral(unit, body, static_cast<std::int64_t>(count)),
      position, element_scope));

  return block.exprs.Add(steps.Build(body.LocalPlace(sequence, sequence_type)));
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
    auto value_or = BuildInstanceMemberValue(lowerer, frame, id, held, coords);
    if (!value_or) return std::unexpected(std::move(value_or.error()));
    const mir::ExprId value = *value_or;
    const mir::ExprId member = block.exprs.Add(
        mir::MakeFieldAccessExpr(
            BuildObjectDeref(
                unit_lowerer.Unit(), block,
                block.exprs.Add(MakeSelfRefExpr(frame))),
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
struct InPublishedScope {};

using RoutePlace = std::variant<InOwnScope, InPublishedScope>;

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
          // Above the reader's own scope a route stands on what each scope
          // published, so only the reader's own scope is read in its layout.
          [&](const hir::InUnitBase& ib) {
            const mir::ExprId expr = BuildEnclosingScopeReceiver(
                frame, unit, mir::EnclosingHops{.value = ib.hops.value});
            if (ib.hops.value > 0) {
              return ReachedPlace{.expr = expr, .place = InPublishedScope{}};
            }
            return ReachedPlace{.expr = expr, .place = InOwnScope{&lowerer}};
          },
          [&](const hir::EnclosingScopeBase& eb) {
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
                                            frame.HopsToUnitRoot()))},
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
                .place = InPublishedScope{}};
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

// Descends one element onto an instance the scope `from` stands in declares:
// the parent's handle on it, selected. Its body is another compilation unit,
// still reached by a typed pointer, but what it declares is that unit's to
// state, so the route stops resolving names against a scope of this one.
auto StepToOwnedChild(
    UnitLowerer& unit_lowerer, mir::Block& block, const ReachedPlace& from,
    hir::InstanceMemberId names, std::span<const std::uint32_t> selects)
    -> ReachedPlace {
  const StructuralScopeLowerer& scope = OwnScopeOf(from, "StepToOwnedChild");
  return ReachedPlace{
      .expr = SelectedMember(
          unit_lowerer, block, from, scope.InstanceMemberField(names), selects),
      .place = InPublishedScope{}};
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
          unit_lowerer, block, from, scope.TranslateInterfacePort(port),
          selects),
      .place = InPublishedScope{}};
}

// Projects the borrowed-pointer value the slot takes out of a typed place: the
// field access, addressed. Everything a scope's bodies declare with a lifetime
// longer than an activation is a field of the scope's object, so the place is
// already standing where the field is.
auto AddressTypedLeaf(
    UnitLowerer& unit_lowerer, mir::Block& block, const ReachedPlace& from,
    const mir::ClassFieldTarget& field, mir::TypeId slot_type) -> mir::ExprId {
  const mir::ExprId object =
      BuildObjectDeref(unit_lowerer.Unit(), block, from.expr);
  const mir::TypeId field_type = FieldTypeOf(unit_lowerer, field);
  const mir::ExprId access =
      block.exprs.Add(mir::MakeFieldAccessExpr(object, field, field_type));
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
                scope.TranslateStructuralDataObject(l.object), pointer_type);
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
          [&](const hir::PublishedMemberLeaf& l) {
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
          [&](const hir::PublishedDisableTargetLeaf& l) {
            return ReachPublishedDisableTarget(
                unit_lowerer, block, reached.expr, l);
          }},
      leaf);
}

// Walks from the base to whatever the last step lands on, which is a scope of
// the elaborated tree. What the walk is for -- reaching something the scope
// holds, or asking the scope a name -- is the caller's, so the walk ends here.
//
// The selects a route's steps were written with belong to the scope holding
// the route, which evaluates them where the walk is written.
auto BuildRouteWalk(
    const StructuralScopeLowerer& lowerer, const WalkFrame& frame,
    const hir::RouteBase& base, std::span<const hir::PathStep> steps)
    -> diag::Result<ReachedPlace> {
  UnitLowerer& unit_lowerer = lowerer.Owner();
  mir::Block& block = *frame.current_block;
  const SelectEvaluator evaluate_select = EvaluateSelectsIn(lowerer, frame);
  ReachedPlace reached = BuildRouteAnchor(lowerer, frame, base);
  for (const hir::PathStep& step : steps) {
    auto next = std::visit(
        Overloaded{
            [&](hir::InstanceMemberId owned) -> diag::Result<ReachedPlace> {
              return StepToOwnedChild(
                  unit_lowerer, block, reached, owned, step.selects);
            },
            [&](const hir::InterfacePortId& port)
                -> diag::Result<ReachedPlace> {
              return StepThroughInterfacePort(
                  unit_lowerer, block, reached, port, step.selects);
            },
            // What another unit published belongs to that unit, so the route
            // has left this artifact.
            [&](const hir::PublishedScopeRef& published)
                -> diag::Result<ReachedPlace> {
              auto stepped = StepThroughPublished(
                  unit_lowerer, block, reached.expr, published, step.selects,
                  evaluate_select);
              if (!stepped) return std::unexpected(std::move(stepped.error()));
              return ReachedPlace{
                  .expr = *stepped, .place = InPublishedScope{}};
            }},
        step.names);
    if (!next) return std::unexpected(std::move(next.error()));
    reached = *std::move(next);
  }
  return reached;
}

// Composes what a route ends at: the walk above, then the leaf materialized
// where it landed. Appends to the frame's block and returns the value, for
// whoever uses it there -- a slot being filled, or an access.
template <typename Leaf>
auto BuildRouteValue(
    const StructuralScopeLowerer& lowerer, const WalkFrame& frame,
    const hir::Route<Leaf>& route) -> diag::Result<mir::ExprId> {
  auto reached = BuildRouteWalk(lowerer, frame, route.base, route.steps);
  if (!reached) return std::unexpected(std::move(reached.error()));
  return MaterializeLeaf(
      lowerer.Owner(), *frame.current_block, *reached, route.leaf);
}

// Stores `value` into the scope's own `slot`. Every slot a scope settles is
// filled this way, once, and read directly at every use afterwards.
void FillScopeSlot(
    const mir::CompilationUnit& unit, const WalkFrame& frame, mir::FieldId slot,
    mir::ExprId value) {
  mir::Class& mir_class = *frame.current_class.cls;
  mir::Block& block = *frame.current_block;
  const mir::TypeId slot_type = mir_class.fields.Get(slot).type;
  const mir::ExprId self = block.exprs.Add(MakeSelfRefExpr(frame));
  const mir::ExprId target = block.exprs.Add(
      mir::MakeFieldAccessExpr(
          BuildObjectDeref(unit, block, self),
          mir::ClassFieldTarget{.owner = frame.current_class.id, .slot = slot},
          slot_type));
  const mir::ExprId assign =
      block.exprs.Add(mir::MakeAssignExpr(unit.builtins, target, value));
  block.AppendStmt(mir::ExprStmt{.expr = assign});
}

// Walks every stored route of one use where the object tree is whole, so what
// its slot holds is what the route ends at rather than anything it had to walk
// to reach it. A climbed route has no slot to fill.
template <typename Leaf, typename Id>
auto InstallStoredRoutes(
    const StructuralScopeLowerer& lowerer, const WalkFrame& resolve_frame,
    const base::Arena<hir::Route<Leaf>, Id>& routes) -> diag::Result<void> {
  for (const Id hir_id : routes.Ids()) {
    const RouteReach& reach = lowerer.ReachOf(hir_id);
    auto installed = std::visit(
        Overloaded{
            [](const ClimbedRoute&) -> diag::Result<void> { return {}; },
            [&](const StoredRoute& stored) -> diag::Result<void> {
              auto value =
                  BuildRouteValue(lowerer, resolve_frame, routes.Get(hir_id));
              if (!value) return std::unexpected(std::move(value.error()));
              FillScopeSlot(
                  lowerer.Owner().Unit(), resolve_frame, stored.slot, *value);
              return {};
            }},
        reach);
    if (!installed) return installed;
  }
  return {};
}

// Fills every slot the scope's walks keep, once the object tree is whole.
auto InstallScopeRoutes(
    const StructuralScopeLowerer& lowerer, const WalkFrame& resolve_frame)
    -> diag::Result<void> {
  const hir::ScopeRoutes& routes = lowerer.HirScope().routes;
  auto values = InstallStoredRoutes(lowerer, resolve_frame, routes.values);
  if (!values) return values;
  auto objects = InstallStoredRoutes(lowerer, resolve_frame, routes.objects);
  if (!objects) return objects;
  return InstallStoredRoutes(lowerer, resolve_frame, routes.disable_targets);
}

// The scope the execution's reports name.
auto OwningScope(const WalkFrame& frame) -> mir::ExprId {
  mir::Block& block = *frame.current_block;
  return block.Read(block.exprs.Add(MakeSelfRefExpr(frame)));
}

// The instance whose text creates an execution, which holds the seeds LRM
// 18.14.1 splits a static process's generator from. It is the scope the
// artifact's own class tree is rooted at, a fixed number of steps out from
// wherever the execution is declared, so the call reaches it by typed
// navigation over a distance the walk already knows.
auto CreatingInstance(UnitLowerer& unit_lowerer, const WalkFrame& frame)
    -> mir::ExprId {
  return BuildEnclosingScopeReceiver(
      frame, unit_lowerer.Unit(), frame.HopsToUnitRoot());
}

// The call of `body`, a callable of the scope `frame` stands on, answering
// `result`.
auto CallOnScope(
    UnitLowerer& unit_lowerer, const WalkFrame& frame, mir::CallableId body,
    mir::TypeId result) -> mir::ExprId {
  mir::Block& block = *frame.current_block;
  const mir::ExprId body_self = BuildObjectDeref(
      unit_lowerer.Unit(), block, block.exprs.Add(MakeSelfRefExpr(frame)));
  return block.exprs.Add(
      mir::Expr{
          .data =
              mir::CallExpr{
                  .callee =
                      mir::Direct{
                          .target =
                              mir::CallableTarget{
                                  .owner = frame.current_class.id,
                                  .slot = body},
                          .receiver = body_self},
                  .arguments = {}},
          .type = result});
}

// The call of a process body, whose result is the execution it runs as.
auto StartedBody(
    UnitLowerer& unit_lowerer, const WalkFrame& frame, mir::CallableId body)
    -> mir::ExprId {
  return CallOnScope(
      unit_lowerer, frame, body, unit_lowerer.Unit().builtins.coroutine_void);
}

// Where the source writes this execution, as "file:line:col". A run prints it
// when it has to say what was running, as when it is stopped from outside or
// names the procedures that keep a time step from ever ending. Empty where no
// source text states one.
auto WrittenAtText(
    UnitLowerer& unit_lowerer, mir::Block& block, diag::SourceSpan written_at)
    -> mir::ExprId {
  return block.exprs.Add(
      mir::MakeStringLiteral(
          unit_lowerer.Unit().builtins.machine_c_string,
          diag::RuntimeOriginOf(written_at, unit_lowerer.SourceManager())));
}

// Registers a continuous driver, which, alone among static processes, may be
// created by one instance's text and evaluate another's code: the drive across
// a child's output port is created by the instantiation and reads the child's
// port (LRM 4.9.6). It is seeded from the instance whose text creates it and
// runs in the region set of `runs_as` (LRM 18.14.1, 24.3.2).
void AppendDriverRegistration(
    UnitLowerer& unit_lowerer, const WalkFrame& activate_frame,
    mir::ExprId runs_as, mir::CallableId body, diag::SourceSpan written_at) {
  mir::Block& block = *activate_frame.current_block;
  block.AppendStmt(
      mir::ExprStmt{
          .expr = block.exprs.Add(
              mir::MakeCallExpr(
                  mir::Direct{
                      .target = support::BuiltinFn::kRegisterContinuousDriver},
                  {OwningScope(activate_frame),
                   CreatingInstance(unit_lowerer, activate_frame), runs_as,
                   StartedBody(unit_lowerer, activate_frame, body),
                   WrittenAtText(unit_lowerer, block, written_at)},
                  unit_lowerer.Unit().builtins.void_type))});
}

// Registers `body` in the scope's `activate` body through `registration`,
// which says which turn it starts in with the scope (LRM 9.2).
void AppendProcessRegistration(
    UnitLowerer& unit_lowerer, const WalkFrame& activate_frame,
    mir::CallableId body, support::BuiltinFn registration,
    diag::SourceSpan written_at) {
  activate_frame.current_block->AppendStmt(
      mir::ExprStmt{
          .expr = BuildExecutionStart(
              unit_lowerer, activate_frame, body, registration, written_at)});
}

// Registers `body`, a final procedure of the scope, to be called at the end of
// simulation (LRM 9.2.3). It is a function rather than a process, so the run is
// handed a call of it on this scope, which it keeps until then, as a C library
// keeps what `atexit` is handed.
void AppendFinalRegistration(
    UnitLowerer& unit_lowerer, const WalkFrame& activate_frame,
    mir::CallableId body, diag::SourceSpan written_at) {
  mir::CompilationUnit& unit = unit_lowerer.Unit();
  mir::Block& block = *activate_frame.current_block;
  ClosureBuilder call(unit, activate_frame, support::ClosureKind::kFn);
  call.Body().AppendStmt(
      mir::ExprStmt{
          .expr = CallOnScope(
              unit_lowerer, call.Frame(), body, unit.builtins.void_type)});
  const mir::ExprId kept = BuildKeptClosure(unit, block, call.BuildVoid());
  block.AppendStmt(
      mir::ExprStmt{
          .expr = block.exprs.Add(
              mir::MakeCallExpr(
                  mir::Direct{.target = support::BuiltinFn::kRegisterFinal},
                  {OwningScope(activate_frame),
                   CreatingInstance(unit_lowerer, activate_frame), kept,
                   WrittenAtText(unit_lowerer, block, written_at)},
                  unit.builtins.void_type))});
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
  const WalkFrame in_extent = init_frame.WithBlock(&extent);
  const mir::EnclosingHops hops = init_frame.HopsToUnitRoot();
  const mir::ExprId receiver =
      BuildEnclosingScopeReceiver(in_extent, unit_lowerer.Unit(), hops);
  AppendRuntimeEffectStmt(
      unit_lowerer, extent, support::BuiltinFn::kEnterScopeStaticInit,
      {receiver});

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
auto InstallInterfacePortConnection(
    StructuralScopeLowerer& lowerer, const WalkFrame& resolve_frame,
    const hir::InterfacePortConnection& conn) -> diag::Result<void> {
  UnitLowerer& unit_lowerer = lowerer.Owner();
  mir::Block& block = *resolve_frame.current_block;
  // What the member holds is a handle on each object it stands for, which is
  // the same function of its declared type the declaring unit built it from.
  const hir::DataCell port = hir::CellOf(conn.endpoint.leaf);
  const mir::TypeId member_type = unit_lowerer.MemberCellType(
      unit_lowerer.TranslateType(port.type), port.storage);
  auto nav = BuildRouteValue(lowerer, resolve_frame, conn.endpoint);
  if (!nav) return std::unexpected(std::move(nav.error()));
  const mir::ExprId target =
      block.Deref(unit_lowerer.Unit().types, *nav, member_type);

  std::vector<mir::ExprId> handles;
  handles.reserve(conn.peers.size());
  for (const hir::ObjectRoute& peer : conn.peers) {
    auto handle = BuildRouteValue(lowerer, resolve_frame, peer);
    if (!handle) return std::unexpected(std::move(handle.error()));
    handles.push_back(*handle);
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
  return {};
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
  const PathPositions within = BitsWithinOwner(unit, block, operand.part);
  return BuildToInt64Call(
      unit, block,
      BuildPositionSum(
          unit, block, within.first,
          BuildConstantPosition(
              unit, block, static_cast<std::int64_t>(offset))));
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
  const mir::ExprId here = OwnerPlace(coupling.here->part);
  const mir::ExprId there =
      block.exprs.Add(mir::MakeAddressOfExpr(other, net_ptr_type));
  const mir::ExprId here_position =
      PositionWithinNet(unit, block, *coupling.here, coupling.here_offset);
  const mir::ExprId there_position =
      PositionWithinNet(unit, block, *coupling.there, coupling.there_offset);
  const mir::ExprId width =
      mir::BuildMachineIntLiteral(unit, block, coupling.width);
  return mir::Stmt{
      .label = std::nullopt,
      .data = mir::ExprStmt{
          .expr = block.exprs.Add(
              mir::MakeNetJoinCallExpr(
                  here, there, here_position, there_position, width,
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

// Whether what an input is connected to is storage the input can stand for:
// the whole of one variable of the input's own type, written as its name. The
// connection is a continuous assignment into the input (LRM 23.3.3), and when
// the sink sees a change of the source is not fixed (LRM 4.8), so a sink that
// is the source's own storage is one of the schedules the standard admits. A
// net takes no store, since what it shows is what its drivers determine (LRM
// 6.5), and any other expression has no storage to stand for.
auto NamesSharedStorage(
    const hir::StructuralScope& scope, const hir::Expr& peer,
    hir::TypeId port_type) -> bool {
  const auto* primary = std::get_if<hir::PrimaryExpr>(&peer.data);
  const auto* named = primary == nullptr
                          ? nullptr
                          : std::get_if<hir::RoutedValueRef>(&primary->data);
  if (named == nullptr || peer.type != port_type) return false;
  const hir::DataCell source =
      hir::CellOf(scope.routes.values.Get(named->id).leaf);
  return std::visit(
      Overloaded{
          [](const hir::VariableStorage&) { return true; },
          [](const hir::ReferenceStorage&) { return true; },
          [](const hir::NetStorage&) { return false; },
          [](const hir::BorrowedObjectStorage&) { return false; }},
      source.storage);
}

// Binds an input whose member owns no cell (LRM 23.3.3.2), while the design's
// references are resolved. Where the connection names storage the input can
// stand for, the member is bound to it and nothing runs afterwards. Every
// other input is bound to a cell this scope keeps, which the implied
// continuous assignment drives where there is a connection, and which
// otherwise holds the data type's default initial value.
auto BindInputPort(
    StructuralScopeLowerer& lowerer, const WalkFrame& frame,
    const WalkFrame& resolve_frame, const WalkFrame& init_frame,
    const WalkFrame& activate_frame, const hir::ValueRoute& route,
    const hir::DataPortConnection& data) -> diag::Result<void> {
  UnitLowerer& unit_lowerer = lowerer.Owner();
  mir::CompilationUnit& unit = unit_lowerer.Unit();
  const hir::StructuralScope& hir_scope = lowerer.HirScope();
  mir::Class& mir_class = *frame.current_class.cls;
  mir::Block& resolve_block = *resolve_frame.current_block;

  const hir::DataCell member = hir::CellOf(route.leaf);
  const mir::TypeId value_type = unit_lowerer.TranslateType(member.type);
  const mir::TypeId ref_type =
      unit_lowerer.MemberCellType(value_type, member.storage);
  const auto bind_to = [&](const AccessPath& storage) -> diag::Result<void> {
    auto nav = BuildRouteValue(lowerer, resolve_frame, route);
    if (!nav) return std::unexpected(std::move(nav.error()));
    const mir::ExprId target =
        resolve_block.Deref(unit_lowerer.Unit().types, *nav, ref_type);
    resolve_block.AppendStmt(
        mir::ExprStmt{
            .expr = resolve_block.exprs.Add(
                mir::MakeCallExpr(
                    mir::Direct{.target = support::BuiltinFn::kBindMember},
                    {resolve_block.AsValue(unit.types, target),
                     PathReference(unit, resolve_block, storage)},
                    unit.builtins.void_type))});
    return {};
  };

  if (data.peer.has_value()) {
    const hir::Expr& peer = hir_scope.exprs.Get(*data.peer);
    if (NamesSharedStorage(hir_scope, peer, member.type)) {
      auto named = lowerer.LowerLhsExpr(peer, resolve_frame);
      if (!named) return std::unexpected(std::move(named.error()));
      return bind_to(*named);
    }
  }

  const mir::TypeId cell_type = mir::ObservableCellOf(unit.types, value_type);
  const mir::FieldId kept =
      mir_class.fields.Add(mir::FieldDecl{.type = cell_type});
  const auto kept_cell = [&](const WalkFrame& in) -> diag::Result<AccessPath> {
    mir::Block& block = *in.current_block;
    const mir::ExprId self = block.exprs.Add(MakeSelfRefExpr(in));
    return AccessPath{
        .owner = block.exprs.Add(
            mir::MakeFieldAccessExpr(
                BuildObjectDeref(unit, block, self),
                mir::ClassFieldTarget{
                    .owner = in.current_class.id, .slot = kept},
                cell_type)),
        .descent = {}};
  };

  mir::Block& ctor_block = *frame.current_block;
  const mir::ExprId prototype =
      BuildDeclaredInitialValue(unit_lowerer, frame, member.type);
  ctor_block.AppendStmt(
      mir::ExprStmt{
          .expr = ctor_block.exprs.Add(
              mir::MakeCapabilityInstallCallExpr(
                  OwnerPlace(*kept_cell(frame)), prototype,
                  support::BuiltinFn::kInitialize, unit.builtins.void_type))});
  auto bound = bind_to(*kept_cell(resolve_frame));
  if (!bound) return bound;
  if (!data.peer.has_value()) return {};

  const hir::Expr& source = hir_scope.exprs.Get(*data.peer);
  auto drive = LowerContinuousDrive(
      lowerer, frame, resolve_frame, init_frame,
      [&](const WalkFrame& in) -> diag::Result<Lvalue> {
        auto cell = kept_cell(in);
        if (!cell) return std::unexpected(std::move(cell.error()));
        return Lvalue{
            .form = *std::move(cell),
            .type = value_type,
            .source_type = member.type,
            .span = source.span};
      },
      source, support::StrengthLevel::kStrong, std::nullopt, data.sensitivity);
  if (!drive) return std::unexpected(std::move(drive.error()));
  const mir::ExprId runs_as = CreatingInstance(unit_lowerer, activate_frame);
  const mir::CallableId body = mir_class.callables.Add(*std::move(drive));
  AppendDriverRegistration(
      unit_lowerer, activate_frame, runs_as, body, source.span);
  return {};
}

// Realizes each connection that carries data across the boundary (LRM 23.3.3).
// A port whose member has a cell of its own is the implied continuous
// assignment between the two cells, materialized as the same synthesized
// process a scope-level `assign` produces, registered as a process; when the
// driven side is a net the edge attaches a driver rather than writing the cell.
// A `ref` port carries no edge and is emitted into the resolve block instead,
// binding the child's reference member -- reached by the route to it -- as
// another name of what the connection names: one statement, with no second
// cell and no continuous assignment.
//
// A bidirectional port carries no data in either direction and states no
// direction at all, so it is not one of these; what it states is which
// positions resolve together, which is a join.
auto InstallPortConnections(
    StructuralScopeLowerer& lowerer, WalkFrame frame, WalkFrame resolve_frame,
    WalkFrame init_frame, WalkFrame activate_frame) -> diag::Result<void> {
  mir::Class& mir_class = *frame.current_class.cls;
  mir::Block& resolve_block = *resolve_frame.current_block;
  const hir::StructuralScope& hir_scope = lowerer.HirScope();
  UnitLowerer& unit_lowerer = lowerer.Owner();
  for (const hir::PortConnectionId id : hir_scope.port_connections.Ids()) {
    const hir::PortConnection& pc = hir_scope.port_connections.Get(id);
    if (const auto* iface =
            std::get_if<hir::InterfacePortConnection>(&pc.kind)) {
      auto installed =
          InstallInterfacePortConnection(lowerer, resolve_frame, *iface);
      if (!installed) return installed;
      continue;
    }
    const auto& data = std::get<hir::DataPortConnection>(pc.kind);
    // A `ref` port binds once and is done, and a bidirectional one joins once;
    // the two value directions share the reactive edge built below and differ
    // only in which end of it drives (LRM 23.3.3).
    switch (data.direction) {
      case hir::PortDirection::kInput:
        if (const auto* route = std::get_if<hir::ValueRoute>(&data.endpoint)) {
          auto bound = BindInputPort(
              lowerer, frame, resolve_frame, init_frame, activate_frame, *route,
              data);
          if (!bound) return std::unexpected(std::move(bound.error()));
          continue;
        }
        break;
      case hir::PortDirection::kOutput:
        break;
      case hir::PortDirection::kInOut:
        throw InternalError(
            "InstallPortConnections: a bidirectional connection carries no "
            "data across the boundary and is recorded as the join it is, so "
            "it never reaches the connection switch");
      case hir::PortDirection::kRef: {
        // A `ref` port reaches the child's reference member by the same route
        // navigation any value reference uses, then binds it as another name of
        // what the peer names (LRM 23.3.3.2): where the peer is an input port,
        // the `ref` port goes on naming whatever that port names. It holds no
        // persistent slot -- a `ref` needs no simulation-time reach, so the
        // member is reached once here in the resolve phase.
        const auto& route = std::get<hir::ValueRoute>(data.endpoint);
        const hir::DataCell member = hir::CellOf(route.leaf);
        const mir::TypeId ref_type = unit_lowerer.MemberCellType(
            unit_lowerer.TranslateType(member.type), member.storage);
        auto nav = BuildRouteValue(lowerer, resolve_frame, route);
        if (!nav) return std::unexpected(std::move(nav.error()));
        const mir::ExprId target =
            resolve_block.Deref(unit_lowerer.Unit().types, *nav, ref_type);

        // The peer is lent as a `ref` actual is. A part of a variable is not
        // yet: the member is bound before the variable takes its declared
        // initial value, and storing a whole value may move the parts of the
        // one it replaces, which the reference would go on naming.
        auto peer_or = lowerer.LowerLhsExpr(
            hir_scope.exprs.Get(*data.peer), resolve_frame);
        if (!peer_or) return std::unexpected(std::move(peer_or.error()));
        if (!peer_or->descent.empty()) {
          return diag::Fail(
              pc.span, diag::DiagCode::kUnsupportedExpressionForm,
              "a 'ref' port connected to an element or a member of a variable "
              "is not yet supported");
        }
        resolve_block.AppendStmt(
            mir::ExprStmt{
                .expr = resolve_block.exprs.Add(
                    mir::MakeCallExpr(
                        mir::Direct{.target = support::BuiltinFn::kBindAlias},
                        {resolve_block.AsValue(
                             unit_lowerer.Unit().types, target),
                         PathReference(
                             unit_lowerer.Unit(), resolve_block, *peer_or)},
                        unit_lowerer.Unit().builtins.void_type))});
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
        .lhs = is_input ? cell.cell : *data.peer,
        .rhs = is_input ? *data.peer : cell.cell,
        .strength = support::StrengthLevel::kStrong,
        .delay = std::nullopt,
        .sensitivity_list = data.sensitivity};
    auto method_or = LowerContinuousAssign(
        lowerer, frame, resolve_frame, init_frame, assign);
    if (!method_or) return std::unexpected(std::move(method_or.error()));
    const mir::CallableId body = mir_class.callables.Add(std::move(*method_or));
    // The connection is created by this instantiation and evaluates the
    // code of the instance its source belongs to: this one for an input, the
    // child for an output, which reads the child's port (LRM 4.9.6, 24.3.2).
    mir::ExprId runs_as = CreatingInstance(unit_lowerer, activate_frame);
    if (!is_input) {
      auto child = BuildRouteWalk(
          lowerer, activate_frame, hir::InUnitBase{.hops = {}},
          std::array{hir::AsPathStep(pc.instance)});
      if (!child) return std::unexpected(std::move(child.error()));
      runs_as = ObjectAs(
          *activate_frame.current_block, child->expr,
          unit_lowerer.Unit().builtins.scope_ptr);
    }
    AppendDriverRegistration(
        unit_lowerer, activate_frame, runs_as, body, pc.span);
  }
  return {};
}

// Builds the block at place `place` of the generate `generate`, the machine
// integer naming where it stands among the generate's places, handing it
// `index`, the index a loop's block stands at, and hands back the borrowed
// pointer the runtime tree returns. Which body is built there is the object's,
// so the entry making it and the table it is built with are read off the table
// this object was handed, and the entry is converted back to the prototype
// every block entry of the generate has. The block is handed this object as
// the base every scope extends, since its code reaches this scope only through
// what the scope published.
auto BuildBlockAt(
    StructuralScopeLowerer& lowerer, const WalkFrame& frame,
    hir::GenerateId generate, mir::ExprId place, std::optional<mir::Expr> index)
    -> mir::ExprId {
  mir::CompilationUnit& unit = lowerer.Owner().Unit();
  mir::Block& block = *frame.current_block;
  const mir::ExprId at = EvaluatedOnce(frame, place);
  const auto read = [&](support::BuiltinFn which, mir::TypeId type) {
    return block.exprs.Add(
        mir::Expr{
            .data =
                mir::CallExpr{
                    .callee = mir::Direct{.target = which},
                    .arguments =
                        {block.exprs.Add(ReadConstructionTable(unit, frame)),
                         mir::BuildMachineIntLiteral(
                             unit, block, generate.value),
                         at}},
            .type = type});
  };

  std::vector<mir::ExprId> arguments{
      ObjectAs(
          block, block.exprs.Add(MakeSelfRefExpr(frame)),
          unit.builtins.scope_ptr),
      read(
          support::BuiltinFn::kBlockTable,
          mir::ConstructionTablePointer(unit.types))};
  const mir::TypeId entry_type = BlockEntryType(unit, index.has_value());
  if (index.has_value()) {
    arguments.push_back(block.exprs.Add(*std::move(index)));
  }
  const mir::ExprId entry = block.exprs.Add(
      mir::Expr{
          .data =
              mir::CastExpr{
                  .operand = read(
                      support::BuiltinFn::kBlockEntry,
                      mir::ErasedFunction(unit.types))},
          .type = entry_type});
  const mir::ExprId built = block.exprs.Add(
      mir::Expr{
          .data =
              mir::CallExpr{
                  .callee = mir::Indirect{.code = entry},
                  .arguments = std::move(arguments)},
          .type = unit.types.Get(entry_type)
                      .Get<mir::MachineFunctionType>()
                      .result});

  // The runtime tree owns the block, under this object, and hands back a
  // borrowed handle, which is what the generate's member holds.
  return block.exprs.Add(
      mir::Expr{
          .data =
              mir::CallExpr{
                  .callee =
                      mir::Direct{.target = support::BuiltinFn::kAttachScope},
                  .arguments = {built}},
          .type = unit.builtins.scope_ptr});
}

// Stores the block `built` into the member the generate `generate` holds what
// it built in, for a generate that builds at most one block per object.
void HoldBuiltBlock(
    StructuralScopeLowerer& lowerer, const WalkFrame& frame,
    hir::GenerateId generate, mir::ExprId built) {
  const mir::CompilationUnit& unit = lowerer.Owner().Unit();
  mir::Block& block = *frame.current_block;
  const mir::ClassFieldTarget& handle = lowerer.GenerateHandle(generate);
  const mir::ExprId object =
      BuildObjectDeref(unit, block, block.exprs.Add(MakeSelfRefExpr(frame)));
  const mir::TypeId field_type = FieldTypeOf(lowerer.Owner(), handle);
  const mir::ExprId member =
      block.exprs.Add(mir::MakeFieldAccessExpr(object, handle, field_type));
  block.AppendStmt(
      mir::ExprStmt{
          .expr = block.exprs.Add(
              mir::MakeAssignExpr(unit.builtins, member, built))});
}

// A loop generate builds a block at every index it counts out (LRM 27.4), the
// block at the k-th index counted at the loop's k-th place. The index is a
// declaration of this scope, so the loop's own expressions read and write it
// the way any name reaches a declaration, and each block is handed the index
// it stands at and filed in the loop's member under the machine integer that
// index names, which is both the coordinate the block's name carries and what
// a hierarchical name reaching it asks the member for (LRM 23.6).
auto LowerRepeatedGenerate(
    StructuralScopeLowerer& lowerer, WalkFrame frame, hir::GenerateId generate,
    const hir::BlocksRepeat& repeat) -> diag::Result<mir::Stmt> {
  UnitLowerer& unit_lowerer = lowerer.Owner();
  const mir::CompilationUnit& unit = unit_lowerer.Unit();
  const hir::StructuralScope& hir_scope = lowerer.HirScope();

  const mir::TypeId index_type = unit_lowerer.TranslateType(
      hir_scope.structural_data_objects.Get(repeat.variable).type);
  const mir::ClassFieldTarget index_field =
      lowerer.TranslateStructuralDataObject(repeat.variable);
  const mir::ClassFieldTarget& blocks = lowerer.GenerateHandle(generate);

  BlockBuilder steps(frame);
  mir::Block& body = steps.Body();
  const WalkFrame body_frame = steps.Frame();

  const auto index_place = [&](mir::Block& in) -> mir::ExprId {
    const mir::ExprId object = BuildObjectDeref(
        unit, in, in.exprs.Add(MakeSelfRefExpr(frame.WithBlock(&in))));
    const mir::TypeId field_type = FieldTypeOf(unit_lowerer, index_field);
    return in.exprs.Add(
        mir::MakeFieldAccessExpr(object, index_field, field_type));
  };
  const auto index_read = [&](mir::Block& in) -> mir::ExprId {
    return in.Read(in.exprs.Add(
        in.WrapperContents(unit.types, index_place(in), index_type)));
  };

  // Where the loop starts is a value it is given: LRM 27.4 spells the
  // initialization as an assignment of one and admits no other form there, so
  // the write belongs to the loop rather than to anything the source wrote.
  auto initial_or =
      lowerer.LowerExpr(hir_scope.exprs.Get(repeat.initial), body_frame);
  if (!initial_or) return std::unexpected(std::move(initial_or.error()));
  const mir::ExprId index_owner = index_place(body);
  const mir::ExprId initial = body.exprs.Add(*std::move(initial_or));
  body.AppendStmt(
      mir::ExprStmt{
          .expr = body.exprs.Add(BuildStoreExpr(
              unit_lowerer.Unit(), body,
              AccessPath{.owner = index_owner, .descent = {}}, initial))});
  const mir::TypeId place_type = unit.builtins.int_type;
  const mir::LocalId place = steps.Bindings().DeclareAnonymous(place_type);
  body.AppendStmt(
      mir::LocalDeclStmt{
          .target = place, .init = mir::BuildIntLiteral(unit, body, 0)});

  mir::Block loop_body;
  const WalkFrame loop_frame = body_frame.WithBlock(&loop_body);
  auto index = lowerer.LowerExpr(hir_scope.exprs.Get(repeat.index), loop_frame);
  if (!index) return std::unexpected(std::move(index.error()));
  const mir::ExprId number = EvaluatedOnce(
      loop_frame, BuildToInt64Call(unit, loop_body, index_read(loop_body)));
  const mir::ExprId built = BuildBlockAt(
      lowerer, loop_frame, generate,
      BuildToInt64Call(unit, loop_body, loop_body.ReadLocal(place, place_type)),
      *std::move(index));
  const mir::ExprId object = BuildObjectDeref(
      unit, loop_body, loop_body.exprs.Add(MakeSelfRefExpr(loop_frame)));
  const mir::TypeId field_type = FieldTypeOf(unit_lowerer, blocks);
  const mir::ExprId member =
      loop_body.exprs.Add(mir::MakeFieldAccessExpr(object, blocks, field_type));
  loop_body.AppendStmt(
      mir::ExprStmt{
          .expr = loop_body.exprs.Add(
              mir::Expr{
                  .data =
                      mir::CallExpr{
                          .callee =
                              mir::Direct{
                                  .target = support::BuiltinFn::kLoopBlocksAdd,
                                  .receiver = member},
                          .arguments = {number, built}},
                  .type = unit.builtins.void_type})});
  const mir::ExprId place_target = loop_body.LocalPlace(place, place_type);
  const mir::ExprId next_place = loop_body.exprs.Add(
      mir::Expr{
          .data =
              mir::BinaryExpr{
                  .op = mir::BinaryOp::kAdd,
                  .lhs = loop_body.ReadLocal(place, place_type),
                  .rhs = mir::BuildIntLiteral(unit, loop_body, 1)},
          .type = place_type});
  loop_body.AppendStmt(
      mir::ExprStmt{
          .expr = loop_body.exprs.Add(
              mir::MakeAssignExpr(unit.builtins, place_target, next_place))});
  // The step is the expression the source wrote, and it reaches the next
  // index by writing the loop's own, so it is placed for its effect and its
  // value is dropped -- every form LRM 27.4 admits for it says where the index
  // goes in exactly that way.
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
  return steps.BuildStatement();
}

auto LowerSelectionChoiceInto(
    StructuralScopeLowerer& lowerer, WalkFrame frame, hir::GenerateId generate,
    const hir::BlocksChoose& chosen, hir::SelectionChoiceId at)
    -> diag::Result<void>;

// What stands on one side of a choice, built into the block the side owns:
// nothing at all, the block at the place of the alternative the source wrote
// there, or a further choice the source wrote inside this side (LRM 27.5).
auto LowerSelectionBranchInto(
    StructuralScopeLowerer& lowerer, WalkFrame frame, hir::GenerateId generate,
    const hir::BlocksChoose& chosen, const hir::SelectionBranch& branch)
    -> diag::Result<void> {
  return std::visit(
      Overloaded{
          [](const hir::NothingStands&) -> diag::Result<void> { return {}; },
          [&](const hir::AlternativeStands& stands) -> diag::Result<void> {
            HoldBuiltBlock(
                lowerer, frame, generate,
                BuildBlockAt(
                    lowerer, frame, generate,
                    mir::BuildMachineIntLiteral(
                        lowerer.Owner().Unit(), *frame.current_block,
                        stands.position),
                    std::nullopt));
            return {};
          },
          [&](hir::SelectionChoiceId nested) -> diag::Result<void> {
            return LowerSelectionChoiceInto(
                lowerer, frame, generate, chosen, nested);
          }},
      branch);
}

// A `case` searches its items in the order the source wrote them and stops at
// the first match, taking the `default` only once every one of them has failed
// (LRM 12.5). An item is reached where the selector matches one of its own
// labels, and LRM 12.5 fixes that comparison: it succeeds only where every bit
// matches exactly, `x` and `z` included. Where every label is a value of the
// selector's type known before the run, that is a switch over the selector's
// value; otherwise the selector is read against each label in turn.
auto LowerLabelledChoiceInto(
    StructuralScopeLowerer& lowerer, WalkFrame frame, hir::GenerateId generate,
    const hir::BlocksChoose& chosen, const hir::ChoiceOnLabel& on)
    -> diag::Result<void> {
  mir::Block& block = *frame.current_block;
  const mir::CompilationUnit& unit = lowerer.Owner().Unit();
  const hir::StructuralScope& hir_scope = lowerer.HirScope();

  // The source wrote the selector once and every item is read against it, so
  // it is evaluated once, ahead of the search, and each item names the result.
  auto selector = lowerer.LowerExpr(hir_scope.exprs.Get(on.selector), frame);
  if (!selector) return std::unexpected(std::move(selector.error()));
  const mir::TypeId selector_type = selector->type;
  const mir::LocalId held = SnapshotExprToLocal(
      lowerer.Owner(), frame, block, selector_type,
      block.exprs.Add(*std::move(selector)));

  const auto branch_into =
      [&](const hir::SelectionBranch& branch) -> diag::Result<mir::Block> {
    mir::Block stands;
    auto built = LowerSelectionBranchInto(
        lowerer, frame.WithBlock(&stands), generate, chosen, branch);
    if (!built) return std::unexpected(std::move(built.error()));
    return stands;
  };

  const auto label_of = [&](hir::ExprId label) -> Evaluation {
    return [&, label](const WalkFrame& at) -> diag::Result<mir::ExprId> {
      auto written = lowerer.LowerAsPlace(hir_scope.exprs.Get(label), at);
      if (!written) return std::unexpected(std::move(written.error()));
      return at.current_block->exprs.Add(*std::move(written));
    };
  };

  std::vector<std::vector<Evaluation>> labels;
  labels.reserve(on.items.size());
  for (const hir::LabeledItem& item : on.items) {
    std::vector<Evaluation>& of_item = labels.emplace_back();
    of_item.reserve(item.labels.size());
    for (const hir::ExprId label : item.labels) {
      of_item.push_back(label_of(label));
    }
  }
  auto values = ValuesOfItems(unit, frame, labels, selector_type);
  if (!values) return std::unexpected(std::move(values.error()));

  auto otherwise = branch_into(on.otherwise);
  if (!otherwise) return std::unexpected(std::move(otherwise.error()));
  if (values->has_value()) {
    std::vector<ValueArm> items;
    items.reserve(on.items.size());
    for (std::size_t i = 0; i < on.items.size(); ++i) {
      items.push_back(
          ValueArm{.values = (**values)[i], .body = [&, i](const WalkFrame&) {
                     return branch_into(on.items[i].stands);
                   }});
    }
    auto choice = BuildCaseOverValues(
        unit, frame, held, selector_type, items, *std::move(otherwise));
    if (!choice) return std::unexpected(std::move(choice.error()));
    block.AppendStmt(*std::move(choice));
    return {};
  }

  std::vector<SearchedArm> items;
  items.reserve(on.items.size());
  for (std::size_t i = 0; i < on.items.size(); ++i) {
    SearchedArm lowering{.conditions = {}, .body = [&, i](const WalkFrame&) {
                           return branch_into(on.items[i].stands);
                         }};
    for (const Evaluation& written : labels[i]) {
      lowering.conditions.emplace_back(
          [&, written](const WalkFrame& at) -> diag::Result<mir::ExprId> {
            mir::Block& in = *at.current_block;
            auto lowered = written(at);
            if (!lowered) return lowered;
            return BuildCaseEquality(
                unit, in, in.LocalPlace(held, selector_type), *lowered);
          });
    }
    items.push_back(std::move(lowering));
  }
  auto choice = BuildArmSearch(unit, frame, items, *std::move(otherwise));
  if (!choice) return std::unexpected(std::move(choice.error()));
  block.AppendStmt(*std::move(choice));
  return {};
}

// One conditional generate construct, built as the source nested it: the
// condition is asked once and each side holds whatever the source wrote there.
auto LowerSelectionChoiceInto(
    StructuralScopeLowerer& lowerer, WalkFrame frame, hir::GenerateId generate,
    const hir::BlocksChoose& chosen, hir::SelectionChoiceId at)
    -> diag::Result<void> {
  const hir::SelectionChoice& choice = chosen.choices.Get(at);
  if (const auto* on = std::get_if<hir::ChoiceOnLabel>(&choice)) {
    return LowerLabelledChoiceInto(lowerer, frame, generate, chosen, *on);
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
      lowerer, frame.WithBlock(&holds), generate, chosen, on.holds);
  if (!taken) return std::unexpected(std::move(taken.error()));

  std::optional<mir::BlockId> otherwise;
  if (!std::holds_alternative<hir::NothingStands>(on.fails)) {
    mir::Block fails;
    auto untaken = LowerSelectionBranchInto(
        lowerer, frame.WithBlock(&fails), generate, chosen, on.fails);
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
    StructuralScopeLowerer& lowerer, WalkFrame frame, hir::GenerateId generate,
    const hir::BlocksChoose& chosen) -> diag::Result<mir::Stmt> {
  mir::Block& block = *frame.current_block;
  mir::Block body;

  auto built = LowerSelectionChoiceInto(
      lowerer, frame.WithBlock(&body), generate, chosen, chosen.root);
  if (!built) return std::unexpected(std::move(built.error()));

  return mir::Stmt{
      .label = std::nullopt,
      .data = mir::BlockStmt{.scope = block.child_scopes.Add(std::move(body))}};
}

// A block no loop and no conditional produced is built once, at its one
// place.
auto LowerSingleBlockGenerate(
    StructuralScopeLowerer& lowerer, WalkFrame frame, hir::GenerateId generate)
    -> diag::Result<mir::Stmt> {
  mir::Block& block = *frame.current_block;
  mir::Block body;
  const WalkFrame body_frame = frame.WithBlock(&body);
  HoldBuiltBlock(
      lowerer, body_frame, generate,
      BuildBlockAt(
          lowerer, body_frame, generate,
          mir::BuildMachineIntLiteral(lowerer.Owner().Unit(), body, 0),
          std::nullopt));
  return mir::Stmt{
      .label = std::nullopt,
      .data = mir::BlockStmt{.scope = block.child_scopes.Add(std::move(body))}};
}

// A generate construct becomes the construction its compiled form calls for.
auto LowerGenerateAsStmt(
    StructuralScopeLowerer& lowerer, WalkFrame frame, hir::GenerateId generate)
    -> diag::Result<mir::Stmt> {
  const hir::Generate& declared = lowerer.HirScope().generates.Get(generate);
  return std::visit(
      Overloaded{
          [&](const hir::SingleBlock&) {
            return LowerSingleBlockGenerate(lowerer, frame, generate);
          },
          [&](const hir::BlocksRepeat& repeat) {
            return LowerRepeatedGenerate(lowerer, frame, generate, repeat);
          },
          [&](const hir::BlocksChoose& chosen) {
            return LowerChosenGenerate(lowerer, frame, generate, chosen);
          }},
      declared.counting);
}

// Describes, as statements of `frame`'s block, what a value dump records of
// an instance (LRM 21.7.1.2): each declaration, as the member it is with the
// constant stating it, and each named block, task or function around the
// static variables of one. An event is described by an entry of its own, since
// what a dump records of it is when the object it names is triggered rather
// than a value it holds.
auto DescribeDumped(
    const StructuralScopeLowerer& lowerer, const WalkFrame& frame,
    const hir::DumpedMembers& dumped) -> diag::Result<void> {
  UnitLowerer& unit_lowerer = lowerer.Owner();
  mir::CompilationUnit& unit = unit_lowerer.Unit();
  mir::Block& block = *frame.current_block;
  const auto runtime = [&] {
    return block.exprs.Add(BuildCurrentRuntimeCallExpr(unit_lowerer));
  };
  const auto append = [&](mir::Expr call) {
    block.AppendStmt(mir::ExprStmt{.expr = block.exprs.Add(std::move(call))});
  };
  for (const hir::DumpedDeclaration& declaration : dumped.declarations) {
    auto path = lowerer.LowerAccessPath(
        lowerer.HirScope().exprs.Get(declaration.named), frame);
    if (!path) return std::unexpected(std::move(path.error()));
    const mir::ExprId member = OwnerPlace(*path);
    const support::BuiltinFn entry =
        OwnerValueType(unit, block, path->owner) == unit.builtins.event
            ? support::BuiltinFn::kDescribeEvent
            : support::BuiltinFn::kDescribe;
    append(
        mir::MakeCallExpr(
            mir::Direct{.target = entry, .receiver = member},
            {runtime(), DumpEntryAddress(
                            unit, block, frame.current_class.id,
                            *frame.current_class.cls, declaration)},
            unit.builtins.void_type));
  }
  for (const hir::DumpedBlock& inside : dumped.blocks) {
    append(
        mir::MakeCallExpr(
            mir::Direct{.target = support::BuiltinFn::kBeginDescribedBlock},
            {runtime(), DumpBlockAddress(
                            unit, block, frame.current_class.id,
                            *frame.current_class.cls, inside)},
            unit.builtins.void_type));
    auto inner = DescribeDumped(lowerer, frame, inside.members);
    if (!inner) return std::unexpected(std::move(inner.error()));
    append(
        mir::MakeCallExpr(
            mir::Direct{.target = support::BuiltinFn::kEndDescribedBlock},
            {runtime()}, unit.builtins.void_type));
  }
  return {};
}

}  // namespace

auto BuildExecutionStart(
    UnitLowerer& unit_lowerer, const WalkFrame& frame, mir::CallableId body,
    support::BuiltinFn start, diag::SourceSpan written_at) -> mir::ExprId {
  mir::Block& block = *frame.current_block;
  return block.exprs.Add(
      mir::MakeCallExpr(
          mir::Direct{.target = start},
          {OwningScope(frame), CreatingInstance(unit_lowerer, frame),
           StartedBody(unit_lowerer, frame, body),
           WrittenAtText(unit_lowerer, block, written_at)},
          unit_lowerer.Unit().builtins.void_type));
}

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
                    .vector =
                        block.AsValue(unit_lowerer.Unit().types, reached.expr),
                    .index = BuildSequenceIndex(unit_lowerer, block, select)},
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
      Overloaded{// A climbed route has no step to walk, so it ends where it is
                 // anchored.
                 [&](const ClimbedRoute&) {
                   return MaterializeLeaf(
                       lowerer.Owner(), *frame.current_block,
                       BuildRouteAnchor(lowerer, frame, route.base),
                       route.leaf);
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
  const hir::ObjectRoute& route = HirScope().routes.objects.Get(id);
  const RouteReach& reach = ReachOf(id);
  return EndAlong(*this, frame, route, reach);
}

auto StructuralScopeLowerer::RouteEnd(
    const WalkFrame& frame, hir::RoutedDisableTargetRefId id) const
    -> mir::ExprId {
  const hir::DisableTargetRoute& route =
      HirScope().routes.disable_targets.Get(id);
  const RouteReach& reach = ReachOf(id);
  return EndAlong(*this, frame, route, reach);
}

auto StructuralScopeLowerer::LoweringOf(
    UnitLowerer& unit_lowerer, const hir::StructuralScope& hir_scope,
    std::optional<DesignRootInputs> design_root)
    -> std::unique_ptr<StructuralScopeLowerer> {
  auto lowerer = std::make_unique<StructuralScopeLowerer>(
      unit_lowerer, hir_scope, std::move(design_root));
  lowerer->published_class_id_ =
      unit_lowerer.ScopeClassIdentity(hir_scope.published.scope_class);
  return lowerer;
}

auto StructuralScopeLowerer::ForRoot(
    UnitLowerer& unit_lowerer, const hir::StructuralScope& root_scope,
    std::optional<DesignRootInputs> design_root) -> Tree {
  Tree tree{
      .root = LoweringOf(unit_lowerer, root_scope, std::move(design_root)),
      .blocks = {}};
  const hir::CompilationUnit& unit = unit_lowerer.Hir();
  // Every scope publishing one class publishes the same class, so the class a
  // path names is read off any scope standing there; the unit's own scope is
  // the one a path leads out of last.
  std::map<support::DefPath, mir::ClassId> published_at;
  published_at.emplace(support::DefPath{}, tree.root->published_class_id_);
  tree.blocks.reserve(unit.block_bodies.size());
  for (const hir::BlockBodyId id : unit.block_bodies.Ids()) {
    std::unique_ptr<StructuralScopeLowerer>& lowering =
        tree.blocks.emplace_back(LoweringOf(
            unit_lowerer, unit.block_bodies.Get(id).scope, std::nullopt));
    published_at.emplace(
        lowering->hir_scope_->published.signature.class_path,
        lowering->published_class_id_);
  }
  // A block body stands inside the scope its class's path leads out of, and
  // that one inside the next, up to the unit's own scope.
  for (std::unique_ptr<StructuralScopeLowerer>& block : tree.blocks) {
    const support::DefPath& path =
        block->hir_scope_->published.signature.class_path;
    for (std::size_t length = 0; length < path.data.size(); ++length) {
      const support::DefPath around{
          .data = {
              path.data.begin(),
              path.data.begin() + static_cast<std::ptrdiff_t>(length)}};
      const auto found = published_at.find(around);
      if (found == published_at.end()) {
        throw InternalError(
            "StructuralScopeLowerer::ForRoot: a block body is published inside "
            "a class no scope of its unit publishes -- please report this as "
            "a bug");
      }
      block->published_around_.push_back(found->second);
    }
  }
  return tree;
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
  const mir::TypeId self_pointer_type =
      BorrowedPointerTo(unit_lowerer.Unit(), class_id_);

  ClassShape shape;
  shape.identity = unit_lowerer.NameUnnamedClass();
  // The scope stands in the tree through what its unit published of it.
  shape.base = mir::ClassRef{published_class_id_};
  shape.is_final = true;
  shape.self_pointer_type = self_pointer_type;
  shape.time_resolution = hir_scope.time_resolution;
  shape.execution = unit_lowerer.ScopeExecution();
  shape.dumped_as = published_around_.empty() ? support::DumpScopeKind::kModule
                                              : support::DumpScopeKind::kBegin;

  AttachRuntimeScopeCtorPrefix(unit_lowerer.Unit(), shape);
  // What construction supplies is what distinguishes one built scope from
  // another built from the same declarations.
  construction_values_ = ConstructionValuesOf(unit_lowerer, hir_scope);

  // What the unit published of this object is laid out from the signature it
  // published, through the one layout every referrer's record of the object
  // uses too, and each published declaration of this scope takes the field its
  // publication was given. Everything else is placed in this scope's own
  // class, after them.
  const mir::ClassId owner = published_class_id_;
  const PublishedScopeLayout& layout =
      unit_lowerer.SettlePublishedScope(owner, hir_scope.published.signature);
  const auto on_published = [&](mir::FieldId slot) {
    return mir::ClassFieldTarget{.owner = owner, .slot = slot};
  };
  if (layout.enclosing.has_value())
    enclosing_ = on_published(*layout.enclosing);

  std::vector<std::optional<mir::ClassFieldTarget>> data_object_fields(
      hir_scope.structural_data_objects.size());
  std::vector<std::optional<mir::ClassFieldTarget>> instance_fields(
      hir_scope.instance_members.size());
  std::vector<std::optional<mir::ClassFieldTarget>> interface_port_fields(
      hir_scope.interface_ports.size());
  // The cells of the published static-lifetime variables, taken over by the
  // walk binding each body's variables, keyed by that body.
  std::vector<std::vector<PlacedStatic>> placed_process_statics(
      hir_scope.processes.size());
  std::vector<std::vector<PlacedStatic>> placed_subroutine_statics(
      hir_scope.structural_subroutines.size());
  // The cells of the static properties of the classes this scope declares,
  // keyed by the class, which takes them over: the scope publishes every one
  // (LRM 8.9, 23.6), so each is where the publication put it.
  std::map<hir::ClassId, std::vector<PlacedProperty>> placed_class_statics;
  std::map<hir::CovergroupId, mir::ClassFieldTarget> covergroup_events;
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
            [&](const hir::ClassStaticPropertyTarget& property) {
              placed_class_statics[property.owner].push_back(
                  PlacedProperty{.property = property.prop, .field = field});
            },
            // The event the blocks of the instance trigger for a covergroup's
            // type is kept here, as a static property of a class it declares
            // would be (LRM 19.3, 6.22).
            [&](const hir::CovergroupBlockEvent& event) {
              covergroup_events.emplace(event.covergroup, field);
            },
            [&](hir::InstanceMemberId id) {
              instance_fields[id.value] = field;
            },
            [&](hir::InterfacePortId id) {
              interface_port_fields[id.value] = field;
            }},
        decl);
  }
  std::uint32_t published_loop = 0;
  for (const hir::GenerateId id : hir_scope.published.loops) {
    generate_handles[id.value] =
        on_published(layout.loops.Get(hir::PublishedLoopId{published_loop++}));
  }
  std::uint32_t published_choice = 0;
  for (const hir::GenerateId id : hir_scope.published.choices) {
    generate_handles[id.value] = on_published(
        layout.choices.Get(hir::PublishedChoiceId{published_choice++}));
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
        unit_lowerer.TranslateType(d.type), hir::PublishedStorageOf(d));
    data_object_fields[id.value] = mir::ClassFieldTarget{
        .owner = class_id_,
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
  // carries nothing, because how wide a position set is and which statements an
  // outcome selects are fixed by filling the storage rather than by naming it.
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

  // Every generate construct is published, so what it built is held in the
  // field its publication was given.
  generate_handles_ = {hir_scope.generates.size(), settled(generate_handles)};

  // The covergroups this scope replicates settle their shapes against this
  // one for the reason its classes do. One sampled where a block of the
  // instance begins or ends waits on the event the instance keeps for its
  // type, which those blocks trigger.
  BlockEventTriggers block_events;
  for (const hir::ReplicatedCovergroup& covergroup :
       hir_scope.replicated_covergroups) {
    CovergroupLowerer& lowerer =
        covergroup_lowerers_.emplace_back(unit_lowerer, covergroup, this);
    const auto event = covergroup_events.find(covergroup.id);
    lowerer.DeclareShape(
        &shape, event == covergroup_events.end()
                    ? std::nullopt
                    : std::optional<StaticStorageHome>{
                          InstanceFieldHome{.field = event->second}});
    lowerer.AddTypeBlockEvents(block_events);
  }

  // A procedural scope is a scope of the text and of nothing at run time:
  // procedures are no branch of the hierarchy (LRM 23.6), `%m` names a block by
  // text fixed where the call is written (LRM 21.2.1.5), and everything a block
  // keeps past an activation -- its statics, what a `disable` of it ends, the
  // events a covergroup samples at it -- is one cell per instance, so a field
  // of this class.
  const base::Translation<hir::ProceduralScopeId, hir::FixedName> names =
      ReportedNames(hir_scope.procedural_scopes, unit_lowerer.Hir().fixed_name);
  std::vector<DeclaredScope> scopes;
  scopes.reserve(hir_scope.procedural_scopes.size());
  for (const hir::ProceduralScopeId scope_id :
       hir_scope.procedural_scopes.Ids()) {
    const auto& scope = hir_scope.procedural_scopes.Get(scope_id);
    DeclaredScope declared{
        .name = names.Get(scope_id),
        .disable_target = std::nullopt,
        .block_events = BlockEventsOf(block_events, scope_id)};

    // What a `disable` of this scope invalidates (LRM 9.6.2). Its targets are
    // the blocks and tasks a name reaches, so a scope the source named owns one
    // for that reason alone and one it did not owns none -- no pass has to
    // first find out which scopes some `disable` names. A scope of this
    // hierarchy is replicated with its instance, so the cell is one per
    // instance, shared by every activation of the scope -- the one placed
    // where the scope published it, where it did.
    if (disable_cells[scope_id.value].has_value()) {
      declared.disable_target =
          InstanceFieldHome{.field = *disable_cells[scope_id.value]};
    } else if (scope.source_name.has_value()) {
      declared.disable_target = DeclareStaticCell(
          InstanceStorage{.owner = class_id_, .shape = &shape, .placed = {}},
          unit_lowerer.Unit().types.Intern(
              mir::Type{mir::RuntimeLibraryType{
                  .kind = mir::RuntimeLibraryKind::kCancellationTarget}}));
    }
    scopes.push_back(std::move(declared));
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
  // A subroutine the scope published overrides the virtual method its
  // published class introduced at the position the signature gave it.
  std::vector<std::optional<mir::VirtualDispatchRole>> dispatch(
      hir_scope.structural_subroutines.size());
  for (const hir::PublishedCallableId published :
       hir_scope.published.callables.Ids()) {
    dispatch[hir_scope.published.callables.Get(published).value] =
        mir::OverridesVirtualSlot{
            .overridden = mir::VirtualSlot{
                .owner_class = published_class_id_,
                .slot = PublishedMethodOf(published)}};
  }
  base::IdAllocator<mir::CallableId> subroutine_ids;
  std::vector<DeclaredCallable> declared_subroutines;
  std::vector<CallableSignature> signatures;
  declared_subroutines.reserve(hir_scope.structural_subroutines.size());
  signatures.reserve(hir_scope.structural_subroutines.size());
  for (const hir::StructuralSubroutineId sub_id :
       hir_scope.structural_subroutines.Ids()) {
    const hir::SubroutineDecl& s = hir_scope.structural_subroutines.Get(sub_id);
    const mir::CallableId body = subroutine_ids.Take();
    signatures.push_back(
        CallableSignature{.virtual_dispatch = dispatch[sub_id.value]});
    declared_subroutines.push_back(
        DeclaredCallable{
            .callable = body,
            .statics = BindBodyStatics(
                unit_lowerer, hir_scope.procedural_scopes,
                InstanceStorage{
                    .owner = class_id_,
                    .shape = &shape,
                    .placed = placed_subroutine_statics[sub_id.value]},
                s.body)});
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
        InstanceStorage{
            .owner = class_id_,
            .shape = &shape,
            .placed = placed_process_statics[id.value]},
        hir_scope.processes.Get(id).body));
  }
  process_static_bindings_ = {
      hir_scope.processes.size(), std::move(process_statics)};

  std::vector<StaticVarBindings> assertion_statics;
  assertion_statics.reserve(hir_scope.concurrent_assertions.size());
  for (const hir::ConcurrentAssertionId id :
       hir_scope.concurrent_assertions.Ids()) {
    assertion_statics.push_back(BindBodyStatics(
        unit_lowerer, hir_scope.procedural_scopes,
        InstanceStorage{.owner = class_id_, .shape = &shape},
        hir_scope.concurrent_assertions.Get(id).action));
  }
  assertion_static_bindings_ = {
      hir_scope.concurrent_assertions.size(), std::move(assertion_statics)};

  // The classes this scope replicates settle their shapes against this one, and
  // before it is published: such a class keeps its cells here, as fields of the
  // instance, so what it places has to land while the shape is still open.
  class_lowerers_.reserve(hir_scope.replicated_classes.size());
  for (const hir::ReplicatedClass& replicated : hir_scope.replicated_classes) {
    const mir::ClassId identity = unit_lowerer.ClassIdentity(replicated.id);
    const mir::TypeId pointee = unit_lowerer.ClassPointee(replicated.id);
    class_lowerers_.emplace_back(
        unit_lowerer, replicated, identity, pointee, hir_scope, this);
  }
  for (auto&& [class_lowerer, replicated] :
       std::views::zip(class_lowerers_, hir_scope.replicated_classes)) {
    if (auto r = class_lowerer.DeclareShape(
            &shape, placed_class_statics[replicated.id]);
        !r) {
      return std::unexpected(std::move(r.error()));
    }
  }

  unit_lowerer.DefineClassShape(class_id_, std::move(shape));
  return class_id_;
}

namespace {

// Appends the construction of a scope class's base on the object -- or, where
// the class extends the root of the design hierarchy, giving the part every
// scope shares its place -- which nothing of the scope's own runs before. The
// parent is handed on as the base every scope extends, whatever class this
// scope takes it as, each other prefix is forwarded as a consuming use, then
// the trailing ones.
void ConstructBaseFirst(
    mir::CompilationUnit& unit, const mir::ClassRef& base,
    mir::CallableCode& ctor_code, mir::LocalId self,
    std::span<const mir::LocalId> prefix_local_ids) {
  mir::Block& body = ctor_code.Body();
  std::vector<mir::ExprId> base_args;
  base_args.reserve(prefix_local_ids.size());
  const mir::LocalId parent = prefix_local_ids[kParentPrefix.value];
  base_args.push_back(ObjectAs(
      body, body.ReadLocal(parent, ctor_code.locals.Get(parent).type),
      unit.builtins.scope_ptr));
  for (const mir::LocalId id :
       prefix_local_ids.subspan(kParentPrefix.value + 1)) {
    const mir::TypeId ty = ctor_code.locals.Get(id).type;
    const mir::ExprId local_ref = body.LocalPlace(id, ty);
    if (unit.types.Get(ty).IsAliasHandle()) {
      base_args.push_back(body.Read(local_ref));
    } else {
      base_args.push_back(body.exprs.Add(
          mir::Expr{.data = mir::MoveExpr{.operand = local_ref}, .type = ty}));
    }
  }
  const mir::ExprId object = BuildObjectDeref(
      unit, body, body.ReadLocal(self, ctor_code.locals.Get(self).type));
  const mir::DirectTarget target = std::visit(
      Overloaded{
          [](const mir::ClassId& extended) -> mir::DirectTarget {
            return mir::ConstructorTarget{.cls = extended};
          },
          [](const mir::ObjectTreeRootRef&) -> mir::DirectTarget {
            return support::BuiltinFn::kSetScopePlace;
          },
          [](const mir::ManagedObjectRootRef&) -> mir::DirectTarget {
            throw InternalError(
                "hir_to_mir: a scope extends the root of objects the program "
                "builds -- please report this as a bug");
          }},
      base);
  body.AppendStmt(
      mir::ExprStmt{
          .expr = body.exprs.Add(
              mir::MakeCallExpr(
                  mir::Direct{.target = target, .receiver = object},
                  std::move(base_args), unit.builtins.void_type))});
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

auto StructuralScopeLowerer::PopulateBodies() -> diag::Result<void> {
  UnitLowerer& unit_lowerer = *owner_;
  const hir::StructuralScope& hir_scope = *hir_scope_;

  const ClassShape& shape = unit_lowerer.GetClassShape(class_id_);
  mir::Class mir_class = shape.OpenClass();

  const mir::TypeId void_type = unit_lowerer.Unit().builtins.void_type;
  const mir::TypeId self_ptr_type = mir_class.self_pointer_type;
  // The scopes around this one are climbed to through what each published,
  // whichever body built it, so each is known here by its published class and
  // the link that class keeps to the scope around it.
  std::vector<ScopeChainNode> around(published_around_.size());
  const ScopeChainNode* outer = nullptr;
  for (std::size_t at = 0; at < published_around_.size(); ++at) {
    const mir::ClassId published = published_around_[at];
    const auto settled = unit_lowerer.PublishedScopes().find(published);
    if (settled == unit_lowerer.PublishedScopes().end()) {
      throw InternalError(
          "StructuralScopeLowerer::PopulateBodies: every class the unit "
          "published of a scope is laid out before any body lowers");
    }
    const std::optional<mir::FieldId>& link =
        settled->second.layout.published.enclosing;
    around[at] = ScopeChainNode{
        .outer =
            {.cls = nullptr,
             .id = published,
             .enclosing = link.transform([&](mir::FieldId slot) {
               return mir::ClassFieldTarget{.owner = published, .slot = slot};
             }),
             .published = BorrowedPointerTo(unit_lowerer.Unit(), published)},
        .parent = outer};
    outer = &around[at];
  }
  const auto seed_self = [&](CallableBindings& bindings) -> mir::LocalId {
    return bindings.Declare(ReceiverOrigin{}, self_ptr_type);
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
      WalkFrame{}
          .WithinScopes(
              {.cls = &mir_class,
               .id = class_id_,
               .enclosing = enclosing_,
               .published =
                   BorrowedPointerTo(unit_lowerer.Unit(), published_class_id_)},
              outer)
          .WithStructuralBase(ScopeIsSelf{})
          .WithScopeName(unit_lowerer.Hir().fixed_name);
  mir::Block& ctor_block = ctor_code.Body();
  const WalkFrame ctor_frame =
      scope_frame.WithBlock(&ctor_block).WithBindings(&ctor_bindings);
  ConstructBaseFirst(
      unit_lowerer.Unit(), mir_class.base.value(), ctor_code, self_id,
      ctor_prefix_local_ids);
  // The construction table follows the prefix, and what construction supplies
  // follows that. Both stay out of the base call: the base contract demands
  // the prefix and nothing this scope alone is handed. The table is what the
  // constructor reads how each of its instantiations is made from.
  const mir::LocalId construction_table = ctor_bindings.Declare(
      ConstructionTableOrigin{},
      mir::ConstructionTablePointer(unit_lowerer.Unit().types));
  struct SuppliedLocal {
    ConstructionValue value;
    mir::LocalId local;
  };
  std::vector<SuppliedLocal> supplied;
  supplied.reserve(construction_values_.size());
  // What the construction handed for each declaration it fills.
  std::map<hir::StructuralDataObjectId, SuppliedLocal> handed_for;
  for (const ConstructionValue& value : construction_values_) {
    const SuppliedLocal local{
        .value = value, .local = ctor_bindings.DeclareAnonymous(value.type)};
    supplied.push_back(local);
    handed_for.emplace(value.declared, local);
  }

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

  mir::CallableCode describe_code = mir::CallableCode::Defined();
  CallableBindings describe_bindings(unit_lowerer.Unit(), describe_code);
  const mir::LocalId describe_self_id = seed_self(describe_bindings);
  const WalkFrame describe_frame = scope_frame.WithBlock(&describe_code.Body())
                                       .WithBindings(&describe_bindings);
  const auto self_object = [&]() -> mir::ExprId {
    return BuildObjectDeref(
        unit_lowerer.Unit(), ctor_block,
        ctor_block.exprs.Add(MakeSelfRefExpr(ctor_frame)));
  };
  const auto init_self_object = [&]() -> mir::ExprId {
    return BuildObjectDeref(
        unit_lowerer.Unit(), initialize_block,
        initialize_block.exprs.Add(MakeSelfRefExpr(init_frame)));
  };

  // A cell takes its declared representation and default as the object is
  // built. What elaboration settles is read that early -- a block's own
  // declarations and connections are written in terms of the index it stands
  // at, and the loop advances that index as it builds (LRM 27.4) -- and
  // whatever names a variable from elsewhere is bound before any value is
  // assigned.
  const auto install_in_constructor =
      [&](hir::StructuralDataObjectId id) -> mir::ExprId {
    const auto& d = hir_scope.structural_data_objects.Get(id);
    const mir::ClassFieldTarget field = TranslateStructuralDataObject(id);
    const mir::ExprId object = self_object();
    const mir::TypeId field_type = FieldTypeOf(unit_lowerer, field);
    const mir::ExprId target = ctor_block.exprs.Add(
        mir::MakeFieldAccessExpr(object, field, field_type));
    ctor_block.AppendStmt(
        mir::ExprStmt{
            .expr = ctor_block.exprs.Add(
                mir::MakeCapabilityInstallCallExpr(
                    target,
                    BuildDeclaredInitialValue(unit_lowerer, ctor_frame, d.type),
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
  // Stores `value` into the cell `id` declares, as a statement of `block`.
  const auto store_into = [&](mir::Block& block, const WalkFrame& at,
                              hir::StructuralDataObjectId id, mir::Expr value) {
    const mir::ClassFieldTarget field = TranslateStructuralDataObject(id);
    const mir::ExprId object = BuildObjectDeref(
        unit_lowerer.Unit(), block, block.exprs.Add(MakeSelfRefExpr(at)));
    const mir::TypeId field_type = FieldTypeOf(unit_lowerer, field);
    const mir::ExprId target =
        block.exprs.Add(mir::MakeFieldAccessExpr(object, field, field_type));
    block.AppendStmt(
        mir::ExprStmt{
            .expr = block.exprs.Add(BuildStoreExpr(
                unit_lowerer.Unit(), block,
                AccessPath{.owner = target, .descent = {}},
                block.exprs.Add(std::move(value))))});
  };

  // A loop's index is always handed, so its block holds it from the start.
  for (const SuppliedLocal& handed : supplied) {
    if (!std::holds_alternative<hir::StructuralConstructionValueDecl>(
            hir_scope.structural_data_objects.Get(handed.value.declared)
                .kind)) {
      continue;
    }
    install_in_constructor(handed.value.declared);
    store_into(
        ctor_block, ctor_frame, handed.value.declared,
        mir::MakeCopyExpr(
            ctor_block.exprs.Add(
                mir::MakeLocalRefExpr(handed.local, handed.value.type)),
            handed.value.type));
  }

  // A constant the scope settles for itself is settled after whatever
  // construction supplied is in its cell, because such a constant may be
  // written from it, and in declaration order, because one may be written from
  // an earlier one (LRM 6.20). So is a parameter the construction hands a
  // flag and a value for: the value assigned where there is one, and otherwise
  // the default its declaration states, evaluated only then (LRM 6.20.1,
  // 23.10.2.2) -- `D = has_value ? value : W * 2`.
  for (const hir::StructuralDataObjectId id :
       hir_scope.structural_data_objects.Ids()) {
    const auto& settled_decl = hir_scope.structural_data_objects.Get(id);
    if (const auto* settled =
            std::get_if<hir::StructuralLocalparamDecl>(&settled_decl.kind)) {
      install_in_constructor(id);
      auto value =
          LowerExpr(hir_scope.exprs.Get(settled->initializer), ctor_frame);
      if (!value) return std::unexpected(std::move(value.error()));
      store_into(ctor_block, ctor_frame, id, *std::move(value));
      continue;
    }
    const auto* parameter =
        std::get_if<hir::StructuralParameterDecl>(&settled_decl.kind);
    if (parameter == nullptr) continue;
    install_in_constructor(id);
    const SuppliedLocal& handed = handed_for.at(id);
    // The component `which` of what the parameter was handed, read where the
    // constructor holds it.
    const auto read_handed = [&](base::ComponentIndex which, mir::TypeId type) {
      return mir::MakeCopyExpr(
          ctor_block.exprs.Add(
              mir::MakeFieldAccessExpr(
                  ctor_block.exprs.Add(
                      mir::MakeLocalRefExpr(handed.local, handed.value.type)),
                  mir::ComponentFieldTarget{.position = which}, type)),
          type);
    };
    const mir::TypeId value_type =
        unit_lowerer.TranslateType(settled_decl.type);
    mir::Expr assigned = read_handed(kValue, value_type);
    if (!parameter->default_value.has_value()) {
      store_into(ctor_block, ctor_frame, id, std::move(assigned));
      continue;
    }
    auto defaulted = ConditionallyEvaluated(
        ctor_frame, [&](const WalkFrame& at) -> diag::Result<mir::ExprId> {
          auto value =
              LowerExpr(hir_scope.exprs.Get(*parameter->default_value), at);
          if (!value) return std::unexpected(std::move(value.error()));
          return at.current_block->exprs.Add(*std::move(value));
        });
    if (!defaulted) return std::unexpected(std::move(defaulted.error()));
    store_into(
        ctor_block, ctor_frame, id,
        mir::Expr{
            .data =
                mir::ConditionalExpr{
                    .condition = ReduceToCondition(
                        unit_lowerer.Unit(), ctor_block,
                        ctor_block.exprs.Add(read_handed(
                            kHasValue, unit_lowerer.Unit().builtins.bit1))),
                    .then_value = ctor_block.exprs.Add(std::move(assigned)),
                    .else_value = *defaulted},
            .type = value_type});
  }

  for (const hir::StructuralDataObjectId hir_id :
       hir_scope.structural_data_objects.Ids()) {
    const auto& d = hir_scope.structural_data_objects.Get(hir_id);
    const mir::ClassFieldTarget cell = TranslateStructuralDataObject(hir_id);
    const mir::TypeId mir_field_type = FieldTypeOf(unit_lowerer, cell);
    const mir::TypeId mir_value_type = unit_lowerer.TranslateType(d.type);
    const auto* net = std::get_if<hir::StructuralNetDecl>(&d.kind);
    const auto* var = std::get_if<hir::StructuralVariableDecl>(&d.kind);
    const mir::Type& value_ty = unit_lowerer.Unit().types.Get(mir_value_type);
    // Owned children and cross-instance reference slots (borrowed pointers
    // filled in the resolve phase) have no "value assignment" -- their
    // declaration shape itself fixes the field at construction. A net takes
    // none either: its value is produced by its drivers, seeded when each
    // driver updates in the initialize phase. Variables receive an LRM 10.5
    // initialization statement, run in the initialize phase after the tree's
    // references resolve, not in the constructor.
    const bool is_assignable_value =
        var != nullptr && !IsOwnedChildOrReferenceSlot(value_ty);
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

      // Every observable value cell takes its declared representation and
      // default with the object, so a reference bound to it while the design is
      // resolved names storage that exists, and a later store -- a user
      // initializer included (LRM 10.5) -- is held to that representation
      // rather than fixing it by running first. A non-observable value member
      // carries no cell wrapper, so an ordinary store of the default is what
      // gives it its value. Either way the initializer is then stored, and may
      // read the member it initializes (LRM 10.5).
      if (unit_lowerer.Unit().types.Get(mir_field_type).IsCapabilityWrapper()) {
        install_in_constructor(hir_id);
      } else {
        emit_value_store(
            BuildDeclaredInitialValue(unit_lowerer, init_frame, d.type));
      }
      if (var->initializer.has_value()) {
        auto value_or =
            LowerExpr(hir_scope.exprs.Get(*var->initializer), init_frame);
        if (!value_or) return std::unexpected(std::move(value_or.error()));
        emit_value_store(initialize_block.exprs.Add(*std::move(value_or)));
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
      const NetData data =
          value_ty.IsIntegral()
              ? NetData{IntegralNetData{}}
              : NetData{AggregateNetData{
                    .prototype = ctor_block.exprs.Add(BuildDefaultValueFromHir(
                        unit_lowerer, ctor_block, d.type))}};
      ctor_block.AppendStmt(
          mir::Stmt{
              .label = std::nullopt,
              .data = mir::ExprStmt{
                  .expr = ctor_block.exprs.Add(BuildNetInstall(
                      unit_lowerer.Unit(), ctor_block, *net, net_target, data,
                      void_type))}});
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
                                       mir::BringUpEntry entry) {
    const mir::ExprId call = initialize_block.exprs.Add(
        mir::Expr{
            .data =
                mir::CallExpr{
                    .callee =
                        mir::Direct{
                            .target =
                                mir::ExternalBringUpTarget{
                                    .unit_name = unit_name, .entry = entry}},
                    .arguments = {}},
            .type = void_type});
    initialize_block.AppendStmt(mir::ExprStmt{.expr = call});
  };
  if (design_root_.has_value()) {
    for (const std::string& unit : design_root_->namespaces.units) {
      call_namespace_unit(unit, mir::BringUpEntry::kInstallStorage);
    }
    for (const std::string& unit : design_root_->namespaces.units) {
      call_namespace_unit(unit, mir::BringUpEntry::kInitializeStorage);
    }
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
    // what its symbol is spelled from; one the scope published overrides the
    // published class's method of that name.
    mir_class.named_callables.push_back(
        mir::NamedCallable{.name = src.name, .body = declared.callable});
    ProcessLowerer subroutine_lowerer(
        unit_lowerer, this, hir_scope.time_resolution, src.body, src.root_stmt,
        ctor_frame, scopes_, declared.statics);
    auto code_or = subroutine_lowerer.Run(
        src, CallableForm::kInstanceMember, std::nullopt);
    if (!code_or) return std::unexpected(std::move(code_or.error()));
    mir_class.callables.Define(
        declared.callable,
        mir::CallableDecl{
            .code = *std::move(code_or),
            .foreign = std::nullopt,
            .virtual_dispatch = unit_lowerer.GetClassShape(class_id_)
                                    .callable_signatures.Get(declared.callable)
                                    .virtual_dispatch,
            .defining_unit = std::nullopt});
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
    PublishForeignScopeName(unit_lowerer, entry);
    const mir::CallableId entry_id = mir_class.callables.Add(
        mir::CallableDecl{
            .code = std::move(entry.code),
            .foreign = std::nullopt,
            .virtual_dispatch = std::nullopt,
            .defining_unit = std::nullopt});
    // A foreign caller hands the entry the scope every instance is.
    exports.push_back(
        mir::NamedCallable{
            .name = std::move(entry.linkage.foreign_name),
            .body = EntryOf(
                unit_lowerer.Unit(), class_id_, mir_class, entry_id,
                unit_lowerer.Unit().builtins.scope_ptr)});
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
            .virtual_dispatch = std::nullopt,
            .defining_unit = std::nullopt});
    // A final procedure is called when the simulation ends, and every other
    // kind starts with the scope (LRM 9.2), registered as the kind it is. What
    // an `always_comb` is at time zero holds of an `always_latch` too (LRM
    // 9.2.2.3).
    using support::BuiltinFn;
    const auto starts_as = [&](BuiltinFn registration) {
      AppendProcessRegistration(
          unit_lowerer, activate_frame, body, registration, p.span);
    };
    std::visit(
        Overloaded{
            [&](const hir::InitialProcess&) {
              starts_as(BuiltinFn::kRegisterInitial);
            },
            [&](const hir::FinalProcess&) {
              AppendFinalRegistration(
                  unit_lowerer, activate_frame, body, p.span);
            },
            [&](const hir::AlwaysProcess&) {
              starts_as(BuiltinFn::kRegisterAlways);
            },
            [&](const hir::AlwaysFfProcess&) {
              starts_as(BuiltinFn::kRegisterAlways);
            },
            [&](const hir::AlwaysCombProcess&) {
              starts_as(BuiltinFn::kRegisterTriggered);
            },
            [&](const hir::AlwaysLatchProcess&) {
              starts_as(BuiltinFn::kRegisterTriggered);
            }},
        p.kind);
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
  auto routes = InstallScopeRoutes(*this, resolve_frame);
  if (!routes) return std::unexpected(std::move(routes.error()));

  for (const hir::ContinuousAssignId id : hir_scope.continuous_assigns.Ids()) {
    const hir::ContinuousAssign& assign = hir_scope.continuous_assigns.Get(id);
    auto method_or = LowerContinuousAssign(
        *this, ctor_frame, resolve_frame, init_frame, assign);
    if (!method_or) return std::unexpected(std::move(method_or.error()));
    const mir::CallableId body = mir_class.callables.Add(std::move(*method_or));
    AppendDriverRegistration(
        unit_lowerer, activate_frame,
        CreatingInstance(unit_lowerer, activate_frame), body, assign.span);
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
    // A sampler is the tool's own, kept for the sampled value functions that
    // read its history, and no one place in the source writes it.
    AppendProcessRegistration(
        unit_lowerer, activate_frame, body, support::BuiltinFn::kRegisterAlways,
        diag::SourceSpan{});
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
  for (CovergroupLowerer& covergroup_lowerer : covergroup_lowerers_) {
    auto covergroup_r = covergroup_lowerer.PopulateBodies(
        ctor_frame,
        StorageBringUp{.install = install_frame, .value = init_frame});
    if (!covergroup_r) return std::unexpected(std::move(covergroup_r.error()));
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
    const hir::ConcurrentAssertionDecl& assertion =
        hir_scope.concurrent_assertions.Get(id);
    auto installed = LowerConcurrentAssertion(
        *this, mir_class, ctor_frame,
        StorageBringUp{.install = install_frame, .value = init_frame}, scopes_,
        assertion_static_bindings_.Get(id), id, assertion);
    if (!installed) return std::unexpected(std::move(installed.error()));
    for (const mir::CallableId process : installed->processes) {
      AppendProcessRegistration(
          unit_lowerer, activate_frame, process,
          support::BuiltinFn::kRegisterAlways, assertion.span);
    }
    installed_assertions.emplace_back(id, *installed);
  }

  for (const hir::GenerateId gen : hir_scope.generates.Ids()) {
    auto stmt = LowerGenerateAsStmt(*this, ctor_frame, gen);
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
  // installs is its default sampled value -- for a static variable, the value
  // its declaration assigns (LRM 16.5.1). That value is in the cell once every
  // initializer in the design has run, which is what puts this here rather
  // than beside the initializer itself: a cell reached across an instance
  // boundary is one this scope cannot order itself against.
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
    const hir::Expr& subject = hir_scope.exprs.Get(history.subject);
    const WalkFrame sampled =
        activate_frame.WithReadsAsOf(ReadsAsOf::kPreponed);
    auto subject_or = LowerExpr(subject, sampled);
    if (!subject_or) return std::unexpected(std::move(subject_or.error()));
    const mir::ExprId value = activate_block.exprs.Add(*std::move(subject_or));
    auto depth_or =
        LowerExpr(hir_scope.exprs.Get(history.depth), activate_frame);
    if (!depth_or) return std::unexpected(std::move(depth_or.error()));
    const mir::ExprId depth = BuildToInt64Call(
        unit_lowerer.Unit(), activate_block,
        activate_block.exprs.Add(*std::move(depth_or)));
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
  ctor_code.params.reserve(2 + ctor_prefix_local_ids.size() + supplied.size());
  ctor_code.params.push_back(self_id);
  for (const mir::LocalId id : ctor_prefix_local_ids) {
    ctor_code.params.push_back(id);
  }
  ctor_code.params.push_back(construction_table);
  for (const SuppliedLocal& handed : supplied) {
    ctor_code.params.push_back(handed.local);
  }
  ctor_code.result_type = void_type;
  // Ctor code stays local so subsequent lowering can still append exprs into
  // its body; once complete, it is moved into the class's method storage and
  // referenced by the construction protocol.

  auto& unit = unit_lowerer.Unit();

  // The three bodies the runtime drives this scope through after construction
  // (LRM 23.3.3.2 / 6.8 / 9.2), and the one a value dump starting has it
  // describe itself with (LRM 21.7.1.2), each answering the library's callback
  // for its phase. `self` is the body's own receiver, typed as this class.
  // None of them answers to a name of the source, because
  // such a name would sit in the same name space as the scope's own subroutines
  // and a method spelled the same way would then share it. A phase with no work
  // is a body with no statements, not an absent body: whether emitting one is
  // worth avoiding is a question for the layer that removes dead work, and
  // answering it here would cost every consumer a case.
  const auto add_body = [&](mir::CallableCode& code, mir::LocalId self,
                            support::LibraryCallback phase) {
    code.params = {self};
    code.result_type = void_type;
    const mir::CallableId body = mir_class.callables.Add(
        mir::CallableDecl{
            .code = std::move(code),
            .foreign = std::nullopt,
            .virtual_dispatch = std::nullopt,
            .defining_unit = std::nullopt});
    mir_class.library_callbacks.push_back(
        mir::LibraryCallbackBody{.callback = phase, .body = body});
  };
  add_body(
      resolve_code, resolve_self_id, support::LibraryCallback::kScopeResolve);
  install_block.AppendStmt(
      mir::BlockStmt{
          .scope =
              install_block.child_scopes.Add(std::move(initialize_block))});
  WrapInScopeStaticInitExtent(unit_lowerer, install_frame, initialize_code);
  add_body(
      initialize_code, init_self_id,
      support::LibraryCallback::kScopeInitialize);
  add_body(
      activate_code, activate_self_id,
      support::LibraryCallback::kScopeCreateProcesses);
  auto described = DescribeDumped(*this, describe_frame, hir_scope.dumped);
  if (!described) return std::unexpected(std::move(described.error()));
  add_body(
      describe_code, describe_self_id,
      support::LibraryCallback::kScopeDescribe);

  mir_class.constructor = mir::ConstructorDecl{.code = std::move(ctor_code)};

  // The design root states every specialization's construction table, its
  // own among them, since nothing hands it one.
  if (design_root_.has_value()) {
    construction_table_ = StateConstructionTables(
        unit, class_id_, mir_class, design_root_->root,
        design_root_->instantiations,
        ObjectEntryTypes(unit_lowerer, design_root_->object_entries));
  }
  mir_class.exports = std::move(exports);
  unit.DefineClass(class_id_, std::move(mir_class));
  return {};
}

auto StructuralScopeLowerer::PublishBlockEntry() const -> mir::CallableId {
  mir::CompilationUnit& unit = owner_->Unit();
  mir::CallableCode code = mir::CallableCode::Defined();
  const mir::TypeId table_type = mir::ConstructionTablePointer(unit.types);
  const mir::LocalId parent = code.AddLocal(unit.builtins.scope_ptr);
  const mir::LocalId table = code.AddLocal(table_type);
  code.params = {parent, table};
  // A block is handed nothing but the index its loop stands at, if it is a
  // loop's, which is its one construction value.
  const mir::MachineFunctionType entry_type =
      unit.types.Get(BlockEntryType(unit, !construction_values_.empty()))
          .Get<mir::MachineFunctionType>();
  for (std::size_t i = 2; i < entry_type.params.size(); ++i) {
    code.params.push_back(code.AddLocal(entry_type.params[i]));
  }
  code.result_type = entry_type.result;

  mir::Block& body = code.Body();
  std::vector<mir::ExprId> values;
  values.reserve(construction_values_.size());
  std::vector<mir::ExprId> numbers;
  numbers.reserve(construction_values_.size());
  for (std::size_t i = 0; i < construction_values_.size(); ++i) {
    const mir::LocalId param = code.params[2 + i];
    const mir::TypeId type = entry_type.params[2 + i];
    values.push_back(body.ReadLocal(param, type));
    numbers.push_back(
        BuildToInt64Call(unit, body, body.ReadLocal(param, type)));
  }
  std::vector<mir::ExprId> arguments{
      body.ReadLocal(parent, unit.builtins.scope_ptr),
      BuildHierarchySegment(
          unit, body, hir_scope_->source_name, std::move(numbers)),
      body.ReadLocal(table, table_type)};
  std::ranges::move(values, std::back_inserter(arguments));
  const mir::TypeId object =
      unit.types.Intern(mir::Type{mir::ObjectType{.of = class_id_}});
  const mir::ExprId built = body.exprs.Add(
      mir::MakeNewExpr(
          body.exprs.Add(
              mir::MakeObjectConstructionExpr(object, std::move(arguments))),
          mir::BoxOf(unit.types, object)));
  body.AppendStmt(
      mir::ReturnStmt{.value = ObjectAs(body, built, code.result_type)});
  return unit.callables.Add(
      mir::CallableDecl{
          .code = std::move(code),
          .foreign = std::nullopt,
          .virtual_dispatch = std::nullopt,
          .defining_unit = std::nullopt});
}

// What the unit published of its object, the class every referrer names it
// by through the unit's signature. No instance is built of it alone, so it
// gives no body to any phase.
void StructuralScopeLowerer::DefinePublishedClasses(UnitLowerer& unit_lowerer) {
  mir::CompilationUnit& unit = unit_lowerer.Unit();
  for (const auto& [id, scope] : unit_lowerer.PublishedScopes()) {
    BuiltPublishedClass published =
        BuildPublishedClass(unit, unit_lowerer.GetClassShape(id), scope);
    ConstructBaseFirst(
        unit, published.cls.base.value(), published.ctor,
        published.ctor.params.front(), published.ctor_prefix);
    // A block keeps the scope that built it, which the class it published
    // holds, so whichever body the block is, a walk climbing out of it reads
    // the link where every body has it.
    if (const std::optional<mir::FieldId> enclosing =
            scope.layout.published.enclosing) {
      mir::Block& body = published.ctor.Body();
      const mir::LocalId self = published.ctor.params.front();
      const mir::ExprId target = body.exprs.Add(
          mir::MakeFieldAccessExpr(
              BuildObjectDeref(
                  unit, body,
                  body.ReadLocal(self, published.ctor.locals.Get(self).type)),
              mir::ClassFieldTarget{.owner = id, .slot = *enclosing},
              unit.builtins.scope_ptr));
      const mir::ExprId parent = body.ReadLocal(
          published.ctor_prefix.front(), unit.builtins.scope_ptr);
      body.AppendStmt(
          mir::ExprStmt{
              .expr = body.exprs.Add(
                  mir::MakeAssignExpr(unit.builtins, target, parent))});
    }
    published.cls.constructor =
        mir::ConstructorDecl{.code = std::move(published.ctor)};
    unit.DefineClass(id, std::move(published.cls));
  }
}

}  // namespace lyra::lowering::hir_to_mir
