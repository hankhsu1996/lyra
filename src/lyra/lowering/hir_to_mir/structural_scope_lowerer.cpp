#include "lyra/lowering/hir_to_mir/structural_scope_lowerer.hpp"

#include <algorithm>
#include <cstddef>
#include <cstdint>
#include <expected>
#include <format>
#include <optional>
#include <span>
#include <string>
#include <string_view>
#include <utility>
#include <variant>
#include <vector>

#include "lyra/base/internal_error.hpp"
#include "lyra/diag/diagnostic.hpp"
#include "lyra/hir/procedural_body.hpp"
#include "lyra/hir/procedural_scope.hpp"
#include "lyra/hir/procedural_var.hpp"
#include "lyra/hir/structural_scope.hpp"
#include "lyra/lowering/hir_to_mir/binding_origin.hpp"
#include "lyra/lowering/hir_to_mir/callable_bindings.hpp"
#include "lyra/lowering/hir_to_mir/continuous_assign.hpp"
#include "lyra/lowering/hir_to_mir/declaration_initializer.hpp"
#include "lyra/lowering/hir_to_mir/default_value.hpp"
#include "lyra/lowering/hir_to_mir/lhs_observable.hpp"
#include "lyra/lowering/hir_to_mir/process_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/self_ref.hpp"
#include "lyra/lowering/hir_to_mir/services_call.hpp"
#include "lyra/lowering/hir_to_mir/walk_frame.hpp"
#include "lyra/mir/base_contract.hpp"
#include "lyra/mir/class.hpp"
#include "lyra/mir/class_ref.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/local.hpp"
#include "lyra/mir/member.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/mir/type.hpp"
#include "lyra/support/builtin_fn.hpp"

namespace lyra::lowering::hir_to_mir {

namespace {

// Wrap a value type in `ObservableType` iff it is a SystemVerilog value-storage
// data type (LRM 6.5 / 7.x). Handle / wrapper types (pointer / vector / object
// / external ref / external unit object) and named events (LRM 15 -- carry
// their own subscribe mechanism) pass through unwrapped.
auto MaybeWrapObservable(ModuleLowerer& module, mir::TypeId t) -> mir::TypeId {
  const auto& data = module.Unit().types.Get(t).data;
  const bool wrap = std::holds_alternative<mir::PackedArrayType>(data) ||
                    std::holds_alternative<mir::EnumType>(data) ||
                    std::holds_alternative<mir::StringType>(data) ||
                    std::holds_alternative<mir::RealType>(data) ||
                    std::holds_alternative<mir::ShortRealType>(data) ||
                    std::holds_alternative<mir::RealTimeType>(data) ||
                    std::holds_alternative<mir::UnpackedArrayType>(data) ||
                    std::holds_alternative<mir::DynamicArrayType>(data) ||
                    std::holds_alternative<mir::QueueType>(data) ||
                    std::holds_alternative<mir::AssociativeArrayType>(data) ||
                    std::holds_alternative<mir::TupleType>(data) ||
                    std::holds_alternative<mir::UnionType>(data);
  if (!wrap) {
    return t;
  }
  return module.Unit().types.Intern(mir::ObservableType{.value = t});
}

auto CompanionVarNameFor(std::string_view child_scope_name) -> std::string {
  return std::string(child_scope_name) + "_obj";
}

void CheckNoNameCollision(
    const mir::ClassShape& owner_shape, std::string_view child_scope_name,
    std::string_view companion_var_name) {
  for (const auto& v : owner_shape.members) {
    if (v.name == companion_var_name || v.name == child_scope_name) {
      throw InternalError(
          "child class or companion var name collides with an existing "
          "member declaration in the enclosing class");
    }
  }
}

struct GenerateChildSpec {
  hir::StructuralScopeId scope_id;
  const hir::StructuralScope* scope;
  std::string scope_name;
};

// Each instantiated block is its own concrete scalar child (its own class),
// distinguished on the hierarchy only by any index it carries; the genvar is
// folded into the body, so there is no runtime structural-param binding.
auto EnumerateGenerateChildSpecs(
    const hir::Generate& gen, ModuleLowerer& module)
    -> std::vector<GenerateChildSpec> {
  std::vector<GenerateChildSpec> specs;
  specs.reserve(gen.data.items.size());
  for (const auto& item : gen.data.items) {
    const auto& item_scope = gen.child_scopes.Get(item.scope);
    specs.push_back(
        {.scope_id = item.scope,
         .scope = &item_scope,
         .scope_name = module.NextGenerateScopeName("gen")});
  }
  return specs;
}

auto MakeUniqueObjectPointer(ModuleLowerer& module, mir::ClassId class_id)
    -> mir::TypeId {
  const mir::TypeId object_type =
      module.Unit().types.Intern(mir::ObjectType{.class_id = class_id});
  return module.Unit().types.PointerTo(
      object_type, mir::PointerOwnership::kUnique);
}

auto MakeUniqueExternalUnitPointer(ModuleLowerer& module, std::string unit_name)
    -> mir::TypeId {
  const mir::TypeId object_type = module.Unit().types.Intern(
      mir::ExternalUnitObjectType{.unit_name = std::move(unit_name)});
  return module.Unit().types.PointerTo(
      object_type, mir::PointerOwnership::kUnique);
}

// Builds an external-unit member type: a unique pointer to the unit's object,
// wrapped in one vector layer per array dimension (`num_dims == 0` is a scalar
// instance). The backend materializes the nested vector by replication.
auto MakeExternalUnitMemberType(
    ModuleLowerer& module, std::string unit_name, std::size_t num_dims)
    -> mir::TypeId {
  mir::TypeId type =
      MakeUniqueExternalUnitPointer(module, std::move(unit_name));
  for (std::size_t i = 0; i < num_dims; ++i) {
    type = module.Unit().types.Intern(mir::VectorType{.element = type});
  }
  return type;
}

// Recursively emits the per-leaf construction stmts for an external-unit
// instance member with `dims` remaining at this level. `chain_expr_id` is the
// vector the next operation pushes into; `chain_type` is its MIR type. At the
// innermost level the leaf is built (`make_unique<ExtUnit>`), pushed into the
// chain, and registered on `self` for by-name lookup. At an outer level the
// recursion enumerates the current dim, pushes an empty inner vector per
// iteration, and recurses on the just-pushed inner via `vector_back`. The
// chain reads are idempotent C++ expressions, so a re-read after a push
// observes the new back element -- the recursive structure relies on that
// ordering.
void EmitExternalUnitDimLevel(
    ModuleLowerer& module, const WalkFrame& frame,
    mir::ExprId self_id_for_register, const std::string& runtime_label,
    mir::TypeId leaf_pointer_type, mir::ExprId chain_expr_id,
    mir::TypeId chain_type, std::span<const std::uint32_t> dims,
    std::vector<mir::ExprId>& indices) {
  mir::Block& block = *frame.current_block;
  const auto& builtins = module.Unit().builtins;
  const auto host_int_lit = [&](std::int64_t v) -> mir::ExprId {
    return block.exprs.Add(mir::MakeInt32Literal(builtins.int32, v));
  };
  const auto string_literal = [&](const std::string& s) -> mir::ExprId {
    return block.exprs.Add(
        mir::Expr{
            .data = mir::StringLiteral{.value = s}, .type = builtins.string});
  };
  const mir::TypeId self_ptr_type = frame.current_class->self_pointer_type;
  const auto self_read = [&]() -> mir::ExprId {
    return block.exprs.Add(MakeSelfRefExpr(frame, self_ptr_type));
  };

  if (dims.empty()) {
    // The child's structural identity, fixed before its constructor runs.
    // Indices reflect the position this leaf occupies in its enclosing
    // dim chain (empty for a scalar instance).
    const mir::TypeId indices_type = module.Unit().types.Intern(
        mir::UnpackedArrayType{
            .element_type = builtins.int32, .size = indices.size()});
    const mir::ExprId indices_id = block.exprs.Add(
        mir::Expr{
            .data = mir::ArrayLiteralExpr{.elements = indices},
            .type = indices_type});
    const mir::ExprId segment_id = block.exprs.Add(
        mir::Expr{
            .data =
                mir::CallExpr{
                    .callee = mir::Construct{},
                    .arguments = {string_literal(runtime_label), indices_id}},
            .type = builtins.hierarchy_segment});

    std::vector<mir::ExprId> ctor_args = {
        self_id_for_register, segment_id,
        block.exprs.Add(BuildServicesCallExpr(module, frame))};
    const mir::ExprId ctor_call_id = block.exprs.Add(
        mir::Expr{
            .data =
                mir::CallExpr{
                    .callee = mir::Construct{},
                    .arguments = std::move(ctor_args)},
            .type = leaf_pointer_type});

    if (indices.empty()) {
      // Scalar member: chain is the member slot itself, assign in place.
      const mir::ExprId assign_id = block.exprs.Add(
          mir::Expr{
              .data =
                  mir::AssignExpr{
                      .target = chain_expr_id, .value = ctor_call_id},
              .type = leaf_pointer_type});
      block.AppendStmt(mir::ExprStmt{.expr = assign_id});
    } else {
      const mir::ExprId emplace_id = block.exprs.Add(
          mir::Expr{
              .data =
                  mir::CallExpr{
                      .callee =
                          mir::Direct{
                              .target = support::BuiltinFn::kVectorEmplace},
                      .arguments = {chain_expr_id, ctor_call_id}},
              .type = builtins.void_type});
      block.AppendStmt(mir::ExprStmt{.expr = emplace_id});
    }

    // The just-built child is reachable via the scalar slot directly or via
    // `chain.back()` for the vector case; either way one dereference yields
    // the ExternalUnitObject reference `AttachChild` consumes.
    mir::ExprId leaf_handle_id = chain_expr_id;
    if (!indices.empty()) {
      leaf_handle_id = block.exprs.Add(
          mir::Expr{
              .data =
                  mir::CallExpr{
                      .callee =
                          mir::Direct{
                              .target = support::BuiltinFn::kVectorBack},
                      .arguments = {chain_expr_id}},
              .type = leaf_pointer_type});
    }
    const mir::TypeId leaf_object_type =
        std::get<mir::PointerType>(
            module.Unit().types.Get(leaf_pointer_type).data)
            .pointee;
    const mir::ExprId leaf_deref_id = block.exprs.Add(
        mir::Expr{
            .data = mir::DerefExpr{.pointer = leaf_handle_id},
            .type = leaf_object_type});

    const mir::ExprId attach_id = block.exprs.Add(
        mir::Expr{
            .data =
                mir::CallExpr{
                    .callee =
                        mir::Direct{.target = support::BuiltinFn::kAttachChild},
                    .arguments = {self_read(), leaf_deref_id}},
            .type = builtins.void_type});
    block.AppendStmt(mir::ExprStmt{.expr = attach_id});
    return;
  }

  const mir::TypeId inner_type =
      std::get<mir::VectorType>(module.Unit().types.Get(chain_type).data)
          .element;
  const std::span<const std::uint32_t> remaining_dims = dims.subspan(1);
  const std::uint32_t count = dims.front();
  for (std::uint32_t i = 0; i < count; ++i) {
    // Push an empty inner vector / leaf slot at this position. For an
    // intermediate level the inner is itself a vector; for the about-to-be
    // leaf level we still emplace through `EmitExternalUnitDimLevel` below.
    const mir::ExprId empty_inner_id = block.exprs.Add(
        mir::Expr{
            .data = mir::CallExpr{.callee = mir::Construct{}, .arguments = {}},
            .type = inner_type});
    if (!remaining_dims.empty()) {
      // For intermediate levels, push the empty inner first then recurse
      // into it via `back()`. The leaf level handles its own emplace below.
      const mir::ExprId emplace_id = block.exprs.Add(
          mir::Expr{
              .data =
                  mir::CallExpr{
                      .callee =
                          mir::Direct{
                              .target = support::BuiltinFn::kVectorEmplace},
                      .arguments = {chain_expr_id, empty_inner_id}},
              .type = builtins.void_type});
      block.AppendStmt(mir::ExprStmt{.expr = emplace_id});
      const mir::ExprId inner_chain_id = block.exprs.Add(
          mir::Expr{
              .data =
                  mir::CallExpr{
                      .callee =
                          mir::Direct{
                              .target = support::BuiltinFn::kVectorBack},
                      .arguments = {chain_expr_id}},
              .type = inner_type});
      indices.push_back(host_int_lit(static_cast<std::int64_t>(i)));
      EmitExternalUnitDimLevel(
          module, frame, self_id_for_register, runtime_label, leaf_pointer_type,
          inner_chain_id, inner_type, remaining_dims, indices);
      indices.pop_back();
    } else {
      // About to emit a leaf at this position. Recursion into the leaf
      // branch above (with dims empty) handles the emplace + register.
      indices.push_back(host_int_lit(static_cast<std::int64_t>(i)));
      EmitExternalUnitDimLevel(
          module, frame, self_id_for_register, runtime_label, leaf_pointer_type,
          chain_expr_id, chain_type, remaining_dims, indices);
      indices.pop_back();
    }
  }
}

// Lowers an external-unit instance member to the explicit MIR call sequence
// that builds each instance, stores it in its (possibly nested) vector slot,
// and registers it on the parent for by-name lookup. The construction lives
// in the parent's constructor block at depth zero; multi-dimensional members
// unroll into `dims[0] * dims[1] * ...` leaf sequences threading a
// `vector::back()` chain at each nesting level so the register call can reach
// the just-emplaced child.
void AppendExternalUnitConstruction(
    ModuleLowerer& module, const WalkFrame& frame, mir::MemberId target,
    const std::string& runtime_label, std::span<const std::uint32_t> dims) {
  mir::Block& block = *frame.current_block;
  const mir::Class& owner_class = *frame.current_class;
  const mir::MemberDecl& var = owner_class.members.Get(target);
  const mir::TypeId self_ptr_type = owner_class.self_pointer_type;

  // Peel the vector layers off the member type to land on the leaf
  // `unique_ptr<ExternalUnit>` type; the constructor builds that pointer and
  // the chain operations propagate down to it.
  mir::TypeId leaf_pointer_type = var.type;
  for (std::size_t i = 0; i < dims.size(); ++i) {
    const auto& chain_data = module.Unit().types.Get(leaf_pointer_type).data;
    if (!std::holds_alternative<mir::VectorType>(chain_data)) {
      throw InternalError(
          "AppendExternalUnitConstruction: dims exceed member vector depth");
    }
    leaf_pointer_type = std::get<mir::VectorType>(chain_data).element;
  }

  const mir::ExprId self_id =
      block.exprs.Add(MakeSelfRefExpr(frame, self_ptr_type));
  const mir::ExprId chain_id = block.exprs.Add(
      mir::MakeMemberAccessExpr(
          self_id, mir::MemberRef{.var = target}, var.type));
  std::vector<mir::ExprId> indices;
  EmitExternalUnitDimLevel(
      module, frame, self_id, runtime_label, leaf_pointer_type, chain_id,
      var.type, dims, indices);
}

// Declares one MIR member per instance member of the HIR scope and registers
// each in the lowerer's HIR-to-MIR map. Pure shape: no construction code is
// emitted; the construction call is appended in a later sweep that consults
// the same map by HIR instance-member id.
void DeclareInstanceMemberShapes(
    StructuralScopeLowerer& lowerer, mir::ClassShape& shape) {
  const hir::StructuralScope& hir_scope = lowerer.HirScope();
  std::uint32_t next_index = 0;
  for (const auto& im : hir_scope.instance_members) {
    for (const auto& v : shape.members) {
      if (v.name == im.instance_name) {
        throw InternalError(
            "instance member name collides with an existing member "
            "declaration in the enclosing class");
      }
    }
    const mir::TypeId var_type = MakeExternalUnitMemberType(
        lowerer.Module(), im.target_unit, im.array_dims.size());
    const mir::MemberId var_id = shape.members.Add(
        mir::MemberDecl{.name = im.instance_name, .type = var_type});
    lowerer.MapInstanceMember(hir::InstanceMemberId{next_index++}, var_id);
  }
}

// Emits the constructor-body construction call for each instance member,
// reading the MemberId through the HIR-to-MIR map populated when the shape
// was declared.
void EmitInstanceMemberConstruction(
    StructuralScopeLowerer& lowerer, WalkFrame frame) {
  const hir::StructuralScope& hir_scope = lowerer.HirScope();
  std::uint32_t next_index = 0;
  for (const auto& im : hir_scope.instance_members) {
    const mir::MemberId var_id =
        lowerer.TranslateInstanceMember(hir::InstanceMemberId{next_index++});
    AppendExternalUnitConstruction(
        lowerer.Module(), frame, var_id, im.instance_name, im.array_dims);
  }
}

// Allocates one MIR member per cross-unit reference. Every reference -- upward
// or downward, `$root`-anchored or named -- takes the same borrowed-pointer
// slot: the pointee matches the producer's actual storage cell so a read or
// drive reaches the right access protocol. A net producer owns a resolved
// cell (`ResolvedType`); any other value-storage signal is wrapped in
// `ObservableType` at its declaration so a write fires subscribers. The
// cross-unit pointer must point at that same cell -- otherwise the C++
// types mismatch. The route that fills each slot runs in the resolve phase,
// after the whole object tree exists.
void DeclareCrossUnitRefSlots(
    StructuralScopeLowerer& lowerer, mir::ClassShape& shape) {
  ModuleLowerer& module = lowerer.Module();
  const hir::StructuralScope& hir_scope = lowerer.HirScope();
  std::uint32_t slot_index = 0;
  for (const auto& cu : hir_scope.cross_unit_refs) {
    std::string member_name = "xref" + std::to_string(slot_index++);
    if (cu.target_net_type.has_value()) {
      const bool is_upward =
          std::holds_alternative<hir::UpwardRootHead>(cu.head) ||
          std::holds_alternative<hir::UpwardNamedHead>(cu.head);
      if (is_upward) {
        throw InternalError(
            "DeclareCrossUnitRefSlots: an upward cross-unit reference to a net "
            "is not yet supported");
      }
    }
    const mir::TypeId value = module.TranslateType(cu.type);
    const mir::TypeId leaf =
        cu.target_net_type.has_value()
            ? module.Unit().types.Intern(mir::ResolvedType{.value = value})
            : MaybeWrapObservable(module, value);
    const mir::TypeId slot_type =
        module.Unit().types.PointerTo(leaf, mir::PointerOwnership::kBorrowed);
    const mir::MemberId slot = shape.members.Add(
        mir::MemberDecl{.name = std::move(member_name), .type = slot_type});
    lowerer.AddCrossUnitRefTarget(mir::MemberRef{.var = slot}, slot_type);
  }
}

// Linear search for a member by its source name (or by the synthesized name
// if no source name was recorded). Per-class member arenas are small enough
// that O(N) at construction time is irrelevant. The within-class name is
// unique by the structural-scope lowering's allocation rule, so this is a
// deterministic lookup despite the linear scan.
auto FindMemberByName(const mir::ClassShape& shape, std::string_view name)
    -> std::optional<mir::MemberId> {
  for (std::size_t i = 0; i < shape.members.size(); ++i) {
    const mir::MemberId id{static_cast<std::uint32_t>(i)};
    const auto& m = shape.members.Get(id);
    const std::string_view key = m.source_name.empty() ? m.name : m.source_name;
    if (key == name) {
      return id;
    }
  }
  return std::nullopt;
}

// A route runs from its origin (the referrer's `self`) to the referenced
// leaf. Its receiver at each step is either **typed** -- known to point at
// a class this artifact owns, so descent takes a typed `MemberAccessExpr`
// on the receiver's class shape -- or **opaque**, a runtime `Scope*` reached
// through the SDK.
//
// Layout visibility is the semantic classification: a segment whose source
// and target classes are both owned by this artifact is layout-visible.
// Emission strategy is separate: an indexed hop on a layout-visible segment
// falls back to the SDK `GetChild(name, indices)` because MIR does not yet
// carry a typed vector-index primitive, but the receiver is immediately
// downcast back through `PointerCastExpr` to the target's typed class so
// subsequent layout-visible segments stay typed. Only a segment that
// crosses into another compilation unit's body switches the route to
// opaque for good.
struct RouteReceiver {
  mir::ExprId expr;
  // When set, the receiver points at this class's instance. When null the
  // receiver is a runtime `Scope*` and every following step goes through
  // the SDK.
  const mir::ClassShape* shape;
};

// Extracts the target class of a member whose type is a pointer /
// unique-pointer to an intra-unit object, whether directly or through one
// or more vector wrappers (`vector<unique_ptr<T>>` for an instance array,
// or the plain pointer for a scalar child). Returns nullopt for pointees
// that leave this artifact (`ExternalUnitObjectType`).
auto ClassBehindOwnedChildMember(
    const mir::CompilationUnit& unit, mir::TypeId member_type)
    -> std::optional<mir::ClassId> {
  mir::TypeId leaf = member_type;
  while (const auto* vec =
             std::get_if<mir::VectorType>(&unit.types.Get(leaf).data)) {
    leaf = vec->element;
  }
  const auto* ptr = std::get_if<mir::PointerType>(&unit.types.Get(leaf).data);
  if (ptr == nullptr) return std::nullopt;
  const auto& pointee = unit.types.Get(ptr->pointee).data;
  if (const auto* obj = std::get_if<mir::ObjectType>(&pointee)) {
    return obj->class_id;
  }
  return std::nullopt;
}

// Builds one `PackedArray[]` value carrying every per-axis index for a
// single hop; the runtime SDK's `GetChild` / `ResolveVisibleChild` accept
// it as a `std::span<PackedArray>`.
auto BuildIndicesLiteral(
    ModuleLowerer& module, mir::Block& block,
    std::span<const std::uint32_t> indices) -> mir::ExprId {
  const auto& builtins = module.Unit().builtins;
  std::vector<mir::ExprId> ids;
  ids.reserve(indices.size());
  for (const std::uint32_t idx : indices) {
    ids.push_back(block.exprs.Add(
        mir::MakeInt32Literal(builtins.int32, static_cast<std::int64_t>(idx))));
  }
  const mir::TypeId indices_type = module.Unit().types.Intern(
      mir::UnpackedArrayType{
          .element_type = builtins.int32, .size = indices.size()});
  return block.exprs.Add(
      mir::Expr{
          .data = mir::ArrayLiteralExpr{.elements = std::move(ids)},
          .type = indices_type});
}

auto BuildStringLiteral(
    ModuleLowerer& module, mir::Block& block, const std::string& s)
    -> mir::ExprId {
  return block.exprs.Add(
      mir::Expr{
          .data = mir::StringLiteral{.value = s},
          .type = module.Unit().builtins.string});
}

// Reaches a layout-visible child by name+indices as an SDK GetChild
// (emission fallback for indexed vector access) and re-tags the result
// back to the target's typed class pointer via `PointerCastExpr`. The
// segment stays classified as layout-visible; only the emission takes
// the SDK detour.
auto SdkChildAsTyped(
    ModuleLowerer& module, mir::Block& block, mir::ExprId receiver,
    const std::string& name, std::span<const std::uint32_t> indices,
    mir::ClassId target_class_id) -> RouteReceiver {
  auto& unit = module.Unit();
  const mir::TypeId scope_ptr_type = unit.builtins.scope_ptr;
  const mir::ExprId raw = block.exprs.Add(
      mir::Expr{
          .data =
              mir::CallExpr{
                  .callee =
                      mir::Direct{.target = support::BuiltinFn::kGetChild},
                  .arguments =
                      {receiver, BuildStringLiteral(module, block, name),
                       BuildIndicesLiteral(module, block, indices)}},
          .type = scope_ptr_type});
  const mir::ClassShape& target_shape = module.GetClassShape(target_class_id);
  const mir::TypeId typed_ptr = unit.types.PointerTo(
      unit.types.Intern(mir::ObjectType{.class_id = target_class_id}),
      mir::PointerOwnership::kBorrowed);
  const mir::ExprId typed = block.exprs.Add(
      mir::Expr{
          .data = mir::PointerCastExpr{.operand = raw}, .type = typed_ptr});
  return RouteReceiver{.expr = typed, .shape = &target_shape};
}

auto SdkChildOpaque(
    ModuleLowerer& module, mir::Block& block, mir::ExprId receiver,
    const std::string& name, std::span<const std::uint32_t> indices)
    -> RouteReceiver {
  const mir::TypeId scope_ptr_type = module.Unit().builtins.scope_ptr;
  const mir::ExprId step = block.exprs.Add(
      mir::Expr{
          .data =
              mir::CallExpr{
                  .callee =
                      mir::Direct{.target = support::BuiltinFn::kGetChild},
                  .arguments =
                      {receiver, BuildStringLiteral(module, block, name),
                       BuildIndicesLiteral(module, block, indices)}},
          .type = scope_ptr_type});
  return RouteReceiver{.expr = step, .shape = nullptr};
}

// Establishes the route's starting receiver from the head, applying any
// head indices when the head is an array element. The returned receiver is
// typed whenever the head's target class is owned by this artifact.
auto BuildRouteAnchor(
    StructuralScopeLowerer& lowerer, const WalkFrame& frame,
    const hir::CrossUnitRefHead& head) -> RouteReceiver {
  ModuleLowerer& module = lowerer.Module();
  auto& unit = module.Unit();
  mir::Block& block = *frame.current_block;
  const mir::TypeId scope_ptr_type = unit.builtins.scope_ptr;

  if (const auto* dh = std::get_if<hir::DownwardHead>(&head)) {
    const mir::EnclosingHops hops{.value = dh->hops.value};
    const mir::ExprId enclosing_self =
        BuildEnclosingScopeReceiver(frame, unit, hops);

    if (std::holds_alternative<hir::ProceduralScopeId>(dh->child)) {
      // A named procedural block's companion pointer lives on some
      // anonymous wrapper class, not on the enclosing structural class
      // shape, so no typed member on the enclosing class names it; the SV
      // reference names the block by label instead. The runtime `GetChild`
      // walk recurses past anonymous begin/ends so the label is reached
      // transparently, and the descent is opaque from this hop on.
      const auto& scope_decl = lowerer.HirScope().procedural_scopes.Get(
          std::get<hir::ProceduralScopeId>(dh->child));
      if (!scope_decl.label.has_value()) {
        throw InternalError(
            "BuildRouteAnchor: procedural-scope head has no SV label");
      }
      if (!dh->head_indices.empty()) {
        throw InternalError(
            "BuildRouteAnchor: a procedural-scope head cannot be indexed");
      }
      return SdkChildOpaque(
          module, block, enclosing_self, *scope_decl.label, {});
    }

    const mir::Class& enclosing_cls = frame.EnclosingClassAtHops(hops);
    const mir::MemberId head_member =
        lowerer.TranslateOwnedChild(dh->hops, dh->child);
    const auto& head_decl = enclosing_cls.members.Get(head_member);
    const mir::TypeId head_field_type = head_decl.type;

    if (!dh->head_indices.empty()) {
      // The head is an array element. Classification stays layout-visible
      // when the artifact owns the target class; emission uses the SDK
      // `GetChild(name, indices)` and downcasts back to typed so the rest
      // of the route can continue typed. Otherwise the route goes opaque.
      const std::string head_name = head_decl.source_name.empty()
                                        ? head_decl.name
                                        : head_decl.source_name;
      const std::optional<mir::ClassId> target =
          ClassBehindOwnedChildMember(unit, head_field_type);
      if (target.has_value()) {
        return SdkChildAsTyped(
            module, block, enclosing_self, head_name, dh->head_indices,
            *target);
      }
      return SdkChildOpaque(
          module, block, enclosing_self, head_name, dh->head_indices);
    }

    const mir::ExprId head_access = block.exprs.Add(
        mir::Expr{
            .data =
                mir::MemberAccessExpr{
                    .receiver = enclosing_self,
                    .member = mir::MemberRef{.var = head_member}},
            .type = head_field_type});
    const auto* head_ptr =
        std::get_if<mir::PointerType>(&unit.types.Get(head_field_type).data);
    if (head_ptr == nullptr) {
      throw InternalError(
          "BuildRouteAnchor: head member is not a pointer type");
    }
    const auto& pointee_data = unit.types.Get(head_ptr->pointee).data;
    if (const auto* obj = std::get_if<mir::ObjectType>(&pointee_data)) {
      return RouteReceiver{
          .expr = head_access, .shape = &module.GetClassShape(obj->class_id)};
    }
    if (std::holds_alternative<mir::ExternalUnitObjectType>(pointee_data)) {
      // Head is typed but the target lives in another compilation unit;
      // every step from here is opaque.
      return RouteReceiver{.expr = head_access, .shape = nullptr};
    }
    throw InternalError(
        "BuildRouteAnchor: downward head pointee is not an object type");
  }

  if (std::holds_alternative<hir::UpwardRootHead>(head)) {
    // `$root` is a runtime shell scope with no MIR-owned class shape; the
    // climb goes through the runtime SDK.
    const mir::ExprId self_ref = block.exprs.Add(
        MakeSelfRefExpr(frame, frame.current_class->self_pointer_type));
    const mir::ExprId root = block.exprs.Add(
        mir::Expr{
            .data =
                mir::CallExpr{
                    .callee =
                        mir::Direct{.target = support::BuiltinFn::kResolveRoot},
                    .arguments = {self_ref}},
            .type = scope_ptr_type});
    return RouteReceiver{.expr = root, .shape = nullptr};
  }

  const auto& un = std::get<hir::UpwardNamedHead>(head);
  // The visible-child climb walks the parent chain by name (LRM 23.8);
  // the ancestor's class is not statically known to the referrer, so the
  // climb runs through the runtime SDK.
  const mir::ExprId self_ref = block.exprs.Add(
      MakeSelfRefExpr(frame, frame.current_class->self_pointer_type));
  const mir::ExprId matched = block.exprs.Add(
      mir::Expr{
          .data =
              mir::CallExpr{
                  .callee =
                      mir::Direct{
                          .target = support::BuiltinFn::kResolveVisibleChild},
                  .arguments =
                      {self_ref,
                       BuildStringLiteral(module, block, un.head_name),
                       BuildIndicesLiteral(module, block, un.head_indices)}},
          .type = scope_ptr_type});
  return RouteReceiver{.expr = matched, .shape = nullptr};
}

// Descends one intermediate segment from `receiver`. A layout-visible
// segment without indices takes a typed `MemberAccessExpr`; a layout-visible
// segment with indices takes the SDK GetChild fallback and re-tags to the
// target's typed class. A segment that reaches into another compilation
// unit's body switches the route to opaque, and every subsequent segment
// stays opaque via `GetChild(name, indices)`.
auto AppendRouteSegment(
    StructuralScopeLowerer& lowerer, mir::Block& block,
    const RouteReceiver& receiver, const hir::PathSegment& segment)
    -> RouteReceiver {
  ModuleLowerer& module = lowerer.Module();
  auto& unit = module.Unit();

  if (receiver.shape == nullptr) {
    return SdkChildOpaque(
        module, block, receiver.expr, segment.name, segment.indices);
  }

  const auto step_member = FindMemberByName(*receiver.shape, segment.name);
  if (!step_member.has_value()) {
    throw InternalError(
        "AppendRouteSegment: descent step '" + segment.name +
        "' not found in typed class shape");
  }
  const mir::TypeId step_type = receiver.shape->members.Get(*step_member).type;

  if (segment.indices.empty()) {
    const mir::ExprId step_access = block.exprs.Add(
        mir::Expr{
            .data =
                mir::MemberAccessExpr{
                    .receiver = receiver.expr,
                    .member = mir::MemberRef{.var = *step_member}},
            .type = step_type});
    const auto* step_ptr =
        std::get_if<mir::PointerType>(&unit.types.Get(step_type).data);
    if (step_ptr == nullptr) {
      throw InternalError(
          "AppendRouteSegment: intermediate typed step is not a pointer");
    }
    const auto& pointee_data = unit.types.Get(step_ptr->pointee).data;
    if (const auto* obj = std::get_if<mir::ObjectType>(&pointee_data)) {
      return RouteReceiver{
          .expr = step_access, .shape = &module.GetClassShape(obj->class_id)};
    }
    if (std::holds_alternative<mir::ExternalUnitObjectType>(pointee_data)) {
      return RouteReceiver{.expr = step_access, .shape = nullptr};
    }
    throw InternalError(
        "AppendRouteSegment: intermediate typed step pointee is not an "
        "object type");
  }

  // Indexed segment: emission fallback via SDK GetChild, then downcast to
  // the target's typed class if the artifact owns it. Classification
  // remains layout-visible so the following segment can stay typed.
  const std::optional<mir::ClassId> target =
      ClassBehindOwnedChildMember(unit, step_type);
  if (target.has_value()) {
    return SdkChildAsTyped(
        module, block, receiver.expr, segment.name, segment.indices, *target);
  }
  return SdkChildOpaque(
      module, block, receiver.expr, segment.name, segment.indices);
}

// Materializes the leaf signal reach as the borrowed-pointer value the slot
// takes: `AddressOf` of the typed member access when the receiver is typed,
// or a `CastExpr` of `GetSignal`'s `void*` when the receiver is opaque.
auto MaterializeLeaf(
    StructuralScopeLowerer& lowerer, mir::Block& block,
    const RouteReceiver& receiver, const hir::PathSegment& leaf,
    mir::TypeId slot_type) -> mir::ExprId {
  ModuleLowerer& module = lowerer.Module();
  auto& unit = module.Unit();

  if (!leaf.indices.empty()) {
    throw InternalError(
        "MaterializeLeaf: leaf signal step carries indices; array selection "
        "on a cross-unit-ref leaf belongs to an expression-level access, not "
        "the descent");
  }

  if (receiver.shape != nullptr) {
    const auto member = FindMemberByName(*receiver.shape, leaf.name);
    if (!member.has_value()) {
      throw InternalError(
          "MaterializeLeaf: leaf signal '" + leaf.name +
          "' not found in typed class shape");
    }
    const mir::TypeId member_type = receiver.shape->members.Get(*member).type;
    const mir::ExprId access = block.exprs.Add(
        mir::Expr{
            .data =
                mir::MemberAccessExpr{
                    .receiver = receiver.expr,
                    .member = mir::MemberRef{.var = *member}},
            .type = member_type});
    return block.exprs.Add(
        mir::Expr{
            .data = mir::AddressOfExpr{.operand = access}, .type = slot_type});
  }

  const mir::TypeId void_ptr_type = unit.types.PointerTo(
      unit.builtins.void_type, mir::PointerOwnership::kBorrowed);
  const mir::ExprId raw = block.exprs.Add(
      mir::Expr{
          .data =
              mir::CallExpr{
                  .callee =
                      mir::Direct{.target = support::BuiltinFn::kGetSignal},
                  .arguments =
                      {receiver.expr,
                       BuildStringLiteral(module, block, leaf.name)}},
          .type = void_ptr_type});
  return block.exprs.Add(
      mir::Expr{.data = mir::CastExpr{.operand = raw}, .type = slot_type});
}

// Composes the resolve-phase pointer value that fills a cross-unit
// reference slot: anchor from the head, walk the descent segments, and
// materialize the leaf. The result flows into the ordinary assignment the
// caller emits into the resolve block.
auto BuildRouteValue(
    StructuralScopeLowerer& lowerer, const WalkFrame& frame,
    const hir::CrossUnitRefHead& head,
    const std::vector<hir::PathSegment>& path, mir::TypeId slot_type)
    -> mir::ExprId {
  if (path.empty()) {
    throw InternalError("BuildRouteValue: route has no leaf signal");
  }
  mir::Block& block = *frame.current_block;
  RouteReceiver receiver = BuildRouteAnchor(lowerer, frame, head);
  for (std::size_t i = 0; i + 1 < path.size(); ++i) {
    receiver = AppendRouteSegment(lowerer, block, receiver, path[i]);
  }
  return MaterializeLeaf(lowerer, block, receiver, path.back(), slot_type);
}

// Each cross-unit reference resolves in the resolve phase: the top-down
// walk over the fully-constructed object tree runs each route, filling the
// scope's `xref_N` slot with a borrowed pointer to the target's observable
// cell. Every route lands as an ordinary `AssignExpr` on the slot member.
void InstallCrossUnitRefs(
    StructuralScopeLowerer& lowerer, const WalkFrame& resolve_frame) {
  mir::Class& mir_class = *resolve_frame.current_class;
  mir::Block& resolve_block = *resolve_frame.current_block;
  const hir::StructuralScope& hir_scope = lowerer.HirScope();
  for (std::size_t ci = 0; ci < hir_scope.cross_unit_refs.size(); ++ci) {
    const hir::CrossUnitRefId hir_id{static_cast<std::uint32_t>(ci)};
    const auto& cu = hir_scope.cross_unit_refs.Get(hir_id);
    const mir::MemberId slot = lowerer.CrossUnitRefTarget(hir_id).target.var;
    const mir::TypeId slot_type = mir_class.members.Get(slot).type;
    const mir::ExprId nav =
        BuildRouteValue(lowerer, resolve_frame, cu.head, cu.path, slot_type);
    const mir::ExprId self_for_target = resolve_block.exprs.Add(
        MakeSelfRefExpr(resolve_frame, mir_class.self_pointer_type));
    const mir::ExprId target = resolve_block.exprs.Add(
        mir::Expr{
            .data =
                mir::MemberAccessExpr{
                    .receiver = self_for_target,
                    .member = mir::MemberRef{.var = slot}},
            .type = slot_type});
    const mir::ExprId assign = resolve_block.exprs.Add(
        mir::Expr{
            .data = mir::AssignExpr{.target = target, .value = nav},
            .type = slot_type});
    resolve_block.AppendStmt(mir::ExprStmt{.expr = assign});
  }
}

// Appends one process activation registration to the scope's `activate` body:
// invokes `body` over the activate frame's `self` to produce the coroutine,
// then registers it for the scope's startup (`is_final == false`) or shutdown
// (`is_final == true`) lifecycle (LRM 9.2). Startup and shutdown are distinct
// registration callees, not one tagged call.
void AppendProcessRegistration(
    ModuleLowerer& module, const WalkFrame& activate_frame, mir::MethodId body,
    bool is_final) {
  mir::Block& block = *activate_frame.current_block;
  const mir::TypeId self_ptr_type =
      activate_frame.current_class->self_pointer_type;
  const mir::ExprId body_self =
      block.exprs.Add(MakeSelfRefExpr(activate_frame, self_ptr_type));
  const mir::ExprId body_call = block.exprs.Add(
      mir::Expr{
          .data =
              mir::CallExpr{
                  .callee = mir::Direct{.target = body},
                  .arguments = {body_self}},
          .type = module.Unit().builtins.coroutine_void});
  const mir::ExprId reg_self =
      block.exprs.Add(MakeSelfRefExpr(activate_frame, self_ptr_type));
  const mir::ExprId reg_call = block.exprs.Add(
      mir::Expr{
          .data =
              mir::CallExpr{
                  .callee =
                      mir::Direct{
                          .target = is_final
                                        ? support::BuiltinFn::kRegisterFinal
                                        : support::BuiltinFn::kRegisterInitial},
                  .arguments = {reg_self, body_call}},
          .type = module.Unit().builtins.void_type});
  block.AppendStmt(mir::ExprStmt{.expr = reg_call});
}

auto AttachNetDriver(
    const StructuralScopeLowerer& lowerer, mir::Class& mir_class,
    mir::ExprId net_access, mir::TypeId value_type,
    const std::string& driver_name, hir::ExprId source,
    const WalkFrame& resolve_frame, const WalkFrame& init_frame,
    mir::TypeId self_ptr_type) -> diag::Result<NetDriver>;

auto ContinuousAssignNetTarget(
    const StructuralScopeLowerer& lowerer, const mir::Class& mir_class,
    const hir::ContinuousAssign& ca) -> std::optional<mir::MemberId>;

// Realizes each port connection (LRM 23.3.3). An input or output port is the
// implied continuous assignment between the two cells, materialized as the same
// synthesized process a scope-level `assign` produces, registered as a process;
// when the driven side is a net the edge attaches a driver rather than writing
// the cell. A `ref` port instead binds the child's reference member --
// navigated by name from the owned child -- to the connected variable's cell,
// emitted into the resolve block: one assignment of a reference, with no second
// cell and no continuous assignment.
auto InstallPortConnections(
    StructuralScopeLowerer& lowerer, WalkFrame frame, WalkFrame resolve_frame,
    WalkFrame init_frame, WalkFrame activate_frame) -> diag::Result<void> {
  mir::Class& mir_class = *frame.current_class;
  mir::Block& resolve_block = *resolve_frame.current_block;
  const hir::StructuralScope& hir_scope = lowerer.HirScope();
  ModuleLowerer& module = lowerer.Module();
  const mir::TypeId self_ptr_type = mir_class.self_pointer_type;
  std::size_t net_port_index = 0;
  for (const auto& pc : hir_scope.port_connections) {
    if (pc.direction == hir::PortDirection::kRef) {
      // Bind the child's reference member to the connected variable's cell in
      // the resolve phase: navigate the owned child by name to its reference
      // member and store a reference to the peer's cell into it.
      const auto& alias = std::get<hir::PortAliasEndpoint>(pc.endpoint);
      if (!std::holds_alternative<hir::DownwardHead>(alias.head)) {
        throw InternalError(
            "InstallPortConnections: a ref port reaches its child downward");
      }
      const mir::TypeId value_type = module.TranslateType(alias.type);
      const mir::TypeId ref_type = module.Unit().types.Intern(
          mir::RefType{
              .pointee = value_type, .mutability = mir::Mutability::kMutable});
      const mir::TypeId slot_type = module.Unit().types.PointerTo(
          ref_type, mir::PointerOwnership::kBorrowed);
      const mir::ExprId nav = BuildRouteValue(
          lowerer, resolve_frame, alias.head, alias.path, slot_type);
      const mir::ExprId target = resolve_block.exprs.Add(
          mir::Expr{.data = mir::DerefExpr{.pointer = nav}, .type = ref_type});

      auto peer_or =
          lowerer.LowerLhsExpr(hir_scope.exprs.Get(pc.peer), resolve_frame);
      if (!peer_or) return std::unexpected(std::move(peer_or.error()));
      const mir::ExprId peer_cell =
          resolve_block.exprs.Add(*std::move(peer_or));
      const mir::ExprId ref_value = BuildReferenceArg(
          module.Unit(), resolve_block, peer_cell,
          resolve_block.exprs.Get(peer_cell).type);

      const mir::ExprId assign = resolve_block.exprs.Add(
          mir::Expr{
              .data = mir::AssignExpr{.target = target, .value = ref_value},
              .type = ref_type});
      resolve_block.AppendStmt(mir::ExprStmt{.expr = assign});
      continue;
    }
    const auto& cell = std::get<hir::PortCellEndpoint>(pc.endpoint);
    const bool is_input = pc.direction == hir::PortDirection::kInput;
    // A port connection is a reactive edge: the source is read, the sink is
    // driven. An input port's source is the parent expression and its sink is
    // the child cell; an output port's source is the child cell and its sink is
    // the parent target. The edge body is the same continuous assignment either
    // way; only the sink's capability picks the write protocol -- a net cell
    // takes a driver, a variable cell a direct assignment (LRM 23.3.3).
    const hir::ContinuousAssign assign{
        .span = pc.span,
        .lhs = is_input ? cell.cell : pc.peer,
        .rhs = is_input ? pc.peer : cell.cell,
        .sensitivity_list = pc.sensitivity};

    // The sink is a net in two shapes: an input port whose child cell is a net
    // (reached cross-unit), or an output port whose parent target is a net
    // (local). Either resolves to a driver the edge's process updates; a
    // non-net sink keeps the direct continuous-assign write.
    std::optional<NetDriver> net_driver;
    auto attach_sink_driver = [&](mir::ExprId net_access,
                                  hir::ExprId source) -> diag::Result<void> {
      const mir::TypeId value_type =
          std::get<mir::ResolvedType>(
              module.Unit()
                  .types.Get(resolve_block.exprs.Get(net_access).type)
                  .data)
              .value;
      const std::string driver_name =
          std::format("net_port_{}__driver", net_port_index++);
      auto driver_or = AttachNetDriver(
          lowerer, mir_class, net_access, value_type, driver_name, source,
          resolve_frame, init_frame, self_ptr_type);
      if (!driver_or) return std::unexpected(std::move(driver_or.error()));
      net_driver = *driver_or;
      return {};
    };
    if (is_input) {
      const auto* prim =
          std::get_if<hir::PrimaryExpr>(&hir_scope.exprs.Get(cell.cell).data);
      const auto* cuv = prim != nullptr
                            ? std::get_if<hir::CrossUnitVarRef>(&prim->data)
                            : nullptr;
      if (cuv != nullptr &&
          hir_scope.cross_unit_refs.Get(cuv->id).target_net_type.has_value()) {
        auto net_or =
            lowerer.LowerLhsExpr(hir_scope.exprs.Get(cell.cell), resolve_frame);
        if (!net_or) return std::unexpected(std::move(net_or.error()));
        const mir::ExprId net_access =
            resolve_block.exprs.Add(*std::move(net_or));
        auto r = attach_sink_driver(net_access, pc.peer);
        if (!r) return std::unexpected(std::move(r.error()));
      }
    } else if (
        const auto net_member =
            ContinuousAssignNetTarget(lowerer, mir_class, assign)) {
      const mir::TypeId net_type = mir_class.members.Get(*net_member).type;
      const mir::ExprId net_self = resolve_block.exprs.Add(
          MakeSelfRefExpr(resolve_frame, self_ptr_type));
      const mir::ExprId net_access = resolve_block.exprs.Add(
          mir::MakeMemberAccessExpr(
              net_self, mir::MemberRef{.var = *net_member}, net_type));
      auto r = attach_sink_driver(net_access, assign.rhs);
      if (!r) return std::unexpected(std::move(r.error()));
    }

    std::string name = std::format("process_{}", mir_class.methods.size());
    auto decl_or = LowerContinuousAssign(
        lowerer, frame, std::move(name), assign, net_driver);
    if (!decl_or) return std::unexpected(std::move(decl_or.error()));
    const mir::MethodId body = mir_class.methods.Add(*std::move(decl_or));
    AppendProcessRegistration(module, activate_frame, body, false);
  }
  return {};
}

// Walks a member's type through any vector wrappers and a unique-pointer
// layer to return the intra-unit `ClassId` it owns, or nullopt for any other
// shape (external-unit object, ref, non-class type).
auto AsOwnedGenerateChildClassId(
    const mir::CompilationUnit& unit, mir::TypeId type)
    -> std::optional<mir::ClassId> {
  mir::TypeId leaf = type;
  while (const auto* vec =
             std::get_if<mir::VectorType>(&unit.types.Get(leaf).data)) {
    leaf = vec->element;
  }
  const auto* ptr = std::get_if<mir::PointerType>(&unit.types.Get(leaf).data);
  if (ptr == nullptr || ptr->ownership != mir::PointerOwnership::kUnique) {
    return std::nullopt;
  }
  const auto& data = unit.types.Get(ptr->pointee).data;
  if (const auto* obj = std::get_if<mir::ObjectType>(&data)) {
    return obj->class_id;
  }
  return std::nullopt;
}

// The net member a continuous assignment drives -- the resolved-net cell its
// left-hand side names -- or nullopt when the target is a variable (a direct
// write, not a driver).
auto ContinuousAssignNetTarget(
    const StructuralScopeLowerer& lowerer, const mir::Class& mir_class,
    const hir::ContinuousAssign& ca) -> std::optional<mir::MemberId> {
  const hir::StructuralScope& hir_scope = lowerer.HirScope();
  const auto* prim =
      std::get_if<hir::PrimaryExpr>(&hir_scope.exprs.Get(ca.lhs).data);
  if (prim == nullptr) return std::nullopt;
  const auto* ref = std::get_if<hir::StructuralDataObjectRef>(&prim->data);
  if (ref == nullptr) return std::nullopt;
  const mir::MemberId target =
      lowerer.TranslateStructuralDataObject(ref->hops, ref->var);
  const mir::TypeId target_type = mir_class.members.Get(target).type;
  if (!std::holds_alternative<mir::ResolvedType>(
          lowerer.Module().Unit().types.Get(target_type).data)) {
    return std::nullopt;
  }
  return target;
}

// Installs a driver on a net cell reached through a route: a driver-handle
// member bound at Resolve (`self->driver = net_access.AttachDriver()`) and
// seeded at Initialize (`self->driver.Update(services, source)`). `net_access`
// is the resolve-phase lvalue of the net cell -- a local member access for a
// continuous assignment, a cross-unit navigation for a port connection -- and
// `source` is the driving expression, lowered in the initialize phase. Returns
// the driver handle the activation process updates whenever the source changes.
auto AttachNetDriver(
    const StructuralScopeLowerer& lowerer, mir::Class& mir_class,
    mir::ExprId net_access, mir::TypeId value_type,
    const std::string& driver_name, hir::ExprId source,
    const WalkFrame& resolve_frame, const WalkFrame& init_frame,
    mir::TypeId self_ptr_type) -> diag::Result<NetDriver> {
  ModuleLowerer& module = lowerer.Module();
  const hir::StructuralScope& hir_scope = lowerer.HirScope();
  mir::Block& resolve_block = *resolve_frame.current_block;
  mir::Block& init_block = *init_frame.current_block;

  const mir::TypeId driver_type =
      module.Unit().types.Intern(mir::DriverType{.value = value_type});
  const mir::MemberId driver_member = mir_class.members.Add(
      mir::MemberDecl{.name = driver_name, .type = driver_type});

  const mir::ExprId attach = resolve_block.exprs.Add(
      mir::MakeNetAttachDriverCallExpr(net_access, driver_type));
  const mir::ExprId resolve_self =
      resolve_block.exprs.Add(MakeSelfRefExpr(resolve_frame, self_ptr_type));
  const mir::ExprId driver_lhs = resolve_block.exprs.Add(
      mir::MakeMemberAccessExpr(
          resolve_self, mir::MemberRef{.var = driver_member}, driver_type));
  const mir::ExprId attach_assign = resolve_block.exprs.Add(
      mir::Expr{
          .data = mir::AssignExpr{.target = driver_lhs, .value = attach},
          .type = driver_type});
  resolve_block.AppendStmt(
      mir::Stmt{
          .label = std::nullopt, .data = mir::ExprStmt{.expr = attach_assign}});

  auto source_or = lowerer.LowerExpr(hir_scope.exprs.Get(source), init_frame);
  if (!source_or) return std::unexpected(std::move(source_or.error()));
  const mir::ExprId seed_value = init_block.exprs.Add(*std::move(source_or));
  const auto init_self = [&] {
    return init_block.exprs.Add(MakeSelfRefExpr(init_frame, self_ptr_type));
  };
  const mir::ExprId seed_services = init_block.exprs.Add(
      mir::MakeServicesCallExpr(init_self(), module.Unit().builtins.services));
  const mir::ExprId seed_driver = init_block.exprs.Add(
      mir::MakeMemberAccessExpr(
          init_self(), mir::MemberRef{.var = driver_member}, driver_type));
  const mir::ExprId seed_update = init_block.exprs.Add(
      mir::MakeNetDriverUpdateCallExpr(
          seed_driver, seed_services, seed_value,
          module.Unit().builtins.void_type));
  init_block.AppendStmt(
      mir::Stmt{
          .label = std::nullopt, .data = mir::ExprStmt{.expr = seed_update}});

  return NetDriver{.driver_member = driver_member, .driver_type = driver_type};
}

void ValidateOwnedChildConstruction(
    ModuleLowerer& module, const mir::Class& owner_class,
    const mir::Block& block, mir::MemberId target_var,
    mir::ClassId child_scope_id, std::span<const mir::ExprId> ctor_args) {
  if (std::ranges::find(owner_class.contained, child_scope_id) ==
      owner_class.contained.end()) {
    throw InternalError(
        "owned-child construction: child scope is not a direct child of the "
        "enclosing class");
  }
  if (target_var.value >= owner_class.members.size()) {
    throw InternalError(
        "owned-child construction: target is out of range in the enclosing "
        "class");
  }
  const auto& var = owner_class.members.Get(target_var);
  const auto target_class_id =
      AsOwnedGenerateChildClassId(module.Unit(), var.type);
  const auto& child_scope_shape = module.GetClassShape(child_scope_id);
  if (!target_class_id.has_value() ||
      module.GetClassShape(*target_class_id).name != child_scope_shape.name) {
    throw InternalError(
        "owned-child construction: target var does not own the requested "
        "class");
  }
  if (ctor_args.size() != child_scope_shape.params.size()) {
    throw InternalError(
        "owned-child construction: args count does not match child class "
        "param count");
  }
  for (std::size_t i = 0; i < ctor_args.size(); ++i) {
    const auto& arg = block.exprs.Get(ctor_args[i]);
    const auto& param = child_scope_shape.params.Get(
        mir::ParamId{static_cast<std::uint32_t>(i)});
    if (arg.type != param.type) {
      throw InternalError(
          "owned-child construction: arg type does not match structural "
          "param type");
    }
  }
}

// Lowers an owned-child construction site to the explicit MIR call shape:
// `make_unique<Child>(self, HierarchySegment{label, indices}, services,
// ctor_args...)` produces the child instance carrying its complete
// hierarchy identity, then `AttachChild(self, *child)` wires the parent
// edge for traversal and by-name lookup. A scalar member assigns the
// unique pointer to its slot; a vector member emplaces into the storage
// vector and the attach call reaches the just-pushed element through
// `vector_back`. `arm_frame` must point at the block where the stmts land
// and carry the constructor's bindings so a `self` read resolves to the
// receiver binding.
void AppendOwnedChildConstruction(
    ModuleLowerer& module, const WalkFrame& arm_frame, mir::MemberId target_var,
    mir::ClassId child_scope_id, std::vector<mir::ExprId> ctor_args,
    std::optional<mir::ExprId> array_index) {
  mir::Block& arm_block = *arm_frame.current_block;
  const mir::Class& owner_class = *arm_frame.current_class;
  ValidateOwnedChildConstruction(
      module, owner_class, arm_block, target_var, child_scope_id, ctor_args);

  const mir::MemberDecl& var = owner_class.members.Get(target_var);
  // The runtime label is the SV-visible identifier. An anonymous scope
  // (no `source_name`) gets an empty label; the scope's runtime base
  // treats an empty label as non-addressable, so a peer by-name lookup
  // walks past it and reaches the addressable descendants underneath.
  const std::string& runtime_label = var.source_name;
  const auto& builtins = module.Unit().builtins;
  const mir::TypeId self_ptr_type = owner_class.self_pointer_type;

  // The child pointer type the constructor produces: `unique_ptr<Child>` for
  // both scalar and vector storage (the vector wraps an N-length sequence of
  // these slots, the scalar holds one directly).
  const mir::TypeId child_ptr_type =
      std::holds_alternative<mir::VectorType>(
          module.Unit().types.Get(var.type).data)
          ? std::get<mir::VectorType>(module.Unit().types.Get(var.type).data)
                .element
          : var.type;
  const auto& child_ptr_data = module.Unit().types.Get(child_ptr_type).data;
  if (!std::holds_alternative<mir::PointerType>(child_ptr_data) ||
      std::get<mir::PointerType>(child_ptr_data).ownership !=
          mir::PointerOwnership::kUnique) {
    throw InternalError(
        "owned-child construction: target slot type is not a unique pointer");
  }
  // Capture the pointee by id now. A reference into the type arena is a
  // transient view: interning any later type into the arena invalidates it, and
  // that happens below. Reads after that point go through the id.
  const mir::TypeId child_pointee =
      std::get<mir::PointerType>(child_ptr_data).pointee;

  const auto string_literal = [&](const std::string& s) -> mir::ExprId {
    return arm_block.exprs.Add(
        mir::Expr{
            .data = mir::StringLiteral{.value = s}, .type = builtins.string});
  };
  const auto self_read = [&]() -> mir::ExprId {
    return arm_block.exprs.Add(MakeSelfRefExpr(arm_frame, self_ptr_type));
  };

  // Build the child's structural identity once and pass it as the child's
  // own ctor argument. The child holds onto it from the moment its
  // constructor returns; %m, by-name lookup, and debug traces all read
  // from that single source. The index list carries the caller-provided
  // hierarchy index when there is one -- a generated scope's constant index --
  // and is empty otherwise; a multi-dim member would extend it per axis.
  std::vector<mir::ExprId> index_elems;
  if (array_index.has_value()) {
    index_elems.push_back(*array_index);
  }
  const mir::TypeId indices_type = module.Unit().types.Intern(
      mir::UnpackedArrayType{
          .element_type = builtins.int32, .size = index_elems.size()});
  const mir::ExprId indices_id = arm_block.exprs.Add(
      mir::Expr{
          .data = mir::ArrayLiteralExpr{.elements = std::move(index_elems)},
          .type = indices_type});
  const mir::ExprId segment_id = arm_block.exprs.Add(
      mir::Expr{
          .data =
              mir::CallExpr{
                  .callee = mir::Construct{},
                  .arguments = {string_literal(runtime_label), indices_id}},
          .type = builtins.hierarchy_segment});

  std::vector<mir::ExprId> ctor_call_args;
  ctor_call_args.reserve(3 + ctor_args.size());
  ctor_call_args.push_back(self_read());
  ctor_call_args.push_back(segment_id);
  ctor_call_args.push_back(
      arm_block.exprs.Add(BuildServicesCallExpr(module, arm_frame)));
  for (const mir::ExprId arg : ctor_args) {
    ctor_call_args.push_back(arg);
  }
  const mir::ExprId ctor_call_id = arm_block.exprs.Add(
      mir::Expr{
          .data =
              mir::CallExpr{
                  .callee = mir::Construct{},
                  .arguments = std::move(ctor_call_args)},
          .type = child_ptr_type});

  // Target member access reads self each time so reads and writes share no
  // implicit pointer; the slot stores either the unique pointer directly
  // (scalar) or the storage vector (vector member).
  const mir::ExprId member_access_id = arm_block.exprs.Add(
      mir::MakeMemberAccessExpr(
          self_read(), mir::MemberRef{.var = target_var}, var.type));

  // The child reference we hand to `AttachChild` is `*self->member` for a
  // scalar slot and `*self->member.back()` for a vector slot (the vector
  // already grew by `push_back` of the just-built pointer). Attach runs
  // after the typed owner commits, so a thrown subobject ctor leaves no
  // half-attached scope in the parent's child relation.
  mir::ExprId child_ref_id{};
  if (std::holds_alternative<mir::VectorType>(
          module.Unit().types.Get(var.type).data)) {
    const mir::ExprId emplace_call_id = arm_block.exprs.Add(
        mir::Expr{
            .data =
                mir::CallExpr{
                    .callee =
                        mir::Direct{
                            .target = support::BuiltinFn::kVectorEmplace},
                    .arguments = {member_access_id, ctor_call_id}},
            .type = builtins.void_type});
    arm_block.AppendStmt(mir::ExprStmt{.expr = emplace_call_id});

    // Re-read the member; the back-element call yields a reference to the
    // most-recently-pushed unique pointer.
    const mir::ExprId vector_access_id = arm_block.exprs.Add(
        mir::MakeMemberAccessExpr(
            self_read(), mir::MemberRef{.var = target_var}, var.type));
    const mir::ExprId back_call_id = arm_block.exprs.Add(
        mir::Expr{
            .data =
                mir::CallExpr{
                    .callee =
                        mir::Direct{.target = support::BuiltinFn::kVectorBack},
                    .arguments = {vector_access_id}},
            .type = child_ptr_type});
    child_ref_id = arm_block.exprs.Add(
        mir::Expr{
            .data = mir::DerefExpr{.pointer = back_call_id},
            .type = child_pointee});
  } else {
    const mir::ExprId assign_id = arm_block.exprs.Add(
        mir::Expr{
            .data =
                mir::AssignExpr{
                    .target = member_access_id, .value = ctor_call_id},
            .type = child_ptr_type});
    arm_block.AppendStmt(mir::ExprStmt{.expr = assign_id});

    const mir::ExprId scalar_access_id = arm_block.exprs.Add(
        mir::MakeMemberAccessExpr(
            self_read(), mir::MemberRef{.var = target_var}, var.type));
    child_ref_id = arm_block.exprs.Add(
        mir::Expr{
            .data = mir::DerefExpr{.pointer = scalar_access_id},
            .type = child_pointee});
  }

  const mir::ExprId attach_call_id = arm_block.exprs.Add(
      mir::Expr{
          .data =
              mir::CallExpr{
                  .callee =
                      mir::Direct{.target = support::BuiltinFn::kAttachChild},
                  .arguments = {self_read(), child_ref_id}},
          .type = builtins.void_type});
  arm_block.AppendStmt(mir::ExprStmt{.expr = attach_call_id});
}

// The correctness baseline for every generate construct: construct each
// instantiated block's own concrete scalar child directly (no runtime branch or
// loop), each carrying any constant hierarchy index it has. The genvar is
// folded into each body, so no induction-variable argument is threaded.
auto LowerResolvedGenerate(
    StructuralScopeLowerer& lowerer, WalkFrame frame,
    const GenerateBindings& gen_bindings, const hir::ResolvedGenerate& resolved)
    -> diag::Result<mir::Stmt> {
  mir::Block& block = *frame.current_block;
  const mir::TypeId int32_type = lowerer.Module().Unit().builtins.int32;

  mir::Block body;
  const WalkFrame body_frame = frame.WithBlock(&body);
  for (const auto& item : resolved.items) {
    const auto& binding = gen_bindings.by_scope_id.at(item.scope.value);
    std::optional<mir::ExprId> index_id;
    if (item.index.has_value()) {
      index_id = body.exprs.Add(mir::MakeInt32Literal(int32_type, *item.index));
    }
    AppendOwnedChildConstruction(
        lowerer.Module(), body_frame, binding.var_id, binding.scope_id, {},
        index_id);
  }
  const mir::BlockId body_id = block.child_scopes.Add(std::move(body));
  return mir::Stmt{
      .label = std::nullopt, .data = mir::BlockStmt{.scope = body_id}};
}

auto LowerGenerateAsStmt(
    StructuralScopeLowerer& lowerer, WalkFrame frame, const hir::Generate& gen,
    const GenerateBindings& gen_bindings) -> diag::Result<mir::Stmt> {
  return LowerResolvedGenerate(lowerer, frame, gen_bindings, gen.data);
}

}  // namespace

auto StructuralScopeLowerer::DeclareShape() -> diag::Result<mir::ClassId> {
  ModuleLowerer& module = *module_;
  const hir::StructuralScope& hir_scope = *hir_scope_;
  StructuralScopeLowerer& lowerer = *this;

  // The identity is minted before the shape is populated so the class's own
  // `self_pointer_type` can name it.
  class_id_ = module.Unit().DeclareClass();
  const mir::TypeId self_object_type =
      module.Unit().types.Intern(mir::ObjectType{.class_id = class_id_});
  const mir::TypeId self_pointer_type = module.Unit().types.PointerTo(
      self_object_type, mir::PointerOwnership::kBorrowed);

  mir::ClassShape shape;
  shape.name = name_;
  shape.base = mir::ClassRef{mir::RuntimeLibraryClassRef{
      .base_type = parent_ == nullptr
                       ? module.Unit().types.Intern(mir::InstanceType{})
                       : module.Unit().types.Intern(mir::GenScopeType{})}};
  shape.self_pointer_type = self_pointer_type;
  shape.time_resolution = hir_scope.time_resolution;

  // An object forwards its base's construction contract straight to the base
  // constructor; the prefix params come from that contract, not restated here,
  // and the render walks them like any other params.
  if (shape.base.has_value()) {
    const mir::BaseContract contract =
        mir::ResolveBaseContract(module.Unit(), *shape.base);
    for (const auto& param : contract.ctor_prefix) {
      shape.ctor_prefix_params.Add(param);
    }
  }
  for (const auto& alias : hir_scope.type_aliases) {
    shape.type_aliases.push_back(
        mir::TypeAliasDecl{
            .name = alias.name, .target = module.TranslateType(alias.target)});
  }

  for (std::size_t i = 0; i < hir_scope.structural_data_objects.size(); ++i) {
    const hir::StructuralDataObjectId hir_id{static_cast<std::uint32_t>(i)};
    const auto& d = hir_scope.structural_data_objects.Get(hir_id);
    const mir::TypeId mir_value_type = module.TranslateType(d.type);
    // A net (LRM 6.5) and a variable are peer kinds: a `ref` / `const ref`
    // port (LRM 23.3.3.2) member aliases the connected variable (a reference);
    // a net owns a resolved cell whose value is the resolution of its drivers;
    // any other variable is an observable cell so writes route through
    // `Var<T>::Set` and subscribers fire.
    const auto* var = std::get_if<hir::StructuralVariableDecl>(&d.kind);
    const bool is_reference = var != nullptr && var->reference.has_value();
    const mir::TypeId mir_field_type = [&] {
      if (is_reference) {
        return module.Unit().types.Intern(
            mir::RefType{
                .pointee = mir_value_type,
                .mutability =
                    *var->reference == hir::ReferenceBinding::kConstRef
                        ? mir::Mutability::kReadOnly
                        : mir::Mutability::kMutable});
      }
      if (var == nullptr) {
        return module.Unit().types.Intern(
            mir::ResolvedType{.value = mir_value_type});
      }
      return MaybeWrapObservable(module, mir_value_type);
    }();
    const mir::MemberId mir_id = shape.members.Add(
        mir::MemberDecl{.name = d.name, .type = mir_field_type});
    lowerer.MapStructuralDataObject(hir_id, mir_id);
  }

  DeclareCrossUnitRefSlots(lowerer, shape);

  // Reserve each subroutine's MIR id before any body lowers, so a call in
  // one body resolves a forward or mutual reference to a peer (LRM 13.7).
  // The id reserved here is the index the decl will occupy when it is later
  // added to the class's methods arena.
  for (std::size_t i = 0; i < hir_scope.structural_subroutines.size(); ++i) {
    lowerer.MapStructuralSubroutine(
        hir::StructuralSubroutineId{static_cast<std::uint32_t>(i)},
        mir::MethodId{static_cast<std::uint32_t>(i)});
  }

  // Recursively declare every owned generate child's class shape; each child
  // lowerer is retained for the body sweep.
  for (std::size_t gen_idx = 0; gen_idx < hir_scope.generates.size();
       ++gen_idx) {
    const auto& gen = hir_scope.generates.Get(
        hir::GenerateId{static_cast<std::uint32_t>(gen_idx)});
    GenerateBindings gen_bindings;
    gen_bindings.by_scope_id.resize(gen.child_scopes.size());

    auto specs = EnumerateGenerateChildSpecs(gen, module);
    for (auto& spec : specs) {
      const auto companion_name = CompanionVarNameFor(spec.scope_name);
      CheckNoNameCollision(shape, spec.scope_name, companion_name);

      auto child = std::make_unique<StructuralScopeLowerer>(
          module, &lowerer, std::move(spec.scope_name), *spec.scope);
      auto child_r = child->DeclareShape();
      if (!child_r) return std::unexpected(std::move(child_r.error()));

      const mir::ClassId child_id = *child_r;
      shape.contained.push_back(child_id);
      const mir::TypeId var_type = MakeUniqueObjectPointer(module, child_id);
      const mir::MemberId var_id = shape.members.Add(
          mir::MemberDecl{
              .name = companion_name,
              .source_name = spec.scope->source_name,
              .type = var_type});

      gen_bindings.by_scope_id.at(spec.scope_id.value) =
          ChildStructuralScopeBinding{.scope_id = child_id, .var_id = var_id};
      children_.push_back(std::move(child));
    }

    lowerer.MapGenerate(
        hir::GenerateId{static_cast<std::uint32_t>(gen_idx)},
        std::move(gen_bindings));
  }

  // Instance members shape: one MIR slot per child instance.
  DeclareInstanceMemberShapes(lowerer, shape);

  // Procedural-storage scope plan (LRM 9.3.5 / 23.9). Every
  // `ProceduralScopeDecl` in the HIR scope tree becomes a runtime class,
  // regardless of kind, label, or contents. Physical containment is
  // uniform; the SV-visible hierarchy is a projection over it, computed
  // at lookup time: a scope with an SV label emits its label as the
  // runtime segment and is addressable by name, while an anonymous scope
  // emits an empty segment so peer by-name lookup walks past it
  // transparently (LRM 23 hierarchical-name semantics). Each static lives
  // on its own lexical scope's class -- no elevation, no
  // lexical-vs-physical owner distinction.
  std::vector<mir::ClassId> scope_classes(hir_scope.procedural_scopes.size());
  std::vector<mir::ClassShape> sub_shapes(hir_scope.procedural_scopes.size());
  for (std::size_t i = 0; i < hir_scope.procedural_scopes.size(); ++i) {
    const auto& decl = hir_scope.procedural_scopes.Get(
        hir::ProceduralScopeId{static_cast<std::uint32_t>(i)});
    const mir::ClassId class_id = module.Unit().DeclareClass();
    scope_classes[i] = class_id;
    const std::string scope_token =
        decl.label.has_value() ? *decl.label : std::format("anon_{}", i);
    mir::ClassShape sub_shape;
    sub_shape.name = std::format("{}__{}", name_, scope_token);
    sub_shape.base = mir::ClassRef{mir::RuntimeLibraryClassRef{
        .base_type =
            module.Unit().types.Intern(mir::ProceduralStorageScopeType{})}};
    const mir::TypeId sub_self_object =
        module.Unit().types.Intern(mir::ObjectType{.class_id = class_id});
    sub_shape.self_pointer_type = module.Unit().types.PointerTo(
        sub_self_object, mir::PointerOwnership::kBorrowed);
    sub_shape.time_resolution = hir_scope.time_resolution;
    const mir::BaseContract sub_contract =
        mir::ResolveBaseContract(module.Unit(), *sub_shape.base);
    for (const auto& param : sub_contract.ctor_prefix) {
      sub_shape.ctor_prefix_params.Add(param);
    }
    // Carry the enclosing structural scope's type aliases (typedefs declared
    // at module / generate level) so the cpp renderer can name those types
    // when emitting a static's slot inside this nested class. C++ enclosing-
    // class scope lookup makes the alias usable unqualified at the use site.
    sub_shape.type_aliases = shape.type_aliases;
    sub_shapes[i] = std::move(sub_shape);
  }

  scope_materialization_.Resize(hir_scope.procedural_scopes.size());
  std::vector<std::vector<std::optional<StaticStoragePlacement>>>
      subroutine_placements(hir_scope.structural_subroutines.size());
  std::vector<std::vector<std::optional<StaticStoragePlacement>>>
      process_placements(hir_scope.processes.size());

  const auto owner_shape_of = [&](StorageOwner owner) -> mir::ClassShape& {
    return std::visit(
        Overloaded{
            [&](EnclosingClass) -> mir::ClassShape& { return shape; },
            [&](hir::ProceduralScopeId sid) -> mir::ClassShape& {
              return sub_shapes[sid.value];
            }},
        owner);
  };

  // Every scope materializes; the recursion just threads `runtime_parent`
  // down. Statics land on their own lexical scope (no elevation).
  const auto place =
      [&](const auto& self_ref, const hir::ProceduralBody& body,
          std::string_view callable_name,
          std::vector<std::optional<StaticStoragePlacement>>& per_callable,
          hir::ProceduralScopeId scope_id,
          StorageOwner runtime_parent) -> void {
    const auto& scope = hir_scope.procedural_scopes.Get(scope_id);
    mir::ClassShape& parent_shape = owner_shape_of(runtime_parent);
    const mir::ClassId my_class = scope_classes[scope_id.value];
    const std::string scope_token =
        scope.label.has_value() ? *scope.label
                                : std::format("anon_{}", scope_id.value);
    const mir::TypeId companion_type =
        MakeUniqueObjectPointer(module, my_class);
    mir::MemberDecl companion_decl{
        .name = CompanionVarNameFor(scope_token), .type = companion_type};
    if (scope.label.has_value()) {
      companion_decl.source_name = *scope.label;
    }
    const mir::MemberId companion_id = parent_shape.members.Add(companion_decl);
    parent_shape.contained.push_back(my_class);
    scope_materialization_.Record(
        scope_id, MaterializedProceduralScope{
                      .class_id = my_class,
                      .companion_member = companion_id,
                      .runtime_parent = runtime_parent});
    const StorageOwner my_owner{scope_id};

    if (per_callable.size() < body.procedural_vars.size()) {
      per_callable.resize(body.procedural_vars.size());
    }
    for (const auto var_id : scope.direct_declarations) {
      const auto& v = body.procedural_vars.Get(var_id);
      if (v.lifetime != hir::VariableLifetime::kStatic) continue;

      const std::string mangled =
          std::format("{}__{}_{}", callable_name, v.name, var_id.value);
      const mir::TypeId type = module.TranslateType(v.type);
      const mir::TypeId field_type = MaybeWrapObservable(module, type);
      mir::MemberDecl member_decl{.name = mangled, .type = field_type};
      if (scope.label.has_value()) {
        member_decl.source_name = v.name;
      }
      const mir::MemberId mid =
          sub_shapes[scope_id.value].members.Add(member_decl);

      per_callable[var_id.value] =
          StaticStoragePlacement{.owner = my_owner, .member = mid};
    }

    for (const auto child_id : scope.direct_child_scopes) {
      self_ref(self_ref, body, callable_name, per_callable, child_id, my_owner);
    }
  };

  for (std::size_t i = 0; i < hir_scope.structural_subroutines.size(); ++i) {
    const auto& s = hir_scope.structural_subroutines.Get(
        hir::StructuralSubroutineId{static_cast<std::uint32_t>(i)});
    place(
        place, s.body, s.name, subroutine_placements[i], s.body.root_scope,
        StorageOwner{EnclosingClass{}});
  }
  for (std::size_t i = 0; i < hir_scope.processes.size(); ++i) {
    const auto& p =
        hir_scope.processes.Get(hir::ProcessId{static_cast<std::uint32_t>(i)});
    const std::string callable_name =
        std::format("process_{}", hir_scope.structural_subroutines.size() + i);
    place(
        place, p.body, callable_name, process_placements[i], p.body.root_scope,
        StorageOwner{EnclosingClass{}});
  }

  subroutine_storage_plans_.reserve(hir_scope.structural_subroutines.size());
  for (auto& placements : subroutine_placements) {
    subroutine_storage_plans_.emplace_back(
        scope_materialization_, std::move(placements));
  }
  process_storage_plans_.reserve(hir_scope.processes.size());
  for (auto& placements : process_placements) {
    process_storage_plans_.emplace_back(
        scope_materialization_, std::move(placements));
  }

  // Publish each procedural-storage scope's shape; the body sweep below
  // builds each class's constructor against the published shape.
  for (std::size_t i = 0; i < hir_scope.procedural_scopes.size(); ++i) {
    module.DefineClassShape(scope_classes[i], std::move(sub_shapes[i]));
  }

  module.DefineClassShape(class_id_, std::move(shape));
  return class_id_;
}

auto StructuralScopeLowerer::PopulateBodies(WalkFrame parent_frame)
    -> diag::Result<void> {
  ModuleLowerer& module = *module_;
  const hir::StructuralScope& hir_scope = *hir_scope_;
  StructuralScopeLowerer& lowerer = *this;

  const mir::ClassShape& shape = module.GetClassShape(class_id_);
  mir::Class mir_class;
  mir_class.name = shape.name;
  mir_class.base = shape.base;
  mir_class.self_pointer_type = shape.self_pointer_type;
  mir_class.time_resolution = shape.time_resolution;
  mir_class.ctor_prefix_params = shape.ctor_prefix_params;
  mir_class.params = shape.params;
  mir_class.members = shape.members;
  mir_class.contained = shape.contained;
  mir_class.type_aliases = shape.type_aliases;

  const mir::TypeId void_type = module.Unit().builtins.void_type;
  const mir::TypeId self_ptr_type = mir_class.self_pointer_type;
  ScopeChainNode outer_scope_link{};
  const auto seed_self = [&](CallableBindings& bindings) -> mir::LocalId {
    return bindings.Declare(
        BindingOriginId::Receiver(),
        mir::LocalDecl{.name = "self", .type = self_ptr_type});
  };

  // Each lifecycle phase is a callable like any other: `self` is the receiver
  // binding seeded into its `locals`, and every nested block's `self` read
  // resolves through that one binding.
  mir::CallableCode ctor_code;
  CallableBindings ctor_bindings(module.Unit(), ctor_code);
  const mir::LocalId self_id = seed_self(ctor_bindings);
  mir::Block& ctor_block = ctor_code.body;
  const WalkFrame ctor_frame =
      parent_frame.WithClass(&mir_class, outer_scope_link)
          .WithBlock(&ctor_block)
          .WithBindings(&ctor_bindings);

  mir::CallableCode initialize_code;
  CallableBindings init_bindings(module.Unit(), initialize_code);
  const mir::LocalId init_self_id = seed_self(init_bindings);
  mir::Block& initialize_block = initialize_code.body;
  const WalkFrame init_frame =
      parent_frame.WithClass(&mir_class, outer_scope_link)
          .WithBlock(&initialize_block)
          .WithBindings(&init_bindings);

  mir::CallableCode resolve_code;
  CallableBindings resolve_bindings(module.Unit(), resolve_code);
  const mir::LocalId resolve_self_id = seed_self(resolve_bindings);
  mir::Block& resolve_block = resolve_code.body;
  const WalkFrame resolve_frame =
      parent_frame.WithClass(&mir_class, outer_scope_link)
          .WithBlock(&resolve_block)
          .WithBindings(&resolve_bindings);

  mir::CallableCode activate_code;
  CallableBindings activate_bindings(module.Unit(), activate_code);
  const mir::LocalId activate_self_id = seed_self(activate_bindings);
  mir::Block& activate_block = activate_code.body;
  const WalkFrame activate_frame =
      parent_frame.WithClass(&mir_class, outer_scope_link)
          .WithBlock(&activate_block)
          .WithBindings(&activate_bindings);
  const auto self_read = [&]() -> mir::ExprId {
    return ctor_block.exprs.Add(MakeSelfRefExpr(ctor_frame, self_ptr_type));
  };
  const auto init_self_read = [&]() -> mir::ExprId {
    return initialize_block.exprs.Add(
        MakeSelfRefExpr(init_frame, self_ptr_type));
  };

  for (std::size_t i = 0; i < hir_scope.structural_data_objects.size(); ++i) {
    const hir::StructuralDataObjectId hir_id{static_cast<std::uint32_t>(i)};
    const auto& d = hir_scope.structural_data_objects.Get(hir_id);
    const mir::MemberId mir_id =
        lowerer.TranslateStructuralDataObject(hir::StructuralHops{0}, hir_id);
    const mir::TypeId mir_field_type = mir_class.members.Get(mir_id).type;
    const mir::TypeId mir_value_type = module.TranslateType(d.type);
    const auto* var = std::get_if<hir::StructuralVariableDecl>(&d.kind);
    const bool is_net = var == nullptr;
    const bool is_reference = var != nullptr && var->reference.has_value();
    const mir::TypeKind var_kind =
        module.Unit().types.Get(mir_value_type).Kind();
    // Owned children (pointer / vector / object), cross-instance reference
    // slots (borrowed pointers filled in the resolve phase), and named events
    // have no "value assignment" -- their declaration shape itself fixes the
    // field at construction. A net takes none either: its value is produced by
    // its drivers, seeded when each driver updates in the initialize phase.
    // Value-typed variables (integral, string, real, unpacked / dynamic array)
    // receive an LRM 10.5 initialization statement, run in the initialize
    // phase after the tree's references resolve, not in the constructor.
    const bool is_assignable_value =
        !is_reference && !is_net && var_kind != mir::TypeKind::kPointer &&
        var_kind != mir::TypeKind::kVector &&
        var_kind != mir::TypeKind::kObject &&
        var_kind != mir::TypeKind::kExternalUnitObject &&
        var_kind != mir::TypeKind::kEvent;
    if (is_assignable_value) {
      const mir::ExprId init_target = initialize_block.exprs.Add(
          mir::MakeMemberAccessExpr(
              init_self_read(), mir::MemberRef{.var = mir_id}, mir_field_type));
      const auto append_stmt = [&](mir::Expr expr) {
        initialize_block.AppendStmt(
            mir::Stmt{
                .label = std::nullopt,
                .data = mir::ExprStmt{
                    .expr = initialize_block.exprs.Add(std::move(expr))}});
      };
      // An observable cell init routes the value through the cell's wrapper so
      // its engine-side change-tracking sees it; a plain field gets a regular
      // assignment.
      const auto emit_value_store = [&](mir::ExprId value_id) {
        const mir::ExprId services_id = initialize_block.exprs.Add(
            mir::MakeServicesCallExpr(
                init_self_read(), module.Unit().builtins.services));
        append_stmt(BuildObservableAssignExpr(
            module.Unit(), initialize_block, services_id, init_target, value_id,
            std::nullopt, mir_value_type, module.Unit().builtins.void_type));
      };

      // Every observable value cell installs its declared representation and
      // default at construction (LRM 10.5), so its type is fixed by
      // construction and a later store -- including a user initializer -- is
      // verified against it rather than discovered from whichever store runs
      // first. A non-observable value member carries no cell wrapper, so it
      // installs its representation through an ordinary store of the default.
      if (mir::IsObservableCellType(module.Unit().types.Get(mir_field_type))) {
        const mir::ExprId prototype = initialize_block.exprs.Add(
            BuildDefaultValueFromHir(module, init_frame, d.type));
        append_stmt(
            mir::MakeObservableInitializeCallExpr(
                init_target, prototype, module.Unit().builtins.void_type));
        if (var->initializer.has_value()) {
          auto value_or = lowerer.LowerExpr(
              hir_scope.exprs.Get(*var->initializer), init_frame);
          if (!value_or) return std::unexpected(std::move(value_or.error()));
          emit_value_store(initialize_block.exprs.Add(*std::move(value_or)));
        }
      } else {
        mir::ExprId value_id{};
        if (var->initializer.has_value()) {
          auto value_or = lowerer.LowerExpr(
              hir_scope.exprs.Get(*var->initializer), init_frame);
          if (!value_or) return std::unexpected(std::move(value_or.error()));
          value_id = initialize_block.exprs.Add(*std::move(value_or));
        } else {
          value_id = initialize_block.exprs.Add(
              BuildDefaultValueFromHir(module, init_frame, d.type));
        }
        emit_value_store(value_id);
      }
    }

    // A net cell installs its empty-driver resolved value at construction (LRM
    // 6.6.1), in the constructor rather than the initialize phase: a net is a
    // readable, well-typed observable before any driver attaches, and before a
    // cross-unit reader seeds from it during the parent-first initialize phase,
    // so an undriven read sees the net type's undriven value, never an
    // uninitialized cell. Drivers, attached at Resolve, update it from there.
    if (is_net) {
      const mir::ExprId net_target = ctor_block.exprs.Add(
          mir::MakeMemberAccessExpr(
              self_read(), mir::MemberRef{.var = mir_id}, mir_field_type));
      const mir::ExprId prototype = ctor_block.exprs.Add(
          BuildDefaultValueFromHir(module, ctor_frame, d.type));
      ctor_block.AppendStmt(
          mir::Stmt{
              .label = std::nullopt,
              .data = mir::ExprStmt{
                  .expr = ctor_block.exprs.Add(
                      mir::MakeObservableInitializeCallExpr(
                          net_target, prototype, void_type))}});
    }

    // A value signal, or a named event, records its address under its name so a
    // cross-unit referrer resolves it by name at construction. The excluded
    // members -- owned children and cross-unit reference slots -- are not
    // signals.
    const bool is_signal = var_kind != mir::TypeKind::kPointer &&
                           var_kind != mir::TypeKind::kVector &&
                           var_kind != mir::TypeKind::kObject &&
                           var_kind != mir::TypeKind::kExternalUnitObject;
    if (is_signal) {
      const mir::ExprId var_ref = ctor_block.exprs.Add(
          mir::MakeMemberAccessExpr(
              self_read(), mir::MemberRef{.var = mir_id}, mir_field_type));
      const mir::TypeId var_ptr_type = module.Unit().types.PointerTo(
          mir_field_type, mir::PointerOwnership::kBorrowed);
      const mir::ExprId addr_id =
          ctor_block.exprs.Add(mir::MakeAddressOfExpr(var_ref, var_ptr_type));
      const mir::ExprId name_id = ctor_block.exprs.Add(
          mir::Expr{
              .data = mir::StringLiteral{.value = d.name},
              .type = module.Unit().builtins.string});
      const mir::ExprId call = ctor_block.exprs.Add(
          mir::Expr{
              .data =
                  mir::CallExpr{
                      .callee =
                          mir::Direct{
                              .target = support::BuiltinFn::kRegisterSignal},
                      .arguments = {self_read(), name_id, addr_id}},
              .type = void_type});
      ctor_block.AppendStmt(mir::ExprStmt{.expr = call});
    }
  }

  // Build each materialized procedural-storage scope's class: copy its
  // already-published shape into a `mir::Class`, build a constructor whose
  // body constructs the immediate-child materialized scopes the same way an
  // owned-child instance is constructed, and commit. This runs before any
  // body lowers so static-init lowering against the structural constructor
  // (which writes `self->companion->...->static = init`) reaches storage the
  // runtime has already built. The structural constructor then appends
  // construction calls for top-level materialized scopes below, before the
  // subroutine and process bodies that initialize their statics.
  for (std::size_t i = 0; i < scope_materialization_.Size(); ++i) {
    const hir::ProceduralScopeId scope_id{static_cast<std::uint32_t>(i)};
    const auto& entry = scope_materialization_.Get(scope_id);
    const mir::ClassShape& sub_shape = module.GetClassShape(entry.class_id);
    mir::Class sub_class;
    sub_class.name = sub_shape.name;
    sub_class.base = sub_shape.base;
    sub_class.self_pointer_type = sub_shape.self_pointer_type;
    sub_class.time_resolution = sub_shape.time_resolution;
    sub_class.ctor_prefix_params = sub_shape.ctor_prefix_params;
    sub_class.params = sub_shape.params;
    sub_class.members = sub_shape.members;
    sub_class.contained = sub_shape.contained;
    sub_class.type_aliases = sub_shape.type_aliases;

    mir::CallableCode sub_ctor_code;
    CallableBindings sub_ctor_bindings(module.Unit(), sub_ctor_code);
    const mir::LocalId sub_self_id = sub_ctor_bindings.Declare(
        BindingOriginId::Receiver(),
        mir::LocalDecl{.name = "self", .type = sub_shape.self_pointer_type});
    mir::Block& sub_ctor_block = sub_ctor_code.body;
    ScopeChainNode sub_scope_link{};
    const WalkFrame sub_ctor_frame =
        parent_frame.WithClass(&sub_class, sub_scope_link)
            .WithBlock(&sub_ctor_block)
            .WithBindings(&sub_ctor_bindings);
    for (std::size_t j = 0; j < scope_materialization_.Size(); ++j) {
      const hir::ProceduralScopeId child_id{static_cast<std::uint32_t>(j)};
      const auto& child_entry = scope_materialization_.Get(child_id);
      const auto* parent_scope =
          std::get_if<hir::ProceduralScopeId>(&child_entry.runtime_parent);
      if (parent_scope == nullptr || parent_scope->value != i) continue;
      AppendOwnedChildConstruction(
          module, sub_ctor_frame, child_entry.companion_member,
          child_entry.class_id, {}, std::nullopt);
    }
    // Register each static on this procedural-storage scope as a by-name
    // signal so a cross-compilation-unit descent (`Top.outer.x` from
    // another unit) finds it through `GetSignal` once it has navigated to
    // this scope by `GetChild`. Companion members are pointers to nested
    // scopes and are registered as children via `AttachChild`, not as
    // signals, so the pointer kinds are excluded.
    for (std::size_t mi = 0; mi < sub_class.members.size(); ++mi) {
      const mir::MemberId mid{static_cast<std::uint32_t>(mi)};
      const auto& m = sub_class.members.Get(mid);
      if (m.source_name.empty()) continue;
      const mir::TypeKind mk = module.Unit().types.Get(m.type).Kind();
      if (mk == mir::TypeKind::kPointer || mk == mir::TypeKind::kVector ||
          mk == mir::TypeKind::kObject) {
        continue;
      }
      const mir::ExprId sub_self = sub_ctor_block.exprs.Add(
          MakeSelfRefExpr(sub_ctor_frame, sub_shape.self_pointer_type));
      const mir::ExprId member_ref = sub_ctor_block.exprs.Add(
          mir::MakeMemberAccessExpr(
              sub_self, mir::MemberRef{.var = mid}, m.type));
      const mir::TypeId addr_type = module.Unit().types.PointerTo(
          m.type, mir::PointerOwnership::kBorrowed);
      const mir::ExprId addr = sub_ctor_block.exprs.Add(
          mir::MakeAddressOfExpr(member_ref, addr_type));
      const mir::ExprId name_lit = sub_ctor_block.exprs.Add(
          mir::Expr{
              .data = mir::StringLiteral{.value = m.source_name},
              .type = module.Unit().builtins.string});
      const mir::ExprId reg_self = sub_ctor_block.exprs.Add(
          MakeSelfRefExpr(sub_ctor_frame, sub_shape.self_pointer_type));
      const mir::ExprId call = sub_ctor_block.exprs.Add(
          mir::Expr{
              .data =
                  mir::CallExpr{
                      .callee =
                          mir::Direct{
                              .target = support::BuiltinFn::kRegisterSignal},
                      .arguments = {reg_self, name_lit, addr}},
              .type = void_type});
      sub_ctor_block.AppendStmt(mir::ExprStmt{.expr = call});
    }
    sub_ctor_code.params = {sub_self_id};
    sub_ctor_code.result_type = void_type;
    sub_class.constructor = std::move(sub_ctor_code);
    module.Unit().DefineClass(entry.class_id, std::move(sub_class));
  }

  // Construct every top-level materialized procedural-storage scope in the
  // structural constructor, before the subroutine and process bodies lower
  // so static inits that target storage inside one of these scopes write
  // through an already-built `self->companion->...->static` chain.
  for (std::size_t i = 0; i < scope_materialization_.Size(); ++i) {
    const hir::ProceduralScopeId scope_id{static_cast<std::uint32_t>(i)};
    const auto& entry = scope_materialization_.Get(scope_id);
    if (!std::holds_alternative<EnclosingClass>(entry.runtime_parent)) continue;
    AppendOwnedChildConstruction(
        module, ctor_frame, entry.companion_member, entry.class_id, {},
        std::nullopt);
  }

  for (std::size_t i = 0; i < hir_scope.structural_subroutines.size(); ++i) {
    const auto& src = hir_scope.structural_subroutines.Get(
        hir::StructuralSubroutineId{static_cast<std::uint32_t>(i)});
    ProcessLowerer subroutine_lowerer(
        module, &lowerer, hir_scope.time_resolution, src.body, src.name,
        mir::MethodVisibility::kInternal, ctor_frame,
        subroutine_storage_plans_[i]);
    auto decl_or = subroutine_lowerer.Run(src);
    if (!decl_or) return std::unexpected(std::move(decl_or.error()));
    const mir::MethodId added = mir_class.methods.Add(*std::move(decl_or));
    if (added.value != i) {
      throw InternalError(
          "StructuralScopeLowerer::PopulateBodies: subroutine added out of "
          "mapped id order");
    }
    for (const auto& pending :
         subroutine_lowerer.TakePendingStaticInitializers()) {
      auto integ = IntegratePendingStaticInitializer(
          subroutine_lowerer, src.body, init_frame, pending);
      if (!integ) return std::unexpected(std::move(integ.error()));
    }
  }

  for (std::size_t i = 0; i < hir_scope.processes.size(); ++i) {
    const auto& p =
        hir_scope.processes.Get(hir::ProcessId{static_cast<std::uint32_t>(i)});
    std::string name = std::format("process_{}", mir_class.methods.size());
    ProcessLowerer process_lowerer(
        module, &lowerer, hir_scope.time_resolution, p.body, std::move(name),
        mir::MethodVisibility::kInternal, ctor_frame,
        process_storage_plans_[i]);
    auto decl_or = process_lowerer.Run(p);
    if (!decl_or) return std::unexpected(std::move(decl_or.error()));
    const mir::MethodId body = mir_class.methods.Add(*std::move(decl_or));
    AppendProcessRegistration(
        module, activate_frame, body, p.kind == hir::ProcessKind::kFinal);
    for (const auto& pending :
         process_lowerer.TakePendingStaticInitializers()) {
      auto integ = IntegratePendingStaticInitializer(
          process_lowerer, p.body, init_frame, pending);
      if (!integ) return std::unexpected(std::move(integ.error()));
    }
  }

  std::vector<mir::MemberId> driven_nets;
  for (const auto& ca : hir_scope.continuous_assigns) {
    // A continuous assignment to a net is a driver (LRM 6.5): it attaches a
    // driver slot at Resolve, seeds it at Initialize, and the activation
    // process updates it. A variable target keeps the direct continuous-assign
    // write.
    std::optional<NetDriver> net_driver;
    if (const auto net_member =
            ContinuousAssignNetTarget(lowerer, mir_class, ca)) {
      // A net carries one driver; a second driver on the same net is rejected
      // at lowering rather than left to fail at runtime.
      const bool already_driven = std::ranges::any_of(
          driven_nets,
          [&](mir::MemberId m) { return m.value == net_member->value; });
      if (already_driven) {
        return diag::Fail(
            ca.span, diag::DiagCode::kUnsupportedContinuousAssignForm,
            "a net with more than one driver is not yet supported");
      }
      driven_nets.push_back(*net_member);
      // The net cell is a local member; its resolve-phase lvalue is a member
      // access off `self`.
      mir::Block& resolve_block = *resolve_frame.current_block;
      const mir::TypeId net_type = mir_class.members.Get(*net_member).type;
      const mir::TypeId value_type =
          std::get<mir::ResolvedType>(module.Unit().types.Get(net_type).data)
              .value;
      const mir::ExprId net_self = resolve_block.exprs.Add(
          MakeSelfRefExpr(resolve_frame, self_ptr_type));
      const mir::ExprId net_access = resolve_block.exprs.Add(
          mir::MakeMemberAccessExpr(
              net_self, mir::MemberRef{.var = *net_member}, net_type));
      const std::string driver_name =
          std::format("{}__driver", mir_class.members.Get(*net_member).name);
      auto driver_or = AttachNetDriver(
          lowerer, mir_class, net_access, value_type, driver_name, ca.rhs,
          resolve_frame, init_frame, self_ptr_type);
      if (!driver_or) return std::unexpected(std::move(driver_or.error()));
      net_driver = *driver_or;
    }

    std::string name = std::format("process_{}", mir_class.methods.size());
    auto decl_or = LowerContinuousAssign(
        lowerer, ctor_frame, std::move(name), ca, net_driver);
    if (!decl_or) return std::unexpected(std::move(decl_or.error()));
    const mir::MethodId body = mir_class.methods.Add(*std::move(decl_or));
    AppendProcessRegistration(module, activate_frame, body, false);
  }

  // Recurse into descendants. Every class's shape is already published, so a
  // body that names a peer's member resolves through the existing identity
  // model regardless of which sibling lowers next.
  for (auto& child : children_) {
    auto child_r = child->PopulateBodies(ctor_frame);
    if (!child_r) return std::unexpected(std::move(child_r.error()));
  }

  for (std::size_t i = 0; i < hir_scope.generates.size(); ++i) {
    auto stmt = LowerGenerateAsStmt(
        lowerer, ctor_frame,
        hir_scope.generates.Get(hir::GenerateId{static_cast<std::uint32_t>(i)}),
        lowerer.LookupGenerateBindings(
            hir::GenerateId{static_cast<std::uint32_t>(i)}));
    if (!stmt) return std::unexpected(std::move(stmt.error()));
    ctor_block.AppendStmt(*std::move(stmt));
  }

  EmitInstanceMemberConstruction(lowerer, ctor_frame);
  InstallCrossUnitRefs(lowerer, resolve_frame);
  auto port_conn_r = InstallPortConnections(
      lowerer, ctor_frame, resolve_frame, init_frame, activate_frame);
  if (!port_conn_r) return std::unexpected(std::move(port_conn_r.error()));

  ctor_code.params = {self_id};
  ctor_code.result_type = void_type;
  mir_class.constructor = std::move(ctor_code);
  // The resolve, initialize, and activate phases are synthesized callables run
  // by the engine after construction (LRM 23.3.3.2 / 6.8 / 9.2), present only
  // when the scope has work for that phase. Each is an ordinary method that
  // overrides the matching runtime-base hook; the override target is a resolved
  // reference, so the engine reaches the body through one override-driven shim
  // -- the same machinery a user-defined virtual method uses.
  if (!resolve_block.root_stmts.empty()) {
    resolve_code.params = {resolve_self_id};
    resolve_code.result_type = void_type;
    mir_class.methods.Add(
        mir::MethodDecl{
            .name = "ResolveState",
            .code = std::move(resolve_code),
            .overrides = mir::OverriddenMethodRef{mir::RuntimeLibraryMethodRef{
                .method = mir::RuntimeMethod::kResolve}},
            .visibility = mir::MethodVisibility::kInternal});
  }
  if (!initialize_block.root_stmts.empty()) {
    initialize_code.params = {init_self_id};
    initialize_code.result_type = void_type;
    mir_class.methods.Add(
        mir::MethodDecl{
            .name = "InitializeState",
            .code = std::move(initialize_code),
            .overrides = mir::OverriddenMethodRef{mir::RuntimeLibraryMethodRef{
                .method = mir::RuntimeMethod::kInitialize}},
            .visibility = mir::MethodVisibility::kInternal});
  }
  if (!activate_block.root_stmts.empty()) {
    activate_code.params = {activate_self_id};
    activate_code.result_type = void_type;
    mir_class.methods.Add(
        mir::MethodDecl{
            .name = "CreateProcesses",
            .code = std::move(activate_code),
            .overrides = mir::OverriddenMethodRef{mir::RuntimeLibraryMethodRef{
                .method = mir::RuntimeMethod::kActivate}},
            .visibility = mir::MethodVisibility::kInternal});
  }

  // A runtime tree node overrides the runtime virtuals that report its constant
  // properties -- its time precision (LRM 3.14.2), and a module instance its
  // def-name (LRM 23.8) -- as ordinary methods whose body returns the constant.
  // Added after the subroutines, whose method ids are pre-mapped.
  if (mir_class.base.has_value()) {
    const mir::BaseContract contract =
        mir::ResolveBaseContract(module.Unit(), *mir_class.base);
    const mir::TypeId self_object_type =
        module.Unit().types.Intern(mir::ObjectType{.class_id = class_id_});
    // These accessors only read the object, so their receiver is a read-only
    // borrow (an immutable `&self`); the render derives the `const` shim from
    // it.
    const mir::TypeId self_readonly_pointer_type =
        module.Unit().types.PointerTo(
            self_object_type, mir::PointerOwnership::kBorrowed,
            mir::Mutability::kReadOnly);
    if (contract.is_runtime_tree_node) {
      // The precision power is plain machine metadata (LRM 3.14.2), so its type
      // is a machine integer rather than a 4-state SV value -- it stays a raw
      // `int8` end to end, with no value-wrapper round-trip at the engine.
      const mir::TypeId precision_type = module.Unit().types.Intern(
          mir::MachineIntType{
              .bit_width = 8, .signedness = mir::Signedness::kSigned});
      mir::CallableCode precision_code;
      const mir::LocalId precision_self = precision_code.locals.Add(
          mir::LocalDecl{.name = "self", .type = self_readonly_pointer_type});
      mir::Block& precision_block = precision_code.body;
      const mir::ExprId precision_value = precision_block.exprs.Add(
          mir::Expr{
              .data =
                  mir::IntegerLiteral{
                      .value =
                          mir::IntegralConstant{
                              .value_words = {static_cast<
                                  std::uint64_t>(static_cast<std::int64_t>(
                                  mir_class.time_resolution.precision_power))},
                              .state_words = {},
                              .width = 8,
                              .signedness = mir::Signedness::kSigned,
                              .state_kind = mir::IntegralStateKind::kTwoState}},
              .type = precision_type});
      precision_block.AppendStmt(
          mir::ReturnStmt{
              .value = precision_value, .is_coroutine_return = false});
      precision_code.params = {precision_self};
      precision_code.result_type = precision_type;
      mir_class.methods.Add(
          mir::MethodDecl{
              .name = "TimePrecisionPower",
              .code = std::move(precision_code),
              .overrides =
                  mir::OverriddenMethodRef{mir::RuntimeLibraryMethodRef{
                      .method = mir::RuntimeMethod::kTimePrecisionPower}},
              .visibility = mir::MethodVisibility::kInternal});
    }
    if (contract.exposes_def_name) {
      const mir::TypeId string_view_type =
          module.Unit().types.Intern(mir::StringViewType{});
      mir::CallableCode def_name_code;
      const mir::LocalId def_name_self = def_name_code.locals.Add(
          mir::LocalDecl{.name = "self", .type = self_readonly_pointer_type});
      mir::Block& def_name_block = def_name_code.body;
      const mir::ExprId def_name_value = def_name_block.exprs.Add(
          mir::Expr{
              .data = mir::StringLiteral{.value = mir_class.name},
              .type = string_view_type});
      def_name_block.AppendStmt(
          mir::ReturnStmt{
              .value = def_name_value, .is_coroutine_return = false});
      def_name_code.params = {def_name_self};
      def_name_code.result_type = string_view_type;
      mir_class.methods.Add(
          mir::MethodDecl{
              .name = "DefName",
              .code = std::move(def_name_code),
              .overrides =
                  mir::OverriddenMethodRef{mir::RuntimeLibraryMethodRef{
                      .method = mir::RuntimeMethod::kDefName}},
              .visibility = mir::MethodVisibility::kInternal});
    }
  }

  module.Unit().DefineClass(class_id_, std::move(mir_class));
  return {};
}

}  // namespace lyra::lowering::hir_to_mir
