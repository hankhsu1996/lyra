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
#include "lyra/base/overloaded.hpp"
#include "lyra/diag/diagnostic.hpp"
#include "lyra/hir/structural_scope.hpp"
#include "lyra/lowering/hir_to_mir/binding_origin.hpp"
#include "lyra/lowering/hir_to_mir/callable_bindings.hpp"
#include "lyra/lowering/hir_to_mir/case_cascade.hpp"
#include "lyra/lowering/hir_to_mir/continuous_assign.hpp"
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
  bool is_repeated;
  std::vector<ScopeEntryStructuralParamBinding> entry_bindings;
};

auto EnumerateGenerateChildSpecs(
    const hir::Generate& gen, const hir::StructuralScope& enclosing_scope,
    ModuleLowerer& module) -> std::vector<GenerateChildSpec> {
  std::vector<GenerateChildSpec> specs;
  std::visit(
      Overloaded{
          [&](const hir::IfGenerate& if_gen) {
            const auto& then_scope = gen.child_scopes.Get(if_gen.then_scope);
            specs.push_back(
                {.scope_id = if_gen.then_scope,
                 .scope = &then_scope,
                 .scope_name = module.NextGenerateScopeName("then"),
                 .is_repeated = false,
                 .entry_bindings = {}});
            if (if_gen.else_scope.has_value()) {
              const auto& else_scope = gen.child_scopes.Get(*if_gen.else_scope);
              specs.push_back(
                  {.scope_id = *if_gen.else_scope,
                   .scope = &else_scope,
                   .scope_name = module.NextGenerateScopeName("else"),
                   .is_repeated = false,
                   .entry_bindings = {}});
            }
          },
          [&](const hir::CaseGenerate& case_gen) {
            for (std::size_t k = 0; k < case_gen.items.size(); ++k) {
              const auto& item_scope =
                  gen.child_scopes.Get(case_gen.items[k].scope);
              specs.push_back(
                  {.scope_id = case_gen.items[k].scope,
                   .scope = &item_scope,
                   .scope_name =
                       module.NextGenerateScopeName(std::format("case{}", k)),
                   .is_repeated = false,
                   .entry_bindings = {}});
            }
            if (case_gen.default_scope.has_value()) {
              const auto& default_scope =
                  gen.child_scopes.Get(*case_gen.default_scope);
              specs.push_back(
                  {.scope_id = *case_gen.default_scope,
                   .scope = &default_scope,
                   .scope_name = module.NextGenerateScopeName("default"),
                   .is_repeated = false,
                   .entry_bindings = {}});
            }
          },
          [&](const hir::LoopGenerate& loop_gen) {
            const auto& loop_scope = gen.child_scopes.Get(loop_gen.scope);
            const auto& var_decl =
                enclosing_scope.loop_var_decls.Get(loop_gen.loop_var);
            std::vector<ScopeEntryStructuralParamBinding> bindings;
            bindings.push_back(
                ScopeEntryStructuralParamBinding{
                    .param =
                        mir::ParamDecl{
                            .name = var_decl.name,
                            .type = module.TranslateType(var_decl.type)},
                    .source_loop_var = loop_gen.loop_var});
            specs.push_back(
                {.scope_id = loop_gen.scope,
                 .scope = &loop_scope,
                 .scope_name = module.NextGenerateScopeName("loop"),
                 .is_repeated = true,
                 .entry_bindings = std::move(bindings)});
          },
      },
      gen.data);
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

// Allocates one MIR member per cross-unit reference. Upward references take
// the wrapper-typed ExternUp form whose per-reference state lands as ordinary
// `CallExpr` statements in the constructor body. Downward references take a
// borrowed-pointer slot the constructor fills by navigating from `self` once
// the children are built. Both run before bodies so reads resolve to the
// member; both record their MIR read target as a StructuralDataObjectRef to
// that member, in HIR slot order.
void DeclareCrossUnitRefSlots(
    StructuralScopeLowerer& lowerer, mir::ClassShape& shape) {
  ModuleLowerer& module = lowerer.Module();
  const hir::StructuralScope& hir_scope = lowerer.HirScope();
  std::uint32_t slot_index = 0;
  for (const auto& cu : hir_scope.cross_unit_refs) {
    std::string member_name = "xref" + std::to_string(slot_index++);
    const bool is_upward =
        std::holds_alternative<hir::UpwardRootHead>(cu.head) ||
        std::holds_alternative<hir::UpwardNamedHead>(cu.head);
    if (is_upward) {
      // The member's type carries only the wrapped element; the
      // per-reference navigation plan is emitted as ordinary `CallExpr`
      // statements in the constructor body when the bind sweep runs.
      const mir::TypeId leaf = module.TranslateType(cu.type);
      const mir::TypeId ext_type =
          module.Unit().types.Intern(mir::ExternalRefType{.element = leaf});
      const mir::MemberId var = shape.members.Add(
          mir::MemberDecl{.name = std::move(member_name), .type = ext_type});
      lowerer.AddCrossUnitRefTarget(mir::MemberRef{.var = var}, ext_type);
    } else {
      // The pointee matches the producer's storage cell. A producer-side
      // value-storage signal is wrapped in `ObservableType` at its declaration
      // (so a write fires subscribers), and the cross-unit pointer must point
      // at the same wrapped cell -- otherwise the C++ types mismatch.
      const mir::TypeId leaf =
          MaybeWrapObservable(module, module.TranslateType(cu.type));
      const mir::TypeId slot_type =
          module.Unit().types.PointerTo(leaf, mir::PointerOwnership::kBorrowed);
      const mir::MemberId slot = shape.members.Add(
          mir::MemberDecl{.name = std::move(member_name), .type = slot_type});
      lowerer.AddCrossUnitRefTarget(mir::MemberRef{.var = slot}, slot_type);
    }
  }
}

// Builds the resolve value for a downward reference: a chain of generic
// navigation calls from the enclosing scope. The owned-child head and each
// crossed member open a `GetChild(name, indices)`; the leaf signal is a
// `GetSignal(name)` whose result type is the slot's borrowed-pointer cell type,
// so render casts the untyped storage pointer mechanically. Per-dimension array
// indices are ordinary integer-literal arguments, never bundled with the name.
auto BuildDownwardNavValue(
    ModuleLowerer& module, WalkFrame frame, const std::string& head_name,
    const std::vector<hir::PathStep>& path, mir::TypeId slot_type,
    mir::TypeId scope_ptr_type) -> mir::Expr {
  mir::Block& ctor_block = *frame.current_block;
  const mir::TypeId self_ptr_type = frame.current_class->self_pointer_type;
  struct NavHop {
    std::string name;
    std::vector<mir::ExprId> indices;
  };
  std::vector<NavHop> hops;
  hops.push_back(NavHop{.name = head_name, .indices = {}});
  for (const auto& step : path) {
    if (const auto* member = std::get_if<hir::MemberHop>(&step)) {
      hops.push_back(NavHop{.name = member->name, .indices = {}});
    } else {
      const std::uint32_t index = std::get<hir::IndexHop>(step).index;
      hops.back().indices.push_back(ctor_block.exprs.Add(
          mir::MakeInt32Literal(
              module.Unit().builtins.int32, static_cast<std::int64_t>(index))));
    }
  }
  if (hops.size() < 2) {
    throw InternalError(
        "BuildDownwardNavValue: downward reference has no leaf signal past its "
        "owned child");
  }

  const auto& builtins = module.Unit().builtins;
  const auto string_literal = [&](const std::string& s) -> mir::ExprId {
    return ctor_block.exprs.Add(
        mir::Expr{
            .data = mir::StringLiteral{.value = s}, .type = builtins.string});
  };

  mir::ExprId cur = ctor_block.exprs.Add(MakeSelfRefExpr(frame, self_ptr_type));
  for (std::size_t i = 0; i + 1 < hops.size(); ++i) {
    const mir::TypeId indices_type = module.Unit().types.Intern(
        mir::UnpackedArrayType{
            .element_type = builtins.int32, .size = hops[i].indices.size()});
    const mir::ExprId indices_id = ctor_block.exprs.Add(
        mir::Expr{
            .data = mir::ArrayLiteralExpr{.elements = hops[i].indices},
            .type = indices_type});
    cur = ctor_block.exprs.Add(
        mir::Expr{
            .data =
                mir::CallExpr{
                    .callee =
                        mir::Direct{.target = support::BuiltinFn::kGetChild},
                    .arguments =
                        {cur, string_literal(hops[i].name), indices_id}},
            .type = scope_ptr_type});
  }
  const mir::TypeId void_ptr_type = module.Unit().types.PointerTo(
      builtins.void_type, mir::PointerOwnership::kBorrowed);
  const mir::ExprId get_signal_id = ctor_block.exprs.Add(
      mir::Expr{
          .data =
              mir::CallExpr{
                  .callee =
                      mir::Direct{.target = support::BuiltinFn::kGetSignal},
                  .arguments = {cur, string_literal(hops.back().name)}},
          .type = void_ptr_type});
  // Pointer-to-pointer reinterpret of the void* slot to the typed pointer
  // the call site expects -- the one true MIR cast (`static_cast<T>(...)`).
  return mir::Expr{
      .data = mir::CastExpr{.operand = get_signal_id}, .type = slot_type};
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

// Builds the resolve value for an intra-unit sibling-of-ancestor reference.
// The install climbs `down.hops` parent edges (`kParent` calls cast to the
// enclosing class's typed pointer) and then composes a chain of typed
// `MemberAccess` through the head and each descent step. Every receiver in
// the chain is a pointer (`Top*` -> `Pointer<unique, gen_a>` -> ...), so
// the C++ render emits `->` for every hop and the chain never needs an
// explicit `DerefExpr` between steps; the leaf `MemberAccess` yields the
// observable cell field whose address goes into the slot.
//
// The parent cast is sound because the structural-containment relation
// built at HIR-to-MIR proves the receiver's N-th runtime parent is exactly
// that enclosing specialization. The same invariant grounds the typed
// enclosing access used for bare-name reads through `StructuralDataObjectRef`;
// this path extends it one `MemberAccess` further.
auto BuildTypedEnclosingNavValue(
    const StructuralScopeLowerer& lowerer, WalkFrame frame,
    const hir::DownwardHead& down, const std::vector<hir::PathStep>& path,
    mir::TypeId slot_type) -> mir::ExprId {
  ModuleLowerer& module = lowerer.Module();
  mir::Block& ctor_block = *frame.current_block;
  const mir::EnclosingHops mir_hops{.value = down.hops.value};
  const mir::ExprId typed_enclosing =
      BuildEnclosingScopeReceiver(frame, module.Unit(), mir_hops);
  const mir::Class& enclosing_cls = frame.EnclosingClassAtHops(mir_hops);
  const mir::MemberId head_member =
      lowerer.TranslateOwnedChild(down.hops, down.child);
  const mir::TypeId head_field_type =
      enclosing_cls.members.Get(head_member).type;
  mir::ExprId receiver = ctor_block.exprs.Add(
      mir::Expr{
          .data =
              mir::MemberAccessExpr{
                  .receiver = typed_enclosing,
                  .member = mir::MemberRef{.var = head_member}},
          .type = head_field_type});
  const auto* head_ptr_ty = std::get_if<mir::PointerType>(
      &module.Unit().types.Get(head_field_type).data);
  if (head_ptr_ty == nullptr) {
    throw InternalError(
        "BuildTypedEnclosingNavValue: head member is not a pointer type");
  }
  const auto* head_obj_ty = std::get_if<mir::ObjectType>(
      &module.Unit().types.Get(head_ptr_ty->pointee).data);
  if (head_obj_ty == nullptr) {
    throw InternalError(
        "BuildTypedEnclosingNavValue: head pointee is not an intra-unit "
        "object type");
  }
  // Peer shape queries read the published shape store, not the unit's class
  // registry, which is not yet fully populated during body lowering.
  const mir::ClassShape* current_shape =
      &module.GetClassShape(head_obj_ty->class_id);
  for (std::size_t i = 0; i < path.size(); ++i) {
    const auto& step = path[i];
    const auto* mh = std::get_if<hir::MemberHop>(&step);
    if (mh == nullptr) {
      throw InternalError(
          "BuildTypedEnclosingNavValue: index hop in typed intra-unit descent "
          "is not yet supported");
    }
    const auto step_member = FindMemberByName(*current_shape, mh->name);
    if (!step_member.has_value()) {
      throw InternalError(
          "BuildTypedEnclosingNavValue: descent step member not found");
    }
    const mir::TypeId step_type = current_shape->members.Get(*step_member).type;
    receiver = ctor_block.exprs.Add(
        mir::Expr{
            .data =
                mir::MemberAccessExpr{
                    .receiver = receiver,
                    .member = mir::MemberRef{.var = *step_member}},
            .type = step_type});
    if (i + 1 == path.size()) {
      break;
    }
    const auto* step_ptr_ty =
        std::get_if<mir::PointerType>(&module.Unit().types.Get(step_type).data);
    if (step_ptr_ty == nullptr) {
      throw InternalError(
          "BuildTypedEnclosingNavValue: intermediate descent step is not a "
          "pointer to an instance");
    }
    const auto* step_obj_ty = std::get_if<mir::ObjectType>(
        &module.Unit().types.Get(step_ptr_ty->pointee).data);
    if (step_obj_ty == nullptr) {
      throw InternalError(
          "BuildTypedEnclosingNavValue: intermediate descent into a non-"
          "object type");
    }
    current_shape = &module.GetClassShape(step_obj_ty->class_id);
  }
  return ctor_block.exprs.Add(
      mir::Expr{
          .data = mir::AddressOfExpr{.operand = receiver}, .type = slot_type});
}

// Builds an `UnpackedArray<int32, N>` literal expression holding one element
// per supplied index. Used as the per-step indices argument to the runtime
// bind / suffix-step methods; an empty index list still produces a typed
// zero-element array so the call's signature is uniform.
auto BuildIndicesArrayExpr(
    ModuleLowerer& module, mir::Block& block,
    std::span<const std::uint32_t> indices) -> mir::ExprId {
  const auto& builtins = module.Unit().builtins;
  std::vector<mir::ExprId> index_exprs;
  index_exprs.reserve(indices.size());
  for (const std::uint32_t idx : indices) {
    index_exprs.push_back(block.exprs.Add(
        mir::MakeInt32Literal(builtins.int32, static_cast<std::int64_t>(idx))));
  }
  const mir::TypeId indices_type = module.Unit().types.Intern(
      mir::UnpackedArrayType{
          .element_type = builtins.int32, .size = indices.size()});
  return block.exprs.Add(
      mir::Expr{
          .data = mir::ArrayLiteralExpr{.elements = std::move(index_exprs)},
          .type = indices_type});
}

// Emits one `CallExpr` statement against the ExternUp member: the receiver
// is `self->member`, the callee is the named builtin method, the arguments
// run through the ordinary primitive forms (string literal, indices array
// literal, self ref). Used for the bind and per-suffix-step calls.
void AppendExternUpMethodCall(
    ModuleLowerer& module, WalkFrame frame, mir::MemberId extern_member,
    support::BuiltinFn method, std::vector<mir::ExprId> arguments) {
  mir::Block& block = *frame.current_block;
  mir::Class& mir_class = *frame.current_class;
  const mir::TypeId self_ptr_type = mir_class.self_pointer_type;
  const mir::TypeId void_type = module.Unit().builtins.void_type;

  const mir::ExprId self_ref =
      block.exprs.Add(MakeSelfRefExpr(frame, self_ptr_type));
  const mir::TypeId receiver_type = mir_class.members.Get(extern_member).type;
  std::vector<mir::ExprId> args;
  args.reserve(arguments.size() + 1);
  args.push_back(block.exprs.Add(
      mir::Expr{
          .data =
              mir::MemberAccessExpr{
                  .receiver = self_ref,
                  .member = mir::MemberRef{.var = extern_member}},
          .type = receiver_type}));
  for (const mir::ExprId arg : arguments) {
    args.push_back(arg);
  }
  const mir::ExprId call = block.exprs.Add(
      mir::Expr{
          .data =
              mir::CallExpr{
                  .callee = mir::Direct{.target = method},
                  .arguments = std::move(args)},
          .type = void_type});
  block.AppendStmt(mir::ExprStmt{.expr = call});
}

// `path_after_anchor` excludes the anchor (the `$root` token for a root
// anchor; the named head and any per-dimension indices attached to it for a
// named anchor) and ends with the leaf signal. The leaf step must carry no
// indices -- array selection on a cross-unit-ref leaf is an expression-level
// access, not part of the descent -- enforced as a compiler invariant.
void AppendExternUpBinding(
    ModuleLowerer& module, WalkFrame frame, mir::MemberId member,
    std::span<const hir::PathStep> path_after_anchor,
    support::BuiltinFn bind_callee,
    std::vector<mir::ExprId> bind_args_before_signal) {
  mir::Block& block = *frame.current_block;
  const auto& builtins = module.Unit().builtins;
  const auto string_literal = [&](std::string s) -> mir::ExprId {
    return block.exprs.Add(
        mir::Expr{
            .data = mir::StringLiteral{.value = std::move(s)},
            .type = builtins.string});
  };

  struct DescentStep {
    std::string name;
    std::vector<std::uint32_t> indices;
  };
  std::vector<DescentStep> steps;
  for (const auto& step : path_after_anchor) {
    if (const auto* m = std::get_if<hir::MemberHop>(&step)) {
      steps.push_back(DescentStep{.name = m->name, .indices = {}});
    } else {
      steps.back().indices.push_back(std::get<hir::IndexHop>(step).index);
    }
  }
  if (!steps.back().indices.empty()) {
    throw InternalError(
        "AppendExternUpBinding: leaf signal step carries indices; "
        "array selection on a cross-unit-ref leaf belongs to an "
        "expression-level access, not the descent");
  }
  std::string signal = std::move(steps.back().name);
  steps.pop_back();

  bind_args_before_signal.push_back(string_literal(std::move(signal)));
  AppendExternUpMethodCall(
      module, frame, member, bind_callee, std::move(bind_args_before_signal));

  for (const auto& step : steps) {
    AppendExternUpMethodCall(
        module, frame, member, support::BuiltinFn::kAddSuffixStep,
        {string_literal(step.name),
         BuildIndicesArrayExpr(module, block, step.indices)});
  }
}

// A downward slot resolves in the constructor by navigating from the enclosing
// scope after the children are built: an ordinary assignment of the navigation
// value into the borrowed-pointer slot. An upward ExternUp member's
// per-reference plan emits as a `BindVisibleChild` or `BindRoot` call against
// the member, followed by one `AddSuffixStep` call per descent step -- ordinary
// `CallExpr` carrying flat MIR primitives, so the backend renders them
// uniformly with every other call.
void InstallCrossUnitRefs(StructuralScopeLowerer& lowerer, WalkFrame frame) {
  mir::Class& mir_class = *frame.current_class;
  mir::Block& ctor_block = *frame.current_block;
  const hir::StructuralScope& hir_scope = lowerer.HirScope();
  ModuleLowerer& module = lowerer.Module();
  const auto& builtins = module.Unit().builtins;
  const mir::TypeId scope_ptr_type = builtins.scope_ptr;
  const auto string_literal = [&](const std::string& s) -> mir::ExprId {
    return ctor_block.exprs.Add(
        mir::Expr{
            .data = mir::StringLiteral{.value = s}, .type = builtins.string});
  };
  for (std::size_t ci = 0; ci < hir_scope.cross_unit_refs.size(); ++ci) {
    const hir::CrossUnitRefId hir_id{static_cast<std::uint32_t>(ci)};
    const auto& cu = hir_scope.cross_unit_refs.Get(hir_id);
    const mir::MemberId member = lowerer.CrossUnitRefTarget(hir_id).target.var;
    if (std::holds_alternative<hir::UpwardRootHead>(cu.head)) {
      const mir::ExprId origin_self = ctor_block.exprs.Add(
          MakeSelfRefExpr(frame, mir_class.self_pointer_type));
      AppendExternUpBinding(
          module, frame, member, cu.path, support::BuiltinFn::kBindRoot,
          {origin_self});
      continue;
    }
    if (const auto* up_named = std::get_if<hir::UpwardNamedHead>(&cu.head)) {
      const mir::ExprId origin_self = ctor_block.exprs.Add(
          MakeSelfRefExpr(frame, mir_class.self_pointer_type));
      const mir::ExprId head_name = string_literal(up_named->head_name);
      const mir::ExprId head_indices =
          BuildIndicesArrayExpr(module, ctor_block, up_named->head_indices);
      AppendExternUpBinding(
          module, frame, member, cu.path, support::BuiltinFn::kBindVisibleChild,
          {origin_self, head_name, head_indices});
      continue;
    }
    const auto* down = std::get_if<hir::DownwardHead>(&cu.head);
    if (down == nullptr) {
      continue;
    }
    const mir::MemberId slot = member;
    const mir::TypeId slot_type = mir_class.members.Get(slot).type;
    mir::ExprId nav{};
    if (down->hops.value != 0) {
      nav = BuildTypedEnclosingNavValue(
          lowerer, frame, *down, cu.path, slot_type);
    } else {
      const mir::MemberId head_var =
          lowerer.TranslateOwnedChild(hir::StructuralHops{0}, down->child);
      const auto& head = mir_class.members.Get(head_var);
      const std::string head_name =
          head.source_name.empty() ? head.name : head.source_name;
      nav = ctor_block.exprs.Add(BuildDownwardNavValue(
          module, frame, head_name, cu.path, slot_type, scope_ptr_type));
    }
    const mir::ExprId self_for_target = ctor_block.exprs.Add(
        MakeSelfRefExpr(frame, mir_class.self_pointer_type));
    const mir::ExprId target = ctor_block.exprs.Add(
        mir::Expr{
            .data =
                mir::MemberAccessExpr{
                    .receiver = self_for_target,
                    .member = mir::MemberRef{.var = slot}},
            .type = slot_type});
    const mir::ExprId assign = ctor_block.exprs.Add(
        mir::Expr{
            .data = mir::AssignExpr{.target = target, .value = nav},
            .type = slot_type});
    ctor_block.AppendStmt(mir::ExprStmt{.expr = assign});
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

// Realizes each port connection (LRM 23.3.3). An input or output port is the
// implied continuous assignment between the two cells, materialized as the same
// synthesized process a scope-level `assign` produces, registered as a process.
// A `ref` port instead binds the child's reference member -- navigated by name
// from the owned child -- to the connected variable's cell, emitted into the
// resolve block: one assignment of a reference, with no second cell and no
// continuous assignment.
auto InstallPortConnections(
    StructuralScopeLowerer& lowerer, WalkFrame frame, WalkFrame resolve_frame,
    WalkFrame activate_frame) -> diag::Result<void> {
  mir::Class& mir_class = *frame.current_class;
  mir::Block& resolve_block = *resolve_frame.current_block;
  const hir::StructuralScope& hir_scope = lowerer.HirScope();
  ModuleLowerer& module = lowerer.Module();
  const mir::TypeId scope_ptr_type = module.Unit().builtins.scope_ptr;
  for (const auto& pc : hir_scope.port_connections) {
    if (pc.direction == hir::PortDirection::kRef) {
      // Bind the child's reference member to the connected variable's cell in
      // the resolve phase: navigate the owned child by name to its reference
      // member and store a reference to the peer's cell into it.
      const auto& alias = std::get<hir::PortAliasEndpoint>(pc.endpoint);
      const auto* down = std::get_if<hir::DownwardHead>(&alias.head);
      if (down == nullptr) {
        throw InternalError(
            "InstallPortConnections: a ref port reaches its child downward");
      }
      const mir::MemberId head_var =
          lowerer.TranslateOwnedChild(hir::StructuralHops{0}, down->child);
      const auto& head_member = mir_class.members.Get(head_var);
      const std::string head_name = head_member.source_name.empty()
                                        ? head_member.name
                                        : head_member.source_name;

      const mir::TypeId value_type = module.TranslateType(alias.type);
      const mir::TypeId ref_type = module.Unit().types.Intern(
          mir::RefType{
              .pointee = value_type, .mutability = mir::Mutability::kMutable});
      const mir::TypeId slot_type = module.Unit().types.PointerTo(
          ref_type, mir::PointerOwnership::kBorrowed);
      const mir::ExprId nav = resolve_block.exprs.Add(BuildDownwardNavValue(
          module, resolve_frame, head_name, alias.path, slot_type,
          scope_ptr_type));
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
    const hir::ContinuousAssign assign{
        .span = pc.span,
        .lhs = is_input ? cell.cell : pc.peer,
        .rhs = is_input ? pc.peer : cell.cell,
        .sensitivity_list = pc.sensitivity};
    std::string name = std::format("process_{}", mir_class.methods.size());
    auto decl_or =
        LowerContinuousAssign(lowerer, frame, std::move(name), assign);
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

// Installs a driver for a net-targeted continuous assignment: a driver-handle
// member bound at Resolve (`self->driver = (self->net).AttachDriver()`) and
// seeded at Initialize (`self->driver.Update(services, rhs)`). Returns the
// driver handle the activation process updates whenever the right-hand side
// changes.
auto AttachNetDriver(
    const StructuralScopeLowerer& lowerer, mir::Class& mir_class,
    mir::MemberId net_member, const hir::ContinuousAssign& ca,
    const WalkFrame& resolve_frame, const WalkFrame& init_frame,
    mir::TypeId self_ptr_type) -> diag::Result<NetDriver> {
  ModuleLowerer& module = lowerer.Module();
  const hir::StructuralScope& hir_scope = lowerer.HirScope();
  mir::Block& resolve_block = *resolve_frame.current_block;
  mir::Block& init_block = *init_frame.current_block;

  const mir::TypeId net_type = mir_class.members.Get(net_member).type;
  const mir::TypeId value_type =
      std::get<mir::ResolvedType>(module.Unit().types.Get(net_type).data).value;
  const mir::TypeId driver_type =
      module.Unit().types.Intern(mir::DriverType{.value = value_type});
  const mir::MemberId driver_member = mir_class.members.Add(
      mir::MemberDecl{
          .name =
              std::format("{}__driver", mir_class.members.Get(net_member).name),
          .type = driver_type});

  const auto resolve_self = [&] {
    return resolve_block.exprs.Add(
        MakeSelfRefExpr(resolve_frame, self_ptr_type));
  };
  const mir::ExprId net_access = resolve_block.exprs.Add(
      mir::MakeMemberAccessExpr(
          resolve_self(), mir::MemberRef{.var = net_member}, net_type));
  const mir::ExprId attach = resolve_block.exprs.Add(
      mir::MakeNetAttachDriverCallExpr(net_access, driver_type));
  const mir::ExprId driver_lhs = resolve_block.exprs.Add(
      mir::MakeMemberAccessExpr(
          resolve_self(), mir::MemberRef{.var = driver_member}, driver_type));
  const mir::ExprId attach_assign = resolve_block.exprs.Add(
      mir::Expr{
          .data = mir::AssignExpr{.target = driver_lhs, .value = attach},
          .type = driver_type});
  resolve_block.AppendStmt(
      mir::Stmt{
          .label = std::nullopt, .data = mir::ExprStmt{.expr = attach_assign}});

  auto rhs_or = lowerer.LowerExpr(hir_scope.exprs.Get(ca.rhs), init_frame);
  if (!rhs_or) return std::unexpected(std::move(rhs_or.error()));
  const mir::ExprId seed_rhs = init_block.exprs.Add(*std::move(rhs_or));
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
          seed_driver, seed_services, seed_rhs,
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
  const std::string& runtime_label =
      var.source_name.empty() ? var.name : var.source_name;
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
  // from that single source. A scalar member yields an empty index list;
  // a vector member yields a one-entry list with the iteration's induction
  // value; a multi-dim member would extend it per axis.
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

// The induction var is a body-local of the constructor; the for-stmt body is a
// child block of that same callable, so a read names the var directly.
auto MakeForBodyInductionVarArg(mir::LocalId induction_var_id, mir::TypeId type)
    -> mir::Expr {
  return mir::MakeLocalRefExpr(induction_var_id, type);
}

// Builds the body block of a single generate arm. Each arm constructs one
// owned-child instance (scalar for `if` / `case` generate, one element of the
// member-vector per iteration for `for` generate) and records it on the parent
// for by-name lookup. The arm body is a child block of the constructor
// callable; its `self` read resolves through the callable's receiver binding,
// independent of how deep the control-flow scaffold nests it. The optional
// `array_index_arg` populates the runtime index slot for a vector member; for
// a scalar member it is `nullopt` (the registered index list is empty).
auto BuildGenerateArmBody(
    ModuleLowerer& module, WalkFrame frame,
    const GenerateBindings& gen_bindings, hir::StructuralScopeId arm_scope_id,
    std::vector<mir::Expr> args, std::optional<mir::Expr> array_index_arg)
    -> mir::Block {
  const auto& binding = gen_bindings.by_scope_id.at(arm_scope_id.value);

  mir::Block arm_block;
  const WalkFrame arm_frame = frame.WithBlock(&arm_block);

  std::vector<mir::ExprId> arg_ids;
  arg_ids.reserve(args.size());
  for (auto& arg : args) {
    arg_ids.push_back(arm_block.exprs.Add(std::move(arg)));
  }
  std::optional<mir::ExprId> array_index_id;
  if (array_index_arg.has_value()) {
    array_index_id = arm_block.exprs.Add(std::move(*array_index_arg));
  }

  AppendOwnedChildConstruction(
      module, arm_frame, binding.var_id, binding.scope_id, std::move(arg_ids),
      array_index_id);
  return arm_block;
}

auto LowerIfGenerate(
    StructuralScopeLowerer& lowerer, WalkFrame frame,
    const GenerateBindings& gen_bindings, const hir::IfGenerate& if_gen)
    -> diag::Result<mir::Stmt> {
  const hir::StructuralScope& enclosing_scope = lowerer.HirScope();
  mir::Block& block = *frame.current_block;

  auto cond_or =
      lowerer.LowerExpr(enclosing_scope.exprs.Get(if_gen.condition), frame);
  if (!cond_or) return std::unexpected(std::move(cond_or.error()));
  const mir::ExprId cond_id = block.exprs.Add(*std::move(cond_or));

  const mir::BlockId then_id = block.child_scopes.Add(BuildGenerateArmBody(
      lowerer.Module(), frame, gen_bindings, if_gen.then_scope, {},
      std::nullopt));

  std::optional<mir::BlockId> else_id;
  if (if_gen.else_scope.has_value()) {
    else_id = block.child_scopes.Add(BuildGenerateArmBody(
        lowerer.Module(), frame, gen_bindings, *if_gen.else_scope, {},
        std::nullopt));
  }

  return mir::Stmt{
      .label = std::nullopt,
      .data = mir::IfStmt{
          .condition = cond_id, .then_scope = then_id, .else_scope = else_id}};
}

auto LowerCaseGenerate(
    StructuralScopeLowerer& lowerer, WalkFrame frame,
    const GenerateBindings& gen_bindings, const hir::CaseGenerate& case_gen)
    -> diag::Result<mir::Stmt> {
  const hir::StructuralScope& enclosing_scope = lowerer.HirScope();
  const mir::TypeId bit_type = lowerer.Module().Unit().builtins.bit1;

  mir::Block wrapper_block;
  const WalkFrame wrapper_frame = frame.WithBlock(&wrapper_block);

  auto cond_or = lowerer.LowerExpr(
      enclosing_scope.exprs.Get(case_gen.condition), wrapper_frame);
  if (!cond_or) return std::unexpected(std::move(cond_or.error()));
  const mir::ExprId cond_expr_id = wrapper_block.exprs.Add(*std::move(cond_or));

  const CaseSnapshotRefs snapshot =
      AppendCaseSnapshot(lowerer.Module(), wrapper_frame, cond_expr_id);

  std::vector<mir::Block> body_scopes;
  body_scopes.reserve(case_gen.items.size());
  for (std::size_t i = 0; i < case_gen.items.size(); ++i) {
    body_scopes.push_back(BuildGenerateArmBody(
        lowerer.Module(), frame, gen_bindings, case_gen.items[i].scope, {},
        std::nullopt));
  }

  std::optional<mir::Block> default_scope;
  if (case_gen.default_scope.has_value()) {
    default_scope = BuildGenerateArmBody(
        lowerer.Module(), frame, gen_bindings, *case_gen.default_scope, {},
        std::nullopt);
  }

  auto build_predicate =
      [&](WalkFrame enc_frame,
          std::size_t item_idx) -> diag::Result<mir::ExprId> {
    return BuildEqualityChain(
        enc_frame, snapshot, bit_type, mir::BinaryOp::kEquality,
        case_gen.items[item_idx].labels.size(),
        [&](WalkFrame label_frame,
            std::size_t li) -> diag::Result<mir::ExprId> {
          auto lab_or = lowerer.LowerExpr(
              enclosing_scope.exprs.Get(case_gen.items[item_idx].labels[li]),
              label_frame);
          if (!lab_or) {
            return std::unexpected(std::move(lab_or.error()));
          }
          return label_frame.current_block->exprs.Add(*std::move(lab_or));
        },
        lowerer.Module().Unit());
  };

  return BuildCaseCascade(
      frame, std::move(wrapper_block), std::nullopt, case_gen.items.size(),
      std::move(body_scopes), std::move(default_scope), build_predicate);
}

auto LowerLoopGenerate(
    StructuralScopeLowerer& lowerer, WalkFrame frame,
    const GenerateBindings& gen_bindings, const hir::LoopGenerate& loop)
    -> diag::Result<mir::Stmt> {
  const hir::StructuralScope& enclosing_scope = lowerer.HirScope();
  mir::Block& block = *frame.current_block;

  const auto& var_decl = enclosing_scope.loop_var_decls.Get(loop.loop_var);
  const mir::TypeId genvar_type = lowerer.Module().TranslateType(var_decl.type);

  const mir::LocalId loop_local_id = frame.bindings->DeclareAnonymous(
      mir::LocalDecl{.name = var_decl.name, .type = genvar_type});
  const mir::LocalRef loop_local{.var = loop_local_id};

  lowerer.MapLoopVarAsProcedural(loop.loop_var, loop_local);

  auto init_or =
      lowerer.LowerExpr(enclosing_scope.exprs.Get(loop.initial), frame);
  if (!init_or) return std::unexpected(std::move(init_or.error()));
  const mir::ExprId init_id = block.exprs.Add(*std::move(init_or));

  auto cond_or = lowerer.LowerExpr(enclosing_scope.exprs.Get(loop.stop), frame);
  if (!cond_or) return std::unexpected(std::move(cond_or.error()));
  const mir::ExprId cond_id = block.exprs.Add(*std::move(cond_or));

  // HIR carries the iter as the next-value expression for the loop variable;
  // the loop semantic (this lowering) owns the actual write back.
  auto step_value_or =
      lowerer.LowerExpr(enclosing_scope.exprs.Get(loop.iter), frame);
  if (!step_value_or) {
    return std::unexpected(std::move(step_value_or.error()));
  }
  const mir::TypeId step_type = (*step_value_or).type;
  const mir::ExprId step_value_id = block.exprs.Add(*std::move(step_value_or));
  const mir::ExprId step_target_id =
      block.exprs.Add(mir::Expr{.data = loop_local, .type = genvar_type});
  const mir::ExprId step_id = block.exprs.Add(
      mir::Expr{
          .data =
              mir::AssignExpr{.target = step_target_id, .value = step_value_id},
          .type = step_type});

  // The induction variable feeds two slots: as a structural-param ctor arg
  // (so each generated child binds its `genvar` per LRM 27.4) and as the
  // index entry the child's `HierarchySegment` carries (so a hierarchical
  // lookup `c[i]` reaches the same slot). Build two fresh reads so each
  // consumer reaches the loop local through its own MIR node.
  std::vector<mir::Expr> body_args;
  body_args.push_back(MakeForBodyInductionVarArg(loop_local_id, genvar_type));
  mir::Expr array_index_arg =
      MakeForBodyInductionVarArg(loop_local_id, genvar_type);

  const mir::BlockId loop_scope_id =
      block.child_scopes.Add(BuildGenerateArmBody(
          lowerer.Module(), frame, gen_bindings, loop.scope,
          std::move(body_args), std::move(array_index_arg)));

  return mir::Stmt{
      .label = std::nullopt,
      .data = mir::ForStmt{
          .init = {mir::ForInitDecl{
              .induction_var = loop_local_id, .init = init_id}},
          .condition = cond_id,
          .step = {step_id},
          .scope = loop_scope_id}};
}

auto LowerGenerateAsStmt(
    StructuralScopeLowerer& lowerer, WalkFrame frame, const hir::Generate& gen,
    const GenerateBindings& gen_bindings) -> diag::Result<mir::Stmt> {
  return std::visit(
      Overloaded{
          [&](const hir::IfGenerate& if_gen) {
            return LowerIfGenerate(lowerer, frame, gen_bindings, if_gen);
          },
          [&](const hir::CaseGenerate& case_gen) {
            return LowerCaseGenerate(lowerer, frame, gen_bindings, case_gen);
          },
          [&](const hir::LoopGenerate& loop) {
            return LowerLoopGenerate(lowerer, frame, gen_bindings, loop);
          },
      },
      gen.data);
}

}  // namespace

auto StructuralScopeLowerer::DeclareShape(
    std::span<const ScopeEntryStructuralParamBinding> entry_bindings)
    -> diag::Result<mir::ClassId> {
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

  for (const auto& binding : entry_bindings) {
    const mir::ParamId mir_id = shape.params.Add(binding.param);
    lowerer.MapLoopVarAsStructuralParam(binding.source_loop_var, mir_id);
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

    auto specs = EnumerateGenerateChildSpecs(gen, hir_scope, module);
    for (auto& spec : specs) {
      const auto companion_name = CompanionVarNameFor(spec.scope_name);
      CheckNoNameCollision(shape, spec.scope_name, companion_name);

      auto child = std::make_unique<StructuralScopeLowerer>(
          module, &lowerer, std::move(spec.scope_name), *spec.scope);
      auto child_r = child->DeclareShape(spec.entry_bindings);
      if (!child_r) return std::unexpected(std::move(child_r.error()));

      const mir::ClassId child_id = *child_r;
      shape.contained.push_back(child_id);
      mir::TypeId var_type = MakeUniqueObjectPointer(module, child_id);
      if (spec.is_repeated) {
        var_type =
            module.Unit().types.Intern(mir::VectorType{.element = var_type});
      }
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
    // Owned children (pointer / vector / object), resolution slots, upward
    // refs, and named events have no "value assignment" -- their declaration
    // shape itself fixes the field at construction. A net takes none either:
    // its value is produced by its drivers, seeded when each driver updates in
    // the initialize phase. Value-typed variables (integral, string, real,
    // unpacked / dynamic array) receive an LRM 10.5 initialization statement,
    // run in the initialize phase after the tree's references resolve, not in
    // the constructor.
    const bool is_assignable_value =
        !is_reference && !is_net && var_kind != mir::TypeKind::kPointer &&
        var_kind != mir::TypeKind::kVector &&
        var_kind != mir::TypeKind::kExternalRef &&
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

    // A value signal, or a named event, records its address under its name so a
    // cross-unit referrer resolves it by name at construction. The excluded
    // members -- owned children and cross-unit reference slots -- are not
    // signals.
    const bool is_signal = var_kind != mir::TypeKind::kPointer &&
                           var_kind != mir::TypeKind::kVector &&
                           var_kind != mir::TypeKind::kExternalRef &&
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

  for (std::size_t i = 0; i < hir_scope.structural_subroutines.size(); ++i) {
    const auto& src = hir_scope.structural_subroutines.Get(
        hir::StructuralSubroutineId{static_cast<std::uint32_t>(i)});
    ProcessLowerer subroutine_lowerer(
        module, &lowerer, hir_scope.time_resolution, src.body, src.name,
        mir::MethodVisibility::kInternal, ctor_frame);
    auto decl_or = subroutine_lowerer.Run(src);
    if (!decl_or) return std::unexpected(std::move(decl_or.error()));
    const mir::MethodId added = mir_class.methods.Add(*std::move(decl_or));
    if (added.value != i) {
      throw InternalError(
          "StructuralScopeLowerer::PopulateBodies: subroutine added out of "
          "mapped id order");
    }
  }

  for (const auto& p : hir_scope.processes) {
    std::string name = std::format("process_{}", mir_class.methods.size());
    ProcessLowerer process_lowerer(
        module, &lowerer, hir_scope.time_resolution, p.body, std::move(name),
        mir::MethodVisibility::kInternal, ctor_frame);
    auto decl_or = process_lowerer.Run(p);
    if (!decl_or) return std::unexpected(std::move(decl_or.error()));
    const mir::MethodId body = mir_class.methods.Add(*std::move(decl_or));
    AppendProcessRegistration(
        module, activate_frame, body, p.kind == hir::ProcessKind::kFinal);
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
      auto driver_or = AttachNetDriver(
          lowerer, mir_class, *net_member, ca, resolve_frame, init_frame,
          self_ptr_type);
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
  InstallCrossUnitRefs(lowerer, ctor_frame);
  auto port_conn_r = InstallPortConnections(
      lowerer, ctor_frame, resolve_frame, activate_frame);
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
