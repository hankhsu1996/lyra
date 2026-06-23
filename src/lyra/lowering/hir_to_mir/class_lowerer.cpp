#include "lyra/lowering/hir_to_mir/class_lowerer.hpp"

#include <cstddef>
#include <cstdint>
#include <expected>
#include <format>
#include <optional>
#include <string>
#include <string_view>
#include <utility>
#include <variant>
#include <vector>

#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/diag/diag_code.hpp"
#include "lyra/diag/diagnostic.hpp"
#include "lyra/hir/expr.hpp"
#include "lyra/hir/structural_scope.hpp"
#include "lyra/lowering/hir_to_mir/case_cascade.hpp"
#include "lyra/lowering/hir_to_mir/continuous_assign.hpp"
#include "lyra/lowering/hir_to_mir/default_value.hpp"
#include "lyra/lowering/hir_to_mir/expression/aggregates.hpp"
#include "lyra/lowering/hir_to_mir/expression/operators.hpp"
#include "lyra/lowering/hir_to_mir/expression/references.hpp"
#include "lyra/lowering/hir_to_mir/expression/selects.hpp"
#include "lyra/lowering/hir_to_mir/lhs_observable.hpp"
#include "lyra/lowering/hir_to_mir/process_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/self_ref.hpp"
#include "lyra/lowering/hir_to_mir/walk_frame.hpp"
#include "lyra/mir/block_hops.hpp"
#include "lyra/mir/class.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/local.hpp"
#include "lyra/mir/member.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/mir/type.hpp"

namespace lyra::lowering::hir_to_mir {

namespace {

// Wrap a value type in `ObservableType` iff it is a SystemVerilog value-storage
// data type (LRM 6.5 / 7.x). Handle / wrapper types (pointer / vector / object
// / external ref / external unit object) and named events (LRM 15 -- carry
// their own subscribe mechanism) pass through unwrapped. See
// `docs/decisions/value-type-concepts.md`.
auto MaybeWrapObservable(ModuleLowerer& module, mir::TypeId t) -> mir::TypeId {
  const auto& data = module.Unit().GetType(t).data;
  const bool wrap = std::holds_alternative<mir::PackedArrayType>(data) ||
                    std::holds_alternative<mir::EnumType>(data) ||
                    std::holds_alternative<mir::StringType>(data) ||
                    std::holds_alternative<mir::RealType>(data) ||
                    std::holds_alternative<mir::ShortRealType>(data) ||
                    std::holds_alternative<mir::RealTimeType>(data) ||
                    std::holds_alternative<mir::UnpackedArrayType>(data) ||
                    std::holds_alternative<mir::DynamicArrayType>(data) ||
                    std::holds_alternative<mir::QueueType>(data) ||
                    std::holds_alternative<mir::AssociativeArrayType>(data);
  if (!wrap) {
    return t;
  }
  return module.Unit().AddType(mir::TypeData{mir::ObservableType{.value = t}});
}

auto ChildScopeNameFor(std::size_t gen_index, std::string_view arm_tag)
    -> std::string {
  return std::format("gen{}_{}", gen_index, arm_tag);
}

auto CompanionVarNameFor(std::string_view child_scope_name) -> std::string {
  return std::string(child_scope_name) + "_obj";
}

void CheckNoNameCollision(
    const mir::Class& owner_class, std::string_view child_scope_name,
    std::string_view companion_var_name) {
  for (const auto& v : owner_class.members) {
    if (v.name == companion_var_name || v.name == child_scope_name) {
      throw InternalError(
          "child class or companion var name collides with an existing "
          "member declaration in the enclosing class");
    }
  }
  for (const auto& c : owner_class.nested_classes) {
    if (c.name == child_scope_name) {
      throw InternalError(
          "child class name collides with an existing nested class "
          "declaration in the enclosing class");
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
    const hir::Generate& gen, std::size_t gen_index,
    const hir::StructuralScope& enclosing_scope, ModuleLowerer& module)
    -> std::vector<GenerateChildSpec> {
  std::vector<GenerateChildSpec> specs;
  std::visit(
      Overloaded{
          [&](const hir::IfGenerate& if_gen) {
            const auto& then_scope = gen.child_scopes.Get(if_gen.then_scope);
            specs.push_back(
                {.scope_id = if_gen.then_scope,
                 .scope = &then_scope,
                 .scope_name = ChildScopeNameFor(gen_index, "then"),
                 .is_repeated = false,
                 .entry_bindings = {}});
            if (if_gen.else_scope.has_value()) {
              const auto& else_scope = gen.child_scopes.Get(*if_gen.else_scope);
              specs.push_back(
                  {.scope_id = *if_gen.else_scope,
                   .scope = &else_scope,
                   .scope_name = ChildScopeNameFor(gen_index, "else"),
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
                       ChildScopeNameFor(gen_index, std::format("case{}", k)),
                   .is_repeated = false,
                   .entry_bindings = {}});
            }
            if (case_gen.default_scope.has_value()) {
              const auto& default_scope =
                  gen.child_scopes.Get(*case_gen.default_scope);
              specs.push_back(
                  {.scope_id = *case_gen.default_scope,
                   .scope = &default_scope,
                   .scope_name = ChildScopeNameFor(gen_index, "default"),
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
                 .scope_name = ChildScopeNameFor(gen_index, "loop"),
                 .is_repeated = true,
                 .entry_bindings = std::move(bindings)});
          },
      },
      gen.data);
  return specs;
}

auto MakeUniqueObjectPointer(ModuleLowerer& module, std::string scope_name)
    -> mir::TypeId {
  const mir::TypeId object_type =
      module.Unit().AddType(mir::ObjectType{.name = std::move(scope_name)});
  return module.Unit().AddType(
      mir::PointerType{
          .pointee = object_type, .ownership = mir::PointerOwnership::kUnique});
}

auto MakeUniqueExternalUnitPointer(ModuleLowerer& module, std::string unit_name)
    -> mir::TypeId {
  const mir::TypeId object_type = module.Unit().AddType(
      mir::ExternalUnitObjectType{.unit_name = std::move(unit_name)});
  return module.Unit().AddType(
      mir::PointerType{
          .pointee = object_type, .ownership = mir::PointerOwnership::kUnique});
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
    type = module.Unit().AddType(mir::VectorType{.element = type});
  }
  return type;
}

// Returns the StructuralVarId of each instance member, indexed by
// InstanceMemberId, so cross-unit reference resolution can reach the child
// instance var by the same id the HIR recipe carries.
auto InstallInstanceMembers(ClassLowerer& lowerer, WalkFrame frame)
    -> std::vector<mir::MemberId> {
  mir::Class& mir_class = *frame.current_class;
  mir::Block& ctor_block = *frame.current_block;
  const hir::StructuralScope& hir_scope = lowerer.HirScope();
  std::vector<mir::MemberId> instance_member_vars;
  instance_member_vars.reserve(hir_scope.instance_members.size());
  for (const auto& im : hir_scope.instance_members) {
    for (const auto& v : mir_class.members) {
      if (v.name == im.instance_name) {
        throw InternalError(
            "instance member name collides with an existing member "
            "declaration in the enclosing class");
      }
    }
    const mir::TypeId var_type = MakeExternalUnitMemberType(
        lowerer.Module(), im.target_unit, im.array_dims.size());
    const mir::MemberId var_id = mir_class.members.Add(
        mir::MemberDecl{.name = im.instance_name, .type = var_type});
    instance_member_vars.push_back(var_id);

    ctor_block.AppendStmt(
        mir::Stmt{
            .label = std::nullopt,
            .data = mir::ConstructExternalUnitStmt{
                .target = var_id,
                .unit_name = im.target_unit,
                .dims = im.array_dims}});
  }
  return instance_member_vars;
}

// An upward cross-unit reference materializes as an ExternalRef member: the
// symbol -- ancestor name, by-name tail through its owned children, and leaf
// signal -- lives on its type, and the runtime ExternUp member self-relocates
// at Bind by climbing the parent chain then walking the tail
// (docs/architecture/emission_model.md). A downward reference gets a borrowed-
// pointer slot member, null until the constructor resolves it. Both
// run before processes so reads resolve to the slot; both record their MIR read
// target as a StructuralVarRef to that member, in HIR slot order. The returned
// vector carries the downward slot var per ref (nullopt for upward), consumed
// by InstallCrossUnitRefs once the children exist.
auto MaterializeCrossUnitRefTargets(ClassLowerer& lowerer, WalkFrame frame)
    -> std::vector<std::optional<mir::MemberId>> {
  ModuleLowerer& module = lowerer.Module();
  mir::Class& mir_class = *frame.current_class;
  const hir::StructuralScope& hir_scope = lowerer.HirScope();
  std::vector<std::optional<mir::MemberId>> slot_vars;
  std::uint32_t slot_index = 0;
  for (const auto& cu : hir_scope.cross_unit_refs) {
    std::string member_name = "xref" + std::to_string(slot_index++);
    if (const auto* up = std::get_if<hir::UpwardHead>(&cu.head)) {
      // `cu.path` runs from the ancestor down to the leaf, shared with the
      // downward direction. Fold it into by-name child hops (each member opens
      // a hop, following array indices attach to it) and the final leaf signal.
      std::vector<mir::ChildStep> tail;
      for (const auto& step : cu.path) {
        if (const auto* member = std::get_if<hir::MemberHop>(&step)) {
          tail.push_back(mir::ChildStep{.name = member->name, .indices = {}});
        } else {
          tail.back().indices.push_back(std::get<hir::IndexHop>(step).index);
        }
      }
      std::string signal = std::move(tail.back().name);
      tail.pop_back();

      const mir::ExternalRefMatch match =
          up->match == hir::UpwardMatch::kDefName
              ? mir::ExternalRefMatch::kDefName
              : mir::ExternalRefMatch::kScopeName;
      const mir::TypeId leaf = module.TranslateType(cu.type);
      const mir::TypeId ext_type = module.Unit().AddType(
          mir::ExternalRefType{
              .element = leaf,
              .ancestor = up->ancestor_name,
              .match = match,
              .tail = std::move(tail),
              .signal = std::move(signal)});
      const mir::MemberId var = mir_class.members.Add(
          mir::MemberDecl{.name = std::move(member_name), .type = ext_type});
      lowerer.AddCrossUnitRefTarget(
          mir::MemberRef{.hops = {.value = 0}, .var = var}, ext_type);
      slot_vars.emplace_back(std::nullopt);
    } else {
      // The pointee matches the producer's storage cell. A producer-side
      // value-storage signal is wrapped in `ObservableType` at its declaration
      // (so a write fires subscribers), and the cross-unit pointer must point
      // at the same wrapped cell -- otherwise the C++ types mismatch.
      const mir::TypeId leaf =
          MaybeWrapObservable(module, module.TranslateType(cu.type));
      const mir::TypeId slot_type = module.Unit().AddType(
          mir::PointerType{
              .pointee = leaf, .ownership = mir::PointerOwnership::kBorrowed});
      const mir::MemberId slot = mir_class.members.Add(
          mir::MemberDecl{.name = std::move(member_name), .type = slot_type});
      lowerer.AddCrossUnitRefTarget(
          mir::MemberRef{.hops = {.value = 0}, .var = slot}, slot_type);
      slot_vars.emplace_back(slot);
    }
  }
  return slot_vars;
}

// Builds the resolve value for a downward reference: a chain of generic
// navigation calls from the enclosing scope. The owned-child head and each
// crossed member open a `GetChild(name, indices)`; the leaf signal is a
// `GetSignal(name)` whose result type is the slot's borrowed-pointer cell type,
// so render casts the untyped storage pointer mechanically. Per-dimension array
// indices are ordinary integer-literal arguments, never bundled with the name.
auto BuildDownwardNavValue(
    const ModuleLowerer& module, WalkFrame frame, const std::string& head_name,
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

  mir::ExprId cur =
      ctor_block.exprs.Add(BuildSelfRefExpr(frame, self_ptr_type));
  for (std::size_t i = 0; i + 1 < hops.size(); ++i) {
    std::vector<mir::ExprId> args;
    args.push_back(cur);
    for (const mir::ExprId idx : hops[i].indices) {
      args.push_back(idx);
    }
    cur = ctor_block.exprs.Add(
        mir::Expr{
            .data =
                mir::CallExpr{
                    .callee =
                        mir::RuntimeNavCallee{
                            .fn = mir::RuntimeFn::kGetChild,
                            .name = hops[i].name},
                    .arguments = std::move(args)},
            .type = scope_ptr_type});
  }
  return mir::Expr{
      .data =
          mir::CallExpr{
              .callee =
                  mir::RuntimeNavCallee{
                      .fn = mir::RuntimeFn::kGetSignal,
                      .name = hops.back().name},
              .arguments = {cur}},
      .type = slot_type};
}

// A downward slot resolves in the constructor by navigating from the enclosing
// scope after the children are built (reference_resolution.md): an ordinary
// assignment of the navigation value into the borrowed-pointer slot. Upward
// slots are materialized as ExternalRef members upstream and skipped here.
void InstallCrossUnitRefs(
    ClassLowerer& lowerer, WalkFrame frame,
    const std::vector<mir::MemberId>& instance_member_vars,
    const std::vector<GenerateBindings>& gen_bindings,
    const std::vector<std::optional<mir::MemberId>>& slot_vars) {
  mir::Class& mir_class = *frame.current_class;
  mir::Block& ctor_block = *frame.current_block;
  const hir::StructuralScope& hir_scope = lowerer.HirScope();
  ModuleLowerer& module = lowerer.Module();
  const mir::TypeId scope_ptr_type = module.Unit().AddType(
      mir::PointerType{
          .pointee = module.Unit().AddType(mir::ScopeType{}),
          .ownership = mir::PointerOwnership::kBorrowed});
  for (std::size_t ci = 0; ci < hir_scope.cross_unit_refs.size(); ++ci) {
    const auto& cu = hir_scope.cross_unit_refs.Get(
        hir::CrossUnitRefId{static_cast<std::uint32_t>(ci)});
    const auto* down = std::get_if<hir::DownwardHead>(&cu.head);
    if (down == nullptr) {
      continue;
    }
    const mir::MemberId slot = *slot_vars.at(ci);
    mir::MemberId head_var{};
    if (const auto* im = std::get_if<hir::InstanceMemberId>(&down->child)) {
      head_var = instance_member_vars.at(im->value);
    } else {
      const auto& g = std::get<hir::GenerateChildRef>(down->child);
      head_var = gen_bindings.at(g.generate.value)
                     .by_scope_id.at(g.scope.value)
                     .var_id;
    }
    const auto& head = mir_class.members.Get(head_var);
    const std::string head_name =
        head.source_name.empty() ? head.name : head.source_name;
    const mir::TypeId slot_type = mir_class.members.Get(slot).type;
    const mir::ExprId nav = ctor_block.exprs.Add(BuildDownwardNavValue(
        module, frame, head_name, cu.path, slot_type, scope_ptr_type));
    const mir::ExprId self_for_target = ctor_block.exprs.Add(
        BuildSelfRefExpr(frame, mir_class.self_pointer_type));
    const mir::ExprId target = ctor_block.exprs.Add(
        mir::Expr{
            .data =
                mir::MemberAccessExpr{
                    .receiver = self_for_target,
                    .member =
                        mir::MemberRef{.hops = {.value = 0}, .var = slot}},
            .type = slot_type});
    const mir::ExprId assign = ctor_block.exprs.Add(
        mir::Expr{
            .data = mir::AssignExpr{.target = target, .value = nav},
            .type = slot_type});
    ctor_block.AppendStmt(
        mir::Stmt{
            .label = std::nullopt, .data = mir::ExprStmt{.expr = assign}});
  }
}

void ValidateConstructOwnedObjectStmt(
    const mir::CompilationUnit& unit, const mir::Class& owner_class,
    const mir::Block& block, const mir::ConstructOwnedObjectStmt& stmt) {
  if (stmt.scope_id.value >= owner_class.nested_classes.size()) {
    throw InternalError(
        "ConstructOwnedObjectStmt: scope_id is not a direct child of the "
        "enclosing class");
  }
  if (stmt.target.value >= owner_class.members.size()) {
    throw InternalError(
        "ConstructOwnedObjectStmt: target is out of range in the enclosing "
        "class");
  }
  const auto& var = owner_class.members.Get(stmt.target);
  const auto child = mir::GetChildScope(unit, var.type);
  const auto* generate =
      child ? std::get_if<mir::GenerateScopeChild>(&*child) : nullptr;
  const auto& child_scope = owner_class.nested_classes.Get(stmt.scope_id);
  if (generate == nullptr || generate->name != child_scope.name) {
    throw InternalError(
        "ConstructOwnedObjectStmt: target var does not own the requested "
        "class");
  }
  if (stmt.args.size() != child_scope.params.size()) {
    throw InternalError(
        "ConstructOwnedObjectStmt: args count does not match child class "
        "param count");
  }
  for (std::size_t i = 0; i < stmt.args.size(); ++i) {
    const auto& arg = block.exprs.Get(stmt.args[i]);
    const auto& param =
        child_scope.params.Get(mir::ParamId{static_cast<std::uint32_t>(i)});
    if (arg.type != param.type) {
      throw InternalError(
          "ConstructOwnedObjectStmt: arg type does not match structural "
          "param type");
    }
  }
}

// The for-stmt body block is one level below the parent constructor block where
// the induction var was declared, so a `LocalRef` to that var read from inside
// the body hops up once.
auto MakeForBodyInductionVarArg(mir::LocalId induction_var_id, mir::TypeId type)
    -> mir::Expr {
  return mir::Expr{
      .data =
          mir::LocalRef{
              .hops = mir::BlockHops{.value = 1}, .var = induction_var_id},
      .type = type};
}

auto BuildGenerateArmBody(
    const ModuleLowerer& module, WalkFrame frame,
    const GenerateBindings& gen_bindings, hir::StructuralScopeId arm_scope_id,
    std::vector<mir::Expr> args) -> mir::Block {
  const auto& binding = gen_bindings.by_scope_id.at(arm_scope_id.value);

  mir::Block arm_block;
  std::vector<mir::ExprId> arg_ids;
  arg_ids.reserve(args.size());
  for (auto& arg : args) {
    arg_ids.push_back(arm_block.exprs.Add(std::move(arg)));
  }

  const mir::ConstructOwnedObjectStmt construct_stmt{
      .target = binding.var_id,
      .scope_id = binding.scope_id,
      .args = std::move(arg_ids)};
  ValidateConstructOwnedObjectStmt(
      module.Unit(), *frame.current_class, arm_block, construct_stmt);

  arm_block.AppendStmt(
      mir::Stmt{.label = std::nullopt, .data = construct_stmt});
  return arm_block;
}

auto LowerIfGenerate(
    ClassLowerer& lowerer, WalkFrame frame,
    const GenerateBindings& gen_bindings, const hir::IfGenerate& if_gen)
    -> diag::Result<mir::Stmt> {
  const hir::StructuralScope& enclosing_scope = lowerer.HirScope();
  mir::Block& block = *frame.current_block;

  auto cond_or =
      lowerer.LowerExpr(enclosing_scope.exprs.Get(if_gen.condition), frame);
  if (!cond_or) return std::unexpected(std::move(cond_or.error()));
  const mir::ExprId cond_id = block.exprs.Add(*std::move(cond_or));

  const mir::BlockId then_id = block.child_scopes.Add(BuildGenerateArmBody(
      lowerer.Module(), frame, gen_bindings, if_gen.then_scope, {}));

  std::optional<mir::BlockId> else_id;
  if (if_gen.else_scope.has_value()) {
    else_id = block.child_scopes.Add(BuildGenerateArmBody(
        lowerer.Module(), frame, gen_bindings, *if_gen.else_scope, {}));
  }

  return mir::Stmt{
      .label = std::nullopt,
      .data = mir::IfStmt{
          .condition = cond_id, .then_scope = then_id, .else_scope = else_id}};
}

auto LowerCaseGenerate(
    ClassLowerer& lowerer, WalkFrame frame,
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
  for (const auto& item : case_gen.items) {
    body_scopes.push_back(BuildGenerateArmBody(
        lowerer.Module(), frame, gen_bindings, item.scope, {}));
  }

  std::optional<mir::Block> default_scope;
  if (case_gen.default_scope.has_value()) {
    default_scope = BuildGenerateArmBody(
        lowerer.Module(), frame, gen_bindings, *case_gen.default_scope, {});
  }

  auto build_predicate =
      [&](WalkFrame enc_frame, std::size_t item_idx,
          std::uint32_t sel_hops) -> diag::Result<mir::ExprId> {
    return BuildEqualityChain(
        enc_frame, snapshot, bit_type, mir::BinaryOp::kEquality, sel_hops,
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
        });
  };

  return BuildCaseCascade(
      frame, std::move(wrapper_block), std::nullopt, case_gen.items.size(),
      std::move(body_scopes), std::move(default_scope), build_predicate);
}

auto LowerLoopGenerate(
    ClassLowerer& lowerer, WalkFrame frame,
    const GenerateBindings& gen_bindings, const hir::LoopGenerate& loop)
    -> diag::Result<mir::Stmt> {
  const hir::StructuralScope& enclosing_scope = lowerer.HirScope();
  mir::Block& block = *frame.current_block;

  const auto& var_decl = enclosing_scope.loop_var_decls.Get(loop.loop_var);
  const mir::TypeId genvar_type = lowerer.Module().TranslateType(var_decl.type);

  const mir::LocalId loop_local_id = block.vars.Add(
      mir::LocalDecl{.name = var_decl.name, .type = genvar_type});
  const mir::LocalRef loop_local{
      .hops = mir::BlockHops{.value = 0}, .var = loop_local_id};

  lowerer.MapLoopVarAsProcedural(loop.loop_var, loop_local);

  const WalkFrame proc_frame =
      frame.WithLoopVarMode(LoopVarLoweringMode::kProceduralInduction);
  auto init_or =
      lowerer.LowerExpr(enclosing_scope.exprs.Get(loop.initial), proc_frame);
  if (!init_or) return std::unexpected(std::move(init_or.error()));
  const mir::ExprId init_id = block.exprs.Add(*std::move(init_or));

  auto cond_or =
      lowerer.LowerExpr(enclosing_scope.exprs.Get(loop.stop), proc_frame);
  if (!cond_or) return std::unexpected(std::move(cond_or.error()));
  const mir::ExprId cond_id = block.exprs.Add(*std::move(cond_or));

  // HIR carries the iter as the next-value expression for the loop variable;
  // the loop semantic (this lowering) owns the actual write back.
  auto step_value_or =
      lowerer.LowerExpr(enclosing_scope.exprs.Get(loop.iter), proc_frame);
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

  std::vector<mir::Expr> body_args;
  body_args.push_back(MakeForBodyInductionVarArg(loop_local_id, genvar_type));

  const mir::BlockId loop_scope_id =
      block.child_scopes.Add(BuildGenerateArmBody(
          lowerer.Module(), frame, gen_bindings, loop.scope,
          std::move(body_args)));

  return mir::Stmt{
      .label = std::nullopt,
      .data = mir::ForStmt{
          .init = {mir::ForInitDecl{
              .induction_var = loop_local, .init = init_id}},
          .condition = cond_id,
          .step = {step_id},
          .scope = loop_scope_id}};
}

auto LowerGenerateAsStmt(
    ClassLowerer& lowerer, WalkFrame frame, const hir::Generate& gen,
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

auto InstallGenerateOwnedChildScopes(ClassLowerer& lowerer, WalkFrame frame)
    -> diag::Result<std::vector<GenerateBindings>> {
  ModuleLowerer& module = lowerer.Module();
  mir::Class& mir_class = *frame.current_class;
  const hir::StructuralScope& hir_scope = lowerer.HirScope();
  std::vector<GenerateBindings> bindings_by_generate;
  bindings_by_generate.reserve(hir_scope.generates.size());

  for (std::size_t gen_idx = 0; gen_idx < hir_scope.generates.size();
       ++gen_idx) {
    const auto& gen = hir_scope.generates.Get(
        hir::GenerateId{static_cast<std::uint32_t>(gen_idx)});
    GenerateBindings gen_bindings;
    gen_bindings.by_scope_id.resize(gen.child_scopes.size());

    auto specs = EnumerateGenerateChildSpecs(gen, gen_idx, hir_scope, module);
    for (auto& spec : specs) {
      const auto companion_name = CompanionVarNameFor(spec.scope_name);
      CheckNoNameCollision(mir_class, spec.scope_name, companion_name);

      ClassLowerer child_scope(
          module, &lowerer, std::move(spec.scope_name), *spec.scope);
      auto child_r = child_scope.Run(frame, spec.entry_bindings);
      if (!child_r) return std::unexpected(std::move(child_r.error()));

      const mir::ClassId child_id =
          mir_class.nested_classes.Add(*std::move(child_r));
      mir::TypeId var_type = MakeUniqueObjectPointer(
          module, mir_class.nested_classes.Get(child_id).name);
      if (spec.is_repeated) {
        var_type = module.Unit().AddType(mir::VectorType{.element = var_type});
      }
      const mir::MemberId var_id = mir_class.members.Add(
          mir::MemberDecl{
              .name = companion_name,
              .source_name = spec.scope->source_name,
              .type = var_type});

      gen_bindings.by_scope_id.at(spec.scope_id.value) =
          ChildStructuralScopeBinding{.scope_id = child_id, .var_id = var_id};
    }

    bindings_by_generate.push_back(std::move(gen_bindings));
  }
  return bindings_by_generate;
}

}  // namespace

auto ClassLowerer::Run(
    WalkFrame frame,
    std::span<const ScopeEntryStructuralParamBinding> entry_bindings)
    -> diag::Result<mir::Class> {
  ModuleLowerer& module = *module_;
  const hir::StructuralScope& hir_scope = *hir_scope_;
  ClassLowerer& lowerer = *this;

  const mir::TypeId self_object_type =
      module.Unit().AddType(mir::ObjectType{.name = name_});
  const mir::TypeId self_pointer_type = module.Unit().AddType(
      mir::PointerType{
          .pointee = self_object_type,
          .ownership = mir::PointerOwnership::kBorrowed});
  mir::Class mir_class{
      .name = name_,
      .self_pointer_type = self_pointer_type,
      .time_resolution = hir_scope.time_resolution,
      .params = {},
      .members = {},
      .constructor_block = {},
      .processes = {},
      .nested_classes = {},
      .methods = {},
      .type_aliases = {}};
  for (const auto& alias : hir_scope.type_aliases) {
    mir_class.type_aliases.push_back(
        mir::TypeAliasDecl{
            .name = alias.name, .target = module.TranslateType(alias.target)});
  }

  for (const auto& binding : entry_bindings) {
    const mir::ParamId mir_id = mir_class.params.Add(binding.param);
    lowerer.MapLoopVarAsStructuralParam(binding.source_loop_var, mir_id);
  }

  mir::Block ctor_block;
  const mir::LocalId self_id = ctor_block.vars.Add(
      mir::LocalDecl{.name = "self", .type = mir_class.self_pointer_type});
  ScopeChainNode outer_scope_link{};
  const WalkFrame scope_frame =
      frame.WithClass(&mir_class, outer_scope_link)
          .WithBlock(&ctor_block)
          .WithSelfBinding(self_id, frame.block_depth);
  const mir::TypeId void_type = module.Unit().AddType(mir::VoidType{});
  const mir::TypeId self_ptr_type = mir_class.self_pointer_type;
  const auto self_read = [&]() -> mir::ExprId {
    return ctor_block.exprs.Add(BuildSelfRefExpr(scope_frame, self_ptr_type));
  };
  for (std::size_t i = 0; i < hir_scope.structural_vars.size(); ++i) {
    const hir::StructuralVarId hir_id{static_cast<std::uint32_t>(i)};
    const auto& d = hir_scope.structural_vars.Get(hir_id);
    const mir::TypeId mir_value_type = module.TranslateType(d.type);

    const auto& var_data = module.Unit().GetType(mir_value_type).data;
    // A module-scope value-storage signal becomes an observable cell so
    // writes route through `Var<T>::Set` and subscribers fire.
    const mir::TypeId mir_field_type =
        MaybeWrapObservable(module, mir_value_type);
    const mir::MemberId mir_id = mir_class.members.Add(
        mir::MemberDecl{.name = d.name, .type = mir_field_type});
    lowerer.MapStructuralVar(hir_id, mir_id);

    // Owned children (pointer / vector / object), resolution slots, upward
    // refs, and named events have no "value assignment" -- their declaration
    // shape itself fixes the field at construction. Value-typed signals
    // (integral, string, real, unpacked / dynamic array) receive an LRM 10.5
    // initialization statement before any RegisterSignal / CreateProcesses.
    const bool is_assignable_value =
        !std::holds_alternative<mir::PointerType>(var_data) &&
        !std::holds_alternative<mir::VectorType>(var_data) &&
        !std::holds_alternative<mir::ExternalRefType>(var_data) &&
        !std::holds_alternative<mir::ObjectType>(var_data) &&
        !std::holds_alternative<mir::ExternalUnitObjectType>(var_data) &&
        !std::holds_alternative<mir::EventType>(var_data);
    if (is_assignable_value) {
      mir::ExprId value_id{};
      if (d.initializer.has_value()) {
        auto value_or =
            lowerer.LowerExpr(hir_scope.exprs.Get(*d.initializer), scope_frame);
        if (!value_or) return std::unexpected(std::move(value_or.error()));
        value_id = ctor_block.exprs.Add(*std::move(value_or));
      } else {
        value_id = ctor_block.exprs.Add(
            BuildDefaultValueExpr(module, scope_frame, mir_value_type));
      }
      const mir::ExprId init_target = ctor_block.exprs.Add(
          mir::MakeMemberAccessExpr(
              self_read(), mir::MemberRef{.hops = {.value = 0}, .var = mir_id},
              mir_field_type));
      // An observable cell init routes through `Var<T>::Set` so the field's
      // engine-side change-tracking sees the initial value
      // (`docs/decisions/value-type-concepts.md`). Plain fields use a regular
      // `AssignExpr`.
      const mir::ExprId services_id = ctor_block.exprs.Add(
          mir::MakeServicesCallExpr(
              self_read(), module.Unit().builtins.services));
      const mir::Expr init_expr = BuildObservableAssignExpr(
          module.Unit(), ctor_block, services_id, init_target, value_id,
          std::nullopt, mir_value_type, module.Unit().builtins.void_type);
      const mir::ExprId assign_id = ctor_block.exprs.Add(init_expr);
      ctor_block.AppendStmt(
          mir::Stmt{
              .label = std::nullopt, .data = mir::ExprStmt{.expr = assign_id}});
    }

    // A value signal, or a named event, records its address under its name so a
    // cross-unit referrer resolves it by name at construction. The excluded
    // members -- owned children and cross-unit reference slots -- are not
    // signals.
    const bool is_signal =
        !std::holds_alternative<mir::PointerType>(var_data) &&
        !std::holds_alternative<mir::VectorType>(var_data) &&
        !std::holds_alternative<mir::ExternalRefType>(var_data) &&
        !std::holds_alternative<mir::ObjectType>(var_data) &&
        !std::holds_alternative<mir::ExternalUnitObjectType>(var_data);
    if (is_signal) {
      const mir::ExprId var_ref = ctor_block.exprs.Add(
          mir::MakeMemberAccessExpr(
              self_read(), mir::MemberRef{.hops = {.value = 0}, .var = mir_id},
              mir_field_type));
      const mir::ExprId call = ctor_block.exprs.Add(
          mir::Expr{
              .data =
                  mir::CallExpr{
                      .callee =
                          mir::RuntimeNavCallee{
                              .fn = mir::RuntimeFn::kRegisterSignal,
                              .name = d.name},
                      .arguments = {self_read(), var_ref}},
              .type = void_type});
      ctor_block.AppendStmt(
          mir::Stmt{
              .label = std::nullopt, .data = mir::ExprStmt{.expr = call}});
    }
  }

  // Upward refs become ExternalRef members and every cross-unit slot's MIR
  // target is recorded before any body is lowered, so reads and sensitivity in
  // subroutines and processes resolve each slot.
  const auto cross_unit_slot_vars =
      MaterializeCrossUnitRefTargets(lowerer, scope_frame);

  // Map every subroutine's identity before lowering any body, so a call in one
  // body resolves a forward or mutual reference to a peer (LRM 13.7). Only the
  // HIR -> MIR id mapping has to precede the bodies; the MIR id is the index
  // each decl will occupy, and the loop below adds the lowered decls in that
  // same order.
  for (std::size_t i = 0; i < hir_scope.structural_subroutines.size(); ++i) {
    lowerer.MapStructuralSubroutine(
        hir::StructuralSubroutineId{static_cast<std::uint32_t>(i)},
        mir::MethodId{static_cast<std::uint32_t>(i)});
  }
  for (std::size_t i = 0; i < hir_scope.structural_subroutines.size(); ++i) {
    const auto& src = hir_scope.structural_subroutines.Get(
        hir::StructuralSubroutineId{static_cast<std::uint32_t>(i)});
    ProcessLowerer subroutine_lowerer(
        module, lowerer, hir_scope.time_resolution, src.body, src.name,
        scope_frame);
    auto decl_or = subroutine_lowerer.Run(src);
    if (!decl_or) return std::unexpected(std::move(decl_or.error()));
    const mir::MethodId added = mir_class.methods.Add(*std::move(decl_or));
    if (added.value != i) {
      throw InternalError(
          "ClassLowerer::Run: subroutine added out of mapped "
          "id order");
    }
  }

  for (const auto& p : hir_scope.processes) {
    std::string name = std::format("process_{}", mir_class.processes.size());
    ProcessLowerer process_lowerer(
        module, lowerer, hir_scope.time_resolution, p.body, std::move(name),
        scope_frame);
    auto proc_or = process_lowerer.Run(p);
    if (!proc_or) return std::unexpected(std::move(proc_or.error()));
    mir_class.processes.Add(*std::move(proc_or));
  }

  for (const auto& ca : hir_scope.continuous_assigns) {
    std::string name = std::format("process_{}", mir_class.processes.size());
    auto proc_or =
        LowerContinuousAssign(lowerer, scope_frame, std::move(name), ca);
    if (!proc_or) return std::unexpected(std::move(proc_or.error()));
    mir_class.processes.Add(*std::move(proc_or));
  }

  auto bindings_r = InstallGenerateOwnedChildScopes(lowerer, scope_frame);
  if (!bindings_r) return std::unexpected(std::move(bindings_r.error()));

  for (std::size_t i = 0; i < hir_scope.generates.size(); ++i) {
    auto stmt = LowerGenerateAsStmt(
        lowerer, scope_frame,
        hir_scope.generates.Get(hir::GenerateId{static_cast<std::uint32_t>(i)}),
        bindings_r->at(i));
    if (!stmt) return std::unexpected(std::move(stmt.error()));
    ctor_block.AppendStmt(*std::move(stmt));
  }

  const auto instance_member_vars =
      InstallInstanceMembers(lowerer, scope_frame);
  InstallCrossUnitRefs(
      lowerer, scope_frame, instance_member_vars, *bindings_r,
      cross_unit_slot_vars);

  mir_class.constructor_block = std::move(ctor_block);

  return mir_class;
}

auto ClassLowerer::LowerExpr(const hir::Expr& expr, WalkFrame frame) const
    -> diag::Result<mir::Expr> {
  const mir::TypeId result_type = module_->TranslateType(expr.type);
  auto raw_or = std::visit(
      Overloaded{
          [&](const hir::PrimaryExpr& p) -> diag::Result<mir::Expr> {
            return LowerHirPrimaryExprStructural(
                *this, frame, p.data, result_type);
          },
          [&](const hir::UnaryExpr& u) -> diag::Result<mir::Expr> {
            return LowerHirUnaryExpr(*this, frame, u, result_type);
          },
          [&](const hir::BinaryExpr& b) -> diag::Result<mir::Expr> {
            return LowerHirBinaryExpr(*this, frame, b, result_type);
          },
          [&](const hir::ConditionalExpr& c) -> diag::Result<mir::Expr> {
            return LowerHirConditionalExpr(*this, frame, c, result_type);
          },
          [](const hir::AssignExpr&) -> diag::Result<mir::Expr> {
            throw InternalError(
                "ClassLowerer::LowerExpr: HIR AssignExpr does not "
                "appear in constructor-side expressions; structural code has "
                "no general assignment");
          },
          [](const hir::IncDecExpr&) -> diag::Result<mir::Expr> {
            throw InternalError(
                "ClassLowerer::LowerExpr: HIR IncDecExpr does not "
                "appear in constructor-side expressions; structural code has "
                "no increment / decrement");
          },
          [&](const hir::ConversionExpr& cv) -> diag::Result<mir::Expr> {
            return LowerHirConversionExpr(*this, frame, cv, result_type);
          },
          [](const hir::CallExpr&) -> diag::Result<mir::Expr> {
            return diag::Unsupported(
                diag::DiagCode::kUnsupportedStructuralExpressionForm,
                "calls are not allowed in constructor expressions",
                diag::UnsupportedCategory::kFeature);
          },
          [](const hir::InsideExpr&) -> diag::Result<mir::Expr> {
            return diag::Unsupported(
                diag::DiagCode::kUnsupportedStructuralExpressionForm,
                "inside operator is not allowed in constructor expressions",
                diag::UnsupportedCategory::kFeature);
          },
          [&](const hir::ElementSelectExpr& s) -> diag::Result<mir::Expr> {
            return LowerHirElementSelectExpr(*this, frame, s, result_type);
          },
          [&](const hir::RangeSelectExpr& s) -> diag::Result<mir::Expr> {
            return LowerHirRangeSelectExpr(*this, frame, s, result_type);
          },
          [&](const hir::MemberAccessExpr& s) -> diag::Result<mir::Expr> {
            return LowerHirMemberAccessExpr(*this, frame, s, result_type);
          },
          [&](const hir::ConcatExpr& c) -> diag::Result<mir::Expr> {
            return LowerHirConcatExpr(*this, frame, c, result_type);
          },
          [](const hir::ReplicationExpr&) -> diag::Result<mir::Expr> {
            return diag::Unsupported(
                diag::DiagCode::kUnsupportedStructuralExpressionForm,
                "replication in constructor expressions is not yet supported",
                diag::UnsupportedCategory::kFeature);
          },
          [&](const hir::AssignmentPatternExpr& a) -> diag::Result<mir::Expr> {
            return LowerHirAssignmentPatternExpr(*this, frame, a, result_type);
          },
          [&](const hir::AssignmentPatternReplicationExpr& a)
              -> diag::Result<mir::Expr> {
            return LowerHirAssignmentPatternReplicationExpr(
                *this, frame, a, result_type);
          },
          [](const hir::DynamicArrayNewExpr&) -> diag::Result<mir::Expr> {
            return diag::Unsupported(
                diag::DiagCode::kUnsupportedStructuralExpressionForm,
                "dynamic-array new[] is not allowed in constructor "
                "expressions; LRM 7.5.1 restricts it to blocking assignments",
                diag::UnsupportedCategory::kFeature);
          },
          [&](const hir::AssociativeAssignmentPatternExpr& a)
              -> diag::Result<mir::Expr> {
            return LowerHirAssociativeAssignmentPatternExpr(
                *this, frame, a, result_type);
          },
      },
      expr.data);
  if (!raw_or) return raw_or;
  // Same as ProcessLowerer::LowerExpr: unwrap an observable cell leaf with
  // an explicit `Get` call so the surrounding structural expression sees
  // the value (e.g. a continuous assign whose RHS reads a signal).
  if (mir::IsObservableCellType(module_->Unit().GetType(raw_or->type))) {
    const mir::ExprId cell_id =
        frame.current_block->exprs.Add(*std::move(raw_or));
    return mir::MakeObservableGetCallExpr(cell_id, result_type);
  }
  return raw_or;
}

auto ClassLowerer::LowerLhsExpr(const hir::Expr& expr, WalkFrame frame) const
    -> diag::Result<mir::Expr> {
  const mir::TypeId result_type = module_->TranslateType(expr.type);
  return std::visit(
      Overloaded{
          [&](const hir::PrimaryExpr& p) -> diag::Result<mir::Expr> {
            return LowerHirPrimaryExprStructural(
                *this, frame, p.data, result_type);
          },
          [&](const hir::ElementSelectExpr& s) -> diag::Result<mir::Expr> {
            return LowerHirElementSelectExprLhs(*this, frame, s, result_type);
          },
          [&](const hir::RangeSelectExpr& s) -> diag::Result<mir::Expr> {
            return LowerHirRangeSelectExprLhs(*this, frame, s, result_type);
          },
          [&](const hir::MemberAccessExpr& s) -> diag::Result<mir::Expr> {
            return LowerHirMemberAccessExprLhs(*this, frame, s, result_type);
          },
          [&](const hir::ConcatExpr& c) -> diag::Result<mir::Expr> {
            return LowerHirConcatExpr(*this, frame, c, result_type);
          },
          [](const auto&) -> diag::Result<mir::Expr> {
            throw InternalError(
                "ClassLowerer::LowerLhsExpr: non-addressable HIR "
                "expression in LHS context");
          },
      },
      expr.data);
}

}  // namespace lyra::lowering::hir_to_mir
