#include "lyra/lowering/hir_to_mir/structural_scope_lowerer.hpp"

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
#include "lyra/lowering/hir_to_mir/process_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/walk_frame.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/procedural_hops.hpp"
#include "lyra/mir/procedural_var.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/mir/structural_scope.hpp"
#include "lyra/mir/structural_var.hpp"
#include "lyra/mir/type.hpp"

namespace lyra::lowering::hir_to_mir {

namespace {

auto ChildScopeNameFor(std::size_t gen_index, std::string_view arm_tag)
    -> std::string {
  return std::format("gen{}_{}", gen_index, arm_tag);
}

auto CompanionVarNameFor(std::string_view child_scope_name) -> std::string {
  return std::string(child_scope_name) + "_obj";
}

void CheckNoNameCollision(
    const mir::StructuralScope& owner_scope, std::string_view child_scope_name,
    std::string_view companion_var_name) {
  for (const auto& v : owner_scope.structural_vars) {
    if (v.name == companion_var_name || v.name == child_scope_name) {
      throw InternalError(
          "child scope or companion var name collides with an existing "
          "structural var declaration in the enclosing scope");
    }
  }
  for (const auto& c : owner_scope.child_structural_scopes) {
    if (c.name == child_scope_name) {
      throw InternalError(
          "child scope name collides with an existing nested structural "
          "scope declaration in the enclosing scope");
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
            const auto& then_scope = gen.GetChildScope(if_gen.then_scope);
            specs.push_back(
                {.scope_id = if_gen.then_scope,
                 .scope = &then_scope,
                 .scope_name = ChildScopeNameFor(gen_index, "then"),
                 .is_repeated = false,
                 .entry_bindings = {}});
            if (if_gen.else_scope.has_value()) {
              const auto& else_scope = gen.GetChildScope(*if_gen.else_scope);
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
                  gen.GetChildScope(case_gen.items[k].scope);
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
                  gen.GetChildScope(*case_gen.default_scope);
              specs.push_back(
                  {.scope_id = *case_gen.default_scope,
                   .scope = &default_scope,
                   .scope_name = ChildScopeNameFor(gen_index, "default"),
                   .is_repeated = false,
                   .entry_bindings = {}});
            }
          },
          [&](const hir::LoopGenerate& loop_gen) {
            const auto& loop_scope = gen.GetChildScope(loop_gen.scope);
            const auto& var_decl =
                enclosing_scope.GetLoopVarDecl(loop_gen.loop_var);
            std::vector<ScopeEntryStructuralParamBinding> bindings;
            bindings.push_back(
                ScopeEntryStructuralParamBinding{
                    .param =
                        mir::StructuralParamDecl{
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

auto MakeUniqueObjectPointer(
    ModuleLowerer& module, mir::StructuralScopeId child_id) -> mir::TypeId {
  const mir::TypeId object_type =
      module.Unit().AddType(mir::ObjectType{.target = child_id});
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
auto InstallInstanceMembers(StructuralScopeLowerer& scope, WalkFrame frame)
    -> std::vector<mir::StructuralVarId> {
  mir::StructuralScope& mir_scope = *frame.current_structural_scope;
  mir::ProceduralScope& ctor_scope = *frame.current_procedural_scope;
  const hir::StructuralScope& hir_scope = scope.HirScope();
  std::vector<mir::StructuralVarId> instance_member_vars;
  instance_member_vars.reserve(hir_scope.instance_members.size());
  for (const auto& im : hir_scope.instance_members) {
    for (const auto& v : mir_scope.structural_vars) {
      if (v.name == im.instance_name) {
        throw InternalError(
            "instance member name collides with an existing structural var "
            "declaration in the enclosing scope");
      }
    }
    const mir::TypeId var_type = MakeExternalUnitMemberType(
        scope.Module(), im.target_unit, im.array_dims.size());
    const mir::ExprId init =
        SynthesizeDefaultValueExpr(scope.Module(), frame, var_type);
    const mir::StructuralVarId var_id = mir_scope.AddStructuralVar(
        mir::StructuralVarDecl{
            .name = im.instance_name, .type = var_type, .initializer = init});
    instance_member_vars.push_back(var_id);

    ctor_scope.AppendStmt(
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
// pointer slot structural var, null until the constructor resolves it. Both
// run before processes so reads resolve to the slot; both record their MIR read
// target as a StructuralVarRef to that member, in HIR slot order. The returned
// vector carries the downward slot var per ref (nullopt for upward), consumed
// by InstallCrossUnitRefs once the children exist.
auto MaterializeCrossUnitRefTargets(
    StructuralScopeLowerer& scope, WalkFrame frame)
    -> std::vector<std::optional<mir::StructuralVarId>> {
  ModuleLowerer& module = scope.Module();
  mir::StructuralScope& mir_scope = *frame.current_structural_scope;
  const hir::StructuralScope& hir_scope = scope.HirScope();
  std::vector<std::optional<mir::StructuralVarId>> slot_vars;
  std::uint32_t downward_slot = 0;
  for (const auto& cu : hir_scope.cross_unit_refs) {
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

      std::string member_name = "up_" + up->ancestor_name;
      for (const auto& hop : tail) {
        member_name += "_" + hop.name;
        for (const std::uint32_t index : hop.indices) {
          member_name += std::to_string(index);
        }
      }
      member_name += "_" + signal;

      const mir::TypeId leaf = module.TranslateType(cu.type);
      const mir::TypeId ext_type = module.Unit().AddType(
          mir::ExternalRefType{
              .element = leaf,
              .ancestor = up->ancestor_name,
              .tail = std::move(tail),
              .signal = std::move(signal)});
      const mir::ExprId init = SynthesizeDefaultValueExpr(module, frame, leaf);
      const mir::StructuralVarId var = mir_scope.AddStructuralVar(
          mir::StructuralVarDecl{
              .name = std::move(member_name),
              .type = ext_type,
              .initializer = init});
      scope.AddCrossUnitRefTarget(
          mir::StructuralVarRef{.hops = {.value = 0}, .var = var}, ext_type);
      slot_vars.emplace_back(std::nullopt);
    } else {
      const mir::TypeId leaf = module.TranslateType(cu.type);
      const mir::TypeId slot_type = module.Unit().AddType(
          mir::PointerType{
              .pointee = leaf, .ownership = mir::PointerOwnership::kBorrowed});
      const mir::ExprId init =
          SynthesizeDefaultValueExpr(module, frame, slot_type);
      const mir::StructuralVarId slot = mir_scope.AddStructuralVar(
          mir::StructuralVarDecl{
              .name = "xref" + std::to_string(downward_slot),
              .type = slot_type,
              .initializer = init});
      scope.AddCrossUnitRefTarget(
          mir::StructuralVarRef{.hops = {.value = 0}, .var = slot}, slot_type);
      slot_vars.emplace_back(slot);
      ++downward_slot;
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
    mir::TypeId scope_ptr_type) -> mir::ExprId {
  mir::ProceduralScope& ctor_scope = *frame.current_procedural_scope;
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
      hops.back().indices.push_back(ctor_scope.AddExpr(
          mir::MakeInt32Literal(
              module.Unit().builtins.int32, static_cast<std::int64_t>(index))));
    }
  }
  if (hops.size() < 2) {
    throw InternalError(
        "BuildDownwardNavValue: downward reference has no leaf signal past its "
        "owned child");
  }

  mir::ExprId cur = ctor_scope.AddExpr(
      mir::Expr{.data = mir::SelfScopeExpr{}, .type = scope_ptr_type});
  for (std::size_t i = 0; i + 1 < hops.size(); ++i) {
    std::vector<mir::ExprId> args;
    args.push_back(cur);
    for (const mir::ExprId idx : hops[i].indices) {
      args.push_back(idx);
    }
    cur = ctor_scope.AddExpr(
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
  return ctor_scope.AddExpr(
      mir::Expr{
          .data =
              mir::CallExpr{
                  .callee =
                      mir::RuntimeNavCallee{
                          .fn = mir::RuntimeFn::kGetSignal,
                          .name = hops.back().name},
                  .arguments = {cur}},
          .type = slot_type});
}

// A downward slot resolves in the constructor by navigating from the enclosing
// scope after the children are built (reference_resolution.md): an ordinary
// assignment of the navigation value into the borrowed-pointer slot. Upward
// slots are materialized as ExternalRef members upstream and skipped here.
void InstallCrossUnitRefs(
    StructuralScopeLowerer& scope, WalkFrame frame,
    const std::vector<mir::StructuralVarId>& instance_member_vars,
    const std::vector<GenerateBindings>& gen_bindings,
    const std::vector<std::optional<mir::StructuralVarId>>& slot_vars) {
  mir::StructuralScope& mir_scope = *frame.current_structural_scope;
  mir::ProceduralScope& ctor_scope = *frame.current_procedural_scope;
  const hir::StructuralScope& hir_scope = scope.HirScope();
  ModuleLowerer& module = scope.Module();
  const mir::TypeId scope_ptr_type = module.Unit().AddType(
      mir::PointerType{
          .pointee = module.Unit().AddType(mir::ScopeType{}),
          .ownership = mir::PointerOwnership::kBorrowed});
  for (std::size_t ci = 0; ci < hir_scope.cross_unit_refs.size(); ++ci) {
    const auto& cu = hir_scope.cross_unit_refs[ci];
    const auto* down = std::get_if<hir::DownwardHead>(&cu.head);
    if (down == nullptr) {
      continue;
    }
    const mir::StructuralVarId slot = *slot_vars.at(ci);
    mir::StructuralVarId head_var{};
    if (const auto* im = std::get_if<hir::InstanceMemberId>(&down->child)) {
      head_var = instance_member_vars.at(im->value);
    } else {
      const auto& g = std::get<hir::GenerateChildRef>(down->child);
      head_var = gen_bindings.at(g.generate.value)
                     .by_scope_id.at(g.scope.value)
                     .var_id;
    }
    const auto& head = mir_scope.GetStructuralVar(head_var);
    const std::string head_name =
        head.source_name.empty() ? head.name : head.source_name;
    const mir::TypeId slot_type = mir_scope.GetStructuralVar(slot).type;
    const mir::ExprId nav = BuildDownwardNavValue(
        module, frame, head_name, cu.path, slot_type, scope_ptr_type);
    const mir::ExprId target = ctor_scope.AddExpr(
        mir::Expr{
            .data = mir::StructuralVarRef{.hops = {.value = 0}, .var = slot},
            .type = slot_type});
    const mir::ExprId assign = ctor_scope.AddExpr(
        mir::Expr{
            .data = mir::AssignExpr{.target = target, .value = nav},
            .type = slot_type});
    ctor_scope.AppendStmt(
        mir::Stmt{
            .label = std::nullopt, .data = mir::ExprStmt{.expr = assign}});
  }
}

void ValidateConstructOwnedObjectStmt(
    const mir::CompilationUnit& unit, const mir::StructuralScope& owner_scope,
    const mir::ProceduralScope& proc_scope,
    const mir::ConstructOwnedObjectStmt& stmt) {
  if (stmt.scope_id.value >= owner_scope.child_structural_scopes.size()) {
    throw InternalError(
        "ConstructOwnedObjectStmt: scope_id is not a direct child of the "
        "enclosing scope");
  }
  if (stmt.target.value >= owner_scope.structural_vars.size()) {
    throw InternalError(
        "ConstructOwnedObjectStmt: target is out of range in the enclosing "
        "scope");
  }
  const auto& var = owner_scope.GetStructuralVar(stmt.target);
  const auto child = mir::GetChildScope(unit, var.type);
  const auto* generate =
      child ? std::get_if<mir::GenerateScopeChild>(&*child) : nullptr;
  if (generate == nullptr || generate->target != stmt.scope_id) {
    throw InternalError(
        "ConstructOwnedObjectStmt: target var does not own the requested "
        "scope");
  }
  const auto& child_scope = owner_scope.GetChildStructuralScope(stmt.scope_id);
  if (stmt.args.size() != child_scope.structural_params.size()) {
    throw InternalError(
        "ConstructOwnedObjectStmt: args count does not match child scope "
        "structural params count");
  }
  for (std::size_t i = 0; i < stmt.args.size(); ++i) {
    const auto& arg = proc_scope.GetExpr(stmt.args[i]);
    const auto& param = child_scope.structural_params[i];
    if (arg.type != param.type) {
      throw InternalError(
          "ConstructOwnedObjectStmt: arg type does not match structural "
          "param type");
    }
  }
}

// The for-stmt body procedural scope is one level below the parent
// constructor scope where the induction var was declared, so a
// `ProceduralVarRef` to that var read from inside the body always hops up
// once. This helper documents the +1 instead of writing it inline.
auto MakeForBodyInductionVarArg(
    mir::ProceduralVarId induction_var_id, mir::TypeId type) -> mir::Expr {
  return mir::Expr{
      .data =
          mir::ProceduralVarRef{
              .hops = mir::ProceduralHops{.value = 1}, .var = induction_var_id},
      .type = type};
}

auto BuildGenerateArmBody(
    const ModuleLowerer& module, WalkFrame frame,
    const GenerateBindings& gen_bindings, hir::StructuralScopeId arm_scope_id,
    std::vector<mir::Expr> args) -> mir::ProceduralScope {
  const auto& binding = gen_bindings.by_scope_id.at(arm_scope_id.value);

  mir::ProceduralScope arm_scope;
  std::vector<mir::ExprId> arg_ids;
  arg_ids.reserve(args.size());
  for (auto& arg : args) {
    arg_ids.push_back(arm_scope.AddExpr(std::move(arg)));
  }

  const mir::ConstructOwnedObjectStmt construct_stmt{
      .target = binding.var_id,
      .scope_id = binding.scope_id,
      .args = std::move(arg_ids)};
  ValidateConstructOwnedObjectStmt(
      module.Unit(), *frame.current_structural_scope, arm_scope,
      construct_stmt);

  arm_scope.AppendStmt(
      mir::Stmt{.label = std::nullopt, .data = construct_stmt});
  return arm_scope;
}

auto LowerIfGenerate(
    StructuralScopeLowerer& scope, WalkFrame frame,
    const GenerateBindings& gen_bindings, const hir::IfGenerate& if_gen)
    -> diag::Result<mir::Stmt> {
  const hir::StructuralScope& enclosing_scope = scope.HirScope();
  mir::ProceduralScope& proc_scope = *frame.current_procedural_scope;

  auto cond_or =
      scope.LowerExpr(enclosing_scope.GetExpr(if_gen.condition), frame);
  if (!cond_or) return std::unexpected(std::move(cond_or.error()));
  const mir::ExprId cond_id = proc_scope.AddExpr(*std::move(cond_or));

  const mir::ProceduralScopeId then_id =
      proc_scope.AddChildScope(BuildGenerateArmBody(
          scope.Module(), frame, gen_bindings, if_gen.then_scope, {}));

  std::optional<mir::ProceduralScopeId> else_id;
  if (if_gen.else_scope.has_value()) {
    else_id = proc_scope.AddChildScope(BuildGenerateArmBody(
        scope.Module(), frame, gen_bindings, *if_gen.else_scope, {}));
  }

  return mir::Stmt{
      .label = std::nullopt,
      .data = mir::IfStmt{
          .condition = cond_id, .then_scope = then_id, .else_scope = else_id}};
}

auto LowerCaseGenerate(
    StructuralScopeLowerer& scope, WalkFrame frame,
    const GenerateBindings& gen_bindings, const hir::CaseGenerate& case_gen)
    -> diag::Result<mir::Stmt> {
  const hir::StructuralScope& enclosing_scope = scope.HirScope();
  const mir::TypeId bit_type = scope.Module().Unit().builtins.bit1;

  mir::ProceduralScope wrapper_scope;
  const WalkFrame wrapper_frame = frame.WithProceduralScope(&wrapper_scope);

  auto cond_or = scope.LowerExpr(
      enclosing_scope.GetExpr(case_gen.condition), wrapper_frame);
  if (!cond_or) return std::unexpected(std::move(cond_or.error()));
  const mir::ExprId cond_expr_id = wrapper_scope.AddExpr(*std::move(cond_or));

  const CaseSnapshotRefs snapshot =
      AppendCaseSnapshot(scope.Module(), wrapper_frame, cond_expr_id);

  std::vector<mir::ProceduralScope> body_scopes;
  body_scopes.reserve(case_gen.items.size());
  for (const auto& item : case_gen.items) {
    body_scopes.push_back(BuildGenerateArmBody(
        scope.Module(), frame, gen_bindings, item.scope, {}));
  }

  std::optional<mir::ProceduralScope> default_scope;
  if (case_gen.default_scope.has_value()) {
    default_scope = BuildGenerateArmBody(
        scope.Module(), frame, gen_bindings, *case_gen.default_scope, {});
  }

  auto build_predicate =
      [&](WalkFrame enc_frame, std::size_t item_idx,
          std::uint32_t sel_hops) -> diag::Result<mir::ExprId> {
    return BuildEqualityChain(
        enc_frame, snapshot, bit_type, mir::BinaryOp::kEquality, sel_hops,
        case_gen.items[item_idx].labels.size(),
        [&](WalkFrame label_frame,
            std::size_t li) -> diag::Result<mir::ExprId> {
          auto lab_or = scope.LowerExpr(
              enclosing_scope.GetExpr(case_gen.items[item_idx].labels[li]),
              label_frame);
          if (!lab_or) {
            return std::unexpected(std::move(lab_or.error()));
          }
          return label_frame.current_procedural_scope->AddExpr(
              *std::move(lab_or));
        });
  };

  return BuildCaseCascade(
      frame, std::move(wrapper_scope), std::nullopt, case_gen.items.size(),
      std::move(body_scopes), std::move(default_scope), build_predicate);
}

auto LowerLoopGenerate(
    StructuralScopeLowerer& scope, WalkFrame frame,
    const GenerateBindings& gen_bindings, const hir::LoopGenerate& loop)
    -> diag::Result<mir::Stmt> {
  const hir::StructuralScope& enclosing_scope = scope.HirScope();
  mir::ProceduralScope& proc_scope = *frame.current_procedural_scope;

  const auto& var_decl = enclosing_scope.GetLoopVarDecl(loop.loop_var);
  const mir::TypeId genvar_type = scope.Module().TranslateType(var_decl.type);

  const mir::ProceduralVarId loop_local_id = proc_scope.AddProceduralVar(
      mir::ProceduralVarDecl{.name = var_decl.name, .type = genvar_type});
  const mir::ProceduralVarRef loop_local{
      .hops = mir::ProceduralHops{.value = 0}, .var = loop_local_id};

  scope.MapLoopVarAsProcedural(loop.loop_var, loop_local);

  const WalkFrame proc_frame =
      frame.WithLoopVarMode(LoopVarLoweringMode::kProceduralInduction);
  auto init_or =
      scope.LowerExpr(enclosing_scope.GetExpr(loop.initial), proc_frame);
  if (!init_or) return std::unexpected(std::move(init_or.error()));
  const mir::ExprId init_id = proc_scope.AddExpr(*std::move(init_or));

  auto cond_or =
      scope.LowerExpr(enclosing_scope.GetExpr(loop.stop), proc_frame);
  if (!cond_or) return std::unexpected(std::move(cond_or.error()));
  const mir::ExprId cond_id = proc_scope.AddExpr(*std::move(cond_or));

  // HIR carries the iter as the next-value expression for the loop variable;
  // the loop semantic (this lowering) owns the actual write back.
  auto step_value_or =
      scope.LowerExpr(enclosing_scope.GetExpr(loop.iter), proc_frame);
  if (!step_value_or) {
    return std::unexpected(std::move(step_value_or.error()));
  }
  const mir::TypeId step_type = (*step_value_or).type;
  const mir::ExprId step_value_id =
      proc_scope.AddExpr(*std::move(step_value_or));
  const mir::ExprId step_target_id =
      proc_scope.AddExpr(mir::Expr{.data = loop_local, .type = genvar_type});
  const mir::ExprId step_id = proc_scope.AddExpr(
      mir::Expr{
          .data =
              mir::AssignExpr{.target = step_target_id, .value = step_value_id},
          .type = step_type});

  std::vector<mir::Expr> body_args;
  body_args.push_back(MakeForBodyInductionVarArg(loop_local_id, genvar_type));

  const mir::ProceduralScopeId loop_scope_id =
      proc_scope.AddChildScope(BuildGenerateArmBody(
          scope.Module(), frame, gen_bindings, loop.scope,
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
    StructuralScopeLowerer& scope, WalkFrame frame, const hir::Generate& gen,
    const GenerateBindings& gen_bindings) -> diag::Result<mir::Stmt> {
  return std::visit(
      Overloaded{
          [&](const hir::IfGenerate& if_gen) {
            return LowerIfGenerate(scope, frame, gen_bindings, if_gen);
          },
          [&](const hir::CaseGenerate& case_gen) {
            return LowerCaseGenerate(scope, frame, gen_bindings, case_gen);
          },
          [&](const hir::LoopGenerate& loop) {
            return LowerLoopGenerate(scope, frame, gen_bindings, loop);
          },
      },
      gen.data);
}

auto InstallGenerateOwnedChildScopes(
    StructuralScopeLowerer& scope, WalkFrame frame)
    -> diag::Result<std::vector<GenerateBindings>> {
  ModuleLowerer& module = scope.Module();
  mir::StructuralScope& mir_scope = *frame.current_structural_scope;
  const hir::StructuralScope& hir_scope = scope.HirScope();
  std::vector<GenerateBindings> bindings_by_generate;
  bindings_by_generate.reserve(hir_scope.generates.size());

  for (std::size_t gen_idx = 0; gen_idx < hir_scope.generates.size();
       ++gen_idx) {
    const auto& gen = hir_scope.generates[gen_idx];
    GenerateBindings gen_bindings;
    gen_bindings.by_scope_id.resize(gen.child_scopes.size());

    auto specs = EnumerateGenerateChildSpecs(gen, gen_idx, hir_scope, module);
    for (auto& spec : specs) {
      const auto companion_name = CompanionVarNameFor(spec.scope_name);
      CheckNoNameCollision(mir_scope, spec.scope_name, companion_name);

      StructuralScopeLowerer child_scope(
          module, &scope, std::move(spec.scope_name), *spec.scope);
      auto child_r = child_scope.Run(frame, spec.entry_bindings);
      if (!child_r) return std::unexpected(std::move(child_r.error()));

      const mir::StructuralScopeId child_id =
          mir_scope.AddChildStructuralScope(*std::move(child_r));
      mir::TypeId var_type = MakeUniqueObjectPointer(module, child_id);
      if (spec.is_repeated) {
        var_type = module.Unit().AddType(mir::VectorType{.element = var_type});
      }
      const mir::ExprId companion_init =
          SynthesizeDefaultValueExpr(module, frame, var_type);
      const mir::StructuralVarId var_id = mir_scope.AddStructuralVar(
          mir::StructuralVarDecl{
              .name = companion_name,
              .source_name = spec.scope->source_name,
              .type = var_type,
              .initializer = companion_init});

      gen_bindings.by_scope_id.at(spec.scope_id.value) =
          ChildStructuralScopeBinding{.scope_id = child_id, .var_id = var_id};
    }

    bindings_by_generate.push_back(std::move(gen_bindings));
  }
  return bindings_by_generate;
}

}  // namespace

auto StructuralScopeLowerer::Run(
    WalkFrame frame,
    std::span<const ScopeEntryStructuralParamBinding> entry_bindings)
    -> diag::Result<mir::StructuralScope> {
  ModuleLowerer& module = *module_;
  const hir::StructuralScope& hir_scope = *hir_scope_;
  StructuralScopeLowerer& scope = *this;

  mir::StructuralScope mir_scope{
      .name = name_,
      .time_resolution = hir_scope.time_resolution,
      .structural_params = {},
      .structural_vars = {},
      .constructor_scope = {},
      .processes = {},
      .child_structural_scopes = {},
      .structural_subroutines = {},
      .type_aliases = {}};
  for (const auto& alias : hir_scope.type_aliases) {
    mir_scope.type_aliases.push_back(
        mir::TypeAliasDecl{
            .name = alias.name, .target = module.TranslateType(alias.target)});
  }

  for (const auto& binding : entry_bindings) {
    const mir::StructuralParamId mir_id =
        mir_scope.AddStructuralParam(binding.param);
    scope.MapLoopVarAsStructuralParam(binding.source_loop_var, mir_id);
  }

  mir::ProceduralScope ctor_scope;
  const WalkFrame scope_frame =
      frame.WithStructuralScope(&mir_scope).WithProceduralScope(&ctor_scope);
  const mir::TypeId scope_ptr_type = module.Unit().AddType(
      mir::PointerType{
          .pointee = module.Unit().AddType(mir::ScopeType{}),
          .ownership = mir::PointerOwnership::kBorrowed});
  const mir::TypeId void_type = module.Unit().AddType(mir::VoidType{});
  for (std::size_t i = 0; i < hir_scope.structural_vars.size(); ++i) {
    const hir::StructuralVarId hir_id{static_cast<std::uint32_t>(i)};
    const auto& d = hir_scope.structural_vars[i];
    const mir::TypeId mir_type = module.TranslateType(d.type);
    mir::ExprId mir_init{};
    if (d.initializer.has_value()) {
      auto init_or =
          scope.LowerExpr(hir_scope.GetExpr(*d.initializer), scope_frame);
      if (!init_or) return std::unexpected(std::move(init_or.error()));
      mir_init = ctor_scope.AddExpr(*std::move(init_or));
    } else {
      mir_init = SynthesizeDefaultValueExpr(module, scope_frame, mir_type);
    }
    const mir::StructuralVarId mir_id = mir_scope.AddStructuralVar(
        mir::StructuralVarDecl{
            .name = d.name, .type = mir_type, .initializer = mir_init});
    scope.MapStructuralVar(hir_id, mir_id);

    // A value-typed var is a signal: record its address under its name so a
    // cross-unit referrer resolves it by name at construction. Owned children
    // (pointer / vector / object) and resolution slots register differently.
    const auto& var_data = module.Unit().GetType(mir_type).data;
    const bool is_signal =
        !std::holds_alternative<mir::PointerType>(var_data) &&
        !std::holds_alternative<mir::VectorType>(var_data) &&
        !std::holds_alternative<mir::ExternalRefType>(var_data) &&
        !std::holds_alternative<mir::ObjectType>(var_data) &&
        !std::holds_alternative<mir::ExternalUnitObjectType>(var_data);
    if (is_signal) {
      const mir::ExprId self = ctor_scope.AddExpr(
          mir::Expr{.data = mir::SelfScopeExpr{}, .type = scope_ptr_type});
      const mir::ExprId var_ref = ctor_scope.AddExpr(
          mir::Expr{
              .data =
                  mir::StructuralVarRef{.hops = {.value = 0}, .var = mir_id},
              .type = mir_type});
      const mir::ExprId call = ctor_scope.AddExpr(
          mir::Expr{
              .data =
                  mir::CallExpr{
                      .callee =
                          mir::RuntimeNavCallee{
                              .fn = mir::RuntimeFn::kRegisterSignal,
                              .name = d.name},
                      .arguments = {self, var_ref}},
              .type = void_type});
      ctor_scope.AppendStmt(
          mir::Stmt{
              .label = std::nullopt, .data = mir::ExprStmt{.expr = call}});
    }
  }

  // Upward refs become ExternalRef members and every cross-unit slot's MIR
  // target is recorded before any body is lowered, so reads and sensitivity in
  // subroutines and processes resolve each slot.
  const auto cross_unit_slot_vars =
      MaterializeCrossUnitRefTargets(scope, scope_frame);

  // Map every subroutine's identity before lowering any body, so a call in one
  // body resolves a forward or mutual reference to a peer (LRM 13.7). Only the
  // HIR -> MIR id mapping has to precede the bodies; the MIR id is the index
  // each decl will occupy, and the loop below adds the lowered decls in that
  // same order.
  for (std::size_t i = 0; i < hir_scope.structural_subroutines.size(); ++i) {
    scope.MapStructuralSubroutine(
        hir::StructuralSubroutineId{static_cast<std::uint32_t>(i)},
        mir::StructuralSubroutineId{static_cast<std::uint32_t>(i)});
  }
  for (std::size_t i = 0; i < hir_scope.structural_subroutines.size(); ++i) {
    const auto& src = hir_scope.structural_subroutines[i];
    ProcessLowerer subroutine_lowerer(
        module, scope, hir_scope.time_resolution, src.body);
    auto decl_or = subroutine_lowerer.Run(scope_frame, src);
    if (!decl_or) return std::unexpected(std::move(decl_or.error()));
    const mir::StructuralSubroutineId added =
        mir_scope.AddStructuralSubroutine(*std::move(decl_or));
    if (added.value != i) {
      throw InternalError(
          "StructuralScopeLowerer::Run: subroutine added out of mapped "
          "id order");
    }
  }

  for (const auto& p : hir_scope.processes) {
    ProcessLowerer process_lowerer(
        module, scope, hir_scope.time_resolution, p.body);
    auto proc_or = process_lowerer.Run(scope_frame, p);
    if (!proc_or) return std::unexpected(std::move(proc_or.error()));
    mir_scope.AddProcess(*std::move(proc_or));
  }

  for (const auto& ca : hir_scope.continuous_assigns) {
    auto proc_or = LowerContinuousAssign(scope, scope_frame, ca);
    if (!proc_or) return std::unexpected(std::move(proc_or.error()));
    mir_scope.AddProcess(*std::move(proc_or));
  }

  auto bindings_r = InstallGenerateOwnedChildScopes(scope, scope_frame);
  if (!bindings_r) return std::unexpected(std::move(bindings_r.error()));

  for (std::size_t i = 0; i < hir_scope.generates.size(); ++i) {
    auto stmt = LowerGenerateAsStmt(
        scope, scope_frame, hir_scope.generates[i], bindings_r->at(i));
    if (!stmt) return std::unexpected(std::move(stmt.error()));
    ctor_scope.AppendStmt(*std::move(stmt));
  }

  const auto instance_member_vars = InstallInstanceMembers(scope, scope_frame);
  InstallCrossUnitRefs(
      scope, scope_frame, instance_member_vars, *bindings_r,
      cross_unit_slot_vars);

  mir_scope.constructor_scope = std::move(ctor_scope);

  return mir_scope;
}

auto StructuralScopeLowerer::LowerExpr(
    const hir::Expr& expr, WalkFrame frame) const -> diag::Result<mir::Expr> {
  const mir::TypeId result_type = module_->TranslateType(expr.type);
  return std::visit(
      Overloaded{
          [&](const hir::PrimaryExpr& p) -> diag::Result<mir::Expr> {
            return LowerHirPrimaryExprStructural(
                *this, frame, p.data, result_type);
          },
          [&](const hir::UnaryExpr& u) -> diag::Result<mir::Expr> {
            return LowerHirUnaryExprStructural(*this, frame, u, result_type);
          },
          [&](const hir::BinaryExpr& b) -> diag::Result<mir::Expr> {
            return LowerHirBinaryExprStructural(*this, frame, b, result_type);
          },
          [&](const hir::ConditionalExpr& c) -> diag::Result<mir::Expr> {
            return LowerHirConditionalExprStructural(
                *this, frame, c, result_type);
          },
          [](const hir::AssignExpr&) -> diag::Result<mir::Expr> {
            throw InternalError(
                "StructuralScopeLowerer::LowerExpr: HIR AssignExpr does not "
                "appear in constructor-side expressions; structural code has "
                "no general assignment");
          },
          [](const hir::IncDecExpr&) -> diag::Result<mir::Expr> {
            throw InternalError(
                "StructuralScopeLowerer::LowerExpr: HIR IncDecExpr does not "
                "appear in constructor-side expressions; structural code has "
                "no increment / decrement");
          },
          [&](const hir::ConversionExpr& cv) -> diag::Result<mir::Expr> {
            return LowerHirConversionExprStructural(
                *this, frame, cv, result_type);
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
            return LowerHirElementSelectExprStructural(
                *this, frame, s, result_type);
          },
          [&](const hir::RangeSelectExpr& s) -> diag::Result<mir::Expr> {
            return LowerHirRangeSelectExprStructural(
                *this, frame, s, result_type);
          },
          [&](const hir::MemberAccessExpr& s) -> diag::Result<mir::Expr> {
            return LowerHirMemberAccessExprStructural(
                *this, frame, s, result_type);
          },
          [&](const hir::ConcatExpr& c) -> diag::Result<mir::Expr> {
            return LowerHirConcatExprStructural(*this, frame, c, result_type);
          },
          [](const hir::ReplicationExpr&) -> diag::Result<mir::Expr> {
            return diag::Unsupported(
                diag::DiagCode::kUnsupportedStructuralExpressionForm,
                "replication in constructor expressions is not yet supported",
                diag::UnsupportedCategory::kFeature);
          },
          [&](const hir::AssignmentPatternExpr& a) -> diag::Result<mir::Expr> {
            return LowerHirAssignmentPatternExprStructural(
                *this, frame, a, result_type);
          },
          [&](const hir::AssignmentPatternReplicationExpr& a)
              -> diag::Result<mir::Expr> {
            return LowerHirAssignmentPatternReplicationExprStructural(
                *this, frame, a, result_type);
          },
          [](const hir::DynamicArrayNewExpr&) -> diag::Result<mir::Expr> {
            return diag::Unsupported(
                diag::DiagCode::kUnsupportedStructuralExpressionForm,
                "dynamic-array new[] is not allowed in constructor "
                "expressions; LRM 7.5.1 restricts it to blocking assignments",
                diag::UnsupportedCategory::kFeature);
          },
      },
      expr.data);
}

}  // namespace lyra::lowering::hir_to_mir
