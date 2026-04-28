#include "lyra/lowering/hir_to_mir/lower_constructor.hpp"

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
#include "lyra/diag/diagnostic.hpp"
#include "lyra/hir/expr.hpp"
#include "lyra/hir/structural_scope.hpp"
#include "lyra/lowering/hir_to_mir/lower_expr.hpp"
#include "lyra/lowering/hir_to_mir/lower_process.hpp"
#include "lyra/lowering/hir_to_mir/state.hpp"
#include "lyra/mir/class_decl.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/local_var.hpp"
#include "lyra/mir/member_var.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/mir/type.hpp"

namespace lyra::lowering::hir_to_mir {

namespace {

auto ChildClassNameFor(std::size_t gen_index, std::string_view arm_tag)
    -> std::string {
  return std::format("gen{}_{}", gen_index, arm_tag);
}

auto CompanionMemberNameFor(std::string_view child_class_name) -> std::string {
  return std::string(child_class_name) + "_obj";
}

void CheckNoNameCollision(
    const mir::ClassDecl& owner_class, std::string_view child_class_name,
    std::string_view companion_member_name) {
  for (const auto& m : owner_class.MemberVars()) {
    if (m.name == companion_member_name || m.name == child_class_name) {
      throw InternalError(
          "child class or companion member name collides with an existing "
          "member declaration in the enclosing class");
    }
  }
  for (const auto& c : owner_class.Classes()) {
    if (c.Name() == child_class_name) {
      throw InternalError(
          "child class name collides with an existing nested class "
          "declaration in the enclosing class");
    }
  }
}

struct GenerateChildSpec {
  hir::StructuralScopeId scope_id;
  const hir::StructuralScope* scope;
  std::string class_name;
  bool is_repeated;
};

auto EnumerateGenerateChildSpecs(
    const hir::Generate& gen, std::size_t gen_index)
    -> std::vector<GenerateChildSpec> {
  std::vector<GenerateChildSpec> specs;
  std::visit(
      Overloaded{
          [&](const hir::IfGenerate& if_gen) {
            const auto& then_scope = gen.GetChildScope(if_gen.then_scope);
            specs.push_back(
                {.scope_id = if_gen.then_scope,
                 .scope = &then_scope,
                 .class_name = ChildClassNameFor(gen_index, "then"),
                 .is_repeated = false});
            if (if_gen.else_scope.has_value()) {
              const auto& else_scope = gen.GetChildScope(*if_gen.else_scope);
              specs.push_back(
                  {.scope_id = *if_gen.else_scope,
                   .scope = &else_scope,
                   .class_name = ChildClassNameFor(gen_index, "else"),
                   .is_repeated = false});
            }
          },
          [&](const hir::CaseGenerate& case_gen) {
            for (std::size_t k = 0; k < case_gen.items.size(); ++k) {
              const auto& item_scope =
                  gen.GetChildScope(case_gen.items[k].scope);
              specs.push_back(
                  {.scope_id = case_gen.items[k].scope,
                   .scope = &item_scope,
                   .class_name =
                       ChildClassNameFor(gen_index, std::format("case{}", k)),
                   .is_repeated = false});
            }
            if (case_gen.default_scope.has_value()) {
              const auto& default_scope =
                  gen.GetChildScope(*case_gen.default_scope);
              specs.push_back(
                  {.scope_id = *case_gen.default_scope,
                   .scope = &default_scope,
                   .class_name = ChildClassNameFor(gen_index, "default"),
                   .is_repeated = false});
            }
          },
          [&](const hir::LoopGenerate& loop_gen) {
            const auto& body_scope = gen.GetChildScope(loop_gen.body_scope);
            specs.push_back(
                {.scope_id = loop_gen.body_scope,
                 .scope = &body_scope,
                 .class_name = ChildClassNameFor(gen_index, "body"),
                 .is_repeated = true});
          },
      },
      gen.data);
  return specs;
}

auto MakeOwnedObjectType(
    UnitLoweringState& unit_state, mir::ClassDeclId child_id) -> mir::TypeId {
  const mir::TypeId object_type =
      unit_state.AddType(mir::ObjectType{.target = child_id});
  return unit_state.AddType(mir::OwningPtrType{.pointee = object_type});
}

auto MakeVectorOfOwnedObjectType(
    UnitLoweringState& unit_state, mir::ClassDeclId child_id) -> mir::TypeId {
  const mir::TypeId owned_type = MakeOwnedObjectType(unit_state, child_id);
  return unit_state.AddType(mir::VectorType{.element = owned_type});
}

void ValidateConstructOwnedObjectStmt(
    const mir::CompilationUnit& unit, const mir::ClassDecl& owner_class,
    const mir::ConstructOwnedObjectStmt& stmt) {
  if (stmt.class_id.value >= owner_class.Classes().size()) {
    throw InternalError(
        "ConstructOwnedObjectStmt: class_id is not a direct child of the "
        "enclosing class");
  }
  if (stmt.target.value >= owner_class.MemberVars().size()) {
    throw InternalError(
        "ConstructOwnedObjectStmt: target is out of range in the enclosing "
        "class");
  }
  const auto& member = owner_class.GetMemberVar(stmt.target);
  const auto target = mir::GetOwnedObjectTarget(unit, member.type);
  if (!target.has_value() || *target != stmt.class_id) {
    throw InternalError(
        "ConstructOwnedObjectStmt: target member does not own the requested "
        "class");
  }
}

auto BuildGenerateArmBody(
    const mir::CompilationUnit& unit, const GenerateBindings& gen_bindings,
    const mir::ClassDecl& owner_class, hir::StructuralScopeId arm_scope_id)
    -> mir::Body {
  const auto& binding = gen_bindings.by_scope_id.at(arm_scope_id.value);
  const mir::ConstructOwnedObjectStmt construct_stmt{
      .target = binding.member_id, .class_id = binding.class_id};
  ValidateConstructOwnedObjectStmt(unit, owner_class, construct_stmt);

  BodyLoweringState body_state;
  const mir::StmtId stmt_id = body_state.AddStmt(
      mir::Stmt{
          .label = std::nullopt, .data = construct_stmt, .child_bodies = {}});
  body_state.AddRootStmt(stmt_id);
  return body_state.Finish();
}

auto AppendGenerateArmBody(
    const mir::CompilationUnit& unit, const GenerateBindings& gen_bindings,
    const mir::ClassDecl& owner_class, hir::StructuralScopeId arm_scope_id,
    std::vector<mir::Body>& child_bodies) -> mir::BodyId {
  const mir::BodyId id{static_cast<std::uint32_t>(child_bodies.size())};
  child_bodies.push_back(
      BuildGenerateArmBody(unit, gen_bindings, owner_class, arm_scope_id));
  return id;
}

auto LowerGenerateAsStmt(
    UnitLoweringState& unit_state, const ClassLoweringState& class_state,
    const hir::StructuralScope& enclosing_scope,
    const mir::ClassDecl& owner_class, const hir::Generate& gen,
    const GenerateBindings& gen_bindings, BodyLoweringState& body_state)
    -> diag::Result<mir::Stmt> {
  const mir::CompilationUnit& unit = unit_state.Unit();
  return std::visit(
      Overloaded{
          [&](const hir::IfGenerate& if_gen) -> diag::Result<mir::Stmt> {
            const ConstructorLoweringState ctor_state;
            auto cond_or = LowerExpr(
                unit_state, class_state, ctor_state, body_state,
                enclosing_scope, enclosing_scope.GetExpr(if_gen.condition));
            if (!cond_or) return std::unexpected(std::move(cond_or.error()));
            const mir::ExprId cond_id = body_state.AddExpr(*std::move(cond_or));

            std::vector<mir::Body> child_bodies;
            const mir::BodyId then_id = AppendGenerateArmBody(
                unit, gen_bindings, owner_class, if_gen.then_scope,
                child_bodies);

            std::optional<mir::BodyId> else_id;
            if (if_gen.else_scope.has_value()) {
              else_id = AppendGenerateArmBody(
                  unit, gen_bindings, owner_class, *if_gen.else_scope,
                  child_bodies);
            }

            return mir::Stmt{
                .label = std::nullopt,
                .data =
                    mir::IfStmt{
                        .condition = cond_id,
                        .then_body = then_id,
                        .else_body = else_id},
                .child_bodies = std::move(child_bodies)};
          },
          [&](const hir::CaseGenerate& case_gen) -> diag::Result<mir::Stmt> {
            const ConstructorLoweringState ctor_state;
            auto cond_or = LowerExpr(
                unit_state, class_state, ctor_state, body_state,
                enclosing_scope, enclosing_scope.GetExpr(case_gen.condition));
            if (!cond_or) return std::unexpected(std::move(cond_or.error()));
            const mir::ExprId cond_id = body_state.AddExpr(*std::move(cond_or));

            std::vector<mir::Body> child_bodies;
            std::vector<mir::SwitchCase> cases;
            cases.reserve(case_gen.items.size());

            for (const auto& item : case_gen.items) {
              std::vector<mir::ExprId> labels;
              labels.reserve(item.labels.size());
              for (const hir::ExprId label_hir_id : item.labels) {
                auto label_or = LowerExpr(
                    unit_state, class_state, ctor_state, body_state,
                    enclosing_scope, enclosing_scope.GetExpr(label_hir_id));
                if (!label_or) {
                  return std::unexpected(std::move(label_or.error()));
                }
                labels.push_back(body_state.AddExpr(*std::move(label_or)));
              }
              const mir::BodyId item_body = AppendGenerateArmBody(
                  unit, gen_bindings, owner_class, item.scope, child_bodies);
              cases.push_back(
                  mir::SwitchCase{
                      .labels = std::move(labels), .body = item_body});
            }

            std::optional<mir::BodyId> default_body;
            if (case_gen.default_scope.has_value()) {
              default_body = AppendGenerateArmBody(
                  unit, gen_bindings, owner_class, *case_gen.default_scope,
                  child_bodies);
            }

            return mir::Stmt{
                .label = std::nullopt,
                .data =
                    mir::SwitchStmt{
                        .condition = cond_id,
                        .cases = std::move(cases),
                        .default_body = default_body},
                .child_bodies = std::move(child_bodies)};
          },
          [&](const hir::LoopGenerate& loop) -> diag::Result<mir::Stmt> {
            const auto& var_decl =
                enclosing_scope.GetLoopVarDecl(loop.loop_var);
            const mir::TypeId genvar_type =
                unit_state.TranslateType(var_decl.type);

            const mir::LocalScopeId for_scope =
                body_state.AddLocalScope(body_state.RootScope());
            const mir::LocalVarId loop_local_id = body_state.AddLocal(
                for_scope,
                mir::LocalVar{.name = var_decl.name, .type = genvar_type});
            const mir::LocalVarRef loop_local{
                .scope = for_scope, .local = loop_local_id};

            ConstructorLoweringState ctor_state;
            ctor_state.MapLoopVar(loop.loop_var, loop_local);

            auto init_or = LowerExpr(
                unit_state, class_state, ctor_state, body_state,
                enclosing_scope, enclosing_scope.GetExpr(loop.initial));
            if (!init_or) return std::unexpected(std::move(init_or.error()));
            const mir::ExprId init_id = body_state.AddExpr(*std::move(init_or));

            auto cond_or = LowerExpr(
                unit_state, class_state, ctor_state, body_state,
                enclosing_scope, enclosing_scope.GetExpr(loop.stop));
            if (!cond_or) return std::unexpected(std::move(cond_or.error()));
            const mir::ExprId cond_id = body_state.AddExpr(*std::move(cond_or));

            auto step_or = LowerExpr(
                unit_state, class_state, ctor_state, body_state,
                enclosing_scope, enclosing_scope.GetExpr(loop.iter));
            if (!step_or) return std::unexpected(std::move(step_or.error()));
            const mir::ExprId step_id = body_state.AddExpr(*std::move(step_or));

            std::vector<mir::Body> child_bodies;
            const mir::BodyId body_id = AppendGenerateArmBody(
                unit, gen_bindings, owner_class, loop.body_scope, child_bodies);

            return mir::Stmt{
                .label = std::nullopt,
                .data =
                    mir::ForStmt{
                        .scope = for_scope,
                        .init = {mir::ForInitDecl{
                            .local = loop_local, .init = init_id}},
                        .condition = cond_id,
                        .step = {step_id},
                        .body = body_id},
                .child_bodies = std::move(child_bodies)};
          },
      },
      gen.data);
}

auto LowerGenerateConstruction(
    UnitLoweringState& unit_state, const ClassLoweringState& class_state,
    const hir::StructuralScope& scope, const mir::ClassDecl& cls,
    const std::vector<GenerateBindings>& bindings_by_generate)
    -> diag::Result<mir::Body> {
  BodyLoweringState body_state;
  for (std::size_t i = 0; i < scope.Generates().size(); ++i) {
    const auto& gen = scope.Generates()[i];
    const auto& gen_bindings = bindings_by_generate.at(i);
    auto stmt = LowerGenerateAsStmt(
        unit_state, class_state, scope, cls, gen, gen_bindings, body_state);
    if (!stmt) return std::unexpected(std::move(stmt.error()));
    const mir::StmtId sid = body_state.AddStmt(*std::move(stmt));
    body_state.AddRootStmt(sid);
  }
  return body_state.Finish();
}

auto InstallGenerateOwnedChildClasses(
    UnitLoweringState& unit_state, const ClassLoweringState& class_state,
    ScopeStack& stack, const hir::StructuralScope& scope, mir::ClassDecl& cls)
    -> diag::Result<std::vector<GenerateBindings>> {
  std::vector<GenerateBindings> bindings_by_generate;
  bindings_by_generate.reserve(scope.Generates().size());

  for (std::size_t gen_idx = 0; gen_idx < scope.Generates().size(); ++gen_idx) {
    const auto& gen = scope.Generates()[gen_idx];
    GenerateBindings gen_bindings;
    gen_bindings.by_scope_id.resize(gen.child_scopes.size());

    auto specs = EnumerateGenerateChildSpecs(gen, gen_idx);
    for (auto& spec : specs) {
      const auto companion_name = CompanionMemberNameFor(spec.class_name);
      CheckNoNameCollision(cls, spec.class_name, companion_name);

      auto child_r = LowerScopeAsClass(
          unit_state, &class_state, stack, *spec.scope,
          std::move(spec.class_name));
      if (!child_r) return std::unexpected(std::move(child_r.error()));

      const mir::ClassDeclId child_id = cls.AddClass(*std::move(child_r));
      const mir::TypeId member_type =
          spec.is_repeated ? MakeVectorOfOwnedObjectType(unit_state, child_id)
                           : MakeOwnedObjectType(unit_state, child_id);
      const mir::MemberVarId member_id = cls.AddMemberVar(
          mir::MemberVar{.name = companion_name, .type = member_type});

      gen_bindings.by_scope_id.at(spec.scope_id.value) =
          ChildClassBinding{.class_id = child_id, .member_id = member_id};
    }

    bindings_by_generate.push_back(std::move(gen_bindings));
  }
  return bindings_by_generate;
}

}  // namespace

auto LowerScopeAsClass(
    UnitLoweringState& unit_state, const ClassLoweringState* parent_class_state,
    ScopeStack& stack, const hir::StructuralScope& scope, std::string name)
    -> diag::Result<mir::ClassDecl> {
  mir::ClassDecl cls(std::move(name));
  ClassLoweringState class_state(parent_class_state, cls);
  const ScopeStackGuard guard(stack, scope);

  for (std::size_t i = 0; i < scope.MemberVars().size(); ++i) {
    const hir::MemberVarId hir_id{static_cast<std::uint32_t>(i)};
    const auto& d = scope.MemberVars()[i];
    const mir::MemberVarId mir_id = cls.AddMemberVar(
        mir::MemberVar{
            .name = d.name, .type = unit_state.TranslateType(d.type)});
    class_state.BindMemberVar(hir_id, mir_id);
  }

  for (std::size_t i = 0; i < scope.Subroutines().size(); ++i) {
    const hir::SubroutineId hir_id{static_cast<std::uint32_t>(i)};
    const auto& decl = scope.Subroutines()[i];
    const mir::UserSubroutineTargetId mir_id = cls.AddUserSubroutineTarget(
        mir::UserSubroutineTarget{.name = decl.name});
    class_state.BindUserSubroutine(hir_id, mir_id);
  }

  for (const auto& p : scope.Processes()) {
    auto proc_or = LowerProcess(unit_state, class_state, p);
    if (!proc_or) return std::unexpected(std::move(proc_or.error()));
    cls.AddProcess(*std::move(proc_or));
  }

  auto bindings_r = InstallGenerateOwnedChildClasses(
      unit_state, class_state, stack, scope, cls);
  if (!bindings_r) return std::unexpected(std::move(bindings_r.error()));

  auto ctor_r = LowerGenerateConstruction(
      unit_state, class_state, scope, cls, *bindings_r);
  if (!ctor_r) return std::unexpected(std::move(ctor_r.error()));
  cls.Constructor() = *std::move(ctor_r);

  return cls;
}

}  // namespace lyra::lowering::hir_to_mir
