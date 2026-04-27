#include "lyra/lowering/hir_to_mir/lower_constructor.hpp"

#include <cstddef>
#include <cstdint>
#include <expected>
#include <format>
#include <optional>
#include <string>
#include <string_view>
#include <utility>
#include <vector>

#include "lyra/diag/diagnostic.hpp"
#include "lyra/hir/expr.hpp"
#include "lyra/hir/structural_scope.hpp"
#include "lyra/lowering/hir_to_mir/lower_expr.hpp"
#include "lyra/lowering/hir_to_mir/lower_process.hpp"
#include "lyra/lowering/hir_to_mir/state.hpp"
#include "lyra/mir/class_decl.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/member_var.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/support/internal_error.hpp"
#include "lyra/support/overloaded.hpp"

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
    const mir::ClassDecl& enclosing, std::string_view child_class_name,
    std::string_view companion_member_name) {
  for (const auto& m : enclosing.MemberVars()) {
    if (m.name == companion_member_name || m.name == child_class_name) {
      throw support::InternalError(
          "child class or companion member name collides with an existing "
          "member declaration in the enclosing class");
    }
  }
  for (const auto& c : enclosing.Classes()) {
    if (c.Name() == child_class_name) {
      throw support::InternalError(
          "child class name collides with an existing nested class "
          "declaration in the enclosing class");
    }
  }
}

struct GenerateChildSpec {
  hir::StructuralScopeId scope_id;
  const hir::StructuralScope* scope;
  std::string class_name;
};

auto EnumerateGenerateChildSpecs(
    const hir::Generate& gen, std::size_t gen_index)
    -> std::vector<GenerateChildSpec> {
  std::vector<GenerateChildSpec> specs;
  std::visit(
      support::Overloaded{
          [&](const hir::IfGenerate& if_gen) {
            const auto& then_scope = gen.GetChildScope(if_gen.then_scope);
            specs.push_back(
                {.scope_id = if_gen.then_scope,
                 .scope = &then_scope,
                 .class_name = ChildClassNameFor(gen_index, "then")});
            if (if_gen.else_scope.has_value()) {
              const auto& else_scope = gen.GetChildScope(*if_gen.else_scope);
              specs.push_back(
                  {.scope_id = *if_gen.else_scope,
                   .scope = &else_scope,
                   .class_name = ChildClassNameFor(gen_index, "else")});
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
                       ChildClassNameFor(gen_index, std::format("case{}", k))});
            }
            if (case_gen.default_scope.has_value()) {
              const auto& default_scope =
                  gen.GetChildScope(*case_gen.default_scope);
              specs.push_back(
                  {.scope_id = *case_gen.default_scope,
                   .scope = &default_scope,
                   .class_name = ChildClassNameFor(gen_index, "default")});
            }
          },
      },
      gen.data);
  return specs;
}

auto LowerStructuralExprFromScope(
    const hir::StructuralScope& owner_scope, hir::ExprId hir_id)
    -> diag::Result<mir::Expr> {
  const hir::Expr& src = owner_scope.GetExpr(hir_id);
  auto data = LowerStructuralExprData(src.data);
  if (!data) return std::unexpected(std::move(data.error()));
  return mir::Expr{.data = *std::move(data)};
}

void ValidateConstructMemberStmt(
    const mir::ClassDecl& enclosing_cls, const mir::ConstructMemberStmt& stmt) {
  if (stmt.class_id.value >= enclosing_cls.Classes().size()) {
    throw support::InternalError(
        "ConstructMemberStmt: class_id is not a direct child of the enclosing "
        "class");
  }
  if (stmt.target.value >= enclosing_cls.MemberVars().size()) {
    throw support::InternalError(
        "ConstructMemberStmt: target is out of range in the enclosing class");
  }
  const auto& member = enclosing_cls.GetMemberVar(stmt.target);
  const auto* child = std::get_if<mir::ChildClassMember>(&member.kind);
  if (child == nullptr) {
    throw support::InternalError(
        "ConstructMemberStmt: target is not a child-class member");
  }
  if (child->target != stmt.class_id) {
    throw support::InternalError(
        "ConstructMemberStmt: child-class member target does not match "
        "class_id");
  }
}

auto BuildGenerateArmBody(
    const GenerateBindings& gen_bindings, const mir::ClassDecl& enclosing_cls,
    hir::StructuralScopeId arm_scope_id) -> mir::Body {
  const auto& binding = gen_bindings.by_scope_id.at(arm_scope_id.value);
  const mir::ConstructMemberStmt construct_stmt{
      .target = binding.member_id, .class_id = binding.class_id};
  ValidateConstructMemberStmt(enclosing_cls, construct_stmt);

  mir::Body body;
  const mir::StmtId stmt_id{static_cast<std::uint32_t>(body.stmts.size())};
  body.stmts.push_back(
      mir::Stmt{
          .label = std::nullopt, .data = construct_stmt, .child_bodies = {}});
  body.root_stmts.push_back(stmt_id);
  return body;
}

auto AppendGenerateArmBody(
    const GenerateBindings& gen_bindings, const mir::ClassDecl& enclosing_cls,
    hir::StructuralScopeId arm_scope_id, std::vector<mir::Body>& child_bodies)
    -> mir::BodyId {
  const mir::BodyId id{static_cast<std::uint32_t>(child_bodies.size())};
  child_bodies.push_back(
      BuildGenerateArmBody(gen_bindings, enclosing_cls, arm_scope_id));
  return id;
}

auto LowerGenerateAsStmt(
    const hir::StructuralScope& enclosing_scope,
    const mir::ClassDecl& enclosing_cls, const hir::Generate& gen,
    const GenerateBindings& gen_bindings, BodyLoweringState& body_state)
    -> diag::Result<mir::Stmt> {
  return std::visit(
      support::Overloaded{
          [&](const hir::IfGenerate& if_gen) -> diag::Result<mir::Stmt> {
            auto cond_expr =
                LowerStructuralExprFromScope(enclosing_scope, if_gen.condition);
            if (!cond_expr) {
              return std::unexpected(std::move(cond_expr.error()));
            }
            const mir::ExprId cond_id =
                body_state.AppendExpr(if_gen.condition, *std::move(cond_expr));

            std::vector<mir::Body> child_bodies;
            const mir::BodyId then_id = AppendGenerateArmBody(
                gen_bindings, enclosing_cls, if_gen.then_scope, child_bodies);

            std::optional<mir::BodyId> else_id;
            if (if_gen.else_scope.has_value()) {
              else_id = AppendGenerateArmBody(
                  gen_bindings, enclosing_cls, *if_gen.else_scope,
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
            auto cond_expr = LowerStructuralExprFromScope(
                enclosing_scope, case_gen.condition);
            if (!cond_expr) {
              return std::unexpected(std::move(cond_expr.error()));
            }
            const mir::ExprId cond_id = body_state.AppendExpr(
                case_gen.condition, *std::move(cond_expr));

            std::vector<mir::Body> child_bodies;
            std::vector<mir::SwitchCase> cases;
            cases.reserve(case_gen.items.size());

            for (const auto& item : case_gen.items) {
              std::vector<mir::ExprId> labels;
              labels.reserve(item.labels.size());
              for (const hir::ExprId label_hir_id : item.labels) {
                auto label_expr =
                    LowerStructuralExprFromScope(enclosing_scope, label_hir_id);
                if (!label_expr) {
                  return std::unexpected(std::move(label_expr.error()));
                }
                labels.push_back(body_state.AppendExpr(
                    label_hir_id, *std::move(label_expr)));
              }
              const mir::BodyId item_body = AppendGenerateArmBody(
                  gen_bindings, enclosing_cls, item.scope, child_bodies);
              cases.push_back(
                  mir::SwitchCase{
                      .labels = std::move(labels), .body = item_body});
            }

            std::optional<mir::BodyId> default_body;
            if (case_gen.default_scope.has_value()) {
              default_body = AppendGenerateArmBody(
                  gen_bindings, enclosing_cls, *case_gen.default_scope,
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
      },
      gen.data);
}

auto LowerGenerateConstruction(
    const hir::StructuralScope& scope, const mir::ClassDecl& cls,
    const std::vector<GenerateBindings>& bindings_by_generate)
    -> diag::Result<mir::Body> {
  BodyLoweringState body_state;
  for (std::size_t i = 0; i < scope.Generates().size(); ++i) {
    const auto& gen = scope.Generates()[i];
    const auto& gen_bindings = bindings_by_generate.at(i);
    auto stmt = LowerGenerateAsStmt(scope, cls, gen, gen_bindings, body_state);
    if (!stmt) return std::unexpected(std::move(stmt.error()));
    const mir::StmtId sid = body_state.AppendStmt(*std::move(stmt));
    body_state.AppendRootStmt(sid);
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
      const mir::MemberVarId member_id = cls.AddMemberVar(
          companion_name, mir::ChildClassMember{.target = child_id});

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
  ClassLoweringState class_state(parent_class_state);
  const ScopeStackGuard guard(stack, scope);

  mir::ClassDecl cls(std::move(name));

  for (std::size_t i = 0; i < scope.MemberVars().size(); ++i) {
    const hir::MemberVarId hir_id{static_cast<std::uint32_t>(i)};
    const auto& d = scope.MemberVars()[i];
    const mir::MemberVarId mir_id = cls.AddMemberVar(
        d.name, mir::ValueMember{.type = unit_state.TranslateType(d.type)});
    class_state.BindMemberVar(hir_id, mir_id);
  }

  for (const auto& p : scope.Processes()) {
    cls.AddProcess(LowerProcess(unit_state, class_state, p));
  }

  auto bindings_r = InstallGenerateOwnedChildClasses(
      unit_state, class_state, stack, scope, cls);
  if (!bindings_r) return std::unexpected(std::move(bindings_r.error()));

  auto ctor_r = LowerGenerateConstruction(scope, cls, *bindings_r);
  if (!ctor_r) return std::unexpected(std::move(ctor_r.error()));
  cls.Constructor() = *std::move(ctor_r);

  return cls;
}

}  // namespace lyra::lowering::hir_to_mir
