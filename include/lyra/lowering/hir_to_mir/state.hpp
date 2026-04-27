#pragma once

#include <cstdint>
#include <cstdlib>
#include <utility>
#include <vector>

#include "lyra/hir/expr.hpp"
#include "lyra/hir/local_var.hpp"
#include "lyra/hir/member_var.hpp"
#include "lyra/hir/structural_scope.hpp"
#include "lyra/hir/subroutine.hpp"
#include "lyra/hir/type.hpp"
#include "lyra/mir/class_decl_id.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/local_var.hpp"
#include "lyra/mir/member_var.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/mir/type.hpp"
#include "lyra/support/internal_error.hpp"

namespace lyra::lowering::hir_to_mir {

class UnitLoweringState {
 public:
  explicit UnitLoweringState(mir::CompilationUnit& unit) : unit_(&unit) {
  }

  auto AddType(mir::TypeData data) -> mir::TypeId {
    return unit_->AddType(std::move(data));
  }

  [[nodiscard]] auto GetType(mir::TypeId id) const -> const mir::Type& {
    return unit_->GetType(id);
  }

  [[nodiscard]] auto TranslateType(hir::TypeId hir_id) const -> mir::TypeId {
    return type_map_.at(hir_id.value);
  }

  void RegisterTypeMapping(hir::TypeId hir_id, mir::TypeId mir_id) {
    if (hir_id.value >= type_map_.size()) {
      type_map_.resize(hir_id.value + 1);
    }
    type_map_[hir_id.value] = mir_id;
  }

 private:
  mir::CompilationUnit* unit_;
  std::vector<mir::TypeId> type_map_;
};

struct ChildClassBinding {
  mir::ClassDeclId class_id;
  mir::MemberVarId member_id;
};

// Bindings produced by `InstallGenerateOwnedChildClasses` for a single
// `hir::Generate`. Indexed by `hir::StructuralScopeId.value`. Phase-local to
// constructor lowering: `LowerGenerateConstruction` consumes them and they
// then go out of scope. They are not part of `ClassLoweringState`.
struct GenerateBindings {
  std::vector<ChildClassBinding> by_scope_id;
};

class ClassLoweringState {
 public:
  explicit ClassLoweringState(const ClassLoweringState* parent)
      : parent_(parent) {
  }

  [[nodiscard]] auto Parent() const -> const ClassLoweringState* {
    return parent_;
  }

  void BindMemberVar(hir::MemberVarId hir_id, mir::MemberVarId mir_id) {
    if (hir_id.value >= member_var_map_.size()) {
      member_var_map_.resize(hir_id.value + 1);
    }
    member_var_map_[hir_id.value] = mir_id;
  }

  // Walk the class chain `hops` levels outward and look up `hir_id` in that
  // class's member-var map. The class chain mirrors the lexical scope chain
  // exactly, so `parent_scope_hops` from a HIR ref maps directly to chain
  // depth.
  [[nodiscard]] auto LookupMemberVar(
      hir::ParentScopeHops hops, hir::MemberVarId hir_id) const
      -> mir::MemberVarId {
    if (hops.value == 0) {
      return member_var_map_.at(hir_id.value);
    }
    if (parent_ == nullptr) {
      throw support::InternalError(
          "ClassLoweringState::LookupMemberVar: hops out of class chain");
    }
    return parent_->LookupMemberVar(
        hir::ParentScopeHops{hops.value - 1}, hir_id);
  }

  void BindUserSubroutine(
      hir::SubroutineId hir_id, mir::UserSubroutineTargetId mir_id) {
    if (hir_id.value >= user_subroutine_map_.size()) {
      user_subroutine_map_.resize(hir_id.value + 1);
    }
    user_subroutine_map_[hir_id.value] = mir_id;
  }

  [[nodiscard]] auto LookupUserSubroutine(
      hir::ParentScopeHops hops, hir::SubroutineId hir_id) const
      -> mir::UserSubroutineTargetId {
    if (hops.value == 0) {
      return user_subroutine_map_.at(hir_id.value);
    }
    if (parent_ == nullptr) {
      throw support::InternalError(
          "ClassLoweringState::LookupUserSubroutine: hops out of class chain");
    }
    return parent_->LookupUserSubroutine(
        hir::ParentScopeHops{hops.value - 1}, hir_id);
  }

 private:
  const ClassLoweringState* parent_;
  std::vector<mir::MemberVarId> member_var_map_;
  std::vector<mir::UserSubroutineTargetId> user_subroutine_map_;
};

class ProcessLoweringState {
 public:
  auto AddLocalVar(mir::LocalVar lv) -> mir::LocalVarId {
    const mir::LocalVarId id{static_cast<std::uint32_t>(locals_.size())};
    locals_.push_back(std::move(lv));
    return id;
  }

  void MapLocalVar(hir::LocalVarId hir_id, mir::LocalVarId mir_id) {
    if (hir_id.value >= map_.size()) {
      map_.resize(hir_id.value + 1);
    }
    map_[hir_id.value] = mir_id;
  }

  [[nodiscard]] auto TranslateLocalVar(hir::LocalVarId hir_id) const
      -> mir::LocalVarId {
    return map_.at(hir_id.value);
  }

  auto MoveLocals() -> std::vector<mir::LocalVar> {
    return std::move(locals_);
  }

 private:
  std::vector<mir::LocalVar> locals_;
  std::vector<mir::LocalVarId> map_;
};

class BodyLoweringState {
 public:
  BodyLoweringState() = default;

  auto AppendExpr(hir::ExprId hir_id, mir::Expr expr) -> mir::ExprId {
    const mir::ExprId id{static_cast<std::uint32_t>(body_.exprs.size())};
    body_.exprs.push_back(std::move(expr));
    if (hir_id.value >= expr_map_.size()) {
      expr_map_.resize(hir_id.value + 1);
    }
    expr_map_[hir_id.value] = id;
    return id;
  }

  [[nodiscard]] auto TranslateExpr(hir::ExprId hir_id) const -> mir::ExprId {
    return expr_map_.at(hir_id.value);
  }

  auto AppendStmt(mir::Stmt stmt) -> mir::StmtId {
    const mir::StmtId id{static_cast<std::uint32_t>(body_.stmts.size())};
    body_.stmts.push_back(std::move(stmt));
    return id;
  }

  void AppendRootStmt(mir::StmtId id) {
    body_.root_stmts.push_back(id);
  }

  auto Finish() -> mir::Body {
    return std::move(body_);
  }

 private:
  mir::Body body_;
  std::vector<mir::ExprId> expr_map_;
};

class ScopeStack {
 public:
  auto Push(const hir::StructuralScope& scope) -> void {
    frames_.push_back(&scope);
  }

  auto Pop(const hir::StructuralScope& expected) noexcept -> void {
    if (frames_.empty() || frames_.back() != &expected) {
      std::abort();
    }
    frames_.pop_back();
  }

  [[nodiscard]] auto Resolve(hir::ParentScopeHops hops) const
      -> const hir::StructuralScope& {
    if (hops.value >= frames_.size()) {
      throw support::InternalError("ScopeStack::Resolve: hops out of range");
    }
    return *frames_[frames_.size() - 1 - hops.value];
  }

  [[nodiscard]] auto Depth() const -> std::uint32_t {
    return static_cast<std::uint32_t>(frames_.size());
  }

 private:
  std::vector<const hir::StructuralScope*> frames_;
};

class ScopeStackGuard {
 public:
  ScopeStackGuard(ScopeStack& stack, const hir::StructuralScope& scope)
      : stack_(&stack), scope_(&scope) {
    stack_->Push(scope);
  }

  ~ScopeStackGuard() noexcept {
    stack_->Pop(*scope_);
  }

  ScopeStackGuard(const ScopeStackGuard&) = delete;
  auto operator=(const ScopeStackGuard&) -> ScopeStackGuard& = delete;
  ScopeStackGuard(ScopeStackGuard&&) = delete;
  auto operator=(ScopeStackGuard&&) -> ScopeStackGuard& = delete;

 private:
  ScopeStack* stack_;
  const hir::StructuralScope* scope_;
};

}  // namespace lyra::lowering::hir_to_mir
