#pragma once

#include <cstdint>
#include <cstdlib>
#include <utility>
#include <vector>

#include "lyra/hir/expr.hpp"
#include "lyra/hir/structural_scope.hpp"
#include "lyra/hir/value_decl_ref.hpp"
#include "lyra/hir/var_decl.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/member.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/mir/type.hpp"
#include "lyra/support/internal_error.hpp"

namespace lyra::lowering::hir_to_mir {

struct UnitLoweringState {
  std::vector<mir::TypeId> type_map;
  std::vector<mir::MemberId> root_var_map;

  [[nodiscard]] auto TranslateType(hir::TypeId hir_id) const -> mir::TypeId {
    return type_map.at(hir_id.value);
  }

  [[nodiscard]] auto TranslateRootVar(hir::VarDeclId hir_id) const
      -> mir::MemberId {
    return root_var_map.at(hir_id.value);
  }

  void SetType(hir::TypeId hir_id, mir::TypeId mir_id) {
    if (hir_id.value >= type_map.size()) {
      type_map.resize(hir_id.value + 1);
    }
    type_map[hir_id.value] = mir_id;
  }

  void SetRootVar(hir::VarDeclId hir_id, mir::MemberId mir_id) {
    if (hir_id.value >= root_var_map.size()) {
      root_var_map.resize(hir_id.value + 1);
    }
    root_var_map[hir_id.value] = mir_id;
  }
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

// Lowering-time lexical chain. Each entry is a pointer to an immutable
// hir::StructuralScope. HIR is fully built before lowering starts, so
// addresses are stable for the duration.
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
