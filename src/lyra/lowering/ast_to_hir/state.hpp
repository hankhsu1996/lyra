#pragma once

#include <compare>
#include <cstdint>
#include <optional>
#include <ranges>
#include <string>
#include <unordered_map>
#include <utility>
#include <vector>

#include <slang/ast/symbols/SubroutineSymbols.h>
#include <slang/ast/symbols/VariableSymbols.h>

#include "lyra/base/internal_error.hpp"
#include "lyra/hir/expr.hpp"
#include "lyra/hir/local_var.hpp"
#include "lyra/hir/member_var.hpp"
#include "lyra/hir/module_unit.hpp"
#include "lyra/hir/process.hpp"
#include "lyra/hir/stmt.hpp"
#include "lyra/hir/structural_scope.hpp"
#include "lyra/hir/subroutine.hpp"
#include "lyra/hir/type.hpp"

namespace lyra::lowering::ast_to_hir {

// Lowering-only identity for a scope on the stack. Monotonically assigned
// by ScopeStack; never reused; never stored in HIR.
struct ScopeFrameId {
  std::uint32_t value;

  auto operator<=>(const ScopeFrameId&) const -> std::strong_ordering = default;
};

struct MemberVarBinding {
  ScopeFrameId home_frame;
  hir::MemberVarId local_id;
};

using MemberVarBindings =
    std::unordered_map<const slang::ast::VariableSymbol*, MemberVarBinding>;

struct SubroutineBinding {
  ScopeFrameId owner_frame;
  hir::SubroutineId local_id;
};

using SubroutineBindings =
    std::unordered_map<const slang::ast::SubroutineSymbol*, SubroutineBinding>;

class UnitLoweringState {
 public:
  explicit UnitLoweringState(std::string name) : hir_unit_(std::move(name)) {
  }

  [[nodiscard]] auto HirUnit() const -> const hir::ModuleUnit& {
    return hir_unit_;
  }

  auto HirUnit() -> hir::ModuleUnit& {
    return hir_unit_;
  }

  auto AddType(hir::TypeData data) -> hir::TypeId {
    return hir_unit_.AddType(std::move(data));
  }

  [[nodiscard]] auto GetType(hir::TypeId id) const -> const hir::Type& {
    return hir_unit_.GetType(id);
  }

  void RegisterMemberVarBinding(
      const slang::ast::VariableSymbol& var, ScopeFrameId home_frame,
      hir::MemberVarId local) {
    member_var_bindings_.emplace(
        &var, MemberVarBinding{.home_frame = home_frame, .local_id = local});
  }

  [[nodiscard]] auto LookupMemberVarBinding(
      const slang::ast::VariableSymbol& var) const
      -> std::optional<MemberVarBinding> {
    const auto it = member_var_bindings_.find(&var);
    if (it == member_var_bindings_.end()) {
      return std::nullopt;
    }
    return it->second;
  }

  void RegisterSubroutineBinding(
      const slang::ast::SubroutineSymbol& sym, ScopeFrameId owner_frame,
      hir::SubroutineId local) {
    subroutine_bindings_.emplace(
        &sym, SubroutineBinding{.owner_frame = owner_frame, .local_id = local});
  }

  [[nodiscard]] auto LookupSubroutineBinding(
      const slang::ast::SubroutineSymbol& sym) const
      -> std::optional<SubroutineBinding> {
    const auto it = subroutine_bindings_.find(&sym);
    if (it == subroutine_bindings_.end()) {
      return std::nullopt;
    }
    return it->second;
  }

  auto MoveHirUnit() -> hir::ModuleUnit {
    return std::move(hir_unit_);
  }

 private:
  hir::ModuleUnit hir_unit_;
  MemberVarBindings member_var_bindings_;
  SubroutineBindings subroutine_bindings_;
};

class ScopeStack {
 public:
  auto Push() -> ScopeFrameId {
    const ScopeFrameId id{.value = next_id_++};
    frames_.push_back(id);
    return id;
  }

  void Pop(ScopeFrameId expected) {
    if (frames_.empty() || frames_.back() != expected) {
      throw InternalError("ScopeStack::Pop: unbalanced pop");
    }
    frames_.pop_back();
  }

  [[nodiscard]] auto HopsTo(ScopeFrameId target) const
      -> std::optional<hir::ParentScopeHops> {
    std::uint32_t hops = 0;
    for (const auto frame : frames_ | std::views::reverse) {
      if (frame == target) {
        return hir::ParentScopeHops{.value = hops};
      }
      ++hops;
    }
    return std::nullopt;
  }

 private:
  std::vector<ScopeFrameId> frames_;
  std::uint32_t next_id_ = 0;
};

class ScopeStackGuard {
 public:
  explicit ScopeStackGuard(ScopeStack& stack)
      : stack_(&stack), frame_(stack.Push()) {
  }

  ~ScopeStackGuard() {
    stack_->Pop(frame_);
  }

  ScopeStackGuard(const ScopeStackGuard&) = delete;
  auto operator=(const ScopeStackGuard&) -> ScopeStackGuard& = delete;
  ScopeStackGuard(ScopeStackGuard&&) = delete;
  auto operator=(ScopeStackGuard&&) -> ScopeStackGuard& = delete;

  [[nodiscard]] auto Frame() const -> ScopeFrameId {
    return frame_;
  }

 private:
  ScopeStack* stack_;
  ScopeFrameId frame_;
};

class ScopeLoweringState {
 public:
  ScopeLoweringState(
      UnitLoweringState& unit_state, hir::StructuralScope& scope,
      ScopeFrameId frame)
      : unit_state_(&unit_state), scope_(&scope), frame_(frame) {
  }

  auto AddMemberVar(const slang::ast::VariableSymbol& var, hir::TypeId type)
      -> hir::MemberVarId {
    const auto local = scope_->AddMemberVar(
        hir::MemberVar{.name = std::string{var.name}, .type = type});
    unit_state_->RegisterMemberVarBinding(var, frame_, local);
    return local;
  }

  auto AddExpr(hir::Expr expr) -> hir::ExprId {
    return scope_->AddExpr(std::move(expr));
  }

  auto AddProcess(hir::Process process) -> hir::ProcessId {
    return scope_->AddProcess(std::move(process));
  }

  auto AddGenerate(hir::Generate generate) -> hir::GenerateId {
    return scope_->AddGenerate(std::move(generate));
  }

  auto AddSubroutine(
      const slang::ast::SubroutineSymbol& sym, hir::UserSubroutineDecl decl)
      -> hir::SubroutineId {
    const auto local = scope_->AddSubroutine(std::move(decl));
    unit_state_->RegisterSubroutineBinding(sym, frame_, local);
    return local;
  }

  [[nodiscard]] auto UnitState() -> UnitLoweringState& {
    return *unit_state_;
  }
  [[nodiscard]] auto UnitState() const -> const UnitLoweringState& {
    return *unit_state_;
  }
  [[nodiscard]] auto Scope() -> hir::StructuralScope& {
    return *scope_;
  }
  [[nodiscard]] auto Scope() const -> const hir::StructuralScope& {
    return *scope_;
  }
  [[nodiscard]] auto Frame() const -> ScopeFrameId {
    return frame_;
  }

 private:
  UnitLoweringState* unit_state_;
  hir::StructuralScope* scope_;
  ScopeFrameId frame_;
};

class ProcessLoweringState {
 public:
  auto AddExpr(hir::Expr expr) -> hir::ExprId {
    const hir::ExprId id{static_cast<std::uint32_t>(hir_process_.exprs.size())};
    hir_process_.exprs.push_back(std::move(expr));
    return id;
  }

  auto AddStmt(hir::Stmt stmt) -> hir::StmtId {
    const hir::StmtId id{static_cast<std::uint32_t>(hir_process_.stmts.size())};
    hir_process_.stmts.push_back(std::move(stmt));
    return id;
  }

  auto AddLocalVar(const slang::ast::VariableSymbol& var, hir::TypeId type)
      -> hir::LocalVarId {
    const hir::LocalVarId id{
        static_cast<std::uint32_t>(hir_process_.local_vars.size())};
    hir_process_.local_vars.push_back(
        hir::LocalVar{.name = std::string{var.name}, .type = type});
    local_var_bindings_.emplace(&var, id);
    return id;
  }

  [[nodiscard]] auto LookupLocalVar(const slang::ast::VariableSymbol& var) const
      -> std::optional<hir::LocalVarId> {
    const auto it = local_var_bindings_.find(&var);
    if (it == local_var_bindings_.end()) {
      return std::nullopt;
    }
    return it->second;
  }

  auto Finalize(hir::ProcessKind kind, hir::StmtId body) -> hir::Process {
    hir_process_.kind = kind;
    hir_process_.body = body;
    return std::move(hir_process_);
  }

 private:
  hir::Process hir_process_;
  std::unordered_map<const slang::ast::VariableSymbol*, hir::LocalVarId>
      local_var_bindings_;
};

}  // namespace lyra::lowering::ast_to_hir
