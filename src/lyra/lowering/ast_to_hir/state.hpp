#pragma once

#include <compare>
#include <cstdint>
#include <optional>
#include <ranges>
#include <string>
#include <unordered_map>
#include <utility>
#include <vector>

#include <slang/ast/symbols/VariableSymbols.h>

#include "lyra/hir/expr.hpp"
#include "lyra/hir/module_unit.hpp"
#include "lyra/hir/process.hpp"
#include "lyra/hir/stmt.hpp"
#include "lyra/hir/structural_scope.hpp"
#include "lyra/hir/type.hpp"
#include "lyra/hir/value_decl_ref.hpp"
#include "lyra/hir/var_decl.hpp"
#include "lyra/support/internal_error.hpp"

namespace lyra::lowering::ast_to_hir {

// Lowering-only identity for a scope on the stack. Monotonically assigned
// by ScopeStack; never reused; never stored in HIR.
struct ScopeFrameId {
  std::uint32_t value;

  auto operator<=>(const ScopeFrameId&) const -> std::strong_ordering = default;
};

struct VarBinding {
  ScopeFrameId home_frame;
  hir::VarDeclId local_id;
};

using VariableDeclBindings =
    std::unordered_map<const slang::ast::VariableSymbol*, VarBinding>;

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

  void RegisterVarBinding(
      const slang::ast::VariableSymbol& var, ScopeFrameId home_frame,
      hir::VarDeclId local) {
    var_bindings_.emplace(
        &var, VarBinding{.home_frame = home_frame, .local_id = local});
  }

  [[nodiscard]] auto LookupVarBinding(const slang::ast::VariableSymbol& var)
      const -> std::optional<VarBinding> {
    const auto it = var_bindings_.find(&var);
    if (it == var_bindings_.end()) {
      return std::nullopt;
    }
    return it->second;
  }

  auto MoveHirUnit() -> hir::ModuleUnit {
    return std::move(hir_unit_);
  }

 private:
  hir::ModuleUnit hir_unit_;
  VariableDeclBindings var_bindings_;
};

class ScopeStack {
 public:
  auto Push(hir::StructuralScope& /*scope*/) -> ScopeFrameId {
    const ScopeFrameId id{.value = next_id_++};
    frames_.push_back(id);
    return id;
  }

  void Pop(ScopeFrameId expected) {
    if (frames_.empty() || frames_.back() != expected) {
      throw support::InternalError("ScopeStack::Pop: unbalanced pop");
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
  ScopeStackGuard(ScopeStack& stack, hir::StructuralScope& scope)
      : stack_(&stack), frame_(stack.Push(scope)) {
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
      UnitLoweringState& unit, hir::StructuralScope& scope, ScopeFrameId frame)
      : unit_(&unit), scope_(&scope), frame_(frame) {
  }

  auto AddVarDecl(const slang::ast::VariableSymbol& var, hir::TypeId type)
      -> hir::VarDeclId {
    const auto local = scope_->AddVarDecl(std::string{var.name}, type);
    unit_->RegisterVarBinding(var, frame_, local);
    return local;
  }

  auto AppendExpr(hir::Expr expr) -> hir::ExprId {
    return scope_->AppendExpr(std::move(expr));
  }

  auto AddProcess(hir::Process process) -> hir::ProcessId {
    return scope_->AddProcess(std::move(process));
  }

  auto AddGenerate(hir::Generate generate) -> hir::GenerateId {
    return scope_->AddGenerate(std::move(generate));
  }

  [[nodiscard]] auto Unit() -> UnitLoweringState& {
    return *unit_;
  }
  [[nodiscard]] auto Unit() const -> const UnitLoweringState& {
    return *unit_;
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
  UnitLoweringState* unit_;
  hir::StructuralScope* scope_;
  ScopeFrameId frame_;
};

class ProcessLoweringState {
 public:
  auto AppendExpr(hir::Expr expr) -> hir::ExprId {
    const hir::ExprId id{static_cast<std::uint32_t>(hir_process_.exprs.size())};
    hir_process_.exprs.push_back(std::move(expr));
    return id;
  }

  auto AppendStmt(hir::Stmt stmt) -> hir::StmtId {
    const hir::StmtId id{static_cast<std::uint32_t>(hir_process_.stmts.size())};
    hir_process_.stmts.push_back(std::move(stmt));
    return id;
  }

  auto Finalize(hir::ProcessData data) -> hir::Process {
    hir_process_.data = std::move(data);
    return std::move(hir_process_);
  }

 private:
  hir::Process hir_process_;
};

}  // namespace lyra::lowering::ast_to_hir
