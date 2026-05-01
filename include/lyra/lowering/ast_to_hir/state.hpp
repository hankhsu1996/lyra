#pragma once

#include <compare>
#include <cstdint>
#include <optional>
#include <ranges>
#include <string>
#include <unordered_map>
#include <utility>
#include <vector>

#include <slang/ast/symbols/ParameterSymbols.h>
#include <slang/ast/symbols/SubroutineSymbols.h>
#include <slang/ast/symbols/ValueSymbol.h>
#include <slang/ast/symbols/VariableSymbols.h>

#include "lyra/base/internal_error.hpp"
#include "lyra/diag/source_span.hpp"
#include "lyra/hir/expr.hpp"
#include "lyra/hir/module_unit.hpp"
#include "lyra/hir/procedural_var.hpp"
#include "lyra/hir/process.hpp"
#include "lyra/hir/stmt.hpp"
#include "lyra/hir/structural_hops.hpp"
#include "lyra/hir/structural_scope.hpp"
#include "lyra/hir/structural_var.hpp"
#include "lyra/hir/subroutine.hpp"
#include "lyra/hir/type.hpp"

namespace lyra::lowering::ast_to_hir {

// Lowering-only identity for a scope on the stack. Monotonically assigned
// by ScopeStack; never reused; never stored in HIR.
struct ScopeFrameId {
  std::uint32_t value;

  auto operator<=>(const ScopeFrameId&) const -> std::strong_ordering = default;
};

struct StructuralVarBinding {
  ScopeFrameId home_frame;
  hir::StructuralVarId var_id;
  hir::TypeId type;
};

using StructuralVarBindings =
    std::unordered_map<const slang::ast::VariableSymbol*, StructuralVarBinding>;

struct SubroutineBinding {
  ScopeFrameId owner_frame;
  hir::StructuralSubroutineId subroutine_id;
};

using SubroutineBindings =
    std::unordered_map<const slang::ast::SubroutineSymbol*, SubroutineBinding>;

struct LoopVarBinding {
  ScopeFrameId home_frame;
  hir::LoopVarDeclId loop_var_id;
  hir::TypeId type;
};

using LoopVarBindings =
    std::unordered_map<const slang::ast::ValueSymbol*, LoopVarBinding>;

class UnitLoweringState {
 public:
  explicit UnitLoweringState(std::string name)
      : hir_unit_{.name = std::move(name), .types = {}, .root_scope = {}} {
  }

  [[nodiscard]] auto HirUnit() const -> const hir::ModuleUnit& {
    return hir_unit_;
  }

  auto HirUnit() -> hir::ModuleUnit& {
    return hir_unit_;
  }

  auto AddType(hir::TypeData data) -> hir::TypeId {
    const hir::TypeId id{static_cast<std::uint32_t>(hir_unit_.types.size())};
    hir_unit_.types.push_back(hir::Type{.data = std::move(data)});
    return id;
  }

  [[nodiscard]] auto GetType(hir::TypeId id) const -> const hir::Type& {
    return hir_unit_.GetType(id);
  }

  void MapStructuralVarBinding(
      const slang::ast::VariableSymbol& var, ScopeFrameId home_frame,
      hir::StructuralVarId local, hir::TypeId type) {
    const auto [_, inserted] = structural_var_bindings_.emplace(
        &var, StructuralVarBinding{
                  .home_frame = home_frame, .var_id = local, .type = type});
    if (!inserted) {
      throw InternalError(
          "UnitLoweringState::MapStructuralVarBinding: structural variable "
          "symbol already mapped");
    }
  }

  [[nodiscard]] auto LookupStructuralVarBinding(
      const slang::ast::VariableSymbol& var) const
      -> std::optional<StructuralVarBinding> {
    const auto it = structural_var_bindings_.find(&var);
    if (it == structural_var_bindings_.end()) {
      return std::nullopt;
    }
    return it->second;
  }

  void MapSubroutineBinding(
      const slang::ast::SubroutineSymbol& sym, ScopeFrameId owner_frame,
      hir::StructuralSubroutineId local) {
    const auto [_, inserted] = subroutine_bindings_.emplace(
        &sym,
        SubroutineBinding{.owner_frame = owner_frame, .subroutine_id = local});
    if (!inserted) {
      throw InternalError(
          "UnitLoweringState::MapSubroutineBinding: subroutine symbol already "
          "mapped");
    }
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

  void MapLoopVarBinding(
      const slang::ast::ValueSymbol& sym, ScopeFrameId home_frame,
      hir::LoopVarDeclId id, hir::TypeId type) {
    const auto [_, inserted] = loop_var_bindings_.emplace(
        &sym, LoopVarBinding{
                  .home_frame = home_frame, .loop_var_id = id, .type = type});
    if (!inserted) {
      throw InternalError(
          "UnitLoweringState::MapLoopVarBinding: loop variable symbol already "
          "mapped");
    }
  }

  [[nodiscard]] auto LookupLoopVarBinding(const slang::ast::ValueSymbol& sym)
      const -> std::optional<LoopVarBinding> {
    const auto it = loop_var_bindings_.find(&sym);
    if (it == loop_var_bindings_.end()) {
      return std::nullopt;
    }
    return it->second;
  }

  auto MoveHirUnit() -> hir::ModuleUnit {
    return std::move(hir_unit_);
  }

 private:
  hir::ModuleUnit hir_unit_;
  StructuralVarBindings structural_var_bindings_;
  SubroutineBindings subroutine_bindings_;
  LoopVarBindings loop_var_bindings_;
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
      -> std::optional<hir::StructuralHops> {
    std::uint32_t hops = 0;
    for (const auto frame : frames_ | std::views::reverse) {
      if (frame == target) {
        return hir::StructuralHops{.value = hops};
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

  auto AddStructuralVar(const slang::ast::VariableSymbol& var, hir::TypeId type)
      -> hir::StructuralVarId {
    const hir::StructuralVarId local{
        static_cast<std::uint32_t>(scope_->structural_vars.size())};
    scope_->structural_vars.push_back(
        hir::StructuralVarDecl{.name = std::string{var.name}, .type = type});
    unit_state_->MapStructuralVarBinding(var, frame_, local, type);
    return local;
  }

  auto AddLoopVarDecl(const slang::ast::ValueSymbol& sym, hir::TypeId type)
      -> hir::LoopVarDeclId {
    const hir::LoopVarDeclId id{
        static_cast<std::uint32_t>(scope_->loop_var_decls.size())};
    scope_->loop_var_decls.push_back(
        hir::LoopVarDecl{.name = std::string{sym.name}, .type = type});
    unit_state_->MapLoopVarBinding(sym, frame_, id, type);
    return id;
  }

  [[nodiscard]] auto GetLoopVarDeclType(hir::LoopVarDeclId id) const
      -> hir::TypeId {
    return scope_->loop_var_decls.at(id.value).type;
  }

  auto AddExpr(hir::Expr expr) -> hir::ExprId {
    const hir::ExprId id{static_cast<std::uint32_t>(scope_->exprs.size())};
    scope_->exprs.push_back(std::move(expr));
    return id;
  }

  auto AddProcess(hir::Process process) -> hir::ProcessId {
    const hir::ProcessId id{
        static_cast<std::uint32_t>(scope_->processes.size())};
    scope_->processes.push_back(std::move(process));
    return id;
  }

  auto AddGenerate(hir::Generate generate) -> hir::GenerateId {
    const hir::GenerateId id{
        static_cast<std::uint32_t>(scope_->generates.size())};
    scope_->generates.push_back(std::move(generate));
    return id;
  }

  auto AddStructuralSubroutine(
      const slang::ast::SubroutineSymbol& sym,
      hir::StructuralSubroutineDecl decl) -> hir::StructuralSubroutineId {
    const hir::StructuralSubroutineId local{
        static_cast<std::uint32_t>(scope_->structural_subroutines.size())};
    scope_->structural_subroutines.push_back(std::move(decl));
    unit_state_->MapSubroutineBinding(sym, frame_, local);
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

  auto AddProceduralVar(const slang::ast::VariableSymbol& var, hir::TypeId type)
      -> hir::ProceduralVarId {
    const hir::ProceduralVarId id{
        static_cast<std::uint32_t>(hir_process_.procedural_vars.size())};
    hir_process_.procedural_vars.push_back(
        hir::ProceduralVarDecl{.name = std::string{var.name}, .type = type});
    const auto [_, inserted] = procedural_var_bindings_.emplace(&var, id);
    if (!inserted) {
      throw InternalError(
          "ProcessLoweringState::AddProceduralVar: procedural variable symbol "
          "already mapped");
    }
    return id;
  }

  [[nodiscard]] auto LookupProceduralVar(const slang::ast::VariableSymbol& var)
      const -> std::optional<hir::ProceduralVarId> {
    const auto it = procedural_var_bindings_.find(&var);
    if (it == procedural_var_bindings_.end()) {
      return std::nullopt;
    }
    return it->second;
  }

  [[nodiscard]] auto GetProceduralVarType(hir::ProceduralVarId id) const
      -> hir::TypeId {
    return hir_process_.procedural_vars.at(id.value).type;
  }

  auto Finalize(
      hir::ProcessKind kind, diag::SourceSpan span, hir::StmtId root_stmt)
      -> hir::Process {
    hir_process_.kind = kind;
    hir_process_.span = span;
    hir_process_.root_stmt = root_stmt;
    return std::move(hir_process_);
  }

 private:
  hir::Process hir_process_;
  std::unordered_map<const slang::ast::VariableSymbol*, hir::ProceduralVarId>
      procedural_var_bindings_;
};

}  // namespace lyra::lowering::ast_to_hir
