#pragma once

#include <compare>
#include <cstdint>
#include <optional>
#include <string>
#include <variant>
#include <vector>

#include "lyra/hir/expr.hpp"
#include "lyra/hir/loop_var.hpp"
#include "lyra/hir/member_var.hpp"
#include "lyra/hir/process.hpp"
#include "lyra/hir/subroutine.hpp"

namespace lyra::hir {

class StructuralScope;

struct GenerateId {
  std::uint32_t value;

  auto operator<=>(const GenerateId&) const -> std::strong_ordering = default;
};

struct StructuralScopeId {
  std::uint32_t value;

  auto operator<=>(const StructuralScopeId&) const
      -> std::strong_ordering = default;
};

struct IfGenerate {
  ExprId condition;
  StructuralScopeId then_scope;
  std::optional<StructuralScopeId> else_scope;
};

struct CaseGenerateItem {
  std::vector<ExprId> labels;
  StructuralScopeId scope;
};

struct CaseGenerate {
  ExprId condition;
  std::vector<CaseGenerateItem> items;
  std::optional<StructuralScopeId> default_scope;
};

struct LoopGenerate {
  LoopVarDeclId loop_var;
  ExprId initial;
  ExprId stop;
  ExprId iter;
  StructuralScopeId body_scope;
};

using GenerateData = std::variant<IfGenerate, CaseGenerate, LoopGenerate>;

struct Generate {
  GenerateData data;
  std::vector<StructuralScope> child_scopes;

  auto AddChildScope(StructuralScope scope) -> StructuralScopeId;
  [[nodiscard]] auto GetChildScope(StructuralScopeId id) const
      -> const StructuralScope&;
};

class StructuralScope {
 public:
  StructuralScope();
  ~StructuralScope();
  StructuralScope(StructuralScope&&) noexcept;
  auto operator=(StructuralScope&&) noexcept -> StructuralScope&;
  StructuralScope(const StructuralScope&) = delete;
  auto operator=(const StructuralScope&) -> StructuralScope& = delete;

  [[nodiscard]] auto Id() const -> StructuralScopeId {
    return id_;
  }

  auto AddMemberVar(std::string name, TypeId type) -> MemberVarId;
  auto AddLoopVarDecl(std::string name) -> LoopVarDeclId;
  auto AddExpr(Expr expr) -> ExprId;
  auto AddProcess(Process process) -> ProcessId;
  auto AddGenerate(Generate generate) -> GenerateId;
  auto AddSubroutine(UserSubroutineDecl decl) -> SubroutineId;

  [[nodiscard]] auto MemberVars() const -> const std::vector<MemberVar>&;
  [[nodiscard]] auto LoopVarDecls() const -> const std::vector<LoopVarDecl>&;
  [[nodiscard]] auto Exprs() const -> const std::vector<Expr>&;
  [[nodiscard]] auto Processes() const -> const std::vector<Process>&;
  [[nodiscard]] auto Generates() const -> const std::vector<Generate>&;
  [[nodiscard]] auto Subroutines() const
      -> const std::vector<UserSubroutineDecl>&;

  [[nodiscard]] auto GetMemberVar(MemberVarId id) const -> const MemberVar&;
  [[nodiscard]] auto GetLoopVarDecl(LoopVarDeclId id) const
      -> const LoopVarDecl&;
  [[nodiscard]] auto GetExpr(ExprId id) const -> const Expr&;
  [[nodiscard]] auto GetProcess(ProcessId id) const -> const Process&;
  [[nodiscard]] auto GetGenerate(GenerateId id) const -> const Generate&;
  [[nodiscard]] auto GetSubroutine(SubroutineId id) const
      -> const UserSubroutineDecl&;

 private:
  friend struct Generate;

  StructuralScopeId id_{};
  std::vector<MemberVar> member_vars_;
  std::vector<LoopVarDecl> loop_var_decls_;
  std::vector<Expr> exprs_;
  std::vector<Process> processes_;
  std::vector<Generate> generates_;
  std::vector<UserSubroutineDecl> subroutines_;
};

inline auto Generate::AddChildScope(StructuralScope scope)
    -> StructuralScopeId {
  const StructuralScopeId id{static_cast<std::uint32_t>(child_scopes.size())};
  scope.id_ = id;
  child_scopes.push_back(std::move(scope));
  return id;
}

inline auto Generate::GetChildScope(StructuralScopeId id) const
    -> const StructuralScope& {
  return child_scopes.at(id.value);
}

}  // namespace lyra::hir
