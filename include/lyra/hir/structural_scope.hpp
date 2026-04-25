#pragma once

#include <compare>
#include <cstdint>
#include <optional>
#include <string>
#include <variant>
#include <vector>

#include "lyra/hir/expr.hpp"
#include "lyra/hir/member_var.hpp"
#include "lyra/hir/process.hpp"

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

using GenerateData = std::variant<IfGenerate, CaseGenerate>;

struct Generate {
  GenerateData data;
  std::vector<StructuralScope> child_scopes;
};

class StructuralScope {
 public:
  StructuralScope();
  ~StructuralScope();
  StructuralScope(StructuralScope&&) noexcept;
  auto operator=(StructuralScope&&) noexcept -> StructuralScope&;
  StructuralScope(const StructuralScope&) = delete;
  auto operator=(const StructuralScope&) -> StructuralScope& = delete;

  auto AddMemberVar(std::string name, TypeId type) -> MemberVarId;
  auto AddExpr(Expr expr) -> ExprId;
  auto AddProcess(Process process) -> ProcessId;
  auto AddGenerate(Generate generate) -> GenerateId;

  [[nodiscard]] auto MemberVars() const -> const std::vector<MemberVar>&;
  [[nodiscard]] auto Exprs() const -> const std::vector<Expr>&;
  [[nodiscard]] auto Processes() const -> const std::vector<Process>&;
  [[nodiscard]] auto Generates() const -> const std::vector<Generate>&;

  [[nodiscard]] auto GetMemberVar(MemberVarId id) const -> const MemberVar&;
  [[nodiscard]] auto GetExpr(ExprId id) const -> const Expr&;
  [[nodiscard]] auto GetProcess(ProcessId id) const -> const Process&;
  [[nodiscard]] auto GetGenerate(GenerateId id) const -> const Generate&;

 private:
  std::vector<MemberVar> member_vars_;
  std::vector<Expr> exprs_;
  std::vector<Process> processes_;
  std::vector<Generate> generates_;
};

}  // namespace lyra::hir
