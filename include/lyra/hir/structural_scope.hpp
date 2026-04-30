#pragma once

#include <compare>
#include <cstdint>
#include <optional>
#include <variant>
#include <vector>

#include "lyra/base/time.hpp"
#include "lyra/hir/expr.hpp"
#include "lyra/hir/loop_var.hpp"
#include "lyra/hir/process.hpp"
#include "lyra/hir/structural_var.hpp"
#include "lyra/hir/subroutine.hpp"

namespace lyra::hir {

struct StructuralScope;

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
  StructuralScopeId scope;
};

using GenerateData = std::variant<IfGenerate, CaseGenerate, LoopGenerate>;

struct Generate {
  GenerateData data;
  std::vector<StructuralScope> child_scopes;

  [[nodiscard]] auto GetChildScope(StructuralScopeId id) const
      -> const StructuralScope&;
};

struct StructuralScope {
  StructuralScopeId id{};
  TimeResolution time_resolution;
  std::vector<StructuralVarDecl> structural_vars;
  std::vector<LoopVarDecl> loop_var_decls;
  std::vector<Expr> exprs;
  std::vector<Process> processes;
  std::vector<Generate> generates;
  std::vector<StructuralSubroutineDecl> structural_subroutines;

  [[nodiscard]] auto GetStructuralVar(StructuralVarId id) const
      -> const StructuralVarDecl& {
    return structural_vars.at(id.value);
  }
  [[nodiscard]] auto GetLoopVarDecl(LoopVarDeclId id) const
      -> const LoopVarDecl& {
    return loop_var_decls.at(id.value);
  }
  [[nodiscard]] auto GetExpr(ExprId id) const -> const Expr& {
    return exprs.at(id.value);
  }
  [[nodiscard]] auto GetProcess(ProcessId id) const -> const Process& {
    return processes.at(id.value);
  }
  [[nodiscard]] auto GetGenerate(GenerateId id) const -> const Generate& {
    return generates.at(id.value);
  }
  [[nodiscard]] auto GetStructuralSubroutine(StructuralSubroutineId id) const
      -> const StructuralSubroutineDecl& {
    return structural_subroutines.at(id.value);
  }
};

inline auto Generate::GetChildScope(StructuralScopeId id) const
    -> const StructuralScope& {
  return child_scopes.at(id.value);
}

}  // namespace lyra::hir
