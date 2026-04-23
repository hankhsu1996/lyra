#pragma once

#include <cstdint>
#include <string>
#include <unordered_map>
#include <utility>

#include <slang/ast/Symbol.h>
#include <slang/ast/symbols/ValueSymbol.h>
#include <slang/ast/symbols/VariableSymbols.h>

#include "lyra/hir/expr.hpp"
#include "lyra/hir/module_unit.hpp"
#include "lyra/hir/process.hpp"
#include "lyra/hir/stmt.hpp"
#include "lyra/hir/type.hpp"
#include "lyra/hir/var_decl.hpp"
#include "lyra/support/unsupported.hpp"

namespace lyra::lowering::ast_to_hir {

// Narrow, module-local, lowering-time-only bridge from slang's resolved
// variable declarations to our owner-local HIR variable ids. Discarded at end
// of module lowering; never stored in HIR or MIR. Owned privately by
// ModuleLoweringState; callers translate via ResolveVariableDecl.
using VariableDeclBindings =
    std::unordered_map<const slang::ast::VariableSymbol*, hir::VarDeclId>;

class ModuleLoweringState {
 public:
  explicit ModuleLoweringState(std::string name) : hir_unit_(std::move(name)) {
  }

  [[nodiscard]] auto HirUnit() const -> const hir::ModuleUnit& {
    return hir_unit_;
  }

  // Owner-bound install. Creates a HIR type in the owning unit.
  auto AddType(hir::TypeData data) -> hir::TypeId {
    return hir_unit_.AddType(std::move(data));
  }

  // Owner-bound install. Creates the HIR var decl and records the frontend
  // variable declaration binding in a single operation.
  auto AddVariableDeclBinding(
      const slang::ast::VariableSymbol& var, hir::TypeId type)
      -> hir::VarDeclId {
    const auto id = hir_unit_.AddVarDecl(std::string{var.name}, type);
    var_bindings_.emplace(&var, id);
    return id;
  }

  // Owner-bound install. Installs a fully-built process into the owning unit.
  auto AddProcess(hir::Process process) -> hir::ProcessId {
    return hir_unit_.AddProcess(std::move(process));
  }

  // Frontend->HIR identity translation. The only use-site path for resolving
  // a slang value reference to an owner-local HIR id. Rejects any reference
  // not resolved to a VariableSymbol -- that kind-check is the boundary.
  [[nodiscard]] auto ResolveVariableDecl(
      const slang::ast::ValueSymbol& symbol) const -> hir::VarDeclId {
    if (symbol.kind != slang::ast::SymbolKind::Variable) {
      support::Unsupported(
          "ResolveVariableDecl: reference to non-variable declaration");
    }
    const auto* var = &symbol.as<slang::ast::VariableSymbol>();
    const auto it = var_bindings_.find(var);
    if (it == var_bindings_.end()) {
      support::Unsupported(
          "ResolveVariableDecl: reference to an unbound variable");
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
