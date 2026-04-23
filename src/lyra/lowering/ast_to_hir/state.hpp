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

// Module-local frontend->HIR binding. Lowering-time only; never enters HIR
// or MIR.
using VariableDeclBindings =
    std::unordered_map<const slang::ast::VariableSymbol*, hir::VarDeclId>;

class ModuleLoweringState {
 public:
  explicit ModuleLoweringState(std::string name) : hir_unit_(std::move(name)) {
  }

  [[nodiscard]] auto HirUnit() const -> const hir::ModuleUnit& {
    return hir_unit_;
  }

  auto AddType(hir::TypeData data) -> hir::TypeId {
    return hir_unit_.AddType(std::move(data));
  }

  auto AddVariableDeclBinding(
      const slang::ast::VariableSymbol& var, hir::TypeId type)
      -> hir::VarDeclId {
    const auto id = hir_unit_.AddVarDecl(std::string{var.name}, type);
    var_bindings_.emplace(&var, id);
    return id;
  }

  auto AddProcess(hir::Process process) -> hir::ProcessId {
    return hir_unit_.AddProcess(std::move(process));
  }

  // Kind-check is the boundary: only VariableSymbol references bind. Anything
  // else fails here.
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
