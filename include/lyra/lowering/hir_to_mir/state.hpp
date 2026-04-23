#pragma once

#include <cstdint>
#include <string>
#include <utility>
#include <vector>

#include "lyra/hir/expr.hpp"
#include "lyra/hir/stmt.hpp"
#include "lyra/hir/type.hpp"
#include "lyra/hir/var_decl.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/member.hpp"
#include "lyra/mir/module_unit.hpp"
#include "lyra/mir/process.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/mir/type.hpp"

namespace lyra::lowering::hir_to_mir {

class UnitLoweringState {
 public:
  explicit UnitLoweringState(std::string name) : mir_unit_(std::move(name)) {
  }

  // Owner-bound install. Creates the MIR type and records the
  // hir_id -> mir_id translation in a single operation.
  auto AddType(hir::TypeId hir_id, mir::TypeData data) -> mir::TypeId {
    const auto id = mir_unit_.AddType(std::move(data));
    if (hir_id.value >= type_map_.size()) {
      type_map_.resize(hir_id.value + 1);
    }
    type_map_[hir_id.value] = id;
    return id;
  }

  auto AddMember(hir::VarDeclId hir_id, std::string name, mir::TypeId type)
      -> mir::MemberId {
    const auto id = mir_unit_.AddMember(std::move(name), type);
    if (hir_id.value >= var_map_.size()) {
      var_map_.resize(hir_id.value + 1);
    }
    var_map_[hir_id.value] = id;
    return id;
  }

  auto AddProcess(mir::Process process) -> mir::ProcessId {
    return mir_unit_.AddProcess(std::move(process));
  }

  [[nodiscard]] auto TranslateType(hir::TypeId hir_id) const -> mir::TypeId {
    return type_map_.at(hir_id.value);
  }

  [[nodiscard]] auto TranslateVar(hir::VarDeclId hir_id) const
      -> mir::MemberId {
    return var_map_.at(hir_id.value);
  }

  auto MoveMirUnit() -> mir::ModuleUnit {
    return std::move(mir_unit_);
  }

 private:
  mir::ModuleUnit mir_unit_;
  std::vector<mir::TypeId> type_map_;
  std::vector<mir::MemberId> var_map_;
};

class ProcessLoweringState {
 public:
  auto AppendExpr(hir::ExprId hir_id, mir::Expr expr) -> mir::ExprId {
    const mir::ExprId id{static_cast<std::uint32_t>(mir_process_.exprs.size())};
    mir_process_.exprs.push_back(std::move(expr));
    if (hir_id.value >= expr_map_.size()) {
      expr_map_.resize(hir_id.value + 1);
    }
    expr_map_[hir_id.value] = id;
    return id;
  }

  auto AppendStmt(hir::StmtId hir_id, mir::Stmt stmt) -> mir::StmtId {
    const mir::StmtId id{static_cast<std::uint32_t>(mir_process_.stmts.size())};
    mir_process_.stmts.push_back(std::move(stmt));
    if (hir_id.value >= stmt_map_.size()) {
      stmt_map_.resize(hir_id.value + 1);
    }
    stmt_map_[hir_id.value] = id;
    return id;
  }

  [[nodiscard]] auto TranslateExpr(hir::ExprId hir_id) const -> mir::ExprId {
    return expr_map_.at(hir_id.value);
  }

  [[nodiscard]] auto TranslateStmt(hir::StmtId hir_id) const -> mir::StmtId {
    return stmt_map_.at(hir_id.value);
  }

  auto Finalize(mir::ProcessData data) -> mir::Process {
    mir_process_.data = std::move(data);
    return std::move(mir_process_);
  }

 private:
  mir::Process mir_process_;
  std::vector<mir::ExprId> expr_map_;
  std::vector<mir::StmtId> stmt_map_;
};

}  // namespace lyra::lowering::hir_to_mir
