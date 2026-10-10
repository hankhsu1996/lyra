#include <expected>
#include <optional>
#include <utility>
#include <vector>

#include <slang/ast/Statement.h>
#include <slang/ast/statements/LoopStatements.h>
#include <slang/ast/symbols/VariableSymbols.h>

#include "lyra/diag/diagnostic.hpp"
#include "lyra/frontend/slang_source_span.hpp"
#include "lyra/hir/procedural_var.hpp"
#include "lyra/hir/stmt.hpp"
#include "lyra/lowering/ast_to_hir/process_lowerer.hpp"
#include "lyra/lowering/ast_to_hir/unit_lowerer.hpp"

namespace lyra::lowering::ast_to_hir {

// LRM 12.7.3. What only the front end knows is settled here: which dimensions
// the list of loop variables reaches, which it leaves empty, and which symbol
// each variable is. The variables belong to the implicit block around the loop,
// which is the scope this statement is reached under, so they are declared
// there.
auto ProcessLowerer::LowerForeachStmt(
    const slang::ast::ForeachLoopStatement& fs, WalkFrame frame)
    -> diag::Result<hir::Stmt> {
  auto& body = *frame.current_procedural_body;
  const auto span = frontend::SpanOf(fs.sourceRange);

  auto array = LowerExpr(fs.arrayRef, frame);
  if (!array) return std::unexpected(std::move(array.error()));
  const hir::ExprId array_id = frame.Exprs().Add(*std::move(array));

  std::vector<std::optional<hir::ProceduralVarId>> loop_vars;
  loop_vars.reserve(fs.loopDims.size());
  for (const auto& dim : fs.loopDims) {
    if (dim.loopVar == nullptr) {
      loop_vars.emplace_back(std::nullopt);
      continue;
    }
    auto type = Owner().InternType(dim.loopVar->getType(), span);
    if (!type) return std::unexpected(std::move(type.error()));
    loop_vars.emplace_back(AddProceduralVar(frame, body, *dim.loopVar, *type));
  }

  auto body_stmt = LowerStmt(fs.body, frame);
  if (!body_stmt) return std::unexpected(std::move(body_stmt.error()));
  const hir::StmtId body_id = body.stmts.Add(*std::move(body_stmt));

  return hir::Stmt{
      .label = std::nullopt,
      .data =
          hir::ForeachStmt{
              .array = array_id,
              .loop_vars = std::move(loop_vars),
              .body = body_id},
      .span = span};
}

}  // namespace lyra::lowering::ast_to_hir
