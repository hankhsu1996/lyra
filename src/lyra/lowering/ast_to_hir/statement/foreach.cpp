#include <cstdint>
#include <expected>
#include <string_view>
#include <utility>
#include <vector>

#include <slang/ast/Expression.h>
#include <slang/ast/Statement.h>
#include <slang/ast/symbols/VariableSymbols.h>
#include <slang/ast/types/AllTypes.h>
#include <slang/ast/types/Type.h>

#include "lyra/base/internal_error.hpp"
#include "lyra/diag/diag_code.hpp"
#include "lyra/diag/diagnostic.hpp"
#include "lyra/diag/kind.hpp"
#include "lyra/hir/binary_op.hpp"
#include "lyra/hir/expr.hpp"
#include "lyra/hir/integral_constant.hpp"
#include "lyra/hir/primary.hpp"
#include "lyra/hir/stmt.hpp"
#include "lyra/hir/value_ref.hpp"
#include "lyra/lowering/ast_to_hir/module_lowerer.hpp"
#include "lyra/lowering/ast_to_hir/process_lowerer.hpp"

namespace lyra::lowering::ast_to_hir {

namespace {

constexpr std::string_view kFlatCounterName = "__lyra_foreach_n";

auto MakeInt32Constant(std::int64_t value) -> hir::IntegralConstant {
  return hir::IntegralConstant{
      .value_words = {static_cast<std::uint64_t>(value) & 0xFFFFFFFFULL},
      .state_words = {},
      .width = 32,
      .signedness = hir::Signedness::kSigned,
      .state_kind = hir::IntegralStateKind::kTwoState,
  };
}

auto MakeInt32LiteralExpr(
    std::int64_t value, hir::TypeId int32_type, diag::SourceSpan span)
    -> hir::Expr {
  return hir::Expr{
      .type = int32_type,
      .data =
          hir::PrimaryExpr{
              .data =
                  hir::IntegerLiteral{
                      .value = MakeInt32Constant(value),
                      .base = hir::IntegerLiteralBase::kDecimal,
                      .declared_unsized = false,
                  }},
      .span = span,
  };
}

auto MakeProcVarRefExpr(
    hir::ProceduralVarId var, hir::TypeId type, diag::SourceSpan span)
    -> hir::Expr {
  return hir::Expr{
      .type = type,
      .data = hir::PrimaryExpr{.data = hir::ProceduralVarRef{.var = var}},
      .span = span,
  };
}

// Ordered outer-to-inner per LRM 12.7.3 cardinality. `inner_product` is the
// product of `count` for every later (more-inner) entry; the innermost has
// inner_product = 1.
struct DimMeta {
  const slang::ast::IteratorSymbol* loop_var;
  std::int64_t lo;
  std::int64_t count;
  std::int64_t inner_product;
  bool ascending;
};

// lo +/- ((counter / inner_product) % count)
auto BuildDecomposeExpr(
    ProcessLowerer& proc, hir::TypeId int32_type, diag::SourceSpan span,
    hir::ProceduralVarId counter_var, const DimMeta& dim) -> hir::ExprId {
  const auto counter_ref_id =
      proc.AddExpr(MakeProcVarRefExpr(counter_var, int32_type, span));

  hir::ExprId divided_id = counter_ref_id;
  if (dim.inner_product != 1) {
    const auto inner_id =
        proc.AddExpr(MakeInt32LiteralExpr(dim.inner_product, int32_type, span));
    divided_id = proc.AddExpr(
        hir::Expr{
            .type = int32_type,
            .data =
                hir::BinaryExpr{
                    .op = hir::BinaryOp::kDiv,
                    .lhs = counter_ref_id,
                    .rhs = inner_id},
            .span = span});
  }

  const auto count_id =
      proc.AddExpr(MakeInt32LiteralExpr(dim.count, int32_type, span));
  const auto offset_id = proc.AddExpr(
      hir::Expr{
          .type = int32_type,
          .data =
              hir::BinaryExpr{
                  .op = hir::BinaryOp::kMod,
                  .lhs = divided_id,
                  .rhs = count_id},
          .span = span});

  const auto lo_id =
      proc.AddExpr(MakeInt32LiteralExpr(dim.lo, int32_type, span));
  return proc.AddExpr(
      hir::Expr{
          .type = int32_type,
          .data =
              hir::BinaryExpr{
                  .op =
                      dim.ascending ? hir::BinaryOp::kAdd : hir::BinaryOp::kSub,
                  .lhs = lo_id,
                  .rhs = offset_id},
          .span = span});
}

auto RejectUnsupportedArrayType(
    const slang::ast::Type& canonical, diag::SourceSpan span)
    -> diag::Result<void> {
  using slang::ast::SymbolKind;
  switch (canonical.kind) {
    case SymbolKind::DynamicArrayType:
      return diag::Unsupported(
          span, diag::DiagCode::kUnsupportedStatementForm,
          "foreach over dynamic array is not yet supported",
          diag::UnsupportedCategory::kFeature);
    case SymbolKind::QueueType:
      return diag::Unsupported(
          span, diag::DiagCode::kUnsupportedStatementForm,
          "foreach over queue is not yet supported",
          diag::UnsupportedCategory::kFeature);
    case SymbolKind::AssociativeArrayType:
      return diag::Unsupported(
          span, diag::DiagCode::kUnsupportedStatementForm,
          "foreach over associative array is not yet supported",
          diag::UnsupportedCategory::kFeature);
    case SymbolKind::StringType:
      return diag::Unsupported(
          span, diag::DiagCode::kUnsupportedStatementForm,
          "foreach over string is not yet supported",
          diag::UnsupportedCategory::kFeature);
    default:
      return {};
  }
}

}  // namespace

auto ProcessLowerer::LowerForeachStmt(
    const slang::ast::ForeachLoopStatement& fs, WalkFrame frame)
    -> diag::Result<hir::Stmt> {
  auto& proc = *this;
  auto& module = proc.Module();
  const auto span = module.SourceMapper().SpanOf(fs.sourceRange);
  const auto& canonical = fs.arrayRef.type->getCanonicalType();
  if (auto check = RejectUnsupportedArrayType(canonical, span); !check) {
    return std::unexpected(std::move(check.error()));
  }

  const hir::TypeId int32_type = module.Builtins().int32;

  std::vector<DimMeta> dims;
  dims.reserve(fs.loopDims.size());
  for (const auto& dim : fs.loopDims) {
    if (dim.loopVar == nullptr) {
      continue;
    }
    if (!dim.range.has_value()) {
      throw InternalError(
          "ProcessLowerer::LowerForeachStmt: non-skipped dim has no constant "
          "range; the type-level rejection above should have caught dynamic / "
          "queue / associative element types");
    }
    const auto& range = *dim.range;
    const bool ascending = range.left <= range.right;
    const std::int64_t count = ascending ? (range.right - range.left + 1)
                                         : (range.left - range.right + 1);
    dims.push_back(
        DimMeta{
            .loop_var = dim.loopVar,
            .lo = range.left,
            .count = count,
            .inner_product = 1,
            .ascending = ascending});
  }

  // All-dims-skipped: arrayRef still needs evaluation for side effects, body
  // runs once.
  if (dims.empty()) {
    auto array_or = proc.LowerExpr(fs.arrayRef, frame);
    if (!array_or) return std::unexpected(std::move(array_or.error()));
    const auto array_eval_id = proc.AddExpr(*std::move(array_or));
    const auto array_eval_stmt = proc.AddStmt(
        hir::Stmt{
            .label = std::nullopt,
            .data = hir::ExprStmt{.expr = array_eval_id},
            .span = span});
    auto body_or = proc.LowerStmt(fs.body, frame);
    if (!body_or) return std::unexpected(std::move(body_or.error()));
    const auto body_stmt_id = proc.AddStmt(*std::move(body_or));
    return hir::Stmt{
        .label = std::nullopt,
        .data = hir::BlockStmt{.statements = {array_eval_stmt, body_stmt_id}},
        .span = span};
  }

  std::int64_t inner = 1;
  for (auto it = dims.rbegin(); it != dims.rend(); ++it) {
    it->inner_product = inner;
    inner *= it->count;
  }
  const std::int64_t total = inner;

  // HIR -> MIR maps procedural vars in HIR-id order driven by VarDecl
  // encounter order. The counter's ForInitDecl is the first VarDecl in the
  // lowered body, so the counter must claim the lowest id; loop variables
  // follow in the same outer-to-inner order their VarDeclStmts appear in.
  const hir::ProceduralVarId counter_var =
      proc.AddSyntheticProceduralVar(kFlatCounterName, int32_type);
  for (const auto& dim : dims) {
    proc.AddProceduralVar(*dim.loop_var, int32_type);
  }

  const auto zero_id = proc.AddExpr(MakeInt32LiteralExpr(0, int32_type, span));
  std::vector<hir::ForInit> init;
  init.emplace_back(hir::ForInitDecl{.var = counter_var, .init = zero_id});

  const auto cond_counter_ref_id =
      proc.AddExpr(MakeProcVarRefExpr(counter_var, int32_type, span));
  const auto total_id =
      proc.AddExpr(MakeInt32LiteralExpr(total, int32_type, span));
  const auto cond_id = proc.AddExpr(
      hir::Expr{
          .type = int32_type,
          .data =
              hir::BinaryExpr{
                  .op = hir::BinaryOp::kLessThan,
                  .lhs = cond_counter_ref_id,
                  .rhs = total_id},
          .span = span});

  const auto step_counter_ref_id =
      proc.AddExpr(MakeProcVarRefExpr(counter_var, int32_type, span));
  const auto step_id = proc.AddExpr(
      hir::Expr{
          .type = int32_type,
          .data =
              hir::IncDecExpr{
                  .op = hir::IncDecOp::kPreInc, .target = step_counter_ref_id},
          .span = span});

  std::vector<hir::StmtId> inner_stmts;
  inner_stmts.reserve(dims.size() + 1);
  for (const auto& dim : dims) {
    const auto local_id = *proc.LookupProceduralVar(*dim.loop_var);
    const auto init_expr_id =
        BuildDecomposeExpr(proc, int32_type, span, counter_var, dim);
    inner_stmts.push_back(proc.AddStmt(
        hir::Stmt{
            .label = std::nullopt,
            .data = hir::VarDeclStmt{.var = local_id, .init = init_expr_id},
            .span = span}));
  }
  auto body_or = proc.LowerStmt(fs.body, frame);
  if (!body_or) return std::unexpected(std::move(body_or.error()));
  inner_stmts.push_back(proc.AddStmt(*std::move(body_or)));

  const auto inner_block_id = proc.AddStmt(
      hir::Stmt{
          .label = std::nullopt,
          .data = hir::BlockStmt{.statements = std::move(inner_stmts)},
          .span = span});

  const auto for_stmt_id = proc.AddStmt(
      hir::Stmt{
          .label = std::nullopt,
          .data =
              hir::ForStmt{
                  .init = std::move(init),
                  .condition = cond_id,
                  .step = {step_id},
                  .body = inner_block_id},
          .span = span});

  // LRM 12.7.3 implicit begin-end around the foreach.
  return hir::Stmt{
      .label = std::nullopt,
      .data = hir::BlockStmt{.statements = {for_stmt_id}},
      .span = span};
}

}  // namespace lyra::lowering::ast_to_hir
