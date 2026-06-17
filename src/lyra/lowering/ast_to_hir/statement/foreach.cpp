#include <expected>
#include <optional>
#include <string>
#include <string_view>
#include <utility>
#include <vector>

#include <slang/ast/Expression.h>
#include <slang/ast/Statement.h>
#include <slang/ast/symbols/VariableSymbols.h>
#include <slang/ast/types/AllTypes.h>
#include <slang/ast/types/Type.h>

#include "lyra/diag/diag_code.hpp"
#include "lyra/diag/diagnostic.hpp"
#include "lyra/diag/kind.hpp"
#include "lyra/hir/binary_op.hpp"
#include "lyra/hir/expr.hpp"
#include "lyra/hir/expr_builders.hpp"
#include "lyra/hir/inc_dec_op.hpp"
#include "lyra/hir/method.hpp"
#include "lyra/hir/stmt.hpp"
#include "lyra/hir/subroutine_ref.hpp"
#include "lyra/lowering/ast_to_hir/module_lowerer.hpp"
#include "lyra/lowering/ast_to_hir/process_lowerer.hpp"

namespace lyra::lowering::ast_to_hir {

namespace {

constexpr std::string_view kLastIndexName = "__lyra_foreach_last";

// slang already describes each `foreach` dimension: a fixed dimension carries a
// constant `range` (its declared bounds and direction); a dynamically sized one
// (queue / dynamic array) has no `range` and takes its bounds from `.size()` at
// run time. `DescribeRange` turns either form into a uniform
// `(first, last, direction)`.
using LoopDim = slang::ast::ForeachLoopStatement::LoopDim;

// A dimension's iteration range: iterate the loop variable from `first` to
// `last` inclusive, ascending or descending. `first` and `last` are HIR
// expressions, so a fixed dimension and a dynamically sized one (whose `last`
// is `size - 1`) are described the same way and iterated by the same loop.
struct DimensionRange {
  hir::ExprId first;
  hir::ExprId last;
  bool ascending;
};

// Describes one iterated dimension's index range. The only place a dimension's
// type is consulted: a fixed dimension yields its declared range and direction;
// a dynamically sized one yields 0..size-1 ascending, with the count sampled
// once on entry (LRM 12.7.3) into a local that `setup` declares. `array` is the
// receiver whose `.size()` bounds a dynamic dimension (queue and dynamic array
// both expose it; the type only names the builtin-method kind).
auto DescribeRange(
    hir::ProceduralBody& body, const LoopDim& dim,
    std::optional<hir::ExprId> array, const slang::ast::Type* array_type,
    hir::TypeId int32_type, diag::SourceSpan span,
    std::vector<hir::StmtId>& setup) -> DimensionRange {
  if (dim.range.has_value()) {
    const auto& declared = *dim.range;
    return DimensionRange{
        .first = body.AddExpr(
            hir::MakeInt32Literal(declared.left, int32_type, span)),
        .last = body.AddExpr(
            hir::MakeInt32Literal(declared.right, int32_type, span)),
        .ascending = declared.left <= declared.right};
  }

  hir::SubroutineRef size_callee =
      array_type->isQueue() ? hir::SubroutineRef{hir::BuiltinMethodRef{
                                  .method = hir::QueueMethodKind::kSize}}
                            : hir::SubroutineRef{hir::BuiltinMethodRef{
                                  .method = hir::ArrayMethodKind::kSize}};
  const hir::ExprId size_id = body.AddExpr(
      hir::Expr{
          .type = int32_type,
          .data =
              hir::CallExpr{
                  .callee = std::move(size_callee), .arguments = {*array}},
          .span = span});
  const hir::ExprId one_id =
      body.AddExpr(hir::MakeInt32Literal(1, int32_type, span));
  const hir::ExprId last_value_id = body.AddExpr(
      hir::Expr{
          .type = int32_type,
          .data =
              hir::BinaryExpr{
                  .op = hir::BinaryOp::kSub, .lhs = size_id, .rhs = one_id},
          .span = span});
  const hir::ProceduralVarId last_var = body.AddProceduralVar(
      hir::ProceduralVarDecl{
          .name = std::string{kLastIndexName},
          .type = int32_type,
          .lifetime = hir::VariableLifetime::kAutomatic});
  setup.push_back(body.AddStmt(
      hir::Stmt{
          .label = std::nullopt,
          .data = hir::VarDeclStmt{.var = last_var, .init = last_value_id},
          .span = span}));
  return DimensionRange{
      .first = body.AddExpr(hir::MakeInt32Literal(0, int32_type, span)),
      .last = body.AddExpr(hir::MakeProcVarRefExpr(last_var, int32_type, span)),
      .ascending = true};
}

// Recursively wraps the loop body in one nested loop per remaining dimension.
//
// `array` is `arrayRef` indexed by the enclosing loop variables -- the receiver
// whose `.size()` bounds a dynamic dimension -- with `array_type` its slang
// type; both are absent when no dimension at or below is dynamically sized (a
// purely fixed nest never touches the array). Descending one level indexes
// `array` by this level's loop variable and peels `array_type` once, so the
// type flows down naturally rather than being recomputed from the root.
//
// Procedural-var ids are minted top to bottom (each level's `last` local before
// its loop variable, then the body's own locals), matching the order HIR-to-MIR
// encounters the declarations. The outermost loop (level 0) is the break
// landing target, marked only when a break in the body actually used the label.
//
// Returns the statements realizing this level: a lone `for`, or a dynamic
// dimension's sampled-`last` declaration followed by its `for`.
auto BuildForeachNest(
    ProcessLowerer& proc, const slang::ast::ForeachLoopStatement& fs,
    WalkFrame frame, const std::vector<const LoopDim*>& levels,
    std::size_t level, std::optional<hir::ExprId> array,
    const slang::ast::Type* array_type, hir::TypeId int32_type,
    diag::SourceSpan span, hir::LoopLabelId foreach_label, bool* label_used)
    -> diag::Result<std::vector<hir::StmtId>> {
  auto& body = *frame.current_procedural_body;

  if (level == levels.size()) {
    // Innermost: the user body, lowered with the foreach label in scope so a
    // `break` that exits the whole nest (LRM 12.8) targets the outermost loop.
    auto body_or = proc.LowerStmt(
        fs.body, frame.WithBreakLabel(foreach_label, label_used));
    if (!body_or) return std::unexpected(std::move(body_or.error()));
    return std::vector<hir::StmtId>{body.AddStmt(*std::move(body_or))};
  }

  const LoopDim& dim = *levels[level];

  // Describe this dimension's range -- the one type-dependent step. Any setup
  // (a dynamic dimension samples its `last` once into a local) lands in `stmts`
  // before the loop.
  std::vector<hir::StmtId> stmts;
  const DimensionRange range =
      DescribeRange(body, dim, array, array_type, int32_type, span, stmts);

  const hir::ProceduralVarId loop_var =
      proc.AddProceduralVar(body, *dim.loopVar, int32_type);

  // Descend: the inner levels iterate `array[loop_var]`, one type peel deeper.
  // Only built while the array is being threaded (some inner level is dynamic);
  // a deeper fixed-only tail leaves it absent.
  std::optional<hir::ExprId> inner_array;
  const slang::ast::Type* inner_array_type = nullptr;
  if (array.has_value()) {
    inner_array_type = array_type->getCanonicalType().getArrayElementType();
    auto inner_hir_type = proc.Module().InternType(*inner_array_type, span);
    if (!inner_hir_type) {
      return std::unexpected(std::move(inner_hir_type.error()));
    }
    const hir::ExprId index_id =
        body.AddExpr(hir::MakeProcVarRefExpr(loop_var, int32_type, span));
    inner_array = body.AddExpr(
        hir::Expr{
            .type = *inner_hir_type,
            .data =
                hir::ElementSelectExpr{.base_value = *array, .index = index_id},
            .span = span});
  }

  auto inner_or = BuildForeachNest(
      proc, fs, frame, levels, level + 1, inner_array, inner_array_type,
      int32_type, span, foreach_label, label_used);
  if (!inner_or) return std::unexpected(std::move(inner_or.error()));
  const hir::StmtId inner_body =
      inner_or->size() == 1
          ? inner_or->front()
          : body.AddStmt(
                hir::Stmt{
                    .label = std::nullopt,
                    .data = hir::BlockStmt{.statements = std::move(*inner_or)},
                    .span = span});

  // Iterate the range: one loop shape for every dimension, driven only by
  // (first, last, direction) -- no dynamic-vs-fixed branch here.
  const hir::ExprId cond_ref =
      body.AddExpr(hir::MakeProcVarRefExpr(loop_var, int32_type, span));
  const hir::ExprId cond_id = body.AddExpr(
      hir::Expr{
          .type = int32_type,
          .data =
              hir::BinaryExpr{
                  .op = range.ascending ? hir::BinaryOp::kLessEqual
                                        : hir::BinaryOp::kGreaterEqual,
                  .lhs = cond_ref,
                  .rhs = range.last},
          .span = span});
  const hir::ExprId step_ref =
      body.AddExpr(hir::MakeProcVarRefExpr(loop_var, int32_type, span));
  const hir::ExprId step_id = body.AddExpr(
      hir::Expr{
          .type = int32_type,
          .data =
              hir::IncDecExpr{
                  .op = range.ascending ? hir::IncDecOp::kPreInc
                                        : hir::IncDecOp::kPreDec,
                  .target = step_ref},
          .span = span});

  std::vector<hir::ForInit> init;
  init.emplace_back(hir::ForInitDecl{.var = loop_var, .init = range.first});
  stmts.push_back(body.AddStmt(
      hir::Stmt{
          .label = std::nullopt,
          .data =
              hir::ForStmt{
                  .init = std::move(init),
                  .condition = cond_id,
                  .step = {step_id},
                  .body = inner_body,
                  .break_label = (level == 0 && *label_used)
                                     ? std::optional{foreach_label}
                                     : std::nullopt},
          .span = span}));
  return stmts;
}

// The associative array iterates by key and the string by byte (LRM 7.9 /
// 6.16), distinct iteration models from the index-counted dynamic array and
// queue handled here. Both stay rejected until their own lowering lands.
auto RejectUnsupportedArrayType(
    const slang::ast::Type& canonical, diag::SourceSpan span)
    -> diag::Result<void> {
  using slang::ast::SymbolKind;
  switch (canonical.kind) {
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
  auto& body = *frame.current_procedural_body;
  const auto span = module.SourceMapper().SpanOf(fs.sourceRange);
  const auto& canonical = fs.arrayRef.type->getCanonicalType();
  if (auto check = RejectUnsupportedArrayType(canonical, span); !check) {
    return std::unexpected(std::move(check.error()));
  }

  const hir::TypeId int32_type = module.Unit().builtins.int32;

  // Collect the iterated (non-skipped) dimensions in cardinal order. A
  // dimension with no constant range is dynamically sized (LRM 7.5 / 7.10).
  std::vector<const LoopDim*> levels;
  levels.reserve(fs.loopDims.size());
  bool any_dynamic = false;
  for (const auto& dim : fs.loopDims) {
    if (dim.loopVar == nullptr) {
      continue;
    }
    any_dynamic = any_dynamic || !dim.range.has_value();
    levels.push_back(&dim);
  }

  // All dimensions skipped (`foreach (a[])`): the array reference is still
  // evaluated for side effects and the body runs once. No loop, so a break in
  // the body is a plain innermost exit.
  if (levels.empty()) {
    auto array_or = proc.LowerExpr(fs.arrayRef, frame);
    if (!array_or) return std::unexpected(std::move(array_or.error()));
    const auto array_eval_stmt = body.AddStmt(
        hir::Stmt{
            .label = std::nullopt,
            .data = hir::ExprStmt{.expr = body.AddExpr(*std::move(array_or))},
            .span = span});
    auto body_or = proc.LowerStmt(fs.body, frame.WithoutBreakLabel());
    if (!body_or) return std::unexpected(std::move(body_or.error()));
    const auto body_stmt_id = body.AddStmt(*std::move(body_or));
    return hir::Stmt{
        .label = std::nullopt,
        .data = hir::BlockStmt{.statements = {array_eval_stmt, body_stmt_id}},
        .span = span};
  }

  // Thread `arrayRef` into the nest only when some dimension needs a runtime
  // size; a purely fixed foreach lowers to plain range loops that never read
  // the array.
  std::optional<hir::ExprId> array;
  const slang::ast::Type* array_type = nullptr;
  if (any_dynamic) {
    auto array_or = proc.LowerExpr(fs.arrayRef, frame);
    if (!array_or) return std::unexpected(std::move(array_or.error()));
    array = body.AddExpr(*std::move(array_or));
    array_type = fs.arrayRef.type;
  }

  const hir::LoopLabelId foreach_label = body.AddLoopLabel();
  bool label_used = false;
  auto top = BuildForeachNest(
      proc, fs, frame, levels, 0, array, array_type, int32_type, span,
      foreach_label, &label_used);
  if (!top) return std::unexpected(std::move(top.error()));

  // LRM 12.7.3 implicit begin-end around the foreach.
  return hir::Stmt{
      .label = std::nullopt,
      .data = hir::BlockStmt{.statements = std::move(*top)},
      .span = span};
}

}  // namespace lyra::lowering::ast_to_hir
