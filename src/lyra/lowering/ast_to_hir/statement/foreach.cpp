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

#include "lyra/diag/diagnostic.hpp"
#include "lyra/hir/binary_op.hpp"
#include "lyra/hir/expr.hpp"
#include "lyra/hir/expr_builders.hpp"
#include "lyra/hir/inc_dec_op.hpp"
#include "lyra/hir/stmt.hpp"
#include "lyra/hir/subroutine_ref.hpp"
#include "lyra/hir/value_ref.hpp"
#include "lyra/lowering/ast_to_hir/module_lowerer.hpp"
#include "lyra/lowering/ast_to_hir/process_lowerer.hpp"

namespace lyra::lowering::ast_to_hir {

namespace {

constexpr std::string_view kLastIndexName = "__lyra_foreach_last";
constexpr std::string_view kMoreFlagName = "__lyra_foreach_more";

using LoopDim = slang::ast::ForeachLoopStatement::LoopDim;

// The pieces that drive one iterated dimension's `for` loop, plus the loop
// variable the body indexes by. Every dimension -- fixed, dynamically sized, or
// associative -- iterates through this one shape; only how the pieces are built
// differs, and that is the sole type-dependent step. A dimension's per-loop
// preamble (a dynamically sized dimension samples its count once, an
// associative dimension declares its key) is appended to a separate `setup`
// stream the caller emits before the `for`.
struct LevelLoop {
  hir::ProceduralVarId loop_var;
  hir::TypeId loop_var_type;
  std::vector<hir::ForInit> init;
  hir::ExprId condition;
  std::vector<hir::ExprId> step;
};

// The element-count query for a runtime-sized dimension: queue and dynamic
// array expose `size`; a string exposes `len` (LRM 6.16.1).
auto SizeMethodFor(const slang::ast::Type& array_type)
    -> hir::BuiltinMethodRef {
  if (array_type.getCanonicalType().isString()) {
    return {.method = support::BuiltinFn::kLen};
  }
  return {.method = support::BuiltinFn::kSize};
}

// Build the loop for one index-counted dimension. A fixed dimension iterates
// its declared range and direction; a dynamically sized one (queue / dynamic
// array / string) counts 0..size-1 ascending, sampling the count once on entry
// (LRM 12.7.3) into a `setup` local that the condition then reads. The loop
// variable is the counter, declared in the `for`.
auto BuildIntegerLevel(
    ProcessLowerer& proc, WalkFrame frame, const LoopDim& dim,
    std::optional<hir::ExprId> array, const slang::ast::Type* array_type,
    hir::TypeId int32_type, diag::SourceSpan span,
    std::vector<hir::StmtId>& setup) -> LevelLoop {
  auto& body = *frame.current_procedural_body;
  hir::ExprId first_id{};
  hir::ExprId last_id{};
  bool ascending = true;
  if (dim.range.has_value()) {
    const auto& declared = *dim.range;
    first_id = frame.Exprs().Add(
        hir::MakeInt32Literal(declared.left, int32_type, span));
    last_id = frame.Exprs().Add(
        hir::MakeInt32Literal(declared.right, int32_type, span));
    ascending = declared.left <= declared.right;
  } else {
    hir::SubroutineRef size_callee = SizeMethodFor(*array_type);
    const hir::ExprId size_id = frame.Exprs().Add(
        hir::Expr{
            .type = int32_type,
            .data =
                hir::CallExpr{
                    .callee = std::move(size_callee), .arguments = {*array}},
            .span = span});
    const hir::ExprId one_id =
        frame.Exprs().Add(hir::MakeInt32Literal(1, int32_type, span));
    const hir::ExprId last_value_id = frame.Exprs().Add(
        hir::Expr{
            .type = int32_type,
            .data =
                hir::BinaryExpr{
                    .op = hir::BinaryOp::kSub, .lhs = size_id, .rhs = one_id},
            .span = span});
    const hir::ProceduralVarId last_var = body.procedural_vars.Add(
        hir::ProceduralVarDecl{
            .name = std::string{kLastIndexName},
            .type = int32_type,
            .lifetime = hir::VariableLifetime::kAutomatic});
    setup.push_back(body.stmts.Add(
        hir::Stmt{
            .label = std::nullopt,
            .data = hir::VarDeclStmt{.var = last_var, .init = last_value_id},
            .span = span}));
    first_id = frame.Exprs().Add(hir::MakeInt32Literal(0, int32_type, span));
    last_id = frame.Exprs().Add(
        hir::MakeRefExpr(
            hir::ProceduralVarRef{.var = last_var}, int32_type, span));
  }

  const hir::ProceduralVarId loop_var =
      proc.AddProceduralVar(body, *dim.loopVar, int32_type);
  std::vector<hir::ForInit> init;
  init.emplace_back(hir::ForInitDecl{.var = loop_var, .init = first_id});
  const hir::ExprId cond_ref = frame.Exprs().Add(
      hir::MakeRefExpr(
          hir::ProceduralVarRef{.var = loop_var}, int32_type, span));
  const hir::ExprId cond_id = frame.Exprs().Add(
      hir::Expr{
          .type = int32_type,
          .data =
              hir::BinaryExpr{
                  .op = ascending ? hir::BinaryOp::kLessEqual
                                  : hir::BinaryOp::kGreaterEqual,
                  .lhs = cond_ref,
                  .rhs = last_id},
          .span = span});
  const hir::ExprId step_ref = frame.Exprs().Add(
      hir::MakeRefExpr(
          hir::ProceduralVarRef{.var = loop_var}, int32_type, span));
  const hir::ExprId step_id = frame.Exprs().Add(
      hir::Expr{
          .type = int32_type,
          .data =
              hir::IncDecExpr{
                  .op = ascending ? hir::IncDecOp::kPreInc
                                  : hir::IncDecOp::kPreDec,
                  .target = step_ref},
          .span = span});
  return LevelLoop{
      .loop_var = loop_var,
      .loop_var_type = int32_type,
      .init = std::move(init),
      .condition = cond_id,
      .step = {step_id}};
}

// Build the loop for one associative dimension (LRM 7.9.4 -- 7.9.7). The array
// iterates by key: a `for` whose counter holds first / next's 0/1 return and
// whose step advances the key through the method's `ref` index argument.
// `continue` lands on the step, so it advances the key (LRM 12.8); an empty
// array makes `first` return 0, so the body never runs. The loop variable is
// the key (the body indexes the array by it); the counter is a synthetic flag.
// The key is declared in `setup` rather than the `for` init because its type
// differs from the counter and cannot share one C++ init declaration.
auto BuildAssociativeLevel(
    ProcessLowerer& proc, WalkFrame frame, const LoopDim& dim,
    hir::ExprId array, hir::TypeId int32_type, diag::SourceSpan span,
    std::vector<hir::StmtId>& setup) -> diag::Result<LevelLoop> {
  auto& body = *frame.current_procedural_body;
  auto key_type_or = proc.Module().InternType(dim.loopVar->getType(), span);
  if (!key_type_or) return std::unexpected(std::move(key_type_or.error()));
  const hir::TypeId key_type = *key_type_or;

  const hir::ProceduralVarId key_var =
      proc.AddProceduralVar(body, *dim.loopVar, key_type);
  setup.push_back(body.stmts.Add(
      hir::Stmt{
          .label = std::nullopt,
          .data = hir::VarDeclStmt{.var = key_var, .init = std::nullopt},
          .span = span}));
  const hir::ProceduralVarId more_var = body.procedural_vars.Add(
      hir::ProceduralVarDecl{
          .name = std::string{kMoreFlagName},
          .type = int32_type,
          .lifetime = hir::VariableLifetime::kAutomatic});

  auto walk_call = [&](support::BuiltinFn method) -> hir::ExprId {
    const hir::ExprId key_ref = frame.Exprs().Add(
        hir::MakeRefExpr(
            hir::ProceduralVarRef{.var = key_var}, key_type, span));
    return frame.Exprs().Add(
        hir::Expr{
            .type = int32_type,
            .data =
                hir::CallExpr{
                    .callee = hir::SubroutineRef{hir::BuiltinMethodRef{
                        .method = method}},
                    .arguments = {array, key_ref}},
            .span = span});
  };

  std::vector<hir::ForInit> init;
  init.emplace_back(
      hir::ForInitDecl{
          .var = more_var, .init = walk_call(support::BuiltinFn::kAssocFirst)});
  const hir::ExprId cond_id = frame.Exprs().Add(
      hir::MakeRefExpr(
          hir::ProceduralVarRef{.var = more_var}, int32_type, span));
  const hir::ExprId more_lhs = frame.Exprs().Add(
      hir::MakeRefExpr(
          hir::ProceduralVarRef{.var = more_var}, int32_type, span));
  const hir::ExprId step_id = frame.Exprs().Add(
      hir::Expr{
          .type = int32_type,
          .data =
              hir::AssignExpr{
                  .kind = hir::AssignKind::kBlocking,
                  .lhs = more_lhs,
                  .compound_op = std::nullopt,
                  .rhs = walk_call(support::BuiltinFn::kAssocNext)},
          .span = span});
  return LevelLoop{
      .loop_var = key_var,
      .loop_var_type = key_type,
      .init = std::move(init),
      .condition = cond_id,
      .step = {step_id}};
}

// Recursively wraps the loop body in one nested loop per remaining dimension.
//
// `array` is `arrayRef` indexed by the enclosing loop variables -- the receiver
// a dynamic dimension reads for its `.size()` and an associative dimension for
// its keys -- with `array_type` its slang type; both are absent when no
// dimension at or below reads the array at run time (a purely fixed nest never
// touches it). Descending one level indexes `array` by this level's loop
// variable and peels `array_type` once, so the type flows down naturally rather
// than being recomputed from the root.
//
// Procedural-var ids are minted top to bottom in statement order (a level's
// pre-loop locals before its `for`-init locals, then the body's own locals),
// matching the order HIR-to-MIR encounters the declarations. The outermost loop
// (level 0) is the break landing target, marked only when a break in the body
// actually used the label.
//
// Returns the statements realizing this level: the `for`, preceded by any
// pre-loop setup the level needs (a sampled `last`, an associative key).
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
    return std::vector<hir::StmtId>{body.stmts.Add(*std::move(body_or))};
  }

  const LoopDim& dim = *levels[level];

  // The one type-dependent step: build this level's `for` loop. A declared
  // range is fixed; an absent range is runtime-sized, and among those an
  // associative array walks by key while a queue / dynamic array counts by
  // index. Any setup (a sampled `last`, an associative key declaration) lands
  // in `stmts` before the loop.
  std::vector<hir::StmtId> stmts;
  const bool is_associative =
      !dim.range.has_value() && array_type != nullptr &&
      array_type->getCanonicalType().isAssociativeArray();
  LevelLoop loop{};
  if (is_associative) {
    auto loop_or = BuildAssociativeLevel(
        proc, frame, dim, *array, int32_type, span, stmts);
    if (!loop_or) return std::unexpected(std::move(loop_or.error()));
    loop = std::move(*loop_or);
  } else {
    loop = BuildIntegerLevel(
        proc, frame, dim, array, array_type, int32_type, span, stmts);
  }

  // Descend: the inner levels iterate `array[loop_var]`, one type peel deeper.
  // Only built while the array is being threaded (some inner level is runtime
  // sized) and a deeper iterated level remains; the innermost level peels no
  // element type, which also keeps a string (a terminal byte sequence with no
  // array element type) from descending.
  std::optional<hir::ExprId> inner_array;
  const slang::ast::Type* inner_array_type = nullptr;
  if (array.has_value() && level + 1 < levels.size()) {
    inner_array_type = array_type->getCanonicalType().getArrayElementType();
    auto inner_hir_type = proc.Module().InternType(*inner_array_type, span);
    if (!inner_hir_type) {
      return std::unexpected(std::move(inner_hir_type.error()));
    }
    const hir::ExprId index_id = frame.Exprs().Add(
        hir::MakeRefExpr(
            hir::ProceduralVarRef{.var = loop.loop_var}, loop.loop_var_type,
            span));
    inner_array = frame.Exprs().Add(
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
          : body.stmts.Add(
                hir::Stmt{
                    .label = std::nullopt,
                    .data = hir::BlockStmt{.statements = std::move(*inner_or)},
                    .span = span});

  stmts.push_back(body.stmts.Add(
      hir::Stmt{
          .label = std::nullopt,
          .data =
              hir::ForStmt{
                  .init = std::move(loop.init),
                  .condition = loop.condition,
                  .step = std::move(loop.step),
                  .body = inner_body,
                  .break_label = (level == 0 && *label_used)
                                     ? std::optional{foreach_label}
                                     : std::nullopt},
          .span = span}));
  return stmts;
}

}  // namespace

auto ProcessLowerer::LowerForeachStmt(
    const slang::ast::ForeachLoopStatement& fs, WalkFrame frame)
    -> diag::Result<hir::Stmt> {
  auto& proc = *this;
  auto& module = proc.Module();
  auto& body = *frame.current_procedural_body;
  const auto span = module.SourceMapper().SpanOf(fs.sourceRange);

  const hir::TypeId int32_type = module.Unit().builtins.int32;

  // Collect the iterated (non-skipped) dimensions in cardinal order. A
  // dimension with no constant range reads the array value at run time -- a
  // dynamic array / queue for its size (LRM 7.5 / 7.10), an associative array
  // for its keys (LRM 7.9) -- so the array must be threaded into the nest.
  std::vector<const LoopDim*> levels;
  levels.reserve(fs.loopDims.size());
  bool needs_array = false;
  for (const auto& dim : fs.loopDims) {
    if (dim.loopVar == nullptr) {
      continue;
    }
    needs_array = needs_array || !dim.range.has_value();
    levels.push_back(&dim);
  }

  // All dimensions skipped (`foreach (a[])`): the array reference is still
  // evaluated for side effects and the body runs once. No loop, so a break in
  // the body is a plain innermost exit.
  if (levels.empty()) {
    auto array_or = proc.LowerExpr(fs.arrayRef, frame);
    if (!array_or) return std::unexpected(std::move(array_or.error()));
    const auto array_eval_stmt = body.stmts.Add(
        hir::Stmt{
            .label = std::nullopt,
            .data =
                hir::ExprStmt{.expr = frame.Exprs().Add(*std::move(array_or))},
            .span = span});
    auto body_or = proc.LowerStmt(fs.body, frame.WithoutBreakLabel());
    if (!body_or) return std::unexpected(std::move(body_or.error()));
    const auto body_stmt_id = body.stmts.Add(*std::move(body_or));
    return hir::Stmt{
        .label = std::nullopt,
        .data = hir::BlockStmt{.statements = {array_eval_stmt, body_stmt_id}},
        .span = span};
  }

  // Thread `arrayRef` into the nest only when some dimension reads it at run
  // time; a purely fixed foreach lowers to plain range loops that never touch
  // the array.
  std::optional<hir::ExprId> array;
  const slang::ast::Type* array_type = nullptr;
  if (needs_array) {
    auto array_or = proc.LowerExpr(fs.arrayRef, frame);
    if (!array_or) return std::unexpected(std::move(array_or.error()));
    array = frame.Exprs().Add(*std::move(array_or));
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
