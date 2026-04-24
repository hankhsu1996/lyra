#include "lyra/lowering/ast_to_hir/statement_foreach.hpp"

#include <cstddef>
#include <cstdint>
#include <format>
#include <optional>
#include <vector>

#include <slang/ast/symbols/VariableSymbols.h>
#include <slang/ast/types/AllTypes.h>

#include "lyra/common/constant_arena.hpp"
#include "lyra/common/diagnostic/diagnostic_sink.hpp"
#include "lyra/common/integral_constant.hpp"
#include "lyra/common/scope_types.hpp"
#include "lyra/common/source_span.hpp"
#include "lyra/common/symbol_types.hpp"
#include "lyra/common/type.hpp"
#include "lyra/common/type_queries.hpp"
#include "lyra/hir/arena.hpp"
#include "lyra/hir/expression.hpp"
#include "lyra/hir/fwd.hpp"
#include "lyra/hir/operator.hpp"
#include "lyra/hir/rvalue.hpp"
#include "lyra/hir/statement.hpp"
#include "lyra/lowering/ast_to_hir/context.hpp"
#include "lyra/lowering/ast_to_hir/expression.hpp"
#include "lyra/lowering/ast_to_hir/module_lowerer.hpp"
#include "lyra/lowering/ast_to_hir/statement.hpp"
#include "lyra/lowering/ast_to_hir/symbol_registrar.hpp"
#include "lyra/lowering/ast_to_hir/type.hpp"

namespace lyra::lowering::ast_to_hir {

namespace {

auto MakeIntConstant(int64_t value, TypeId type, SourceSpan span, Context* ctx)
    -> hir::ExpressionId {
  const Type& type_info = (*ctx->type_arena)[type];
  uint32_t bit_width = PackedBitWidth(type_info, *ctx->type_arena);

  auto masked_value = static_cast<uint64_t>(value);
  if (bit_width < 64) {
    masked_value &= (static_cast<uint64_t>(1) << bit_width) - 1;
  }

  IntegralConstant constant;
  constant.value.push_back(masked_value);
  constant.unknown.push_back(0);
  ConstId cid = ctx->active_constant_arena->Intern(type, std::move(constant));
  return ctx->hir_arena->AddExpression(
      hir::Expression{
          .kind = hir::ExpressionKind::kConstant,
          .type = type,
          .span = span,
          .data = hir::ConstantExpressionData{.constant = cid}});
}

auto MakeNameRef(SymbolId sym, TypeId type, SourceSpan span, Context* ctx)
    -> hir::ExpressionId {
  return ctx->hir_arena->AddExpression(
      hir::Expression{
          .kind = hir::ExpressionKind::kNameRef,
          .type = type,
          .span = span,
          .data = hir::NameRefExpressionData{.symbol = sym}});
}

auto GetArrayElementType(const slang::ast::Type* array_type)
    -> const slang::ast::Type* {
  auto type_kind = array_type->getCanonicalType().kind;
  if (type_kind == slang::ast::SymbolKind::FixedSizeUnpackedArrayType) {
    return &array_type->as<slang::ast::FixedSizeUnpackedArrayType>()
                .elementType;
  }
  if (type_kind == slang::ast::SymbolKind::DynamicArrayType) {
    return &array_type->as<slang::ast::DynamicArrayType>().elementType;
  }
  if (array_type->isQueue()) {
    return &array_type->as<slang::ast::QueueType>().elementType;
  }
  if (array_type->isPackedArray()) {
    return &array_type->as<slang::ast::PackedArrayType>().elementType;
  }
  return nullptr;
}

auto GetSelectKind(const slang::ast::Type* array_type) -> hir::ExpressionKind {
  if (array_type->isPackedArray()) {
    return hir::ExpressionKind::kPackedElementSelect;
  }
  if (array_type->isIntegral()) {
    return hir::ExpressionKind::kBitSelect;
  }
  return hir::ExpressionKind::kElementAccess;
}

auto BuildForLoop(
    hir::ExpressionId init_assign, hir::ExpressionId condition,
    hir::ExpressionId step, hir::StatementId body, SourceSpan span,
    Context* ctx) -> hir::StatementId {
  return ctx->hir_arena->AddStatement(
      hir::Statement{
          .kind = hir::StatementKind::kForLoop,
          .span = span,
          .data =
              hir::ForLoopStatementData{
                  .var_decls = {},
                  .init_exprs = {init_assign},
                  .condition = condition,
                  .steps = {step},
                  .body = body,
              },
      });
}

// Lower associative array foreach via snapshot-based desugaring.
// foreach (aa[k]) { body; }  desugars to:
//   { auto __keys = aa.__snapshot();
//     for (int __i = 0; __i < __keys.size(); __i++) {
//       key_type k = __keys[__i]; body; } }
auto LowerAssociativeForeach(
    const slang::ast::ForeachLoopStatement& fs, ScopeLowerer& lowerer)
    -> std::optional<hir::StatementId> {
  auto& registrar = lowerer.Registrar();
  auto* ctx = &lowerer.Ctx();
  SourceSpan span = ctx->SpanOf(fs.sourceRange);

  auto lower_expr = [&](const slang::ast::Expression& expr) {
    return LowerScopedExpression(expr, *ctx, registrar, lowerer.Frame());
  };

  if (fs.loopDims.size() != 1 || fs.loopDims[0].loopVar == nullptr) {
    ctx->sink->Error(
        span,
        "associative array foreach requires exactly "
        "one loop variable");
    return hir::kInvalidStatementId;
  }

  ScopeGuard scope_guard(registrar, ScopeKind::kBlock);

  hir::ExpressionId aa_expr = lower_expr(fs.arrayRef);
  if (!aa_expr) return hir::kInvalidStatementId;

  TypeId aa_type = LowerType(*fs.arrayRef.type, span, ctx);
  if (!aa_type) return hir::kInvalidStatementId;
  TypeId key_type_id = ForeachSnapshotKeyType(aa_type, ctx);
  TypeId keys_array_type = ctx->type_arena->Intern(
      TypeKind::kDynamicArray, DynamicArrayInfo{.element_type = key_type_id});

  hir::ExpressionId snapshot_call = ctx->hir_arena->AddExpression(
      hir::Expression{
          .kind = hir::ExpressionKind::kBuiltinMethodCall,
          .type = keys_array_type,
          .span = span,
          .data = hir::BuiltinMethodCallExpressionData{
              .receiver = aa_expr,
              .method = hir::BuiltinMethod::kAssocSnapshot,
              .args = {}}});

  SymbolId keys_sym = registrar.RegisterSynthetic(
      ctx->MakeTempName("aa_keys"), SymbolKind::kVariable, keys_array_type);

  hir::StatementId keys_decl = ctx->hir_arena->AddStatement(
      hir::Statement{
          .kind = hir::StatementKind::kVariableDeclaration,
          .span = span,
          .data = hir::VariableDeclarationStatementData{
              .symbol = keys_sym,
              .initializer = hir::RValue::Expression(snapshot_call)}});

  TypeId key_var_type = LowerType(fs.loopDims[0].loopVar->getType(), span, ctx);
  if (!key_var_type) return hir::kInvalidStatementId;

  SymbolId key_sym = registrar.Register(
      *fs.loopDims[0].loopVar, SymbolKind::kVariable, key_var_type,
      StorageClass::kLocalStorage);

  hir::StatementId key_decl = ctx->hir_arena->AddStatement(
      hir::Statement{
          .kind = hir::StatementKind::kVariableDeclaration,
          .span = span,
          .data = hir::VariableDeclarationStatementData{
              .symbol = key_sym, .initializer = std::nullopt}});

  TypeId int_type = ctx->IntType();

  SymbolId idx_sym = registrar.RegisterSynthetic(
      ctx->MakeTempName("aa_idx"), SymbolKind::kVariable, int_type);

  hir::StatementId idx_decl = ctx->hir_arena->AddStatement(
      hir::Statement{
          .kind = hir::StatementKind::kVariableDeclaration,
          .span = span,
          .data = hir::VariableDeclarationStatementData{
              .symbol = idx_sym, .initializer = std::nullopt}});

  auto body_result = LowerStatement(fs.body, lowerer);
  if (!body_result.has_value()) {
    ctx->sink->Error(span, "foreach loop body cannot be empty");
    return hir::kInvalidStatementId;
  }
  if (!*body_result) return hir::kInvalidStatementId;

  // Build: k = __keys[__i]
  hir::ExpressionId keys_ref = ctx->hir_arena->AddExpression(
      hir::Expression{
          .kind = hir::ExpressionKind::kNameRef,
          .type = keys_array_type,
          .span = span,
          .data = hir::NameRefExpressionData{.symbol = keys_sym}});

  hir::ExpressionId idx_ref = MakeNameRef(idx_sym, int_type, span, ctx);

  hir::ExpressionId key_element = ctx->hir_arena->AddExpression(
      hir::Expression{
          .kind = hir::ExpressionKind::kElementAccess,
          .type = key_type_id,
          .span = span,
          .data = hir::ElementAccessExpressionData{
              .base = keys_ref, .index = idx_ref}});

  hir::ExpressionId key_ref = MakeNameRef(key_sym, key_var_type, span, ctx);

  hir::ExpressionId key_assign = ctx->hir_arena->AddExpression(
      hir::Expression{
          .kind = hir::ExpressionKind::kAssignment,
          .type = key_var_type,
          .span = span,
          .data = hir::AssignmentExpressionData{
              .target = key_ref,
              .value = key_element,
              .is_non_blocking = false}});

  hir::StatementId key_assign_stmt = ctx->hir_arena->AddStatement(
      hir::Statement{
          .kind = hir::StatementKind::kExpression,
          .span = span,
          .data = hir::ExpressionStatementData{.expression = key_assign}});

  hir::StatementId loop_body = ctx->hir_arena->AddStatement(
      hir::Statement{
          .kind = hir::StatementKind::kBlock,
          .span = span,
          .data = hir::BlockStatementData{
              .statements = {key_assign_stmt, *body_result}}});

  // Build: __keys.size()
  hir::ExpressionId keys_ref2 = ctx->hir_arena->AddExpression(
      hir::Expression{
          .kind = hir::ExpressionKind::kNameRef,
          .type = keys_array_type,
          .span = span,
          .data = hir::NameRefExpressionData{.symbol = keys_sym}});

  hir::ExpressionId size_call = ctx->hir_arena->AddExpression(
      hir::Expression{
          .kind = hir::ExpressionKind::kBuiltinMethodCall,
          .type = int_type,
          .span = span,
          .data = hir::BuiltinMethodCallExpressionData{
              .receiver = keys_ref2,
              .method = hir::BuiltinMethod::kSize,
              .args = {}}});

  // Build for loop: for (__i = 0; __i < size; __i++)
  hir::ExpressionId idx_ref2 = MakeNameRef(idx_sym, int_type, span, ctx);

  TypeId bool_type = ctx->BitType();

  hir::ExpressionId condition = ctx->hir_arena->AddExpression(
      hir::Expression{
          .kind = hir::ExpressionKind::kBinaryOp,
          .type = bool_type,
          .span = span,
          .data = hir::BinaryExpressionData{
              .op = hir::BinaryOp::kLessThan,
              .lhs = idx_ref2,
              .rhs = size_call}});

  hir::ExpressionId idx_ref3 = MakeNameRef(idx_sym, int_type, span, ctx);
  hir::ExpressionId step = ctx->hir_arena->AddExpression(
      hir::Expression{
          .kind = hir::ExpressionKind::kUnaryOp,
          .type = int_type,
          .span = span,
          .data = hir::UnaryExpressionData{
              .op = hir::UnaryOp::kPostincrement, .operand = idx_ref3}});

  hir::ExpressionId init_val = MakeIntConstant(0, int_type, span, ctx);

  hir::ExpressionId init_idx_ref = MakeNameRef(idx_sym, int_type, span, ctx);
  hir::ExpressionId init_assign = ctx->hir_arena->AddExpression(
      hir::Expression{
          .kind = hir::ExpressionKind::kAssignment,
          .type = int_type,
          .span = span,
          .data = hir::AssignmentExpressionData{
              .target = init_idx_ref, .value = init_val}});

  hir::StatementId for_loop =
      BuildForLoop(init_assign, condition, step, loop_body, span, ctx);

  return ctx->hir_arena->AddStatement(
      hir::Statement{
          .kind = hir::StatementKind::kBlock,
          .span = span,
          .data = hir::BlockStatementData{
              .statements = {keys_decl, key_decl, idx_decl, for_loop}}});
}

// Lower regular (non-associative) foreach with nested for loops.
auto LowerRegularForeach(
    const slang::ast::ForeachLoopStatement& fs, ScopeLowerer& lowerer)
    -> std::optional<hir::StatementId> {
  auto& registrar = lowerer.Registrar();
  auto* ctx = &lowerer.Ctx();
  SourceSpan span = ctx->SpanOf(fs.sourceRange);

  auto lower_expr = [&](const slang::ast::Expression& expr) {
    return LowerScopedExpression(expr, *ctx, registrar, lowerer.Frame());
  };

  // Count active dimensions (non-skipped)
  size_t active_count = 0;
  for (const auto& dim : fs.loopDims) {
    if (dim.loopVar != nullptr) {
      ++active_count;
    }
  }

  // Reject skipped-before-active for dynamic dimensions.
  bool seen_skip = false;
  for (const auto& dim : fs.loopDims) {
    if (dim.loopVar == nullptr) {
      seen_skip = true;
    } else if (seen_skip && !dim.range.has_value()) {
      ctx->sink->Error(
          span,
          "foreach with skipped dimension before dynamic dimension is not "
          "supported");
      return hir::kInvalidStatementId;
    }
  }

  // If all dimensions are skipped, evaluate arrayRef for side effects,
  // then execute body once
  if (active_count == 0) {
    hir::ExpressionId array_expr = lower_expr(fs.arrayRef);
    if (!array_expr) {
      return hir::kInvalidStatementId;
    }

    hir::StatementId array_eval = ctx->hir_arena->AddStatement(
        hir::Statement{
            .kind = hir::StatementKind::kExpression,
            .span = span,
            .data = hir::ExpressionStatementData{.expression = array_expr},
        });

    auto body_result = LowerStatement(fs.body, lowerer);
    if (!body_result.has_value()) {
      return array_eval;
    }
    if (!*body_result) {
      return hir::kInvalidStatementId;
    }

    return ctx->hir_arena->AddStatement(
        hir::Statement{
            .kind = hir::StatementKind::kBlock,
            .span = span,
            .data =
                hir::BlockStatementData{
                    .statements = {array_eval, *body_result}},
        });
  }

  ScopeGuard scope_guard(registrar, ScopeKind::kBlock);

  // Declare all loop variables upfront (no init expression)
  std::vector<hir::StatementId> var_decls;
  for (const auto& dim : fs.loopDims) {
    if (dim.loopVar == nullptr) {
      continue;
    }

    TypeId var_type = LowerType(dim.loopVar->getType(), span, ctx);
    if (!var_type) {
      return hir::kInvalidStatementId;
    }

    SymbolId sym = registrar.Register(
        *dim.loopVar, SymbolKind::kVariable, var_type,
        StorageClass::kLocalStorage);

    hir::StatementId var_decl = ctx->hir_arena->AddStatement(
        hir::Statement{
            .kind = hir::StatementKind::kVariableDeclaration,
            .span = span,
            .data =
                hir::VariableDeclarationStatementData{
                    .symbol = sym, .initializer = std::nullopt},
        });
    var_decls.push_back(var_decl);
  }

  // Lower arrayRef expression and store in temp to ensure once-only eval
  hir::ExpressionId array_expr = lower_expr(fs.arrayRef);
  if (!array_expr) {
    return hir::kInvalidStatementId;
  }

  TypeId array_type = LowerType(*fs.arrayRef.type, span, ctx);
  if (!array_type) {
    return hir::kInvalidStatementId;
  }

  SymbolId array_temp_sym = registrar.RegisterSynthetic(
      ctx->MakeTempName("foreach_arr"), SymbolKind::kVariable, array_type);

  hir::StatementId temp_decl = ctx->hir_arena->AddStatement(
      hir::Statement{
          .kind = hir::StatementKind::kVariableDeclaration,
          .span = span,
          .data =
              hir::VariableDeclarationStatementData{
                  .symbol = array_temp_sym,
                  .initializer = hir::RValue::Expression(array_expr)},
      });
  var_decls.push_back(temp_decl);

  hir::ExpressionId array_temp_ref = ctx->hir_arena->AddExpression(
      hir::Expression{
          .kind = hir::ExpressionKind::kNameRef,
          .type = array_type,
          .span = span,
          .data = hir::NameRefExpressionData{.symbol = array_temp_sym}});

  // Build subarray expressions for each dimension (forward order)
  std::vector<hir::ExpressionId> dim_arrays(fs.loopDims.size());
  hir::ExpressionId current_array = array_temp_ref;
  const slang::ast::Type* current_type = &fs.arrayRef.type->getCanonicalType();

  for (size_t i = 0; i < fs.loopDims.size(); ++i) {
    dim_arrays[i] = current_array;

    if (fs.loopDims[i].loopVar != nullptr) {
      SymbolId var_sym = registrar.Lookup(*fs.loopDims[i].loopVar);
      TypeId var_type = LowerType(fs.loopDims[i].loopVar->getType(), span, ctx);
      if (!var_type) {
        return hir::kInvalidStatementId;
      }

      hir::ExpressionId index_ref = MakeNameRef(var_sym, var_type, span, ctx);

      const slang::ast::Type* elem_type = GetArrayElementType(current_type);
      hir::ExpressionKind select_kind = GetSelectKind(current_type);

      TypeId elem_type_id;
      if (select_kind == hir::ExpressionKind::kBitSelect) {
        elem_type_id =
            current_type->isFourState() ? ctx->LogicType() : ctx->BitType();
      } else if (elem_type != nullptr) {
        elem_type_id = LowerType(*elem_type, span, ctx);
      } else {
        ctx->ErrorFmt(
            span, "unexpected array type in foreach: {}",
            toString(current_type->getCanonicalType().kind));
        return hir::kInvalidStatementId;
      }

      if (!elem_type_id) {
        return hir::kInvalidStatementId;
      }

      if (select_kind == hir::ExpressionKind::kElementAccess) {
        current_array = ctx->hir_arena->AddExpression(
            hir::Expression{
                .kind = hir::ExpressionKind::kElementAccess,
                .type = elem_type_id,
                .span = span,
                .data =
                    hir::ElementAccessExpressionData{
                        .base = current_array, .index = index_ref},
            });
      } else if (select_kind == hir::ExpressionKind::kPackedElementSelect) {
        current_array = ctx->hir_arena->AddExpression(
            hir::Expression{
                .kind = hir::ExpressionKind::kPackedElementSelect,
                .type = elem_type_id,
                .span = span,
                .data =
                    hir::PackedElementSelectExpressionData{
                        .base = current_array, .index = index_ref},
            });
      } else {
        current_array = ctx->hir_arena->AddExpression(
            hir::Expression{
                .kind = hir::ExpressionKind::kBitSelect,
                .type = elem_type_id,
                .span = span,
                .data =
                    hir::BitSelectExpressionData{
                        .base = current_array, .index = index_ref},
            });
      }

      if (elem_type != nullptr) {
        current_type = elem_type;
      }
    }
  }

  // Lower body
  auto body_result = LowerStatement(fs.body, lowerer);
  if (!body_result.has_value()) {
    ctx->sink->Error(span, "foreach loop body cannot be empty");
    return hir::kInvalidStatementId;
  }
  if (!*body_result) {
    return hir::kInvalidStatementId;
  }

  // Build nested for loops (reverse order: innermost first)
  hir::StatementId current = *body_result;

  TypeId int_type = ctx->IntType();

  for (auto dim_it = fs.loopDims.rbegin(); dim_it != fs.loopDims.rend();
       ++dim_it) {
    const auto& dim = *dim_it;
    if (dim.loopVar == nullptr) {
      continue;
    }

    auto dim_idx =
        static_cast<size_t>(std::distance(dim_it, fs.loopDims.rend()) - 1);

    TypeId iter_type = LowerType(dim.loopVar->getType(), span, ctx);
    if (!iter_type) {
      return hir::kInvalidStatementId;
    }

    SymbolId loop_var_sym = registrar.Lookup(*dim.loopVar);
    hir::ExpressionId var_ref = MakeNameRef(loop_var_sym, iter_type, span, ctx);

    hir::ExpressionId init_val;
    hir::ExpressionId bound;
    hir::BinaryOp cmp_op = hir::BinaryOp::kLessThan;
    hir::UnaryOp step_op = hir::UnaryOp::kPostincrement;

    if (dim.range.has_value()) {
      const auto& range = *dim.range;
      bool descending = range.left > range.right;

      init_val = MakeIntConstant(range.left, iter_type, span, ctx);
      bound = MakeIntConstant(range.right, iter_type, span, ctx);

      if (descending) {
        cmp_op = hir::BinaryOp::kGreaterThanEqual;
        step_op = hir::UnaryOp::kPostdecrement;
      } else {
        cmp_op = hir::BinaryOp::kLessThanEqual;
        step_op = hir::UnaryOp::kPostincrement;
      }
    } else {
      init_val = MakeIntConstant(0, iter_type, span, ctx);

      hir::ExpressionId size_call = ctx->hir_arena->AddExpression(
          hir::Expression{
              .kind = hir::ExpressionKind::kBuiltinMethodCall,
              .type = int_type,
              .span = span,
              .data = hir::BuiltinMethodCallExpressionData{
                  .receiver = dim_arrays[dim_idx],
                  .method = hir::BuiltinMethod::kSize,
                  .args = {}}});

      if (iter_type != int_type) {
        bound = ctx->hir_arena->AddExpression(
            hir::Expression{
                .kind = hir::ExpressionKind::kCast,
                .type = iter_type,
                .span = span,
                .data = hir::CastExpressionData{.operand = size_call}});
      } else {
        bound = size_call;
      }

      cmp_op = hir::BinaryOp::kLessThan;
      step_op = hir::UnaryOp::kPostincrement;
    }

    TypeId bool_type = ctx->BitType();

    hir::ExpressionId condition = ctx->hir_arena->AddExpression(
        hir::Expression{
            .kind = hir::ExpressionKind::kBinaryOp,
            .type = bool_type,
            .span = span,
            .data = hir::BinaryExpressionData{
                .op = cmp_op, .lhs = var_ref, .rhs = bound}});

    hir::ExpressionId step_var_ref =
        MakeNameRef(loop_var_sym, iter_type, span, ctx);
    hir::ExpressionId step = ctx->hir_arena->AddExpression(
        hir::Expression{
            .kind = hir::ExpressionKind::kUnaryOp,
            .type = iter_type,
            .span = span,
            .data = hir::UnaryExpressionData{
                .op = step_op, .operand = step_var_ref}});

    hir::ExpressionId init_var_ref =
        MakeNameRef(loop_var_sym, iter_type, span, ctx);
    hir::ExpressionId init_assign = ctx->hir_arena->AddExpression(
        hir::Expression{
            .kind = hir::ExpressionKind::kAssignment,
            .type = iter_type,
            .span = span,
            .data = hir::AssignmentExpressionData{
                .target = init_var_ref, .value = init_val}});

    current = BuildForLoop(init_assign, condition, step, current, span, ctx);
  }

  // Build outer block with var decls + nested loops
  var_decls.push_back(current);
  return ctx->hir_arena->AddStatement(
      hir::Statement{
          .kind = hir::StatementKind::kBlock,
          .span = span,
          .data = hir::BlockStatementData{.statements = std::move(var_decls)},
      });
}

}  // namespace

auto LowerForeachStatement(
    const slang::ast::ForeachLoopStatement& stmt, ScopeLowerer& lowerer)
    -> std::optional<hir::StatementId> {
  if (stmt.arrayRef.type->isAssociativeArray()) {
    return LowerAssociativeForeach(stmt, lowerer);
  }
  return LowerRegularForeach(stmt, lowerer);
}

}  // namespace lyra::lowering::ast_to_hir
