#include "lyra/lowering/ast_to_hir/statement.hpp"

#include <cstddef>
#include <cstdint>
#include <format>
#include <iterator>
#include <optional>
#include <string_view>
#include <utility>
#include <variant>
#include <vector>

#include <slang/ast/TimingControl.h>
#include <slang/ast/expressions/AssignmentExpressions.h>
#include <slang/ast/expressions/CallExpression.h>
#include <slang/ast/expressions/LiteralExpressions.h>
#include <slang/ast/expressions/OperatorExpressions.h>
#include <slang/ast/expressions/SelectExpressions.h>
#include <slang/ast/statements/ConditionalStatements.h>
#include <slang/ast/statements/LoopStatements.h>
#include <slang/ast/statements/MiscStatements.h>
#include <slang/ast/symbols/VariableSymbols.h>
#include <slang/ast/types/AllTypes.h>

#include "lyra/common/constant.hpp"
#include "lyra/common/constant_arena.hpp"
#include "lyra/common/diagnostic/diagnostic_sink.hpp"
#include "lyra/common/integral_constant.hpp"
#include "lyra/common/scope_types.hpp"
#include "lyra/common/source_span.hpp"
#include "lyra/common/symbol_types.hpp"
#include "lyra/common/system_function.hpp"
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
#include "lyra/lowering/ast_to_hir/pattern.hpp"
#include "lyra/lowering/ast_to_hir/symbol_registrar.hpp"
#include "lyra/lowering/ast_to_hir/type.hpp"

namespace lyra::lowering::ast_to_hir {

namespace {

// Helper to create an integer constant expression
auto MakeIntConstant(int64_t value, TypeId type, SourceSpan span, Context* ctx)
    -> hir::ExpressionId {
  // Get bit width from type to properly mask the value
  const Type& type_info = (*ctx->type_arena)[type];
  uint32_t bit_width = PackedBitWidth(type_info, *ctx->type_arena);

  // Mask value to bit width (handles negative values correctly)
  auto masked_value = static_cast<uint64_t>(value);
  if (bit_width < 64) {
    masked_value &= (static_cast<uint64_t>(1) << bit_width) - 1;
  }

  IntegralConstant constant;
  constant.value.push_back(masked_value);
  constant.unknown.push_back(0);
  ConstId cid = ctx->constant_arena->Intern(type, std::move(constant));
  return ctx->hir_arena->AddExpression(
      hir::Expression{
          .kind = hir::ExpressionKind::kConstant,
          .type = type,
          .span = span,
          .data = hir::ConstantExpressionData{.constant = cid}});
}

// Helper to create a name reference expression
auto MakeNameRef(SymbolId sym, TypeId type, SourceSpan span, Context* ctx)
    -> hir::ExpressionId {
  return ctx->hir_arena->AddExpression(
      hir::Expression{
          .kind = hir::ExpressionKind::kNameRef,
          .type = type,
          .span = span,
          .data = hir::NameRefExpressionData{.symbol = sym}});
}

// Helper to get the element type of an array type (for foreach dimension
// traversal)
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
  return nullptr;  // Not an array type (e.g., integral for bit select)
}

// Determine the expression kind for indexing into an array type
auto GetSelectKind(const slang::ast::Type* array_type) -> hir::ExpressionKind {
  if (array_type->isPackedArray()) {
    return hir::ExpressionKind::kPackedElementSelect;
  }
  if (array_type->isIntegral()) {
    return hir::ExpressionKind::kBitSelect;
  }
  return hir::ExpressionKind::kElementAccess;
}

// Build a for loop statement from loop components
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

}  // namespace

auto LowerStatement(const slang::ast::Statement& stmt, ScopeLowerer& lowerer)
    -> std::optional<hir::StatementId> {
  // Alias for convenience
  auto& registrar = lowerer.Registrar();
  auto* ctx = &lowerer.Ctx();
  const auto* frame = &lowerer.Frame();

  // Helper to lower expressions using the public API
  auto lower_expr = [&](const slang::ast::Expression& expr) {
    return LowerScopedExpression(expr, *ctx, registrar, *frame);
  };

  using slang::ast::StatementKind;

  switch (stmt.kind) {
    case StatementKind::Empty: {
      return std::nullopt;
    }

    case StatementKind::List: {
      const auto& list = stmt.as<slang::ast::StatementList>();
      SourceSpan span = ctx->SpanOf(stmt.sourceRange);

      ScopeGuard scope_guard(registrar, ScopeKind::kBlock);

      std::vector<hir::StatementId> children;
      children.reserve(list.list.size());
      for (const slang::ast::Statement* child : list.list) {
        auto result = LowerStatement(*child, lowerer);
        if (!result.has_value()) {
          continue;  // Empty statement, skip
        }
        if (!*result) {
          return hir::kInvalidStatementId;
        }
        children.push_back(*result);
      }

      return ctx->hir_arena->AddStatement(
          hir::Statement{
              .kind = hir::StatementKind::kBlock,
              .span = span,
              .data =
                  hir::BlockStatementData{.statements = std::move(children)}});
    }

    case StatementKind::Block: {
      const auto& block = stmt.as<slang::ast::BlockStatement>();
      return LowerStatement(block.body, lowerer);
    }

    case StatementKind::VariableDeclaration: {
      const auto& var_decl = stmt.as<slang::ast::VariableDeclStatement>();
      SourceSpan span = ctx->SpanOf(stmt.sourceRange);
      const slang::ast::VariableSymbol& var_sym = var_decl.symbol;

      TypeId type = LowerType(var_sym.getType(), span, ctx);
      if (!type) {
        return hir::kInvalidStatementId;
      }

      SymbolId sym = registrar.Register(
          var_sym, SymbolKind::kVariable, type, StorageClass::kLocalStorage);

      std::optional<hir::RValue> initializer;
      if (const slang::ast::Expression* init_expr = var_sym.getInitializer()) {
        // Check if initializer is a structured assignment pattern
        if (init_expr->kind ==
            slang::ast::ExpressionKind::StructuredAssignmentPattern) {
          const auto& pattern_expr =
              init_expr
                  ->as<slang::ast::StructuredAssignmentPatternExpression>();

          // Check if target type is a packed container
          const Type& var_type = (*ctx->type_arena)[type];
          if (var_type.Kind() == TypeKind::kPackedArray ||
              var_type.Kind() == TypeKind::kIntegral) {
            // Lower as Pattern
            ExpressionLoweringView view{
                .context = ctx, .registrar = &registrar, .frame = frame};
            hir::PatternId pattern_id =
                LowerPattern(pattern_expr, type, span, view);

            if (!pattern_id) {
              return hir::kInvalidStatementId;
            }

            // Store as Pattern RValue
            initializer = hir::RValue::Pattern(pattern_id);
          } else {
            // Unpacked containers still use expression path for now
            hir::ExpressionId init_id = lower_expr(*init_expr);
            if (!init_id) {
              return hir::kInvalidStatementId;
            }
            initializer = hir::RValue::Expression(init_id);
          }
        } else {
          // Regular expression initialization
          hir::ExpressionId init_id = lower_expr(*init_expr);
          if (!init_id) {
            return hir::kInvalidStatementId;
          }
          initializer = hir::RValue::Expression(init_id);
        }
      }

      return ctx->hir_arena->AddStatement(
          hir::Statement{
              .kind = hir::StatementKind::kVariableDeclaration,
              .span = span,
              .data = hir::VariableDeclarationStatementData{
                  .symbol = sym, .initializer = initializer}});
    }

    case StatementKind::ExpressionStatement: {
      const auto& expr_stmt = stmt.as<slang::ast::ExpressionStatement>();
      SourceSpan span = ctx->SpanOf(stmt.sourceRange);

      // Check if this is a termination system call ($finish, $stop, $exit)
      if (expr_stmt.expr.kind == slang::ast::ExpressionKind::Call) {
        const auto& call = expr_stmt.expr.as<slang::ast::CallExpression>();
        if (call.isSystemCall()) {
          std::string_view name = call.getSubroutineName();
          const SystemFunctionInfo* info = FindSystemFunction(name);
          if (info != nullptr) {
            if (const auto* term_info =
                    std::get_if<TerminationFunctionInfo>(&info->payload)) {
              // Parse the level argument (default if not provided)
              int level = term_info->default_level;
              if (!call.arguments().empty()) {
                const slang::ast::Expression& arg = *call.arguments()[0];
                if (arg.kind == slang::ast::ExpressionKind::IntegerLiteral) {
                  const auto& literal = arg.as<slang::ast::IntegerLiteral>();
                  auto val = literal.getValue();
                  if (val.hasUnknown()) {
                    ctx->sink->Error(
                        span, "termination level cannot contain X or Z");
                    return hir::kInvalidStatementId;
                  }
                  level = static_cast<int>(val.as<int>().value());
                } else {
                  ctx->sink->Error(
                      span, "termination level must be a constant integer");
                  return hir::kInvalidStatementId;
                }
              }

              // Map TerminationType to HIR TerminationKind
              hir::TerminationKind kind = hir::TerminationKind::kFinish;
              switch (term_info->type) {
                case TerminationType::kFinish:
                  kind = hir::TerminationKind::kFinish;
                  break;
                case TerminationType::kStop:
                  kind = hir::TerminationKind::kStop;
                  break;
                case TerminationType::kExit:
                  kind = hir::TerminationKind::kExit;
                  break;
              }

              return ctx->hir_arena->AddStatement(
                  hir::Statement{
                      .kind = hir::StatementKind::kTerminate,
                      .span = span,
                      .data =
                          hir::TerminateStatementData{
                              .kind = kind, .level = level, .message_args = {}},
                  });
            }

            // Handle $fatal - terminating severity message
            if (std::get_if<FatalFunctionInfo>(&info->payload) != nullptr) {
              // $fatal([finish_number], [fmt, args...])
              // LRM signature: first arg is optional finish_number (integer
              // literal), remaining args are format string and values.
              //
              // Disambiguation:
              // - Integer literal -> level, remaining args are message
              // - String literal -> format string, level defaults to 1
              // - Other expression -> error (level must be compile-time
              // literal)
              int level = 1;
              size_t message_start = 0;

              if (!call.arguments().empty()) {
                const slang::ast::Expression& first_arg = *call.arguments()[0];
                if (first_arg.kind ==
                    slang::ast::ExpressionKind::IntegerLiteral) {
                  // Integer literal: extract as level
                  const auto& literal =
                      first_arg.as<slang::ast::IntegerLiteral>();
                  auto val = literal.getValue();
                  if (val.hasUnknown()) {
                    ctx->sink->Error(
                        span, "$fatal level cannot contain X or Z");
                    return hir::kInvalidStatementId;
                  }
                  level = static_cast<int>(val.as<int>().value());
                  message_start = 1;
                } else if (
                    first_arg.kind ==
                    slang::ast::ExpressionKind::StringLiteral) {
                  // String literal: format string, level defaults to 1
                  message_start = 0;
                } else if (first_arg.type->isIntegral()) {
                  // Non-literal integral expression: error
                  // (could be a level, but we require literals)
                  ctx->sink->Error(
                      span, "$fatal level must be a constant integer literal");
                  return hir::kInvalidStatementId;
                } else {
                  // Non-integral, non-string: treat as message arg
                  message_start = 0;
                }
              }

              // Lower remaining arguments as message
              std::vector<hir::ExpressionId> message_args;
              for (size_t i = message_start; i < call.arguments().size(); ++i) {
                hir::ExpressionId arg_id = lower_expr(*call.arguments()[i]);
                if (!arg_id) {
                  return hir::kInvalidStatementId;
                }
                message_args.push_back(arg_id);
              }

              return ctx->hir_arena->AddStatement(
                  hir::Statement{
                      .kind = hir::StatementKind::kTerminate,
                      .span = span,
                      .data =
                          hir::TerminateStatementData{
                              .kind = hir::TerminationKind::kFatal,
                              .level = level,
                              .message_args = std::move(message_args)},
                  });
            }
          }
        }
      }

      // Not a termination call - lower as regular expression statement
      // All builtin methods (including void methods like push_back, delete)
      // are handled uniformly through LowerExpression.
      hir::ExpressionId expr = lower_expr(expr_stmt.expr);
      if (!expr) {
        return hir::kInvalidStatementId;
      }
      return ctx->hir_arena->AddStatement(
          hir::Statement{
              .kind = hir::StatementKind::kExpression,
              .span = span,
              .data = hir::ExpressionStatementData{.expression = expr},
          });
    }

    case StatementKind::Conditional: {
      const auto& cond_stmt = stmt.as<slang::ast::ConditionalStatement>();
      SourceSpan span = ctx->SpanOf(stmt.sourceRange);

      if (cond_stmt.conditions.size() != 1 ||
          cond_stmt.conditions[0].pattern != nullptr) {
        ctx->sink->Error(span, "only simple if conditions are supported");
        return hir::kInvalidStatementId;
      }

      // Map slang check to HIR check
      hir::UniquePriorityCheck hir_check = hir::UniquePriorityCheck::kNone;
      switch (cond_stmt.check) {
        case slang::ast::UniquePriorityCheck::None:
          hir_check = hir::UniquePriorityCheck::kNone;
          break;
        case slang::ast::UniquePriorityCheck::Unique:
          hir_check = hir::UniquePriorityCheck::kUnique;
          break;
        case slang::ast::UniquePriorityCheck::Unique0:
          hir_check = hir::UniquePriorityCheck::kUnique0;
          break;
        case slang::ast::UniquePriorityCheck::Priority:
          hir_check = hir::UniquePriorityCheck::kPriority;
          break;
      }

      hir::ExpressionId condition = lower_expr(*cond_stmt.conditions[0].expr);
      if (!condition) {
        return hir::kInvalidStatementId;
      }

      auto then_result = LowerStatement(cond_stmt.ifTrue, lowerer);
      if (!then_result.has_value()) {
        ctx->sink->Error(span, "if-true branch cannot be empty");
        return hir::kInvalidStatementId;
      }
      if (!*then_result) {
        return hir::kInvalidStatementId;
      }
      hir::StatementId then_branch = *then_result;

      std::optional<hir::StatementId> else_branch;
      if (cond_stmt.ifFalse != nullptr) {
        auto else_result = LowerStatement(*cond_stmt.ifFalse, lowerer);
        if (!else_result.has_value()) {
          ctx->sink->Error(span, "if-false branch cannot be empty");
          return hir::kInvalidStatementId;
        }
        if (!*else_result) {
          return hir::kInvalidStatementId;
        }
        else_branch = *else_result;
      }

      return ctx->hir_arena->AddStatement(
          hir::Statement{
              .kind = hir::StatementKind::kConditional,
              .span = span,
              .data = hir::ConditionalStatementData{
                  .condition = condition,
                  .then_branch = then_branch,
                  .else_branch = else_branch,
                  .check = hir_check}});
    }

    case StatementKind::Case: {
      const auto& case_stmt = stmt.as<slang::ast::CaseStatement>();
      SourceSpan span = ctx->SpanOf(stmt.sourceRange);

      hir::CaseCondition hir_condition = hir::CaseCondition::kNormal;
      switch (case_stmt.condition) {
        case slang::ast::CaseStatementCondition::Normal:
          hir_condition = hir::CaseCondition::kNormal;
          break;
        case slang::ast::CaseStatementCondition::WildcardJustZ:
          hir_condition = hir::CaseCondition::kCaseZ;
          break;
        case slang::ast::CaseStatementCondition::WildcardXOrZ:
          hir_condition = hir::CaseCondition::kCaseX;
          break;
        case slang::ast::CaseStatementCondition::Inside:
          hir_condition = hir::CaseCondition::kInside;
          break;
      }

      // Map slang check to HIR check
      hir::UniquePriorityCheck hir_check = hir::UniquePriorityCheck::kNone;
      switch (case_stmt.check) {
        case slang::ast::UniquePriorityCheck::None:
          hir_check = hir::UniquePriorityCheck::kNone;
          break;
        case slang::ast::UniquePriorityCheck::Unique:
          hir_check = hir::UniquePriorityCheck::kUnique;
          break;
        case slang::ast::UniquePriorityCheck::Unique0:
          hir_check = hir::UniquePriorityCheck::kUnique0;
          break;
        case slang::ast::UniquePriorityCheck::Priority:
          hir_check = hir::UniquePriorityCheck::kPriority;
          break;
      }

      // Lower selector expression
      hir::ExpressionId selector = lower_expr(case_stmt.expr);
      if (!selector) {
        return hir::kInvalidStatementId;
      }

      // 1-bit result type for predicates
      TypeId bit_type = ctx->type_arena->Intern(
          TypeKind::kIntegral,
          IntegralInfo{
              .bit_width = 1, .is_signed = false, .is_four_state = true});

      // Build case items with predicates
      std::vector<hir::CaseItem> items;
      for (const auto& group : case_stmt.items) {
        // Build predicate: OR together all expressions in this item
        std::optional<hir::ExpressionId> raw_pred;

        for (const slang::ast::Expression* expr : group.expressions) {
          hir::ExpressionId item_pred;

          if (hir_condition == hir::CaseCondition::kInside) {
            // Inside: handle ranges and wildcard equality
            if (expr->kind == slang::ast::ExpressionKind::ValueRange) {
              const auto& range = expr->as<slang::ast::ValueRangeExpression>();

              if (range.rangeKind != slang::ast::ValueRangeKind::Simple) {
                ctx->sink->Error(
                    span, "tolerance ranges not supported in case inside");
                return hir::kInvalidStatementId;
              }

              // Range: (selector >= lo) & (selector <= hi)
              hir::ExpressionId lo = lower_expr(range.left());
              if (!lo) {
                return hir::kInvalidStatementId;
              }
              hir::ExpressionId hi = lower_expr(range.right());
              if (!hi) {
                return hir::kInvalidStatementId;
              }

              hir::ExpressionId ge = ctx->hir_arena->AddExpression(
                  hir::Expression{
                      .kind = hir::ExpressionKind::kBinaryOp,
                      .type = bit_type,
                      .span = span,
                      .data = hir::BinaryExpressionData{
                          .op = hir::BinaryOp::kGreaterThanEqual,
                          .lhs = selector,
                          .rhs = lo}});
              hir::ExpressionId le = ctx->hir_arena->AddExpression(
                  hir::Expression{
                      .kind = hir::ExpressionKind::kBinaryOp,
                      .type = bit_type,
                      .span = span,
                      .data = hir::BinaryExpressionData{
                          .op = hir::BinaryOp::kLessThanEqual,
                          .lhs = selector,
                          .rhs = hi}});
              // Use bitwise AND to preserve X propagation
              item_pred = ctx->hir_arena->AddExpression(
                  hir::Expression{
                      .kind = hir::ExpressionKind::kBinaryOp,
                      .type = bit_type,
                      .span = span,
                      .data = hir::BinaryExpressionData{
                          .op = hir::BinaryOp::kBitwiseAnd,
                          .lhs = ge,
                          .rhs = le}});
            } else {
              // Discrete item: choose == or ==? based on X/Z content
              hir::ExpressionId item_val = lower_expr(*expr);
              if (!item_val) {
                return hir::kInvalidStatementId;
              }

              // Use wildcard equality if constant has X/Z
              bool use_wildcard = InsideItemUsesWildcardEq(*expr);
              hir::BinaryOp op = use_wildcard ? hir::BinaryOp::kWildcardEqual
                                              : hir::BinaryOp::kEqual;

              item_pred = ctx->hir_arena->AddExpression(
                  hir::Expression{
                      .kind = hir::ExpressionKind::kBinaryOp,
                      .type = bit_type,
                      .span = span,
                      .data = hir::BinaryExpressionData{
                          .op = op, .lhs = selector, .rhs = item_val}});
            }
          } else {
            // case/casez/casex: use appropriate comparison operator
            hir::ExpressionId item_val = lower_expr(*expr);
            if (!item_val) {
              return hir::kInvalidStatementId;
            }

            hir::BinaryOp cmp_op = hir::BinaryOp::kEqual;
            switch (hir_condition) {
              case hir::CaseCondition::kNormal:
                cmp_op = hir::BinaryOp::kCaseEqual;
                break;
              case hir::CaseCondition::kCaseZ:
                cmp_op = hir::BinaryOp::kCaseZMatch;
                break;
              case hir::CaseCondition::kCaseX:
                cmp_op = hir::BinaryOp::kCaseXMatch;
                break;
              case hir::CaseCondition::kInside:
                break;  // Handled above
            }

            item_pred = ctx->hir_arena->AddExpression(
                hir::Expression{
                    .kind = hir::ExpressionKind::kBinaryOp,
                    .type = bit_type,
                    .span = span,
                    .data = hir::BinaryExpressionData{
                        .op = cmp_op, .lhs = selector, .rhs = item_val}});
          }

          // Combine with OR
          if (raw_pred) {
            raw_pred = ctx->hir_arena->AddExpression(
                hir::Expression{
                    .kind = hir::ExpressionKind::kBinaryOp,
                    .type = bit_type,
                    .span = span,
                    .data = hir::BinaryExpressionData{
                        .op = hir::BinaryOp::kBitwiseOr,
                        .lhs = *raw_pred,
                        .rhs = item_pred}});
          } else {
            raw_pred = item_pred;
          }
        }

        // Apply 2-state clamp: predicate = (raw_pred === 1'b1)
        // This ensures the predicate is always 0 or 1, never X
        hir::ExpressionId predicate;
        if (raw_pred) {
          // Create constant 1'b1
          IntegralConstant one_const;
          one_const.value.push_back(1);
          one_const.unknown.push_back(0);
          ConstId one_id =
              ctx->constant_arena->Intern(bit_type, std::move(one_const));
          hir::ExpressionId one_expr = ctx->hir_arena->AddExpression(
              hir::Expression{
                  .kind = hir::ExpressionKind::kConstant,
                  .type = bit_type,
                  .span = span,
                  .data = hir::ConstantExpressionData{.constant = one_id}});

          // predicate = raw_pred === 1'b1
          predicate = ctx->hir_arena->AddExpression(
              hir::Expression{
                  .kind = hir::ExpressionKind::kBinaryOp,
                  .type = bit_type,
                  .span = span,
                  .data = hir::BinaryExpressionData{
                      .op = hir::BinaryOp::kCaseEqual,
                      .lhs = *raw_pred,
                      .rhs = one_expr}});
        } else {
          // Empty expressions list - create constant 0 (never matches)
          IntegralConstant zero_const;
          zero_const.value.push_back(0);
          zero_const.unknown.push_back(0);
          ConstId zero_id =
              ctx->constant_arena->Intern(bit_type, std::move(zero_const));
          predicate = ctx->hir_arena->AddExpression(
              hir::Expression{
                  .kind = hir::ExpressionKind::kConstant,
                  .type = bit_type,
                  .span = span,
                  .data = hir::ConstantExpressionData{.constant = zero_id}});
        }

        auto body_result = LowerStatement(*group.stmt, lowerer);
        if (body_result.has_value() && !*body_result) {
          return hir::kInvalidStatementId;
        }
        items.push_back({.predicate = predicate, .statement = body_result});
      }

      std::optional<hir::StatementId> default_statement;
      if (case_stmt.defaultCase != nullptr) {
        auto default_result = LowerStatement(*case_stmt.defaultCase, lowerer);
        // nullopt means empty statement (e.g., "default: ;") - valid, just no
        // action
        if (default_result.has_value()) {
          if (!*default_result) {
            return hir::kInvalidStatementId;
          }
          default_statement = *default_result;
        }
      }

      return ctx->hir_arena->AddStatement(
          hir::Statement{
              .kind = hir::StatementKind::kCase,
              .span = span,
              .data = hir::CaseStatementData{
                  .selector = selector,
                  .items = std::move(items),
                  .default_statement = default_statement,
                  .condition = hir_condition,
                  .check = hir_check}});
    }

    case StatementKind::ForLoop: {
      const auto& for_stmt = stmt.as<slang::ast::ForLoopStatement>();
      SourceSpan span = ctx->SpanOf(stmt.sourceRange);

      // RAII guard ensures PopScope on all exit paths
      ScopeGuard scope_guard(registrar, ScopeKind::kBlock);

      std::vector<hir::StatementId> var_decls;
      std::vector<hir::ExpressionId> init_exprs;

      if (!for_stmt.loopVars.empty()) {
        // Loop-declared variables: for (int i = 0; ...)
        for (const slang::ast::VariableSymbol* var_sym : for_stmt.loopVars) {
          TypeId type = LowerType(var_sym->getType(), span, ctx);
          if (!type) {
            return hir::kInvalidStatementId;
          }

          SymbolId sym = registrar.Register(
              *var_sym, SymbolKind::kVariable, type,
              StorageClass::kLocalStorage);

          hir::ExpressionId init = hir::kInvalidExpressionId;
          if (const slang::ast::Expression* init_expr =
                  var_sym->getInitializer()) {
            init = lower_expr(*init_expr);
            if (!init) {
              return hir::kInvalidStatementId;
            }
          }

          std::optional<hir::RValue> initializer;
          if (init) {
            initializer = hir::RValue::Expression(init);
          }
          hir::StatementId var_decl = ctx->hir_arena->AddStatement(
              hir::Statement{
                  .kind = hir::StatementKind::kVariableDeclaration,
                  .span = span,
                  .data =
                      hir::VariableDeclarationStatementData{
                          .symbol = sym, .initializer = initializer},
              });
          var_decls.push_back(var_decl);
        }
      } else {
        // Expression initializers: for (i = 0, j = 3; ...)
        for (const slang::ast::Expression* init_expr : for_stmt.initializers) {
          hir::ExpressionId expr = lower_expr(*init_expr);
          if (!expr) {
            return hir::kInvalidStatementId;
          }
          init_exprs.push_back(expr);
        }
      }

      std::optional<hir::ExpressionId> condition;
      if (for_stmt.stopExpr != nullptr) {
        hir::ExpressionId cond = lower_expr(*for_stmt.stopExpr);
        if (!cond) {
          return hir::kInvalidStatementId;
        }
        condition = cond;
      }

      std::vector<hir::ExpressionId> steps;
      for (const slang::ast::Expression* step_expr : for_stmt.steps) {
        hir::ExpressionId expr = lower_expr(*step_expr);
        if (!expr) {
          return hir::kInvalidStatementId;
        }
        steps.push_back(expr);
      }

      auto body_result = LowerStatement(for_stmt.body, lowerer);
      if (!body_result.has_value()) {
        ctx->sink->Error(span, "for loop body cannot be empty");
        return hir::kInvalidStatementId;
      }
      if (!*body_result) {
        return hir::kInvalidStatementId;
      }

      return ctx->hir_arena->AddStatement(
          hir::Statement{
              .kind = hir::StatementKind::kForLoop,
              .span = span,
              .data =
                  hir::ForLoopStatementData{
                      .var_decls = std::move(var_decls),
                      .init_exprs = std::move(init_exprs),
                      .condition = condition,
                      .steps = std::move(steps),
                      .body = *body_result,
                  },
          });
    }

    case StatementKind::WhileLoop: {
      const auto& while_stmt = stmt.as<slang::ast::WhileLoopStatement>();
      SourceSpan span = ctx->SpanOf(stmt.sourceRange);

      hir::ExpressionId condition = lower_expr(while_stmt.cond);
      if (!condition) {
        return hir::kInvalidStatementId;
      }

      auto body_result = LowerStatement(while_stmt.body, lowerer);
      if (!body_result.has_value()) {
        ctx->sink->Error(span, "while loop body cannot be empty");
        return hir::kInvalidStatementId;
      }
      if (!*body_result) {
        return hir::kInvalidStatementId;
      }

      return ctx->hir_arena->AddStatement(
          hir::Statement{
              .kind = hir::StatementKind::kWhileLoop,
              .span = span,
              .data =
                  hir::WhileLoopStatementData{
                      .condition = condition, .body = *body_result},
          });
    }

    case StatementKind::DoWhileLoop: {
      const auto& dowhile_stmt = stmt.as<slang::ast::DoWhileLoopStatement>();
      SourceSpan span = ctx->SpanOf(stmt.sourceRange);

      hir::ExpressionId condition = lower_expr(dowhile_stmt.cond);
      if (!condition) {
        return hir::kInvalidStatementId;
      }

      auto body_result = LowerStatement(dowhile_stmt.body, lowerer);
      if (!body_result.has_value()) {
        ctx->sink->Error(span, "do-while loop body cannot be empty");
        return hir::kInvalidStatementId;
      }
      if (!*body_result) {
        return hir::kInvalidStatementId;
      }

      return ctx->hir_arena->AddStatement(
          hir::Statement{
              .kind = hir::StatementKind::kDoWhileLoop,
              .span = span,
              .data =
                  hir::DoWhileLoopStatementData{
                      .condition = condition, .body = *body_result},
          });
    }

    case StatementKind::ForeverLoop: {
      const auto& forever_stmt = stmt.as<slang::ast::ForeverLoopStatement>();
      SourceSpan span = ctx->SpanOf(stmt.sourceRange);

      // Create constant "1" expression for while(1)
      // Use 1-bit logic type (standard for boolean conditions)
      TypeId bit_type = ctx->type_arena->Intern(
          TypeKind::kIntegral,
          IntegralInfo{
              .bit_width = 1, .is_signed = false, .is_four_state = true});
      IntegralConstant one_const;
      one_const.value.push_back(1);
      one_const.unknown.push_back(0);
      ConstId const_id =
          ctx->constant_arena->Intern(bit_type, std::move(one_const));
      hir::ExpressionId true_expr = ctx->hir_arena->AddExpression(
          hir::Expression{
              .kind = hir::ExpressionKind::kConstant,
              .type = bit_type,
              .span = span,
              .data = hir::ConstantExpressionData{.constant = const_id},
          });

      auto body_result = LowerStatement(forever_stmt.body, lowerer);
      if (!body_result.has_value()) {
        ctx->sink->Error(span, "forever loop body cannot be empty");
        return hir::kInvalidStatementId;
      }
      if (!*body_result) {
        return hir::kInvalidStatementId;
      }

      return ctx->hir_arena->AddStatement(
          hir::Statement{
              .kind = hir::StatementKind::kWhileLoop,
              .span = span,
              .data =
                  hir::WhileLoopStatementData{
                      .condition = true_expr, .body = *body_result},
          });
    }

    case StatementKind::RepeatLoop: {
      const auto& repeat_stmt = stmt.as<slang::ast::RepeatLoopStatement>();
      SourceSpan span = ctx->SpanOf(stmt.sourceRange);

      hir::ExpressionId count = lower_expr(repeat_stmt.count);
      if (!count) {
        return hir::kInvalidStatementId;
      }

      auto body_result = LowerStatement(repeat_stmt.body, lowerer);
      if (!body_result.has_value()) {
        ctx->sink->Error(span, "repeat loop body cannot be empty");
        return hir::kInvalidStatementId;
      }
      if (!*body_result) {
        return hir::kInvalidStatementId;
      }

      return ctx->hir_arena->AddStatement(
          hir::Statement{
              .kind = hir::StatementKind::kRepeatLoop,
              .span = span,
              .data =
                  hir::RepeatLoopStatementData{
                      .count = count, .body = *body_result},
          });
    }

    case StatementKind::Break: {
      return ctx->hir_arena->AddStatement(
          hir::Statement{
              .kind = hir::StatementKind::kBreak,
              .span = ctx->SpanOf(stmt.sourceRange),
              .data = hir::BreakStatementData{},
          });
    }

    case StatementKind::Continue: {
      return ctx->hir_arena->AddStatement(
          hir::Statement{
              .kind = hir::StatementKind::kContinue,
              .span = ctx->SpanOf(stmt.sourceRange),
              .data = hir::ContinueStatementData{},
          });
    }

    case StatementKind::ForeachLoop: {
      const auto& fs = stmt.as<slang::ast::ForeachLoopStatement>();
      SourceSpan span = ctx->SpanOf(stmt.sourceRange);

      // Reject associative arrays
      if (fs.arrayRef.type->isAssociativeArray()) {
        ctx->sink->Error(span, "foreach over associative arrays not supported");
        return hir::kInvalidStatementId;
      }

      // Count active dimensions (non-skipped)
      size_t active_count = 0;
      for (const auto& dim : fs.loopDims) {
        if (dim.loopVar != nullptr) {
          ++active_count;
        }
      }

      // Reject skipped-before-active for dynamic dimensions.
      // For ragged arrays like int a[][], `foreach(a[,j])` is ill-defined
      // because the bound for j depends on which row, but the row index is
      // skipped.
      bool seen_skip = false;
      for (const auto& dim : fs.loopDims) {
        if (dim.loopVar == nullptr) {
          seen_skip = true;
        } else if (seen_skip && !dim.range.has_value()) {
          // Active dynamic dimension after a skipped dimension
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

        // Create expression statement to evaluate arrayRef
        hir::StatementId array_eval = ctx->hir_arena->AddStatement(
            hir::Statement{
                .kind = hir::StatementKind::kExpression,
                .span = span,
                .data = hir::ExpressionStatementData{.expression = array_expr},
            });

        auto body_result = LowerStatement(fs.body, lowerer);
        if (!body_result.has_value()) {
          // Empty body - just return the array evaluation
          return array_eval;
        }
        if (!*body_result) {
          return hir::kInvalidStatementId;
        }

        // Return block with array eval + body
        return ctx->hir_arena->AddStatement(
            hir::Statement{
                .kind = hir::StatementKind::kBlock,
                .span = span,
                .data =
                    hir::BlockStatementData{
                        .statements = {array_eval, *body_result}},
            });
      }

      // Create scope for loop variables
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

      // Create temp variable for arrayRef to avoid re-evaluation
      TypeId array_type = LowerType(*fs.arrayRef.type, span, ctx);
      if (!array_type) {
        return hir::kInvalidStatementId;
      }

      SymbolId array_temp_sym = registrar.RegisterSynthetic(
          ctx->MakeTempName("foreach_arr"), SymbolKind::kVariable, array_type);

      // Declare temp with arrayRef as initializer
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

      // Create reference to temp for use in loop expressions
      hir::ExpressionId array_temp_ref = ctx->hir_arena->AddExpression(
          hir::Expression{
              .kind = hir::ExpressionKind::kNameRef,
              .type = array_type,
              .span = span,
              .data = hir::NameRefExpressionData{.symbol = array_temp_sym}});

      // Build subarray expressions for each dimension (forward order)
      // dim_arrays[i] is the array whose size() we use for dimension i
      // current_type tracks the type at current indexing depth
      std::vector<hir::ExpressionId> dim_arrays(fs.loopDims.size());
      hir::ExpressionId current_array = array_temp_ref;
      const slang::ast::Type* current_type =
          &fs.arrayRef.type->getCanonicalType();

      for (size_t i = 0; i < fs.loopDims.size(); ++i) {
        dim_arrays[i] = current_array;

        if (fs.loopDims[i].loopVar != nullptr) {
          // Build arr[loop_var] for next dimension
          SymbolId var_sym = registrar.Lookup(*fs.loopDims[i].loopVar);
          TypeId var_type =
              LowerType(fs.loopDims[i].loopVar->getType(), span, ctx);
          if (!var_type) {
            return hir::kInvalidStatementId;
          }

          hir::ExpressionId index_ref =
              MakeNameRef(var_sym, var_type, span, ctx);

          // Get element type and select kind for this dimension
          const slang::ast::Type* elem_type = GetArrayElementType(current_type);
          hir::ExpressionKind select_kind = GetSelectKind(current_type);

          // Determine result type
          TypeId elem_type_id;
          if (select_kind == hir::ExpressionKind::kBitSelect) {
            // Bit select returns 1-bit logic
            elem_type_id = ctx->type_arena->Intern(
                TypeKind::kIntegral,
                IntegralInfo{
                    .bit_width = 1,
                    .is_signed = false,
                    .is_four_state = current_type->isFourState()});
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

          // Build element access expression
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

          // Advance type to element type for next iteration
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

      // Signed 32-bit int type for size() return
      TypeId int_type = ctx->type_arena->Intern(
          TypeKind::kIntegral,
          IntegralInfo{
              .bit_width = 32, .is_signed = true, .is_four_state = false});

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
        hir::ExpressionId var_ref =
            MakeNameRef(loop_var_sym, iter_type, span, ctx);

        hir::ExpressionId init_val;
        hir::ExpressionId bound;
        hir::BinaryOp cmp_op = hir::BinaryOp::kLessThan;
        hir::UnaryOp step_op = hir::UnaryOp::kPostincrement;

        if (dim.range.has_value()) {
          // Fixed-size dimension: iterate based on range direction
          // [3:0] -> left=3, right=0, descending (iterate 3,2,1,0)
          // [0:3] -> left=0, right=3, ascending (iterate 0,1,2,3)
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
          // Dynamic dimension - iterate 0 to size()-1
          init_val = MakeIntConstant(0, iter_type, span, ctx);

          // Build size() call on dim_arrays[dim_idx]
          hir::ExpressionId size_call = ctx->hir_arena->AddExpression(
              hir::Expression{
                  .kind = hir::ExpressionKind::kBuiltinMethodCall,
                  .type = int_type,
                  .span = span,
                  .data = hir::BuiltinMethodCallExpressionData{
                      .receiver = dim_arrays[dim_idx],
                      .method = hir::BuiltinMethod::kSize,
                      .args = {}}});

          // Cast to iterator type if different
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

        // Build condition: var_ref cmp_op bound
        TypeId bool_type = ctx->type_arena->Intern(
            TypeKind::kIntegral,
            IntegralInfo{
                .bit_width = 1, .is_signed = false, .is_four_state = false});

        hir::ExpressionId condition = ctx->hir_arena->AddExpression(
            hir::Expression{
                .kind = hir::ExpressionKind::kBinaryOp,
                .type = bool_type,
                .span = span,
                .data = hir::BinaryExpressionData{
                    .op = cmp_op, .lhs = var_ref, .rhs = bound}});

        // Build step: var_ref++/--
        hir::ExpressionId step_var_ref =
            MakeNameRef(loop_var_sym, iter_type, span, ctx);
        hir::ExpressionId step = ctx->hir_arena->AddExpression(
            hir::Expression{
                .kind = hir::ExpressionKind::kUnaryOp,
                .type = iter_type,
                .span = span,
                .data = hir::UnaryExpressionData{
                    .op = step_op, .operand = step_var_ref}});

        // Build init assignment: loop_var = init_val
        hir::ExpressionId init_var_ref =
            MakeNameRef(loop_var_sym, iter_type, span, ctx);
        hir::ExpressionId init_assign = ctx->hir_arena->AddExpression(
            hir::Expression{
                .kind = hir::ExpressionKind::kAssignment,
                .type = iter_type,
                .span = span,
                .data = hir::AssignmentExpressionData{
                    .target = init_var_ref, .value = init_val}});

        // Build ForLoop
        current =
            BuildForLoop(init_assign, condition, step, current, span, ctx);
      }

      // Build outer block with var decls + nested loops
      var_decls.push_back(current);
      return ctx->hir_arena->AddStatement(
          hir::Statement{
              .kind = hir::StatementKind::kBlock,
              .span = span,
              .data =
                  hir::BlockStatementData{.statements = std::move(var_decls)},
          });
    }

    case StatementKind::Return: {
      const auto& ret = stmt.as<slang::ast::ReturnStatement>();
      SourceSpan span = ctx->SpanOf(stmt.sourceRange);

      hir::ExpressionId value = hir::kInvalidExpressionId;
      if (ret.expr != nullptr) {
        value = lower_expr(*ret.expr);
        if (!value) {
          return hir::kInvalidStatementId;
        }
      }

      return ctx->hir_arena->AddStatement(
          hir::Statement{
              .kind = hir::StatementKind::kReturn,
              .span = span,
              .data = hir::ReturnStatementData{.value = value},
          });
    }

    case StatementKind::Timed: {
      const auto& timed = stmt.as<slang::ast::TimedStatement>();
      SourceSpan span = ctx->SpanOf(stmt.sourceRange);

      if (timed.timing.kind == slang::ast::TimingControlKind::Delay) {
        const auto& delay_ctrl = timed.timing.as<slang::ast::DelayControl>();
        const slang::ast::Expression& delay_expr = delay_ctrl.expr;

        uint64_t ticks = 0;

        if (delay_expr.kind == slang::ast::ExpressionKind::IntegerLiteral) {
          // Integer literal path: #10, #100, etc.
          const auto& literal = delay_expr.as<slang::ast::IntegerLiteral>();
          auto val = literal.getValue();
          if (val.hasUnknown()) {
            ctx->sink->Error(span, "delay value cannot contain X or Z");
            return hir::kInvalidStatementId;
          }
          if (val.isSigned() && val.isNegative()) {
            ctx->sink->Error(span, "delay value cannot be negative");
            return hir::kInvalidStatementId;
          }
          auto literal_value =
              static_cast<uint64_t>(val.as<uint64_t>().value());
          ticks = lowerer.ScaleDelayTicks(literal_value);
        } else if (delay_expr.kind == slang::ast::ExpressionKind::TimeLiteral) {
          // Time literal path: #10ns, #1.5us, etc.
          // Slang already converts to module's timeunit as a double
          const auto& literal = delay_expr.as<slang::ast::TimeLiteral>();
          double value = literal.getValue();
          ticks = lowerer.ScaleDelayReal(value);
        } else {
          ctx->sink->Error(span, "only constant delays are supported");
          return hir::kInvalidStatementId;
        }

        hir::StatementId delay_stmt = ctx->hir_arena->AddStatement(
            hir::Statement{
                .kind = hir::StatementKind::kDelay,
                .span = span,
                .data = hir::DelayStatementData{.ticks = ticks},
            });

        if (timed.stmt.kind != slang::ast::StatementKind::Empty) {
          auto body_result = LowerStatement(timed.stmt, lowerer);
          if (!body_result.has_value()) {
            return delay_stmt;
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
                          .statements = {delay_stmt, *body_result}},
              });
        }
        return delay_stmt;
      }

      if (timed.timing.kind == slang::ast::TimingControlKind::SignalEvent) {
        const auto& sig_event =
            timed.timing.as<slang::ast::SignalEventControl>();

        hir::ExpressionId signal_expr = lower_expr(sig_event.expr);
        if (!signal_expr) {
          return hir::kInvalidStatementId;
        }

        hir::EventEdgeKind edge = hir::EventEdgeKind::kNone;
        switch (sig_event.edge) {
          case slang::ast::EdgeKind::None:
            edge = hir::EventEdgeKind::kNone;
            break;
          case slang::ast::EdgeKind::PosEdge:
            edge = hir::EventEdgeKind::kPosedge;
            break;
          case slang::ast::EdgeKind::NegEdge:
            edge = hir::EventEdgeKind::kNegedge;
            break;
          case slang::ast::EdgeKind::BothEdges:
            edge = hir::EventEdgeKind::kBothEdges;
            break;
        }

        if (edge != hir::EventEdgeKind::kNone &&
            sig_event.expr.kind != slang::ast::ExpressionKind::NamedValue &&
            sig_event.expr.kind !=
                slang::ast::ExpressionKind::HierarchicalValue) {
          if (sig_event.expr.kind ==
              slang::ast::ExpressionKind::ElementSelect) {
            const auto& select =
                sig_event.expr.as<slang::ast::ElementSelectExpression>();
            const auto& base_type = select.value().type->getCanonicalType();
            if (base_type.isIntegral() || base_type.isPackedArray()) {
              if (sig_event.expr.type->getBitWidth() != 1) {
                ctx->sink->Unsupported(
                    span,
                    "edge triggers on multi-bit packed element "
                    "selects are not supported; only single-bit "
                    "selects are allowed",
                    UnsupportedCategory::kFeature);
                return hir::kInvalidStatementId;
              }
              const auto* cv = select.selector().getConstant();
              if (cv != nullptr && !cv->bad() && cv->isInteger()) {
                const auto& idx = cv->integer();
                if (!idx.hasUnknown() && !idx.isNegative()) {
                  auto idx_val = idx.as<int64_t>();
                  auto range = base_type.getFixedRange();
                  if (idx_val && *idx_val >= range.lower() &&
                      *idx_val <= range.upper()) {
                    // Constant bit-select in range - allowed for edge
                    // triggers. Fall through to create the trigger.
                  } else {
                    ctx->sink->Unsupported(
                        span,
                        "edge trigger bit-select index is out of range "
                        "for the packed type width",
                        UnsupportedCategory::kFeature);
                    return hir::kInvalidStatementId;
                  }
                } else {
                  ctx->sink->Unsupported(
                      span,
                      "edge trigger bit-select index is out of range "
                      "for the packed type width",
                      UnsupportedCategory::kFeature);
                  return hir::kInvalidStatementId;
                }
              } else {
                ctx->sink->Unsupported(
                    span,
                    "dynamic bit-select edge triggers are not "
                    "supported; use a constant index",
                    UnsupportedCategory::kFeature);
                return hir::kInvalidStatementId;
              }
            } else {
              ctx->sink->Unsupported(
                  span,
                  "edge triggers on unpacked array elements are not "
                  "supported; use @(*) or @(signal) for "
                  "level-sensitive observation",
                  UnsupportedCategory::kFeature);
              return hir::kInvalidStatementId;
            }
          } else {
            ctx->sink->Unsupported(
                span,
                "edge triggers (@posedge/@negedge) are only supported "
                "on plain signal variables or constant bit-selects; "
                "sub-expressions (struct fields, part-selects) are not "
                "supported; use @(*) or @(signal) for level-sensitive "
                "observation",
                UnsupportedCategory::kFeature);
            return hir::kInvalidStatementId;
          }
        }

        std::vector<hir::EventTrigger> triggers;
        triggers.push_back({.signal = signal_expr, .edge = edge});

        hir::StatementId wait_stmt = ctx->hir_arena->AddStatement(
            hir::Statement{
                .kind = hir::StatementKind::kEventWait,
                .span = span,
                .data =
                    hir::EventWaitStatementData{
                        .triggers = std::move(triggers)},
            });

        if (timed.stmt.kind != slang::ast::StatementKind::Empty) {
          auto body_result = LowerStatement(timed.stmt, lowerer);
          if (!body_result.has_value()) {
            return wait_stmt;
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
                          .statements = {wait_stmt, *body_result}},
              });
        }
        return wait_stmt;
      }

      if (timed.timing.kind == slang::ast::TimingControlKind::EventList) {
        const auto& event_list =
            timed.timing.as<slang::ast::EventListControl>();

        std::vector<hir::EventTrigger> triggers;
        for (const slang::ast::TimingControl* event : event_list.events) {
          if (event->kind != slang::ast::TimingControlKind::SignalEvent) {
            ctx->sink->Error(
                span, "only signal events are supported in event lists");
            return hir::kInvalidStatementId;
          }
          const auto& sig_event = event->as<slang::ast::SignalEventControl>();

          hir::ExpressionId signal_expr = lower_expr(sig_event.expr);
          if (!signal_expr) {
            return hir::kInvalidStatementId;
          }

          hir::EventEdgeKind edge = hir::EventEdgeKind::kNone;
          switch (sig_event.edge) {
            case slang::ast::EdgeKind::None:
              edge = hir::EventEdgeKind::kNone;
              break;
            case slang::ast::EdgeKind::PosEdge:
              edge = hir::EventEdgeKind::kPosedge;
              break;
            case slang::ast::EdgeKind::NegEdge:
              edge = hir::EventEdgeKind::kNegedge;
              break;
            case slang::ast::EdgeKind::BothEdges:
              edge = hir::EventEdgeKind::kBothEdges;
              break;
          }

          if (edge != hir::EventEdgeKind::kNone &&
              sig_event.expr.kind != slang::ast::ExpressionKind::NamedValue &&
              sig_event.expr.kind !=
                  slang::ast::ExpressionKind::HierarchicalValue) {
            if (sig_event.expr.kind ==
                slang::ast::ExpressionKind::ElementSelect) {
              const auto& select =
                  sig_event.expr.as<slang::ast::ElementSelectExpression>();
              const auto& base_type = select.value().type->getCanonicalType();
              if (base_type.isIntegral() || base_type.isPackedArray()) {
                if (sig_event.expr.type->getBitWidth() != 1) {
                  ctx->sink->Unsupported(
                      span,
                      "edge triggers on multi-bit packed element "
                      "selects are not supported; only single-bit "
                      "selects are allowed",
                      UnsupportedCategory::kFeature);
                  return hir::kInvalidStatementId;
                }
                const auto* cv = select.selector().getConstant();
                if (cv != nullptr && !cv->bad() && cv->isInteger()) {
                  const auto& idx = cv->integer();
                  if (!idx.hasUnknown() && !idx.isNegative()) {
                    auto idx_val = idx.as<int64_t>();
                    auto range = base_type.getFixedRange();
                    if (idx_val && *idx_val >= range.lower() &&
                        *idx_val <= range.upper()) {
                      // Constant bit-select in range - allowed for edge
                      // triggers. Fall through to create the trigger.
                    } else {
                      ctx->sink->Unsupported(
                          span,
                          "edge trigger bit-select index is out of range "
                          "for the packed type width",
                          UnsupportedCategory::kFeature);
                      return hir::kInvalidStatementId;
                    }
                  } else {
                    ctx->sink->Unsupported(
                        span,
                        "edge trigger bit-select index is out of range "
                        "for the packed type width",
                        UnsupportedCategory::kFeature);
                    return hir::kInvalidStatementId;
                  }
                } else {
                  ctx->sink->Unsupported(
                      span,
                      "dynamic bit-select edge triggers are not "
                      "supported; use a constant index",
                      UnsupportedCategory::kFeature);
                  return hir::kInvalidStatementId;
                }
              } else {
                ctx->sink->Unsupported(
                    span,
                    "edge triggers on unpacked array elements are not "
                    "supported; use @(*) or @(signal) for "
                    "level-sensitive observation",
                    UnsupportedCategory::kFeature);
                return hir::kInvalidStatementId;
              }
            } else {
              ctx->sink->Unsupported(
                  span,
                  "edge triggers (@posedge/@negedge) are only supported "
                  "on plain signal variables or constant bit-selects; "
                  "sub-expressions (struct fields, part-selects) are not "
                  "supported; use @(*) or @(signal) for level-sensitive "
                  "observation",
                  UnsupportedCategory::kFeature);
              return hir::kInvalidStatementId;
            }
          }

          triggers.push_back({.signal = signal_expr, .edge = edge});
        }

        hir::StatementId wait_stmt = ctx->hir_arena->AddStatement(
            hir::Statement{
                .kind = hir::StatementKind::kEventWait,
                .span = span,
                .data =
                    hir::EventWaitStatementData{
                        .triggers = std::move(triggers)},
            });

        if (timed.stmt.kind != slang::ast::StatementKind::Empty) {
          auto body_result = LowerStatement(timed.stmt, lowerer);
          if (!body_result.has_value()) {
            return wait_stmt;
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
                          .statements = {wait_stmt, *body_result}},
              });
        }
        return wait_stmt;
      }

      ctx->sink->Error(
          span, "unsupported timing control kind (only #N and @ supported)");
      return hir::kInvalidStatementId;
    }

    default:
      ctx->sink->Error(
          ctx->SpanOf(stmt.sourceRange),
          std::format("unsupported statement kind '{}'", toString(stmt.kind)));
      return hir::kInvalidStatementId;
  }
}

}  // namespace lyra::lowering::ast_to_hir
