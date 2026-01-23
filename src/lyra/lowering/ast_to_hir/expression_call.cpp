#include "lyra/lowering/ast_to_hir/expression_call.hpp"

#include <format>

#include <slang/ast/expressions/CallExpression.h>
#include <slang/ast/symbols/SubroutineSymbols.h>
#include <slang/ast/types/AllTypes.h>

#include "lyra/common/constant_arena.hpp"
#include "lyra/common/diagnostic/diagnostic_sink.hpp"
#include "lyra/common/internal_error.hpp"
#include "lyra/common/type.hpp"
#include "lyra/hir/arena.hpp"
#include "lyra/hir/expression.hpp"
#include "lyra/lowering/ast_to_hir/builtin_method.hpp"
#include "lyra/lowering/ast_to_hir/context.hpp"
#include "lyra/lowering/ast_to_hir/expression.hpp"
#include "lyra/lowering/ast_to_hir/symbol_registrar.hpp"
#include "lyra/lowering/ast_to_hir/system_call.hpp"
#include "lyra/lowering/ast_to_hir/system_function_desugar.hpp"
#include "lyra/lowering/ast_to_hir/type.hpp"

namespace lyra::lowering::ast_to_hir {

auto LowerCallExpression(
    const slang::ast::Expression& expr, SymbolRegistrar& registrar,
    Context* ctx) -> hir::ExpressionId {
  const auto& call = expr.as<slang::ast::CallExpression>();
  SourceSpan span = ctx->SpanOf(expr.sourceRange);

  // Check if this is a builtin method call on a dynamic array or queue.
  // IMPORTANT: Must check BEFORE isSystemCall() - slang's isSystemCall()
  // returns true for built-in array methods like size().
  if (auto info = ClassifyBuiltinMethod(call)) {
    // Lower the receiver (first argument).
    const auto* first_arg = call.arguments()[0];
    hir::ExpressionId receiver = LowerExpression(*first_arg, registrar, ctx);
    if (!receiver) {
      return hir::kInvalidExpressionId;
    }

    // Determine HIR method and result type based on classification.
    hir::BuiltinMethod hir_method{};
    TypeId result_type{};
    std::vector<hir::ExpressionId> args;

    switch (info->method) {
      case BuiltinMethodKind::kSize: {
        hir_method = hir::BuiltinMethod::kSize;
        result_type = ctx->type_arena->Intern(
            TypeKind::kIntegral,
            IntegralInfo{
                .bit_width = 32, .is_signed = true, .is_four_state = false});
        break;
      }
      case BuiltinMethodKind::kPopBack:
      case BuiltinMethodKind::kPopFront: {
        hir_method = (info->method == BuiltinMethodKind::kPopBack)
                         ? hir::BuiltinMethod::kPopBack
                         : hir::BuiltinMethod::kPopFront;
        // Get element type from queue.
        const auto& queue_type =
            first_arg->type->getCanonicalType().as<slang::ast::QueueType>();
        result_type = LowerType(queue_type.elementType, span, ctx);
        if (!result_type) {
          return hir::kInvalidExpressionId;
        }
        break;
      }
      case BuiltinMethodKind::kDelete: {
        hir_method = hir::BuiltinMethod::kDelete;
        result_type =
            ctx->type_arena->Intern(TypeKind::kVoid, std::monostate{});
        // delete() or delete(idx): 0 or 1 arg after receiver
        if (call.arguments().size() > 2) {
          ctx->sink->Error(span, "delete() takes at most one argument");
          return hir::kInvalidExpressionId;
        }
        if (call.arguments().size() == 2) {
          hir::ExpressionId idx =
              LowerExpression(*call.arguments()[1], registrar, ctx);
          if (!idx) {
            return hir::kInvalidExpressionId;
          }
          args.push_back(idx);
        }
        break;
      }
      case BuiltinMethodKind::kPushBack:
      case BuiltinMethodKind::kPushFront: {
        hir_method = (info->method == BuiltinMethodKind::kPushBack)
                         ? hir::BuiltinMethod::kPushBack
                         : hir::BuiltinMethod::kPushFront;
        result_type =
            ctx->type_arena->Intern(TypeKind::kVoid, std::monostate{});
        // push_back(val) / push_front(val): exactly 1 arg after receiver
        if (call.arguments().size() != 2) {
          ctx->sink->Error(
              span, std::format(
                        "{}() requires exactly one argument",
                        call.getSubroutineName()));
          return hir::kInvalidExpressionId;
        }
        hir::ExpressionId value =
            LowerExpression(*call.arguments()[1], registrar, ctx);
        if (!value) {
          return hir::kInvalidExpressionId;
        }
        args.push_back(value);
        break;
      }
      case BuiltinMethodKind::kInsert: {
        hir_method = hir::BuiltinMethod::kInsert;
        result_type =
            ctx->type_arena->Intern(TypeKind::kVoid, std::monostate{});
        // insert(idx, val): exactly 2 args after receiver
        if (call.arguments().size() != 3) {
          ctx->sink->Error(
              span,
              "insert() requires exactly two arguments (index and value)");
          return hir::kInvalidExpressionId;
        }
        hir::ExpressionId idx =
            LowerExpression(*call.arguments()[1], registrar, ctx);
        if (!idx) {
          return hir::kInvalidExpressionId;
        }
        hir::ExpressionId value =
            LowerExpression(*call.arguments()[2], registrar, ctx);
        if (!value) {
          return hir::kInvalidExpressionId;
        }
        args.push_back(idx);
        args.push_back(value);
        break;
      }

      // Enum compile-time methods - constant fold from EnumInfo
      case BuiltinMethodKind::kEnumFirst:
      case BuiltinMethodKind::kEnumLast:
      case BuiltinMethodKind::kEnumNum: {
        // These methods take no arguments (receiver only)
        if (call.arguments().size() > 1) {
          ctx->sink->Error(span, "enum first/last/num takes no arguments");
          return hir::kInvalidExpressionId;
        }

        // Get the enum type from the receiver expression
        const hir::Expression& receiver_expr = (*ctx->hir_arena)[receiver];
        TypeId enum_type_id = receiver_expr.type;
        const Type& enum_type = (*ctx->type_arena)[enum_type_id];

        if (enum_type.Kind() != TypeKind::kEnum) {
          throw common::InternalError(
              "LowerCallExpression", "enum method on non-enum type");
        }
        const auto& enum_info = enum_type.AsEnum();

        // Defensive check: SV doesn't allow empty enums, but be safe
        if (enum_info.members.empty()) {
          throw common::InternalError(
              "LowerCallExpression", "enum has no members");
        }

        if (info->method == BuiltinMethodKind::kEnumNum) {
          // num() returns int (32-bit signed 2-state)
          TypeId int_type = ctx->type_arena->Intern(
              TypeKind::kIntegral,
              IntegralInfo{
                  .bit_width = 32, .is_signed = true, .is_four_state = false});
          auto num_val = static_cast<uint64_t>(enum_info.members.size());
          IntegralConstant num_ic;
          num_ic.a = {num_val};
          num_ic.b = {0};
          ConstId num_const =
              ctx->constant_arena->Intern(int_type, std::move(num_ic));
          return ctx->hir_arena->AddExpression(
              hir::Expression{
                  .kind = hir::ExpressionKind::kConstant,
                  .type = int_type,
                  .span = span,
                  .data = hir::ConstantExpressionData{.constant = num_const}});
        }

        // first() and last() return the enum type
        const IntegralConstant& member_val =
            (info->method == BuiltinMethodKind::kEnumFirst)
                ? enum_info.members.front().value
                : enum_info.members.back().value;
        IntegralConstant ic_copy = member_val;
        ConstId member_const =
            ctx->constant_arena->Intern(enum_type_id, std::move(ic_copy));
        return ctx->hir_arena->AddExpression(
            hir::Expression{
                .kind = hir::ExpressionKind::kConstant,
                .type = enum_type_id,
                .span = span,
                .data = hir::ConstantExpressionData{.constant = member_const}});
      }

      // Enum runtime methods - emit BuiltinMethodCall
      case BuiltinMethodKind::kEnumNext:
      case BuiltinMethodKind::kEnumPrev: {
        // next/prev take at most one argument (step)
        if (call.arguments().size() > 2) {
          ctx->sink->Error(span, "enum next/prev takes at most one argument");
          return hir::kInvalidExpressionId;
        }

        hir_method = (info->method == BuiltinMethodKind::kEnumNext)
                         ? hir::BuiltinMethod::kEnumNext
                         : hir::BuiltinMethod::kEnumPrev;
        // Get enum type from receiver for result type
        const hir::Expression& receiver_expr = (*ctx->hir_arena)[receiver];
        result_type = receiver_expr.type;

        // Optional step argument: next(N) or prev(N)
        if (call.arguments().size() > 1) {
          hir::ExpressionId step =
              LowerExpression(*call.arguments()[1], registrar, ctx);
          if (!step) {
            return hir::kInvalidExpressionId;
          }
          args.push_back(step);
        }
        break;
      }

      case BuiltinMethodKind::kEnumName: {
        // name() takes no arguments (receiver only)
        if (call.arguments().size() > 1) {
          ctx->sink->Error(span, "enum name takes no arguments");
          return hir::kInvalidExpressionId;
        }

        hir_method = hir::BuiltinMethod::kEnumName;
        result_type =
            ctx->type_arena->Intern(TypeKind::kString, std::monostate{});
        break;
      }
    }

    return ctx->hir_arena->AddExpression(
        hir::Expression{
            .kind = hir::ExpressionKind::kBuiltinMethodCall,
            .type = result_type,
            .span = span,
            .data = hir::BuiltinMethodCallExpressionData{
                .receiver = receiver,
                .method = hir_method,
                .args = std::move(args)}});
  }

  // Pure system functions ($signed, $unsigned, $itor, etc.) -> desugar
  if (auto pure_kind = ClassifyPureSystemFunction(call)) {
    return LowerPureSystemFunction(call, *pure_kind, registrar, ctx);
  }

  // Effectful system calls ($display, etc.)
  if (call.isSystemCall()) {
    return LowerSystemCall(call, registrar, ctx);
  }

  // User function call
  const auto* user_sub =
      std::get_if<const slang::ast::SubroutineSymbol*>(&call.subroutine);
  if (user_sub == nullptr) {
    ctx->sink->Error(span, "indirect function calls not supported");
    return hir::kInvalidExpressionId;
  }

  // Reject task calls from functions
  if ((*user_sub)->subroutineKind == slang::ast::SubroutineKind::Task) {
    ctx->sink->Error(span, "task calls not supported");
    return hir::kInvalidExpressionId;
  }

  SymbolId callee = registrar.Lookup(**user_sub);
  if (!callee) {
    ctx->ErrorFmt(span, "undefined function '{}'", (*user_sub)->name);
    return hir::kInvalidExpressionId;
  }

  // Lower arguments
  std::vector<hir::ExpressionId> args;
  args.reserve(call.arguments().size());
  for (const auto* arg_expr : call.arguments()) {
    hir::ExpressionId arg = LowerExpression(*arg_expr, registrar, ctx);
    if (!arg) {
      return hir::kInvalidExpressionId;
    }
    args.push_back(arg);
  }

  if (expr.type == nullptr) {
    return hir::kInvalidExpressionId;
  }
  TypeId type = LowerType(*expr.type, span, ctx);
  if (!type) {
    return hir::kInvalidExpressionId;
  }

  return ctx->hir_arena->AddExpression(
      hir::Expression{
          .kind = hir::ExpressionKind::kCall,
          .type = type,
          .span = span,
          .data = hir::CallExpressionData{
              .callee = callee, .arguments = std::move(args)}});
}

}  // namespace lyra::lowering::ast_to_hir
