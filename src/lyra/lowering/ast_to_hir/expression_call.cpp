#include "lyra/lowering/ast_to_hir/expression_call.hpp"

#include <format>
#include <optional>
#include <span>
#include <string_view>
#include <vector>

#include <slang/ast/expressions/AssignmentExpressions.h>
#include <slang/ast/expressions/CallExpression.h>
#include <slang/ast/symbols/SubroutineSymbols.h>
#include <slang/ast/types/AllTypes.h>

#include "lyra/common/constant_arena.hpp"
#include "lyra/common/diagnostic/diagnostic_sink.hpp"
#include "lyra/common/format.hpp"
#include "lyra/common/internal_error.hpp"
#include "lyra/common/system_function.hpp"
#include "lyra/common/type.hpp"
#include "lyra/hir/arena.hpp"
#include "lyra/hir/expression.hpp"
#include "lyra/hir/system_call.hpp"
#include "lyra/lowering/ast_to_hir/builtin_method.hpp"
#include "lyra/lowering/ast_to_hir/context.hpp"
#include "lyra/lowering/ast_to_hir/expression.hpp"
#include "lyra/lowering/ast_to_hir/symbol_registrar.hpp"
#include "lyra/lowering/ast_to_hir/system_call.hpp"
#include "lyra/lowering/ast_to_hir/system_function_desugar.hpp"
#include "lyra/lowering/ast_to_hir/type.hpp"

namespace lyra::lowering::ast_to_hir {

namespace {

// Classify $fdisplay/$fwrite family system tasks.
// Returns DisplayFunctionInfo if name matches, nullopt otherwise.
auto ClassifyFdisplay(std::string_view name)
    -> std::optional<DisplayFunctionInfo> {
  if (name == "$fdisplay") {
    return DisplayFunctionInfo{
        .radix = PrintRadix::kDecimal, .append_newline = true};
  }
  if (name == "$fdisplayb") {
    return DisplayFunctionInfo{
        .radix = PrintRadix::kBinary, .append_newline = true};
  }
  if (name == "$fdisplayo") {
    return DisplayFunctionInfo{
        .radix = PrintRadix::kOctal, .append_newline = true};
  }
  if (name == "$fdisplayh") {
    return DisplayFunctionInfo{
        .radix = PrintRadix::kHex, .append_newline = true};
  }
  if (name == "$fwrite") {
    return DisplayFunctionInfo{
        .radix = PrintRadix::kDecimal, .append_newline = false};
  }
  if (name == "$fwriteb") {
    return DisplayFunctionInfo{
        .radix = PrintRadix::kBinary, .append_newline = false};
  }
  if (name == "$fwriteo") {
    return DisplayFunctionInfo{
        .radix = PrintRadix::kOctal, .append_newline = false};
  }
  if (name == "$fwriteh") {
    return DisplayFunctionInfo{
        .radix = PrintRadix::kHex, .append_newline = false};
  }
  return std::nullopt;
}

auto RadixToFormatKind(PrintRadix radix) -> FormatKind {
  switch (radix) {
    case PrintRadix::kDecimal:
      return FormatKind::kDecimal;
    case PrintRadix::kBinary:
      return FormatKind::kBinary;
    case PrintRadix::kOctal:
      return FormatKind::kOctal;
    case PrintRadix::kHex:
      return FormatKind::kHex;
  }
  return FormatKind::kDecimal;
}

}  // namespace

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
          num_ic.value = {num_val};
          num_ic.unknown = {0};
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

  // Plusargs system functions - intercept before general system call dispatch
  if (call.isSystemCall()) {
    std::string_view name = call.getSubroutineName();
    if (name == "$test$plusargs") {
      if (call.arguments().size() != 1) {
        ctx->ErrorFmt(span, "$test$plusargs expects 1 argument");
        return hir::kInvalidExpressionId;
      }
      hir::ExpressionId query =
          LowerExpression(*call.arguments()[0], registrar, ctx);
      if (!query) {
        return hir::kInvalidExpressionId;
      }
      TypeId result_type = LowerType(*expr.type, span, ctx);
      return ctx->hir_arena->AddExpression(
          hir::Expression{
              .kind = hir::ExpressionKind::kSystemCall,
              .type = result_type,
              .span = span,
              .data = hir::SystemCallExpressionData{
                  hir::TestPlusargsData{.query = query}}});
    }
    if (name == "$value$plusargs") {
      if (call.arguments().size() != 2) {
        ctx->ErrorFmt(span, "$value$plusargs expects 2 arguments");
        return hir::kInvalidExpressionId;
      }
      hir::ExpressionId format_expr =
          LowerExpression(*call.arguments()[0], registrar, ctx);
      if (!format_expr) {
        return hir::kInvalidExpressionId;
      }
      // Output argument: slang may wrap as AssignmentExpression
      const slang::ast::Expression* out_arg = call.arguments()[1];
      hir::ExpressionId output_expr;
      if (out_arg->kind == slang::ast::ExpressionKind::Assignment) {
        const auto& assign = out_arg->as<slang::ast::AssignmentExpression>();
        output_expr = LowerExpression(assign.left(), registrar, ctx);
      } else {
        output_expr = LowerExpression(*out_arg, registrar, ctx);
      }
      if (!output_expr) {
        return hir::kInvalidExpressionId;
      }
      TypeId result_type = LowerType(*expr.type, span, ctx);
      return ctx->hir_arena->AddExpression(
          hir::Expression{
              .kind = hir::ExpressionKind::kSystemCall,
              .type = result_type,
              .span = span,
              .data = hir::SystemCallExpressionData{hir::ValuePlusargsData{
                  .format = format_expr, .output = output_expr}}});
    }
    if (name == "$readmemh" || name == "$readmemb" || name == "$writememh" ||
        name == "$writememb") {
      bool is_read = (name == "$readmemh" || name == "$readmemb");
      bool is_hex = (name == "$readmemh" || name == "$writememh");

      if (call.arguments().size() < 2 || call.arguments().size() > 4) {
        ctx->ErrorFmt(span, "{} expects 2-4 arguments", name);
        return hir::kInvalidExpressionId;
      }

      // Lower filename (arg 0)
      hir::ExpressionId filename =
          LowerExpression(*call.arguments()[0], registrar, ctx);
      if (!filename) {
        return hir::kInvalidExpressionId;
      }

      // Lower target lvalue (arg 1) - unwrap AssignmentExpression if present
      const slang::ast::Expression* target_arg = call.arguments()[1];
      hir::ExpressionId target;
      if (target_arg->kind == slang::ast::ExpressionKind::Assignment) {
        const auto& assign = target_arg->as<slang::ast::AssignmentExpression>();
        target = LowerExpression(assign.left(), registrar, ctx);
      } else {
        target = LowerExpression(*target_arg, registrar, ctx);
      }
      if (!target) {
        return hir::kInvalidExpressionId;
      }

      // Lower optional start/end address args
      std::optional<hir::ExpressionId> start_addr;
      std::optional<hir::ExpressionId> end_addr;
      if (call.arguments().size() >= 3) {
        hir::ExpressionId addr =
            LowerExpression(*call.arguments()[2], registrar, ctx);
        if (!addr) {
          return hir::kInvalidExpressionId;
        }
        start_addr = addr;
      }
      if (call.arguments().size() >= 4) {
        hir::ExpressionId addr =
            LowerExpression(*call.arguments()[3], registrar, ctx);
        if (!addr) {
          return hir::kInvalidExpressionId;
        }
        end_addr = addr;
      }

      TypeId result_type =
          ctx->type_arena->Intern(TypeKind::kVoid, std::monostate{});
      return ctx->hir_arena->AddExpression(
          hir::Expression{
              .kind = hir::ExpressionKind::kSystemCall,
              .type = result_type,
              .span = span,
              .data = hir::SystemCallExpressionData{hir::MemIOData{
                  .is_read = is_read,
                  .is_hex = is_hex,
                  .filename = filename,
                  .target = target,
                  .start_addr = start_addr,
                  .end_addr = end_addr}}});
    }
    if (name == "$fopen") {
      if (call.arguments().empty() || call.arguments().size() > 2) {
        ctx->ErrorFmt(span, "$fopen expects 1 or 2 arguments");
        return hir::kInvalidExpressionId;
      }
      hir::ExpressionId filename =
          LowerExpression(*call.arguments()[0], registrar, ctx);
      if (!filename) {
        return hir::kInvalidExpressionId;
      }
      std::optional<hir::ExpressionId> mode;
      if (call.arguments().size() == 2) {
        hir::ExpressionId mode_expr =
            LowerExpression(*call.arguments()[1], registrar, ctx);
        if (!mode_expr) {
          return hir::kInvalidExpressionId;
        }
        mode = mode_expr;
      }
      TypeId result_type = LowerType(*expr.type, span, ctx);
      return ctx->hir_arena->AddExpression(
          hir::Expression{
              .kind = hir::ExpressionKind::kSystemCall,
              .type = result_type,
              .span = span,
              .data = hir::SystemCallExpressionData{
                  hir::FopenData{.filename = filename, .mode = mode}}});
    }
    if (name == "$fclose") {
      if (call.arguments().size() != 1) {
        ctx->ErrorFmt(span, "$fclose expects 1 argument");
        return hir::kInvalidExpressionId;
      }
      hir::ExpressionId descriptor =
          LowerExpression(*call.arguments()[0], registrar, ctx);
      if (!descriptor) {
        return hir::kInvalidExpressionId;
      }
      TypeId result_type =
          ctx->type_arena->Intern(TypeKind::kVoid, std::monostate{});
      return ctx->hir_arena->AddExpression(
          hir::Expression{
              .kind = hir::ExpressionKind::kSystemCall,
              .type = result_type,
              .span = span,
              .data = hir::SystemCallExpressionData{
                  hir::FcloseData{.descriptor = descriptor}}});
    }
    if (auto fdisp_info = ClassifyFdisplay(name)) {
      // $fdisplay/$fwrite family: first arg is descriptor, rest are display
      // args
      if (call.arguments().empty()) {
        ctx->ErrorFmt(span, "{} requires at least a file descriptor", name);
        return hir::kInvalidExpressionId;
      }

      // Lower descriptor (arg 0)
      hir::ExpressionId desc_expr =
          LowerExpression(*call.arguments()[0], registrar, ctx);
      if (!desc_expr) {
        return hir::kInvalidExpressionId;
      }

      // Lower remaining display arguments
      std::vector<hir::ExpressionId> display_args;
      std::vector<const slang::ast::Expression*> slang_display_args;
      for (size_t i = 1; i < call.arguments().size(); ++i) {
        hir::ExpressionId arg =
            LowerExpression(*call.arguments()[i], registrar, ctx);
        if (!arg) {
          return hir::kInvalidExpressionId;
        }
        display_args.push_back(arg);
        slang_display_args.push_back(call.arguments()[i]);
      }

      PrintKind print_kind =
          fdisp_info->append_newline ? PrintKind::kDisplay : PrintKind::kWrite;
      FormatKind default_format = RadixToFormatKind(fdisp_info->radix);

      std::vector<hir::FormatOp> ops = BuildDisplayFormatOps(
          slang_display_args, display_args, default_format, ctx);

      TypeId result_type =
          ctx->type_arena->Intern(TypeKind::kVoid, std::monostate{});
      return ctx->hir_arena->AddExpression(
          hir::Expression{
              .kind = hir::ExpressionKind::kSystemCall,
              .type = result_type,
              .span = span,
              .data = hir::DisplaySystemCallData{
                  .print_kind = print_kind,
                  .ops = std::move(ops),
                  .descriptor = desc_expr}});
    }
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
