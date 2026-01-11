#include "lyra/lowering/ast_to_mir/call.hpp"

#include <cstddef>
#include <cstdint>
#include <memory>
#include <optional>
#include <string>
#include <utility>
#include <vector>

#include <fmt/format.h>
#include <slang/ast/Compilation.h>
#include <slang/ast/Expression.h>
#include <slang/ast/expressions/CallExpression.h>
#include <slang/ast/expressions/MiscExpressions.h>
#include <slang/ast/symbols/SubroutineSymbols.h>
#include <slang/ast/types/AllTypes.h>
#include <slang/text/SourceManager.h>

#include "lyra/common/builtin_method.hpp"
#include "lyra/common/diagnostic.hpp"
#include "lyra/common/literal.hpp"
#include "lyra/common/system_function.hpp"
#include "lyra/common/type.hpp"
#include "lyra/lowering/ast_to_mir/expression.hpp"
#include "lyra/lowering/ast_to_mir/type.hpp"
#include "lyra/mir/expression.hpp"

namespace lyra::lowering::ast_to_mir {

auto LowerCall(const slang::ast::CallExpression& call)
    -> std::unique_ptr<mir::Expression> {
  const auto& expression = static_cast<const slang::ast::Expression&>(call);

  // Handle compile-time constant calls (includes enum methods like first(),
  // last()). slang evaluates these at compile time.
  if (const auto* cv = call.getConstant(); cv != nullptr) {
    auto type_result = LowerType(*call.type, expression.sourceRange);
    if (!type_result) {
      throw DiagnosticException(std::move(type_result.error()));
    }

    if (type_result->kind == common::Type::Kind::kIntegral) {
      int64_t value = cv->integer().as<int64_t>().value_or(0);
      const auto& integral_data =
          std::get<common::IntegralData>(type_result->data);
      auto literal =
          integral_data.is_signed
              ? common::Literal::IntegralSigned(value, integral_data.bit_width)
              : common::Literal::IntegralUnsigned(
                    static_cast<uint64_t>(value), integral_data.bit_width);
      return std::make_unique<mir::LiteralExpression>(std::move(literal));
    }
    if (type_result->kind == common::Type::Kind::kString) {
      return std::make_unique<mir::LiteralExpression>(
          common::Literal::String(std::string(cv->str())));
    }
    // Fall through for other constant types
  }

  // Handle enum method calls - slang doesn't always pre-evaluate num() as
  // constant, so we handle it explicitly here.
  // Note: isSystemCall() returns true for built-in methods like enum.num(),
  // so we check the subroutine name first before the isSystemCall check.
  auto subroutine_name = call.getSubroutineName();
  if (!call.arguments().empty()) {
    const auto& first_arg = *call.arguments()[0];
    // Get canonical type to handle typedef'd enums (e.g., typedef enum
    // {...} t)
    const auto& canonical_type = first_arg.type->getCanonicalType();
    if (canonical_type.isEnum()) {
      const auto& enum_type = canonical_type.as<slang::ast::EnumType>();

      if (subroutine_name == "num") {
        // num() returns the count of enum members
        int64_t count = 0;
        for ([[maybe_unused]] const auto& _ : enum_type.values()) {
          ++count;
        }
        // num() returns int type
        return std::make_unique<mir::LiteralExpression>(
            common::Literal::Int(static_cast<int32_t>(count)));
      }
      if (subroutine_name == "first") {
        // first() returns the first enum value
        auto it = enum_type.values().begin();
        if (it != enum_type.values().end()) {
          int64_t value = it->getValue().integer().as<int64_t>().value_or(0);
          auto type_result = LowerType(*call.type, expression.sourceRange);
          if (!type_result) {
            throw DiagnosticException(std::move(type_result.error()));
          }
          const auto& integral_data =
              std::get<common::IntegralData>(type_result->data);
          auto literal =
              integral_data.is_signed
                  ? common::Literal::IntegralSigned(
                        value, integral_data.bit_width)
                  : common::Literal::IntegralUnsigned(
                        static_cast<uint64_t>(value), integral_data.bit_width);
          return std::make_unique<mir::LiteralExpression>(std::move(literal));
        }
      }
      if (subroutine_name == "last") {
        // last() returns the last enum value
        int64_t last_value = 0;
        for (const auto& member : enum_type.values()) {
          last_value = member.getValue().integer().as<int64_t>().value_or(0);
        }
        auto type_result = LowerType(*call.type, expression.sourceRange);
        if (!type_result) {
          throw DiagnosticException(std::move(type_result.error()));
        }
        const auto& integral_data =
            std::get<common::IntegralData>(type_result->data);
        auto literal = integral_data.is_signed
                           ? common::Literal::IntegralSigned(
                                 last_value, integral_data.bit_width)
                           : common::Literal::IntegralUnsigned(
                                 static_cast<uint64_t>(last_value),
                                 integral_data.bit_width);
        return std::make_unique<mir::LiteralExpression>(std::move(literal));
      }

      // Runtime enum methods (next, prev, name)
      // Validate method using registry
      const auto* method_info = common::FindBuiltinMethod(
          common::BuiltinTypeKind::kEnum, subroutine_name);
      if (method_info != nullptr) {
        auto receiver = LowerExpression(first_arg);

        // Collect enum member info for codegen
        std::vector<mir::EnumMemberInfo> members;
        for (const auto& member : enum_type.values()) {
          members.push_back(
              {std::string(member.name),
               member.getValue().integer().as<int64_t>().value_or(0)});
        }

        // Get step argument for next(N) / prev(N), default is 1
        // IEEE 1800-2023 requires step to be a constant expression
        std::vector<std::unique_ptr<mir::Expression>> args;
        if (call.arguments().size() > 1) {
          const auto* step_cv = call.arguments()[1]->getConstant();
          if (step_cv == nullptr) {
            throw DiagnosticException(
                Diagnostic::Error(
                    call.arguments()[1]->sourceRange,
                    fmt::format(
                        "step argument to {}() must be a constant "
                        "expression",
                        subroutine_name)));
          }
          int64_t step = step_cv->integer().as<int64_t>().value_or(1);
          // Store step as literal argument
          args.push_back(
              std::make_unique<mir::LiteralExpression>(
                  common::Literal::IntegralSigned(step, 32)));
        }

        auto type_result = LowerType(*call.type, expression.sourceRange);
        if (!type_result) {
          throw DiagnosticException(std::move(type_result.error()));
        }

        return std::make_unique<mir::MethodCallExpression>(
            *type_result, std::move(receiver), std::string(subroutine_name),
            std::move(args), std::move(members));
      }

      // Unknown enum method
      throw DiagnosticException(
          Diagnostic::Error(
              expression.sourceRange,
              fmt::format("unknown enum method '{}'", subroutine_name)));
    }

    // Handle dynamic array built-in methods: size() and delete()
    // These are type methods, validated using the builtin method registry
    if (first_arg.type->kind == slang::ast::SymbolKind::DynamicArrayType) {
      const auto* method_info = common::FindBuiltinMethod(
          common::BuiltinTypeKind::kDynamicArray, subroutine_name);
      if (method_info != nullptr) {
        auto receiver = LowerExpression(first_arg);
        auto return_type =
            (method_info->return_type == common::BuiltinMethodReturnType::kInt)
                ? common::Type::Int()
                : common::Type::Void();
        return std::make_unique<mir::MethodCallExpression>(
            return_type, std::move(receiver), std::string(subroutine_name));
      }
    }
  }

  if (call.isSystemCall()) {
    auto name = call.getSubroutineName();

    // Validate supported system calls using registry
    if (!common::IsSystemFunctionSupported(name)) {
      throw DiagnosticException(
          Diagnostic::Error(
              expression.sourceRange,
              fmt::format("unsupported system call '{}'", name)));
    }

    // Get task/function distinction from slang
    bool is_task = call.getSubroutineKind() == slang::ast::SubroutineKind::Task;

    // Handle $timeunit($root), $timeprecision($root),
    // $printtimescale($root) Transform to $timeunit_root /
    // $timeprecision_root / $printtimescale_root
    std::string effective_name(name);
    bool is_root_variant = false;
    if ((name == "$timeunit" || name == "$timeprecision" ||
         name == "$printtimescale") &&
        call.arguments().size() == 1) {
      const auto& arg = *call.arguments()[0];
      if (arg.kind == slang::ast::ExpressionKind::ArbitrarySymbol) {
        const auto& arb_sym = arg.as<slang::ast::ArbitrarySymbolExpression>();
        if (arb_sym.symbol->name == "$root") {
          effective_name = std::string(name) + "_root";
          is_root_variant = true;
        }
      }
    }

    // Check if this is a display-like or severity task
    const auto* func_info = common::FindSystemFunction(name);
    bool is_display_like =
        func_info != nullptr &&
        (func_info->category == common::SystemFunctionCategory::kDisplay ||
         func_info->category == common::SystemFunctionCategory::kSeverity);
    bool is_severity_task =
        func_info != nullptr &&
        func_info->category == common::SystemFunctionCategory::kSeverity;

    // Override is_task for severity tasks - slang classifies them as
    // functions but they should be treated as tasks in codegen
    if (is_severity_task) {
      is_task = true;
    }

    std::unique_ptr<mir::Expression> format_expr;
    bool format_expr_is_literal = false;
    std::vector<std::unique_ptr<mir::Expression>> arguments;
    std::vector<mir::AssignmentTarget> output_targets;

    // For severity tasks, capture source location as metadata
    std::optional<std::string> source_file;
    std::optional<uint32_t> source_line;
    if (is_severity_task) {
      auto loc = expression.sourceRange.start();
      const auto& sys_info = std::get<1>(call.subroutine);
      const auto* sm = sys_info.scope->getCompilation().getSourceManager();
      if (sm != nullptr) {
        source_file = std::string(sm->getFileName(loc));
        source_line = static_cast<uint32_t>(sm->getLineNumber(loc));
      }
    }

    if (is_display_like && !is_root_variant) {
      // Display-like tasks: separate format_expr from arguments
      size_t format_idx = 0;
      size_t args_start_idx = 0;

      // For $fatal, first argument is finish_number (goes to arguments[0])
      if (name == "$fatal") {
        if (!call.arguments().empty()) {
          arguments.push_back(LowerExpression(*call.arguments()[0]));
        } else {
          // Default finish_number is 1 if no arguments
          arguments.push_back(
              std::make_unique<mir::LiteralExpression>(
                  common::Literal::Int(1)));
        }
        format_idx = 1;
        args_start_idx = 1;
      }

      // Check if format argument is a string literal
      if (format_idx < call.arguments().size()) {
        const auto& format_arg = *call.arguments()[format_idx];
        if (format_arg.kind == slang::ast::ExpressionKind::StringLiteral) {
          // Extract format string into format_expr
          format_expr = LowerExpression(format_arg);
          format_expr_is_literal = true;
          args_start_idx = format_idx + 1;  // Skip format in arguments
        } else {
          // First arg is a value, not a format string
          args_start_idx = format_idx;
        }
      }

      // Add remaining arguments (format args / values to display)
      for (size_t i = args_start_idx; i < call.arguments().size(); ++i) {
        arguments.push_back(LowerExpression(*call.arguments()[i]));
      }
    } else if (!is_root_variant) {
      // Handle $value$plusargs specially: second argument is output target
      if (name == "$value$plusargs") {
        auto args = call.arguments();
        if (!args.empty()) {
          // First arg is format string (input)
          arguments.push_back(LowerExpression(*args[0]));
        }
        if (args.size() >= 2) {
          // Second arg is output variable
          const auto* output_arg = args[1];
          if (output_arg->kind == slang::ast::ExpressionKind::NamedValue) {
            const auto& named_value =
                output_arg->as<slang::ast::NamedValueExpression>();
            // Get the type of the output variable for codegen
            auto type_result = LowerType(
                named_value.symbol.getType(), output_arg->sourceRange);
            if (!type_result) {
              throw DiagnosticException(std::move(type_result.error()));
            }
            mir::AssignmentTarget target(&named_value.symbol);
            target.base_type = *type_result;
            output_targets.push_back(std::move(target));
          } else {
            throw DiagnosticException(
                Diagnostic::Error(
                    output_arg->sourceRange,
                    "only simple variables supported as output argument"));
          }
        }
      } else {
        // Other non-display system calls: process all arguments normally
        for (const auto* arg : call.arguments()) {
          arguments.push_back(LowerExpression(*arg));
        }
      }
      // For mem_io tasks, check if first arg (filename) is a string literal
      // This is needed by interpreter to properly decode the filename
      bool is_mem_io =
          func_info != nullptr &&
          func_info->category == common::SystemFunctionCategory::kMemIo;
      if (is_mem_io && !call.arguments().empty()) {
        const auto& first_arg = *call.arguments()[0];
        if (first_arg.kind == slang::ast::ExpressionKind::StringLiteral) {
          format_expr_is_literal = true;
        }
      }
    }

    auto return_type_result = LowerType(*call.type, expression.sourceRange);
    if (!return_type_result) {
      throw DiagnosticException(std::move(return_type_result.error()));
    }
    auto syscall = std::make_unique<mir::SystemCallExpression>(
        effective_name, std::move(arguments), std::move(output_targets),
        *return_type_result, is_task);
    if (format_expr) {
      syscall->format_expr = std::move(format_expr);
    }
    syscall->format_expr_is_literal = format_expr_is_literal;
    syscall->source_file = std::move(source_file);
    syscall->source_line = source_line;
    return syscall;
  }

  // Check if this is a user-defined function call
  if (call.subroutine.index() == 0) {
    const auto* func = std::get<0>(call.subroutine);
    if (func != nullptr && func->kind == slang::ast::SymbolKind::Subroutine) {
      const auto& subroutine_sym = func->as<slang::ast::SubroutineSymbol>();

      // Tasks not yet supported
      if (subroutine_sym.subroutineKind == slang::ast::SubroutineKind::Task) {
        throw DiagnosticException(
            Diagnostic::Error(
                expression.sourceRange,
                fmt::format(
                    "task call '{}' is not yet supported", func->name)));
      }

      // Build qualified function name (e.g., "MyPkg::add" for package
      // functions)
      std::string qualified_name = std::string(func->name);
      const slang::ast::Scope* parent_scope = func->getParentScope();
      if (parent_scope != nullptr) {
        const auto& parent_symbol = parent_scope->asSymbol();
        if (parent_symbol.kind == slang::ast::SymbolKind::Package) {
          qualified_name =
              std::string(parent_symbol.name) + "::" + qualified_name;
        }
      }

      // Lower arguments
      std::vector<std::unique_ptr<mir::Expression>> arguments;
      for (const auto* arg : call.arguments()) {
        arguments.push_back(LowerExpression(*arg));
      }

      // Get return type
      auto return_type_result = LowerType(*call.type, expression.sourceRange);
      if (!return_type_result) {
        throw DiagnosticException(std::move(return_type_result.error()));
      }

      return std::make_unique<mir::FunctionCallExpression>(
          std::move(qualified_name), std::move(arguments), *return_type_result);
    }
  }

  // Fallback: unsupported subroutine call
  throw DiagnosticException(
      Diagnostic::Error(
          expression.sourceRange,
          fmt::format(
              "unsupported subroutine call '{}'", call.getSubroutineName())));
}

}  // namespace lyra::lowering::ast_to_mir
