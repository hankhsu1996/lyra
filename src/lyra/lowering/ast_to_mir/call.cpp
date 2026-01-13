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
#include "lyra/common/constant.hpp"
#include "lyra/common/diagnostic.hpp"
#include "lyra/common/system_function.hpp"
#include "lyra/common/type.hpp"
#include "lyra/common/type_arena.hpp"
#include "lyra/lowering/ast_to_mir/expression.hpp"
#include "lyra/lowering/ast_to_mir/type.hpp"
#include "lyra/mir/expression.hpp"

namespace lyra::lowering::ast_to_mir {

auto LowerCall(const slang::ast::CallExpression& call, common::TypeArena& arena)
    -> std::unique_ptr<mir::Expression> {
  const auto& expression = static_cast<const slang::ast::Expression&>(call);

  // Handle compile-time constant calls (includes enum methods like first(),
  // last()). slang evaluates these at compile time.
  if (const auto* cv = call.getConstant(); cv != nullptr) {
    auto type_result = LowerType(*call.type, expression.sourceRange, arena);
    if (!type_result) {
      throw DiagnosticException(std::move(type_result.error()));
    }

    if (type_result->IsBitvector()) {
      int64_t value = cv->integer().as<int64_t>().value_or(0);
      size_t bit_width = type_result->GetBitWidth();
      bool is_signed = type_result->IsSigned();
      auto constant = is_signed
                          ? common::Constant::IntegralSigned(value, bit_width)
                          : common::Constant::IntegralUnsigned(
                                static_cast<uint64_t>(value), bit_width);
      return std::make_unique<mir::ConstantExpression>(std::move(constant));
    }
    if (type_result->kind == common::Type::Kind::kString) {
      return std::make_unique<mir::ConstantExpression>(
          common::Constant::String(std::string(cv->str())));
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
        return std::make_unique<mir::ConstantExpression>(
            common::Constant::Int(static_cast<int32_t>(count)));
      }
      if (subroutine_name == "first") {
        // first() returns the first enum value
        auto it = enum_type.values().begin();
        if (it != enum_type.values().end()) {
          int64_t value = it->getValue().integer().as<int64_t>().value_or(0);
          auto type_result =
              LowerType(*call.type, expression.sourceRange, arena);
          if (!type_result) {
            throw DiagnosticException(std::move(type_result.error()));
          }
          size_t bit_width = type_result->GetBitWidth();
          bool is_signed = type_result->IsSigned();
          auto constant =
              is_signed ? common::Constant::IntegralSigned(value, bit_width)
                        : common::Constant::IntegralUnsigned(
                              static_cast<uint64_t>(value), bit_width);
          return std::make_unique<mir::ConstantExpression>(std::move(constant));
        }
      }
      if (subroutine_name == "last") {
        // last() returns the last enum value
        int64_t last_value = 0;
        for (const auto& member : enum_type.values()) {
          last_value = member.getValue().integer().as<int64_t>().value_or(0);
        }
        auto type_result = LowerType(*call.type, expression.sourceRange, arena);
        if (!type_result) {
          throw DiagnosticException(std::move(type_result.error()));
        }
        size_t bit_width = type_result->GetBitWidth();
        bool is_signed = type_result->IsSigned();
        auto constant =
            is_signed ? common::Constant::IntegralSigned(last_value, bit_width)
                      : common::Constant::IntegralUnsigned(
                            static_cast<uint64_t>(last_value), bit_width);
        return std::make_unique<mir::ConstantExpression>(std::move(constant));
      }

      // Runtime enum methods (next, prev, name)
      // Validate method using registry
      const auto* method_info = common::FindBuiltinMethod(
          common::BuiltinTypeKind::kEnum, subroutine_name);
      if (method_info != nullptr) {
        auto receiver = LowerExpression(first_arg, arena);

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
              std::make_unique<mir::ConstantExpression>(
                  common::Constant::IntegralSigned(step, 32)));
        }

        auto type_result = LowerType(*call.type, expression.sourceRange, arena);
        if (!type_result) {
          throw DiagnosticException(std::move(type_result.error()));
        }

        auto method = mir::ParseBuiltinMethod(subroutine_name);
        if (!method) {
          throw DiagnosticException(
              Diagnostic::Error(
                  expression.sourceRange,
                  fmt::format("unknown enum method '{}'", subroutine_name)));
        }
        return std::make_unique<mir::MethodCallExpression>(
            *type_result, std::move(receiver), *method, std::move(args),
            std::move(members));
      }
    }

    // Handle dynamic array built-in methods: size() and delete()
    // These are type methods, validated using the builtin method registry
    if (first_arg.type->kind == slang::ast::SymbolKind::DynamicArrayType) {
      const auto* method_info = common::FindBuiltinMethod(
          common::BuiltinTypeKind::kDynamicArray, subroutine_name);
      if (method_info != nullptr) {
        auto method = mir::ParseBuiltinMethod(subroutine_name);
        assert(method && "builtin method registry and enum out of sync");
        auto receiver = LowerExpression(first_arg, arena);
        auto return_type =
            (method_info->return_type == common::BuiltinMethodReturnType::kInt)
                ? common::Type::Int()
                : common::Type::Void();
        return std::make_unique<mir::MethodCallExpression>(
            return_type, std::move(receiver), *method);
      }
    }

    // Handle queue built-in methods
    if (first_arg.type->kind == slang::ast::SymbolKind::QueueType) {
      const auto* method_info = common::FindBuiltinMethod(
          common::BuiltinTypeKind::kQueue, subroutine_name);
      if (method_info != nullptr) {
        auto receiver = LowerExpression(first_arg, arena);

        // Determine return type based on method
        // For SSA semantics: mutating methods return the modified receiver
        const auto& queue_ast_type =
            first_arg.type->as<slang::ast::QueueType>();
        auto queue_type_result =
            LowerType(*first_arg.type, expression.sourceRange, arena);
        if (!queue_type_result) {
          throw DiagnosticException(queue_type_result.error());
        }

        common::Type return_type = common::Type::Void();
        if (method_info->return_type == common::BuiltinMethodReturnType::kInt) {
          return_type = common::Type::Int();
        } else if (
            method_info->return_type ==
            common::BuiltinMethodReturnType::kElement) {
          // pop_front/pop_back return the element type
          auto elem_result = LowerType(
              queue_ast_type.elementType, expression.sourceRange, arena);
          if (!elem_result) {
            throw DiagnosticException(elem_result.error());
          }
          return_type = *elem_result;
        } else if (
            method_info->return_type ==
            common::BuiltinMethodReturnType::kVoid) {
          // SSA: mutating methods (push_back, push_front, insert, delete)
          // return the modified receiver (queue type)
          return_type = *queue_type_result;
        }

        auto method = mir::ParseBuiltinMethod(subroutine_name);
        assert(method && "builtin method registry and enum out of sync");

        // Collect method arguments (skip first arg which is receiver)
        std::vector<std::unique_ptr<mir::Expression>> args;
        for (size_t i = 1; i < call.arguments().size(); ++i) {
          args.push_back(LowerExpression(*call.arguments()[i], arena));
        }

        return std::make_unique<mir::MethodCallExpression>(
            return_type, std::move(receiver), *method, std::move(args));
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
          arguments.push_back(LowerExpression(*call.arguments()[0], arena));
        } else {
          // Default finish_number is 1 if no arguments
          arguments.push_back(
              std::make_unique<mir::ConstantExpression>(
                  common::Constant::Int(1)));
        }
        format_idx = 1;
        args_start_idx = 1;
      }

      // Check if format argument is a string literal
      if (format_idx < call.arguments().size()) {
        const auto& format_arg = *call.arguments()[format_idx];
        if (format_arg.kind == slang::ast::ExpressionKind::StringLiteral) {
          // Extract format string into format_expr
          format_expr = LowerExpression(format_arg, arena);
          format_expr_is_literal = true;
          args_start_idx = format_idx + 1;  // Skip format in arguments
        } else {
          // First arg is a value, not a format string
          args_start_idx = format_idx;
        }
      }

      // Add remaining arguments (format args / values to display)
      for (size_t i = args_start_idx; i < call.arguments().size(); ++i) {
        arguments.push_back(LowerExpression(*call.arguments()[i], arena));
      }
    } else if (!is_root_variant) {
      // Handle $value$plusargs specially: second argument is output target
      if (name == "$value$plusargs") {
        auto args = call.arguments();
        if (!args.empty()) {
          // First arg is format string (input)
          arguments.push_back(LowerExpression(*args[0], arena));
        }
        if (args.size() >= 2) {
          // Second arg is output variable
          const auto* output_arg = args[1];
          if (output_arg->kind == slang::ast::ExpressionKind::NamedValue) {
            const auto& named_value =
                output_arg->as<slang::ast::NamedValueExpression>();
            // Get the type of the output variable for codegen
            auto type_result = LowerType(
                named_value.symbol.getType(), output_arg->sourceRange, arena);
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
          arguments.push_back(LowerExpression(*arg, arena));
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

    auto return_type_result =
        LowerType(*call.type, expression.sourceRange, arena);
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
        arguments.push_back(LowerExpression(*arg, arena));
      }

      // Get return type
      auto return_type_result =
          LowerType(*call.type, expression.sourceRange, arena);
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
