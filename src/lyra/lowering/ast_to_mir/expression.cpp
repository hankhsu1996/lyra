#include "lyra/lowering/ast_to_mir/expression.hpp"

#include <cstdint>
#include <memory>
#include <utility>
#include <vector>

#include <fmt/format.h>
#include <slang/ast/Expression.h>
#include <slang/ast/expressions/AssignmentExpressions.h>
#include <slang/ast/expressions/CallExpression.h>
#include <slang/ast/expressions/ConversionExpression.h>
#include <slang/ast/expressions/LiteralExpressions.h>
#include <slang/ast/expressions/MiscExpressions.h>
#include <slang/ast/expressions/OperatorExpressions.h>
#include <slang/ast/expressions/SelectExpressions.h>
#include <slang/ast/symbols/PortSymbols.h>
#include <spdlog/spdlog.h>

#include "lyra/common/diagnostic.hpp"
#include "lyra/lowering/ast_to_mir/literal.hpp"
#include "lyra/lowering/ast_to_mir/type.hpp"
#include "lyra/mir/expression.hpp"
#include "lyra/mir/operators.hpp"

namespace lyra::lowering::ast_to_mir {

auto LowerExpression(const slang::ast::Expression& expression)
    -> std::unique_ptr<mir::Expression> {
  switch (expression.kind) {
    case slang::ast::ExpressionKind::IntegerLiteral: {
      const auto& literal = expression.as<slang::ast::IntegerLiteral>();
      auto mir_literal_result = LowerLiteral(literal);
      if (!mir_literal_result) {
        throw DiagnosticException(std::move(mir_literal_result.error()));
      }
      return std::make_unique<mir::LiteralExpression>(
          std::move(*mir_literal_result));
    }

    case slang::ast::ExpressionKind::StringLiteral: {
      const auto& literal = expression.as<slang::ast::StringLiteral>();
      auto mir_literal = LowerLiteral(literal);
      return std::make_unique<mir::LiteralExpression>(std::move(mir_literal));
    }

    case slang::ast::ExpressionKind::RealLiteral: {
      const auto& literal = expression.as<slang::ast::RealLiteral>();
      auto mir_literal = LowerLiteral(literal);
      return std::make_unique<mir::LiteralExpression>(std::move(mir_literal));
    }

    case slang::ast::ExpressionKind::NamedValue: {
      const auto& named_value =
          expression.as<slang::ast::NamedValueExpression>();
      auto type_result =
          LowerType(named_value.symbol.getType(), expression.sourceRange);
      if (!type_result) {
        throw DiagnosticException(std::move(type_result.error()));
      }
      return std::make_unique<mir::IdentifierExpression>(
          *type_result, &named_value.symbol);
    }

    case slang::ast::ExpressionKind::UnaryOp: {
      const auto& unary_expression =
          expression.as<slang::ast::UnaryExpression>();

      auto mir_operator =
          mir::ConvertSlangUnaryOperatorToMir(unary_expression.op);

      // Check increment/decrement requires a variable operand
      using Op = mir::UnaryOperator;
      bool is_inc_dec = mir_operator == Op::kPreincrement ||
                        mir_operator == Op::kPostincrement ||
                        mir_operator == Op::kPredecrement ||
                        mir_operator == Op::kPostdecrement;
      if (is_inc_dec && unary_expression.operand().kind !=
                            slang::ast::ExpressionKind::NamedValue) {
        throw DiagnosticException(
            Diagnostic::Error(
                expression.sourceRange,
                "increment/decrement requires a variable operand"));
      }

      auto operand = LowerExpression(unary_expression.operand());

      return std::make_unique<mir::UnaryExpression>(
          mir_operator, std::move(operand));
    }

    case slang::ast::ExpressionKind::BinaryOp: {
      const auto& binary_expression =
          expression.as<slang::ast::BinaryExpression>();

      auto mir_operator =
          mir::ConvertSlangBinaryOperatorToMir(binary_expression.op);

      // Check for unsupported operators
      using Op = mir::BinaryOperator;
      if (mir_operator == Op::kLogicalImplication ||
          mir_operator == Op::kLogicalEquivalence) {
        throw DiagnosticException(
            Diagnostic::Error(
                expression.sourceRange,
                fmt::format("unsupported operator '{}'", mir_operator)));
      }
      if (mir_operator == Op::kCaseEquality ||
          mir_operator == Op::kCaseInequality ||
          mir_operator == Op::kWildcardEquality ||
          mir_operator == Op::kWildcardInequality) {
        throw DiagnosticException(
            Diagnostic::Error(
                expression.sourceRange,
                fmt::format(
                    "operator '{}' is not yet supported", mir_operator)));
      }

      // Check string operand restrictions
      bool has_string_operand = binary_expression.left().type->isString() ||
                                binary_expression.right().type->isString();
      if (has_string_operand && mir_operator != Op::kEquality &&
          mir_operator != Op::kInequality) {
        throw DiagnosticException(
            Diagnostic::Error(
                expression.sourceRange,
                fmt::format(
                    "operator '{}' is not supported for string operands",
                    mir_operator)));
      }

      auto left = LowerExpression(binary_expression.left());
      auto right = LowerExpression(binary_expression.right());

      return std::make_unique<mir::BinaryExpression>(
          mir_operator, std::move(left), std::move(right));
    }

    case slang::ast::ExpressionKind::ConditionalOp: {
      const auto& conditional_expression =
          expression.as<slang::ast::ConditionalExpression>();

      if (conditional_expression.conditions.size() != 1) {
        throw DiagnosticException(
            Diagnostic::Error(
                expression.sourceRange,
                fmt::format(
                    "unsupported conditional expression with {} conditions",
                    conditional_expression.conditions.size())));
      }

      auto condition =
          LowerExpression(*conditional_expression.conditions[0].expr);
      auto true_expression = LowerExpression(conditional_expression.left());
      auto false_expression = LowerExpression(conditional_expression.right());
      return std::make_unique<mir::TernaryExpression>(
          std::move(condition), std::move(true_expression),
          std::move(false_expression));
    }

    case slang::ast::ExpressionKind::Assignment: {
      const auto& assignment =
          expression.as<slang::ast::AssignmentExpression>();
      const auto& left = assignment.left();
      auto value = LowerExpression(assignment.right());
      auto is_non_blocking = assignment.isNonBlocking();

      // Simple variable assignment
      if (left.kind == slang::ast::ExpressionKind::NamedValue) {
        const auto& target = left.as<slang::ast::NamedValueExpression>().symbol;

        // Check if target is an input port (read-only)
        if (const auto* scope = target.getParentScope()) {
          for (const auto& member : scope->members()) {
            if (member.kind == slang::ast::SymbolKind::Port) {
              const auto& port = member.as<slang::ast::PortSymbol>();
              if (port.internalSymbol == &target &&
                  port.direction == slang::ast::ArgumentDirection::In) {
                throw DiagnosticException(
                    Diagnostic::Error(
                        left.sourceRange, fmt::format(
                                              "cannot assign to input port "
                                              "'{}'",
                                              target.name)));
              }
            }
          }
        }

        return std::make_unique<mir::AssignmentExpression>(
            &target, std::move(value), is_non_blocking);
      }

      // Element select assignment (arr[i] = value)
      if (left.kind == slang::ast::ExpressionKind::ElementSelect) {
        const auto& element_select =
            left.as<slang::ast::ElementSelectExpression>();
        const auto& array_expr = element_select.value();

        // For now, we only support direct array variable access
        if (array_expr.kind != slang::ast::ExpressionKind::NamedValue) {
          throw DiagnosticException(
              Diagnostic::Error(
                  array_expr.sourceRange,
                  "only direct array variable assignment is supported"));
        }

        const auto& array_symbol =
            array_expr.as<slang::ast::NamedValueExpression>().symbol;
        auto index = LowerExpression(element_select.selector());

        // Lower base type to distinguish packed vs unpacked
        auto base_type_result =
            LowerType(array_symbol.getType(), array_expr.sourceRange);
        if (!base_type_result) {
          throw DiagnosticException(std::move(base_type_result.error()));
        }

        mir::AssignmentTarget target(
            &array_symbol, std::move(index), *base_type_result);
        return std::make_unique<mir::AssignmentExpression>(
            std::move(target), std::move(value), is_non_blocking);
      }

      // Hierarchical assignment (child.signal = value)
      if (left.kind == slang::ast::ExpressionKind::HierarchicalValue) {
        const auto& hier_expr =
            left.as<slang::ast::HierarchicalValueExpression>();

        // Build hierarchical path from slang's reference info
        // ref.path contains all elements including the final target symbol
        std::vector<std::string> path;
        for (const auto& element : hier_expr.ref.path) {
          path.emplace_back(element.symbol->name);
        }

        mir::AssignmentTarget target(std::move(path));
        return std::make_unique<mir::AssignmentExpression>(
            std::move(target), std::move(value), is_non_blocking);
      }

      throw DiagnosticException(
          Diagnostic::Error(
              left.sourceRange, fmt::format(
                                    "unsupported assignment target kind '{}'",
                                    slang::ast::toString(left.kind))));
    }

    case slang::ast::ExpressionKind::ElementSelect: {
      const auto& element_select =
          expression.as<slang::ast::ElementSelectExpression>();
      auto array_value = LowerExpression(element_select.value());
      auto selector = LowerExpression(element_select.selector());

      // Get the element type from the array type
      auto type_result =
          LowerType(*element_select.type, expression.sourceRange);
      if (!type_result) {
        throw DiagnosticException(std::move(type_result.error()));
      }

      return std::make_unique<mir::ElementSelectExpression>(
          std::move(array_value), std::move(selector), *type_result);
    }

    case slang::ast::ExpressionKind::RangeSelect: {
      const auto& range_select =
          expression.as<slang::ast::RangeSelectExpression>();

      auto type_result = LowerType(*expression.type, expression.sourceRange);
      if (!type_result) {
        throw DiagnosticException(std::move(type_result.error()));
      }

      auto value = LowerExpression(range_select.value());
      auto selection_kind = range_select.getSelectionKind();

      if (selection_kind == slang::ast::RangeSelectionKind::Simple) {
        // Constant range select: a[7:4]
        const auto* left_cv = range_select.left().getConstant();
        const auto* right_cv = range_select.right().getConstant();
        if (left_cv == nullptr || right_cv == nullptr) {
          throw DiagnosticException(
              Diagnostic::Error(
                  expression.sourceRange,
                  "range select bounds must be constant"));
        }

        auto left =
            static_cast<int32_t>(left_cv->integer().as<int64_t>().value());
        auto right =
            static_cast<int32_t>(right_cv->integer().as<int64_t>().value());

        return std::make_unique<mir::RangeSelectExpression>(
            std::move(value), left, right, *type_result);
      }

      // Indexed part-select: a[i+:4] or a[i-:4]
      auto start = LowerExpression(range_select.left());

      // Width must be constant
      const auto* width_cv = range_select.right().getConstant();
      if (width_cv == nullptr) {
        throw DiagnosticException(
            Diagnostic::Error(
                expression.sourceRange,
                "indexed part-select width must be constant"));
      }
      auto width =
          static_cast<int32_t>(width_cv->integer().as<int64_t>().value());

      bool is_ascending =
          (selection_kind == slang::ast::RangeSelectionKind::IndexedUp);

      return std::make_unique<mir::IndexedRangeSelectExpression>(
          std::move(value), std::move(start), is_ascending, width,
          *type_result);
    }

    case slang::ast::ExpressionKind::Call: {
      const auto& call_expression = expression.as<slang::ast::CallExpression>();
      if (call_expression.isSystemCall()) {
        auto name = call_expression.getSubroutineName();

        // Validate supported system calls
        // System tasks (no return value): $finish, $stop, $exit, $display,
        //                                 $timeformat, $printtimescale
        // System functions (return value): $time, $stime, $realtime,
        //                                  $timeunit, $timeprecision
        if (name != "$finish" && name != "$stop" && name != "$exit" &&
            name != "$display" && name != "$timeformat" &&
            name != "$printtimescale" && name != "$time" && name != "$stime" &&
            name != "$realtime" && name != "$timeunit" &&
            name != "$timeprecision") {
          throw DiagnosticException(
              Diagnostic::Error(
                  expression.sourceRange,
                  fmt::format("unsupported system call '{}'", name)));
        }

        // Handle $timeunit($root), $timeprecision($root),
        // $printtimescale($root) Transform to $timeunit_root /
        // $timeprecision_root / $printtimescale_root
        std::string effective_name(name);
        bool is_root_variant = false;
        if ((name == "$timeunit" || name == "$timeprecision" ||
             name == "$printtimescale") &&
            call_expression.arguments().size() == 1) {
          const auto& arg = *call_expression.arguments()[0];
          if (arg.kind == slang::ast::ExpressionKind::ArbitrarySymbol) {
            const auto& arb_sym =
                arg.as<slang::ast::ArbitrarySymbolExpression>();
            if (arb_sym.symbol->name == "$root") {
              effective_name = std::string(name) + "_root";
              is_root_variant = true;
            }
          }
        }

        std::vector<std::unique_ptr<mir::Expression>> arguments;
        // Don't lower arguments for _root variants
        if (!is_root_variant) {
          for (const auto& arg : call_expression.arguments()) {
            arguments.push_back(LowerExpression(*arg));
          }
        }

        auto return_type_result =
            LowerType(*call_expression.type, expression.sourceRange);
        if (!return_type_result) {
          throw DiagnosticException(std::move(return_type_result.error()));
        }
        return std::make_unique<mir::SystemCallExpression>(
            effective_name, std::move(arguments), *return_type_result);
      }

      throw DiagnosticException(
          Diagnostic::Error(
              expression.sourceRange,
              fmt::format(
                  "unsupported subroutine call '{}'",
                  call_expression.getSubroutineName())));
    }

    case slang::ast::ExpressionKind::Conversion: {
      const auto& conversion =
          expression.as<slang::ast::ConversionExpression>();
      auto value = LowerExpression(conversion.operand());
      auto type_result = LowerType(*conversion.type, expression.sourceRange);
      if (!type_result) {
        throw DiagnosticException(std::move(type_result.error()));
      }
      return std::make_unique<mir::ConversionExpression>(
          std::move(value), *type_result);
    }

    case slang::ast::ExpressionKind::Invalid:
      // Slang produces InvalidExpression when it detects semantic issues.
      // Slang should have already reported a diagnostic explaining the problem.
      // We cannot proceed with invalid AST nodes.
      throw DiagnosticException(
          Diagnostic::Error({}, "cannot lower invalid expression"));

    default:
      throw DiagnosticException(
          Diagnostic::Error(
              expression.sourceRange,
              fmt::format(
                  "unsupported expression kind '{}'",
                  slang::ast::toString(expression.kind))));
  }
}

}  // namespace lyra::lowering::ast_to_mir
