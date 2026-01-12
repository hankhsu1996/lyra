#include "lyra/lowering/ast_to_mir/expression.hpp"

#include <cstddef>
#include <cstdint>
#include <memory>
#include <span>
#include <string>
#include <utility>
#include <variant>
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
#include <slang/ast/symbols/ParameterSymbols.h>
#include <slang/ast/symbols/VariableSymbols.h>
#include <slang/ast/types/AllTypes.h>

#include "lyra/common/diagnostic.hpp"
#include "lyra/common/type.hpp"
#include "lyra/common/type_arena.hpp"
#include "lyra/lowering/ast_to_mir/assignment.hpp"
#include "lyra/lowering/ast_to_mir/call.hpp"
#include "lyra/lowering/ast_to_mir/literal.hpp"
#include "lyra/lowering/ast_to_mir/type.hpp"
#include "lyra/mir/expression.hpp"
#include "lyra/mir/operators.hpp"

namespace lyra::lowering::ast_to_mir {

auto LowerExpression(
    const slang::ast::Expression& expression, common::TypeArena& arena)
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

    case slang::ast::ExpressionKind::UnbasedUnsizedIntegerLiteral: {
      const auto& literal =
          expression.as<slang::ast::UnbasedUnsizedIntegerLiteral>();
      auto mir_literal_result = LowerLiteral(literal);
      if (!mir_literal_result) {
        throw DiagnosticException(std::move(mir_literal_result.error()));
      }
      return std::make_unique<mir::LiteralExpression>(
          std::move(*mir_literal_result));
    }

    case slang::ast::ExpressionKind::StringLiteral: {
      // LowerLiteral handles both string and integral contexts:
      // - Integral context (bit[N]): returns integral literal with
      // is_string_literal=true
      // - String context: returns string literal
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

      // Handle enum value references - emit as EnumValueExpression
      if (named_value.symbol.kind == slang::ast::SymbolKind::EnumValue) {
        const auto& enum_val =
            named_value.symbol.as<slang::ast::EnumValueSymbol>();
        const auto& cv = enum_val.getValue();
        int64_t value = cv.integer().as<int64_t>().value_or(0);

        // Get enum type name from parent scope
        const auto* parent_scope = enum_val.getParentScope();
        std::string enum_name = std::string(
            parent_scope->asSymbol().as<slang::ast::EnumType>().name);

        auto type_result = LowerType(
            named_value.symbol.getType(), expression.sourceRange, arena);
        if (!type_result) {
          throw DiagnosticException(std::move(type_result.error()));
        }

        return std::make_unique<mir::EnumValueExpression>(
            *type_result, std::move(enum_name), std::string(enum_val.name),
            value);
      }

      // Handle parameter references
      if (named_value.symbol.kind == slang::ast::SymbolKind::Parameter) {
        const auto& param =
            named_value.symbol.as<slang::ast::ParameterSymbol>();

        // Only preserve as identifier for port parameters with non-template
        // types. Localparams are always inlined since they're internal
        // constants.
        if (param.isPortParam()) {
          auto param_type_result =
              LowerType(param.getType(), expression.sourceRange, arena);
          if (!param_type_result) {
            throw DiagnosticException(std::move(param_type_result.error()));
          }

          if (!common::IsTemplateParamType(*param_type_result)) {
            // Constructor params (non-template port params): preserve as
            // identifier for runtime access via class member
            return std::make_unique<mir::IdentifierExpression>(
                *param_type_result, &named_value.symbol);
          }
        }

        // Template params and localparams: inline the constant value
        const auto& cv = param.getValue();
        auto expr_result = ConstantValueToExpression(
            cv, param.getType(), expression.sourceRange, arena);
        if (!expr_result) {
          throw DiagnosticException(std::move(expr_result.error()));
        }
        return std::move(*expr_result);
      }

      // Regular variable reference
      auto type_result = LowerType(
          named_value.symbol.getType(), expression.sourceRange, arena);
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

      auto operand = LowerExpression(unary_expression.operand(), arena);

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

      auto left = LowerExpression(binary_expression.left(), arena);
      auto right = LowerExpression(binary_expression.right(), arena);

      // Get result type from slang (important for comparison operators which
      // return 1-bit, not the operand type)
      auto type_result =
          LowerType(*expression.type, expression.sourceRange, arena);
      if (!type_result) {
        throw DiagnosticException(std::move(type_result.error()));
      }

      return std::make_unique<mir::BinaryExpression>(
          mir_operator, std::move(left), std::move(right), *type_result);
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
          LowerExpression(*conditional_expression.conditions[0].expr, arena);
      auto true_expression =
          LowerExpression(conditional_expression.left(), arena);
      auto false_expression =
          LowerExpression(conditional_expression.right(), arena);
      return std::make_unique<mir::TernaryExpression>(
          std::move(condition), std::move(true_expression),
          std::move(false_expression));
    }

    case slang::ast::ExpressionKind::Assignment:
      return LowerAssignment(
          expression.as<slang::ast::AssignmentExpression>(), arena);

    case slang::ast::ExpressionKind::ElementSelect: {
      const auto& element_select =
          expression.as<slang::ast::ElementSelectExpression>();
      auto array_value = LowerExpression(element_select.value(), arena);
      auto selector = LowerExpression(element_select.selector(), arena);

      // Get the element type from the array type
      auto type_result =
          LowerType(*element_select.type, expression.sourceRange, arena);
      if (!type_result) {
        throw DiagnosticException(std::move(type_result.error()));
      }

      return std::make_unique<mir::ElementSelectExpression>(
          std::move(array_value), std::move(selector), *type_result);
    }

    case slang::ast::ExpressionKind::RangeSelect: {
      const auto& range_select =
          expression.as<slang::ast::RangeSelectExpression>();

      auto type_result =
          LowerType(*expression.type, expression.sourceRange, arena);
      if (!type_result) {
        throw DiagnosticException(std::move(type_result.error()));
      }

      auto value = LowerExpression(range_select.value(), arena);
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
      auto start = LowerExpression(range_select.left(), arena);

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

    case slang::ast::ExpressionKind::Call:
      return LowerCall(expression.as<slang::ast::CallExpression>(), arena);

    case slang::ast::ExpressionKind::Conversion: {
      const auto& conversion =
          expression.as<slang::ast::ConversionExpression>();
      auto value = LowerExpression(conversion.operand(), arena);
      auto type_result =
          LowerType(*conversion.type, expression.sourceRange, arena);
      if (!type_result) {
        throw DiagnosticException(std::move(type_result.error()));
      }
      return std::make_unique<mir::ConversionExpression>(
          std::move(value), *type_result);
    }

    case slang::ast::ExpressionKind::HierarchicalValue: {
      const auto& hier_expr =
          expression.as<slang::ast::HierarchicalValueExpression>();

      // Target symbol
      mir::SymbolRef target_symbol = &hier_expr.symbol;

      // Instance path with array indices (all but last element in path)
      std::vector<mir::HierarchicalPathElement> instance_path;
      for (size_t i = 0; i + 1 < hier_expr.ref.path.size(); ++i) {
        const auto& elem = hier_expr.ref.path[i];
        if (const auto* idx = std::get_if<int32_t>(&elem.selector)) {
          instance_path.emplace_back(elem.symbol, *idx);
        } else {
          instance_path.emplace_back(elem.symbol);
        }
      }

      // Get type from the expression
      auto type_result =
          LowerType(*expression.type, expression.sourceRange, arena);
      if (!type_result) {
        throw DiagnosticException(std::move(type_result.error()));
      }

      return std::make_unique<mir::HierarchicalReferenceExpression>(
          target_symbol, std::move(instance_path), *type_result);
    }

    case slang::ast::ExpressionKind::Concatenation: {
      const auto& concat_expr =
          expression.as<slang::ast::ConcatenationExpression>();

      // Get the result type from slang (already computed with correct width)
      auto type_result =
          LowerType(*expression.type, expression.sourceRange, arena);
      if (!type_result) {
        throw DiagnosticException(std::move(type_result.error()));
      }

      // Lower all operands, skipping void-type operands (zero replications)
      std::vector<std::unique_ptr<mir::Expression>> operands;
      operands.reserve(concat_expr.operands().size());
      for (const auto* operand : concat_expr.operands()) {
        if (operand->type->isVoid()) {
          // Skip zero-width operands (e.g., {0{x}} inside concatenation)
          continue;
        }
        operands.push_back(LowerExpression(*operand, arena));
      }

      // For queues/dynamic arrays, create an ArrayLiteralExpression
      // (assignment patterns used to initialize unpacked arrays)
      if (type_result->IsQueue() || type_result->IsDynamicArray()) {
        return std::make_unique<mir::ArrayLiteralExpression>(
            *type_result, std::move(operands));
      }

      // For packed types, create a ConcatenationExpression
      return std::make_unique<mir::ConcatenationExpression>(
          std::move(operands), *type_result);
    }

    case slang::ast::ExpressionKind::Replication: {
      const auto& rep_expr = expression.as<slang::ast::ReplicationExpression>();

      // Get count - slang guarantees it's a constant integer and already
      // evaluated
      const auto* count_const = rep_expr.count().getConstant();
      if (count_const == nullptr || !count_const->isInteger()) {
        throw DiagnosticException(
            Diagnostic::Error(
                expression.sourceRange, "replication count must be constant"));
      }
      auto count = count_const->integer().as<size_t>();
      if (!count) {
        throw DiagnosticException(
            Diagnostic::Error(
                expression.sourceRange, "replication count out of range"));
      }

      // Lower the inner expression (concat() returns the replicated expression)
      auto operand = LowerExpression(rep_expr.concat(), arena);

      // Get the result type from slang
      auto type_result =
          LowerType(*expression.type, expression.sourceRange, arena);
      if (!type_result) {
        throw DiagnosticException(std::move(type_result.error()));
      }

      return std::make_unique<mir::ReplicationExpression>(
          std::move(operand), *count, *type_result);
    }

    case slang::ast::ExpressionKind::MemberAccess: {
      const auto& member_access =
          expression.as<slang::ast::MemberAccessExpression>();
      auto value = LowerExpression(member_access.value(), arena);

      // Get field info from the FieldSymbol
      const auto& field = member_access.member.as<slang::ast::FieldSymbol>();

      auto type_result =
          LowerType(*expression.type, expression.sourceRange, arena);
      if (!type_result) {
        throw DiagnosticException(std::move(type_result.error()));
      }

      // For packed structs: bit_offset is LSB position, bit_width is field
      // width. For unpacked structs/unions: bit_offset is field index,
      // bit_width is 0 (unused).
      // Use getCanonicalType() to handle typedef'd struct/union types
      const auto& value_type = member_access.value().type->getCanonicalType();
      bool is_unpacked_struct =
          value_type.kind == slang::ast::SymbolKind::UnpackedStructType;
      bool is_unpacked_union =
          value_type.kind == slang::ast::SymbolKind::UnpackedUnionType;

      if (is_unpacked_struct) {
        // Compute field index for unpacked struct
        const auto& struct_type =
            value_type.as<slang::ast::UnpackedStructType>();
        size_t field_index = 0;
        for (const auto* f : struct_type.fields) {
          if (f == &field) {
            break;
          }
          ++field_index;
        }
        return std::make_unique<mir::MemberAccessExpression>(
            std::move(value), std::string(field.name), field_index, 0,
            *type_result);
      }

      if (is_unpacked_union) {
        // Compute field index for unpacked union
        const auto& union_type = value_type.as<slang::ast::UnpackedUnionType>();
        size_t field_index = 0;
        for (const auto* f : union_type.fields) {
          if (f == &field) {
            break;
          }
          ++field_index;
        }
        return std::make_unique<mir::MemberAccessExpression>(
            std::move(value), std::string(field.name), field_index, 0,
            *type_result);
      }

      // Packed struct: use bit offset and width
      uint64_t bit_offset = field.bitOffset;
      size_t bit_width = field.getType().getBitWidth();
      return std::make_unique<mir::MemberAccessExpression>(
          std::move(value), std::string(field.name), bit_offset, bit_width,
          *type_result);
    }

    case slang::ast::ExpressionKind::SimpleAssignmentPattern:
    case slang::ast::ExpressionKind::StructuredAssignmentPattern: {
      // Both pattern types provide elements() in field declaration order.
      auto lower_struct_literal =
          [&](std::span<const slang::ast::Expression* const> elements)
          -> std::unique_ptr<mir::Expression> {
        if (!expression.type->isStruct()) {
          throw DiagnosticException(
              Diagnostic::Error(
                  expression.sourceRange,
                  "only struct assignment patterns are supported"));
        }

        auto type_result =
            LowerType(*expression.type, expression.sourceRange, arena);
        if (!type_result) {
          throw DiagnosticException(std::move(type_result.error()));
        }

        std::vector<std::unique_ptr<mir::Expression>> operands;
        operands.reserve(elements.size());
        for (const auto* element : elements) {
          operands.push_back(LowerExpression(*element, arena));
        }

        // Check if unpacked struct (use canonical type to handle typedefs)
        const auto& canonical = expression.type->getCanonicalType();
        if (canonical.kind == slang::ast::SymbolKind::UnpackedStructType) {
          // Unpacked struct: create UnpackedStructLiteralExpression
          return std::make_unique<mir::UnpackedStructLiteralExpression>(
              *type_result, std::move(operands));
        }

        // Packed struct: concatenation of fields (first = MSB)
        return std::make_unique<mir::ConcatenationExpression>(
            std::move(operands), *type_result);
      };

      if (expression.kind ==
          slang::ast::ExpressionKind::SimpleAssignmentPattern) {
        return lower_struct_literal(
            expression.as<slang::ast::SimpleAssignmentPatternExpression>()
                .elements());
      }
      return lower_struct_literal(
          expression.as<slang::ast::StructuredAssignmentPatternExpression>()
              .elements());
    }

    case slang::ast::ExpressionKind::NewArray: {
      const auto& new_array = expression.as<slang::ast::NewArrayExpression>();
      auto size = LowerExpression(new_array.sizeExpr(), arena);
      std::unique_ptr<mir::Expression> init = nullptr;
      if (new_array.initExpr() != nullptr) {
        init = LowerExpression(*new_array.initExpr(), arena);
      }
      auto type_result =
          LowerType(*expression.type, expression.sourceRange, arena);
      if (!type_result) {
        throw DiagnosticException(std::move(type_result.error()));
      }
      return std::make_unique<mir::NewArrayExpression>(
          *type_result, std::move(size), std::move(init));
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
