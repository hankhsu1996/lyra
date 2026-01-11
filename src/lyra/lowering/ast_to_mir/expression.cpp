#include "lyra/lowering/ast_to_mir/expression.hpp"

#include <algorithm>
#include <cstddef>
#include <cstdint>
#include <memory>
#include <optional>
#include <span>
#include <string>
#include <utility>
#include <vector>

#include <fmt/format.h>
#include <slang/ast/Compilation.h>
#include <slang/ast/Expression.h>
#include <slang/ast/expressions/AssignmentExpressions.h>
#include <slang/ast/expressions/CallExpression.h>
#include <slang/ast/expressions/ConversionExpression.h>
#include <slang/ast/expressions/LiteralExpressions.h>
#include <slang/ast/expressions/MiscExpressions.h>
#include <slang/ast/expressions/OperatorExpressions.h>
#include <slang/ast/expressions/SelectExpressions.h>
#include <slang/ast/symbols/CompilationUnitSymbols.h>
#include <slang/ast/symbols/ParameterSymbols.h>
#include <slang/ast/symbols/PortSymbols.h>
#include <slang/ast/symbols/SubroutineSymbols.h>
#include <slang/ast/types/AllTypes.h>
#include <slang/text/SourceManager.h>
#include <spdlog/spdlog.h>

#include "lyra/common/builtin_method.hpp"
#include "lyra/common/diagnostic.hpp"
#include "lyra/common/literal.hpp"
#include "lyra/common/system_function.hpp"
#include "lyra/common/type.hpp"
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

        auto type_result =
            LowerType(named_value.symbol.getType(), expression.sourceRange);
        if (!type_result) {
          throw DiagnosticException(std::move(type_result.error()));
        }

        return std::make_unique<mir::EnumValueExpression>(
            *type_result, std::move(enum_name), std::string(enum_val.name),
            value);
      }

      // Handle parameter references - evaluate to constant value
      if (named_value.symbol.kind == slang::ast::SymbolKind::Parameter) {
        const auto& param =
            named_value.symbol.as<slang::ast::ParameterSymbol>();
        const auto& cv = param.getValue();
        auto literal_result = ConstantValueToLiteral(cv);
        if (!literal_result) {
          throw DiagnosticException(
              Diagnostic::Error(
                  expression.sourceRange,
                  fmt::format(
                      "unsupported parameter '{}': {}", param.name,
                      literal_result.error().message)));
        }
        return std::make_unique<mir::LiteralExpression>(
            std::move(*literal_result));
      }

      // Regular variable reference
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

      // Get result type from slang (important for comparison operators which
      // return 1-bit, not the operand type)
      auto type_result = LowerType(*expression.type, expression.sourceRange);
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

      // Element select assignment (arr[i] = value or arr[i][j] = value)
      if (left.kind == slang::ast::ExpressionKind::ElementSelect) {
        // Collect all indices from nested element selects (for multi-dim)
        std::vector<std::unique_ptr<mir::Expression>> indices;
        const slang::ast::Expression* current = &left;

        while (current->kind == slang::ast::ExpressionKind::ElementSelect) {
          const auto& es = current->as<slang::ast::ElementSelectExpression>();
          indices.push_back(LowerExpression(es.selector()));
          current = &es.value();
        }

        // Now current should be a NamedValue (the base variable)
        if (current->kind != slang::ast::ExpressionKind::NamedValue) {
          throw DiagnosticException(
              Diagnostic::Error(
                  current->sourceRange, "unsupported assignment target"));
        }

        // Reverse indices: we collected outer-to-inner, need inner-to-outer
        // For data[i][j], we collected [j, i], need [i, j]
        std::ranges::reverse(indices);

        const auto& array_symbol =
            current->as<slang::ast::NamedValueExpression>().symbol;

        // Lower base type to distinguish packed vs unpacked
        auto base_type_result =
            LowerType(array_symbol.getType(), current->sourceRange);
        if (!base_type_result) {
          throw DiagnosticException(std::move(base_type_result.error()));
        }

        mir::AssignmentTarget target(
            &array_symbol, std::move(indices), *base_type_result);
        return std::make_unique<mir::AssignmentExpression>(
            std::move(target), std::move(value), is_non_blocking);
      }

      // Hierarchical assignment (child.signal = value)
      if (left.kind == slang::ast::ExpressionKind::HierarchicalValue) {
        const auto& hier_expr =
            left.as<slang::ast::HierarchicalValueExpression>();

        // Target symbol (the variable being assigned)
        mir::SymbolRef target_symbol = &hier_expr.symbol;

        // Instance symbols (for path traversal)
        std::vector<mir::SymbolRef> instance_path;
        for (size_t i = 0; i + 1 < hier_expr.ref.path.size(); ++i) {
          instance_path.push_back(hier_expr.ref.path[i].symbol);
        }

        mir::AssignmentTarget target(target_symbol, std::move(instance_path));
        return std::make_unique<mir::AssignmentExpression>(
            std::move(target), std::move(value), is_non_blocking);
      }

      // Struct field assignment (my_struct.field = value)
      if (left.kind == slang::ast::ExpressionKind::MemberAccess) {
        const auto& member_access =
            left.as<slang::ast::MemberAccessExpression>();
        const auto& field = member_access.member.as<slang::ast::FieldSymbol>();

        // Get the struct variable (for now, require simple NamedValue)
        const auto& struct_expr = member_access.value();
        if (struct_expr.kind != slang::ast::ExpressionKind::NamedValue) {
          throw DiagnosticException(
              Diagnostic::Error(
                  struct_expr.sourceRange,
                  "only simple struct variables supported as assignment "
                  "target"));
        }

        const auto& struct_symbol =
            struct_expr.as<slang::ast::NamedValueExpression>().symbol;

        auto struct_type_result = LowerType(
            struct_expr.type->getCanonicalType(), struct_expr.sourceRange);
        if (!struct_type_result) {
          throw DiagnosticException(std::move(struct_type_result.error()));
        }

        // For unpacked structs, use field index instead of bit offset
        // Use getCanonicalType() to handle typedef'd struct types
        const auto& value_type = member_access.value().type->getCanonicalType();
        bool is_unpacked_struct =
            value_type.kind == slang::ast::SymbolKind::UnpackedStructType;

        uint64_t offset_or_index = 0;
        size_t width = 0;
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
          offset_or_index = field_index;
          width = 0;  // Not used for unpacked structs
        } else {
          // Packed struct: use bit offset and width
          offset_or_index = field.bitOffset;
          width = field.getType().getBitWidth();
        }

        mir::AssignmentTarget target(
            &struct_symbol, std::string(field.name), offset_or_index, width,
            *struct_type_result);
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

      // Handle compile-time constant calls (includes enum methods like first(),
      // last()). slang evaluates these at compile time.
      if (const auto* cv = call_expression.getConstant(); cv != nullptr) {
        auto type_result =
            LowerType(*call_expression.type, expression.sourceRange);
        if (!type_result) {
          throw DiagnosticException(std::move(type_result.error()));
        }

        if (type_result->kind == common::Type::Kind::kIntegral) {
          int64_t value = cv->integer().as<int64_t>().value_or(0);
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
      auto subroutine_name = call_expression.getSubroutineName();
      if (!call_expression.arguments().empty()) {
        const auto& first_arg = *call_expression.arguments()[0];
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
              int64_t value =
                  it->getValue().integer().as<int64_t>().value_or(0);
              auto type_result =
                  LowerType(*call_expression.type, expression.sourceRange);
              if (!type_result) {
                throw DiagnosticException(std::move(type_result.error()));
              }
              const auto& integral_data =
                  std::get<common::IntegralData>(type_result->data);
              auto literal = integral_data.is_signed
                                 ? common::Literal::IntegralSigned(
                                       value, integral_data.bit_width)
                                 : common::Literal::IntegralUnsigned(
                                       static_cast<uint64_t>(value),
                                       integral_data.bit_width);
              return std::make_unique<mir::LiteralExpression>(
                  std::move(literal));
            }
          }
          if (subroutine_name == "last") {
            // last() returns the last enum value
            int64_t last_value = 0;
            for (const auto& member : enum_type.values()) {
              last_value =
                  member.getValue().integer().as<int64_t>().value_or(0);
            }
            auto type_result =
                LowerType(*call_expression.type, expression.sourceRange);
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
            if (call_expression.arguments().size() > 1) {
              const auto* step_cv =
                  call_expression.arguments()[1]->getConstant();
              if (step_cv == nullptr) {
                throw DiagnosticException(
                    Diagnostic::Error(
                        call_expression.arguments()[1]->sourceRange,
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

            auto type_result =
                LowerType(*call_expression.type, expression.sourceRange);
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
            auto return_type = (method_info->return_type ==
                                common::BuiltinMethodReturnType::kInt)
                                   ? common::Type::Int()
                                   : common::Type::Void();
            return std::make_unique<mir::MethodCallExpression>(
                return_type, std::move(receiver), std::string(subroutine_name));
          }
        }
      }

      if (call_expression.isSystemCall()) {
        auto name = call_expression.getSubroutineName();

        // Validate supported system calls using registry
        if (!common::IsSystemFunctionSupported(name)) {
          throw DiagnosticException(
              Diagnostic::Error(
                  expression.sourceRange,
                  fmt::format("unsupported system call '{}'", name)));
        }

        // Get task/function distinction from slang
        bool is_task = call_expression.getSubroutineKind() ==
                       slang::ast::SubroutineKind::Task;

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
          const auto& sys_info = std::get<1>(call_expression.subroutine);
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
            if (!call_expression.arguments().empty()) {
              arguments.push_back(
                  LowerExpression(*call_expression.arguments()[0]));
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
          if (format_idx < call_expression.arguments().size()) {
            const auto& format_arg = *call_expression.arguments()[format_idx];
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
          for (size_t i = args_start_idx;
               i < call_expression.arguments().size(); ++i) {
            arguments.push_back(
                LowerExpression(*call_expression.arguments()[i]));
          }
        } else if (!is_root_variant) {
          // Handle $value$plusargs specially: second argument is output target
          if (name == "$value$plusargs") {
            auto args = call_expression.arguments();
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
            for (const auto* arg : call_expression.arguments()) {
              arguments.push_back(LowerExpression(*arg));
            }
          }
          // For mem_io tasks, check if first arg (filename) is a string literal
          // This is needed by interpreter to properly decode the filename
          bool is_mem_io =
              func_info != nullptr &&
              func_info->category == common::SystemFunctionCategory::kMemIo;
          if (is_mem_io && !call_expression.arguments().empty()) {
            const auto& first_arg = *call_expression.arguments()[0];
            if (first_arg.kind == slang::ast::ExpressionKind::StringLiteral) {
              format_expr_is_literal = true;
            }
          }
        }

        auto return_type_result =
            LowerType(*call_expression.type, expression.sourceRange);
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
      if (call_expression.subroutine.index() == 0) {
        const auto* func = std::get<0>(call_expression.subroutine);
        if (func != nullptr &&
            func->kind == slang::ast::SymbolKind::Subroutine) {
          const auto& subroutine_sym = func->as<slang::ast::SubroutineSymbol>();

          // Tasks not yet supported
          if (subroutine_sym.subroutineKind ==
              slang::ast::SubroutineKind::Task) {
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
          for (const auto* arg : call_expression.arguments()) {
            arguments.push_back(LowerExpression(*arg));
          }

          // Get return type
          auto return_type_result =
              LowerType(*call_expression.type, expression.sourceRange);
          if (!return_type_result) {
            throw DiagnosticException(std::move(return_type_result.error()));
          }

          return std::make_unique<mir::FunctionCallExpression>(
              std::move(qualified_name), std::move(arguments),
              *return_type_result);
        }
      }

      // Fallback: unsupported subroutine call
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

    case slang::ast::ExpressionKind::HierarchicalValue: {
      const auto& hier_expr =
          expression.as<slang::ast::HierarchicalValueExpression>();

      // Target symbol
      mir::SymbolRef target_symbol = &hier_expr.symbol;

      // Instance symbols (all but last element in path)
      std::vector<mir::SymbolRef> instance_path;
      for (size_t i = 0; i + 1 < hier_expr.ref.path.size(); ++i) {
        instance_path.push_back(hier_expr.ref.path[i].symbol);
      }

      // Get type from the expression
      auto type_result = LowerType(*expression.type, expression.sourceRange);
      if (!type_result) {
        throw DiagnosticException(std::move(type_result.error()));
      }

      return std::make_unique<mir::HierarchicalReferenceExpression>(
          target_symbol, std::move(instance_path), *type_result);
    }

    case slang::ast::ExpressionKind::Concatenation: {
      const auto& concat_expr =
          expression.as<slang::ast::ConcatenationExpression>();

      // Lower all operands, skipping void-type operands (zero replications)
      std::vector<std::unique_ptr<mir::Expression>> operands;
      operands.reserve(concat_expr.operands().size());
      for (const auto* operand : concat_expr.operands()) {
        if (operand->type->isVoid()) {
          // Skip zero-width operands (e.g., {0{x}} inside concatenation)
          continue;
        }
        operands.push_back(LowerExpression(*operand));
      }

      // Get the result type from slang (already computed with correct width)
      auto type_result = LowerType(*expression.type, expression.sourceRange);
      if (!type_result) {
        throw DiagnosticException(std::move(type_result.error()));
      }

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
      auto operand = LowerExpression(rep_expr.concat());

      // Get the result type from slang
      auto type_result = LowerType(*expression.type, expression.sourceRange);
      if (!type_result) {
        throw DiagnosticException(std::move(type_result.error()));
      }

      return std::make_unique<mir::ReplicationExpression>(
          std::move(operand), *count, *type_result);
    }

    case slang::ast::ExpressionKind::MemberAccess: {
      const auto& member_access =
          expression.as<slang::ast::MemberAccessExpression>();
      auto value = LowerExpression(member_access.value());

      // Get field info from the FieldSymbol
      const auto& field = member_access.member.as<slang::ast::FieldSymbol>();

      auto type_result = LowerType(*expression.type, expression.sourceRange);
      if (!type_result) {
        throw DiagnosticException(std::move(type_result.error()));
      }

      // For packed structs: bit_offset is LSB position, bit_width is field
      // width. For unpacked structs: bit_offset is field index, bit_width is 0
      // (unused).
      // Use getCanonicalType() to handle typedef'd struct types
      const auto& value_type = member_access.value().type->getCanonicalType();
      bool is_unpacked_struct =
          value_type.kind == slang::ast::SymbolKind::UnpackedStructType;

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

        auto type_result = LowerType(*expression.type, expression.sourceRange);
        if (!type_result) {
          throw DiagnosticException(std::move(type_result.error()));
        }

        std::vector<std::unique_ptr<mir::Expression>> operands;
        operands.reserve(elements.size());
        for (const auto* element : elements) {
          operands.push_back(LowerExpression(*element));
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
      auto size = LowerExpression(new_array.sizeExpr());
      std::unique_ptr<mir::Expression> init = nullptr;
      if (new_array.initExpr() != nullptr) {
        init = LowerExpression(*new_array.initExpr());
      }
      auto type_result = LowerType(*expression.type, expression.sourceRange);
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
