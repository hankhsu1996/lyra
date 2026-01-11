#include "lyra/lowering/ast_to_mir/assignment.hpp"

#include <algorithm>
#include <cstddef>
#include <memory>
#include <string>
#include <utility>
#include <vector>

#include <fmt/format.h>
#include <slang/ast/Expression.h>
#include <slang/ast/expressions/AssignmentExpressions.h>
#include <slang/ast/expressions/MiscExpressions.h>
#include <slang/ast/expressions/SelectExpressions.h>
#include <slang/ast/symbols/PortSymbols.h>
#include <slang/ast/symbols/VariableSymbols.h>
#include <slang/ast/types/AllTypes.h>

#include "lyra/common/diagnostic.hpp"
#include "lyra/common/type.hpp"
#include "lyra/lowering/ast_to_mir/expression.hpp"
#include "lyra/lowering/ast_to_mir/type.hpp"
#include "lyra/mir/expression.hpp"

namespace lyra::lowering::ast_to_mir {

auto LowerAssignment(const slang::ast::AssignmentExpression& assignment)
    -> std::unique_ptr<mir::Expression> {
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
    const auto& hier_expr = left.as<slang::ast::HierarchicalValueExpression>();

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

  // Struct/union field assignment (supports nested: s.inner.x = value)
  if (left.kind == slang::ast::ExpressionKind::MemberAccess) {
    // Walk up the member access chain to find the root variable
    // and build the field path along the way
    std::vector<mir::FieldPathElement> field_path;
    const slang::ast::Expression* current = &left;
    const slang::ast::Symbol* root_symbol = nullptr;
    common::Type root_type;

    while (current->kind == slang::ast::ExpressionKind::MemberAccess) {
      const auto& member_access =
          current->as<slang::ast::MemberAccessExpression>();
      const auto& field = member_access.member.as<slang::ast::FieldSymbol>();

      // Get the field type
      auto field_type_result =
          LowerType(field.getType(), member_access.sourceRange);
      if (!field_type_result) {
        throw DiagnosticException(std::move(field_type_result.error()));
      }

      // Determine the aggregate type and compute field index
      const auto& value_type = member_access.value().type->getCanonicalType();
      bool is_unpacked_struct =
          value_type.kind == slang::ast::SymbolKind::UnpackedStructType;
      bool is_unpacked_union =
          value_type.kind == slang::ast::SymbolKind::UnpackedUnionType;

      if (is_unpacked_struct) {
        const auto& struct_type =
            value_type.as<slang::ast::UnpackedStructType>();
        size_t field_index = 0;
        for (const auto* f : struct_type.fields) {
          if (f == &field) {
            break;
          }
          ++field_index;
        }
        field_path.emplace_back(
            std::string(field.name), field_index, *field_type_result);
      } else if (is_unpacked_union) {
        const auto& union_type = value_type.as<slang::ast::UnpackedUnionType>();
        size_t field_index = 0;
        for (const auto* f : union_type.fields) {
          if (f == &field) {
            break;
          }
          ++field_index;
        }
        field_path.emplace_back(
            std::string(field.name), field_index, *field_type_result);
      } else {
        // Packed struct: use bit offset and width
        field_path.emplace_back(
            std::string(field.name), field.bitOffset,
            field.getType().getBitWidth(), *field_type_result);
      }

      current = &member_access.value();
    }

    // Now current should be a NamedValue (the root variable)
    if (current->kind != slang::ast::ExpressionKind::NamedValue) {
      throw DiagnosticException(
          Diagnostic::Error(
              current->sourceRange,
              "nested struct field assignment requires a variable base"));
    }

    root_symbol = &current->as<slang::ast::NamedValueExpression>().symbol;
    auto root_type_result =
        LowerType(current->type->getCanonicalType(), current->sourceRange);
    if (!root_type_result) {
      throw DiagnosticException(std::move(root_type_result.error()));
    }
    root_type = *root_type_result;

    // Reverse the field path (we built it from leaf to root)
    std::ranges::reverse(field_path);

    mir::AssignmentTarget target(root_symbol, std::move(field_path), root_type);
    return std::make_unique<mir::AssignmentExpression>(
        std::move(target), std::move(value), is_non_blocking);
  }

  throw DiagnosticException(
      Diagnostic::Error(
          left.sourceRange, fmt::format(
                                "unsupported assignment target kind '{}'",
                                slang::ast::toString(left.kind))));
}

}  // namespace lyra::lowering::ast_to_mir
