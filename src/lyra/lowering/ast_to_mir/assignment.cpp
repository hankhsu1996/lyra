#include "lyra/lowering/ast_to_mir/assignment.hpp"

#include <algorithm>
#include <cstddef>
#include <cstdint>
#include <memory>
#include <optional>
#include <string>
#include <utility>
#include <variant>
#include <vector>

#include <fmt/format.h>
#include <slang/ast/Expression.h>
#include <slang/ast/expressions/AssignmentExpressions.h>
#include <slang/ast/expressions/ConversionExpression.h>
#include <slang/ast/expressions/MiscExpressions.h>
#include <slang/ast/expressions/OperatorExpressions.h>
#include <slang/ast/expressions/SelectExpressions.h>
#include <slang/ast/symbols/PortSymbols.h>
#include <slang/ast/symbols/VariableSymbols.h>
#include <slang/ast/types/AllTypes.h>

#include "lyra/common/diagnostic.hpp"
#include "lyra/common/type.hpp"
#include "lyra/common/type_arena.hpp"
#include "lyra/lowering/ast_to_mir/expression.hpp"
#include "lyra/lowering/ast_to_mir/symbol_registrar.hpp"
#include "lyra/lowering/ast_to_mir/type.hpp"
#include "lyra/mir/expression.hpp"
#include "lyra/mir/operators.hpp"

namespace lyra::lowering::ast_to_mir {

namespace {

// Helpers for compound assignment lowering.
//
// Slang expands `x op= y` to `x = x op y` where the inner `x` is an
// LValueReference. The assignment node carries explicit metadata:
//   - assignment.op.has_value() -> true for compound assignments
//   - *assignment.op -> the binary operator (source of truth)
//
// We use assignment.op as the authoritative signal, NOT pattern matching.
// These helpers only exist to extract the user's RHS operand from slang's
// expanded BinaryOp, skipping the LValueReference placeholder. Conversion
// nodes are unwrapped only for shape detection; the returned RHS preserves
// any Conversion wrappers for correct type semantics.

auto UnwrapConversions(const slang::ast::Expression& expr)
    -> const slang::ast::Expression& {
  const slang::ast::Expression* current = &expr;
  while (current->kind == slang::ast::ExpressionKind::Conversion) {
    current = &current->as<slang::ast::ConversionExpression>().operand();
  }
  return *current;
}

auto IsLValueReference(const slang::ast::Expression& expr) -> bool {
  return UnwrapConversions(expr).kind ==
         slang::ast::ExpressionKind::LValueReference;
}

auto ExtractCompoundAssignmentRhs(const slang::ast::Expression& rhs)
    -> const slang::ast::Expression& {
  const auto& unwrapped = UnwrapConversions(rhs);

  if (unwrapped.kind != slang::ast::ExpressionKind::BinaryOp) {
    throw DiagnosticException(
        Diagnostic::Error(
            rhs.sourceRange,
            "compound assignment RHS must be a binary expression"));
  }

  const auto& binary = unwrapped.as<slang::ast::BinaryExpression>();
  bool left_is_lvalue_ref = IsLValueReference(binary.left());
  bool right_is_lvalue_ref = IsLValueReference(binary.right());

  // Return raw operand (with Conversion intact) - only unwrapped for detection
  if (left_is_lvalue_ref && !right_is_lvalue_ref) {
    return binary.right();
  }
  if (right_is_lvalue_ref && !left_is_lvalue_ref) {
    return binary.left();
  }

  throw DiagnosticException(
      Diagnostic::Error(
          rhs.sourceRange,
          "compound assignment: expected exactly one LValueReference operand"));
}

}  // namespace

auto LowerAssignment(
    const slang::ast::AssignmentExpression& assignment,
    common::TypeArena& arena, SymbolRegistrar& registrar)
    -> std::unique_ptr<mir::Expression> {
  const auto& left = assignment.left();
  auto is_non_blocking = assignment.isNonBlocking();

  // For compound assignments (x op= y), we don't use slang's expanded form.
  // Instead, we store the operator and just the RHS operand. The MIR->LIR
  // lowering will generate: ptr = resolve(target); old = load(ptr);
  //                         new = old op rhs; store(ptr, new)
  // This ensures single evaluation of the lvalue for read-modify-write.
  std::optional<mir::BinaryOperator> compound_op;
  std::unique_ptr<mir::Expression> value;

  if (assignment.op.has_value()) {
    // Compound assignment: extract RHS and convert operator
    compound_op = mir::ConvertSlangBinaryOperatorToMir(*assignment.op);
    const auto& rhs = ExtractCompoundAssignmentRhs(assignment.right());
    value = LowerExpression(rhs, arena, registrar);
  } else {
    // Regular assignment
    value = LowerExpression(assignment.right(), arena, registrar);
  }

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

    auto target_id = registrar.Register(&target);
    if (compound_op) {
      mir::AssignmentTarget mir_target(target_id);
      return std::make_unique<mir::AssignmentExpression>(
          std::move(mir_target), std::move(value), is_non_blocking,
          compound_op);
    }
    return std::make_unique<mir::AssignmentExpression>(
        target_id, std::move(value), is_non_blocking);
  }

  // Element select assignment (arr[i] = value or arr[i][j] = value)
  if (left.kind == slang::ast::ExpressionKind::ElementSelect) {
    // Collect all indices from nested element selects (for multi-dim)
    std::vector<std::unique_ptr<mir::Expression>> indices;
    const slang::ast::Expression* current = &left;

    while (current->kind == slang::ast::ExpressionKind::ElementSelect) {
      const auto& es = current->as<slang::ast::ElementSelectExpression>();
      indices.push_back(LowerExpression(es.selector(), arena, registrar));
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
        LowerType(array_symbol.getType(), current->sourceRange, arena);
    if (!base_type_result) {
      throw DiagnosticException(std::move(base_type_result.error()));
    }

    mir::AssignmentTarget target(
        registrar.Register(&array_symbol), std::move(indices),
        *base_type_result);
    return std::make_unique<mir::AssignmentExpression>(
        std::move(target), std::move(value), is_non_blocking, compound_op);
  }

  // Hierarchical assignment (child.signal = value or gen_block[0].signal =
  // value)
  if (left.kind == slang::ast::ExpressionKind::HierarchicalValue) {
    const auto& hier_expr = left.as<slang::ast::HierarchicalValueExpression>();

    // Target symbol (the variable being assigned)
    mir::SymbolId target_symbol = registrar.Register(&hier_expr.symbol);

    // Instance path with array indices (for path traversal)
    std::vector<mir::HierarchicalPathElement> instance_path;
    for (size_t i = 0; i + 1 < hier_expr.ref.path.size(); ++i) {
      const auto& elem = hier_expr.ref.path[i];
      auto elem_id = registrar.Register(elem.symbol);
      if (const auto* idx = std::get_if<int32_t>(&elem.selector)) {
        instance_path.emplace_back(elem_id, *idx);
      } else {
        instance_path.emplace_back(elem_id);
      }
    }

    mir::AssignmentTarget target(target_symbol, std::move(instance_path));
    return std::make_unique<mir::AssignmentExpression>(
        std::move(target), std::move(value), is_non_blocking, compound_op);
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
          LowerType(field.getType(), member_access.sourceRange, arena);
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
    auto root_type_result = LowerType(
        current->type->getCanonicalType(), current->sourceRange, arena);
    if (!root_type_result) {
      throw DiagnosticException(std::move(root_type_result.error()));
    }
    root_type = *root_type_result;

    // Reverse the field path (we built it from leaf to root)
    std::ranges::reverse(field_path);

    mir::AssignmentTarget target(
        registrar.Register(root_symbol), std::move(field_path), root_type);
    return std::make_unique<mir::AssignmentExpression>(
        std::move(target), std::move(value), is_non_blocking, compound_op);
  }

  throw DiagnosticException(
      Diagnostic::Error(
          left.sourceRange, fmt::format(
                                "unsupported assignment target kind '{}'",
                                slang::ast::toString(left.kind))));
}

}  // namespace lyra::lowering::ast_to_mir
