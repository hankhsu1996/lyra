#pragma once

#include <optional>

#include <slang/ast/expressions/CallExpression.h>

#include "lyra/hir/fwd.hpp"
#include "lyra/lowering/ast_to_hir/detail/expression_lowering.hpp"

namespace lyra::lowering::ast_to_hir {

// Container type that owns the builtin method.
enum class ContainerKind {
  kDynamicArray,
  kQueue,
  kEnum,
};

// Builtin method kind (shared across container types where applicable).
enum class BuiltinMethodKind {
  kSize,
  kDelete,
  kPushBack,
  kPushFront,
  kPopBack,
  kPopFront,
  kInsert,
  // Enum methods
  kEnumFirst,  // compile-time
  kEnumLast,   // compile-time
  kEnumNum,    // compile-time
  kEnumNext,   // runtime
  kEnumPrev,   // runtime
  kEnumName,   // runtime
};

// Whether the method returns a value or is void.
enum class ReturnKind {
  kVoid,
  kValue,
};

// Classification of a builtin method call based on SV semantics.
// This captures what the method IS, not how to lower it.
struct BuiltinMethodInfo {
  ContainerKind container;
  BuiltinMethodKind method;
  ReturnKind return_kind;
  bool has_side_effect;

  [[nodiscard]] auto ReturnsValue() const -> bool {
    return return_kind == ReturnKind::kValue;
  }

  [[nodiscard]] auto IsEffectOnly() const -> bool {
    return return_kind == ReturnKind::kVoid && has_side_effect;
  }
};

// Classify a CallExpression as a builtin method call on a dynamic array or
// queue. Returns nullopt if this is not a recognized builtin method call.
auto ClassifyBuiltinMethod(const slang::ast::CallExpression& call)
    -> std::optional<BuiltinMethodInfo>;

// Lower a builtin method call to HIR.
// Precondition: ClassifyBuiltinMethod returned a valid classification.
auto LowerBuiltinMethodCall(
    const slang::ast::CallExpression& call, const BuiltinMethodInfo& info,
    ExpressionLoweringView view) -> hir::ExpressionId;

}  // namespace lyra::lowering::ast_to_hir
