#include "lyra/lowering/ast_to_hir/builtin_method.hpp"

#include <slang/ast/types/AllTypes.h>

namespace lyra::lowering {

auto ClassifyBuiltinMethod(const slang::ast::CallExpression& call)
    -> std::optional<BuiltinMethodInfo> {
  // Builtin methods must NOT be user-defined subroutines.
  // IMPORTANT: Must check this before isSystemCall() - slang's isSystemCall()
  // returns true for built-in array methods like size().
  const auto* user_sub =
      std::get_if<const slang::ast::SubroutineSymbol*>(&call.subroutine);
  if (user_sub != nullptr) {
    return std::nullopt;
  }

  // Builtin methods have at least one argument (the receiver).
  if (call.arguments().empty()) {
    return std::nullopt;
  }

  const auto* first_arg = call.arguments()[0];
  if (first_arg->type == nullptr) {
    return std::nullopt;
  }

  // Determine container kind from receiver type.
  std::optional<ContainerKind> container;
  const auto& receiver_type = first_arg->type->getCanonicalType();
  if (receiver_type.kind == slang::ast::SymbolKind::DynamicArrayType) {
    container = ContainerKind::kDynamicArray;
  } else if (receiver_type.kind == slang::ast::SymbolKind::QueueType) {
    container = ContainerKind::kQueue;
  } else if (receiver_type.isEnum()) {
    container = ContainerKind::kEnum;
  }

  if (!container) {
    return std::nullopt;
  }

  // Match method name to kind.
  std::string_view name = call.getSubroutineName();

  // Methods available on both dynamic arrays and queues.
  if (name == "size") {
    return BuiltinMethodInfo{
        .container = *container,
        .method = BuiltinMethodKind::kSize,
        .return_kind = ReturnKind::kValue,
        .has_side_effect = false};
  }
  if (name == "delete") {
    return BuiltinMethodInfo{
        .container = *container,
        .method = BuiltinMethodKind::kDelete,
        .return_kind = ReturnKind::kVoid,
        .has_side_effect = true};
  }

  // Queue-only methods.
  if (*container == ContainerKind::kQueue) {
    if (name == "push_back") {
      return BuiltinMethodInfo{
          .container = *container,
          .method = BuiltinMethodKind::kPushBack,
          .return_kind = ReturnKind::kVoid,
          .has_side_effect = true};
    }
    if (name == "push_front") {
      return BuiltinMethodInfo{
          .container = *container,
          .method = BuiltinMethodKind::kPushFront,
          .return_kind = ReturnKind::kVoid,
          .has_side_effect = true};
    }
    if (name == "pop_back") {
      return BuiltinMethodInfo{
          .container = *container,
          .method = BuiltinMethodKind::kPopBack,
          .return_kind = ReturnKind::kValue,
          .has_side_effect = true};
    }
    if (name == "pop_front") {
      return BuiltinMethodInfo{
          .container = *container,
          .method = BuiltinMethodKind::kPopFront,
          .return_kind = ReturnKind::kValue,
          .has_side_effect = true};
    }
    if (name == "insert") {
      return BuiltinMethodInfo{
          .container = *container,
          .method = BuiltinMethodKind::kInsert,
          .return_kind = ReturnKind::kVoid,
          .has_side_effect = true};
    }
  }

  // Enum-only methods.
  if (*container == ContainerKind::kEnum) {
    if (name == "first") {
      return BuiltinMethodInfo{
          .container = *container,
          .method = BuiltinMethodKind::kEnumFirst,
          .return_kind = ReturnKind::kValue,
          .has_side_effect = false};
    }
    if (name == "last") {
      return BuiltinMethodInfo{
          .container = *container,
          .method = BuiltinMethodKind::kEnumLast,
          .return_kind = ReturnKind::kValue,
          .has_side_effect = false};
    }
    if (name == "num") {
      return BuiltinMethodInfo{
          .container = *container,
          .method = BuiltinMethodKind::kEnumNum,
          .return_kind = ReturnKind::kValue,
          .has_side_effect = false};
    }
    if (name == "next") {
      return BuiltinMethodInfo{
          .container = *container,
          .method = BuiltinMethodKind::kEnumNext,
          .return_kind = ReturnKind::kValue,
          .has_side_effect = false};
    }
    if (name == "prev") {
      return BuiltinMethodInfo{
          .container = *container,
          .method = BuiltinMethodKind::kEnumPrev,
          .return_kind = ReturnKind::kValue,
          .has_side_effect = false};
    }
    if (name == "name") {
      return BuiltinMethodInfo{
          .container = *container,
          .method = BuiltinMethodKind::kEnumName,
          .return_kind = ReturnKind::kValue,
          .has_side_effect = false};
    }
  }

  return std::nullopt;
}

}  // namespace lyra::lowering
