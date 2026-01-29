#include "lyra/lowering/ast_to_hir/builtin_method.hpp"

#include <cstdint>
#include <format>
#include <optional>
#include <string_view>
#include <utility>
#include <variant>
#include <vector>

#include <slang/ast/types/AllTypes.h>

#include "lyra/common/constant.hpp"
#include "lyra/common/constant_arena.hpp"
#include "lyra/common/diagnostic/diagnostic_sink.hpp"
#include "lyra/common/integral_constant.hpp"
#include "lyra/common/internal_error.hpp"
#include "lyra/common/source_span.hpp"
#include "lyra/common/type.hpp"
#include "lyra/hir/arena.hpp"
#include "lyra/hir/expression.hpp"
#include "lyra/hir/fwd.hpp"
#include "lyra/lowering/ast_to_hir/context.hpp"
#include "lyra/lowering/ast_to_hir/detail/expression_lowering.hpp"
#include "lyra/lowering/ast_to_hir/type.hpp"

namespace lyra::lowering::ast_to_hir {

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

namespace {

auto MakeConstant(uint64_t value, TypeId type, SourceSpan span, Context* ctx)
    -> hir::ExpressionId {
  IntegralConstant constant;
  constant.value.push_back(value);
  constant.unknown.push_back(0);
  ConstId cid = ctx->constant_arena->Intern(type, std::move(constant));
  return ctx->hir_arena->AddExpression(
      hir::Expression{
          .kind = hir::ExpressionKind::kConstant,
          .type = type,
          .span = span,
          .data = hir::ConstantExpressionData{.constant = cid}});
}

}  // namespace

auto LowerBuiltinMethodCall(
    const slang::ast::CallExpression& call, const BuiltinMethodInfo& info,
    ExpressionLoweringView view) -> hir::ExpressionId {
  auto* ctx = view.context;
  SourceSpan span = ctx->SpanOf(call.sourceRange);

  // Lower the receiver (first argument).
  const auto* first_arg = call.arguments()[0];
  hir::ExpressionId receiver = LowerExpression(*first_arg, view);
  if (!receiver) {
    return hir::kInvalidExpressionId;
  }

  // Determine HIR method and result type based on classification.
  hir::BuiltinMethod hir_method{};
  TypeId result_type{};
  std::vector<hir::ExpressionId> args;

  switch (info.method) {
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
      hir_method = (info.method == BuiltinMethodKind::kPopBack)
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
      result_type = ctx->type_arena->Intern(TypeKind::kVoid, std::monostate{});
      // delete() or delete(idx): 0 or 1 arg after receiver
      if (call.arguments().size() > 2) {
        ctx->sink->Error(span, "delete() takes at most one argument");
        return hir::kInvalidExpressionId;
      }
      if (call.arguments().size() == 2) {
        hir::ExpressionId idx = LowerExpression(*call.arguments()[1], view);
        if (!idx) {
          return hir::kInvalidExpressionId;
        }
        args.push_back(idx);
      }
      break;
    }
    case BuiltinMethodKind::kPushBack:
    case BuiltinMethodKind::kPushFront: {
      hir_method = (info.method == BuiltinMethodKind::kPushBack)
                       ? hir::BuiltinMethod::kPushBack
                       : hir::BuiltinMethod::kPushFront;
      result_type = ctx->type_arena->Intern(TypeKind::kVoid, std::monostate{});
      // push_back(val) / push_front(val): exactly 1 arg after receiver
      if (call.arguments().size() != 2) {
        ctx->sink->Error(
            span, std::format(
                      "{}() requires exactly one argument",
                      call.getSubroutineName()));
        return hir::kInvalidExpressionId;
      }
      hir::ExpressionId value = LowerExpression(*call.arguments()[1], view);
      if (!value) {
        return hir::kInvalidExpressionId;
      }
      args.push_back(value);
      break;
    }
    case BuiltinMethodKind::kInsert: {
      hir_method = hir::BuiltinMethod::kInsert;
      result_type = ctx->type_arena->Intern(TypeKind::kVoid, std::monostate{});
      // insert(idx, val): exactly 2 args after receiver
      if (call.arguments().size() != 3) {
        ctx->sink->Error(
            span, "insert() requires exactly two arguments (index and value)");
        return hir::kInvalidExpressionId;
      }
      hir::ExpressionId idx = LowerExpression(*call.arguments()[1], view);
      if (!idx) {
        return hir::kInvalidExpressionId;
      }
      hir::ExpressionId value = LowerExpression(*call.arguments()[2], view);
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
            "LowerBuiltinMethodCall", "enum method on non-enum type");
      }
      const auto& enum_info = enum_type.AsEnum();

      // Defensive check: SV doesn't allow empty enums, but be safe
      if (enum_info.members.empty()) {
        throw common::InternalError(
            "LowerBuiltinMethodCall", "enum has no members");
      }

      if (info.method == BuiltinMethodKind::kEnumNum) {
        // num() returns int (32-bit signed 2-state)
        TypeId int_type = ctx->type_arena->Intern(
            TypeKind::kIntegral,
            IntegralInfo{
                .bit_width = 32, .is_signed = true, .is_four_state = false});
        auto num_val = static_cast<uint64_t>(enum_info.members.size());
        return MakeConstant(num_val, int_type, span, ctx);
      }

      // first() and last() return the enum type
      const IntegralConstant& member_val =
          (info.method == BuiltinMethodKind::kEnumFirst)
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

      hir_method = (info.method == BuiltinMethodKind::kEnumNext)
                       ? hir::BuiltinMethod::kEnumNext
                       : hir::BuiltinMethod::kEnumPrev;
      // Get enum type from receiver for result type
      const hir::Expression& receiver_expr = (*ctx->hir_arena)[receiver];
      result_type = receiver_expr.type;

      // Optional step argument: next(N) or prev(N)
      if (call.arguments().size() > 1) {
        hir::ExpressionId step = LowerExpression(*call.arguments()[1], view);
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

}  // namespace lyra::lowering::ast_to_hir
