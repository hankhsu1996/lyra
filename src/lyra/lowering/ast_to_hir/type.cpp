#include "lyra/lowering/ast_to_hir/type.hpp"

#include <format>

#include <slang/ast/types/AllTypes.h>

#include "lyra/common/diagnostic/diagnostic_sink.hpp"
#include "lyra/common/type_arena.hpp"
#include "lyra/lowering/ast_to_hir/context.hpp"

namespace lyra::lowering::ast_to_hir {

auto LowerType(const slang::ast::Type& type, SourceSpan source, Context* ctx)
    -> TypeId {
  if (type.isError()) {
    return kInvalidTypeId;
  }

  if (type.isVoid()) {
    return ctx->type_arena->Intern(TypeKind::kVoid, std::monostate{});
  }

  if (type.isIntegral()) {
    return ctx->type_arena->Intern(
        TypeKind::kIntegral, IntegralInfo{
                                 .bit_width = type.getBitWidth(),
                                 .is_signed = type.isSigned(),
                                 .is_four_state = type.isFourState()});
  }

  if (type.isString()) {
    return ctx->type_arena->Intern(TypeKind::kString, std::monostate{});
  }

  if (type.isFloating()) {
    const auto& ft = type.as<slang::ast::FloatingType>();
    if (ft.floatKind != slang::ast::FloatingType::Real) {
      ctx->sink->Error(
          source, std::format(
                      "unsupported floating type '{}' (only 'real' supported)",
                      type.toString()));
      return kInvalidTypeId;
    }
    return ctx->type_arena->Intern(TypeKind::kReal, std::monostate{});
  }

  if (type.isUnpackedArray()) {
    if (type.kind != slang::ast::SymbolKind::FixedSizeUnpackedArrayType) {
      ctx->sink->Error(source, "only fixed-size unpacked arrays supported");
      return kInvalidTypeId;
    }
    const auto& array = type.as<slang::ast::FixedSizeUnpackedArrayType>();
    TypeId element = LowerType(array.elementType, source, ctx);
    return ctx->type_arena->Intern(
        TypeKind::kUnpackedArray,
        UnpackedArrayInfo{
            .element_type = element,
            .range = ConstantRange{
                .lower = array.range.lower(), .upper = array.range.upper()}});
  }

  ctx->sink->Error(
      source, std::format("unsupported type '{}'", type.toString()));
  return kInvalidTypeId;
}

}  // namespace lyra::lowering::ast_to_hir
