#include "type.hpp"

#include <cstdint>

#include <slang/ast/Symbol.h>
#include <slang/ast/types/AllTypes.h>
#include <slang/ast/types/Type.h>
#include <slang/numeric/ConstantValue.h>

#include "lyra/base/internal_error.hpp"
#include "lyra/diag/diagnostic.hpp"
#include "lyra/diag/source_span.hpp"
#include "lyra/hir/type.hpp"

namespace lyra::lowering::ast_to_hir {

namespace {

auto LowerScalarAtom(slang::ast::ScalarType::Kind k) -> hir::BitAtom {
  switch (k) {
    case slang::ast::ScalarType::Bit:
      return hir::BitAtom::kBit;
    case slang::ast::ScalarType::Logic:
      return hir::BitAtom::kLogic;
    case slang::ast::ScalarType::Reg:
      return hir::BitAtom::kReg;
  }
  throw InternalError("LowerScalarAtom: unknown scalar kind");
}

auto LowerRange(const slang::ConstantRange& r) -> hir::PackedRange {
  return hir::PackedRange{
      .left = static_cast<std::int64_t>(r.left),
      .right = static_cast<std::int64_t>(r.right),
  };
}

auto LowerPredefinedInteger(slang::ast::PredefinedIntegerType::Kind k)
    -> hir::PackedArrayType {
  using SK = slang::ast::PredefinedIntegerType::Kind;
  switch (k) {
    case SK::Byte:
      return hir::PackedArrayType{
          .atom = hir::BitAtom::kBit,
          .signedness = hir::Signedness::kSigned,
          .dims = {hir::PackedRange{.left = 7, .right = 0}},
          .form = hir::PackedArrayForm::kByte,
      };
    case SK::ShortInt:
      return hir::PackedArrayType{
          .atom = hir::BitAtom::kBit,
          .signedness = hir::Signedness::kSigned,
          .dims = {hir::PackedRange{.left = 15, .right = 0}},
          .form = hir::PackedArrayForm::kShortInt,
      };
    case SK::Int:
      return hir::PackedArrayType{
          .atom = hir::BitAtom::kBit,
          .signedness = hir::Signedness::kSigned,
          .dims = {hir::PackedRange{.left = 31, .right = 0}},
          .form = hir::PackedArrayForm::kInt,
      };
    case SK::LongInt:
      return hir::PackedArrayType{
          .atom = hir::BitAtom::kBit,
          .signedness = hir::Signedness::kSigned,
          .dims = {hir::PackedRange{.left = 63, .right = 0}},
          .form = hir::PackedArrayForm::kLongInt,
      };
    case SK::Integer:
      return hir::PackedArrayType{
          .atom = hir::BitAtom::kLogic,
          .signedness = hir::Signedness::kSigned,
          .dims = {hir::PackedRange{.left = 31, .right = 0}},
          .form = hir::PackedArrayForm::kInteger,
      };
    case SK::Time:
      return hir::PackedArrayType{
          .atom = hir::BitAtom::kLogic,
          .signedness = hir::Signedness::kUnsigned,
          .dims = {hir::PackedRange{.left = 63, .right = 0}},
          .form = hir::PackedArrayForm::kTime,
      };
  }
  throw InternalError(
      "LowerPredefinedInteger: unknown predefined integer kind");
}

auto LowerExplicitPackedArray(
    const slang::ast::PackedArrayType& outer, bool outer_signed,
    diag::SourceSpan decl_span) -> diag::Result<hir::PackedArrayType> {
  std::vector<hir::PackedRange> dims;
  const slang::ast::Type* cur = &outer;
  while (cur->kind == slang::ast::SymbolKind::PackedArrayType) {
    const auto& pa = cur->as<slang::ast::PackedArrayType>();
    dims.push_back(LowerRange(pa.range));
    cur = &pa.elementType.getCanonicalType();
  }
  if (cur->kind != slang::ast::SymbolKind::ScalarType) {
    return diag::Unsupported(
        decl_span, diag::DiagCode::kUnsupportedPackedArrayElementType,
        "packed array element must be a bit, logic, or reg scalar",
        diag::UnsupportedCategory::kType);
  }
  const auto& scalar = cur->as<slang::ast::ScalarType>();
  return hir::PackedArrayType{
      .atom = LowerScalarAtom(scalar.scalarKind),
      .signedness =
          outer_signed ? hir::Signedness::kSigned : hir::Signedness::kUnsigned,
      .dims = std::move(dims),
      .form = hir::PackedArrayForm::kExplicit,
  };
}

}  // namespace

auto LowerTypeData(const slang::ast::Type& type, diag::SourceSpan decl_span)
    -> diag::Result<hir::TypeData> {
  const auto& canonical = type.getCanonicalType();

  switch (canonical.kind) {
    case slang::ast::SymbolKind::ScalarType: {
      const auto& scalar = canonical.as<slang::ast::ScalarType>();
      return hir::TypeData{hir::PackedArrayType{
          .atom = LowerScalarAtom(scalar.scalarKind),
          .signedness = hir::Signedness::kUnsigned,
          .dims = {},
          .form = hir::PackedArrayForm::kExplicit,
      }};
    }
    case slang::ast::SymbolKind::PredefinedIntegerType: {
      const auto& pi = canonical.as<slang::ast::PredefinedIntegerType>();
      return hir::TypeData{LowerPredefinedInteger(pi.integerKind)};
    }
    case slang::ast::SymbolKind::PackedArrayType: {
      auto pa = LowerExplicitPackedArray(
          canonical.as<slang::ast::PackedArrayType>(), canonical.isSigned(),
          decl_span);
      if (!pa.has_value()) {
        return std::unexpected(std::move(pa.error()));
      }
      return hir::TypeData{std::move(pa.value())};
    }
    case slang::ast::SymbolKind::FloatingType: {
      const auto& f = canonical.as<slang::ast::FloatingType>();
      switch (f.floatKind) {
        case slang::ast::FloatingType::Real:
          return hir::TypeData{hir::RealType{}};
        case slang::ast::FloatingType::ShortReal:
          return hir::TypeData{hir::ShortRealType{}};
        case slang::ast::FloatingType::RealTime:
          return hir::TypeData{hir::RealTimeType{}};
      }
      throw InternalError("LowerTypeData: unknown FloatingType kind");
    }
    case slang::ast::SymbolKind::StringType:
      return hir::TypeData{hir::StringType{}};
    case slang::ast::SymbolKind::EventType:
      return hir::TypeData{hir::EventType{}};
    case slang::ast::SymbolKind::CHandleType:
      return hir::TypeData{hir::ChandleType{}};
    case slang::ast::SymbolKind::VoidType:
      return hir::TypeData{hir::VoidType{}};
    case slang::ast::SymbolKind::PackedStructType:
      return diag::Unsupported(
          decl_span, diag::DiagCode::kUnsupportedPackedStructType,
          "packed struct types are not supported",
          diag::UnsupportedCategory::kType);
    case slang::ast::SymbolKind::PackedUnionType:
      return diag::Unsupported(
          decl_span, diag::DiagCode::kUnsupportedPackedUnionType,
          "packed union types are not supported",
          diag::UnsupportedCategory::kType);
    case slang::ast::SymbolKind::EnumType:
      return diag::Unsupported(
          decl_span, diag::DiagCode::kUnsupportedEnumType,
          "enum types are not supported", diag::UnsupportedCategory::kType);
    case slang::ast::SymbolKind::FixedSizeUnpackedArrayType:
      return diag::Unsupported(
          decl_span, diag::DiagCode::kUnsupportedFixedSizeUnpackedArrayType,
          "unpacked array types are not supported",
          diag::UnsupportedCategory::kType);
    case slang::ast::SymbolKind::DynamicArrayType:
      return diag::Unsupported(
          decl_span, diag::DiagCode::kUnsupportedDynamicArrayType,
          "dynamic array types are not supported",
          diag::UnsupportedCategory::kType);
    case slang::ast::SymbolKind::QueueType:
      return diag::Unsupported(
          decl_span, diag::DiagCode::kUnsupportedQueueType,
          "queue types are not supported", diag::UnsupportedCategory::kType);
    case slang::ast::SymbolKind::AssociativeArrayType:
      return diag::Unsupported(
          decl_span, diag::DiagCode::kUnsupportedAssociativeArrayType,
          "associative array types are not supported",
          diag::UnsupportedCategory::kType);
    case slang::ast::SymbolKind::UnpackedStructType:
      return diag::Unsupported(
          decl_span, diag::DiagCode::kUnsupportedUnpackedStructType,
          "unpacked struct types are not supported",
          diag::UnsupportedCategory::kType);
    case slang::ast::SymbolKind::UnpackedUnionType:
      return diag::Unsupported(
          decl_span, diag::DiagCode::kUnsupportedUnpackedUnionType,
          "unpacked union types are not supported",
          diag::UnsupportedCategory::kType);
    default:
      return diag::Unsupported(
          decl_span, diag::DiagCode::kUnsupportedTypeKind,
          "unsupported type kind", diag::UnsupportedCategory::kType);
  }
}

}  // namespace lyra::lowering::ast_to_hir
