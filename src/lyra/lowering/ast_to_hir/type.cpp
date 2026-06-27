#include "lyra/hir/type.hpp"

#include <cstdint>
#include <optional>
#include <span>
#include <string>
#include <utility>
#include <vector>

#include <slang/ast/EvalContext.h>
#include <slang/ast/Expression.h>
#include <slang/ast/Symbol.h>
#include <slang/ast/symbols/VariableSymbols.h>
#include <slang/ast/types/AllTypes.h>
#include <slang/ast/types/Type.h>
#include <slang/numeric/ConstantValue.h>
#include <slang/numeric/SVInt.h>

#include "lyra/base/internal_error.hpp"
#include "lyra/diag/diagnostic.hpp"
#include "lyra/diag/source_span.hpp"
#include "lyra/hir/integral_constant.hpp"
#include "lyra/lowering/ast_to_hir/constant_value.hpp"
#include "lyra/lowering/ast_to_hir/integral_constant.hpp"
#include "lyra/lowering/ast_to_hir/module_lowerer.hpp"

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
  throw InternalError("LowerPredefinedInteger: unknown integer kind");
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
    return diag::Fail(
        decl_span, diag::DiagCode::kUnsupportedPackedArrayElementType,
        "packed array element must be a bit, logic, or reg scalar");
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

auto LowerPackedStruct(
    const slang::ast::PackedStructType& struct_type, diag::SourceSpan decl_span,
    ModuleLowerer& module) -> diag::Result<hir::PackedStructType> {
  const auto width = static_cast<std::int64_t>(struct_type.bitWidth);
  if (width <= 0) {
    throw InternalError("LowerPackedStruct: zero-width packed struct");
  }
  const auto atom =
      struct_type.isFourState ? hir::BitAtom::kLogic : hir::BitAtom::kBit;
  const auto signedness = struct_type.isSigned ? hir::Signedness::kSigned
                                               : hir::Signedness::kUnsigned;
  hir::PackedArrayType base{
      .atom = atom,
      .signedness = signedness,
      .dims = {hir::PackedRange{.left = width - 1, .right = 0}},
      .form = hir::PackedArrayForm::kExplicit,
  };
  std::vector<hir::PackedAggregateField> fields;
  for (const auto& field :
       struct_type.membersOfType<slang::ast::FieldSymbol>()) {
    auto field_type_or = module.InternType(field.getType(), decl_span);
    if (!field_type_or) {
      return std::unexpected(std::move(field_type_or.error()));
    }
    const auto field_width =
        static_cast<std::uint64_t>(field.getType().getBitWidth());
    fields.push_back(
        hir::PackedAggregateField{
            .name = std::string(field.name),
            .type = *field_type_or,
            .bit_offset = field.bitOffset,
            .bit_width = field_width,
        });
  }
  return hir::PackedStructType{
      .base = std::move(base),
      .fields = std::move(fields),
  };
}

auto LowerPackedUnion(
    const slang::ast::PackedUnionType& union_type, diag::SourceSpan decl_span,
    ModuleLowerer& module) -> diag::Result<hir::PackedUnionType> {
  if (union_type.isTagged) {
    return diag::Fail(
        decl_span, diag::DiagCode::kUnsupportedTaggedPackedUnion,
        "tagged packed unions are not yet supported");
  }
  const auto width = static_cast<std::int64_t>(union_type.bitWidth);
  if (width <= 0) {
    throw InternalError("LowerPackedUnion: zero-width packed union");
  }
  const auto atom =
      union_type.isFourState ? hir::BitAtom::kLogic : hir::BitAtom::kBit;
  const auto signedness = union_type.isSigned ? hir::Signedness::kSigned
                                              : hir::Signedness::kUnsigned;
  hir::PackedArrayType base{
      .atom = atom,
      .signedness = signedness,
      .dims = {hir::PackedRange{.left = width - 1, .right = 0}},
      .form = hir::PackedArrayForm::kExplicit,
  };
  std::vector<hir::PackedAggregateField> fields;
  for (const auto& field :
       union_type.membersOfType<slang::ast::FieldSymbol>()) {
    auto field_type_or = module.InternType(field.getType(), decl_span);
    if (!field_type_or) {
      return std::unexpected(std::move(field_type_or.error()));
    }
    const auto field_width =
        static_cast<std::uint64_t>(field.getType().getBitWidth());
    fields.push_back(
        hir::PackedAggregateField{
            .name = std::string(field.name),
            .type = *field_type_or,
            .bit_offset = 0U,
            .bit_width = field_width,
        });
  }
  return hir::PackedUnionType{
      .base = std::move(base),
      .fields = std::move(fields),
  };
}

// LRM 7.2.2: a member declaration may carry a constant default value, used in
// place of the member type's Table 7-1 default when the enclosing struct is
// default-constructed. Slang has bound and constant-checked it; evaluate it and
// fold it into the member's value-form metadata, the same shape an enum member
// carries its value -- the default is a value the type holds, not an
// expression.
auto LowerMemberDefault(
    const slang::ast::FieldSymbol& field, diag::SourceSpan span)
    -> diag::Result<std::optional<hir::ConstantValue>> {
  const auto* init = field.getInitializer();
  if (init == nullptr) {
    return std::optional<hir::ConstantValue>{};
  }
  slang::ast::EvalContext eval_context(field);
  const slang::ConstantValue constant = init->eval(eval_context);
  auto value_or = MakeConstantValue(constant, span);
  if (!value_or) {
    return std::unexpected(std::move(value_or.error()));
  }
  return std::optional<hir::ConstantValue>{*std::move(value_or)};
}

auto LowerUnpackedAggregateFields(
    std::span<const slang::ast::FieldSymbol* const> fields,
    diag::SourceSpan decl_span, ModuleLowerer& module)
    -> diag::Result<std::vector<hir::UnpackedAggregateField>> {
  std::vector<hir::UnpackedAggregateField> out;
  out.reserve(fields.size());
  for (const auto* field : fields) {
    auto field_type_or = module.InternType(field->getType(), decl_span);
    if (!field_type_or) {
      return std::unexpected(std::move(field_type_or.error()));
    }
    auto default_or = LowerMemberDefault(*field, decl_span);
    if (!default_or) {
      return std::unexpected(std::move(default_or.error()));
    }
    out.push_back(
        hir::UnpackedAggregateField{
            .name = std::string(field->name),
            .type = *field_type_or,
            .default_init = *std::move(default_or),
        });
  }
  return out;
}

auto LowerUnpackedStruct(
    const slang::ast::UnpackedStructType& struct_type,
    diag::SourceSpan decl_span, ModuleLowerer& module)
    -> diag::Result<hir::UnpackedStructType> {
  auto fields_or =
      LowerUnpackedAggregateFields(struct_type.fields, decl_span, module);
  if (!fields_or) return std::unexpected(std::move(fields_or.error()));
  return hir::UnpackedStructType{.fields = *std::move(fields_or)};
}

auto LowerUnpackedUnion(
    const slang::ast::UnpackedUnionType& union_type, diag::SourceSpan decl_span,
    ModuleLowerer& module) -> diag::Result<hir::UnpackedUnionType> {
  auto fields_or =
      LowerUnpackedAggregateFields(union_type.fields, decl_span, module);
  if (!fields_or) return std::unexpected(std::move(fields_or.error()));
  return hir::UnpackedUnionType{
      .fields = *std::move(fields_or),
      .tagged = union_type.isTagged,
  };
}

auto LowerEnum(
    const slang::ast::EnumType& enum_type, diag::SourceSpan decl_span,
    ModuleLowerer& module) -> diag::Result<hir::EnumType> {
  auto base_id_or = module.InternType(enum_type.baseType, decl_span);
  if (!base_id_or) return std::unexpected(std::move(base_id_or.error()));
  std::vector<hir::EnumMember> members;
  for (const auto& value_sym : enum_type.values()) {
    const auto& cv = value_sym.getValue();
    if (!cv.isInteger()) {
      throw InternalError("LowerEnum: enum value is not integral");
    }
    members.push_back(
        hir::EnumMember{
            .name = std::string(value_sym.name),
            .value = LowerSVIntToIntegralConstant(cv.integer()),
        });
  }
  return hir::EnumType{
      .base_type = *base_id_or,
      .members = std::move(members),
  };
}

auto TranslateTypeData(
    ModuleLowerer& module, const slang::ast::Type& type,
    diag::SourceSpan decl_span) -> diag::Result<hir::TypeData> {
  const auto& canonical = type.getCanonicalType();

  switch (canonical.kind) {
    case slang::ast::SymbolKind::ScalarType: {
      const auto& scalar = canonical.as<slang::ast::ScalarType>();
      return hir::TypeData{hir::PackedArrayType{
          .atom = LowerScalarAtom(scalar.scalarKind),
          .signedness = hir::Signedness::kUnsigned,
          .dims = {hir::PackedRange{.left = 0, .right = 0}},
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
    case slang::ast::SymbolKind::EnumType: {
      auto e =
          LowerEnum(canonical.as<slang::ast::EnumType>(), decl_span, module);
      if (!e.has_value()) {
        return std::unexpected(std::move(e.error()));
      }
      return hir::TypeData{*std::move(e)};
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
      throw InternalError("TranslateTypeData: unknown FloatingType kind");
    }
    case slang::ast::SymbolKind::StringType:
      return hir::TypeData{hir::StringType{}};
    case slang::ast::SymbolKind::EventType:
      return hir::TypeData{hir::EventType{}};
    case slang::ast::SymbolKind::CHandleType:
      return hir::TypeData{hir::ChandleType{}};
    case slang::ast::SymbolKind::VoidType:
      return hir::TypeData{hir::VoidType{}};
    case slang::ast::SymbolKind::PackedStructType: {
      auto s = LowerPackedStruct(
          canonical.as<slang::ast::PackedStructType>(), decl_span, module);
      if (!s.has_value()) {
        return std::unexpected(std::move(s.error()));
      }
      return hir::TypeData{*std::move(s)};
    }
    case slang::ast::SymbolKind::PackedUnionType: {
      auto u = LowerPackedUnion(
          canonical.as<slang::ast::PackedUnionType>(), decl_span, module);
      if (!u.has_value()) {
        return std::unexpected(std::move(u.error()));
      }
      return hir::TypeData{*std::move(u)};
    }
    case slang::ast::SymbolKind::FixedSizeUnpackedArrayType: {
      const auto& fa = canonical.as<slang::ast::FixedSizeUnpackedArrayType>();
      auto elem_id_or = module.InternType(fa.elementType, decl_span);
      if (!elem_id_or) {
        return std::unexpected(std::move(elem_id_or.error()));
      }
      return hir::TypeData{hir::UnpackedArrayType{
          .element_type = *elem_id_or,
          .dim =
              hir::UnpackedRange{
                  .left = static_cast<std::int64_t>(fa.range.left),
                  .right = static_cast<std::int64_t>(fa.range.right)},
      }};
    }
    case slang::ast::SymbolKind::DynamicArrayType: {
      const auto& da = canonical.as<slang::ast::DynamicArrayType>();
      auto elem_id_or = module.InternType(da.elementType, decl_span);
      if (!elem_id_or) {
        return std::unexpected(std::move(elem_id_or.error()));
      }
      return hir::TypeData{hir::DynamicArrayType{.element_type = *elem_id_or}};
    }
    case slang::ast::SymbolKind::QueueType: {
      const auto& q = canonical.as<slang::ast::QueueType>();
      auto elem_id_or = module.InternType(q.elementType, decl_span);
      if (!elem_id_or) {
        return std::unexpected(std::move(elem_id_or.error()));
      }
      // slang encodes the unbounded queue (`[$]`) as maxBound == 0; a bounded
      // queue (`[$:N]`) carries the bound (LRM 7.10.4).
      return hir::TypeData{hir::QueueType{
          .element_type = *elem_id_or,
          .max_bound = q.maxBound == 0
                           ? std::nullopt
                           : std::optional<std::uint64_t>{q.maxBound},
      }};
    }
    case slang::ast::SymbolKind::AssociativeArrayType: {
      const auto& aa = canonical.as<slang::ast::AssociativeArrayType>();
      auto elem_id_or = module.InternType(aa.elementType, decl_span);
      if (!elem_id_or) {
        return std::unexpected(std::move(elem_id_or.error()));
      }
      // LRM 7.8.1 wildcard index (`[*]`) declares no index type: the key is any
      // integral value identified by its magnitude, carried by the dedicated
      // wildcard-index key type.
      if (aa.indexType == nullptr) {
        return hir::TypeData{hir::AssociativeArrayType{
            .element_type = *elem_id_or,
            .key_type = module.Unit().builtins.wildcard_index,
        }};
      }
      // A declared index covers string (LRM 7.8.2) and integral, which includes
      // a packed struct / enum (LRM 7.8.4 / 7.8.5). A class index (LRM 7.8.3)
      // and a real index are rejected.
      if (!(aa.indexType->isIntegral() || aa.indexType->isString())) {
        return diag::Fail(
            decl_span, diag::DiagCode::kUnsupportedAssociativeArrayType,
            "associative arrays are only supported with a string, integral, or "
            "wildcard index type");
      }
      auto key_id_or = module.InternType(*aa.indexType, decl_span);
      if (!key_id_or) {
        return std::unexpected(std::move(key_id_or.error()));
      }
      return hir::TypeData{hir::AssociativeArrayType{
          .element_type = *elem_id_or,
          .key_type = *key_id_or,
      }};
    }
    case slang::ast::SymbolKind::UnpackedStructType: {
      auto s = LowerUnpackedStruct(
          canonical.as<slang::ast::UnpackedStructType>(), decl_span, module);
      if (!s) return std::unexpected(std::move(s.error()));
      return hir::TypeData{*std::move(s)};
    }
    case slang::ast::SymbolKind::UnpackedUnionType: {
      auto u = LowerUnpackedUnion(
          canonical.as<slang::ast::UnpackedUnionType>(), decl_span, module);
      if (!u) return std::unexpected(std::move(u.error()));
      return hir::TypeData{*std::move(u)};
    }
    default:
      return diag::Fail(
          decl_span, diag::DiagCode::kUnsupportedTypeKind,
          "unsupported type kind");
  }
}

}  // namespace

auto ModuleLowerer::InternType(
    const slang::ast::Type& type, diag::SourceSpan span)
    -> diag::Result<hir::TypeId> {
  const auto* canonical = &type.getCanonicalType();
  if (const auto it = type_cache_.find(canonical); it != type_cache_.end()) {
    return it->second;
  }
  auto data_or = TranslateTypeData(*this, type, span);
  if (!data_or) return std::unexpected(std::move(data_or.error()));
  const hir::TypeId id =
      unit_.types.Add(hir::Type{.data = *std::move(data_or), .span = span});
  type_cache_.emplace(canonical, id);
  return id;
}

}  // namespace lyra::lowering::ast_to_hir
