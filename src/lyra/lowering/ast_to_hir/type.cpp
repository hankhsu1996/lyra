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
#include <slang/ast/symbols/ClassSymbols.h>
#include <slang/ast/symbols/SubroutineSymbols.h>
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
#include "lyra/lowering/ast_to_hir/subroutine_decl.hpp"

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

// A predefined-width integer is a single-dimension packed array over a scalar
// bit (LRM 7.4.1); `form` records the syntactic origin (`int` vs the equivalent
// `bit signed [31:0]`).
auto LowerPredefinedInteger(
    const ModuleLowerer& module, slang::ast::PredefinedIntegerType::Kind k)
    -> hir::PackedArrayType {
  using SK = slang::ast::PredefinedIntegerType::Kind;
  const auto& builtins = module.Unit().builtins;
  const auto make = [&](hir::TypeId element, std::int64_t msb,
                        hir::Signedness signedness,
                        hir::PackedArrayForm form) -> hir::PackedArrayType {
    return hir::PackedArrayType{
        .dim = hir::PackedRange{.left = msb, .right = 0},
        .element_type = element,
        .signedness = signedness,
        .form = form,
    };
  };
  switch (k) {
    case SK::Byte:
      return make(
          builtins.scalar_bit, 7, hir::Signedness::kSigned,
          hir::PackedArrayForm::kByte);
    case SK::ShortInt:
      return make(
          builtins.scalar_bit, 15, hir::Signedness::kSigned,
          hir::PackedArrayForm::kShortInt);
    case SK::Int:
      return make(
          builtins.scalar_bit, 31, hir::Signedness::kSigned,
          hir::PackedArrayForm::kInt);
    case SK::LongInt:
      return make(
          builtins.scalar_bit, 63, hir::Signedness::kSigned,
          hir::PackedArrayForm::kLongInt);
    case SK::Integer:
      return make(
          builtins.scalar_logic, 31, hir::Signedness::kSigned,
          hir::PackedArrayForm::kInteger);
    case SK::Time:
      return make(
          builtins.scalar_logic, 63, hir::Signedness::kUnsigned,
          hir::PackedArrayForm::kTime);
  }
  throw InternalError("LowerPredefinedInteger: unknown integer kind");
}

// One HIR node per declared dimension (LRM 7.4.1: a packed array is recursively
// other packed arrays / structures). The element is interned and named by its
// `TypeId`, so the nest -- and an aggregate element's identity -- is carried by
// recursion, not flattened here.
auto LowerExplicitPackedArray(
    ModuleLowerer& module, const slang::ast::PackedArrayType& array,
    bool outer_signed, diag::SourceSpan span)
    -> diag::Result<hir::PackedArrayType> {
  auto element = module.InternType(array.elementType, span);
  if (!element) return std::unexpected(std::move(element.error()));
  return hir::PackedArrayType{
      .dim = LowerRange(array.range),
      .element_type = *element,
      .signedness =
          outer_signed ? hir::Signedness::kSigned : hir::Signedness::kUnsigned,
      .form = hir::PackedArrayForm::kExplicit,
  };
}

auto LowerPackedStruct(
    const slang::ast::PackedStructType& struct_type, diag::SourceSpan decl_span,
    ModuleLowerer& module) -> diag::Result<hir::PackedStructType> {
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
      .fields = std::move(fields),
      .signedness = struct_type.isSigned ? hir::Signedness::kSigned
                                         : hir::Signedness::kUnsigned,
      .four_state = struct_type.isFourState,
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
      .fields = std::move(fields),
      .signedness = union_type.isSigned ? hir::Signedness::kSigned
                                        : hir::Signedness::kUnsigned,
      .four_state = union_type.isFourState,
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
  // A tagged union (LRM 7.3.2) is a type-checked sum type, a separate concept
  // from the untagged overlapping-storage form; reject it here where the
  // declaration span is available, so MIR translation only ever sees untagged
  // unions.
  if (union_type.isTagged) {
    return diag::Fail(
        decl_span, diag::DiagCode::kUnsupportedUnpackedUnionType,
        "tagged unions are not yet supported");
  }
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
      return hir::TypeData{
          hir::ScalarBitType{.atom = LowerScalarAtom(scalar.scalarKind)}};
    }
    case slang::ast::SymbolKind::PredefinedIntegerType: {
      const auto& pi = canonical.as<slang::ast::PredefinedIntegerType>();
      return hir::TypeData{LowerPredefinedInteger(module, pi.integerKind)};
    }
    case slang::ast::SymbolKind::PackedArrayType: {
      auto pa = LowerExplicitPackedArray(
          module, canonical.as<slang::ast::PackedArrayType>(),
          canonical.isSigned(), decl_span);
      if (!pa.has_value()) {
        return std::unexpected(std::move(pa.error()));
      }
      return hir::TypeData{*std::move(pa)};
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
    case slang::ast::SymbolKind::NullType:
      return hir::TypeData{hir::NullType{}};
    case slang::ast::SymbolKind::ClassType: {
      auto class_id_or =
          module.InternClass(canonical.as<slang::ast::ClassType>(), decl_span);
      if (!class_id_or) {
        return std::unexpected(std::move(class_id_or.error()));
      }
      return hir::TypeData{hir::ClassHandleType{.class_id = *class_id_or}};
    }
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
      // A declared index covers string (LRM 7.8.2), integral, which includes a
      // packed struct / enum (LRM 7.8.4 / 7.8.5), and chandle, whose entries
      // may order arbitrarily (LRM 6.14). A class index (LRM 7.8.3) and a real
      // index are rejected.
      if (!(aa.indexType->isIntegral() || aa.indexType->isString() ||
            aa.indexType->isCHandle())) {
        return diag::Fail(
            decl_span, diag::DiagCode::kUnsupportedAssociativeArrayType,
            "associative arrays are only supported with a string, integral, "
            "chandle, or wildcard index type");
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

auto ModuleLowerer::InternClass(
    const slang::ast::ClassType& cls, diag::SourceSpan span)
    -> diag::Result<hir::ClassId> {
  if (const auto it = class_cache_.find(&cls); it != class_cache_.end()) {
    return it->second;
  }
  const hir::ClassId id = unit_.classes.Declare();
  class_cache_.emplace(&cls, id);

  hir::ClassDecl decl;
  decl.name = std::string(cls.name);
  for (const auto& prop :
       cls.membersOfType<slang::ast::ClassPropertySymbol>()) {
    if (prop.lifetime == slang::ast::VariableLifetime::Static) {
      return diag::Fail(
          span, diag::DiagCode::kUnsupportedClassFeature,
          "static class properties are not yet supported");
    }
    if (prop.getInitializer() != nullptr) {
      return diag::Fail(
          span, diag::DiagCode::kUnsupportedClassFeature,
          "class property initializers are not yet supported");
    }
    auto field_type = InternType(prop.getType(), span);
    if (!field_type) return std::unexpected(std::move(field_type.error()));
    decl.fields.push_back(
        hir::ClassField{
            .name = std::string(prop.name),
            .type = *field_type,
            .initializer = std::nullopt});
  }
  for (const auto& method : cls.membersOfType<slang::ast::SubroutineSymbol>()) {
    // Every class carries compiler-generated built-ins (the randomize family,
    // LRM 18.6); they are provided by the runtime, not lowered from source.
    if (method.flags.has(slang::ast::MethodFlags::BuiltIn)) {
      continue;
    }
    if (method.flags.has(slang::ast::MethodFlags::Constructor)) {
      for (const auto* formal : method.getArguments()) {
        if (formal->direction != slang::ast::ArgumentDirection::In) {
          return diag::Fail(
              span, diag::DiagCode::kUnsupportedClassFeature,
              "a constructor with an output / inout / ref argument is not yet "
              "supported");
        }
      }
      auto ctor_decl = LowerSubroutineDecl(*this, method, WalkFrame{});
      if (!ctor_decl) return std::unexpected(std::move(ctor_decl.error()));
      decl.constructor = *std::move(ctor_decl);
      continue;
    }
    if (method.flags.has(slang::ast::MethodFlags::Static)) {
      return diag::Fail(
          span, diag::DiagCode::kUnsupportedClassFeature,
          "static class methods are not yet supported");
    }
    if (method.flags.has(
            slang::ast::MethodFlags::Virtual | slang::ast::MethodFlags::Pure)) {
      return diag::Fail(
          span, diag::DiagCode::kUnsupportedClassFeature,
          "virtual class methods are not yet supported");
    }
    if (method.subroutineKind == slang::ast::SubroutineKind::Task) {
      return diag::Fail(
          span, diag::DiagCode::kUnsupportedClassFeature,
          "class task methods are not yet supported");
    }
    auto method_decl = LowerSubroutineDecl(*this, method, WalkFrame{});
    if (!method_decl) return std::unexpected(std::move(method_decl.error()));
    decl.methods.push_back(*std::move(method_decl));
  }
  unit_.classes.Define(id, std::move(decl));
  return id;
}

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
      unit_.types.Add(hir::Type{.data = *std::move(data_or)});
  type_cache_.emplace(canonical, id);
  return id;
}

}  // namespace lyra::lowering::ast_to_hir
