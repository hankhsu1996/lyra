#include "lyra/hir/type.hpp"

#include <algorithm>
#include <cstddef>
#include <cstdint>
#include <format>
#include <optional>
#include <set>
#include <span>
#include <string>
#include <tuple>
#include <utility>
#include <variant>
#include <vector>

#include <slang/ast/Compilation.h>
#include <slang/ast/Expression.h>
#include <slang/ast/Scope.h>
#include <slang/ast/Symbol.h>
#include <slang/ast/expressions/CallExpression.h>
#include <slang/ast/symbols/ClassSymbols.h>
#include <slang/ast/symbols/CompilationUnitSymbols.h>
#include <slang/ast/symbols/InstanceSymbols.h>
#include <slang/ast/symbols/MemberSymbols.h>
#include <slang/ast/symbols/SubroutineSymbols.h>
#include <slang/ast/symbols/VariableSymbols.h>
#include <slang/ast/types/AllTypes.h>
#include <slang/ast/types/Type.h>
#include <slang/numeric/ConstantValue.h>
#include <slang/numeric/SVInt.h>

#include "lyra/base/internal_error.hpp"
#include "lyra/diag/diagnostic.hpp"
#include "lyra/diag/source_span.hpp"
#include "lyra/hir/class_decl.hpp"
#include "lyra/hir/integral_constant.hpp"
#include "lyra/hir/procedural_body.hpp"
#include "lyra/hir/stmt.hpp"
#include "lyra/hir/subroutine.hpp"
#include "lyra/lowering/ast_to_hir/constant_value.hpp"
#include "lyra/lowering/ast_to_hir/constraint_block.hpp"
#include "lyra/lowering/ast_to_hir/covergroup_decl.hpp"
#include "lyra/lowering/ast_to_hir/expression/slang_atoms.hpp"
#include "lyra/lowering/ast_to_hir/fixed_name.hpp"
#include "lyra/lowering/ast_to_hir/integral_constant.hpp"
#include "lyra/lowering/ast_to_hir/name_in_a_path.hpp"
#include "lyra/lowering/ast_to_hir/process_lowerer.hpp"
#include "lyra/lowering/ast_to_hir/reads.hpp"
#include "lyra/lowering/ast_to_hir/std_classes.hpp"
#include "lyra/lowering/ast_to_hir/subroutine_decl.hpp"
#include "lyra/lowering/ast_to_hir/unit_identity.hpp"
#include "lyra/lowering/ast_to_hir/unit_lowerer.hpp"
#include "lyra/support/def_path.hpp"

namespace lyra::lowering::ast_to_hir {

namespace {

// An enum nested in an imported runtime class (LRM 9.7 `process::state`) is a
// value the runtime returns and the program compares, with no unit-emitted enum
// declaration behind it. It lowers to its underlying integral type.
auto EnumBelongsToImportedRuntimeClass(const slang::ast::EnumType& enum_type)
    -> bool {
  const slang::ast::Scope* scope = enum_type.getParentScope();
  if (scope == nullptr) {
    return false;
  }
  const slang::ast::Symbol& owner = scope->asSymbol();
  if (owner.kind != slang::ast::SymbolKind::ClassType) {
    return false;
  }
  return ImportedRuntimeClassOf(owner.as<slang::ast::ClassType>()).has_value();
}

auto LowerScalarAtom(slang::ast::ScalarType::Kind k) -> hir::BitAtom {
  switch (k) {
    case slang::ast::ScalarType::Bit:
      return hir::BitAtom::kBit;
    case slang::ast::ScalarType::Logic:
    case slang::ast::ScalarType::Reg:
      return hir::BitAtom::kLogic;
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
// `bit signed [31:0]`). Signedness is the declaration's, not the kind's: a
// `signed` or `unsigned` keyword overrides the Table 6-8 default (LRM 6.11.3).
auto LowerPredefinedInteger(
    const UnitLowerer& unit_lowerer,
    const slang::ast::PredefinedIntegerType& type) -> hir::PackedArrayType {
  using SK = slang::ast::PredefinedIntegerType::Kind;
  const auto& builtins = unit_lowerer.Unit().builtins;
  const auto make = [&](hir::TypeId element,
                        std::int64_t msb) -> hir::PackedArrayType {
    return hir::PackedArrayType{
        .dim = hir::PackedRange{.left = msb, .right = 0},
        .element_type = element,
        .signedness = type.isSigned ? hir::Signedness::kSigned
                                    : hir::Signedness::kUnsigned,
    };
  };
  switch (type.integerKind) {
    case SK::Byte:
      return make(builtins.scalar_bit, 7);
    case SK::ShortInt:
      return make(builtins.scalar_bit, 15);
    case SK::Int:
      return make(builtins.scalar_bit, 31);
    case SK::LongInt:
      return make(builtins.scalar_bit, 63);
    case SK::Integer:
      return make(builtins.scalar_logic, 31);
    case SK::Time:
      return make(builtins.scalar_logic, 63);
  }
  throw InternalError("LowerPredefinedInteger: unknown integer kind");
}

// One HIR node per declared dimension (LRM 7.4.1: a packed array is recursively
// other packed arrays / structures). The element is interned and named by its
// `TypeId`, so the nest -- and an aggregate element's identity -- is carried by
// recursion, not flattened here.
auto LowerExplicitPackedArray(
    UnitLowerer& unit_lowerer, const slang::ast::PackedArrayType& array,
    bool outer_signed, diag::SourceSpan span)
    -> diag::Result<hir::PackedArrayType> {
  auto element = unit_lowerer.InternType(array.elementType, span);
  if (!element) return std::unexpected(std::move(element.error()));
  return hir::PackedArrayType{
      .dim = LowerRange(array.range),
      .element_type = *element,
      .signedness =
          outer_signed ? hir::Signedness::kSigned : hir::Signedness::kUnsigned,
  };
}

auto LowerPackedStruct(
    const slang::ast::PackedStructType& struct_type, diag::SourceSpan decl_span,
    UnitLowerer& unit_lowerer) -> diag::Result<hir::PackedStructType> {
  std::vector<hir::PackedAggregateField> fields;
  for (const auto& field :
       struct_type.membersOfType<slang::ast::FieldSymbol>()) {
    auto field_type_or = unit_lowerer.InternType(field.getType(), decl_span);
    if (!field_type_or) {
      return std::unexpected(std::move(field_type_or.error()));
    }
    fields.push_back(
        hir::PackedAggregateField{
            .name = std::string(field.name),
            .type = *field_type_or,
        });
  }
  return hir::PackedStructType{
      .fields = std::move(fields),
      .signedness = struct_type.isSigned ? hir::Signedness::kSigned
                                         : hir::Signedness::kUnsigned,
  };
}

auto LowerPackedUnion(
    const slang::ast::PackedUnionType& union_type, diag::SourceSpan decl_span,
    UnitLowerer& unit_lowerer) -> diag::Result<hir::PackedUnionType> {
  std::vector<hir::PackedAggregateField> fields;
  for (const auto& field :
       union_type.membersOfType<slang::ast::FieldSymbol>()) {
    auto field_type_or = unit_lowerer.InternType(field.getType(), decl_span);
    if (!field_type_or) {
      return std::unexpected(std::move(field_type_or.error()));
    }
    fields.push_back(
        hir::PackedAggregateField{
            .name = std::string(field.name),
            .type = *field_type_or,
        });
  }
  return hir::PackedUnionType{
      .fields = std::move(fields),
      .signedness = union_type.isSigned ? hir::Signedness::kSigned
                                        : hir::Signedness::kUnsigned,
      .tagged = union_type.isTagged,
  };
}

// LRM 7.2.2: a member declaration may carry a default value, a constant
// expression used in place of the member type's Table 7-1 default when the
// enclosing struct is default-constructed. It is held in the member's
// value-form metadata, the shape an enum member carries its value in: the
// default is a value the type holds, not an expression. The front end
// evaluated it when the type was declared, which is what records the
// parameters it was computed from, so the value read here is that one.
auto LowerMemberDefault(
    const slang::ast::FieldSymbol& field, diag::SourceSpan span)
    -> diag::Result<std::optional<hir::ConstantValue>> {
  const auto* init = field.getInitializer();
  if (init == nullptr) {
    return std::optional<hir::ConstantValue>{};
  }
  const slang::ConstantValue* constant = init->getConstant();
  if (constant == nullptr) {
    throw InternalError(
        std::format(
            "LowerMemberDefault: the default of member '{}' was evaluated as "
            "a constant where its type was declared, and kept nothing",
            field.name));
  }
  auto value_or = MakeConstantValue(*constant, span);
  if (!value_or) {
    return std::unexpected(std::move(value_or.error()));
  }
  return std::optional<hir::ConstantValue>{*std::move(value_or)};
}

// How a property or a structure member of type `type` is declared random
// (LRM 18.4). A `randc` one runs through a permutation of its values, held
// for a value of up to 64 bits, which LRM 18.4.2 lets an implementation limit
// it to; an array's elements are each such a value.
auto RandomQualifierOf(
    const slang::ast::Type& type, slang::ast::RandMode mode,
    diag::SourceSpan span) -> diag::Result<hir::RandomQualifier> {
  switch (mode) {
    case slang::ast::RandMode::None:
      return hir::RandomQualifier::kNone;
    case slang::ast::RandMode::Rand:
      return hir::RandomQualifier::kRand;
    case slang::ast::RandMode::RandC: {
      const slang::ast::Type* value = &type.getCanonicalType();
      while (value->isUnpackedArray()) {
        value = &value->getArrayElementType()->getCanonicalType();
      }
      if (value->isIntegral() && value->getBitWidth() > 64) {
        return diag::Fail(
            span, diag::DiagCode::kUnsupportedClassFeature,
            "a 'randc' variable of {} bits is wider than the 64 bits "
            "supported",
            value->getBitWidth());
      }
      return hir::RandomQualifier::kRandc;
    }
  }
  throw InternalError("RandomQualifierOf: unknown rand mode");
}

auto LowerUnpackedAggregateFields(
    std::span<const slang::ast::FieldSymbol* const> fields,
    diag::SourceSpan decl_span, UnitLowerer& unit_lowerer)
    -> diag::Result<std::vector<hir::UnpackedAggregateField>> {
  std::vector<hir::UnpackedAggregateField> out;
  out.reserve(fields.size());
  for (const auto* field : fields) {
    auto field_type_or = unit_lowerer.InternType(field->getType(), decl_span);
    if (!field_type_or) {
      return std::unexpected(std::move(field_type_or.error()));
    }
    auto default_or = LowerMemberDefault(*field, decl_span);
    if (!default_or) {
      return std::unexpected(std::move(default_or.error()));
    }
    auto random =
        RandomQualifierOf(field->getType(), field->randMode, decl_span);
    if (!random) return std::unexpected(std::move(random.error()));
    out.push_back(
        hir::UnpackedAggregateField{
            .name = std::string(field->name),
            .type = *field_type_or,
            .default_init = *std::move(default_or),
            .random = *random,
        });
  }
  return out;
}

auto LowerUnpackedStruct(
    const slang::ast::UnpackedStructType& struct_type,
    diag::SourceSpan decl_span, UnitLowerer& unit_lowerer)
    -> diag::Result<hir::UnpackedStructType> {
  auto fields_or =
      LowerUnpackedAggregateFields(struct_type.fields, decl_span, unit_lowerer);
  if (!fields_or) return std::unexpected(std::move(fields_or.error()));
  return hir::UnpackedStructType{
      .declaration =
          hir::TypeDeclarationRef{
              .unit_name = CompilationUnitName(
                  UnitHomeOf(struct_type), unit_lowerer.Specialization()),
              .path = DefPathOf(struct_type, unit_lowerer.Specialization())},
      .fields = *std::move(fields_or)};
}

// The part of a value of `type` only a tagged union may hold (LRM 7.3) -- a
// string, a handle of any kind, a dynamically sized array -- however deep in
// unpacked arrays, structures and unions it lies, or nothing where no part is
// one.
auto PartOnlyATaggedUnionMayHold(const slang::ast::Type& type)
    -> const slang::ast::Type* {
  const slang::ast::Type& canonical = type.getCanonicalType();
  if (canonical.isString() || canonical.isHandleType() ||
      canonical.isDynamicallySizedArray()) {
    return &canonical;
  }
  if (canonical.isUnpackedArray()) {
    return PartOnlyATaggedUnionMayHold(*canonical.getArrayElementType());
  }
  const auto field_holding_one = [](const auto& aggregate) {
    for (const slang::ast::FieldSymbol* field : aggregate.fields) {
      if (const slang::ast::Type* part =
              PartOnlyATaggedUnionMayHold(field->getType())) {
        return part;
      }
    }
    return static_cast<const slang::ast::Type*>(nullptr);
  };
  if (const auto* structure =
          canonical.as_if<slang::ast::UnpackedStructType>()) {
    return field_holding_one(*structure);
  }
  if (const auto* inner = canonical.as_if<slang::ast::UnpackedUnionType>()) {
    return field_holding_one(*inner);
  }
  return nullptr;
}

auto LowerUnpackedUnion(
    const slang::ast::UnpackedUnionType& union_type, diag::SourceSpan decl_span,
    UnitLowerer& unit_lowerer) -> diag::Result<hir::UnpackedUnionType> {
  // Both the untagged overlapping-storage form (LRM 7.3) and the tagged
  // type-checked sum form (LRM 7.3.2) come through here.
  //
  // An untagged union's members lie over one another's bytes, so a member
  // owning what it holds, or one whose value is no bits, has no such bytes to
  // share. The standard admits one only in a tagged union (LRM 7.3, 6.16), and
  // the front end lets a string, a class handle and an event through.
  if (!union_type.isTagged) {
    for (const slang::ast::FieldSymbol* field : union_type.fields) {
      if (const slang::ast::Type* part =
              PartOnlyATaggedUnionMayHold(field->getType())) {
        return diag::Fail(
            decl_span, diag::DiagCode::kUnsupportedTypeKind,
            "an untagged union member cannot hold a '{}'; only a tagged "
            "union may hold one",
            part->toString());
      }
    }
  }
  auto fields_or =
      LowerUnpackedAggregateFields(union_type.fields, decl_span, unit_lowerer);
  if (!fields_or) return std::unexpected(std::move(fields_or.error()));
  return hir::UnpackedUnionType{
      .declaration =
          hir::TypeDeclarationRef{
              .unit_name = CompilationUnitName(
                  UnitHomeOf(union_type), unit_lowerer.Specialization()),
              .path = DefPathOf(union_type, unit_lowerer.Specialization())},
      .fields = *std::move(fields_or),
      .tagged = union_type.isTagged,
  };
}

// A member's value varies with the enumeration it belongs to (LRM 6.19), and a
// type is an axis an artifact's identity already has, so settling it here
// cannot cost a second artifact. Where a repeated block declares the
// enumeration itself, the blocks declare different types and are compiled apart
// for that reason rather than for this one.
auto LowerEnum(
    const slang::ast::EnumType& enum_type, diag::SourceSpan decl_span,
    UnitLowerer& unit_lowerer) -> diag::Result<hir::EnumType> {
  auto base_id_or = unit_lowerer.InternType(enum_type.baseType, decl_span);
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
      .declaration =
          hir::TypeDeclarationRef{
              .unit_name = CompilationUnitName(
                  UnitHomeOf(enum_type), unit_lowerer.Specialization()),
              .path = DefPathOf(enum_type, unit_lowerer.Specialization())},
      .base_type = *base_id_or,
      .members = std::move(members),
  };
}

auto TranslateType(
    UnitLowerer& unit_lowerer, const slang::ast::Type& type,
    diag::SourceSpan decl_span) -> diag::Result<hir::Type> {
  const auto& canonical = type.getCanonicalType();

  switch (canonical.kind) {
    case slang::ast::SymbolKind::ScalarType: {
      const auto& scalar = canonical.as<slang::ast::ScalarType>();
      return hir::Type{
          hir::ScalarBitType{.atom = LowerScalarAtom(scalar.scalarKind)}};
    }
    case slang::ast::SymbolKind::PredefinedIntegerType: {
      return hir::Type{LowerPredefinedInteger(
          unit_lowerer, canonical.as<slang::ast::PredefinedIntegerType>())};
    }
    case slang::ast::SymbolKind::PackedArrayType: {
      auto pa = LowerExplicitPackedArray(
          unit_lowerer, canonical.as<slang::ast::PackedArrayType>(),
          canonical.isSigned(), decl_span);
      if (!pa.has_value()) {
        return std::unexpected(std::move(pa.error()));
      }
      return hir::Type{*std::move(pa)};
    }
    case slang::ast::SymbolKind::EnumType: {
      const auto& enum_type = canonical.as<slang::ast::EnumType>();
      if (EnumBelongsToImportedRuntimeClass(enum_type)) {
        return TranslateType(unit_lowerer, enum_type.baseType, decl_span);
      }
      auto e = LowerEnum(enum_type, decl_span, unit_lowerer);
      if (!e.has_value()) {
        return std::unexpected(std::move(e.error()));
      }
      return hir::Type{*std::move(e)};
    }
    case slang::ast::SymbolKind::FloatingType: {
      const auto& f = canonical.as<slang::ast::FloatingType>();
      switch (f.floatKind) {
        case slang::ast::FloatingType::Real:
          return hir::Type{hir::RealType{}};
        case slang::ast::FloatingType::ShortReal:
          return hir::Type{hir::ShortRealType{}};
        case slang::ast::FloatingType::RealTime:
          return hir::Type{hir::RealTimeType{}};
      }
      throw InternalError("TranslateType: unknown FloatingType kind");
    }
    case slang::ast::SymbolKind::StringType:
      return hir::Type{hir::StringType{}};
    case slang::ast::SymbolKind::EventType:
      return hir::Type{hir::EventType{}};
    case slang::ast::SymbolKind::CHandleType:
      return hir::Type{hir::ChandleType{}};
    case slang::ast::SymbolKind::NullType:
      return hir::Type{hir::NullType{}};
    // What the front end gives a mailbox's message type where none was
    // stated, LRM 15.4.9's `dynamic_type`.
    case slang::ast::SymbolKind::UntypedType:
      return hir::Type{hir::DynamicType{}};
    case slang::ast::SymbolKind::ClassType: {
      const auto& class_type = canonical.as<slang::ast::ClassType>();
      if (const auto imported = ImportedRuntimeClassOf(class_type)) {
        return hir::Type{hir::ImportedClassHandleType{.klass = *imported}};
      }
      auto class_ref_or = unit_lowerer.ResolveClassRef(class_type, decl_span);
      if (!class_ref_or) {
        return std::unexpected(std::move(class_ref_or.error()));
      }
      return hir::Type{
          hir::ClassHandleType{.class_ref = *std::move(class_ref_or)}};
    }
    // A variable of a covergroup's type holds an instance of it (LRM 19.3).
    case slang::ast::SymbolKind::CovergroupType: {
      auto covergroup = unit_lowerer.ResolveCovergroupRef(
          canonical.as<slang::ast::CovergroupType>(), decl_span);
      if (!covergroup) return std::unexpected(std::move(covergroup.error()));
      return hir::Type{
          hir::CovergroupHandleType{.covergroup = *std::move(covergroup)}};
    }
    case slang::ast::SymbolKind::VoidType:
      return hir::Type{hir::VoidType{}};
    case slang::ast::SymbolKind::PackedStructType: {
      auto s = LowerPackedStruct(
          canonical.as<slang::ast::PackedStructType>(), decl_span,
          unit_lowerer);
      if (!s.has_value()) {
        return std::unexpected(std::move(s.error()));
      }
      return hir::Type{*std::move(s)};
    }
    case slang::ast::SymbolKind::PackedUnionType: {
      auto u = LowerPackedUnion(
          canonical.as<slang::ast::PackedUnionType>(), decl_span, unit_lowerer);
      if (!u.has_value()) {
        return std::unexpected(std::move(u.error()));
      }
      return hir::Type{*std::move(u)};
    }
    case slang::ast::SymbolKind::FixedSizeUnpackedArrayType: {
      const auto& fa = canonical.as<slang::ast::FixedSizeUnpackedArrayType>();
      auto elem_id_or = unit_lowerer.InternType(fa.elementType, decl_span);
      if (!elem_id_or) {
        return std::unexpected(std::move(elem_id_or.error()));
      }
      return hir::Type{hir::UnpackedArrayType{
          .element_type = *elem_id_or,
          .dim =
              hir::UnpackedRange{
                  .left = static_cast<std::int64_t>(fa.range.left),
                  .right = static_cast<std::int64_t>(fa.range.right)},
      }};
    }
    case slang::ast::SymbolKind::DynamicArrayType: {
      const auto& da = canonical.as<slang::ast::DynamicArrayType>();
      auto elem_id_or = unit_lowerer.InternType(da.elementType, decl_span);
      if (!elem_id_or) {
        return std::unexpected(std::move(elem_id_or.error()));
      }
      return hir::Type{hir::DynamicArrayType{.element_type = *elem_id_or}};
    }
    case slang::ast::SymbolKind::QueueType: {
      const auto& q = canonical.as<slang::ast::QueueType>();
      auto elem_id_or = unit_lowerer.InternType(q.elementType, decl_span);
      if (!elem_id_or) {
        return std::unexpected(std::move(elem_id_or.error()));
      }
      // slang encodes the unbounded queue (`[$]`) as maxBound == 0; a bounded
      // queue (`[$:N]`) carries the bound (LRM 7.10.4).
      return hir::Type{hir::QueueType{
          .element_type = *elem_id_or,
          .max_bound = q.maxBound == 0
                           ? std::nullopt
                           : std::optional<std::uint64_t>{q.maxBound},
      }};
    }
    case slang::ast::SymbolKind::AssociativeArrayType: {
      const auto& aa = canonical.as<slang::ast::AssociativeArrayType>();
      auto elem_id_or = unit_lowerer.InternType(aa.elementType, decl_span);
      if (!elem_id_or) {
        return std::unexpected(std::move(elem_id_or.error()));
      }
      // LRM 7.8.1 wildcard index (`[*]`) declares no index type: the key is any
      // integral value identified by its magnitude, carried by the dedicated
      // wildcard-index key type.
      if (aa.indexType == nullptr) {
        return hir::Type{hir::AssociativeArrayType{
            .element_type = *elem_id_or,
            .key_type = unit_lowerer.Unit().builtins.wildcard_index,
        }};
      }
      // A declared index covers string (LRM 7.8.2), class, whose entries order
      // deterministically but arbitrarily and whose null is a valid index
      // (LRM 7.8.3), integral, which includes a packed struct / enum
      // (LRM 7.8.4 / 7.8.5), and chandle, ordering as arbitrarily as a class
      // does (LRM 6.14). A real index is rejected.
      if (!(aa.indexType->isIntegral() || aa.indexType->isString() ||
            aa.indexType->isCHandle() || aa.indexType->isClass())) {
        return diag::Fail(
            decl_span, diag::DiagCode::kUnsupportedAssociativeArrayType,
            "an associative array with an index of this type is not yet "
            "supported; a string, class, integral, 'chandle' or wildcard "
            "index is");
      }
      auto key_id_or = unit_lowerer.InternType(*aa.indexType, decl_span);
      if (!key_id_or) {
        return std::unexpected(std::move(key_id_or.error()));
      }
      return hir::Type{hir::AssociativeArrayType{
          .element_type = *elem_id_or,
          .key_type = *key_id_or,
      }};
    }
    case slang::ast::SymbolKind::UnpackedStructType: {
      auto s = LowerUnpackedStruct(
          canonical.as<slang::ast::UnpackedStructType>(), decl_span,
          unit_lowerer);
      if (!s) return std::unexpected(std::move(s.error()));
      return hir::Type{*std::move(s)};
    }
    case slang::ast::SymbolKind::UnpackedUnionType: {
      auto u = LowerUnpackedUnion(
          canonical.as<slang::ast::UnpackedUnionType>(), decl_span,
          unit_lowerer);
      if (!u) return std::unexpected(std::move(u.error()));
      return hir::Type{*std::move(u)};
    }
    // LRM 25.9: the type names the interface together with the parameters it
    // was given, which is exactly what names the unit an instance of it is.
    case slang::ast::SymbolKind::VirtualInterfaceType:
      return hir::Type{hir::VirtualInterfaceType{
          .unit_name = unit_lowerer.Specialization().NameOf(
              canonical.as<slang::ast::VirtualInterfaceType>().iface)}};
    default:
      return diag::Fail(
          decl_span, diag::DiagCode::kUnsupportedTypeKind,
          "the type '{}' is not yet supported", canonical.toString());
  }
}

// Looks up the given method name in `derived_cls`'s base chain for a pure
// virtual prototype (LRM 8.21) an implementation on `derived_cls` should
// override. Slang's `checkForOverride` establishes a
// `SubroutineSymbol::getOverride()` link only when the base method is itself
// a `SubroutineSymbol`; when the base declares the method as `pure virtual`
// (a `MethodPrototypeSymbol`), slang leaves the derived method's
// `overrides` unset, so this lookup re-establishes the link. Slang's own
// `Scope::find` already flattens the ancestor chain by unwrapping the
// `TransparentMember` wrappers each inheriting scope inserts, so one query
// on the immediate base scope reaches the actual declaration wherever it
// sits in the chain.
auto FindOverriddenPureInBaseChain(
    const slang::ast::ClassType& derived_cls, std::string_view method_name)
    -> const slang::ast::MethodPrototypeSymbol* {
  const auto* base_type = derived_cls.getBaseClass();
  if (base_type == nullptr) return nullptr;
  const auto& base_cls =
      base_type->getCanonicalType().as<slang::ast::ClassType>();
  const auto* found = base_cls.find(method_name);
  if (found == nullptr ||
      found->kind != slang::ast::SymbolKind::MethodPrototype) {
    return nullptr;
  }
  const auto& proto = found->as<slang::ast::MethodPrototypeSymbol>();
  if (!proto.flags.has(slang::ast::MethodFlags::Pure)) return nullptr;
  return &proto;
}

// Locates the pure virtual method prototype (LRM 8.26) an interface class
// the interface class `cls` extends declares, matched by name: an interface
// class restating a method of one it extends states the same behavior.
// Interfaces are consulted in declaration order, and the first match wins
// (LRM 8.26.6.1 name conflict resolution).
//
// A class of the source language implementing an interface class is not
// asked this. Its method answers the interface's behavior, and it is also a
// behavior of the class's own lineage, which a class extending it may take
// over; which lineage behavior answers each interface behavior is the class's
// conformance, stated beside its methods.
auto FindOverriddenPureInExtendedInterfaces(
    const slang::ast::ClassType& cls, std::string_view method_name)
    -> const slang::ast::MethodPrototypeSymbol* {
  for (const auto* iface : cls.getDeclaredInterfaces()) {
    const auto& iface_cls =
        iface->getCanonicalType().as<slang::ast::ClassType>();
    const auto* found = iface_cls.find(method_name);
    if (found == nullptr ||
        found->kind != slang::ast::SymbolKind::MethodPrototype) {
      continue;
    }
    const auto& proto = found->as<slang::ast::MethodPrototypeSymbol>();
    if (!proto.flags.has(slang::ast::MethodFlags::Pure)) continue;
    return &proto;
  }
  return nullptr;
}

// LRM 8.7: a class the source declares without a `function new` still has a
// constructor -- the implicit `new`, whose only effect is the property
// initialization every class performs. It is modeled as a constructor with no
// formals and an empty body; what construction does is composed onto that body
// afterwards, the same way it is for a user-written constructor -- the property
// initializers, and the arguments the base's construction is entered with.
auto SynthesizeDefaultConstructor(
    hir::TypeId void_type, diag::SourceSpan span, const WalkFrame& class_frame)
    -> hir::SubroutineDecl {
  hir::ProceduralBody body;
  // A default constructor runs nothing of its own; the class's field
  // initializers are composed into it afterwards.
  const hir::StmtId root_stmt = body.stmts.Add(
      hir::Stmt{.label = std::nullopt, .data = hir::EmptyStmt{}, .span = span});
  body.root_scope = class_frame.SealScope(
      OpenProceduralScope{
          class_frame.ProceduralScopes().Declare(),
          hir::ProceduralScopeKind::kSubroutineRoot, std::string{"new"}});
  return hir::SubroutineDecl{
      .name = "new",
      .kind = hir::SubroutineKind::kFunction,
      .result_type = void_type,
      .params = {},
      .result_var = std::nullopt,
      .body = std::move(body),
      .root_stmt = root_stmt,
      .is_virtual = false,
      .overrides = std::nullopt,
      .reads = {}};
}

// The class declaring `member`, a method or a constraint block. A method
// defined out of block (LRM 8.24) and the stub standing for a pure virtual one
// (LRM 8.21) sit in that class as well.
auto OwningClassOf(const slang::ast::Symbol& member)
    -> const slang::ast::ClassType& {
  return member.getParentScope()->asSymbol().as<slang::ast::ClassType>();
}

// Whether `overridden` belongs to the lineage of `cls`: always for an interface
// class, whose lineage is what it extends, and otherwise unless it is declared
// by an interface class.
auto InLineageOf(
    const slang::ast::ClassType& cls,
    const slang::ast::SubroutineSymbol& overridden) -> bool {
  return cls.isInterface || !OwningClassOf(overridden).isInterface;
}

// The method of `cls`'s lineage the name `name` finds, as the front end finds
// the implementation of an interface class's behavior; nothing where the
// lineage declares none, which only an abstract class leaves so.
auto ImplementationOf(const slang::ast::ClassType& cls, std::string_view name)
    -> const slang::ast::SubroutineSymbol* {
  const slang::ast::Symbol* found = cls.find(name);
  if (found == nullptr) {
    return nullptr;
  }
  const slang::ast::SubroutineSymbol* method = SubroutineOf(*found);
  return method != nullptr && InLineageOf(cls, *method) ? method : nullptr;
}

// Builds the forwarding method for one interface pure virtual method a class
// satisfies through an inherited concrete-base method rather than a local
// definition (LRM 8.26.2). It overrides the inherited implementation's
// behavior, and its body forwards to that implementation through a
// `super`-qualified call, so a backend renders it as an ordinary method rather
// than fabricating the forward.
auto BuildInterfaceForwardingMethod(
    UnitLowerer& unit_lowerer, diag::SourceSpan span, std::string_view name,
    const slang::ast::SubroutineSymbol& impl, const WalkFrame& class_frame)
    -> diag::Result<hir::SubroutineDecl> {
  // The implementation is inherited, so the class declaring it may be another
  // unit's, and what a call to it passes and yields is read as any call to it
  // reads it.
  auto forwarded = unit_lowerer.ClassMethodPrototype(impl, span);
  if (!forwarded) return std::unexpected(std::move(forwarded.error()));
  const hir::TypeId result_type = forwarded->result_type;

  hir::ProceduralBody body;
  OpenProceduralScope root{
      class_frame.ProceduralScopes().Declare(),
      hir::ProceduralScopeKind::kSubroutineRoot,
      NameInAPath(name, impl.getParentScope()->getCompilation())};
  std::vector<hir::SubroutineParam> params;
  std::vector<std::optional<hir::ExprId>> args;
  for (const hir::ExternalCalleeParam& formal : forwarded->interface.params) {
    // The forwarder would have to hand its own completion whatever the
    // forwarded call's completion carried back, which is a composition its
    // one-statement body does not express.
    switch (formal.direction) {
      case hir::ParamDirection::kInput:
        break;
      case hir::ParamDirection::kOutput:
      case hir::ParamDirection::kInOut:
      case hir::ParamDirection::kRef:
      case hir::ParamDirection::kConstRef:
        return diag::Fail(
            span, diag::DiagCode::kUnsupportedClassFeature,
            "an interface method satisfied by an inherited implementation with "
            "an 'output', 'inout' or 'ref' argument is not yet supported");
    }
    const hir::ProceduralVarId var = body.procedural_vars.Declare();
    body.procedural_vars.Define(
        var, hir::ProceduralVarDecl{.name = std::nullopt, .type = formal.type});
    root.declarations.push_back(var);
    params.push_back(
        hir::SubroutineParam{
            .var = var, .direction = hir::ParamDirection::kInput});
    args.emplace_back(body.exprs.Add(
        hir::Expr{
            .type = formal.type,
            .data = hir::PrimaryExpr{hir::ProceduralVarRef{.var = var}},
            .span = span}));
  }

  std::optional<hir::ProceduralVarId> result_var;
  if (result_type != unit_lowerer.Unit().builtins.void_type) {
    result_var = body.procedural_vars.Declare();
    body.procedural_vars.Define(
        *result_var,
        hir::ProceduralVarDecl{.name = std::nullopt, .type = result_type});
    root.declarations.push_back(*result_var);
  }

  auto impl_callee =
      unit_lowerer.MakeMethodCallee(OwningClassOf(impl), impl, span);
  if (!impl_callee) return std::unexpected(std::move(impl_callee.error()));
  const hir::ExprId call = body.exprs.Add(
      hir::Expr{
          .type = result_type,
          .data =
              hir::CallExpr{
                  .callee =
                      hir::MethodCallRef{
                          .receiver = hir::SuperReceiver{},
                          .callee = *std::move(impl_callee)},
                  .arguments = std::move(args)},
          .span = span});
  // Forwarding is the whole body: one return of the forwarded call, with the
  // formals and the result cell in the method's root scope.
  const hir::StmtId root_stmt = body.stmts.Add(
      hir::Stmt{
          .label = std::nullopt,
          .data = hir::ReturnStmt{.value = call},
          .span = span});
  body.root_scope = class_frame.SealScope(std::move(root));

  // The front end requires the inherited implementation to be virtual, so it
  // is a behavior of this class's lineage, and the forward takes it over: a
  // value of this class answers it with the forward, which enters the
  // implementation it was inherited with.
  auto taken = unit_lowerer.MakeVirtualBehavior(impl, span);
  if (!taken) return std::unexpected(std::move(taken.error()));

  // The forwarder's text reads no cell of its own: what a call of it reads is
  // what the implementation reads, which states its own as the forwarder calls
  // it, and a method of an object is no call an implicit list follows.
  return hir::SubroutineDecl{
      .name = std::string{name},
      .kind = hir::SubroutineKind::kFunction,
      .result_type = result_type,
      .params = std::move(params),
      .result_var = result_var,
      .body = std::move(body),
      .root_stmt = root_stmt,
      .is_virtual = true,
      .is_prototype = false,
      .is_static = false,
      .overrides = *std::move(taken),
      .reads = {}};
}

// Synthesizes forwarding methods for every interface pure virtual method a
// class satisfies by inheriting a concrete-base implementation with no local
// override (LRM 8.26.2). C++ multiple inheritance does not let one sibling
// base's method override another's, so without the forward the class stays
// abstract and a call through it is ambiguous. One forwarding method per
// inherited implementation: a single method satisfying several same-name
// interface slots (LRM 8.26.6.1) forwards once, and the target language's
// implicit override fills the remaining slots.
auto SynthesizeInterfaceForwardingMethods(
    UnitLowerer& unit_lowerer, const slang::ast::ClassType& cls,
    diag::SourceSpan span, hir::ClassDecl& decl, const WalkFrame& class_frame)
    -> diag::Result<void> {
  // An interface class only aggregates contracts (LRM 8.26); it carries no
  // implementation to forward to, so it never bridges. Its `extends` parents
  // are pure prototypes, not satisfiers.
  if (cls.isInterface) return {};
  // The interface classes the declaration names and the ones those extend. What
  // the class it extends is also a value of is that class's to forward.
  std::vector<UnitLowerer::LocalOrPublishedClass> named;
  for (const auto* iface_type : cls.getDeclaredInterfaces()) {
    auto iface = unit_lowerer.ReadAsLocalOrPublished(*iface_type, span);
    if (!iface) return std::unexpected(std::move(iface.error()));
    if (auto reached = unit_lowerer.ReachInterface(*iface, span, named);
        !reached) {
      return std::unexpected(std::move(reached.error()));
    }
  }
  std::set<const slang::ast::SubroutineSymbol*> bridged;
  for (const UnitLowerer::LocalOrPublishedClass& iface : named) {
    for (const std::string& name : unit_lowerer.MethodNamesOf(iface)) {
      // A satisfier the class defines is wired by its own method loop, so only
      // an inherited one needs a bridge, and only one with a body to forward
      // to: a still-abstract class may inherit nothing but a pure virtual.
      const slang::ast::SubroutineSymbol* impl = ImplementationOf(cls, name);
      if (impl == nullptr || &OwningClassOf(*impl) == &cls ||
          impl->flags.has(slang::ast::MethodFlags::Pure)) {
        continue;
      }
      if (!bridged.insert(impl).second) continue;

      auto method_or = BuildInterfaceForwardingMethod(
          unit_lowerer, span, name, *impl, class_frame);
      if (!method_or) return std::unexpected(std::move(method_or.error()));
      decl.methods.Add(*std::move(method_or));
    }
  }
  return {};
}

// The method this one overrides in its own lineage, or nothing where it
// introduces its own behaviour. The frontend resolves the ordinary case, having
// already matched signature, direction, return type and every other LRM 8.20
// compatibility rule, so that answer is consumed rather than recomputed. It
// leaves the link unset in the two cases where the overridden declaration
// carries no body -- a pure virtual method in the base chain (LRM 8.21) and,
// for an interface class, the contract of one it extends (LRM 8.26) -- so those
// are re-established against the chains that can hold one, and each answers
// through the stub carrying its signature. An interface class a class of the
// source implements is not in its lineage, so a method answering one of its
// behaviors overrides nothing by doing so.
auto OverriddenMethod(
    const slang::ast::ClassType& cls,
    const slang::ast::SubroutineSymbol& method)
    -> const slang::ast::SubroutineSymbol* {
  if (const auto* resolved = method.getOverride(); resolved != nullptr) {
    return InLineageOf(cls, *resolved) ? resolved : nullptr;
  }
  // A method declared as an `extern` prototype and defined out of block (LRM
  // 8.24) is two symbols, and the override link sits on the prototype -- the
  // declaration standing in the class body, which is where the base chain was
  // in scope. The definition is the same method and answers the same, so a
  // consumer that asked only the definition would read every out-of-block
  // override as introducing behaviour of its own.
  if (const auto* prototype = method.getPrototype(); prototype != nullptr) {
    if (const auto* target = prototype->getOverride(); target != nullptr) {
      if (const auto* resolved = SubroutineOf(*target);
          resolved != nullptr && InLineageOf(cls, *resolved)) {
        return resolved;
      }
    }
  }
  const slang::ast::MethodPrototypeSymbol* contract =
      FindOverriddenPureInBaseChain(cls, method.name);
  if (contract == nullptr && cls.isInterface) {
    contract = FindOverriddenPureInExtendedInterfaces(cls, method.name);
  }
  if (contract == nullptr) {
    return nullptr;
  }
  const auto* stub = contract->getSubroutine();
  if (stub == nullptr) {
    throw InternalError(
        "OverriddenMethod: pure virtual prototype has no stub subroutine "
        "slang normally materializes");
  }
  return stub;
}

// A constraint block (LRM 18.5) as the method of its class it is: called with
// the problem of the call of `randomize()` that reaches it, its body the one
// constraint tree it states. Every block is virtual, and one of a name a class
// it extends declares overrides that one (LRM 18.5.2); a static block's mode is
// its class's rather than the object's, and it still constrains the object it
// is called on (LRM 18.5.10). A `pure` block states only that every class
// extending it defines one (LRM 18.5.1).
auto LowerConstraintBlockDecl(
    UnitLowerer& unit_lowerer, const slang::ast::ClassType& cls,
    const slang::ast::ConstraintBlockSymbol& block,
    const WalkFrame& class_frame, diag::SourceSpan span)
    -> diag::Result<hir::SubroutineDecl> {
  auto void_type =
      unit_lowerer.InternType(cls.getCompilation().getVoidType(), span);
  if (!void_type) return std::unexpected(std::move(void_type.error()));
  hir::ProceduralBody body = unit_lowerer.MakeProceduralBody(block);
  ProcessLowerer lowerer(unit_lowerer, block);
  OpenProceduralScope root{
      unit_lowerer.LookupProceduralScope(block),
      hir::ProceduralScopeKind::kSubroutineRoot, NameInAPath(block)};
  const WalkFrame frame =
      class_frame.WithProceduralBody(&body).WithOpenScope(&root);
  const bool is_pure = block.flags.has(slang::ast::ConstraintBlockFlags::Pure);
  const bool is_static =
      block.flags.has(slang::ast::ConstraintBlockFlags::Static);
  hir::StmtId root_stmt{};
  if (is_pure) {
    root_stmt = body.stmts.Add(
        hir::Stmt{
            .label = std::nullopt, .data = hir::EmptyStmt{}, .span = span});
  } else {
    auto constraints = lowerer.LowerConstraint(block.getConstraints(), frame);
    if (!constraints) return std::unexpected(std::move(constraints.error()));
    root_stmt = body.stmts.Add(
        hir::Stmt{
            .label = std::nullopt,
            .data =
                hir::ConstraintStmt{
                    .constraints =
                        body.constraints.Add(*std::move(constraints))},
            .span = span});
  }
  body.root_scope = frame.SealScope(std::move(root));

  std::optional<hir::VirtualBehavior> overrides;
  if (const auto* base = OverriddenConstraintBlock(cls, block.name)) {
    auto taken = unit_lowerer.MakeVirtualBehavior(*base, span);
    if (!taken) return std::unexpected(std::move(taken.error()));
    overrides = *std::move(taken);
  }
  return hir::SubroutineDecl{
      .name = std::string{block.name},
      .kind = is_static ? hir::SubroutineKind::kStaticConstraintBlock
                        : hir::SubroutineKind::kConstraintBlock,
      .result_type = *void_type,
      .params = {},
      .result_var = std::nullopt,
      .body = std::move(body),
      .root_stmt = root_stmt,
      .is_virtual = true,
      .is_prototype = is_pure,
      .is_static = false,
      .overrides = std::move(overrides),
      .reads = {}};
}

// Lowers one method a class defines. SystemVerilog has two spellings for that
// and they differ only in where the source put the body: written inline in the
// class body, or declared there as an `extern` prototype with the definition
// outside it (LRM 8.24). Both arrive here as the subroutine the frontend
// resolved the declaration to, carrying the same source-level dispatch facts --
// `static` (LRM 8.10) makes the signature receiver-less, `virtual` (LRM 8.20)
// puts the method in the class's dispatch table.
auto LowerDefinedClassMethod(
    UnitLowerer& unit_lowerer, const slang::ast::ClassType& cls,
    const slang::ast::SubroutineSymbol& method, const WalkFrame& class_frame,
    diag::SourceSpan span) -> diag::Result<hir::SubroutineDecl> {
  auto method_decl = LowerSubroutineDecl(unit_lowerer, method, class_frame);
  if (!method_decl) return std::unexpected(std::move(method_decl.error()));
  method_decl->is_static = method.flags.has(slang::ast::MethodFlags::Static);
  method_decl->is_virtual = method.flags.has(slang::ast::MethodFlags::Virtual);
  // Slang enforces at declaration that static and virtual are mutually
  // exclusive, so a receiver-less method fills no slot.
  const auto* overridden =
      method_decl->is_static ? nullptr : OverriddenMethod(cls, method);
  if (overridden == nullptr) {
    return method_decl;
  }
  auto taken = unit_lowerer.MakeVirtualBehavior(*overridden, span);
  if (!taken) return std::unexpected(std::move(taken.error()));
  method_decl->overrides = *std::move(taken);
  // A method that overrides another is itself virtual, whether or not the
  // derived declaration repeated the keyword (LRM 8.20, 8.26.2).
  method_decl->is_virtual = true;
  return method_decl;
}

}  // namespace

auto UnitLowerer::MakeClassMethodTarget(
    const hir::ClassRef& class_ref,
    const slang::ast::SubroutineSymbol& method) const
    -> hir::ClassMethodTarget {
  if (const auto* local = std::get_if<hir::LocalClassRef>(&class_ref)) {
    return hir::LocalClassMethodTarget{
        .owner = local->class_id, .method = LookupMethodId(*Defining(method))};
  }
  const auto& ext = std::get<hir::ExternalClassRef>(class_ref);
  return hir::ExternalClassMethodTarget{
      .unit_name = ext.unit_name,
      .class_path = ext.class_path,
      .method_name = std::string(method.name)};
}

auto UnitLowerer::MakeMethodCallee(
    const slang::ast::ClassType& owner,
    const slang::ast::SubroutineSymbol& method, diag::SourceSpan span)
    -> diag::Result<hir::MethodCallee> {
  auto owner_ref = ResolveClassRef(owner, span);
  if (!owner_ref) return std::unexpected(std::move(owner_ref.error()));
  const hir::ClassRef& class_ref = *owner_ref;
  auto target = MakeClassMethodTarget(class_ref, method);
  if (const auto* local = std::get_if<hir::LocalClassMethodTarget>(&target)) {
    return *local;
  }
  const auto& ext = std::get<hir::ExternalClassRef>(class_ref);
  // The class declaring the method published it, so what the call passes and
  // awaits (LRM 13.5) and whether the object decides which body runs (LRM
  // 8.20) are read off that class's signature.
  const hir::PublishedMethod* declared = hir::FindMethod(
      ExternalClassOf(ext.unit_name, ext.class_path).methods, method.name);
  if (declared == nullptr) {
    throw InternalError(
        std::format(
            "UnitLowerer::MakeMethodCallee: '{}::{}' declares '{}' and "
            "published no such method",
            ext.unit_name, support::DisplayOf(ext.class_path), method.name));
  }
  hir::ExternalCalleeInterface interface = declared->prototype.interface;
  const bool is_virtual = std::visit(
      Overloaded{
          [](const hir::TypeAssociated&) { return false; },
          [](const hir::NotVirtual&) { return false; },
          [](const hir::IntroducesVirtual&) { return true; },
          [](const hir::OverridesVirtual&) { return true; }},
      declared->dispatch);
  std::optional<hir::VirtualBehavior> behavior;
  if (is_virtual) {
    auto resolved = MakeVirtualBehavior(ext, method.name, span);
    if (!resolved) return std::unexpected(std::move(resolved.error()));
    behavior = *std::move(resolved);
  }
  return hir::ExternalMethodCallee{
      .target = std::get<hir::ExternalClassMethodTarget>(std::move(target)),
      .behavior = std::move(behavior),
      .interface = std::move(interface)};
}

auto UnitLowerer::PublishedMethodOf(
    const slang::ast::SubroutineSymbol& method, diag::SourceSpan span)
    -> diag::Result<std::optional<hir::PublishedMethod>> {
  auto declaring = ResolveClassRef(OwningClassOf(method), span);
  if (!declaring) return std::unexpected(std::move(declaring.error()));
  if (const auto* ext = std::get_if<hir::ExternalClassRef>(&*declaring)) {
    if (const hir::PublishedMethod* declared = hir::FindMethod(
            ExternalClassOf(ext->unit_name, ext->class_path).methods,
            method.name)) {
      return std::optional{*declared};
    }
  }
  return std::nullopt;
}

auto UnitLowerer::IsTypeAssociatedMethod(
    const slang::ast::SubroutineSymbol& method, diag::SourceSpan span)
    -> diag::Result<bool> {
  auto published = PublishedMethodOf(method, span);
  if (!published) return std::unexpected(std::move(published.error()));
  if (published->has_value()) {
    return std::visit(
        Overloaded{
            [](const hir::TypeAssociated&) { return true; },
            [](const hir::NotVirtual&) { return false; },
            [](const hir::IntroducesVirtual&) { return false; },
            [](const hir::OverridesVirtual&) { return false; }},
        (*published)->dispatch);
  }
  return method.flags.has(slang::ast::MethodFlags::Static);
}

auto UnitLowerer::ClassMethodPrototype(
    const slang::ast::SubroutineSymbol& method, diag::SourceSpan span)
    -> diag::Result<hir::PublishedCallable> {
  auto published = PublishedMethodOf(method, span);
  if (!published) return std::unexpected(std::move(published.error()));
  if (published->has_value()) {
    return (*published)->prototype;
  }
  auto interface = MakeExternalCalleeInterface(method, span);
  if (!interface) return std::unexpected(std::move(interface.error()));
  auto result_type = InternType(method.getReturnType(), span);
  if (!result_type) return std::unexpected(std::move(result_type.error()));
  return hir::PublishedCallable{
      .name = std::string{method.name},
      .interface = *std::move(interface),
      .result_type = *result_type};
}

auto UnitLowerer::ReadAsLocalOrPublished(
    const slang::ast::Type& type, diag::SourceSpan span)
    -> diag::Result<LocalOrPublishedClass> {
  const auto& cls = type.getCanonicalType().as<slang::ast::ClassType>();
  auto ref = ResolveClassRef(cls, span);
  if (!ref) return std::unexpected(std::move(ref.error()));
  return std::visit(
      Overloaded{
          [&](const hir::LocalClassRef&) -> LocalOrPublishedClass {
            return &cls;
          },
          [](const hir::ExternalClassRef& ext) -> LocalOrPublishedClass {
            return ext;
          }},
      *ref);
}

auto UnitLowerer::ParentsOf(
    const LocalOrPublishedClass& cls, diag::SourceSpan span)
    -> diag::Result<ClassParents> {
  return std::visit(
      Overloaded{
          [&](const slang::ast::ClassType* local)
              -> diag::Result<ClassParents> {
            ClassParents parents;
            if (const auto* base = local->getBaseClass(); base != nullptr) {
              auto read = ReadAsLocalOrPublished(*base, span);
              if (!read) return std::unexpected(std::move(read.error()));
              parents.base = *std::move(read);
            }
            for (const auto* named : local->getDeclaredInterfaces()) {
              auto read = ReadAsLocalOrPublished(*named, span);
              if (!read) return std::unexpected(std::move(read.error()));
              parents.implements.push_back(*std::move(read));
            }
            return parents;
          },
          [&](const hir::ExternalClassRef& ext) -> diag::Result<ClassParents> {
            const hir::ExternalClass& published =
                ExternalClassOf(ext.unit_name, ext.class_path);
            ClassParents parents;
            if (published.base.has_value()) {
              parents.base = AsLocalOrPublished(*published.base);
            }
            for (const hir::ExternalClassRef& named : published.implements) {
              parents.implements.push_back(AsLocalOrPublished(named));
            }
            return parents;
          }},
      cls);
}

auto UnitLowerer::NameOf(const LocalOrPublishedClass& cls) const
    -> hir::ExternalClassRef {
  return std::visit(
      Overloaded{
          [&](const slang::ast::ClassType* local) {
            return hir::ExternalClassRef{
                .unit_name = unit_.name,
                .class_path = DefPathOf(*local, Specialization())};
          },
          [](const hir::ExternalClassRef& ext) { return ext; }},
      cls);
}

auto UnitLowerer::ReachInterface(
    const LocalOrPublishedClass& iface, diag::SourceSpan span,
    std::vector<LocalOrPublishedClass>& reached) -> diag::Result<void> {
  // By name, which is one whichever of the two ways the class was read.
  const hir::ExternalClassRef name = NameOf(iface);
  for (const LocalOrPublishedClass& held : reached) {
    if (NameOf(held) == name) return {};
  }
  reached.push_back(iface);
  auto parents = ParentsOf(iface, span);
  if (!parents) return std::unexpected(std::move(parents.error()));
  for (const LocalOrPublishedClass& extended : parents->implements) {
    if (auto more = ReachInterface(extended, span, reached); !more) {
      return std::unexpected(std::move(more.error()));
    }
  }
  return {};
}

auto UnitLowerer::AllInterfacesOf(
    const LocalOrPublishedClass& cls, diag::SourceSpan span)
    -> diag::Result<std::vector<LocalOrPublishedClass>> {
  auto parents = ParentsOf(cls, span);
  if (!parents) return std::unexpected(std::move(parents.error()));
  std::vector<LocalOrPublishedClass> interfaces;
  if (parents->base.has_value()) {
    auto inherited = AllInterfacesOf(*parents->base, span);
    if (!inherited) return std::unexpected(std::move(inherited.error()));
    interfaces = *std::move(inherited);
  }
  for (const LocalOrPublishedClass& named : parents->implements) {
    if (auto more = ReachInterface(named, span, interfaces); !more) {
      return std::unexpected(std::move(more.error()));
    }
  }
  return interfaces;
}

auto UnitLowerer::MethodNamesOf(const LocalOrPublishedClass& iface)
    -> std::vector<std::string> {
  return std::visit(
      Overloaded{
          [](const slang::ast::ClassType* local) {
            std::vector<std::string> names;
            for (const auto& proto :
                 local->membersOfType<slang::ast::MethodPrototypeSymbol>()) {
              if (proto.getParentScope() == local) {
                names.emplace_back(proto.name);
              }
            }
            return names;
          },
          [&](const hir::ExternalClassRef& ext) {
            std::vector<std::string> names;
            for (const hir::PublishedMethod& method :
                 ExternalClassOf(ext.unit_name, ext.class_path).methods) {
              names.push_back(method.prototype.name);
            }
            return names;
          }},
      iface);
}

auto UnitLowerer::StateConformance(
    const slang::ast::ClassType& cls, diag::SourceSpan span,
    hir::ClassDecl& decl) -> diag::Result<void> {
  // An interface class answers none of what it extends; a class answering it
  // states the answers.
  if (cls.isInterface) {
    return {};
  }
  const auto answer_of = [&](std::string_view name)
      -> diag::Result<std::optional<hir::VirtualBehavior>> {
    const slang::ast::SubroutineSymbol* method = ImplementationOf(cls, name);
    if (method == nullptr) return std::nullopt;
    auto answer = MakeVirtualBehavior(*method, span);
    if (!answer) return std::unexpected(std::move(answer.error()));
    return std::optional{*std::move(answer)};
  };
  auto interfaces = AllInterfacesOf(&cls, span);
  if (!interfaces) return std::unexpected(std::move(interfaces.error()));
  for (const LocalOrPublishedClass& iface : *interfaces) {
    auto stated = std::visit(
        Overloaded{
            [&](const slang::ast::ClassType* local) -> diag::Result<void> {
              for (const auto& member : local->members()) {
                if (member.kind != slang::ast::SymbolKind::MethodPrototype) {
                  continue;
                }
                const auto& proto =
                    member.as<slang::ast::MethodPrototypeSymbol>();
                const slang::ast::SubroutineSymbol* stub =
                    proto.getSubroutine();
                if (stub == nullptr) {
                  throw InternalError(
                      "UnitLowerer::StateConformance: interface pure virtual "
                      "prototype has no stub subroutine slang normally "
                      "materializes");
                }
                // A restatement of a behavior of an interface class this one
                // extends is that behavior, stated where it was introduced.
                if (OverriddenMethod(*local, *stub) != nullptr) {
                  continue;
                }
                auto behavior = MakeVirtualBehavior(*stub, span);
                if (!behavior) {
                  return std::unexpected(std::move(behavior.error()));
                }
                auto answered_by = answer_of(proto.name);
                if (!answered_by) {
                  return std::unexpected(std::move(answered_by.error()));
                }
                decl.conforming.emplace_back(
                    *std::move(behavior), *std::move(answered_by));
              }
              return {};
            },
            [&](const hir::ExternalClassRef& ext) -> diag::Result<void> {
              // The virtual methods it introduces, in the order that counts
              // their ordinals.
              hir::PublishedBehaviorId ordinal{0};
              for (const hir::PublishedMethod& method :
                   ExternalClassOf(ext.unit_name, ext.class_path).methods) {
                if (!std::holds_alternative<hir::IntroducesVirtual>(
                        method.dispatch)) {
                  continue;
                }
                auto answered_by = answer_of(method.prototype.name);
                if (!answered_by) {
                  return std::unexpected(std::move(answered_by.error()));
                }
                decl.conforming.emplace_back(
                    hir::ExternalDispatchSlot{
                        .unit_name = ext.unit_name,
                        .class_path = ext.class_path,
                        .behavior = ordinal},
                    *std::move(answered_by));
                ++ordinal.value;
              }
              return {};
            }},
        iface);
    if (!stated) return std::unexpected(std::move(stated.error()));
  }
  return {};
}

auto UnitLowerer::MakeVirtualBehavior(
    const slang::ast::Symbol& method, diag::SourceSpan span)
    -> diag::Result<hir::VirtualBehavior> {
  auto declaring = ResolveClassRef(OwningClassOf(method), span);
  if (!declaring) return std::unexpected(std::move(declaring.error()));
  return std::visit(
      Overloaded{
          [&](const hir::LocalClassRef& local)
              -> diag::Result<hir::VirtualBehavior> {
            return hir::LocalClassMethodTarget{
                .owner = local.class_id, .method = LookupMethodId(method)};
          },
          [&](const hir::ExternalClassRef& external)
              -> diag::Result<hir::VirtualBehavior> {
            return MakeVirtualBehavior(external, method.name, span);
          }},
      *declaring);
}

auto UnitLowerer::MakeVirtualBehavior(
    const hir::ExternalClassRef& cls, std::string_view method_name,
    diag::SourceSpan span) -> diag::Result<hir::VirtualBehavior> {
  if (std::optional<hir::VirtualBehavior> behavior =
          FindVirtualBehavior(cls, method_name)) {
    return *std::move(behavior);
  }
  return diag::Fail(
      span, diag::DiagCode::kUnsupportedExpressionForm,
      "no class reachable through '{}::{}' declares '{}', so there is no "
      "method to call",
      cls.unit_name, support::DisplayOf(cls.class_path), method_name);
}

auto UnitLowerer::FindVirtualBehavior(
    const hir::ExternalClassRef& cls, std::string_view method_name)
    -> std::optional<hir::VirtualBehavior> {
  // A virtual method is named by the class that introduced it, which is the
  // one identity every class overriding it agrees on -- so a class overriding
  // it is a step on the way, not the answer. The walk follows what each class
  // published about the class it extends, and reading those signatures is what
  // makes their units dependencies of this one. A lineage another unit
  // published may pass back through a class of this unit, which is read off
  // what this unit states it publishes; a behavior that class introduced is
  // named by this unit's identity for it.
  for (std::optional<hir::ExternalClassRef> at = cls; at.has_value();) {
    if (at->unit_name == unit_.name) {
      const OwnClass& own = OwnClassAt(at->class_path);
      const auto introduced = std::ranges::find_if(
          own.signature.methods, [&](const hir::PublishedMethod& method) {
            return method.prototype.name == method_name &&
                   std::holds_alternative<hir::IntroducesVirtual>(
                       method.dispatch);
          });
      if (introduced != own.signature.methods.end()) {
        return own.methods.at(
            static_cast<std::size_t>(
                introduced - own.signature.methods.begin()));
      }
      at = own.signature.base;
      continue;
    }
    const hir::ExternalClass& published =
        ExternalClassOf(at->unit_name, at->class_path);
    if (const std::optional<hir::PublishedBehaviorId> behavior =
            hir::FindIntroducedVirtual(published.methods, method_name)) {
      return hir::ExternalDispatchSlot{
          .unit_name = at->unit_name,
          .class_path = at->class_path,
          .behavior = *behavior};
    }
    at = published.base;
  }
  return std::nullopt;
}

auto UnitLowerer::OwnClassAt(const support::DefPath& class_path) const
    -> const OwnClass& {
  const auto it = own_classes_by_path_.find(class_path);
  if (it == own_classes_by_path_.end()) {
    throw InternalError(
        std::format(
            "UnitLowerer::OwnClassAt: this unit holds no class '{}' -- "
            "please report this as a bug",
            support::DisplayOf(class_path)));
  }
  return own_classes_.at(it->second);
}

auto UnitLowerer::AsLocalOrPublished(const hir::ExternalClassRef& named) const
    -> LocalOrPublishedClass {
  if (named.unit_name != unit_.name) return named;
  const auto it = own_classes_by_path_.find(named.class_path);
  if (it == own_classes_by_path_.end()) {
    throw InternalError(
        std::format(
            "UnitLowerer::AsLocalOrPublished: this unit holds no class '{}' -- "
            "please report this as a bug",
            support::DisplayOf(named.class_path)));
  }
  return it->second;
}

auto UnitLowerer::MakeExternalCalleeInterface(
    const slang::ast::SubroutineSymbol& sym, diag::SourceSpan span)
    -> diag::Result<hir::ExternalCalleeInterface> {
  std::vector<hir::ExternalCalleeParam> params;
  params.reserve(sym.getArguments().size());
  for (const auto* formal : sym.getArguments()) {
    auto formal_type = InternType(formal->getType(), span);
    if (!formal_type) return std::unexpected(std::move(formal_type.error()));
    params.push_back(
        hir::ExternalCalleeParam{
            .direction = ParamDirectionOf(*formal), .type = *formal_type});
  }
  return hir::ExternalCalleeInterface{
      .kind = ToHirSubroutineKind(sym.subroutineKind),
      .params = std::move(params)};
}

auto UnitLowerer::MakeClassPropertyTarget(
    const slang::ast::ClassType& owner,
    const slang::ast::ClassPropertySymbol& prop, diag::SourceSpan span)
    -> diag::Result<hir::ClassPropertyTarget> {
  if (const auto* held = InheritedCovergroupProperty(prop)) {
    return MakeClassPropertyTarget(
        held->getParentScope()->asSymbol().as<slang::ast::ClassType>(), *held,
        span);
  }
  auto owner_ref = ResolveClassRef(owner, span);
  if (!owner_ref) return std::unexpected(std::move(owner_ref.error()));
  const hir::ClassRef& class_ref = *owner_ref;
  if (const auto* local = std::get_if<hir::LocalClassRef>(&class_ref)) {
    return hir::LocalClassPropertyTarget{
        .owner = local->class_id, .field = LookupClassPropertyFieldId(prop)};
  }
  // `owner` is the class that declares the property, which the front end
  // settled when it resolved the name -- so the signature to read is that
  // class's and no lineage is climbed. Which storage the access reaches follows
  // from the class the access names rather than from what the value turns out
  // to be (LRM 8.14), and an ancestor answering to the same identifier is a
  // different property that an access naming this class must not reach. A class
  // publishes every property but a `local` one, which only the class's own
  // bodies name (LRM 8.18), and those compile in the unit declaring it.
  const auto& ext = std::get<hir::ExternalClassRef>(class_ref);
  const std::optional<hir::PublishedPropertyId> property = hir::FindProperty(
      ExternalClassOf(ext.unit_name, ext.class_path).properties, prop.name);
  if (!property.has_value()) {
    throw InternalError(
        std::format(
            "UnitLowerer::MakeClassPropertyTarget: '{}::{}' declares '{}' and "
            "published no such property",
            ext.unit_name, support::DisplayOf(ext.class_path), prop.name));
  }
  return hir::ExternalClassPropertyTarget{
      .unit_name = ext.unit_name,
      .class_path = ext.class_path,
      .property = *property};
}

auto UnitLowerer::ResolveClassRef(
    const slang::ast::ClassType& cls, diag::SourceSpan span)
    -> diag::Result<hir::ClassRef> {
  // A class `std` declares is the class Lyra's definition of it is, wherever
  // it is named.
  if (const slang::ast::ClassType* definition = facts_.StdDefinitionOf(cls)) {
    return ResolveClassRef(*definition, span);
  }
  // A class this unit has already classified reads its `ClassRef` straight
  // from the cache. Only the first reference to a class runs the boundary
  // walk that answers "which CU declares it?" and stores the result.
  if (const auto it = class_cache_.find(&cls); it != class_cache_.end()) {
    return it->second;
  }
  if (!Owns(cls)) {
    const auto [it, _] = class_cache_.emplace(
        &cls,
        hir::ClassRef{hir::ExternalClassRef{
            .unit_name = CompilationUnitName(UnitHomeOf(cls), Specialization()),
            .class_path = DefPathOf(cls, Specialization())}});
    return it->second;
  }
  // A local class not yet minted (e.g. a class nested inside a generate
  // block, which the top-level scope walk does not reach): mint it lazily
  // now, so a reference and the pre-pass both converge on the same identity
  // regardless of which route saw the class first.
  auto id_or = InternLocalClass(cls, span);
  if (!id_or) return std::unexpected(std::move(id_or.error()));
  return hir::ClassRef{hir::LocalClassRef{.class_id = *id_or}};
}

auto UnitLowerer::Defining(const slang::ast::ClassType& cls) const
    -> const slang::ast::ClassType* {
  const slang::ast::ClassType* definition = facts_.StdDefinitionOf(cls);
  return definition == nullptr ? &cls : definition;
}

auto UnitLowerer::Defining(const slang::ast::SubroutineSymbol& method) const
    -> const slang::ast::SubroutineSymbol* {
  const auto& declaring = OwningClassOf(method);
  const slang::ast::ClassType* definition = facts_.StdDefinitionOf(declaring);
  return definition == nullptr ? &method : &DefiningMethod(*definition, method);
}

auto UnitLowerer::InternLocalClass(
    const slang::ast::ClassType& cls, diag::SourceSpan span)
    -> diag::Result<hir::ClassId> {
  // Idempotent: a repeat call for the same class returns the id the first
  // mint installed. The cache entry is populated before body population, so a
  // cyclic reference during that population -- a class that names itself in
  // one of its member types, or a mutually-referential pair -- resolves
  // against a stable id already visible to peer lookups.
  if (const auto it = class_cache_.find(&cls); it != class_cache_.end()) {
    return std::get<hir::LocalClassRef>(it->second).class_id;
  }
  // A class is one declaration of its unit wherever its text was elaborated: a
  // class an instance declares is a type of that instance's own (LRM 8.25),
  // and each block of a loop builds its own copy, yet every such copy at one
  // path is the same text compiled once. So a copy at a path already declared
  // is that class, built again here so what it names is named alike.
  support::DefPath path = DefPathOf(cls, Specialization());
  const auto declared = own_classes_by_path_.find(path);
  const hir::ClassId id =
      declared == own_classes_by_path_.end()
          ? unit_.classes.Declare(std::move(path))
          : std::get<hir::LocalClassRef>(class_cache_.at(declared->second))
                .class_id;
  class_cache_.emplace(&cls, hir::ClassRef{hir::LocalClassRef{.class_id = id}});
  own_classes_by_path_.emplace(unit_.classes.PathOf(id), &cls);

  auto decl_owner = std::make_unique<hir::ClassDecl>();
  hir::ClassDecl& decl = *decl_owner;
  decl.is_interface_class = cls.isInterface;
  decl.takes_declaring_instance = BelongsToAnInstance(cls) && !cls.isInterface;
  decl.fixed_name = FixedNameOf(cls);

  // A class is the declaration scope that owns the lexical scopes of every
  // body it declares, so that ownership is stated once here and each method,
  // prototype, and property initializer below lowers under it.
  //
  // The frame descends from the structural scope that replicates the class,
  // because a class is a scope of the name tree (LRM 23.9) and its bodies name
  // what encloses it the same way a process of that scope does. That scope's
  // instance is what they name it against: each instance has a type of its own
  // (LRM 6.22), and a specialization on such a type is one too (LRM 8.25).
  const slang::ast::Scope& replicating = ReplicatingScope(cls);
  classes_by_scope_[&replicating].push_back(id);
  DeclareClassStatics(cls, replicating);
  const WalkFrame class_frame =
      WalkFrame{}
          .WithDeclaringScope(DeclaringScopeChain(replicating), &replicating)
          .WithProceduralScopeOwner(&cls, &decl.procedural_scopes);
  DeclareProceduralScopes(cls, cls, *this, decl.procedural_scopes);

  std::optional<hir::ExternalClassRef> published_base;

  // What this class publishes about another class is the pair naming it,
  // whichever unit declares it: a signature is read where no id of this unit
  // means anything, and a class of this unit is as much "somewhere else" to
  // that reader as any other.
  const auto as_published = [&](const hir::ClassRef& ref) {
    return std::visit(
        Overloaded{
            [&](const hir::LocalClassRef& local) {
              return hir::ExternalClassRef{
                  .unit_name = unit_.name,
                  .class_path = unit_.classes.PathOf(local.class_id)};
            },
            [](const hir::ExternalClassRef& ext) { return ext; }},
        ref);
  };
  // The class this one extends (LRM 8.13), which may live in another unit. An
  // interface class extends none and reaches its parents through `implements`
  // instead. Slang flattens inherited members into this class's member list,
  // so each member iteration below keeps only those declared here.
  if (const auto* base_type = cls.getBaseClass()) {
    const auto& base_class =
        base_type->getCanonicalType().as<slang::ast::ClassType>();
    auto base_ref = ResolveClassRef(base_class, span);
    if (!base_ref) return std::unexpected(std::move(base_ref.error()));
    published_base = as_published(*base_ref);
    decl.base = *std::move(base_ref);
  }

  // Interface class contracts (LRM 8.26.2). Slang's `getDeclaredInterfaces`
  // returns the source-declared list: for a regular class it is the
  // `implements` clause; for an interface class it is the `extends` clause
  // parents. Both source keywords land in the same field because at the
  // object model layer they name the same relation -- aggregate these
  // interface classes' pure virtual method contracts. What the base implements
  // is not repeated here: it is read off the base.
  for (const auto* iface_type : cls.getDeclaredInterfaces()) {
    const auto& iface_class =
        iface_type->getCanonicalType().as<slang::ast::ClassType>();
    auto iface_ref = ResolveClassRef(iface_class, span);
    if (!iface_ref) return std::unexpected(std::move(iface_ref.error()));
    decl.implements.push_back(*std::move(iface_ref));
  }
  std::vector<hir::ExternalClassRef> published_interfaces;
  published_interfaces.reserve(decl.implements.size());
  for (const hir::ClassRef& iface : decl.implements) {
    published_interfaces.push_back(as_published(iface));
  }
  std::vector<hir::PublishedProperty> published_statics;

  for (const auto& prop :
       cls.membersOfType<slang::ast::ClassPropertySymbol>()) {
    if (prop.getParentScope() != &cls) {
      continue;
    }
    // The variable of a covergroup extending a base's names the base's
    // instance, which it replaces (LRM 19.4.1), and so is no storage here.
    if (InheritedCovergroupProperty(prop) != nullptr) {
      continue;
    }
    auto prop_type = InternPropertyType(prop, span);
    if (!prop_type) return std::unexpected(std::move(prop_type.error()));
    auto random = RandomQualifierOf(prop.getType(), prop.randMode, span);
    if (!random) return std::unexpected(std::move(random.error()));
    // LRM 8.9 / 8.10 keyword position: `static <T> x` marks a type-associated
    // property, whose storage the type owns rather than each object of it.
    // How many such cells exist is a separate question, answered by how many
    // times the class declaration is replicated, and not one HIR settles.
    // Instance properties and static properties live in disjoint arenas because
    // their identity spaces do not overlap and each downstream reference form
    // names one or the other.
    if (prop.lifetime == slang::ast::VariableLifetime::Static) {
      const hir::StaticPropertyId static_id = decl.static_properties.Add(
          hir::ClassStaticProperty{
              .name = std::string(prop.name),
              .type = *prop_type,
              .random = *random});
      RegisterClassPropertyStaticId(prop, static_id);
      // A `local` one is named nowhere outside the class (LRM 8.18), so it is
      // published to nobody.
      if (prop.visibility != slang::ast::Visibility::Local) {
        published_statics.push_back(
            hir::PublishedProperty{
                .name = std::string(prop.name), .type = *prop_type});
      }
      continue;
    }
    const hir::FieldId field_id = decl.fields.Add(
        hir::ClassField{
            .name = std::string(prop.name),
            .type = *prop_type,
            .is_published = prop.visibility != slang::ast::Visibility::Local,
            .random = *random});
    RegisterClassPropertyFieldId(prop, field_id);
  }
  // Every callable the class body declares, resolved to the subroutine that
  // carries its declaration. SystemVerilog spells a definition two ways and
  // they differ only in where the body sits: written inline, the class scope
  // holds the subroutine; declared `extern`, the class scope holds a prototype
  // and the definition sits outside the class body (LRM 8.24). A pure virtual
  // prototype (LRM 8.21) is the one form with no definition behind it. The
  // constructor is taken out of both: it is a construction-protocol fact the
  // class holds beside its methods (LRM 8.7), not a slot in the method arena.
  std::vector<const slang::ast::SubroutineSymbol*> defined_methods;
  std::vector<const slang::ast::MethodPrototypeSymbol*> pure_prototypes;
  const slang::ast::SubroutineSymbol* constructor_sym = nullptr;
  for (const auto& method : cls.membersOfType<slang::ast::SubroutineSymbol>()) {
    if (method.getParentScope() != &cls) {
      continue;
    }
    // Every class carries methods the front end supplies itself, for its
    // generator and its constraints (LRM 18.6, 18.8, 18.9, 18.13.3 --
    // 18.13.5); none has a body to lower.
    if (method.flags.has(slang::ast::MethodFlags::BuiltIn)) {
      continue;
    }
    if (method.flags.has(slang::ast::MethodFlags::Constructor)) {
      constructor_sym = &method;
      continue;
    }
    defined_methods.push_back(&method);
  }
  for (const auto& proto :
       cls.membersOfType<slang::ast::MethodPrototypeSymbol>()) {
    if (proto.getParentScope() != &cls) {
      continue;
    }
    const auto* subroutine = proto.getSubroutine();
    if (subroutine == nullptr) {
      throw InternalError(
          "UnitLowerer::InternLocalClass: a class method prototype has no "
          "subroutine slang normally materializes");
    }
    if (proto.flags.has(slang::ast::MethodFlags::Constructor)) {
      constructor_sym = subroutine;
      continue;
    }
    if (proto.flags.has(slang::ast::MethodFlags::Pure)) {
      pure_prototypes.push_back(&proto);
      continue;
    }
    defined_methods.push_back(subroutine);
  }

  // Take an identity for every method before any body lowers, so a body may
  // name a peer whatever order the source declared them in (LRM 13.7) and so a
  // method whose definition sits outside the class body is still nameable from
  // inside it (LRM 8.24).
  for (const auto* method : defined_methods) {
    RegisterMethodId(*method, decl.methods.Declare());
  }
  for (const auto* proto : pure_prototypes) {
    RegisterMethodId(*proto->getSubroutine(), decl.methods.Declare());
  }
  // A constraint block is a member of the class as a method is (LRM 18.5),
  // overridden by a block of its name in a class extending this one (LRM
  // 18.5.2), so it takes a method's identity, after every method, and is
  // published as one.
  std::vector<const slang::ast::ConstraintBlockSymbol*> constraint_blocks;
  for (const auto& block :
       cls.membersOfType<slang::ast::ConstraintBlockSymbol>()) {
    if (block.getParentScope() != &cls) {
      continue;
    }
    constraint_blocks.push_back(&block);
    RegisterMethodId(block, decl.methods.Declare());
  }

  // What this class would publish to another unit, taken in the one order its
  // own arenas were just built in, so the signature and the class cannot
  // describe different positions. Which classes a unit publishes is a separate
  // question, settled where the signature is derived.
  hir::ClassSignature own_signature{
      .class_path = unit_.classes.PathOf(id),
      .base = published_base,
      .is_interface_class = decl.is_interface_class,
      .implements = std::move(published_interfaces),
      .properties = {},
      .local_property_types = {},
      .static_properties = std::move(published_statics),
      .constructor = std::nullopt,
      .methods = {},
      .takes_declaring_instance = decl.takes_declaring_instance};
  // A class that declares no `new` has the implicit one, which takes nothing
  // (LRM 8.7).
  if (!decl.is_interface_class) {
    hir::ExternalCalleeInterface entered{
        .kind = hir::SubroutineKind::kFunction, .params = {}};
    if (constructor_sym != nullptr) {
      auto declared = MakeExternalCalleeInterface(*constructor_sym, span);
      if (!declared) return std::unexpected(std::move(declared.error()));
      entered = *std::move(declared);
    }
    own_signature.constructor = std::move(entered);
  }
  for (const hir::FieldId id : decl.fields.Ids()) {
    const hir::ClassField& property = decl.fields.Get(id);
    if (!property.is_published) {
      own_signature.local_property_types.push_back(property.type);
      continue;
    }
    own_signature.properties.Add(
        hir::PublishedProperty{.name = property.name, .type = property.type});
  }
  const auto dispatch_of = [&](const slang::ast::SubroutineSymbol& method,
                               bool is_pure) -> hir::MethodDispatch {
    if (method.flags.has(slang::ast::MethodFlags::Static)) {
      return hir::TypeAssociated{};
    }
    if (!method.isVirtual()) {
      return hir::NotVirtual{};
    }
    if (OverriddenMethod(cls, method) == nullptr) {
      return hir::IntroducesVirtual{.is_pure = is_pure};
    }
    return hir::OverridesVirtual{.is_pure = is_pure};
  };
  std::vector<hir::VirtualBehavior> own_methods;
  const auto publish_method = [&](const slang::ast::SubroutineSymbol& method,
                                  bool is_pure) -> diag::Result<void> {
    auto interface = MakeExternalCalleeInterface(method, span);
    if (!interface) return std::unexpected(std::move(interface.error()));
    auto result_type = InternType(method.getReturnType(), span);
    if (!result_type) return std::unexpected(std::move(result_type.error()));
    own_signature.methods.push_back(
        hir::PublishedMethod{
            .prototype =
                hir::PublishedCallable{
                    .name = std::string{method.name},
                    .interface = *std::move(interface),
                    .result_type = *result_type},
            .dispatch = dispatch_of(method, is_pure)});
    own_methods.emplace_back(
        hir::LocalClassMethodTarget{
            .owner = id, .method = LookupMethodId(method)});
    return {};
  };
  for (const auto* method : defined_methods) {
    if (auto added = publish_method(*method, false); !added) {
      return std::unexpected(std::move(added.error()));
    }
  }
  for (const auto* proto : pure_prototypes) {
    if (auto added = publish_method(*proto->getSubroutine(), true); !added) {
      return std::unexpected(std::move(added.error()));
    }
  }
  auto void_type = InternType(cls.getCompilation().getVoidType(), span);
  if (!void_type) return std::unexpected(std::move(void_type.error()));
  for (const auto* block : constraint_blocks) {
    const bool is_pure =
        block->flags.has(slang::ast::ConstraintBlockFlags::Pure);
    const bool is_static =
        block->flags.has(slang::ast::ConstraintBlockFlags::Static);
    hir::MethodDispatch dispatch = hir::IntroducesVirtual{.is_pure = is_pure};
    if (OverriddenConstraintBlock(cls, block->name) != nullptr) {
      dispatch = hir::OverridesVirtual{.is_pure = is_pure};
    }
    own_signature.methods.push_back(
        hir::PublishedMethod{
            .prototype =
                hir::PublishedCallable{
                    .name = std::string{block->name},
                    .interface =
                        hir::ExternalCalleeInterface{
                            .kind = is_static
                                        ? hir::SubroutineKind::
                                              kStaticConstraintBlock
                                        : hir::SubroutineKind::kConstraintBlock,
                            .params = {}},
                    .result_type = *void_type},
            .dispatch = dispatch});
    own_methods.emplace_back(
        hir::LocalClassMethodTarget{
            .owner = id, .method = LookupMethodId(*block)});
  }
  // Each covergroup the class embeds is made by a method of the class that a
  // class extending it overrides where its covergroup extends this one (LRM
  // 19.4.1), after the methods the source declares.
  for (const auto& covergroup :
       cls.membersOfType<slang::ast::CovergroupType>()) {
    auto factory = PublishedFactoryOf(covergroup, span);
    if (!factory) return std::unexpected(std::move(factory.error()));
    own_signature.methods.push_back(*std::move(factory));
    auto embedded = InternLocalCovergroup(covergroup, span);
    if (!embedded) return std::unexpected(std::move(embedded.error()));
    decl.embedded_covergroups.push_back(*embedded);
    own_methods.emplace_back(
        hir::LocalCovergroupFactory{.covergroup = *embedded});
  }
  own_classes_.emplace(
      &cls, OwnClass{
                .signature = std::move(own_signature),
                .methods = std::move(own_methods)});

  pending_class_bodies_[&replicating].push_back(
      UnitLowerer::PendingClassBody{
          .cls = &cls,
          .id = id,
          .span = span,
          .replicating_scope = &replicating,
          .decl = std::move(decl_owner),
          .defined_methods = std::move(defined_methods),
          .pure_prototypes = std::move(pure_prototypes),
          .constraint_blocks = std::move(constraint_blocks),
          .constructor_sym = constructor_sym});
  return id;
}

auto UnitLowerer::PopulateClassBodiesReplicatedBy(
    const slang::ast::Scope& scope) -> diag::Result<void> {
  // A class body reaches the declarations of the scope that replicates it and
  // records its routes against that scope's frame, so it lowers while that
  // scope is being lowered -- with the same reach a process of the scope has,
  // and before the scope takes the routes recorded against it.
  const auto pending = pending_class_bodies_.find(&scope);
  if (pending == pending_class_bodies_.end()) return {};
  std::vector<PendingClassBody> batch = std::move(pending->second);
  pending_class_bodies_.erase(pending);
  for (PendingClassBody& body : batch) {
    if (auto r = PopulateClassBody(body); !r) {
      return std::unexpected(std::move(r.error()));
    }
  }
  return {};
}

void UnitLowerer::RequireEveryClassBodyLowered() const {
  std::string left;
  for (const auto& [scope, bodies] : pending_class_bodies_) {
    for (const PendingClassBody& body : bodies) {
      if (unit_.classes.IsDefined(body.id)) continue;
      left += std::format(
          "{}'{}' in '{}'", left.empty() ? "" : ", ",
          SpecializationName(*body.cls, Specialization()),
          scope->asSymbol().name);
    }
  }
  if (left.empty()) return;
  throw InternalError(
      std::format(
          "UnitLowerer::RequireEveryClassBodyLowered: every class this unit "
          "holds is replicated by a structural scope of it, and every scope "
          "lowered lowers the classes it replicates; left: {}",
          left));
}

void UnitLowerer::ReadSignaturesOfNamedClasses() {
  // By name, since the cache is keyed by address and walks in no fixed order.
  std::vector<hir::ExternalClassRef> named;
  for (const auto& [_, ref] : class_cache_) {
    if (const auto* ext = std::get_if<hir::ExternalClassRef>(&ref)) {
      named.push_back(*ext);
    }
  }
  std::ranges::sort(named, {}, [](const hir::ExternalClassRef& ref) {
    return std::tie(ref.unit_name, ref.class_path);
  });
  for (const hir::ExternalClassRef& ref : named) {
    ExternalClassOf(ref.unit_name, ref.class_path);
  }
}

auto UnitLowerer::PopulateClassBody(PendingClassBody& pending)
    -> diag::Result<void> {
  const slang::ast::ClassType& cls = *pending.cls;
  const diag::SourceSpan span = pending.span;
  hir::ClassDecl& decl = *pending.decl;
  const std::vector<const slang::ast::SubroutineSymbol*>& defined_methods =
      pending.defined_methods;
  const std::vector<const slang::ast::MethodPrototypeSymbol*>& pure_prototypes =
      pending.pure_prototypes;
  const slang::ast::SubroutineSymbol* constructor_sym = pending.constructor_sym;
  const WalkFrame class_frame =
      WalkFrame{}
          .WithDeclaringScope(
              DeclaringScopeChain(*pending.replicating_scope),
              pending.replicating_scope)
          .WithProceduralScopeOwner(&cls, &decl.procedural_scopes);

  for (const auto* method : defined_methods) {
    auto method_decl =
        LowerDefinedClassMethod(*this, cls, *method, class_frame, span);
    if (!method_decl) return std::unexpected(std::move(method_decl.error()));
    const hir::MethodId id = LookupMethodId(*method);
    decl.methods.Define(id, *std::move(method_decl));
    // What `randomize()` calls around itself on an object of the class (LRM
    // 18.6.2): the one the class declares, where it declares one.
    if (method->flags.has(slang::ast::MethodFlags::PrePostRandomize)) {
      (method->name == "pre_randomize" ? decl.pre_randomize
                                       : decl.post_randomize) = id;
    }
  }

  // A pure virtual method's signature occupies a class-owned method slot the
  // dispatch table introduces and every extended concrete class must fill, so
  // it enters the same arena that carries ordinary instance methods; the record
  // is marked a prototype so a backend renders a declaration rather than a
  // body.
  for (const auto* proto : pure_prototypes) {
    auto proto_decl = LowerMethodPrototypeDecl(*this, *proto, class_frame);
    if (!proto_decl) return std::unexpected(std::move(proto_decl.error()));
    // A middle abstract class re-declaring an ancestor's pure virtual method
    // as another `pure virtual` produces a prototype whose overrides link
    // slang exposes as a `Symbol*` (either the ancestor prototype's stub or
    // another prototype). Translate the reachable stub form to the HIR
    // identity registered when that ancestor was interned.
    if (const auto* overridden = proto->getOverride(); overridden != nullptr) {
      const slang::ast::SubroutineSymbol* overridden_sub =
          SubroutineOf(*overridden);
      if (overridden_sub != nullptr) {
        auto taken = MakeVirtualBehavior(*overridden_sub, span);
        if (!taken) return std::unexpected(std::move(taken.error()));
        proto_decl->overrides = *std::move(taken);
      }
    }
    decl.methods.Define(
        LookupMethodId(*proto->getSubroutine()), *std::move(proto_decl));
  }

  for (const auto* block : pending.constraint_blocks) {
    auto block_decl =
        LowerConstraintBlockDecl(*this, cls, *block, class_frame, span);
    if (!block_decl) return std::unexpected(std::move(block_decl.error()));
    decl.methods.Define(LookupMethodId(*block), *std::move(block_decl));
  }

  // The class's `new` (LRM 8.7), lowered once every method identity exists so
  // its body reaches them the same way any other body does.
  std::optional<hir::SubroutineDecl> user_constructor;
  // The arguments the base's construction is entered with, while they are
  // still being worked out: the constructor lowering answers them where the
  // source wrote the call, and the block below settles every other way they
  // arrive.
  std::optional<std::vector<hir::ExprId>> base_arguments;
  if (constructor_sym != nullptr) {
    // LRM 8.7 gives a constructor the argument conventions of any other
    // subroutine call, but a construction yields the object it built and so
    // carries nothing a value could travel back to the caller in.
    for (const auto* formal : constructor_sym->getArguments()) {
      if (formal->direction == slang::ast::ArgumentDirection::Out ||
          formal->direction == slang::ast::ArgumentDirection::InOut) {
        return diag::Fail(
            span, diag::DiagCode::kUnsupportedClassFeature,
            "a constructor with an 'output' or 'inout' argument is not yet "
            "supported");
      }
    }
    auto ctor_or = LowerConstructorDecl(
        *this, *constructor_sym, class_frame, cls.getBaseConstructorCall());
    if (!ctor_or) return std::unexpected(std::move(ctor_or.error()));
    user_constructor = std::move(ctor_or->constructor);
    base_arguments = std::move(ctor_or->base_arguments);
  }

  // Forward every interface pure virtual this class satisfies by inheritance
  // rather than a local definition (LRM 8.26.2); a contract a method of this
  // class satisfies is wired when that method is lowered.
  if (auto forwards = SynthesizeInterfaceForwardingMethods(
          *this, cls, span, decl, class_frame);
      !forwards) {
    return std::unexpected(std::move(forwards.error()));
  }
  if (auto conformed = StateConformance(cls, span, decl); !conformed) {
    return std::unexpected(std::move(conformed.error()));
  }

  hir::SubroutineDecl constructor =
      user_constructor.has_value()
          ? *std::move(user_constructor)
          : SynthesizeDefaultConstructor(
                unit_.builtins.void_type, span, class_frame);

  // A property initializer (LRM 8.7) runs during construction and may read
  // another property through the receiver, so it lowers on the procedural path
  // -- the one that resolves a property name to a receiver-relative reference
  // -- into the constructor body's expression arena that holds it. The class is
  // the containing symbol so any structural name in the initializer routes from
  // the class's position.
  ProcessLowerer init_lowerer(*this, cls);
  const WalkFrame init_frame =
      class_frame.WithProceduralBody(&constructor.body);

  // A class that extends another always enters its base's construction, so the
  // arguments that construction carries are settled here whether or not the
  // source wrote them: an explicit `super.new(...)`, the arguments on an
  // extends specifier, or the base constructor's own default values (LRM 8.7,
  // 8.17). The front end resolves the first two into one fully bound call and
  // answers nothing for the third, having first established that every formal
  // of the base constructor has a default -- so an answer of nothing is the
  // third case.
  const slang::ast::Expression* base_clause = nullptr;
  if (const auto* base_type = cls.getBaseClass(); base_type != nullptr) {
    if (!base_arguments.has_value()) {
      if (const auto* written = cls.getBaseConstructorCall()) {
        base_clause = written;
        std::vector<hir::ExprId> lowered;
        const auto actuals = BaseCallArguments(*written);
        lowered.reserve(actuals.size());
        for (const auto* actual : actuals) {
          auto arg_or = init_lowerer.LowerExpr(*actual, init_frame);
          if (!arg_or) return std::unexpected(std::move(arg_or.error()));
          lowered.push_back(constructor.body.exprs.Add(*std::move(arg_or)));
        }
        base_arguments = std::move(lowered);
      } else {
        // Each formal takes its default, as an argument a call leaves out
        // does (LRM 13.5.3).
        std::vector<hir::ExprId> defaults;
        if (const slang::ast::SubroutineSymbol* base_constructor =
                Defining(
                    base_type->getCanonicalType().as<slang::ast::ClassType>())
                    ->getConstructor()) {
          for (const auto* formal : base_constructor->getArguments()) {
            auto arg_or =
                init_lowerer.LowerExpr(*formal->getDefaultValue(), init_frame);
            if (!arg_or) return std::unexpected(std::move(arg_or.error()));
            defaults.push_back(constructor.body.exprs.Add(*std::move(arg_or)));
          }
        }
        base_arguments = std::move(defaults);
      }
    }
    // The base's instance is found from where this class is declared, the way
    // a `new` of the base written there would find it.
    auto base_instance = DeclaringInstanceFrom(
        base_type->getCanonicalType().as<slang::ast::ClassType>(), class_frame,
        span);
    if (!base_instance)
      return std::unexpected(std::move(base_instance.error()));
    decl.base_call = hir::BaseCall{
        .declaring_instance = *std::move(base_instance),
        .arguments = *std::move(base_arguments)};
  }
  // A static property initializer (LRM 8.9 / 10.5) runs once for the cell
  // rather than once per instance constructed, so its expression lands in the
  // arena the class keeps for that rather than the constructor body's. It
  // cannot read a per-instance formal or `self`, and the lowering routes that
  // no such receiver is in scope; a downstream initializer that names another
  // static property of the same class reads it as `Cls::other_prop` (a
  // `StaticPropertyRef`), never through the constructor's receiver.
  const WalkFrame static_init_frame =
      class_frame.WithProceduralBody(&decl.static_init);
  for (const auto& prop :
       cls.membersOfType<slang::ast::ClassPropertySymbol>()) {
    if (prop.getParentScope() != &cls) continue;
    const auto* init = prop.getInitializer();
    if (init == nullptr) continue;
    if (prop.lifetime == slang::ast::VariableLifetime::Static) {
      auto init_expr = init_lowerer.LowerExpr(*init, static_init_frame);
      if (!init_expr) return std::unexpected(std::move(init_expr.error()));
      decl.static_property_inits.push_back(
          hir::StaticPropertyInit{
              .target = LookupClassPropertyStaticId(prop),
              .value = decl.static_init.exprs.Add(*std::move(init_expr))});
      continue;
    }
    auto init_expr = init_lowerer.LowerExpr(*init, init_frame);
    if (!init_expr) return std::unexpected(std::move(init_expr.error()));
    decl.field_inits.push_back(
        hir::FieldInit{
            .target = LookupClassPropertyFieldId(prop),
            .value = constructor.body.exprs.Add(*std::move(init_expr))});
  }
  // The cells constructing an object of the class reads are what a `new` of it
  // reads (LRM 9.4.2), stated for whichever waited evaluation constructs one.
  constructor.reads = ReadsOfConstruction(
      init_lowerer, init_frame, cls, constructor_sym, base_clause);
  decl.constructor = std::move(constructor);

  // Where the compile checks itself, every application of the unit builds the
  // class at its path, each from its own copy of the text (LRM 8.25), and they
  // agree on everything the class holds for the same reason their scopes
  // agree: the lowering reads a value only through the unit's identity or as
  // one the object is built with.
  if (!unit_.classes.IsDefined(pending.id)) {
    unit_.classes.Define(pending.id, std::move(*pending.decl));
  } else if (!(unit_.classes.Get(pending.id) == *pending.decl)) {
    throw InternalError(
        std::format(
            "UnitLowerer::PopulateClassBody: two applications of unit '{}' "
            "build the class '{}' differently, so the lowering read a value "
            "the unit's identity does not hold -- please report this as a bug",
            unit_.name, support::DisplayOf(unit_.classes.PathOf(pending.id))));
  }
  return {};
}

auto UnitLowerer::AddComposedType(hir::Type type) const -> hir::TypeId {
  return unit_.types.Intern(std::move(type));
}

auto UnitLowerer::InternPropertyType(
    const slang::ast::ClassPropertySymbol& prop, diag::SourceSpan span)
    -> diag::Result<hir::TypeId> {
  auto declared = InternType(prop.getType(), span);
  if (!declared || !IsHeldWeakly(prop)) return declared;
  return AddComposedType(hir::Type{hir::WeakHandleType{.handle = *declared}});
}

auto UnitLowerer::InternType(
    const slang::ast::Type& type, diag::SourceSpan span)
    -> diag::Result<hir::TypeId> {
  const auto* canonical = &type.getCanonicalType();
  if (const auto it = type_cache_.find(canonical); it != type_cache_.end()) {
    return it->second;
  }
  auto type_or = TranslateType(*this, type, span);
  if (!type_or) return std::unexpected(std::move(type_or.error()));
  const hir::TypeId id = unit_.types.Intern(*std::move(type_or));
  type_cache_.emplace(canonical, id);
  return id;
}

namespace {

// Whether `expr` yields a value with no range of its own, which no select or
// key is ever read against: what LRM 11.4 calls an operator, a conversion (LRM
// 6.24), an assignment used as an expression, a part-select, a concatenation, a
// replication, a stream, a set membership test, a value range, a min:typ:max
// choice, what a system subroutine returns, and an unbased unsized literal,
// whose every bit is the one it spells at whatever width its context gives it
// (LRM 5.7.1).
auto HasNoRangeOfItsOwn(const slang::ast::Expression& expr) -> bool {
  switch (expr.kind) {
    case slang::ast::ExpressionKind::UnaryOp:
    case slang::ast::ExpressionKind::BinaryOp:
    case slang::ast::ExpressionKind::ConditionalOp:
    case slang::ast::ExpressionKind::Conversion:
    case slang::ast::ExpressionKind::Assignment:
    case slang::ast::ExpressionKind::RangeSelect:
    case slang::ast::ExpressionKind::Concatenation:
    case slang::ast::ExpressionKind::Replication:
    case slang::ast::ExpressionKind::Streaming:
    case slang::ast::ExpressionKind::Inside:
    case slang::ast::ExpressionKind::ValueRange:
    case slang::ast::ExpressionKind::MinTypMax:
    case slang::ast::ExpressionKind::UnbasedUnsizedIntegerLiteral:
      return true;
    case slang::ast::ExpressionKind::Call:
      return expr.as<slang::ast::CallExpression>().isSystemCall();
    case slang::ast::ExpressionKind::Invalid:
    case slang::ast::ExpressionKind::IntegerLiteral:
    case slang::ast::ExpressionKind::RealLiteral:
    case slang::ast::ExpressionKind::TimeLiteral:
    case slang::ast::ExpressionKind::NullLiteral:
    case slang::ast::ExpressionKind::UnboundedLiteral:
    case slang::ast::ExpressionKind::StringLiteral:
    case slang::ast::ExpressionKind::NamedValue:
    case slang::ast::ExpressionKind::HierarchicalValue:
    case slang::ast::ExpressionKind::ElementSelect:
    case slang::ast::ExpressionKind::MemberAccess:
    case slang::ast::ExpressionKind::DataType:
    case slang::ast::ExpressionKind::TypeReference:
    case slang::ast::ExpressionKind::ArbitrarySymbol:
    case slang::ast::ExpressionKind::LValueReference:
    case slang::ast::ExpressionKind::SimpleAssignmentPattern:
    case slang::ast::ExpressionKind::StructuredAssignmentPattern:
    case slang::ast::ExpressionKind::ReplicatedAssignmentPattern:
    case slang::ast::ExpressionKind::EmptyArgument:
    case slang::ast::ExpressionKind::Dist:
    case slang::ast::ExpressionKind::NewArray:
    case slang::ast::ExpressionKind::NewClass:
    case slang::ast::ExpressionKind::NewCovergroup:
    case slang::ast::ExpressionKind::CopyClass:
    case slang::ast::ExpressionKind::ClockingEvent:
    case slang::ast::ExpressionKind::AssertionInstance:
    case slang::ast::ExpressionKind::TaggedUnion:
      return false;
  }
  throw InternalError("HasNoRangeOfItsOwn: unknown slang ExpressionKind");
}

}  // namespace

auto UnitLowerer::InternExpressionType(
    const slang::ast::Expression& expr, diag::SourceSpan span)
    -> diag::Result<hir::TypeId> {
  return HasNoRangeOfItsOwn(expr) ? InternResultType(*expr.type, span)
                                  : InternType(*expr.type, span);
}

auto UnitLowerer::InternResultType(
    const slang::ast::Type& type, diag::SourceSpan span)
    -> diag::Result<hir::TypeId> {
  // The type the operand handed up is translated and not interned: its
  // outermost range is one a value outside the name may have placed, and a type
  // interned with it would make what the unit holds follow the instance it was
  // lowered from. Only that range is renumbered.
  const slang::ast::SymbolKind kind = type.getCanonicalType().kind;
  if (kind != slang::ast::SymbolKind::PackedArrayType &&
      kind != slang::ast::SymbolKind::FixedSizeUnpackedArrayType) {
    return InternType(type, span);
  }
  auto handed = TranslateType(*this, type, span);
  if (!handed) return std::unexpected(std::move(handed.error()));
  const auto from_zero = [](std::int64_t left, std::int64_t right,
                            std::uint64_t count) {
    const auto last = static_cast<std::int64_t>(count) - 1;
    return left >= right ? std::pair{last, std::int64_t{0}}
                         : std::pair{std::int64_t{0}, last};
  };
  if (const auto* packed = handed->As<hir::PackedArrayType>()) {
    const auto [left, right] = from_zero(
        packed->dim.left, packed->dim.right, packed->dim.ElementCount());
    hir::PackedArrayType numbered = *packed;
    numbered.dim = hir::PackedRange{.left = left, .right = right};
    return unit_.types.Intern(hir::Type{std::move(numbered)});
  }
  if (const auto* unpacked = handed->As<hir::UnpackedArrayType>()) {
    const auto [left, right] = from_zero(
        unpacked->dim.left, unpacked->dim.right, unpacked->dim.ElementCount());
    hir::UnpackedArrayType numbered = *unpacked;
    numbered.dim = hir::UnpackedRange{.left = left, .right = right};
    return unit_.types.Intern(hir::Type{std::move(numbered)});
  }
  throw InternalError(
      "UnitLowerer::InternResultType: an array type translated to something "
      "other than an array -- please report this as a bug");
}

}  // namespace lyra::lowering::ast_to_hir
