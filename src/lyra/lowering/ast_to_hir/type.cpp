#include "lyra/hir/type.hpp"

#include <algorithm>
#include <cstdint>
#include <format>
#include <optional>
#include <span>
#include <string>
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
#include "lyra/hir/subroutine_prototype.hpp"
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
                  UnitHomeOf(struct_type), unit_lowerer.Naming()),
              .path = DefPathOf(struct_type, unit_lowerer.Naming())},
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
                  UnitHomeOf(union_type), unit_lowerer.Naming()),
              .path = DefPathOf(union_type, unit_lowerer.Naming())},
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
                  UnitHomeOf(enum_type), unit_lowerer.Naming()),
              .path = DefPathOf(enum_type, unit_lowerer.Naming())},
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
      auto class_id = unit_lowerer.ResolveClass(class_type, decl_span);
      if (!class_id) return std::unexpected(std::move(class_id.error()));
      return hir::Type{hir::ClassHandleType{.class_id = *class_id}};
    }
    // A variable of a covergroup's type holds an instance of it (LRM 19.3).
    case slang::ast::SymbolKind::CovergroupType: {
      auto covergroup = unit_lowerer.ResolveCovergroup(
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
          .unit_name = unit_lowerer.Naming().NameOf(
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

// The method whose introduction the behavior `method` of `cls` overrides
// (LRM 8.20): the root of its override chain, or nothing where `method`
// introduces its own behavior.
auto IntroducingMethod(
    const slang::ast::ClassType& cls,
    const slang::ast::SubroutineSymbol& method)
    -> const slang::ast::SubroutineSymbol* {
  const slang::ast::SubroutineSymbol* introducing = nullptr;
  const slang::ast::ClassType* in = &cls;
  const slang::ast::SubroutineSymbol* at = &method;
  while (const slang::ast::SubroutineSymbol* overridden =
             OverriddenMethod(*in, *at)) {
    introducing = overridden;
    in = &OwningClassOf(*overridden);
    at = overridden;
  }
  return introducing;
}

// The member that introduced the behavior `member` is part of -- a method, or a
// constraint block, which is overridden as one (LRM 18.5.2): itself where it
// introduces one.
auto IntroducingOf(const slang::ast::Symbol& member)
    -> const slang::ast::Symbol& {
  if (const auto* block = member.as_if<slang::ast::ConstraintBlockSymbol>()) {
    return IntroducingConstraintBlock(*block);
  }
  const auto& method = member.as<slang::ast::SubroutineSymbol>();
  const slang::ast::SubroutineSymbol* introducing =
      IntroducingMethod(OwningClassOf(method), method);
  return introducing == nullptr ? method : *introducing;
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
  std::optional<hir::StmtId> root_stmt;
  if (!is_pure) {
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

  return hir::SubroutineDecl{
      .name = std::string{block.name},
      .kind = is_static ? hir::SubroutineKind::kStaticConstraintBlock
                        : hir::SubroutineKind::kConstraintBlock,
      .result_type = *void_type,
      .params = {},
      .result_var = std::nullopt,
      .body = std::move(body),
      .root_stmt = root_stmt,
      .reads = {}};
}

}  // namespace

auto UnitLowerer::MakeClassMethodTarget(
    hir::ClassId owner, const slang::ast::SubroutineSymbol& method)
    -> hir::ClassMethodTarget {
  return hir::ClassMethodTarget{
      .owner = owner, .method = MethodPositionOf(owner, method.name)};
}

// A member of a class is at the position its declaration gives it, found by
// its name, whichever unit declares the class: a class declares at most one
// member under a name.
auto UnitLowerer::MethodPositionOf(hir::ClassId cls, std::string_view name)
    -> hir::MethodId {
  return Positioned(
      cls, name, hir::FindMethod(DeclarationOf(cls).methods, name));
}

auto UnitLowerer::PropertyPositionOf(hir::ClassId cls, std::string_view name)
    -> hir::FieldId {
  return Positioned(
      cls, name, hir::FindProperty(DeclarationOf(cls).properties, name));
}

auto UnitLowerer::StaticPropertyPositionOf(
    hir::ClassId cls, std::string_view name) -> hir::StaticPropertyId {
  return Positioned(
      cls, name, hir::FindProperty(DeclarationOf(cls).static_properties, name));
}

auto UnitLowerer::MakeClassMethodTarget(
    const slang::ast::ClassType& owner,
    const slang::ast::SubroutineSymbol& method, diag::SourceSpan span)
    -> diag::Result<hir::ClassMethodTarget> {
  auto owner_id = ResolveClass(owner, span);
  if (!owner_id) return std::unexpected(std::move(owner_id.error()));
  return MakeClassMethodTarget(*owner_id, method);
}

auto UnitLowerer::DeclaredMethodOf(
    const slang::ast::SubroutineSymbol& method, diag::SourceSpan span)
    -> diag::Result<std::optional<hir::DeclaredMethod>> {
  auto declaring = ResolveClass(OwningClassOf(method), span);
  if (!declaring) return std::unexpected(std::move(declaring.error()));
  const hir::DeclaredMethods& methods = DeclarationOf(*declaring).methods;
  if (const std::optional<hir::MethodId> declared =
          hir::FindMethod(methods, method.name)) {
    return std::optional{methods.Get(*declared)};
  }
  return std::nullopt;
}

auto UnitLowerer::IsTypeAssociatedMethod(
    const slang::ast::SubroutineSymbol& method, diag::SourceSpan span)
    -> diag::Result<bool> {
  auto published = DeclaredMethodOf(method, span);
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

auto UnitLowerer::ResolveClassType(
    const slang::ast::Type& type, diag::SourceSpan span)
    -> diag::Result<hir::ClassId> {
  return ResolveClass(
      type.getCanonicalType().as<slang::ast::ClassType>(), span);
}

auto UnitLowerer::ParentsOf(hir::ClassId cls) -> ClassParents {
  const hir::ClassDeclaration& declared = DeclarationOf(cls);
  return ClassParents{.base = declared.base, .implements = declared.implements};
}

void UnitLowerer::ReachInterface(
    hir::ClassId iface, std::vector<hir::ClassId>& reached) {
  if (std::ranges::find(reached, iface) != reached.end()) return;
  reached.push_back(iface);
  for (const hir::ClassId extended : ParentsOf(iface).implements) {
    ReachInterface(extended, reached);
  }
}

auto UnitLowerer::AllInterfacesOf(hir::ClassId cls)
    -> std::vector<hir::ClassId> {
  const ClassParents parents = ParentsOf(cls);
  std::vector<hir::ClassId> interfaces;
  if (parents.base.has_value()) {
    interfaces = AllInterfacesOf(*parents.base);
  }
  for (const hir::ClassId named : parents.implements) {
    ReachInterface(named, interfaces);
  }
  return interfaces;
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
      -> diag::Result<std::optional<hir::ClassMethodTarget>> {
    const slang::ast::SubroutineSymbol* method = ImplementationOf(cls, name);
    if (method == nullptr) return std::nullopt;
    auto answer = IntroducingMethodTarget(*method, span);
    if (!answer) return std::unexpected(std::move(answer.error()));
    return std::optional{*std::move(answer)};
  };
  auto self = ResolveClass(cls, span);
  if (!self) return std::unexpected(std::move(self.error()));
  // Every behavior an interface class introduces is stated where it was
  // introduced, so a restatement of one an interface class it extends
  // introduced is not stated again.
  for (const hir::ClassId iface : AllInterfacesOf(*self)) {
    const hir::DeclaredMethods& methods = DeclarationOf(iface).methods;
    for (const hir::MethodId method : methods.Ids()) {
      const hir::DeclaredMethod& declared = methods.Get(method);
      if (!std::holds_alternative<hir::IntroducesVirtual>(declared.dispatch)) {
        continue;
      }
      auto answered_by = answer_of(declared.prototype.name);
      if (!answered_by) return std::unexpected(std::move(answered_by.error()));
      decl.conforming.emplace_back(
          hir::ClassMethodTarget{.owner = iface, .method = method},
          *std::move(answered_by));
    }
  }
  return {};
}

auto UnitLowerer::IntroducingMethodTarget(
    const slang::ast::Symbol& method, diag::SourceSpan span)
    -> diag::Result<hir::ClassMethodTarget> {
  // A behavior is named by the member that introduced it, which the front end
  // finds along the lineage; the class introducing it may be another unit's,
  // or this unit's own, which a lineage another unit published passes back
  // through.
  const slang::ast::Symbol& introducing = IntroducingOf(method);
  auto introducer = ResolveClass(OwningClassOf(introducing), span);
  if (!introducer) return std::unexpected(std::move(introducer.error()));
  return hir::ClassMethodTarget{
      .owner = *introducer,
      .method = MethodPositionOf(*introducer, introducing.name)};
}

auto UnitLowerer::MakeCalleeInterface(
    const slang::ast::SubroutineSymbol& sym, diag::SourceSpan span)
    -> diag::Result<hir::CalleeInterface> {
  std::vector<hir::CalleeParam> params;
  params.reserve(sym.getArguments().size());
  for (const auto* formal : sym.getArguments()) {
    auto formal_type = InternType(formal->getType(), span);
    if (!formal_type) return std::unexpected(std::move(formal_type.error()));
    params.push_back(
        hir::CalleeParam{
            .direction = ParamDirectionOf(*formal),
            .type = *formal_type,
            .default_evaluator = formal->getDefaultValue() == nullptr ||
                                         !EvaluatesItsOwnDefaults(sym)
                                     ? std::nullopt
                                     : std::optional{ArgumentDefaultName(
                                           sym.name, formal->name)}});
  }
  return hir::CalleeInterface{
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
  auto owner_id = ResolveClass(owner, span);
  if (!owner_id) return std::unexpected(std::move(owner_id.error()));
  // `owner` is the class that declares the property, which the front end
  // settled when it resolved the name -- so the declaration to read is that
  // class's and no lineage is climbed. Which storage the access reaches follows
  // from the class the access names rather than from what the value turns out
  // to be (LRM 8.14), and an ancestor answering to the same identifier is a
  // different property that an access naming this class must not reach.
  return hir::ClassPropertyTarget{
      .owner = *owner_id, .field = PropertyPositionOf(*owner_id, prop.name)};
}

auto UnitLowerer::ResolveClass(
    const slang::ast::ClassType& cls, diag::SourceSpan span)
    -> diag::Result<hir::ClassId> {
  // A class `std` declares is the class Lyra's definition of it is, wherever
  // it is named.
  if (const slang::ast::ClassType* definition = facts_.StdDefinitionOf(cls)) {
    return ResolveClass(*definition, span);
  }
  // A class this unit has already classified reads its identity straight from
  // the cache. Only the first reference to a class runs the boundary walk that
  // answers "which CU declares it?" and stores the result.
  if (const auto it = class_cache_.find(&cls); it != class_cache_.end()) {
    return it->second;
  }
  if (!Owns(cls)) {
    const hir::ClassId id = unit_.classes.Import(
        hir::ClassKey{
            .unit_name = CompilationUnitName(UnitHomeOf(cls), Naming()),
            .class_path = DefPathOf(cls, Naming())});
    class_cache_.emplace(&cls, id);
    return id;
  }
  // A class of this unit not yet minted (e.g. a class nested inside a generate
  // block, which the top-level scope walk does not reach): mint it lazily
  // now, so a reference and the pre-pass both converge on the same identity
  // regardless of which route saw the class first.
  return InternOwnClass(cls, span);
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

auto UnitLowerer::InternOwnClass(
    const slang::ast::ClassType& cls, diag::SourceSpan span)
    -> diag::Result<hir::ClassId> {
  // Idempotent: a repeat call for the same class returns the id the first
  // mint installed. The cache entry is populated before body population, so a
  // cyclic reference during that population -- a class that names itself in
  // one of its member types, or a mutually-referential pair -- resolves
  // against a stable id already visible to peer lookups.
  if (const auto it = class_cache_.find(&cls); it != class_cache_.end()) {
    return it->second;
  }
  // A class is one declaration of its unit wherever its text was elaborated: a
  // class an instance declares is a type of that instance's own (LRM 8.25),
  // and each block of a loop builds its own copy, yet every such copy at one
  // path is the same text compiled once. So a copy at a path already declared
  // is that class, built again here so what it names is named alike.
  const hir::ClassId id = unit_.classes.DeclareOwn(DefPathOf(cls, Naming()));
  class_cache_.emplace(&cls, id);

  auto decl_owner = std::make_unique<hir::ClassDecl>();
  hir::ClassDecl& decl = *decl_owner;
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

  // The class this one extends (LRM 8.13), which may live in another unit. An
  // interface class extends none and reaches its parents through `implements`
  // instead. Slang flattens inherited members into this class's member list,
  // so each member iteration below keeps only those declared here.
  std::optional<hir::ClassId> base;
  if (const auto* base_type = cls.getBaseClass()) {
    const auto& base_class =
        base_type->getCanonicalType().as<slang::ast::ClassType>();
    auto base_id = ResolveClass(base_class, span);
    if (!base_id) return std::unexpected(std::move(base_id.error()));
    base = *base_id;
  }

  // Interface class contracts (LRM 8.26.2). Slang's `getDeclaredInterfaces`
  // returns the source-declared list: for a regular class it is the
  // `implements` clause; for an interface class it is the `extends` clause
  // parents. Both source keywords land in the same field because at the
  // object model layer they name the same relation -- aggregate these
  // interface classes' pure virtual method contracts. What the base implements
  // is not repeated here: it is read off the base.
  std::vector<hir::ClassId> implements;
  for (const auto* iface_type : cls.getDeclaredInterfaces()) {
    const auto& iface_class =
        iface_type->getCanonicalType().as<slang::ast::ClassType>();
    auto iface_id = ResolveClass(iface_class, span);
    if (!iface_id) return std::unexpected(std::move(iface_id.error()));
    implements.push_back(*iface_id);
  }
  hir::DeclaredProperties declared_properties;
  hir::DeclaredStaticProperties declared_statics;

  // A property is numbered where the class places it: the ones another unit
  // may name first, in declaration order, then the `local` ones (LRM 8.18), so
  // the class and every unit reading its declaration count one position for
  // it, and adding a `local` property moves none a referrer counted. Instance
  // and static properties alike.
  std::vector<const slang::ast::ClassPropertySymbol*> properties;
  for (const bool published : {true, false}) {
    for (const auto& prop :
         cls.membersOfType<slang::ast::ClassPropertySymbol>()) {
      if (prop.getParentScope() == &cls &&
          (prop.visibility != slang::ast::Visibility::Local) == published) {
        properties.push_back(&prop);
      }
    }
  }
  for (const slang::ast::ClassPropertySymbol* property : properties) {
    const slang::ast::ClassPropertySymbol& prop = *property;
    // The variable of a covergroup extending a base's names the base's
    // instance, which it replaces (LRM 19.4.1), and so is no storage here.
    if (InheritedCovergroupProperty(prop) != nullptr) {
      continue;
    }
    auto prop_type = InternPropertyType(prop, span);
    if (!prop_type) return std::unexpected(std::move(prop_type.error()));
    auto random = RandomQualifierOf(prop.getType(), prop.randMode, span);
    if (!random) return std::unexpected(std::move(random.error()));
    hir::DeclaredProperty declared{
        .name = std::string(prop.name), .type = *prop_type, .random = *random};
    // LRM 8.9 / 8.10 keyword position: `static <T> x` marks a type-associated
    // property, whose storage the type owns rather than each object of it.
    // How many such cells exist is a separate question, answered by how many
    // times the class declaration is replicated, and not one HIR settles.
    // Instance properties and static properties live in disjoint arenas because
    // their identity spaces do not overlap and each downstream reference form
    // names one or the other.
    if (prop.lifetime == slang::ast::VariableLifetime::Static) {
      declared_statics.Add(std::move(declared));
      continue;
    }
    declared_properties.Add(std::move(declared));
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
          "UnitLowerer::InternOwnClass: a class method prototype has no "
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

  // A formal's default is evaluated in the class each time a call omits it
  // (LRM 13.5.3), whatever object or scope the call is made from, so each is a
  // method of the class a caller asks for. The call binds to the method the
  // front end resolved, so the default is that method's and is not dispatched.
  std::vector<std::pair<
      const slang::ast::SubroutineSymbol*,
      const slang::ast::FormalArgumentSymbol*>>
      defaulted_formals;
  const auto take_defaults = [&](const slang::ast::SubroutineSymbol& method) {
    if (!EvaluatesItsOwnDefaults(method)) return;
    for (const slang::ast::FormalArgumentSymbol* formal :
         method.getArguments()) {
      if (formal->getDefaultValue() != nullptr) {
        defaulted_formals.emplace_back(&method, formal);
      }
    }
  };
  for (const auto* method : defined_methods) take_defaults(*method);
  for (const auto* proto : pure_prototypes) {
    take_defaults(*proto->getSubroutine());
  }

  // Take an identity for every method before any body lowers, so a body may
  // name a peer whatever order the source declared them in (LRM 13.7) and so a
  // method whose definition sits outside the class body is still nameable from
  // inside it (LRM 8.24).
  for (std::size_t i = 0; i < defined_methods.size() + pure_prototypes.size() +
                                  defaulted_formals.size();
       ++i) {
    decl.methods.Declare();
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
    decl.methods.Declare();
  }

  // What this class declares, as every unit reads it, taken in the one order
  // its own arenas were just built in, so the signature and the class cannot
  // describe different positions. It names classes and types as this unit
  // does; publishing it renames both to the signature's own. Which classes a
  // unit publishes is a separate question, settled where the signature is
  // derived.
  hir::ClassDeclaration declaration{
      .class_path = unit_.classes.PathOf(id),
      .base = base,
      .is_interface_class = cls.isInterface,
      .implements = std::move(implements),
      .properties = std::move(declared_properties),
      .static_properties = std::move(declared_statics),
      .constructor = std::nullopt,
      .methods = {},
      .takes_declaring_instance = BelongsToAnInstance(cls) && !cls.isInterface};
  // A class that declares no `new` has the implicit one, which takes nothing
  // (LRM 8.7).
  if (!cls.isInterface) {
    hir::CalleeInterface entered{
        .kind = hir::SubroutineKind::kFunction, .params = {}};
    if (constructor_sym != nullptr) {
      auto declared = MakeCalleeInterface(*constructor_sym, span);
      if (!declared) return std::unexpected(std::move(declared.error()));
      entered = *std::move(declared);
    }
    declaration.constructor = std::move(entered);
  }
  // Which class introduced the behavior an overriding member overrides is
  // found here, once, along this class's own lineage as the front end resolved
  // it, and published; a unit reading the member reads the answer.
  const auto overriding =
      [&](const slang::ast::Symbol& introducing,
          bool is_pure) -> diag::Result<hir::MethodDispatch> {
    auto introducer = ResolveClass(OwningClassOf(introducing), span);
    if (!introducer) return std::unexpected(std::move(introducer.error()));
    return hir::OverridesVirtual{
        .is_pure = is_pure,
        .introducer = *introducer,
        .name = std::string{introducing.name}};
  };
  const auto dispatch_of =
      [&](const slang::ast::SubroutineSymbol& method,
          bool is_pure) -> diag::Result<hir::MethodDispatch> {
    if (method.flags.has(slang::ast::MethodFlags::Static)) {
      return hir::TypeAssociated{};
    }
    // A method overriding another is virtual whether or not it repeats the
    // keyword (LRM 8.20), so the override is asked first.
    if (const slang::ast::SubroutineSymbol* introducing =
            IntroducingMethod(cls, method)) {
      return overriding(*introducing, is_pure);
    }
    if (method.isVirtual()) {
      return hir::IntroducesVirtual{.is_pure = is_pure};
    }
    return hir::NotVirtual{};
  };
  // What `randomize()` calls in place of the hook a method the class declares
  // is named for, on an object of the class (LRM 18.6.2).
  const auto hook_replaced = [](const slang::ast::SubroutineSymbol& method)
      -> std::optional<hir::RandomizationHook> {
    if (!method.flags.has(slang::ast::MethodFlags::PrePostRandomize)) {
      return std::nullopt;
    }
    return LowerRandomizationHookName(method.name);
  };
  const auto publish_method = [&](const slang::ast::SubroutineSymbol& method,
                                  bool is_pure) -> diag::Result<void> {
    auto interface = MakeCalleeInterface(method, span);
    if (!interface) return std::unexpected(std::move(interface.error()));
    auto result_type = InternType(method.getReturnType(), span);
    if (!result_type) return std::unexpected(std::move(result_type.error()));
    auto dispatch = dispatch_of(method, is_pure);
    if (!dispatch) return std::unexpected(std::move(dispatch.error()));
    declaration.methods.Add(
        hir::DeclaredMethod{
            .prototype =
                hir::SubroutinePrototype{
                    .name = std::string{method.name},
                    .interface = *std::move(interface),
                    .result_type = *result_type},
            .dispatch = *dispatch,
            .replaces = is_pure ? std::nullopt : hook_replaced(method),
            .makes = std::nullopt});
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
  for (const auto& [method, formal] : defaulted_formals) {
    auto result_type = InternType(formal->getType(), span);
    if (!result_type) return std::unexpected(std::move(result_type.error()));
    declaration.methods.Add(
        hir::DeclaredMethod{
            .prototype =
                hir::SubroutinePrototype{
                    .name = ArgumentDefaultName(method->name, formal->name),
                    .interface =
                        hir::CalleeInterface{
                            .kind = hir::SubroutineKind::kFunction,
                            .params = {}},
                    .result_type = *result_type},
            .dispatch = method->flags.has(slang::ast::MethodFlags::Static)
                            ? hir::MethodDispatch{hir::TypeAssociated{}}
                            : hir::MethodDispatch{hir::NotVirtual{}},
            .replaces = std::nullopt,
            .makes = std::nullopt});
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
      auto overrides = overriding(IntroducingConstraintBlock(*block), is_pure);
      if (!overrides) return std::unexpected(std::move(overrides.error()));
      dispatch = *overrides;
    }
    declaration.methods.Add(
        hir::DeclaredMethod{
            .prototype =
                hir::SubroutinePrototype{
                    .name = std::string{block->name},
                    .interface =
                        hir::CalleeInterface{
                            .kind = is_static
                                        ? hir::SubroutineKind::
                                              kStaticConstraintBlock
                                        : hir::SubroutineKind::kConstraintBlock,
                            .params = {}},
                    .result_type = *void_type},
            .dispatch = dispatch,
            .replaces = std::nullopt,
            .makes = std::nullopt});
  }
  // Each covergroup the class embeds is made by a method of the class that a
  // class extending it overrides where its covergroup extends this one (LRM
  // 19.4.1), after the methods the source declares.
  for (const auto& covergroup :
       cls.membersOfType<slang::ast::CovergroupType>()) {
    auto factory = PublishedFactoryOf(covergroup, span);
    if (!factory) return std::unexpected(std::move(factory.error()));
    declaration.methods.Add(*std::move(factory));
  }
  // A copy at a path already declared declares the same class, which its first
  // copy has already stated, as its body is defined once below.
  if (!unit_.classes.HasDeclaration(id)) {
    unit_.classes.SetDeclaration(id, std::move(declaration));
  } else if (!(unit_.classes.Declaration(id) == declaration)) {
    throw InternalError(
        std::format(
            "UnitLowerer::InternOwnClass: two copies of class '{}' of unit "
            "'{}' declare it differently -- please report this as a bug",
            support::DisplayOf(unit_.classes.PathOf(id)), unit_.name));
  }

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
          .defaulted_formals = std::move(defaulted_formals),
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
      if (unit_.classes.HasBodies(body.id)) continue;
      left += std::format(
          "{}'{}' in '{}'", left.empty() ? "" : ", ",
          UnitName(*body.cls, Naming()), scope->asSymbol().name);
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

void UnitLowerer::ReadDeclarationLater(hir::ClassId cls) {
  if (!unit_.classes.IsOwn(cls)) {
    pending_declaration_reads_.push_back(cls);
  }
}

void UnitLowerer::ReadPendingDeclarations() {
  for (const hir::ClassId cls : pending_declaration_reads_) {
    DeclarationOf(cls);
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
    auto method_decl = LowerSubroutineDecl(*this, *method, class_frame);
    if (!method_decl) return std::unexpected(std::move(method_decl.error()));
    decl.methods.Define(
        MethodPositionOf(pending.id, method->name), *std::move(method_decl));
  }

  // A pure virtual method (LRM 8.21) is a member of the class like any other,
  // so it enters the same arena, with no statement to enter, so a backend
  // renders a declaration rather than a body.
  for (const auto* proto : pure_prototypes) {
    auto proto_decl = LowerMethodPrototypeDecl(*this, *proto, class_frame);
    if (!proto_decl) return std::unexpected(std::move(proto_decl.error()));
    decl.methods.Define(
        MethodPositionOf(pending.id, proto->name), *std::move(proto_decl));
  }

  for (const auto& [method, formal] : pending.defaulted_formals) {
    std::string name = ArgumentDefaultName(method->name, formal->name);
    const hir::MethodId position = MethodPositionOf(pending.id, name);
    auto evaluator = LowerEvaluatorDecl(
        *this, *formal, std::move(name), *formal->getDefaultValue(),
        class_frame);
    if (!evaluator) return std::unexpected(std::move(evaluator.error()));
    decl.methods.Define(position, *std::move(evaluator));
  }

  for (const auto* block : pending.constraint_blocks) {
    auto block_decl =
        LowerConstraintBlockDecl(*this, cls, *block, class_frame, span);
    if (!block_decl) return std::unexpected(std::move(block_decl.error()));
    decl.methods.Define(
        MethodPositionOf(pending.id, block->name), *std::move(block_decl));
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
              .target = StaticPropertyPositionOf(pending.id, prop.name),
              .value = decl.static_init.exprs.Add(*std::move(init_expr))});
      continue;
    }
    auto init_expr = init_lowerer.LowerExpr(*init, init_frame);
    if (!init_expr) return std::unexpected(std::move(init_expr.error()));
    decl.field_inits.push_back(
        hir::FieldInit{
            .target = PropertyPositionOf(pending.id, prop.name),
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
  if (!unit_.classes.HasBodies(pending.id)) {
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
