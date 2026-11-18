#include "lyra/lowering/ast_to_hir/typespec.hpp"

#include <cstdint>
#include <expected>
#include <optional>
#include <span>
#include <string>
#include <unordered_map>
#include <utility>
#include <vector>

#include <slang/ast/symbols/MemberSymbols.h>
#include <slang/ast/types/AllTypes.h>
#include <slang/ast/types/Type.h>

#include "lyra/diag/diagnostic.hpp"
#include "lyra/diag/source_span.hpp"
#include "lyra/hir/typespec.hpp"
#include "lyra/lowering/ast_to_hir/unit_lowerer.hpp"
#include "lyra/support/data_object.hpp"

namespace lyra::lowering::ast_to_hir {

namespace {

using MaybeTypespec = diag::Result<std::optional<hir::TypespecId>>;

auto RangeOf(const slang::ConstantRange& range) -> support::Range {
  return support::Range{.left = range.left, .right = range.right};
}

// The keyword a predefined integer type is written with (LRM 6.11).
auto IntegerKeywordOf(const slang::ast::PredefinedIntegerType& type)
    -> hir::IntegerKeyword {
  switch (type.integerKind) {
    case slang::ast::PredefinedIntegerType::Integer:
      return hir::IntegerKeyword::kInteger;
    case slang::ast::PredefinedIntegerType::Int:
      return hir::IntegerKeyword::kInt;
    case slang::ast::PredefinedIntegerType::Time:
      return hir::IntegerKeyword::kTime;
    case slang::ast::PredefinedIntegerType::ShortInt:
      return hir::IntegerKeyword::kShortInt;
    case slang::ast::PredefinedIntegerType::LongInt:
      return hir::IntegerKeyword::kLongInt;
    case slang::ast::PredefinedIntegerType::Byte:
      return hir::IntegerKeyword::kByte;
  }
  std::unreachable();
}

// The keyword a single bit is written with (LRM 6.11).
auto BitKeywordOf(const slang::ast::ScalarType& type) -> hir::BitKeyword {
  switch (type.scalarKind) {
    case slang::ast::ScalarType::Bit:
      return hir::BitKeyword::kBit;
    case slang::ast::ScalarType::Logic:
    case slang::ast::ScalarType::Reg:
      return hir::BitKeyword::kLogic;
  }
  std::unreachable();
}

// Whether `type` is written as a name -- a typedef's, or a predefined type's
// keyword -- rather than built where it is written, which is what a typedef
// of it reports as the type it aliases (LRM 37.25 details 1, 6).
auto IsNamedType(const slang::ast::Type& type) -> bool {
  return type.isAlias() ||
         type.as_if<slang::ast::PredefinedIntegerType>() != nullptr ||
         type.as_if<slang::ast::ScalarType>() != nullptr ||
         type.as_if<slang::ast::FloatingType>() != nullptr || type.isString() ||
         type.isCHandle() || type.isEvent();
}

auto PlainKindOf(const slang::ast::Type& type)
    -> std::optional<hir::PlainKind> {
  if (type.isEvent()) return hir::PlainKind::kEvent;
  if (type.isString()) return hir::PlainKind::kString;
  if (type.isCHandle()) return hir::PlainKind::kChandle;
  if (type.isVirtualInterface()) return hir::PlainKind::kVirtualInterface;
  if (const auto* floating = type.as_if<slang::ast::FloatingType>()) {
    switch (floating->floatKind) {
      case slang::ast::FloatingType::Real:
        return hir::PlainKind::kReal;
      case slang::ast::FloatingType::ShortReal:
        return hir::PlainKind::kShortReal;
      case slang::ast::FloatingType::RealTime:
        return hir::PlainKind::kRealtime;
    }
  }
  return std::nullopt;
}

class TypespecBuilder {
 public:
  TypespecBuilder(UnitLowerer& owner, diag::SourceSpan span)
      : owner_(&owner), span_(span) {
  }

  // A type reached again -- members of one type -- is the typespec it was the
  // first time, so building one costs the type's edges, never its parts'
  // number.
  auto Of(const slang::ast::Type& declared) -> MaybeTypespec {
    if (const auto found = built_.find(&declared); found != built_.end()) {
      return found->second;
    }
    auto built = Build(declared);
    if (built) built_.emplace(&declared, *built);
    return built;
  }

 private:
  auto Build(const slang::ast::Type& declared) -> MaybeTypespec {
    const slang::ast::Type& type = declared.getCanonicalType();
    // A class's typespec is the class itself, whatever typedef the source
    // names it by, and is named (LRM 37.32 details 1, 4).
    if (const auto* cls = type.as_if<slang::ast::ClassType>()) {
      return Intern(hir::ClassTypespec{.name = std::string{cls->name}});
    }
    if (const auto* alias = declared.as_if<slang::ast::TypeAliasType>()) {
      return OfTypedef(*alias);
    }
    if (const std::optional<hir::PlainKind> plain = PlainKindOf(type)) {
      return Intern(hir::PlainTypespec{.kind = *plain});
    }
    if (const auto* enumeration = type.as_if<slang::ast::EnumType>()) {
      return OfEnum(*enumeration);
    }
    if (const auto* integer = type.as_if<slang::ast::PredefinedIntegerType>()) {
      return Intern(
          hir::IntegerTypespec{
              .keyword = IntegerKeywordOf(*integer),
              .is_signed = type.isSigned()});
    }
    if (std::optional<MaybeTypespec> packed = OfPacked(type, type.isSigned())) {
      return *std::move(packed);
    }
    if (const auto* unpacked = type.as_if<slang::ast::UnpackedStructType>()) {
      return OfUnpackedAggregate(type, unpacked->fields);
    }
    if (const auto* unpacked = type.as_if<slang::ast::UnpackedUnionType>()) {
      return OfUnpackedAggregate(type, unpacked->fields);
    }
    if (const auto* fixed =
            type.as_if<slang::ast::FixedSizeUnpackedArrayType>()) {
      return OfElement(fixed->elementType, [&](hir::TypespecId element) {
        return hir::TypespecData{hir::FixedArrayTypespec{
            .range = RangeOf(fixed->range), .element = element}};
      });
    }
    if (const auto* dynamic = type.as_if<slang::ast::DynamicArrayType>()) {
      return OfElement(dynamic->elementType, [](hir::TypespecId element) {
        return hir::TypespecData{hir::DynamicArrayTypespec{.element = element}};
      });
    }
    if (const auto* queue = type.as_if<slang::ast::QueueType>()) {
      return OfElement(queue->elementType, [](hir::TypespecId element) {
        return hir::TypespecData{hir::QueueTypespec{.element = element}};
      });
    }
    if (const auto* associative =
            type.as_if<slang::ast::AssociativeArrayType>()) {
      return OfAssociative(*associative);
    }
    return std::nullopt;
  }

  [[nodiscard]] auto Intern(hir::TypespecData data) const -> MaybeTypespec {
    return owner_->Unit().typespecs.Intern(
        hir::Typespec{.data = std::move(data)});
  }

  // A typedef's typespec: its name, the typespec of the type it names, and
  // whether it aliases that typespec, which it does where that type is named.
  auto OfTypedef(const slang::ast::TypeAliasType& typedef_type)
      -> MaybeTypespec {
    const slang::ast::Type& target = typedef_type.targetType.getType();
    auto named = Of(target);
    if (!named) return std::unexpected(std::move(named.error()));
    if (!named->has_value()) return std::nullopt;
    return Intern(
        hir::TypedefTypespec{
            .name = std::string{typedef_type.name},
            .names = **named,
            .aliases = IsNamedType(target)});
  }

  // A typespec built over the one of `element`, none where that has none.
  template <typename Build>
  auto OfElement(const slang::ast::Type& element, const Build& build)
      -> MaybeTypespec {
    auto held = Of(element);
    if (!held) return std::unexpected(std::move(held.error()));
    if (!held->has_value()) return std::nullopt;
    return Intern(build(**held));
  }

  auto OfEnum(const slang::ast::EnumType& enumeration) -> MaybeTypespec {
    auto base = Of(enumeration.baseType);
    if (!base) return std::unexpected(std::move(base.error()));
    auto type = owner_->InternType(enumeration, span_);
    if (!type) return std::unexpected(std::move(type.error()));
    if (!base->has_value()) return std::nullopt;
    return Intern(hir::EnumTypespec{.base = **base, .enumeration = *type});
  }

  // The typespec of a single bit, a packed array or a packed structure or
  // union `type` is, signed where `is_signed` says; none where it is none of
  // these.
  auto OfPacked(const slang::ast::Type& type, bool is_signed)
      -> std::optional<MaybeTypespec> {
    if (const auto* scalar = type.as_if<slang::ast::ScalarType>()) {
      return Intern(
          hir::BitTypespec{
              .keyword = BitKeywordOf(*scalar), .is_signed = is_signed});
    }
    if (const auto* packed = type.as_if<slang::ast::PackedArrayType>()) {
      auto element = OfPackedElement(packed->elementType);
      if (!element) return std::unexpected(std::move(element.error()));
      if (!element->has_value()) return MaybeTypespec{std::nullopt};
      return Intern(
          hir::PackedArrayTypespec{
              .range = RangeOf(packed->range),
              .element = **element,
              .is_signed = is_signed});
    }
    const auto* packed_struct = type.as_if<slang::ast::PackedStructType>();
    const auto* packed_union = type.as_if<slang::ast::PackedUnionType>();
    if (packed_struct == nullptr && packed_union == nullptr) {
      return std::nullopt;
    }
    auto aggregate = owner_->InternType(type, span_);
    if (!aggregate) return std::unexpected(std::move(aggregate.error()));
    auto members = MembersOf(
        packed_struct != nullptr ? FieldsOf(*packed_struct)
                                 : FieldsOf(*packed_union));
    if (!members) return std::unexpected(std::move(members.error()));
    if (!members->has_value()) return MaybeTypespec{std::nullopt};
    if (packed_union != nullptr) {
      return Intern(
          hir::PackedUnionTypespec{
              .type = *aggregate,
              .is_signed = is_signed,
              .members = **std::move(members),
              .soft = packed_union->isSoft});
    }
    return Intern(
        hir::PackedStructTypespec{
            .type = *aggregate,
            .is_signed = is_signed,
            .members = **std::move(members)});
  }

  // The typespec of an element of a packed array, which is unsigned unless it
  // is of a named type declared signed (LRM 7.4.1), whatever the array is: a
  // typedef's keeps its own sign, and any other packed one is built unsigned.
  auto OfPackedElement(const slang::ast::Type& declared) -> MaybeTypespec {
    if (declared.as_if<slang::ast::TypeAliasType>() == nullptr) {
      if (std::optional<MaybeTypespec> packed =
              OfPacked(declared.getCanonicalType(), false)) {
        return *std::move(packed);
      }
    }
    return Of(declared);
  }

  template <typename Packed>
  static auto FieldsOf(const Packed& aggregate)
      -> std::vector<const slang::ast::FieldSymbol*> {
    std::vector<const slang::ast::FieldSymbol*> fields;
    for (const slang::ast::FieldSymbol& field :
         aggregate.template membersOfType<slang::ast::FieldSymbol>()) {
      fields.push_back(&field);
    }
    return fields;
  }

  // The typespec of each member of `fields`, in order, none where one of them
  // has none.
  auto MembersOf(std::span<const slang::ast::FieldSymbol* const> fields)
      -> diag::Result<std::optional<std::vector<hir::TypespecId>>> {
    std::vector<hir::TypespecId> members;
    members.reserve(fields.size());
    for (const slang::ast::FieldSymbol* field : fields) {
      auto typespec = Of(field->getType());
      if (!typespec) return std::unexpected(std::move(typespec.error()));
      if (!typespec->has_value()) return std::nullopt;
      members.push_back(**typespec);
    }
    return members;
  }

  // An unpacked structure or union: its type, which states its members, and
  // each member's typespec.
  auto OfUnpackedAggregate(
      const slang::ast::Type& type,
      std::span<const slang::ast::FieldSymbol* const> fields) -> MaybeTypespec {
    auto aggregate = owner_->InternType(type, span_);
    if (!aggregate) return std::unexpected(std::move(aggregate.error()));
    auto members = MembersOf(fields);
    if (!members) return std::unexpected(std::move(members.error()));
    if (!members->has_value()) return std::nullopt;
    return Intern(
        hir::UnpackedAggregateTypespec{
            .type = *aggregate, .members = **std::move(members)});
  }

  auto OfAssociative(const slang::ast::AssociativeArrayType& associative)
      -> MaybeTypespec {
    auto element = Of(associative.elementType);
    if (!element) return std::unexpected(std::move(element.error()));
    if (!element->has_value()) return std::nullopt;
    std::optional<hir::TypespecId> index;
    if (associative.indexType != nullptr) {
      auto declared = Of(*associative.indexType);
      if (!declared) return std::unexpected(std::move(declared.error()));
      if (!declared->has_value()) return std::nullopt;
      index = **declared;
    }
    return Intern(
        hir::AssociativeArrayTypespec{.element = **element, .index = index});
  }

  UnitLowerer* owner_;
  diag::SourceSpan span_;
  std::unordered_map<const slang::ast::Type*, std::optional<hir::TypespecId>>
      built_;
};

}  // namespace

auto TypespecOf(
    UnitLowerer& owner, const slang::ast::Type& declared, diag::SourceSpan span)
    -> diag::Result<std::optional<hir::TypespecId>> {
  return TypespecBuilder(owner, span).Of(declared);
}

}  // namespace lyra::lowering::ast_to_hir
