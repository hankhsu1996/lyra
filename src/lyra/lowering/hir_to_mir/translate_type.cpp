#include <cstdint>
#include <variant>
#include <vector>

#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/hir/type.hpp"
#include "lyra/lowering/hir_to_mir/module_lowerer.hpp"
#include "lyra/mir/type.hpp"

namespace lyra::lowering::hir_to_mir {

namespace {

auto TranslateBitAtom(hir::BitAtom a) -> mir::BitAtom {
  switch (a) {
    case hir::BitAtom::kBit:
      return mir::BitAtom::kBit;
    case hir::BitAtom::kLogic:
      return mir::BitAtom::kLogic;
    case hir::BitAtom::kReg:
      return mir::BitAtom::kReg;
  }
  throw InternalError("TranslateBitAtom: unknown BitAtom");
}

auto TranslateSignedness(hir::Signedness s) -> mir::Signedness {
  return s == hir::Signedness::kSigned ? mir::Signedness::kSigned
                                       : mir::Signedness::kUnsigned;
}

auto TranslatePackedArrayForm(hir::PackedArrayForm f) -> mir::PackedArrayForm {
  switch (f) {
    case hir::PackedArrayForm::kExplicit:
      return mir::PackedArrayForm::kExplicit;
    case hir::PackedArrayForm::kByte:
      return mir::PackedArrayForm::kByte;
    case hir::PackedArrayForm::kShortInt:
      return mir::PackedArrayForm::kShortInt;
    case hir::PackedArrayForm::kInt:
      return mir::PackedArrayForm::kInt;
    case hir::PackedArrayForm::kLongInt:
      return mir::PackedArrayForm::kLongInt;
    case hir::PackedArrayForm::kInteger:
      return mir::PackedArrayForm::kInteger;
    case hir::PackedArrayForm::kTime:
      return mir::PackedArrayForm::kTime;
  }
  throw InternalError("TranslatePackedArrayForm: unknown PackedArrayForm");
}

// Projects a recursive HIR packed array onto MIR's flat single-vector shape
// (LRM 7.4.1). A scalar-bit terminal contributes the atom and this one
// dimension; any other element (a nested packed array, or a packed aggregate's
// single-vector projection) contributes its own flat dimensions, onto which
// this dimension prepends.
auto FlattenPackedArray(
    const ModuleLowerer& module, const hir::PackedArrayType& pa)
    -> mir::PackedArrayType {
  const mir::PackedRange dim{.left = pa.dim.left, .right = pa.dim.right};
  const hir::Type& element = module.Hir().types.Get(pa.element_type);
  if (const auto* scalar = std::get_if<hir::ScalarBitType>(&element.data)) {
    return mir::PackedArrayType{
        .atom = TranslateBitAtom(scalar->atom),
        .signedness = TranslateSignedness(pa.signedness),
        .dims = {dim},
        .form = TranslatePackedArrayForm(pa.form),
    };
  }
  const mir::PackedArrayType& inner =
      module.Unit()
          .types.Get(module.TranslateType(pa.element_type))
          .AsIntegralPacked();
  std::vector<mir::PackedRange> dims;
  dims.reserve(inner.dims.size() + 1U);
  dims.push_back(dim);
  dims.insert(dims.end(), inner.dims.begin(), inner.dims.end());
  return mir::PackedArrayType{
      .atom = inner.atom,
      .signedness = TranslateSignedness(pa.signedness),
      .dims = std::move(dims),
      .form = TranslatePackedArrayForm(pa.form),
  };
}

// A packed aggregate's single-vector projection (LRM 7.2.1 / 7.3.1): one flat
// `width`-bit vector, `logic` iff the aggregate is 4-state. The width is the
// caller's -- a struct sums its fields, a union takes its widest.
auto FlattenPackedAggregate(
    std::uint64_t width, hir::Signedness signedness, bool four_state)
    -> mir::PackedArrayType {
  return mir::PackedArrayType{
      .atom = four_state ? mir::BitAtom::kLogic : mir::BitAtom::kBit,
      .signedness = TranslateSignedness(signedness),
      .dims = {mir::PackedRange{
          .left = static_cast<std::int64_t>(width) - 1, .right = 0}},
      .form = mir::PackedArrayForm::kExplicit,
  };
}

}  // namespace

auto ModuleLowerer::TranslateTypeData(const hir::TypeData& data) const
    -> mir::TypeData {
  return std::visit(
      Overloaded{
          [&](const hir::ScalarBitType& src) -> mir::TypeData {
            // A bare scalar is a one-bit unsigned vector in MIR's flat shape.
            return mir::PackedArrayType{
                .atom = TranslateBitAtom(src.atom),
                .signedness = mir::Signedness::kUnsigned,
                .dims = {mir::PackedRange{.left = 0, .right = 0}},
                .form = mir::PackedArrayForm::kExplicit,
            };
          },
          [&](const hir::PackedArrayType& src) -> mir::TypeData {
            return FlattenPackedArray(*this, src);
          },
          [&](const hir::PackedStructType& src) -> mir::TypeData {
            // LRM 7.2.1: a packed struct projects to one vector summing its
            // fields. Per-field offset / width bake into constant-bounds
            // RangeSelect at expression lowering, so MIR keeps no struct type.
            std::uint64_t width = 0;
            for (const auto& field : src.fields) {
              width += field.bit_width;
            }
            return FlattenPackedAggregate(
                width, src.signedness, src.four_state);
          },
          [&](const hir::PackedUnionType& src) -> mir::TypeData {
            // LRM 7.3.1: an untagged packed union projects to one vector as
            // wide as its widest member; members overlap at the LSBs and each
            // flows to RangeSelect at expression lowering.
            std::uint64_t width = 0;
            for (const auto& field : src.fields) {
              width = field.bit_width > width ? field.bit_width : width;
            }
            return FlattenPackedAggregate(
                width, src.signedness, src.four_state);
          },
          [&](const hir::EnumType& src) -> mir::TypeData {
            // Enum is kept as a distinct mir::EnumType wrapping its base
            // PackedArray plus the member table. Value-level operations
            // unwrap via Type::AsIntegralPacked(); method dispatch reads the
            // members from this struct directly.
            const auto& base_mir_data =
                Unit().types.Get(TranslateType(src.base_type)).data;
            const auto* base_pa =
                std::get_if<mir::PackedArrayType>(&base_mir_data);
            if (base_pa == nullptr) {
              throw InternalError(
                  "TranslateTypeData: enum base did not lower to a "
                  "PackedArrayType");
            }
            std::vector<mir::EnumMember> members;
            members.reserve(src.members.size());
            for (const auto& m : src.members) {
              const std::int64_t value =
                  m.value.value_words.empty()
                      ? 0
                      : static_cast<std::int64_t>(m.value.value_words[0]);
              members.push_back(
                  mir::EnumMember{.name = m.name, .value = value});
            }
            return mir::EnumType{
                .base = *base_pa,
                .members = std::move(members),
            };
          },
          [&](const hir::UnpackedStructType& src) -> mir::TypeData {
            std::vector<mir::TypeId> elements;
            elements.reserve(src.fields.size());
            for (const auto& field : src.fields) {
              elements.push_back(TranslateType(field.type));
            }
            return mir::TupleType{.elements = std::move(elements)};
          },
          [&](const hir::UnpackedUnionType& src) -> mir::TypeData {
            // Only the untagged overlapping-storage form maps to `UnionType`
            // (LRM 7.3). A tagged union is a sum type rejected at ast_to_hir,
            // so it never reaches MIR translation.
            if (src.tagged) {
              throw InternalError(
                  "TranslateTypeData: tagged unpacked union reached MIR "
                  "translation");
            }
            std::vector<mir::TypeId> elements;
            elements.reserve(src.fields.size());
            for (const auto& field : src.fields) {
              elements.push_back(TranslateType(field.type));
            }
            return mir::UnionType{.elements = std::move(elements)};
          },
          [&](const hir::UnpackedArrayType& src) -> mir::TypeData {
            const std::int64_t span = (src.dim.left >= src.dim.right)
                                          ? (src.dim.left - src.dim.right)
                                          : (src.dim.right - src.dim.left);
            return mir::UnpackedArrayType{
                .element_type = TranslateType(src.element_type),
                .size = static_cast<std::uint64_t>(span) + 1U,
            };
          },
          [&](const hir::DynamicArrayType& src) -> mir::TypeData {
            return mir::DynamicArrayType{
                .element_type = TranslateType(src.element_type),
            };
          },
          [&](const hir::QueueType& src) -> mir::TypeData {
            return mir::QueueType{
                .element_type = TranslateType(src.element_type),
                .max_bound = src.max_bound,
            };
          },
          [&](const hir::AssociativeArrayType& src) -> mir::TypeData {
            return mir::AssociativeArrayType{
                .element_type = TranslateType(src.element_type),
                .key_type = TranslateType(src.key_type),
            };
          },
          [](const hir::WildcardIndexType&) -> mir::TypeData {
            return mir::WildcardIndexType{};
          },
          [](const hir::StringType&) -> mir::TypeData {
            return mir::StringType{};
          },
          [](const hir::EventType&) -> mir::TypeData {
            return mir::EventType{};
          },
          [](const hir::RealType&) -> mir::TypeData { return mir::RealType{}; },
          [](const hir::ShortRealType&) -> mir::TypeData {
            return mir::ShortRealType{};
          },
          [](const hir::RealTimeType&) -> mir::TypeData {
            return mir::RealTimeType{};
          },
          [](const hir::ChandleType&) -> mir::TypeData {
            return mir::ChandleType{};
          },
          [](const hir::VoidType&) -> mir::TypeData { return mir::VoidType{}; },
      },
      data);
}

}  // namespace lyra::lowering::hir_to_mir
