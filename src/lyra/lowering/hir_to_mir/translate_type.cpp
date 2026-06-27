#include <variant>
#include <vector>

#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/diag/diagnostic.hpp"
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

auto TranslatePackedRanges(const std::vector<hir::PackedRange>& src)
    -> std::vector<mir::PackedRange> {
  std::vector<mir::PackedRange> out;
  out.reserve(src.size());
  for (const auto& r : src) {
    out.push_back(mir::PackedRange{.left = r.left, .right = r.right});
  }
  return out;
}

}  // namespace

auto ModuleLowerer::TranslateTypeData(
    const hir::TypeData& data, diag::DiagSpan type_span) const
    -> diag::Result<mir::TypeData> {
  return std::visit(
      Overloaded{
          [&](const hir::PackedArrayType& src) -> diag::Result<mir::TypeData> {
            return mir::PackedArrayType{
                .atom = TranslateBitAtom(src.atom),
                .signedness = TranslateSignedness(src.signedness),
                .dims = TranslatePackedRanges(src.dims),
                .form = TranslatePackedArrayForm(src.form),
            };
          },
          [&](const hir::PackedStructType& src) -> diag::Result<mir::TypeData> {
            // LRM 7.2.1: "treated as a single vector". MIR keeps only that
            // projection -- a PackedArrayType. Per-field offset / width get
            // baked into constant-bounds RangeSelect at expression
            // lowering, so MIR carries no struct-specific type.
            return mir::PackedArrayType{
                .atom = TranslateBitAtom(src.base.atom),
                .signedness = TranslateSignedness(src.base.signedness),
                .dims = TranslatePackedRanges(src.base.dims),
                .form = TranslatePackedArrayForm(src.base.form),
            };
          },
          [&](const hir::PackedUnionType& src) -> diag::Result<mir::TypeData> {
            // LRM 7.3.1: untagged packed union "appears as a primary" is
            // "treated as a single vector". Same MIR shape as packed
            // struct -- the per-member (offset=0, width) table flows
            // through to RangeSelect at expression lowering.
            return mir::PackedArrayType{
                .atom = TranslateBitAtom(src.base.atom),
                .signedness = TranslateSignedness(src.base.signedness),
                .dims = TranslatePackedRanges(src.base.dims),
                .form = TranslatePackedArrayForm(src.base.form),
            };
          },
          [&](const hir::EnumType& src) -> diag::Result<mir::TypeData> {
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
          [&](const hir::UnpackedStructType& src)
              -> diag::Result<mir::TypeData> {
            std::vector<mir::TypeId> elements;
            elements.reserve(src.fields.size());
            for (const auto& field : src.fields) {
              elements.push_back(TranslateType(field.type));
            }
            return mir::TupleType{.elements = std::move(elements)};
          },
          [&](const hir::UnpackedUnionType& src)
              -> diag::Result<mir::TypeData> {
            // A tagged union is a sum type, a separate concept; only the
            // untagged overlapping-storage form maps to `UnionType` (LRM 7.3).
            if (src.tagged) {
              return diag::Fail(
                  type_span, diag::DiagCode::kUnsupportedUnpackedUnionType,
                  "tagged unions are not yet supported");
            }
            std::vector<mir::TypeId> elements;
            elements.reserve(src.fields.size());
            for (const auto& field : src.fields) {
              elements.push_back(TranslateType(field.type));
            }
            return mir::UnionType{.elements = std::move(elements)};
          },
          [&](const hir::UnpackedArrayType& src)
              -> diag::Result<mir::TypeData> {
            const std::int64_t span = (src.dim.left >= src.dim.right)
                                          ? (src.dim.left - src.dim.right)
                                          : (src.dim.right - src.dim.left);
            return mir::UnpackedArrayType{
                .element_type = TranslateType(src.element_type),
                .size = static_cast<std::uint64_t>(span) + 1U,
            };
          },
          [&](const hir::DynamicArrayType& src) -> diag::Result<mir::TypeData> {
            return mir::DynamicArrayType{
                .element_type = TranslateType(src.element_type),
            };
          },
          [&](const hir::QueueType& src) -> diag::Result<mir::TypeData> {
            return mir::QueueType{
                .element_type = TranslateType(src.element_type),
                .max_bound = src.max_bound,
            };
          },
          [&](const hir::AssociativeArrayType& src)
              -> diag::Result<mir::TypeData> {
            return mir::AssociativeArrayType{
                .element_type = TranslateType(src.element_type),
                .key_type = TranslateType(src.key_type),
            };
          },
          [](const hir::WildcardIndexType&) -> diag::Result<mir::TypeData> {
            return mir::WildcardIndexType{};
          },
          [](const hir::StringType&) -> diag::Result<mir::TypeData> {
            return mir::StringType{};
          },
          [](const hir::EventType&) -> diag::Result<mir::TypeData> {
            return mir::EventType{};
          },
          [](const hir::RealType&) -> diag::Result<mir::TypeData> {
            return mir::RealType{};
          },
          [](const hir::ShortRealType&) -> diag::Result<mir::TypeData> {
            return mir::ShortRealType{};
          },
          [](const hir::RealTimeType&) -> diag::Result<mir::TypeData> {
            return mir::RealTimeType{};
          },
          [](const hir::ChandleType&) -> diag::Result<mir::TypeData> {
            return mir::ChandleType{};
          },
          [](const hir::VoidType&) -> diag::Result<mir::TypeData> {
            return mir::VoidType{};
          },
      },
      data);
}

}  // namespace lyra::lowering::hir_to_mir
