#include "lyra/lowering/hir_to_mir/lower_type.hpp"

#include <variant>
#include <vector>

#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/hir/type.hpp"
#include "lyra/lowering/hir_to_mir/state.hpp"
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

auto TranslateUnpackedRanges(const std::vector<hir::UnpackedRange>& src)
    -> std::vector<mir::UnpackedRange> {
  std::vector<mir::UnpackedRange> out;
  out.reserve(src.size());
  for (const auto& r : src) {
    out.push_back(mir::UnpackedRange{.left = r.left, .right = r.right});
  }
  return out;
}

}  // namespace

auto TranslateTypeData(
    const hir::TypeData& data, const UnitLoweringState& state)
    -> mir::TypeData {
  return std::visit(
      Overloaded{
          [&](const hir::PackedArrayType& src) -> mir::TypeData {
            return mir::PackedArrayType{
                .atom = TranslateBitAtom(src.atom),
                .signedness = TranslateSignedness(src.signedness),
                .dims = TranslatePackedRanges(src.dims),
                .form = TranslatePackedArrayForm(src.form),
            };
          },
          [&](const hir::UnpackedArrayType& src) -> mir::TypeData {
            return mir::UnpackedArrayType{
                .element_type = state.TranslateType(src.element_type),
                .dims = TranslateUnpackedRanges(src.dims),
            };
          },
          [&](const hir::DynamicArrayType& src) -> mir::TypeData {
            return mir::DynamicArrayType{
                .element_type = state.TranslateType(src.element_type),
            };
          },
          [&](const hir::QueueType& src) -> mir::TypeData {
            return mir::QueueType{
                .element_type = state.TranslateType(src.element_type),
                .max_bound = src.max_bound,
            };
          },
          [&](const hir::AssociativeArrayType& src) -> mir::TypeData {
            return mir::AssociativeArrayType{
                .element_type = state.TranslateType(src.element_type),
                .key_type =
                    src.key_type.has_value()
                        ? std::optional<mir::TypeId>{state.TranslateType(
                              *src.key_type)}
                        : std::nullopt,
            };
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
