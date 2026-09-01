#include <cstdint>
#include <variant>
#include <vector>

#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/hir/type.hpp"
#include "lyra/lowering/hir_to_mir/packed_projection.hpp"
#include "lyra/lowering/hir_to_mir/unit_lowerer.hpp"
#include "lyra/mir/type.hpp"

namespace lyra::lowering::hir_to_mir {

namespace {

auto StateKindOf(hir::BitAtom atom) -> mir::IntegralStateKind {
  switch (atom) {
    case hir::BitAtom::kBit:
      return mir::IntegralStateKind::kTwoState;
    case hir::BitAtom::kLogic:
      return mir::IntegralStateKind::kFourState;
  }
  throw InternalError("StateKindOf: unknown BitAtom");
}

auto TranslateSignedness(hir::Signedness s) -> mir::Signedness {
  return s == hir::Signedness::kSigned ? mir::Signedness::kSigned
                                       : mir::Signedness::kUnsigned;
}

// Projects a recursive HIR packed array onto MIR's flat single-vector shape
// (LRM 7.4.1). A scalar-bit terminal contributes how many states its bits have
// and this one dimension; any other element (a nested packed array, or a packed
// aggregate's
// single-vector projection) contributes its own flat dimensions, onto which
// this dimension prepends.
auto FlattenPackedArray(
    const UnitLowerer& unit_lowerer, const hir::PackedArrayType& pa)
    -> mir::PackedArrayType {
  const mir::PackedRange dim{.left = pa.dim.left, .right = pa.dim.right};
  const hir::Type& element = unit_lowerer.Hir().types.Get(pa.element_type);
  if (const auto* scalar = element.As<hir::ScalarBitType>()) {
    return mir::PackedArrayType{
        .state_kind = StateKindOf(scalar->atom),
        .signedness = TranslateSignedness(pa.signedness),
        .dims = {dim},
    };
  }
  const mir::PackedArrayType& inner =
      (unit_lowerer.Unit().types.Get(
           unit_lowerer.TranslateType(pa.element_type)))
          .PackedShape();
  std::vector<mir::PackedRange> dims;
  dims.reserve(inner.dims.size() + 1U);
  dims.push_back(dim);
  dims.insert(dims.end(), inner.dims.begin(), inner.dims.end());
  return mir::PackedArrayType{
      .state_kind = inner.state_kind,
      .signedness = TranslateSignedness(pa.signedness),
      .dims = std::move(dims),
  };
}

// A packed aggregate's single-vector projection (LRM 7.2.1 / 7.3.1 / 7.3.2):
// one flat vector as wide as the aggregate's members place it, `logic` iff any
// member is 4-state.
auto FlattenPackedAggregate(
    const PackedProjection& layout, hir::Signedness signedness)
    -> mir::PackedArrayType {
  return mir::PackedArrayType{
      .state_kind = layout.state_kind,
      .signedness = TranslateSignedness(signedness),
      .dims = {mir::PackedRange{
          .left = static_cast<std::int64_t>(layout.bit_width) - 1, .right = 0}},
  };
}

}  // namespace

auto UnitLowerer::TranslateType(const hir::Type& type) -> mir::Type {
  return type.Visit(
      Overloaded{
          [&](const hir::ScalarBitType& src) -> mir::Type {
            // A bare scalar is a one-bit unsigned vector in MIR's flat shape.
            return mir::Type{mir::PackedArrayType{
                .state_kind = StateKindOf(src.atom),
                .signedness = mir::Signedness::kUnsigned,
                .dims = {mir::PackedRange{.left = 0, .right = 0}},
            }};
          },
          [&](const hir::PackedArrayType& src) -> mir::Type {
            return mir::Type{FlattenPackedArray(*this, src)};
          },
          [&](const hir::PackedStructType& src) -> mir::Type {
            // Per-member position bakes into a constant-bounds slice at
            // expression lowering, so MIR keeps no aggregate type -- only the
            // vector the members are placed in.
            return mir::Type{FlattenPackedAggregate(
                ProjectPackedAggregate(*this, type), src.signedness)};
          },
          [&](const hir::PackedUnionType& src) -> mir::Type {
            return mir::Type{FlattenPackedAggregate(
                ProjectPackedAggregate(*this, type), src.signedness)};
          },
          [&](const hir::EnumType& src) -> mir::Type {
            // An enumeration keeps a MIR type of its own, carrying its base's
            // packed shape and its member table. A value operation reads the
            // packed shape and so treats the value as its base integral; only
            // the LRM 6.19.5 methods read the member table.
            const auto& base_mir_data =
                Unit().types.Get(TranslateType(src.base_type));
            const auto* base_pa = base_mir_data.As<mir::PackedArrayType>();
            if (base_pa == nullptr) {
              throw InternalError(
                  "TranslateType: enum base did not lower to a "
                  "PackedArrayType");
            }
            std::vector<mir::EnumMember> members;
            members.reserve(src.members.size());
            for (const auto& m : src.members) {
              const auto value =
                  static_cast<std::int64_t>(m.value.value_words[0]);
              members.push_back(
                  mir::EnumMember{.name = m.name, .value = value});
            }
            return mir::Type{mir::EnumType{
                .base = *base_pa,
                .members = std::move(members),
            }};
          },
          [&](const hir::UnpackedStructType& src) -> mir::Type {
            std::vector<mir::TypeId> elements;
            elements.reserve(src.fields.size());
            for (const auto& field : src.fields) {
              elements.push_back(TranslateType(field.type));
            }
            return mir::Type{mir::TupleType{.elements = std::move(elements)}};
          },
          [&](const hir::UnpackedUnionType& src) -> mir::Type {
            // The untagged overlapping-storage form (LRM 7.3) maps to
            // `UnionType`; the tagged, type-checked sum form (LRM 7.3.2) to
            // `TaggedUnionType` -- MIR keeps them as distinct types because
            // their value spaces and access semantics genuinely differ.
            std::vector<mir::TypeId> elements;
            elements.reserve(src.fields.size());
            for (const auto& field : src.fields) {
              elements.push_back(TranslateType(field.type));
            }
            if (!src.tagged) {
              return mir::Type{mir::UnionType{.elements = std::move(elements)}};
            }
            // A `void` member (LRM 7.3.2) occupies a value slot, so its
            // component is the type carrying no information rather than the
            // absence of a type the SV keyword otherwise names.
            for (mir::TypeId& element : elements) {
              if (unit_.types.Get(element).Is<mir::VoidType>()) {
                element = unit_.types.Intern(mir::Type{mir::EmptyType{}});
              }
            }
            return mir::Type{
                mir::TaggedUnionType{.elements = std::move(elements)}};
          },
          [&](const hir::UnpackedArrayType& src) -> mir::Type {
            return mir::Type{mir::UnpackedArrayType{
                .element_type = TranslateType(src.element_type),
                .dim =
                    mir::UnpackedRange{
                        .left = src.dim.left, .right = src.dim.right},
            }};
          },
          [&](const hir::DynamicArrayType& src) -> mir::Type {
            return mir::Type{mir::DynamicArrayType{
                .element_type = TranslateType(src.element_type),
            }};
          },
          [&](const hir::QueueType& src) -> mir::Type {
            return mir::Type{mir::QueueType{
                .element_type = TranslateType(src.element_type),
                .max_bound = src.max_bound,
            }};
          },
          [&](const hir::AssociativeArrayType& src) -> mir::Type {
            return mir::Type{mir::AssociativeArrayType{
                .element_type = TranslateType(src.element_type),
                .key_type = TranslateType(src.key_type),
            }};
          },
          [](const hir::WildcardIndexType&) -> mir::Type {
            return mir::Type{mir::WildcardIndexType{}};
          },
          [](const hir::StringType&) -> mir::Type {
            return mir::Type{mir::StringType{}};
          },
          [](const hir::EventType&) -> mir::Type {
            return mir::Type{mir::EventType{}};
          },
          [](const hir::RealType&) -> mir::Type {
            return mir::Type{mir::RealType{}};
          },
          [](const hir::ShortRealType&) -> mir::Type {
            return mir::Type{mir::ShortRealType{}};
          },
          [](const hir::RealTimeType&) -> mir::Type {
            return mir::Type{mir::RealTimeType{}};
          },
          [](const hir::ChandleType&) -> mir::Type {
            return mir::Type{mir::ChandleType{}};
          },
          [&](const hir::ClassHandleType& src) -> mir::Type {
            // A class handle is a managed reference to the class object: the
            // pointee is the object type naming the class's registry identity
            // (local) or the class's fully qualified name (external). The
            // external arm routes through the unit-lowerer's builder so the
            // cross-unit dependency is recorded in the same step as the type
            // intern.
            if (const auto* local =
                    std::get_if<hir::LocalClassRef>(&src.class_ref)) {
              return mir::Type{mir::ManagedRefType{
                  .pointee = ClassObjectType(local->class_id)}};
            }
            return mir::Type{mir::ManagedRefType{
                .pointee = MakeExternalClassPointee(
                    std::get<hir::ExternalClassRef>(src.class_ref))}};
          },
          [&](const hir::ImportedClassHandleType& src) -> mir::Type {
            // A handle to an imported runtime-library class is the same managed
            // reference, its pointee the runtime-provided object type.
            return mir::Type{mir::ManagedRefType{
                .pointee = ImportedRuntimeObjectType(src.klass)}};
          },
          [&](const hir::UnitObjectType& src) -> mir::Type {
            return UnitObjectNamed(src.unit_name);
          },
          [](const hir::NullType&) -> mir::Type {
            // The `null` literal carries no class identity; it renders as a
            // null pointer that any handle absorbs, so MIR types it as the
            // opaque handle. Its value, not its type, drives the comparison.
            return mir::Type{mir::ChandleType{}};
          },
          [](const hir::VoidType&) -> mir::Type {
            return mir::Type{mir::VoidType{}};
          },
      });
}

}  // namespace lyra::lowering::hir_to_mir
