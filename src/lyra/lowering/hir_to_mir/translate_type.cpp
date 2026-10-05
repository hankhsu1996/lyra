#include <cstdint>
#include <utility>
#include <variant>
#include <vector>

#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/hir/type.hpp"
#include "lyra/lowering/hir_to_mir/expression/references.hpp"
#include "lyra/lowering/hir_to_mir/integral_literal.hpp"
#include "lyra/lowering/hir_to_mir/packed_projection.hpp"
#include "lyra/lowering/hir_to_mir/struct_methods.hpp"
#include "lyra/lowering/hir_to_mir/unit_lowerer.hpp"
#include "lyra/mir/struct_decl.hpp"
#include "lyra/mir/struct_id.hpp"
#include "lyra/mir/type.hpp"
#include "lyra/mir/type_declaration_ref.hpp"
#include "lyra/support/runtime_class.hpp"

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

// What separates an array a declaration builds from an array a program computes
// with, which the source spells the same way. Asked of the declared type rather
// than of what it lowered to, because a referrer that never reaches inside such
// an object carries no identity for it and so cannot be asked which unit it is.
auto DeclaresObjects(const hir::TypePool& types, hir::TypeId type) -> bool {
  return hir::ObjectsBehind(types, type).has_value();
}

// Projects a recursive HIR packed array onto MIR's flat single-vector shape
// (LRM 7.4.1). A scalar-bit terminal contributes how many states its bits have
// and this one dimension; any other element (a nested packed array, or a packed
// aggregate's single-vector projection) contributes its own flat dimensions,
// onto which this dimension prepends.
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

// The types of an unpacked aggregate's members, in declaration order, which is
// the position an access reaches each by. A member's own declaration
// initializer (LRM 7.2.2) is not part of what the type is: it is the value a
// default construction composes at the site that needs one.
template <typename Field>
auto TranslateMemberTypes(
    UnitLowerer& unit_lowerer, const std::vector<Field>& fields)
    -> std::vector<mir::TypeId> {
  std::vector<mir::TypeId> types;
  types.reserve(fields.size());
  for (const Field& field : fields) {
    types.push_back(unit_lowerer.TranslateType(field.type));
  }
  return types;
}

}  // namespace

auto UnitLowerer::TranslateStructType(const hir::UnpackedStructType& src)
    -> mir::StructType {
  if (src.declaration.unit_name == unit_.name) {
    return mir::StructType{.declaration = unit_.structs.Declare()};
  }
  mir::TypeDeclarationRef declaration{
      .unit_name = src.declaration.unit_name, .name = src.declaration.name};
  // A struct another unit declares brings that unit's statement of its
  // operations with it, which is a dependency on that unit.
  unit_.ConsumeNamespaceOf(declaration.unit_name);
  if (mir::FindExternalStruct(unit_, declaration) == nullptr) {
    unit_.external_structs.push_back(
        mir::ExternalStruct{
            .declaration = declaration,
            .elements = TranslateMemberTypes(*this, src.fields)});
  }
  return mir::StructType{.declaration = std::move(declaration)};
}

void UnitLowerer::DefineOwnStruct(
    const hir::UnpackedStructType& src, mir::StructId id,
    mir::TypeId structure) {
  std::vector<mir::TypeId> elements = TranslateMemberTypes(*this, src.fields);
  std::vector<mir::StructMethod> methods = StructMethodsOf(
      *this,
      mir::TypeDeclarationRef{
          .unit_name = unit_.name, .name = src.declaration.name},
      structure, elements);
  unit_.structs.Define(
      id, mir::StructDecl{
              .name = src.declaration.name,
              .elements = std::move(elements),
              .methods = std::move(methods)});
}

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
          // A packed aggregate is the vector its members project onto: every
          // value operation runs on that vector, and a member is reached as a
          // part-select of it, which is settled before this layer.
          [&](const hir::PackedStructType& src) -> mir::Type {
            return mir::Type{FlattenPackedAggregate(
                ProjectPackedAggregate(*this, type), src.signedness)};
          },
          [&](const hir::PackedUnionType& src) -> mir::Type {
            return mir::Type{FlattenPackedAggregate(
                ProjectPackedAggregate(*this, type), src.signedness)};
          },
          [&](const hir::EnumType& src) -> mir::Type {
            // An enumeration keeps a MIR type of its own, carrying its base's
            // packed shape and its members. A value operation reads the packed
            // shape and so treats the value as its base integral; only what
            // an enumeration answers about a value (LRM 6.19.5, 6.24.2) reads
            // the members.
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
              members.push_back(
                  mir::EnumMember{
                      .name = m.name,
                      .value = CanonicalIntegralConstant(
                          *base_pa, LowerHirIntegralConstant(m.value))});
            }
            return mir::Type{mir::EnumType{
                .base = *base_pa,
                .members = std::move(members),
            }};
          },
          [&](const hir::UnpackedStructType& src) -> mir::Type {
            return mir::Type{TranslateStructType(src)};
          },
          [&](const hir::UnpackedUnionType& src) -> mir::Type {
            // The untagged overlapping-storage form (LRM 7.3) maps to
            // `UnionType`; the tagged, type-checked sum form (LRM 7.3.2) to
            // `TaggedUnionType` -- MIR keeps them as distinct types because
            // their value spaces and access semantics genuinely differ.
            std::vector<mir::TypeId> members =
                TranslateMemberTypes(*this, src.fields);
            if (!src.tagged) {
              return mir::Type{mir::UnionType{.members = std::move(members)}};
            }
            // A `void` member (LRM 7.3.2) occupies a value slot, so its
            // component is the type carrying no information rather than the
            // absence of a type the SV keyword otherwise names.
            for (mir::TypeId& member : members) {
              if (unit_.types.Get(member).Is<mir::VoidType>()) {
                member = unit_.types.Intern(mir::Type{mir::EmptyType{}});
              }
            }
            return mir::Type{
                mir::TaggedUnionType{.members = std::move(members)}};
          },
          [&](const hir::UnpackedArrayType& src) -> mir::Type {
            const mir::TypeId element = TranslateType(src.element_type);
            // An array of objects is a sequence, never a value array: an object
            // is reached by its address and has no value form, so there is
            // nothing for a value array to hold. Which element a coordinate
            // names is settled where the reference is built, which is what
            // spends the declared range and leaves the sequence stating only
            // that there are several.
            if (DeclaresObjects(Hir().types, src.element_type)) {
              return mir::Type{mir::VectorType{.element = element}};
            }
            return mir::Type{mir::UnpackedArrayType{
                .element_type = element,
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
          // LRM 6.12: `realtime` is synonymous with `real`.
          [](const hir::RealTimeType&) -> mir::Type {
            return mir::Type{mir::RealType{}};
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
            return UnitObjectNamed(src.unit_name, src.class_name);
          },
          // Objects of several classes are held as the scope every one of them
          // is, and a step viewing the one a select picks out names its class.
          [](const hir::UnitObjectByPositionType&) -> mir::Type {
            return mir::Type{
                mir::RuntimeClassType{.which = support::RuntimeClass::kScope}};
          },
          // What a virtual interface holds is which instance it names, or none
          // (LRM 25.9): a host pointer compared by identity and null until
          // assigned, which the instance's lifetime -- the simulation's --
          // lets it hold without owning anything. That is the chandle's value
          // exactly, so it is held, copied, compared and stored in containers
          // as one; an access turns it into a pointer to the unit's object
          // there, where the access names that unit.
          [](const hir::VirtualInterfaceType&) -> mir::Type {
            return mir::Type{mir::ChandleType{}};
          },
          [](const hir::NullType&) -> mir::Type {
            // The `null` literal names no object, so it carries no class of its
            // own and is typed as the opaque handle. What it is read at is
            // whatever it meets: a comparison against a handle brings it to
            // that handle's type first, because which kind of value it is
            // decides which runtime object it is, and an opaque handle holds a
            // bare pointer where a class handle holds a share of ownership.
            return mir::Type{mir::ChandleType{}};
          },
          [](const hir::VoidType&) -> mir::Type {
            return mir::Type{mir::VoidType{}};
          },
      });
}

}  // namespace lyra::lowering::hir_to_mir
