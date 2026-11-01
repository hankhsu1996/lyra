#include <cstddef>
#include <optional>
#include <utility>
#include <vector>

#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/hir/type.hpp"
#include "lyra/hir/type_definition.hpp"
#include "lyra/lowering/hir_to_mir/expression/references.hpp"
#include "lyra/lowering/hir_to_mir/packed_projection.hpp"
#include "lyra/lowering/hir_to_mir/unit_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/value_operations.hpp"
#include "lyra/mir/expr_builders.hpp"
#include "lyra/mir/record_decl.hpp"
#include "lyra/mir/record_id.hpp"
#include "lyra/mir/type.hpp"
#include "lyra/mir/type_builders.hpp"
#include "lyra/support/float_width.hpp"
#include "lyra/support/library_decl.hpp"

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

// A packed array is one vector of bits (LRM 7.4.1): as many elements as its
// dimension spans, each as wide as its element type. The bits take as many
// values as the element's do.
auto IntegralOfPackedArray(
    const UnitLowerer& unit_lowerer, const hir::PackedArrayType& packed)
    -> mir::IntegralType {
  const mir::IntegralType element =
      unit_lowerer.Unit()
          .types.Get(unit_lowerer.TranslateType(packed.element_type))
          .Integral();
  return mir::IntegralType{
      .bit_width = packed.dim.ElementCount() * element.bit_width,
      .signedness = TranslateSignedness(packed.signedness),
      .state_kind = element.state_kind,
  };
}

// The vector a packed aggregate's members project onto (LRM 7.2.1 / 7.3.1 /
// 7.3.2): as wide as its members place it, `logic` iff any member is 4-state.
auto IntegralOfPackedAggregate(
    const PackedProjection& layout, hir::Signedness signedness)
    -> mir::IntegralType {
  return mir::IntegralType{
      .bit_width = layout.bit_width,
      .signedness = TranslateSignedness(signedness),
      .state_kind = layout.state_kind,
  };
}

// The types of an unpacked aggregate's members, in declaration order, which is
// the position an access reaches each by. A member's own declaration
// initializer (LRM 7.2.2) is no part of the member's type: a structure states
// it in the default it answers for itself.
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

auto UnitLowerer::DeclareRecord() -> mir::RecordType {
  const mir::RecordId id = unit_.records.Declare();
  if (unit_.record_methods.Declare() != id) {
    throw InternalError(
        "hir_to_mir: a record's methods and its declaration took different "
        "identities -- please report this as a bug");
  }
  return mir::RecordType{.declaration = id};
}

auto UnitLowerer::UnionMemberTypes(const hir::UnpackedUnionDefinition& src)
    -> std::vector<mir::TypeId> {
  std::vector<mir::TypeId> members = TranslateMemberTypes(*this, src.fields);
  // A `void` member (LRM 7.3.2) occupies a value slot, so its component is
  // the tuple of no components, whose one value carries no information, rather
  // than the absence of a type the SV keyword otherwise names.
  for (mir::TypeId& member : members) {
    if (unit_.types.Get(member).Is<mir::VoidType>()) {
      member = unit_.types.Intern(mir::Type{mir::TupleType{}});
    }
  }
  return members;
}

void UnitLowerer::DefineRecord(const hir::Type& source, mir::RecordId id) {
  const hir::TypeDeclarationRef& declaration = *source.NamedDeclaration();
  const bool is_own = hir_->IsItself(declaration.unit_name);
  const auto declared = [&](mir::RecordKind kind,
                            std::vector<mir::TypeId> elements) {
    return mir::RecordDecl{
        .defining_unit =
            is_own ? std::nullopt : std::optional{declaration.unit_name},
        .path = declaration.path,
        .kind = kind,
        .elements = std::move(elements)};
  };
  if (const auto* structure = source.As<hir::UnpackedStructType>()) {
    unit_.records.Define(
        id, declared(
                mir::RecordKind::kStruct,
                TranslateMemberTypes(
                    *this, hir_->DefinitionOf(*structure).fields)));
  } else {
    const hir::UnpackedUnionDefinition& overlay =
        hir_->DefinitionOf(source.Get<hir::UnpackedUnionType>());
    unit_.records.Define(
        id, declared(
                overlay.tagged ? mir::RecordKind::kTaggedUnion
                               : mir::RecordKind::kUnion,
                UnionMemberTypes(overlay)));
  }
}

void UnitLowerer::DefineRecordMethods(
    const hir::Type& source, mir::RecordId id, mir::TypeId type) {
  const mir::RecordDecl& declared = unit_.GetRecord(id);
  // Another unit states the methods of a record it declares.
  if (declared.defining_unit.has_value()) {
    unit_.record_methods.Define(id, mir::RecordMethods{});
    return;
  }
  unit_.record_methods.Define(id, [&] {
    switch (declared.kind) {
      case mir::RecordKind::kStruct:
        return StructMethodsOf(
            *this, Hir().DefinitionOf(source.Get<hir::UnpackedStructType>()),
            id, type, declared.elements);
      case mir::RecordKind::kUnion:
        return UnionMethodsOf(*this, id, type, declared.elements);
      case mir::RecordKind::kTaggedUnion:
        return TaggedUnionMethodsOf(*this, id, type, declared.elements);
    }
    throw InternalError("hir_to_mir: unknown record kind");
  }());
}

auto UnitLowerer::TranslateType(const hir::Type& type)
    -> std::optional<mir::Type> {
  return type.Visit(
      Overloaded{
          [&](const hir::ScalarBitType& src) -> std::optional<mir::Type> {
            // A bare scalar is a one-bit unsigned vector.
            return mir::Type{mir::IntegralType{
                .bit_width = 1,
                .signedness = mir::Signedness::kUnsigned,
                .state_kind = StateKindOf(src.atom),
            }};
          },
          [&](const hir::PackedArrayType& src) -> std::optional<mir::Type> {
            return mir::Type{IntegralOfPackedArray(*this, src)};
          },
          // A packed aggregate is the vector its members project onto: every
          // value operation runs on that vector, and a member is reached as a
          // part-select of it, placed where the select is lowered.
          [&](const hir::PackedStructType& src) -> std::optional<mir::Type> {
            return mir::Type{IntegralOfPackedAggregate(
                ProjectPackedAggregate(*this, type), src.signedness)};
          },
          [&](const hir::PackedUnionType& src) -> std::optional<mir::Type> {
            return mir::Type{IntegralOfPackedAggregate(
                ProjectPackedAggregate(*this, type), src.signedness)};
          },
          [&](const hir::EnumType& declared) -> std::optional<mir::Type> {
            // An enumeration keeps a MIR type of its own, carrying its base
            // integral type and its members. A value operation reads the base
            // and so treats the value as that integral; only what an
            // enumeration answers about a value (LRM 6.19.5, 6.24.2) reads the
            // members.
            // The base is read as a type of its own rather than by its
            // translation: a definition another unit published names types
            // minted after the enumeration naming it.
            const hir::EnumDefinition& src = Hir().DefinitionOf(declared);
            const std::optional<mir::Type> base_type =
                TranslateType(Hir().types.Get(src.base_type));
            const auto* base = base_type.has_value()
                                   ? base_type->As<mir::IntegralType>()
                                   : nullptr;
            if (base == nullptr) {
              throw InternalError(
                  "TranslateType: enum base did not lower to an integral "
                  "type");
            }
            std::vector<mir::EnumMember> members;
            members.reserve(src.members.size());
            for (const auto& m : src.members) {
              members.push_back(
                  mir::EnumMember{
                      .name = m.name,
                      .value = mir::CanonicalIntegralConstant(
                          *base, LowerHirIntegralConstant(m.value))});
            }
            return mir::Type{mir::EnumType{
                .base = *base,
                .members = std::move(members),
            }};
          },
          [&](const hir::UnpackedStructType&) -> std::optional<mir::Type> {
            return mir::Type{DeclareRecord()};
          },
          [&](const hir::UnpackedUnionType&) -> std::optional<mir::Type> {
            return mir::Type{DeclareRecord()};
          },
          [&](const hir::UnpackedArrayType& src) -> std::optional<mir::Type> {
            return mir::Type{mir::UnpackedArrayType{
                .element_type = TranslateType(src.element_type),
                .count = src.dim.ElementCount(),
            }};
          },
          [&](const hir::DynamicArrayType& src) -> std::optional<mir::Type> {
            return unit_.types.Get(
                mir::DynamicArrayOf(
                    unit_.types, TranslateType(src.element_type)));
          },
          [&](const hir::QueueType& src) -> std::optional<mir::Type> {
            const support::QueueBound bound =
                src.max_bound.has_value()
                    ? support::QueueBound{support::BoundedQueue{
                          .count = *src.max_bound}}
                    : support::QueueBound{support::UnboundedQueue{}};
            return unit_.types.Get(
                mir::QueueOf(
                    unit_.types, TranslateType(src.element_type), bound));
          },
          [&](const hir::AssociativeArrayType& src)
              -> std::optional<mir::Type> {
            return unit_.types.Get(
                mir::AssociativeArrayOf(
                    unit_.types, TranslateType(src.key_type),
                    TranslateType(src.element_type)));
          },
          [](const hir::WildcardIndexType&) -> std::optional<mir::Type> {
            return mir::Type{mir::LibraryType{
                .decl = support::LibraryDecl::kWildcardIndex, .args = {}}};
          },
          [](const hir::StringType&) -> std::optional<mir::Type> {
            return mir::Type{mir::LibraryType{
                .decl = support::LibraryDecl::kString, .args = {}}};
          },
          [](const hir::DynamicType&) -> std::optional<mir::Type> {
            return mir::Type{mir::LibraryType{
                .decl = support::LibraryDecl::kAny, .args = {}}};
          },
          // LRM 6.17: an event is a handle to a synchronization object, so
          // it is held, copied, compared and nulled as a class handle is.
          [&](const hir::EventType&) -> std::optional<mir::Type> {
            return unit_.types.Get(unit_.builtins.event);
          },
          // LRM 6.12: a `real` is a C double and a `shortreal` a C float,
          // and `realtime` is synonymous with `real`.
          [](const hir::RealType&) -> std::optional<mir::Type> {
            return mir::Type{mir::RealType{.width = support::FloatWidth::k64}};
          },
          [](const hir::ShortRealType&) -> std::optional<mir::Type> {
            return mir::Type{mir::RealType{.width = support::FloatWidth::k32}};
          },
          [](const hir::RealTimeType&) -> std::optional<mir::Type> {
            return mir::Type{mir::RealType{.width = support::FloatWidth::k64}};
          },
          // LRM 6.14: a chandle holds a pointer a foreign call handed back,
          // which the simulation never reaches through.
          [&](const hir::ChandleType&) -> std::optional<mir::Type> {
            return mir::Type{
                mir::UnmanagedRefType{.pointee = unit_.builtins.void_type}};
          },
          // A class handle is a managed reference to the class object.
          [&](const hir::ClassHandleType& src) -> std::optional<mir::Type> {
            return mir::Type{
                mir::ManagedRefType{.pointee = ClassPointee(src.class_id)}};
          },
          [&](const hir::ImportedClassHandleType& src)
              -> std::optional<mir::Type> {
            // A handle to an imported runtime-library class is the same managed
            // reference, its pointee the runtime-provided object type.
            return mir::Type{mir::ManagedRefType{
                .pointee = ImportedRuntimeObjectType(src.klass)}};
          },
          // A handle held weakly refers to the object the handle names.
          [&](const hir::WeakHandleType& src) -> std::optional<mir::Type> {
            const mir::TypeId handle = TranslateType(src.handle);
            return mir::Type{mir::LibraryType{
                .decl = support::LibraryDecl::kWeakReference,
                .args = {unit_.types.Get(handle)
                             .Get<mir::ManagedRefType>()
                             .pointee}}};
          },
          // A covergroup instance is an object of the class the covergroup is
          // written as (LRM 19.3), reached through a managed reference as an
          // object of any class is.
          [&](const hir::CovergroupHandleType& src)
              -> std::optional<mir::Type> {
            return mir::Type{mir::ManagedRefType{
                .pointee = CovergroupObjectType(src.covergroup)}};
          },
          // An object of another unit is named by its class whether or not
          // this unit reaches into it, as a pointer to a class whose
          // definition is unseen is still a pointer to that class: what this
          // unit depends on that unit for is decided by what it reads, and the
          // name alone reads nothing.
          [&](const hir::UnitObjectType& src) -> std::optional<mir::Type> {
            return mir::Type{
                mir::ObjectType{.of = ScopeClassIdentity(src.scope_class)}};
          },
          // A set of objects is a sequence per dimension, never a value array:
          // an object is reached by its address and has no value form, and
          // which one a select names is settled where the reference is built,
          // which spends the declared range and leaves the sequence stating
          // only that there are several. One kind is held as itself; several
          // are held as the scope every one of them is, and a step viewing
          // the one a select picks out names its kind.
          [&](const hir::UnitObjectsType& src) -> std::optional<mir::Type> {
            mir::Type held =
                src.kinds.size() == 1
                    ? mir::Type{mir::ObjectType{
                          .of = ScopeClassIdentity(
                              src.kinds.front().scope_class)}}
                    : mir::Type{mir::LibraryType{
                          .decl = support::LibraryDecl::kScope, .args = {}}};
            for (std::size_t d = 0; d < src.ranges.size(); ++d) {
              held = mir::Type{mir::VectorType{
                  .element = unit_.types.Intern(std::move(held))}};
            }
            return held;
          },
          // What a virtual interface holds is which instance of its interface
          // it names, or none (LRM 25.9), and the instance lives as long as the
          // simulation, so the reference owns nothing. An access reaches
          // through it to the interface's object.
          [&](const hir::VirtualInterfaceType& src)
              -> std::optional<mir::Type> {
            return mir::Type{mir::UnmanagedRefType{
                .pointee = UnitObjectType(src.scope_class)}};
          },
          [](const hir::NullType&) -> std::optional<mir::Type> {
            return std::nullopt;
          },
          [](const hir::VoidType&) -> std::optional<mir::Type> {
            return mir::Type{mir::VoidType{}};
          },
      });
}

}  // namespace lyra::lowering::hir_to_mir
