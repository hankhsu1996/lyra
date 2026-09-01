#include "lyra/hir/type_import.hpp"

#include <optional>
#include <string>
#include <variant>
#include <vector>

#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/hir/class_ref.hpp"
#include "lyra/hir/type.hpp"
#include "lyra/hir/type_id.hpp"

namespace lyra::hir {

namespace {

auto ImportPackedFields(
    const std::vector<PackedAggregateField>& fields, TypeImporter& importer)
    -> std::vector<PackedAggregateField> {
  std::vector<PackedAggregateField> copied;
  copied.reserve(fields.size());
  for (const auto& field : fields) {
    copied.push_back(
        PackedAggregateField{
            .name = field.name, .type = importer.Import(field.type)});
  }
  return copied;
}

auto ImportUnpackedFields(
    const std::vector<UnpackedAggregateField>& fields, TypeImporter& importer)
    -> std::vector<UnpackedAggregateField> {
  std::vector<UnpackedAggregateField> copied;
  copied.reserve(fields.size());
  for (const auto& field : fields) {
    copied.push_back(
        UnpackedAggregateField{
            .name = field.name,
            .type = importer.Import(field.type),
            .default_init = field.default_init});
  }
  return copied;
}

}  // namespace

TypeImporter::TypeImporter(
    const TypePool& source, std::optional<TypePoolOwner> source_owner,
    TypePool& destination, TypeImportMemo& memo)
    : source_(&source),
      source_owner_(source_owner),
      destination_(&destination),
      memo_(&memo) {
}

auto TypeImporter::Import(TypeId id) -> TypeId {
  // The source pool grows while the unit that owns it goes on lowering, so the
  // memo is stretched to reach the id rather than sized once; an id the source
  // never minted is caught by the pool itself.
  if (memo_->size() <= id.value) {
    memo_->resize(id.value + 1);
  }
  if (const std::optional<TypeId> done = (*memo_)[id.value]) {
    return *done;
  }
  const TypeId here = destination_->Intern(Import(source_->Get(id)));
  (*memo_)[id.value] = here;
  return here;
}

auto TypeImporter::ImportClassRef(const ClassRef& ref) const -> ClassRef {
  return std::visit(
      Overloaded{
          [this](const LocalClassRef& local) -> ClassRef {
            if (!source_owner_.has_value()) {
              throw InternalError(
                  "TypeImporter: a pool that has left its unit names every "
                  "class by declaring unit and name, so a local class id "
                  "cannot appear in one");
            }
            return ExternalClassRef{
                .unit_name = std::string{source_owner_->unit_name},
                .class_name = source_owner_->classes->Get(local.class_id).name};
          },
          [](const ExternalClassRef& external) -> ClassRef {
            return external;
          }},
      ref);
}

auto TypeImporter::Import(const Type& type) -> Type {
  return type.Visit(
      Overloaded{
          [](const ScalarBitType& t) -> Type { return Type{t}; },
          [this](const PackedArrayType& t) -> Type {
            return Type{PackedArrayType{
                .dim = t.dim,
                .element_type = Import(t.element_type),
                .signedness = t.signedness}};
          },
          [this](const PackedStructType& t) -> Type {
            return Type{PackedStructType{
                .fields = ImportPackedFields(t.fields, *this),
                .signedness = t.signedness}};
          },
          [this](const PackedUnionType& t) -> Type {
            return Type{PackedUnionType{
                .fields = ImportPackedFields(t.fields, *this),
                .signedness = t.signedness,
                .tagged = t.tagged}};
          },
          [this](const EnumType& t) -> Type {
            return Type{EnumType{
                .base_type = Import(t.base_type), .members = t.members}};
          },
          [this](const UnpackedStructType& t) -> Type {
            return Type{UnpackedStructType{
                .fields = ImportUnpackedFields(t.fields, *this)}};
          },
          [this](const UnpackedUnionType& t) -> Type {
            return Type{UnpackedUnionType{
                .fields = ImportUnpackedFields(t.fields, *this),
                .tagged = t.tagged}};
          },
          [this](const UnpackedArrayType& t) -> Type {
            return Type{UnpackedArrayType{
                .element_type = Import(t.element_type), .dim = t.dim}};
          },
          [this](const DynamicArrayType& t) -> Type {
            return Type{
                DynamicArrayType{.element_type = Import(t.element_type)}};
          },
          [this](const QueueType& t) -> Type {
            return Type{QueueType{
                .element_type = Import(t.element_type),
                .max_bound = t.max_bound}};
          },
          [this](const AssociativeArrayType& t) -> Type {
            return Type{AssociativeArrayType{
                .element_type = Import(t.element_type),
                .key_type = Import(t.key_type)}};
          },
          [](const WildcardIndexType& t) -> Type { return Type{t}; },
          [](const StringType& t) -> Type { return Type{t}; },
          [](const EventType& t) -> Type { return Type{t}; },
          [](const RealType& t) -> Type { return Type{t}; },
          [](const ShortRealType& t) -> Type { return Type{t}; },
          [](const RealTimeType& t) -> Type { return Type{t}; },
          [](const ChandleType& t) -> Type { return Type{t}; },
          [this](const ClassHandleType& t) -> Type {
            return Type{
                ClassHandleType{.class_ref = ImportClassRef(t.class_ref)}};
          },
          [](const ImportedClassHandleType& t) -> Type { return Type{t}; },
          [](const UnitObjectType& t) -> Type { return Type{t}; },
          [](const NullType& t) -> Type { return Type{t}; },
          [](const VoidType& t) -> Type { return Type{t}; }});
}

}  // namespace lyra::hir
