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
#include "lyra/hir/type_pool.hpp"

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
  const TypeId here = destination_->Intern(ImportData(source_->Get(id).data));
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

auto TypeImporter::ImportData(const TypeData& data) -> TypeData {
  return std::visit(
      Overloaded{
          [](const ScalarBitType& t) -> TypeData { return t; },
          [this](const PackedArrayType& t) -> TypeData {
            return PackedArrayType{
                .dim = t.dim,
                .element_type = Import(t.element_type),
                .signedness = t.signedness,
                .form = t.form};
          },
          [this](const PackedStructType& t) -> TypeData {
            return PackedStructType{
                .fields = ImportPackedFields(t.fields, *this),
                .signedness = t.signedness};
          },
          [this](const PackedUnionType& t) -> TypeData {
            return PackedUnionType{
                .fields = ImportPackedFields(t.fields, *this),
                .signedness = t.signedness,
                .tagged = t.tagged};
          },
          [this](const EnumType& t) -> TypeData {
            return EnumType{
                .base_type = Import(t.base_type), .members = t.members};
          },
          [this](const UnpackedStructType& t) -> TypeData {
            return UnpackedStructType{
                .fields = ImportUnpackedFields(t.fields, *this)};
          },
          [this](const UnpackedUnionType& t) -> TypeData {
            return UnpackedUnionType{
                .fields = ImportUnpackedFields(t.fields, *this),
                .tagged = t.tagged};
          },
          [this](const UnpackedArrayType& t) -> TypeData {
            return UnpackedArrayType{
                .element_type = Import(t.element_type), .dim = t.dim};
          },
          [this](const DynamicArrayType& t) -> TypeData {
            return DynamicArrayType{.element_type = Import(t.element_type)};
          },
          [this](const QueueType& t) -> TypeData {
            return QueueType{
                .element_type = Import(t.element_type),
                .max_bound = t.max_bound};
          },
          [this](const AssociativeArrayType& t) -> TypeData {
            return AssociativeArrayType{
                .element_type = Import(t.element_type),
                .key_type = Import(t.key_type)};
          },
          [](const WildcardIndexType& t) -> TypeData { return t; },
          [](const StringType& t) -> TypeData { return t; },
          [](const EventType& t) -> TypeData { return t; },
          [](const RealType& t) -> TypeData { return t; },
          [](const ShortRealType& t) -> TypeData { return t; },
          [](const RealTimeType& t) -> TypeData { return t; },
          [](const ChandleType& t) -> TypeData { return t; },
          [this](const ClassHandleType& t) -> TypeData {
            return ClassHandleType{.class_ref = ImportClassRef(t.class_ref)};
          },
          [](const ImportedClassHandleType& t) -> TypeData { return t; },
          [](const UnitObjectType& t) -> TypeData { return t; },
          [](const NullType& t) -> TypeData { return t; },
          [](const VoidType& t) -> TypeData { return t; }},
      data);
}

}  // namespace lyra::hir
