#include "lyra/hir/type_import.hpp"

#include <optional>
#include <utility>
#include <variant>
#include <vector>

#include "lyra/base/overloaded.hpp"
#include "lyra/hir/class_id.hpp"
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

}  // namespace

TypeImporter::TypeImporter(
    const TypePool& source, TypePool& destination, TypeImportMemo& memo,
    ClassIdMap classes, CovergroupIdMap covergroups,
    ScopeClassIdMap scope_classes)
    : source_(&source),
      destination_(&destination),
      memo_(&memo),
      classes_(std::move(classes)),
      covergroups_(std::move(covergroups)),
      scope_classes_(std::move(scope_classes)) {
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

auto TypeImporter::ImportClass(ClassId id) const -> ClassId {
  return classes_(id);
}

auto TypeImporter::ImportCovergroup(CovergroupId id) const -> CovergroupId {
  return covergroups_(id);
}

auto TypeImporter::ImportScopeClass(ScopeClassId id) const -> ScopeClassId {
  return scope_classes_(id);
}

auto TypeImporter::Import(const PublishedCovergroup& published)
    -> PublishedCovergroup {
  const auto formals =
      [&](const std::vector<PublishedCovergroupFormal>& declared) {
        std::vector<PublishedCovergroupFormal> imported;
        imported.reserve(declared.size());
        for (const PublishedCovergroupFormal& formal : declared) {
          imported.push_back(
              PublishedCovergroupFormal{
                  .name = formal.name,
                  .type = Import(formal.type),
                  .direction = formal.direction});
        }
        return imported;
      };
  std::vector<PublishedCoverpoint> coverpoints;
  coverpoints.reserve(published.coverpoints.size());
  for (const PublishedCoverpoint& point : published.coverpoints) {
    coverpoints.push_back(
        PublishedCoverpoint{
            .name = point.name,
            .value_type = Import(point.value_type),
            .library_class = ImportClass(point.library_class)});
  }
  return PublishedCovergroup{
      .path = published.path,
      .formals = formals(published.formals),
      .sample_formals = formals(published.sample_formals),
      .embedding = published.embedding.transform(
          [&](ClassId cls) { return ImportClass(cls); }),
      .coverpoints = std::move(coverpoints),
      .crosses = published.crosses,
      .base = published.base.transform(
          [&](CovergroupId base) { return ImportCovergroup(base); })};
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
          // A declared type is its declaration, whatever pool holds it; what
          // the declaration declares crosses on its own, where it is asked
          // for.
          [](const EnumType& t) -> Type { return Type{t}; },
          [](const UnpackedStructType& t) -> Type { return Type{t}; },
          [](const UnpackedUnionType& t) -> Type { return Type{t}; },
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
            return Type{ClassHandleType{.class_id = ImportClass(t.class_id)}};
          },
          [](const ImportedClassHandleType& t) -> Type { return Type{t}; },
          [this](const WeakHandleType& t) -> Type {
            return Type{WeakHandleType{.handle = Import(t.handle)}};
          },
          [this](const CovergroupHandleType& t) -> Type {
            return Type{CovergroupHandleType{
                .covergroup = ImportCovergroup(t.covergroup)}};
          },
          [this](const UnitObjectType& t) -> Type {
            return Type{
                UnitObjectType{.scope_class = ImportScopeClass(t.scope_class)}};
          },
          [this](const UnitObjectsType& t) -> Type {
            UnitObjectsType imported = t;
            for (UnitObjectType& kind : imported.kinds) {
              kind.scope_class = ImportScopeClass(kind.scope_class);
            }
            return Type{std::move(imported)};
          },
          [](const VirtualInterfaceType& t) -> Type { return Type{t}; },
          [](const NullType& t) -> Type { return Type{t}; },
          [](const DynamicType& t) -> Type { return Type{t}; },
          [](const VoidType& t) -> Type { return Type{t}; }});
}

}  // namespace lyra::hir
