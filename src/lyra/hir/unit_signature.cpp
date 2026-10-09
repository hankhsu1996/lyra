#include "lyra/hir/unit_signature.hpp"

#include <optional>
#include <span>
#include <utility>
#include <variant>
#include <vector>

#include "lyra/base/overloaded.hpp"
#include "lyra/hir/external_callee.hpp"
#include "lyra/hir/external_class.hpp"
#include "lyra/hir/external_scope_class.hpp"
#include "lyra/hir/published_callable.hpp"
#include "lyra/hir/published_member.hpp"
#include "lyra/hir/published_method.hpp"
#include "lyra/hir/published_modport.hpp"
#include "lyra/hir/published_target.hpp"
#include "lyra/hir/type.hpp"
#include "lyra/hir/type_import.hpp"

namespace lyra::hir {

auto ImportCalleeInterface(
    TypeImporter& importer, ExternalCalleeInterface interface)
    -> ExternalCalleeInterface {
  for (ExternalCalleeParam& param : interface.params) {
    param.type = importer.Import(param.type);
  }
  return interface;
}

auto ImportCallable(TypeImporter& importer, PublishedCallable callable)
    -> PublishedCallable {
  callable.result_type = importer.Import(callable.result_type);
  callable.interface =
      ImportCalleeInterface(importer, std::move(callable.interface));
  return callable;
}

auto ImportProjection(TypeImporter& importer, MemberProjection projection)
    -> MemberProjection {
  for (PublishedSelector& step : projection.path) {
    std::visit(
        [&](auto& selector) {
          selector.projected_type = importer.Import(selector.projected_type);
        },
        step);
  }
  return projection;
}

auto ImportScopeClass(TypeImporter& importer, ScopeClassSignature published)
    -> ScopeClassSignature {
  // The whole class crosses, and a type is the only part of it that cannot
  // cross as it stands: an identity on one pool indexes the storage that pool
  // carries, so it is answered again out of the other while every other fact
  // is what it already was. Re-pointing the types of what was handed in states
  // exactly that.
  //
  // So every type anywhere on the class is re-pointed below, and a type added
  // to it later has to join that walk. An unlisted field is carried unchanged,
  // which is right for a name, a position and an identity into the class
  // itself, and wrong for a type: it would keep indexing the pool it came from
  // and mean something else here, with nothing to report it.
  ScopeClassSignature imported{
      .class_path = std::move(published.class_path),
      .members = {},
      .callables = {},
      .generates = std::move(published.generates),
      .disable_targets = std::move(published.disable_targets),
      .modports = std::move(published.modports)};
  for (const PublishedMemberId id : published.members.Ids()) {
    PublishedMember member = published.members.Get(id);
    member.type = importer.Import(member.type);
    imported.members.Add(std::move(member));
  }
  for (const PublishedCallableId id : published.callables.Ids()) {
    imported.callables.Add(
        ImportCallable(importer, published.callables.Get(id)));
  }
  // A name a view defines by designating storage states the type each step of
  // its descent lands on, so those are types on the class like any other.
  for (PublishedModport& view : imported.modports) {
    for (PublishedModportPort& port : view.ports) {
      std::visit(
          Overloaded{
              [&](ViewDefinedPlace& place) {
                place.type = importer.Import(place.type);
                for (MemberProjection& part : place.parts) {
                  part = ImportProjection(importer, std::move(part));
                }
              },
              // What it names is a callable and the members it reads, both of
              // which are identities into the class itself.
              [](ViewComputedValue&) {}},
          port.meaning);
    }
  }
  return imported;
}

auto ImportExternalScopeClass(
    const UnitSignature& signature, const ScopeClassSignature& published,
    TypePool& into) -> ExternalScopeClass {
  TypeImportMemo memo;
  TypeImporter importer(signature.types, std::nullopt, into, memo);
  return ExternalScopeClass{
      .unit_name = signature.unit_name,
      .signature = ImportScopeClass(importer, published)};
}

auto ImportExternalClass(
    const UnitSignature& signature, const ClassSignature& published,
    TypePool& into) -> ExternalClass {
  ExternalClass cls{
      .unit_name = signature.unit_name,
      .class_path = published.class_path,
      .base = published.base,
      .is_interface_class = published.is_interface_class,
      .implements = published.implements,
      .properties = {},
      .local_property_types = {},
      .static_properties = {},
      .constructor = std::nullopt,
      .methods = {},
      .overrides = {},
      .takes_declaring_instance = published.takes_declaring_instance};
  TypeImportMemo memo;
  TypeImporter importer(signature.types, std::nullopt, into, memo);
  cls.properties = ImportProperties(importer, published.properties);
  cls.local_property_types =
      ImportTypes(importer, published.local_property_types);
  cls.static_properties =
      ImportStaticProperties(importer, published.static_properties);
  cls.constructor = published.constructor.transform(
      [&](const ExternalCalleeInterface& stated) {
        return ImportCalleeInterface(importer, stated);
      });
  cls.methods = ImportMethods(importer, published.methods);
  return cls;
}

auto ImportTypes(TypeImporter& importer, std::span<const TypeId> types)
    -> std::vector<TypeId> {
  std::vector<TypeId> imported;
  imported.reserve(types.size());
  for (const TypeId type : types) {
    imported.push_back(importer.Import(type));
  }
  return imported;
}

auto ImportProperties(
    TypeImporter& importer, const PublishedProperties& properties)
    -> PublishedProperties {
  PublishedProperties imported;
  for (const PublishedPropertyId id : properties.Ids()) {
    const PublishedProperty& property = properties.Get(id);
    imported.Add(
        PublishedProperty{
            .name = property.name, .type = importer.Import(property.type)});
  }
  return imported;
}

auto ImportStaticProperties(
    TypeImporter& importer, std::span<const PublishedProperty> properties)
    -> std::vector<PublishedProperty> {
  std::vector<PublishedProperty> imported;
  imported.reserve(properties.size());
  for (const PublishedProperty& property : properties) {
    imported.push_back(
        PublishedProperty{
            .name = property.name, .type = importer.Import(property.type)});
  }
  return imported;
}

auto ImportMethods(
    TypeImporter& importer, std::span<const PublishedMethod> methods)
    -> std::vector<PublishedMethod> {
  std::vector<PublishedMethod> imported;
  imported.reserve(methods.size());
  for (const PublishedMethod& method : methods) {
    imported.push_back(
        PublishedMethod{
            .prototype = ImportCallable(importer, method.prototype),
            .dispatch = method.dispatch});
  }
  return imported;
}

}  // namespace lyra::hir
