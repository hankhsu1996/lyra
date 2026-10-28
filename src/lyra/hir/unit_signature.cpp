#include "lyra/hir/unit_signature.hpp"

#include <format>
#include <optional>
#include <span>
#include <utility>
#include <variant>
#include <vector>

#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/hir/callee_interface.hpp"
#include "lyra/hir/class_declaration.hpp"
#include "lyra/hir/declared_method.hpp"
#include "lyra/hir/published_callable.hpp"
#include "lyra/hir/published_member.hpp"
#include "lyra/hir/published_modport.hpp"
#include "lyra/hir/published_target.hpp"
#include "lyra/hir/scope_class.hpp"
#include "lyra/hir/subroutine_prototype.hpp"
#include "lyra/hir/type.hpp"
#include "lyra/hir/type_definition.hpp"
#include "lyra/hir/type_import.hpp"
#include "lyra/support/def_path.hpp"

namespace lyra::hir {

auto ImportCalleeInterface(TypeImporter& importer, CalleeInterface interface)
    -> CalleeInterface {
  for (CalleeParam& param : interface.params) {
    param.type = importer.Import(param.type);
  }
  return interface;
}

auto ImportCallable(TypeImporter& importer, SubroutinePrototype callable)
    -> SubroutinePrototype {
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
      .loops = std::move(published.loops),
      .choices = std::move(published.choices),
      .disable_targets = std::move(published.disable_targets),
      .modports = std::move(published.modports),
      .clocking_blocks = std::move(published.clocking_blocks)};
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

auto ImportTypeDefinition(
    TypeImporter& importer, const TypeDefinition& definition)
    -> TypeDefinition {
  const auto fields = [&](const std::vector<UnpackedAggregateField>& declared) {
    std::vector<UnpackedAggregateField> imported;
    imported.reserve(declared.size());
    for (const UnpackedAggregateField& field : declared) {
      imported.push_back(
          UnpackedAggregateField{
              .name = field.name,
              .type = importer.Import(field.type),
              .default_init = field.default_init,
              .random = field.random});
    }
    return imported;
  };
  return std::visit(
      Overloaded{
          [&](const EnumDefinition& enumeration) -> TypeDefinition {
            return EnumDefinition{
                .base_type = importer.Import(enumeration.base_type),
                .members = enumeration.members};
          },
          [&](const UnpackedStructDefinition& structure) -> TypeDefinition {
            return UnpackedStructDefinition{.fields = fields(structure.fields)};
          },
          [&](const UnpackedUnionDefinition& overlay) -> TypeDefinition {
            return UnpackedUnionDefinition{
                .fields = fields(overlay.fields), .tagged = overlay.tagged};
          }},
      definition);
}

auto PublishedTypeDefinition(
    const UnitSignature& signature, const TypeDeclarationRef& declaration)
    -> const TypeDefinition& {
  const TypeDefinition* published =
      signature.type_definitions.Find(declaration);
  if (published == nullptr) {
    throw InternalError(
        std::format(
            "hir::PublishedTypeDefinition: '{}' published no type '{}', and a "
            "type named it -- please report this as a bug",
            declaration.unit_name, support::DisplayOf(declaration.path)));
  }
  return *published;
}

auto ReaderImporter(
    const UnitSignature& signature, TypePool& into, ClassRegistry& classes,
    CovergroupRegistry& covergroups, ScopeClassRegistry& scope_classes,
    TypeImportMemo& memo) -> TypeImporter {
  return {
      signature.types,
      into,
      memo,
      [&](ClassId id) { return classes.Import(signature.ClassKeyOf(id)); },
      [&](CovergroupId id) {
        return covergroups.Import(signature.CovergroupKeyOf(id));
      },
      [&](ScopeClassId id) {
        return scope_classes.Import(signature.ScopeClassKeyOf(id));
      }};
}

auto ImportClassDeclaration(
    TypeImporter& importer, const ClassDeclaration& declared)
    -> ClassDeclaration {
  ClassDeclaration cls{
      .class_path = declared.class_path,
      .base = declared.base.transform(
          [&](ClassId base) { return importer.ImportClass(base); }),
      .is_interface_class = declared.is_interface_class,
      .implements = {},
      .properties = ImportProperties(importer, declared.properties),
      .static_properties =
          ImportStaticProperties(importer, declared.static_properties),
      .constructor =
          declared.constructor.transform([&](const CalleeInterface& stated) {
            return ImportCalleeInterface(importer, stated);
          }),
      .methods = ImportMethods(importer, declared.methods),
      .takes_declaring_instance = declared.takes_declaring_instance};
  for (const ClassId iface : declared.implements) {
    cls.implements.push_back(importer.ImportClass(iface));
  }
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
    TypeImporter& importer, const DeclaredProperties& properties)
    -> DeclaredProperties {
  DeclaredProperties imported;
  for (const DeclaredProperty& property : properties) {
    DeclaredProperty taken = property;
    taken.type = importer.Import(property.type);
    imported.Add(std::move(taken));
  }
  return imported;
}

auto ImportStaticProperties(
    TypeImporter& importer, const DeclaredStaticProperties& properties)
    -> DeclaredStaticProperties {
  DeclaredStaticProperties imported;
  for (const DeclaredProperty& property : properties) {
    DeclaredProperty taken = property;
    taken.type = importer.Import(property.type);
    imported.Add(std::move(taken));
  }
  return imported;
}

auto ImportMethods(TypeImporter& importer, const DeclaredMethods& methods)
    -> DeclaredMethods {
  DeclaredMethods imported;
  for (const DeclaredMethod& method : methods) {
    // Only an override names a class, the one introducing what it overrides.
    MethodDispatch dispatch = std::visit(
        Overloaded{
            [&](const OverridesVirtual& overriding) -> MethodDispatch {
              return OverridesVirtual{
                  .is_pure = overriding.is_pure,
                  .introducer = importer.ImportClass(overriding.introducer),
                  .name = overriding.name};
            },
            [](const TypeAssociated& kept) -> MethodDispatch { return kept; },
            [](const NotVirtual& kept) -> MethodDispatch { return kept; },
            [](const IntroducesVirtual& kept) -> MethodDispatch {
              return kept;
            }},
        method.dispatch);
    imported.Add(
        DeclaredMethod{
            .prototype = ImportCallable(importer, method.prototype),
            .dispatch = std::move(dispatch),
            .replaces = method.replaces,
            .makes = method.makes.transform([&](CovergroupId made) {
              return importer.ImportCovergroup(made);
            })});
  }
  return imported;
}

}  // namespace lyra::hir
