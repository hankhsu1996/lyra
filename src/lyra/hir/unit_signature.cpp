#include "lyra/hir/unit_signature.hpp"

#include <optional>
#include <utility>
#include <variant>

#include "lyra/base/overloaded.hpp"
#include "lyra/hir/external_callee.hpp"
#include "lyra/hir/external_unit_object.hpp"
#include "lyra/hir/published_callable.hpp"
#include "lyra/hir/published_member.hpp"
#include "lyra/hir/published_modport.hpp"
#include "lyra/hir/published_target.hpp"
#include "lyra/hir/type.hpp"
#include "lyra/hir/type_import.hpp"

namespace lyra::hir {

auto ImportExternalUnitObject(const UnitSignature& signature, TypePool& into)
    -> ExternalUnitObject {
  const InstanceClassSignature& published = InstanceClassOf(signature);
  ExternalUnitObject object{
      .unit_name = signature.unit_name,
      .class_name = published.class_name,
      .members = {},
      .callables = {},
      .modports = published.modports};
  TypeImportMemo memo;
  TypeImporter importer(signature.types, std::nullopt, into, memo);
  // The whole promise crosses, and a type is the only part of it that cannot
  // cross as it stands: an identity on a signature indexes the storage the
  // signature carries, so it is answered again out of the reader's pool while
  // every other fact is what it already was. Taking a copy and re-pointing the
  // types states exactly that.
  //
  // So every type anywhere on the promise is re-pointed below, and a type added
  // to it later has to join that walk. A copy carries an unlisted field
  // unchanged, which is right for a name, a position and an identity into the
  // promise itself, and wrong for a type: it would keep indexing the publishing
  // unit's pool and mean something else here, with nothing to report it.
  for (const PublishedMemberId id : published.members.Ids()) {
    PublishedMember member = published.members.Get(id);
    member.type = importer.Import(member.type);
    object.members.Add(std::move(member));
  }
  for (const PublishedCallableId id : published.callables.Ids()) {
    PublishedCallable callable = published.callables.Get(id);
    callable.result_type = importer.Import(callable.result_type);
    for (ExternalCalleeParam& param : callable.params) {
      param.type = importer.Import(param.type);
    }
    object.callables.Add(std::move(callable));
  }
  // A name a view defines by designating storage states the type each step of
  // its descent lands on, so those are types on this promise like any other and
  // are answered again out of the reader's pool.
  for (PublishedModport& view : object.modports) {
    for (PublishedModportPort& port : view.ports) {
      std::visit(
          Overloaded{
              [&](ViewDefinedPlace& place) {
                place.type = importer.Import(place.type);
                for (MemberProjection& part : place.parts) {
                  for (PublishedSelector& step : part.path) {
                    std::visit(
                        [&](auto& selector) {
                          selector.projected_type =
                              importer.Import(selector.projected_type);
                        },
                        step);
                  }
                }
              },
              // What it names is a callable and the members it reads, both of
              // which are identities this record already re-points.
              [](ViewComputedValue&) {}},
          port.meaning);
    }
  }
  return object;
}

auto ImportExternalClass(
    const UnitSignature& signature, const ClassSignature& published,
    TypePool& into) -> ExternalClass {
  ExternalClass cls{
      .unit_name = signature.unit_name,
      .class_name = published.class_name,
      .base = published.base,
      .is_interface_class = published.is_interface_class,
      .members = {},
      .behaviors = {}};
  TypeImportMemo memo;
  TypeImporter importer(signature.types, std::nullopt, into, memo);
  for (const PublishedMemberId id : published.members.Ids()) {
    PublishedMember member = published.members.Get(id);
    member.type = importer.Import(member.type);
    cls.members.Add(std::move(member));
  }
  for (const PublishedBehaviorId id : published.behaviors.Ids()) {
    cls.behaviors.Add(published.behaviors.Get(id));
  }
  return cls;
}

}  // namespace lyra::hir
