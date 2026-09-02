#include "lyra/hir/unit_signature.hpp"

#include <optional>
#include <utility>
#include <vector>

#include "lyra/hir/external_callee.hpp"
#include "lyra/hir/external_unit_object.hpp"
#include "lyra/hir/published_callable.hpp"
#include "lyra/hir/published_member.hpp"
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
      .callables = {}};
  TypeImportMemo memo;
  TypeImporter importer(signature.types, std::nullopt, into, memo);
  // The whole promise crosses, and a type is the only part of it that cannot
  // cross as it stands: an identity on a signature indexes the storage the
  // signature carries, so it is answered again out of the reader's pool while
  // every other fact is what it already was. Taking a copy and re-pointing the
  // types states exactly that, and leaves a fact added later carried rather
  // than silently dropped here.
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
  return object;
}

}  // namespace lyra::hir
