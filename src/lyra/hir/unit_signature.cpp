#include "lyra/hir/unit_signature.hpp"

#include <optional>

#include "lyra/hir/external_unit_object.hpp"
#include "lyra/hir/published_member.hpp"
#include "lyra/hir/type_import.hpp"
#include "lyra/hir/type_pool.hpp"

namespace lyra::hir {

auto ImportExternalUnitObject(const UnitSignature& signature, TypePool& into)
    -> ExternalUnitObject {
  const InstanceClassSignature& published = InstanceClassOf(signature);
  ExternalUnitObject object{
      .unit_name = signature.unit_name,
      .class_name = published.class_name,
      .members = {}};
  TypeImportMemo memo;
  TypeImporter importer(signature.types, std::nullopt, into, memo);
  for (const PublishedMemberId id : published.members.Ids()) {
    const PublishedMember& member = published.members.Get(id);
    object.members.Add(
        PublishedMember{
            .name = member.name,
            .type = importer.Import(member.type),
            .storage = member.storage});
  }
  return object;
}

}  // namespace lyra::hir
