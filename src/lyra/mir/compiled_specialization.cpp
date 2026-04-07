#include "lyra/mir/compiled_specialization.hpp"

#include <format>
#include <unordered_set>

#include "lyra/common/internal_error.hpp"

namespace lyra::mir {

auto CompiledModuleBody::FindChildSite(ChildBindingSiteId id) const
    -> const ChildInstantiationSite* {
  auto it = site_index_.find(id.value);
  if (it == site_index_.end()) {
    return nullptr;
  }
  return &child_sites[it->second];
}

void CompiledModuleBody::BuildChildSiteIndex() {
  site_index_.clear();
  site_index_.reserve(child_sites.size());
  for (uint32_t i = 0; i < child_sites.size(); ++i) {
    site_index_[child_sites[i].site.value] = i;
  }
}

auto CompiledModuleBody::GetExternalAccessRecipe(ExternalRefId id) const
    -> const ExternalAccessRecipe* {
  if (id.value >= external_refs.size()) {
    return nullptr;
  }
  return &external_refs[id.value];
}

namespace {

// Verify table integrity for canonical artifact tables.
// Called from MakeCompiledSpecialization before storing the body.
void VerifyBodyTables(const CompiledModuleBody& body) {
  // external_refs: ref_id must match vector position.
  for (uint32_t i = 0; i < body.external_refs.size(); ++i) {
    if (body.external_refs[i].ref_id.value != i) {
      throw common::InternalError(
          "VerifyBodyTables",
          std::format(
              "external_refs[{}] has ref_id={}, expected {}", i,
              body.external_refs[i].ref_id.value, i));
    }
  }

  // child_sites: ChildBindingSiteId must be unique.
  std::unordered_set<uint32_t> site_ids;
  for (const auto& cs : body.child_sites) {
    if (!site_ids.insert(cs.site.value).second) {
      throw common::InternalError(
          "VerifyBodyTables",
          std::format("duplicate ChildBindingSiteId {}", cs.site.value));
    }
  }

  // child_sites: DurableChildId must be unique within one body.
  std::unordered_set<DurableChildId, DurableChildIdHash> durable_ids;
  for (const auto& cs : body.child_sites) {
    if (!durable_ids.insert(cs.id).second) {
      throw common::InternalError(
          "VerifyBodyTables",
          std::format("duplicate DurableChildId at site {}", cs.site.value));
    }
  }
}

}  // namespace

auto MakeCompiledSpecialization(
    common::ModuleSpecId spec_id, CompiledModuleHeaderId header_id,
    const HeaderDatabase& headers, CompiledModuleBody body)
    -> CompiledSpecialization {
  if (!header_id.IsValid()) {
    throw common::InternalError(
        "MakeCompiledSpecialization", "invalid header_id");
  }
  const auto& header = headers.GetHeader(header_id);
  if (!(header.SpecId() == spec_id)) {
    throw common::InternalError(
        "MakeCompiledSpecialization",
        std::format(
            "header spec_id mismatch: header has (def={}, fp={}), "
            "expected (def={}, fp={})",
            header.SpecId().def_id.value, header.SpecId().fingerprint.value,
            spec_id.def_id.value, spec_id.fingerprint.value));
  }
  VerifyBodyTables(body);
  body.BuildChildSiteIndex();
  CompiledSpecialization result;
  result.spec_id_ = spec_id;
  result.header_id_ = header_id;
  result.body_ = std::move(body);
  return result;
}

}  // namespace lyra::mir
