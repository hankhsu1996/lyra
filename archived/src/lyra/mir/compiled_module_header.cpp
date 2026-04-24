#include "lyra/mir/compiled_module_header.hpp"

#include <algorithm>
#include <format>

#include "lyra/common/internal_error.hpp"
#include "lyra/mir/compiled_specialization.hpp"
#include "lyra/mir/connection_recipe.hpp"

namespace lyra::mir {

auto CompiledModuleHeader::Create(
    common::ModuleSpecId spec_id, common::ModuleDefId def_id,
    std::vector<PortEntry> ports) -> CompiledModuleHeader {
  CompiledModuleHeader header;
  header.spec_id_ = spec_id;
  header.def_id_ = def_id;
  header.ports_ = std::move(ports);
  return header;
}

auto CompiledModuleHeader::FindPort(SymbolId sym) const -> const PortEntry* {
  auto it = std::ranges::find_if(
      ports_, [&](const PortEntry& e) { return e.sym == sym; });
  if (it == ports_.end()) {
    return nullptr;
  }
  return &*it;
}

auto HeaderDatabase::Add(CompiledModuleHeader header)
    -> CompiledModuleHeaderId {
  auto spec = header.SpecId();
  auto id = CompiledModuleHeaderId{static_cast<uint32_t>(headers_.size())};
  auto [it, inserted] = spec_index_.emplace(spec, id);
  if (!inserted) {
    throw common::InternalError(
        "HeaderDatabase::Add", std::format(
                                   "duplicate ModuleSpecId (def={}, fp={})",
                                   spec.def_id.value, spec.fingerprint.value));
  }
  headers_.push_back(std::move(header));
  return id;
}

auto HeaderDatabase::GetHeader(CompiledModuleHeaderId id) const
    -> const CompiledModuleHeader& {
  if (!id.IsValid() || id.value >= headers_.size()) {
    throw common::InternalError(
        "HeaderDatabase::GetHeader",
        std::format("invalid or out-of-range header id {}", id.value));
  }
  return headers_[id.value];
}

auto HeaderDatabase::GetHeader(common::ModuleSpecId spec) const
    -> const CompiledModuleHeader& {
  auto it = spec_index_.find(spec);
  if (it == spec_index_.end()) {
    throw common::InternalError(
        "HeaderDatabase::GetHeader",
        std::format(
            "spec (def={}, fp={}) not registered", spec.def_id.value,
            spec.fingerprint.value));
  }
  return headers_[it->second.value];
}

auto HeaderDatabase::FindPortEntry(
    common::ModuleSpecId spec, SymbolId port_sym) const -> const PortEntry* {
  auto it = spec_index_.find(spec);
  if (it == spec_index_.end()) {
    return nullptr;
  }
  return headers_[it->second.value].FindPort(port_sym);
}

auto GetChildPortContract(
    const CompiledModuleBody& body, const HeaderDatabase& headers,
    ChildBindingSiteId site, SymbolId child_port_sym) -> ChildPortContract {
  const auto* child_site = body.FindChildSite(site);
  if (child_site == nullptr) {
    throw common::InternalError(
        "GetChildPortContract",
        std::format("child binding site {} not found", site.value));
  }
  const auto& header = headers.GetHeader(child_site->child_spec);
  const auto* port = header.FindPort(child_port_sym);
  if (port == nullptr) {
    throw common::InternalError(
        "GetChildPortContract",
        std::format(
            "port sym {} not found in child header", child_port_sym.value));
  }
  return ChildPortContract{
      .slot = port->slot, .type = port->type, .dir = port->dir};
}

}  // namespace lyra::mir
