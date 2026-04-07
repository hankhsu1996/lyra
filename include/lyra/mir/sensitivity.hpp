#pragma once

#include <optional>
#include <span>
#include <variant>
#include <vector>

#include "lyra/mir/arena.hpp"
#include "lyra/mir/construction_input.hpp"
#include "lyra/mir/external_ref.hpp"
#include "lyra/mir/routine.hpp"
#include "lyra/mir/terminator.hpp"

namespace lyra::mir {

// Post-binding env: identity-based bindings resolved through construction.
// Used after BuildResolvedExternalRefBindings.
struct PostBindingSensitivityEnv {
  const std::vector<ResolvedExternalRefBinding>* bindings = nullptr;
  const ConstructionInput* construction = nullptr;
};

// Pre-binding env: pre-computed ExternalRefId -> design-global slot.
// Used during body lowering where bindings don't exist yet but
// cross_instance_places is available. Each entry corresponds 1:1 to
// ExternalRefId.value. nullopt entries are hard errors.
struct PreBindingSensitivityEnv {
  std::span<const std::optional<uint32_t>> resolved_slots;
};

// Tagged environment for resolving ExternalRefId reads during sensitivity
// collection. When present, ExternalRefId operands are resolved to
// design-global triggers. When absent (nullopt), ExternalRefId in process
// MIR is a hard error. Design-level processes pass nullopt.
using SensitivityExternalRefEnv =
    std::variant<PostBindingSensitivityEnv, PreBindingSensitivityEnv>;

// Collect the sensitivity set for a process.
auto CollectSensitivity(
    const Process& process, const Arena& arena,
    const std::optional<SensitivityExternalRefEnv>& ext_ref_env = std::nullopt)
    -> std::vector<WaitTrigger>;

}  // namespace lyra::mir
