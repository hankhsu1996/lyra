#pragma once

#include "lyra/link/design_metadata.hpp"

namespace lyra::link {

auto BuildDesignMetadata(const DesignMetadataInputs& input) -> DesignMetadata;

}  // namespace lyra::link
