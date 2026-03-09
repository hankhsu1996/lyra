#pragma once

#include "lyra/realization/design_metadata.hpp"

namespace lyra::realization {

auto BuildDesignMetadata(const DesignMetadataInputs& input) -> DesignMetadata;

}  // namespace lyra::realization
