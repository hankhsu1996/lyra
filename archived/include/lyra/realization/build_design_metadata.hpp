#pragma once

#include "lyra/metadata/design_metadata.hpp"

namespace lyra::realization {

auto BuildDesignMetadata(const metadata::DesignMetadataInputs& input)
    -> metadata::DesignMetadata;

}  // namespace lyra::realization
