#pragma once

#include <filesystem>
#include <span>
#include <vector>

#include "lyra/common/diagnostic/diagnostic.hpp"

namespace lyra::driver {

// Validate raw DPI link input paths from config/CLI merge.
// Converts to absolute paths, checks existence and regular-file status,
// deduplicates while preserving first-occurrence order.
// Returns canonical absolute paths on success.
auto ValidateDpiLinkInputs(std::span<const std::filesystem::path> raw_inputs)
    -> lyra::Result<std::vector<std::filesystem::path>>;

}  // namespace lyra::driver
