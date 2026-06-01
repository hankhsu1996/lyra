#pragma once

#include <expected>
#include <filesystem>
#include <string>

namespace lyra::test {

// Create a unique temp directory using mkdtemp. Returns the absolute path on
// success, or an error description on failure. Never throws.
auto MakeTempCaseDir() -> std::expected<std::filesystem::path, std::string>;

}  // namespace lyra::test
