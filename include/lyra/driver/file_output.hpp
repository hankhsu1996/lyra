#pragma once

#include <filesystem>
#include <string_view>

#include "lyra/diag/diagnostic.hpp"

namespace lyra::driver {

// Writes `content` to `path`, creating the parent directories and replacing any
// previous file.
auto WriteFile(const std::filesystem::path& path, std::string_view content)
    -> diag::Result<void>;

// Copies `from` over `to` and leaves the result owner-writable. Sources staged
// for the running binary are read-only, so without the permission fix a second
// emit into the same directory could not replace its own previous copy.
auto CopyFileWritable(
    const std::filesystem::path& from, const std::filesystem::path& to)
    -> diag::Result<void>;

}  // namespace lyra::driver
