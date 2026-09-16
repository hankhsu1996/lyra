#pragma once

#include <filesystem>
#include <string_view>

#include "lyra/diag/diagnostic.hpp"

namespace lyra::driver {

// Writes `content` to `path`, creating the parent directories. A file already
// holding exactly that content is left alone rather than rewritten: a
// generator produces the same bytes on every run, and a cache that validates
// by timestamp -- clang's precompiled header does -- rejects what it was just
// handed when an unchanged file gets a new modification time.
auto WriteFile(const std::filesystem::path& path, std::string_view content)
    -> diag::Result<void>;

// Places a copy of `from` at `to`, under the same rule. The bytes are written
// rather than the file copied, which is also what leaves the result writable:
// sources staged for the running binary are read-only, and a copy preserving
// that would stop a later emit replacing its own previous one.
auto CopyFile(
    const std::filesystem::path& from, const std::filesystem::path& to)
    -> diag::Result<void>;

}  // namespace lyra::driver
