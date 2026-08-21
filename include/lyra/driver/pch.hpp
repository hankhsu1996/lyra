#pragma once

#include <cstddef>
#include <filesystem>
#include <optional>

#include "lyra/diag/diagnostic.hpp"

namespace lyra::driver::pch {

// Caller-supplied configuration for precompiled-header cache operations.
// Every Lyra-specific decision arrives here explicitly, translated from the
// command line at the CLI boundary, so no layer below re-derives one. The sole
// environment read left underneath is the XDG cache location, which belongs to
// the platform rather than to Lyra and is the same for every tool on the host.
struct Options {
  // Skip the cache entirely and compile without `-include-pch`. Threaded from
  // the `--no-pch` CLI flag.
  bool disabled = false;

  // Explicit override of where PCH artifacts live. When unset, helpers fall
  // back to `$XDG_CACHE_HOME/lyra/pch` with `$HOME/.cache/lyra/pch` as the
  // XDG-spec fallback. Threaded from the `--pch-cache-dir` CLI flag.
  std::optional<std::filesystem::path> cache_dir_override;
};

// Return the PCH path to pass via `-include-pch`, building it on demand. The
// cache filename is fully content-addressed (clang identity + include-root
// path + every header's content), so a cache hit means content match by
// construction and no staleness check is needed at lookup time. Returns
// nullopt when PCH is disabled, the compiler is not clang, or no writable
// cache directory is available -- the caller then falls back to plain
// compilation.
auto EnsureCached(
    const std::filesystem::path& cxx, const std::filesystem::path& include_root,
    const Options& opts) -> std::optional<std::filesystem::path>;

// Remove every PCH file in the active cache directory. Returns the number of
// files actually removed; a failure to resolve the cache directory surfaces
// as a diagnostic.
auto Clear(const Options& opts) -> diag::Result<std::size_t>;

}  // namespace lyra::driver::pch
