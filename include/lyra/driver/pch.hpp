#pragma once

#include <cstddef>
#include <filesystem>
#include <optional>

#include "lyra/diag/diagnostic.hpp"

namespace lyra::driver::pch {

// Caller-supplied configuration for precompiled-header cache operations.
// All fields are explicit; helpers in this namespace do not read environment
// variables themselves. The translation of CLI flags and environment hints
// into an Options value lives at the CLI layer, so lower layers see a single
// source of truth.
struct Options {
  // When true, EnsureCached returns nullopt and the caller compiles without
  // `-include-pch`. Threaded from the `--no-pch` CLI flag (which may itself
  // be raised by the `LYRA_NO_PCH` environment hint at the CLI boundary).
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
