#pragma once

#include <cstddef>
#include <filesystem>
#include <optional>

#include "lyra/diag/diagnostic.hpp"
#include "lyra/driver/project_layout.hpp"

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
// path + every header's content + the optimization the header is compiled
// at), so a cache hit means content match by construction and no staleness
// check is needed at lookup time.
//
// What comes back is an offer and never a requirement. Whether the compiler
// accepts it is the compiler's to answer, and it answers by failing, so a
// caller passes it only where it can compile again without it -- nothing a
// build produces, and no reason a build fails, may depend on what this
// returned. `optimization` must be the one the including translation unit is
// compiled at: a header prepared under different options is refused. Returns
// nullopt when this is switched off, the compiler is not clang, or no writable
// cache directory is available.
auto EnsureCached(
    const std::filesystem::path& cxx, const std::filesystem::path& include_root,
    const Options& opts, Optimization optimization)
    -> std::optional<std::filesystem::path>;

// Drop one precompiled header, so that whoever asks next builds it again. For
// the caller that has just watched the compiler refuse this one, which is the
// only evidence there is that it had gone stale.
auto Discard(const std::filesystem::path& pch_path) -> void;

// Remove every PCH file in the active cache directory. Returns the number of
// files actually removed; a failure to resolve the cache directory surfaces
// as a diagnostic.
auto Clear(const Options& opts) -> diag::Result<std::size_t>;

}  // namespace lyra::driver::pch
