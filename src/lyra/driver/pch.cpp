#include "lyra/driver/pch.hpp"

#include <algorithm>
#include <chrono>
#include <cstddef>
#include <cstdint>
#include <cstdlib>
#include <expected>
#include <filesystem>
#include <format>
#include <fstream>
#include <functional>
#include <iterator>
#include <random>
#include <string>
#include <system_error>
#include <utility>
#include <vector>

#include "lyra/diag/diag_code.hpp"
#include "lyra/driver/project_layout.hpp"
#include "lyra/support/subprocess.hpp"

namespace lyra::driver::pch {

namespace {

auto IoError(std::string message) {
  return diag::HostError(diag::DiagCode::kHostIoError, std::move(message));
}

// True when the resolved compiler is clang-based. PCH file format and the
// `-include-pch` driver flag are clang-specific; gcc uses `.gch` placed next
// to the header. Sniff by basename rather than running `--version` to keep
// the check cheap. Misclassification only degrades to a non-PCH compile, so
// correctness is unaffected.
auto IsClangCompiler(const std::filesystem::path& cxx) -> bool {
  return cxx.filename().string().find("clang") != std::string::npos;
}

// Read the file at `path` into `out`, appended. Bulk seek + read is much
// faster than the `istreambuf_iterator` idiom on inputs of any meaningful
// size. Silently no-ops on open/read failure -- the caller treats partial
// content as a fingerprint divergence, which is the correct outcome anyway.
auto AppendFileContent(const std::filesystem::path& path, std::string& out)
    -> void {
  std::ifstream f(path, std::ios::binary | std::ios::ate);
  if (!f) return;
  const auto size = static_cast<std::streamsize>(f.tellg());
  if (size <= 0) return;
  const auto offset = out.size();
  out.resize(offset + static_cast<std::size_t>(size));
  f.seekg(0);
  f.read(std::next(out.data(), static_cast<std::ptrdiff_t>(offset)), size);
}

// Resolve the cache directory. Order: caller-supplied override first, then
// the XDG-spec defaults. The CLI layer is responsible for surfacing any
// environment hints (e.g. `LYRA_NO_PCH`) into the Options value; this helper
// limits its env reads to `XDG_CACHE_HOME` and `HOME`, both of which are
// XDG-standard rather than Lyra-specific.
auto ResolveCacheDir(const Options& opts)
    -> std::expected<std::filesystem::path, std::string> {
  std::vector<std::filesystem::path> candidates;
  if (opts.cache_dir_override) {
    candidates.push_back(*opts.cache_dir_override);
  } else {
    const auto from_env =
        [](const char* var) -> std::optional<std::filesystem::path> {
      const char* v = std::getenv(var);
      if (v == nullptr || *v == '\0') return std::nullopt;
      return std::filesystem::path(v);
    };
    if (auto p = from_env("XDG_CACHE_HOME")) {
      candidates.push_back(*p / "lyra/pch");
    }
    if (auto p = from_env("HOME")) {
      candidates.push_back(*p / ".cache/lyra/pch");
    }
  }

  for (const auto& dir : candidates) {
    std::error_code ec;
    std::filesystem::create_directories(dir, ec);
    if (!ec) return dir;
  }
  return std::unexpected("no writable PCH cache directory found");
}

// Identity of the compiler binary, condensed into a 64-bit hash. The
// realpath catches `$CXX` pointing at a different installation; the mtime
// catches an in-place upgrade of the same binary. Together they ensure two
// clang versions never share a PCH file (the PCH binary format is
// clang-version-sensitive).
auto HashCxxIdentity(const std::filesystem::path& cxx) -> std::uint64_t {
  std::error_code ec;
  const auto canonical = std::filesystem::canonical(cxx, ec);
  std::string key = ec ? cxx.string() : canonical.string();
  std::error_code mtime_ec;
  const auto mtime = std::filesystem::last_write_time(canonical, mtime_ec);
  if (!mtime_ec) {
    const auto nanos = std::chrono::duration_cast<std::chrono::nanoseconds>(
                           mtime.time_since_epoch())
                           .count();
    key.push_back('\0');
    key += std::format("{}", nanos);
  }
  return std::hash<std::string>{}(key);
}

// Canonical path of the include root, hashed. Distinguishes cache entries
// when two runtime installations exist side by side; PCH files store
// absolute paths to their inputs and cannot be shared across trees.
auto HashIncludeRootPath(const std::filesystem::path& include_root)
    -> std::uint64_t {
  std::error_code ec;
  const auto canonical = std::filesystem::canonical(include_root, ec);
  const std::string key = ec ? include_root.string() : canonical.string();
  return std::hash<std::string>{}(key);
}

// Content fingerprint of every `.hpp` under `include_root`. Concatenates each
// file's relative path and full byte content into one buffer (NUL-separated
// to keep boundaries unambiguous) and hashes the buffer with std::hash. Any
// edit, addition, or removal under `include_root` perturbs the buffer and
// therefore the hash, driving the cache filename to a new value.
auto HeaderTreeFingerprint(const std::filesystem::path& include_root)
    -> std::uint64_t {
  std::vector<std::filesystem::path> paths;
  std::error_code ec;
  for (const auto& e : std::filesystem::recursive_directory_iterator(
           include_root,
           std::filesystem::directory_options::follow_directory_symlink, ec)) {
    if (e.is_regular_file() && e.path().extension() == ".hpp") {
      paths.push_back(e.path());
    }
  }
  std::ranges::sort(paths);

  std::string buf;
  for (const auto& p : paths) {
    buf += p.lexically_relative(include_root).generic_string();
    buf.push_back('\0');
    AppendFileContent(p, buf);
    buf.push_back('\0');
  }
  return std::hash<std::string>{}(buf);
}

// Cache filename derived from cxx identity + include-root path + tree
// content. The triple provides three independent invalidation axes: upgrading
// the compiler, swapping runtime installations, or editing any header. Two
// cache entries with the same name are byte-equivalent by construction.
auto CacheFilename(
    const std::filesystem::path& cxx, const std::filesystem::path& include_root)
    -> std::string {
  return std::format(
      "prelude-{:016x}-{:016x}-{:016x}.pch", HashCxxIdentity(cxx),
      HashIncludeRootPath(include_root), HeaderTreeFingerprint(include_root));
}

// 64 bits of entropy for a tmp filename suffix. Two random_device draws are
// concatenated because the underlying RNG only guarantees 32 bits per call.
auto RandomTmpSuffix() -> std::string {
  std::random_device rd;
  const auto hi = static_cast<std::uint64_t>(rd());
  const auto lo = static_cast<std::uint64_t>(rd());
  return std::format("{:08x}{:08x}", hi, lo);
}

// Build the PCH at a per-call tmp path, then rename atomically. Concurrent
// shards racing on the same final cache file are harmless: the rename is
// last-writer-wins on a fully-formed PCH; tmp paths are unique per call so
// writers do not corrupt each other.
//
// Validation strategy: clang stores each input header's mtime in the PCH and
// validates them on load by default, so a system-stdlib or libc upgrade
// surfaces as a loud load-time error rather than a stale PCH. We rely on
// that default for files outside `include_root`; everything inside
// `include_root` is captured proactively by the cache filename's content
// fingerprint, so the two layers together produce a coherent cache.
auto BuildAt(
    const std::filesystem::path& cxx, const std::filesystem::path& include_root,
    const std::filesystem::path& pch_path) -> diag::Result<void> {
  const auto prelude = include_root / kPreludeHeader;
  const auto tmp = pch_path.parent_path() /
                   std::format(".prelude.pch.tmp.{}", RandomTmpSuffix());
  const std::vector<std::string> args = {
      std::string(kCxxStandardFlag),
      "-I",
      include_root.string(),
      "-xc++-header",
      prelude.string(),
      "-o",
      tmp.string()};
  auto result_or = support::RunProcessCaptured(cxx, args);
  if (!result_or) {
    return IoError(std::move(result_or.error()));
  }
  if (result_or->exit_code != 0) {
    return diag::HostError(
        diag::DiagCode::kHostBuildFailed,
        std::format(
            "PCH build exited with {}:\n{}", result_or->exit_code,
            result_or->stderr_text));
  }
  std::error_code ec;
  std::filesystem::rename(tmp, pch_path, ec);
  if (ec) {
    std::filesystem::remove(tmp, ec);
    return IoError(
        std::format(
            "failed to install PCH at '{}': {}", pch_path.string(),
            ec.message()));
  }
  return {};
}

}  // namespace

auto EnsureCached(
    const std::filesystem::path& cxx, const std::filesystem::path& include_root,
    const Options& opts) -> std::optional<std::filesystem::path> {
  if (opts.disabled) return std::nullopt;
  if (!IsClangCompiler(cxx)) return std::nullopt;

  auto cache_or = ResolveCacheDir(opts);
  if (!cache_or) return std::nullopt;
  const auto pch_path = *cache_or / CacheFilename(cxx, include_root);

  std::error_code ec;
  if (std::filesystem::exists(pch_path, ec) && !ec) {
    return pch_path;
  }
  if (auto r = BuildAt(cxx, include_root, pch_path); !r) {
    return std::nullopt;
  }
  return pch_path;
}

auto Clear(const Options& opts) -> diag::Result<std::size_t> {
  auto dir_or = ResolveCacheDir(opts);
  if (!dir_or) {
    return IoError(std::move(dir_or.error()));
  }
  std::size_t removed = 0;
  std::error_code ec;
  for (const auto& e : std::filesystem::directory_iterator(*dir_or, ec)) {
    if (!e.is_regular_file()) continue;
    if (!e.path().filename().string().starts_with("prelude-")) continue;
    std::error_code remove_ec;
    if (std::filesystem::remove(e.path(), remove_ec)) {
      ++removed;
    }
  }
  return removed;
}

}  // namespace lyra::driver::pch
