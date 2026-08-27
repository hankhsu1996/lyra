#include <algorithm>
#include <cstddef>
#include <filesystem>
#include <format>
#include <gtest/gtest.h>
#include <memory>
#include <ranges>
#include <span>
#include <string>
#include <string_view>
#include <vector>

#include <fmt/core.h>

#include "lyra/driver/runtime_export.hpp"
#include "lyra/support/runtime_prelude.hpp"
#include "lyra/support/subprocess.hpp"
#include "tools/cpp/runfiles/runfiles.h"

namespace {

using bazel::tools::cpp::runfiles::Runfiles;

// `clang -H` writes one stderr line per header it opened. Each line starts
// with one or more `.` characters denoting include depth, then a space, then
// the absolute path. Non-trace lines (e.g. warnings, "Multiple include
// guards may be useful for ..." footer) lack the leading dot or have no
// path after the prefix, and are skipped.
auto ParseHeaderTrace(std::string_view stderr_text)
    -> std::vector<std::filesystem::path> {
  std::vector<std::filesystem::path> out;
  std::size_t start = 0;
  while (start < stderr_text.size()) {
    const auto end = stderr_text.find('\n', start);
    const auto line = stderr_text.substr(
        start, end == std::string_view::npos ? end : end - start);
    start = end == std::string_view::npos ? stderr_text.size() : end + 1;
    if (line.empty() || line.front() != '.') continue;
    const auto dot_end = line.find_first_not_of('.');
    if (dot_end == std::string_view::npos) continue;
    if (line[dot_end] != ' ') continue;
    const auto path_begin = line.find_first_not_of(' ', dot_end);
    if (path_begin == std::string_view::npos) continue;
    out.emplace_back(std::string{line.substr(path_begin)});
  }
  return out;
}

// Whether a header's content is part of what names the cache entry. The
// fingerprint that does the naming walks `include_root` recursively, so a
// header under it cannot change without the cache filename changing too.
// Both the raw and the symlink-resolved form are compared, since either side
// may have dereferenced already.
auto IsFingerprintCovered(
    const std::filesystem::path& p, const std::filesystem::path& include_root)
    -> bool {
  const auto starts_under = [](const std::filesystem::path& candidate,
                               const std::filesystem::path& root) {
    const auto rel = candidate.lexically_relative(root);
    if (rel.empty()) return false;
    return rel.begin() != rel.end() && rel.begin()->string() != "..";
  };
  if (starts_under(p, include_root)) return true;
  std::error_code ec;
  const auto canonical_h = std::filesystem::weakly_canonical(p, ec);
  const auto canonical_root =
      std::filesystem::weakly_canonical(include_root, ec);
  return starts_under(canonical_h, canonical_root);
}

// The compiler's own system include directories, as the compiler reports them:
// `-E -v` prints the search list to stderr between two fixed markers.
//
// Asking beats a hardcoded prefix list. A toolchain is not obliged to live
// under the FHS roots -- clang's own resource directory sits next to wherever
// the binary was installed, so a `~/.local`, Homebrew, Nix, or module-system
// install puts genuine system headers outside `/usr` and `/opt` and a prefix
// test then reports dozens of false positives.
auto SystemIncludeDirs(const std::filesystem::path& cxx)
    -> std::vector<std::filesystem::path> {
  const std::vector<std::string> args = {"-E",        "-x", "c++",      "-v",
                                         "/dev/null", "-o", "/dev/null"};
  auto result_or = lyra::support::RunProcessCaptured(cxx, args);
  if (!result_or) return {};

  constexpr std::string_view kBegin = "#include <...> search starts here:";
  constexpr std::string_view kEnd = "End of search list.";
  const std::string_view text = result_or->stderr_text;
  const auto begin = text.find(kBegin);
  if (begin == std::string_view::npos) return {};
  const auto list_start = begin + kBegin.size();
  const auto end = text.find(kEnd, list_start);
  const auto list = text.substr(
      list_start, end == std::string_view::npos ? end : end - list_start);

  std::vector<std::filesystem::path> dirs;
  for (const auto part : std::views::split(list, '\n')) {
    std::string_view line(part.begin(), part.end());
    const auto first = line.find_first_not_of(" \t");
    if (first == std::string_view::npos) continue;
    line.remove_prefix(first);
    const auto last = line.find_last_not_of(" \t\r");
    line = line.substr(0, last + 1);
    // clang appends " (framework directory)" to framework entries.
    if (const auto paren = line.find(" ("); paren != std::string_view::npos) {
      line = line.substr(0, paren);
    }
    if (line.empty()) continue;
    std::error_code ec;
    auto canonical = std::filesystem::canonical(line, ec);
    dirs.push_back(ec ? std::filesystem::path(line) : std::move(canonical));
  }
  return dirs;
}

auto IsUnder(const std::filesystem::path& p, const std::filesystem::path& dir)
    -> bool {
  const auto rel = p.lexically_relative(dir);
  return !rel.empty() && *rel.begin() != "..";
}

// Whether a header is one the compiler validates for itself. clang stores each
// input header's mtime in the PCH and re-checks it on load, so a stdlib or libc
// upgrade surfaces as a loud load-time error rather than a silent stale cache.
// That is the safety net for inputs no content fingerprint of ours can see.
auto IsSystemPath(
    const std::filesystem::path& p,
    std::span<const std::filesystem::path> system_dirs) -> bool {
  std::error_code ec;
  auto canonical = std::filesystem::canonical(p, ec);
  const std::filesystem::path& probe = ec ? p : canonical;
  return std::ranges::any_of(system_dirs, [&](const std::filesystem::path& d) {
    return IsUnder(probe, d);
  });
}

}  // namespace

// Audits the prelude header tree: each input clang opens while building the
// PCH must fall into one of two coverage categories. If a new include slips
// in from a third-party root, CI flags it here rather than letting it become
// a silent staleness hole.
TEST(PchCoverage, EveryInputIsCovered) {
  std::string err;
  std::unique_ptr<Runfiles> runfiles{Runfiles::CreateForTest(&err)};
  ASSERT_TRUE(runfiles) << err;

  const std::filesystem::path lyra_exe = runfiles->Rlocation("_main/lyra");
  ASSERT_FALSE(lyra_exe.empty());

  auto loc_or = lyra::driver::ResolveRuntimeLocation(lyra_exe.string());
  ASSERT_TRUE(loc_or) << loc_or.error();

  // The same compiler Lyra defaults to, so the audit measures what a plain
  // `lyra run` on this host would produce.
  auto cxx_or = lyra::support::FindOnPath("clang++");
  ASSERT_TRUE(cxx_or) << cxx_or.error();
  if (cxx_or->filename().string().find("clang") == std::string::npos) {
    GTEST_SKIP() << "audit requires a clang-based compiler (resolved: "
                 << cxx_or->string() << ")";
  }

  const auto prelude =
      loc_or->include_root / lyra::support::kRuntimePreludeHeader;
  const std::vector<std::string> args = {
      "-std=c++23",   "-H",
      "-I",           loc_or->include_root.string(),
      "-xc++-header", prelude.string(),
      "-o",           "/dev/null"};

  auto result_or = lyra::support::RunProcessCaptured(*cxx_or, args);
  ASSERT_TRUE(result_or) << result_or.error();
  ASSERT_EQ(result_or->exit_code, 0) << "PCH-trace build failed:\n"
                                     << result_or->stderr_text;

  const auto headers = ParseHeaderTrace(result_or->stderr_text);
  ASSERT_FALSE(headers.empty())
      << "clang -H produced no header trace; parsing logic is likely stale";

  const auto system_dirs = SystemIncludeDirs(*cxx_or);
  ASSERT_FALSE(system_dirs.empty())
      << "could not read the compiler's system include search list; the "
         "`-E -v` parsing is likely stale";

  std::vector<std::filesystem::path> uncovered;
  for (const auto& h : headers) {
    if (IsFingerprintCovered(h, loc_or->include_root)) continue;
    if (IsSystemPath(h, system_dirs)) continue;
    uncovered.push_back(h);
  }

  if (uncovered.empty()) return;

  std::string msg = std::format(
      "PCH opened {} header(s) that are neither under include_root "
      "({}) nor in a recognized system path:\n",
      uncovered.size(), loc_or->include_root.string());
  for (const auto& u : uncovered) {
    msg += std::format("  {}\n", u.string());
  }
  msg +=
      "Either relocate them under include_root (so the content "
      "fingerprint covers them) or extend tests/pch_audit_test.cpp's "
      "IsSystemPath to recognize their root.";
  FAIL() << msg;
}
