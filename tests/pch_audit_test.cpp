#include <algorithm>
#include <array>
#include <cstddef>
#include <filesystem>
#include <format>
#include <gtest/gtest.h>
#include <memory>
#include <string>
#include <string_view>
#include <vector>

#include <fmt/core.h>

#include "lyra/driver/runtime_export.hpp"
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

// A path is "fingerprint-covered" if it lives under `include_root`. The
// HeaderTreeFingerprint walks `include_root` recursively, so any content
// change to a covered header drives a different cache filename. Compare
// both the raw runfiles-relative form and the symlink-resolved form so the
// check is robust against either side dereferencing first.
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

// A path is "system-covered" if it lives in a well-known system include
// path. clang stores each input header's mtime in the PCH and re-validates
// them on load by default, so a system-stdlib or libc upgrade that bumps
// header mtimes surfaces as a loud load-time error rather than a silent
// stale PCH; this is the safety net for inputs the fingerprint cannot see.
auto IsSystemPath(const std::filesystem::path& p) -> bool {
  static const std::array<std::string_view, 6> kRoots = {
      "/usr/include",   "/usr/lib", "/usr/local/include",
      "/usr/local/lib", "/opt",     "/Library"};
  const auto s = p.string();
  return std::ranges::any_of(
      kRoots, [&](std::string_view r) { return s.starts_with(r); });
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

  auto cxx_or = lyra::support::ResolveCxxCompiler();
  ASSERT_TRUE(cxx_or) << cxx_or.error();
  if (cxx_or->filename().string().find("clang") == std::string::npos) {
    GTEST_SKIP() << "audit requires clang-based $CXX (resolved: "
                 << cxx_or->string() << ")";
  }

  const auto prelude = loc_or->include_root / "lyra/runtime/prelude.hpp";
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

  std::vector<std::filesystem::path> uncovered;
  for (const auto& h : headers) {
    if (IsFingerprintCovered(h, loc_or->include_root)) continue;
    if (IsSystemPath(h)) continue;
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
