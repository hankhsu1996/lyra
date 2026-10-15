#include "lyra/driver/pch.hpp"

#include <filesystem>
#include <format>
#include <optional>
#include <string>
#include <system_error>
#include <utility>
#include <vector>

#include "lyra/diag/diag_code.hpp"
#include "lyra/driver/action_cache.hpp"
#include "lyra/driver/project_layout.hpp"
#include "lyra/driver/subprocess.hpp"
#include "lyra/stable_hash/fingerprint.hpp"
#include "lyra/status/status.hpp"
#include "lyra/support/runtime_prelude.hpp"

namespace lyra::driver::pch {

namespace {

auto IoError(std::string message) {
  return diag::Fail(diag::DiagCode::kHostIoError, std::move(message));
}

// True when the resolved compiler is clang-based. PCH file format and the
// `-include-pch` driver flag are clang-specific; gcc uses `.gch` placed next
// to the header. Sniff by basename rather than running `--version` to keep
// the check cheap. Misclassification only degrades to a non-PCH compile, so
// correctness is unaffected.
auto IsClangCompiler(const std::filesystem::path& cxx) -> bool {
  return cxx.filename().string().find("clang") != std::string::npos;
}

// Everything the compiler is told when a prelude is prepared. One list, so the
// command that prepares one and the action it is kept under cannot describe
// different things.
auto PreparationFlags(Optimization optimization) -> std::vector<std::string> {
  return {
      std::string(kCxxStandardFlag),
      std::string(OptimizationFlag(optimization)),
      std::string(kPchContentValidationFlag),
      std::string(kPchInstantiateTemplatesFlag)};
}

// The action that prepares a header: the compiler that prepares it, the
// runtime tree it is prepared from -- where that tree is, because a prepared
// header records the absolute paths of its inputs, and the digest of every
// byte under it -- and how it is prepared. Upgrading the compiler, switching
// runtime installations, editing a header, or asking for something different to
// go into the artifact each make a different action; that holds only while the
// last covers the whole command line a preparation runs.
auto PrepareHeader(
    const std::filesystem::path& cxx, const std::filesystem::path& include_root,
    const stable_hash::Fingerprint& runtime_headers, Optimization optimization)
    -> Action {
  Action action("prepare header");
  action.InputExecutable("compiler", cxx);
  std::error_code ec;
  const auto root = std::filesystem::canonical(include_root, ec);
  action.Input("runtime root", (ec ? include_root : root).string());
  action.InputDigest("runtime headers", runtime_headers);
  for (const auto& flag : PreparationFlags(optimization)) {
    action.Input("flag", flag);
  }
  return action;
}

// Validation strategy: the compiler re-checks every input header when it loads
// the prepared one, which is the safety net for the files outside
// `include_root` that no name of ours can see -- a system stdlib or libc
// upgrade surfaces as a loud error rather than as a stale header. That check
// reads content, so it agrees with the action instead of overruling it:
// everything under `include_root` is already an input by its bytes, and a
// rewrite that leaves the bytes alone cannot make the two disagree.
auto BuildAt(
    const std::filesystem::path& cxx, const std::filesystem::path& include_root,
    const std::filesystem::path& pch_path, Optimization optimization)
    -> diag::Result<void> {
  const auto prelude = include_root / support::kRuntimePreludeHeader;
  std::vector<std::string> args = PreparationFlags(optimization);
  args.insert(
      args.end(), {"-I", include_root.string(), "-xc++-header",
                   prelude.string(), "-o", pch_path.string()});
  const status::Phase phase("Preparing runtime header");
  auto result_or = RunProcessCaptured(cxx, args);
  if (!result_or) {
    return IoError(std::move(result_or.error()));
  }
  if (result_or->exit_code != 0) {
    return diag::Fail(
        diag::DiagCode::kHostBuildFailed,
        std::format(
            "PCH build exited with {}:\n{}", result_or->exit_code,
            result_or->stderr_text));
  }
  return {};
}

}  // namespace

auto EnsureCached(
    const std::filesystem::path& cxx, const std::filesystem::path& include_root,
    const stable_hash::Fingerprint& runtime_headers, Policy policy,
    ActionCache& actions, Optimization optimization)
    -> std::optional<ActionResult> {
  if (policy == Policy::kSkip || !actions.Keeps() || !IsClangCompiler(cxx)) {
    return std::nullopt;
  }
  auto prepared = actions.Execute(
      PrepareHeader(cxx, include_root, runtime_headers, optimization),
      [&](const std::filesystem::path& pch_path) {
        return BuildAt(cxx, include_root, pch_path, optimization);
      });
  if (!prepared) {
    return std::nullopt;
  }
  return *std::move(prepared);
}

}  // namespace lyra::driver::pch
