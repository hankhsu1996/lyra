#include "lyra/driver/pch.hpp"

#include <filesystem>
#include <format>
#include <optional>
#include <string>
#include <system_error>
#include <utility>
#include <vector>

#include "lyra/diag/diag_code.hpp"
#include "lyra/driver/artifact_store.hpp"
#include "lyra/driver/project_layout.hpp"
#include "lyra/support/runtime_prelude.hpp"
#include "lyra/support/subprocess.hpp"

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
// command that prepares one and the name it is kept under cannot describe
// different things.
auto PreparationFlags(Optimization optimization) -> std::vector<std::string> {
  return {
      std::string(kCxxStandardFlag),
      std::string(OptimizationFlag(optimization)),
      std::string(kPchContentValidationFlag),
      std::string(kPchInstantiateTemplatesFlag)};
}

// The name a prepared header is kept under: the compiler that prepared it, the
// runtime tree it was prepared from -- where that tree is, because a prepared
// header records the absolute paths of its inputs, and every byte under it --
// and how it was prepared. Upgrading the compiler, switching runtime
// installations, editing a header, or asking for something different to go
// into the artifact each name a different entry, so two entries with one name
// are equivalent by construction; that holds only while the last covers the
// whole command line a preparation runs.
auto PreparedHeaderName(
    const std::filesystem::path& cxx, const std::filesystem::path& include_root,
    Optimization optimization) -> ContentName {
  ContentNamer namer;
  namer.AddExecutable("compiler", cxx);
  std::error_code ec;
  const auto root = std::filesystem::canonical(include_root, ec);
  namer.Add("runtime root", (ec ? include_root : root).string());
  namer.AddTree("runtime headers", include_root);
  for (const auto& flag : PreparationFlags(optimization)) {
    namer.Add("flag", flag);
  }
  return namer.Finish();
}

// Prepares the header under a name of its own and renames it into place, so
// concurrent builds preparing the same one each leave a whole file behind and
// none reads a partial one.
//
// Validation strategy: the compiler re-checks every input header when it loads
// the prepared one, which is the safety net for the files outside
// `include_root` that no name of ours can see -- a system stdlib or libc
// upgrade surfaces as a loud error rather than as a stale header. That check
// reads content, so it agrees with the name instead of overruling it:
// everything under `include_root` is already named by its bytes, and a rewrite
// that leaves the bytes alone cannot make the two disagree.
auto BuildAt(
    const std::filesystem::path& cxx, const std::filesystem::path& include_root,
    const std::filesystem::path& pch_path, Optimization optimization)
    -> diag::Result<void> {
  const auto prelude = include_root / support::kRuntimePreludeHeader;
  const auto tmp = TemporaryBeside(pch_path);
  std::vector<std::string> args = PreparationFlags(optimization);
  args.insert(
      args.end(), {"-I", include_root.string(), "-xc++-header",
                   prelude.string(), "-o", tmp.string()});
  auto result_or = support::RunProcessCaptured(cxx, args);
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
    Policy policy, const std::optional<std::filesystem::path>& store,
    Optimization optimization) -> std::optional<std::filesystem::path> {
  if (policy == Policy::kSkip || !store || !IsClangCompiler(cxx)) {
    return std::nullopt;
  }
  const ContentName name = PreparedHeaderName(cxx, include_root, optimization);
  if (auto found = FindStored(*store, kStoredHeaderDir, name)) {
    return found;
  }
  const auto pch_path = StoredPath(*store, kStoredHeaderDir, name);
  std::error_code ec;
  std::filesystem::create_directories(pch_path.parent_path(), ec);
  if (ec) {
    return std::nullopt;
  }
  if (auto r = BuildAt(cxx, include_root, pch_path, optimization); !r) {
    return std::nullopt;
  }
  TrimStore(*store);
  return pch_path;
}

auto Discard(const std::filesystem::path& pch_path) -> void {
  std::error_code ec;
  std::filesystem::remove(pch_path, ec);
}

}  // namespace lyra::driver::pch
