#include "lyra/driver/pch.hpp"

#include <cstddef>
#include <filesystem>
#include <format>
#include <fstream>
#include <optional>
#include <string>
#include <string_view>
#include <system_error>
#include <utility>
#include <vector>

#include "lyra/diag/diag_code.hpp"
#include "lyra/driver/action_cache.hpp"
#include "lyra/driver/file_output.hpp"
#include "lyra/driver/project_layout.hpp"
#include "lyra/driver/subprocess.hpp"
#include "lyra/stable_hash/fingerprint.hpp"
#include "lyra/status/status.hpp"
#include "lyra/support/runtime_prelude.hpp"

namespace lyra::driver::pch {

namespace {

// Everything the compiler is told when a prelude is prepared. One list, so the
// command that prepares one and the action it is kept under cannot describe
// different things.
auto PreparationFlags(
    const PreparedHeaderFlags& accepted, OptimizationLevel optimization)
    -> std::vector<std::string> {
  std::vector<std::string> flags = DesignCompileFlags(optimization);
  flags.insert(
      flags.end(), accepted.preparing.begin(), accepted.preparing.end());
  return flags;
}

// The action that prepares a header: the compiler that prepares it, where the
// runtime tree it is prepared from is, because a prepared header records the
// absolute paths of its inputs, every file it reads by its content, and how it
// is prepared. Upgrading the compiler, switching runtime installations, editing
// any header it reads -- the C++ library's and the system's as much as the
// runtime's -- or asking for something different to go into the artifact each
// make a different action; that holds only while the last covers the whole
// command line a preparation runs.
auto PrepareHeader(
    const std::filesystem::path& cxx, const std::filesystem::path& include_root,
    const stable_hash::Fingerprint& prelude_inputs,
    const PreparedHeaderFlags& accepted, OptimizationLevel optimization)
    -> Action {
  Action action("prepare header");
  action.InputExecutable("compiler", cxx);
  std::error_code ec;
  const auto root = std::filesystem::canonical(include_root, ec);
  action.Input("runtime root", (ec ? include_root : root).string());
  action.InputDigest("prelude inputs", prelude_inputs);
  for (const auto& flag : PreparationFlags(accepted, optimization)) {
    action.Input("flag", flag);
  }
  return action;
}

auto BuildAt(
    const std::filesystem::path& cxx, const std::filesystem::path& include_root,
    const PreparedHeaderFlags& accepted, const std::filesystem::path& pch_path,
    OptimizationLevel optimization) -> diag::Result<void> {
  const auto prelude = include_root / support::kRuntimePreludeHeader;
  std::vector<std::string> args = PreparationFlags(accepted, optimization);
  args.insert(
      args.end(), {"-I", include_root.string(), "-xc++-header",
                   prelude.string(), "-o", pch_path.string()});
  const status::Phase phase("Preparing runtime header");
  auto result_or = RunProcessCaptured(cxx, args);
  if (!result_or) {
    return diag::Fail(
        diag::DiagCode::kHostIoError, std::move(result_or.error()));
  }
  if (result_or->exit_code != 0) {
    return std::unexpected(
        diag::Make(
            diag::DiagCode::kHostBuildFailed,
            "preparing the runtime header exited with {}", result_or->exit_code)
            .WithNote("{}", result_or->stderr_text));
  }
  return {};
}

// Writes the header a compile is told to include, and points the name beside
// it at `prepared`. The link is made again on every build, since the entry it
// names is the one this build found.
auto LayOutOffer(
    const std::filesystem::path& prepared, const std::filesystem::path& dir)
    -> std::optional<std::filesystem::path> {
  const std::filesystem::path header = dir / kPreludeOfferHeader;
  if (!WriteFile(
          header,
          std::format("#include \"{}\"\n", support::kRuntimePreludeHeader))) {
    return std::nullopt;
  }
  std::filesystem::path link = header;
  link += kPreparedHeaderSuffix;
  std::error_code ec;
  std::filesystem::remove(link, ec);
  std::filesystem::create_symlink(prepared, link, ec);
  if (ec) {
    return std::nullopt;
  }
  return header;
}

}  // namespace

auto AcceptedPreparedHeaderFlags(
    const std::filesystem::path& cxx, ActionCache& actions)
    -> std::optional<PreparedHeaderFlags> {
  Action action("find what the host compiler takes to prepare a header");
  action.InputExecutable("compiler", cxx);
  action.Input("probe", kCxxStandardFlag);
  for (const OptionalFlag& optional : kOptionalFlags) {
    action.Input("flag", optional.flag);
  }
  // The output is empty where the compiler prepares no header, and otherwise
  // holds one line, a mark for each optional flag in order: whether it is
  // taken.
  auto probed = actions.Execute(
      action, [&](const std::filesystem::path& output) -> diag::Result<void> {
        std::filesystem::path dir = output;
        dir += ".probe";
        const std::filesystem::path header = dir / "probe.hpp";
        if (auto written = WriteFile(header, ""); !written) {
          return written;
        }
        const auto takes = [&](std::optional<std::string_view> flag) {
          std::vector<std::string> args{std::string(kCxxStandardFlag)};
          if (flag) {
            args.emplace_back(*flag);
          }
          args.insert(
              args.end(),
              {"-xc++-header", header.string(), "-o",
               (dir / std::format("probe.hpp{}", kPreparedHeaderSuffix))
                   .string()});
          const auto run = RunProcessCaptured(cxx, args);
          return run && run->exit_code == 0 && run->stderr_text.empty() &&
                 run->stdout_text.empty();
        };
        std::string marks;
        if (takes(std::nullopt)) {
          for (const OptionalFlag& optional : kOptionalFlags) {
            marks += takes(optional.flag) ? '1' : '0';
          }
          marks += '\n';
        }
        return WriteFile(output, marks);
      });
  if (!probed) {
    return std::nullopt;
  }
  std::ifstream in(probed->path);
  std::string marks;
  if (!std::getline(in, marks) || marks.size() != kOptionalFlags.size()) {
    return std::nullopt;
  }
  PreparedHeaderFlags accepted;
  for (std::size_t i = 0; i < kOptionalFlags.size(); ++i) {
    if (marks.at(i) != '1') {
      continue;
    }
    const OptionalFlag& optional = kOptionalFlags.at(i);
    switch (optional.reach) {
      case FlagReach::kPreparing:
        accepted.preparing.emplace_back(optional.flag);
        break;
      case FlagReach::kCompiling:
        accepted.compiling.emplace_back(optional.flag);
        break;
      case FlagReach::kPreparingAndCompiling:
        accepted.preparing.emplace_back(optional.flag);
        accepted.compiling.emplace_back(optional.flag);
        break;
    }
  }
  return accepted;
}

auto OfferPrelude(
    const std::filesystem::path& cxx, const std::filesystem::path& include_root,
    const stable_hash::Fingerprint& prelude_inputs, Policy policy,
    ActionCache& actions, OptimizationLevel optimization,
    const std::filesystem::path& dir) -> std::optional<Offer> {
  if (policy == Policy::kSkip || !actions.Keeps()) {
    return std::nullopt;
  }
  const std::optional<PreparedHeaderFlags> accepted =
      AcceptedPreparedHeaderFlags(cxx, actions);
  if (!accepted) {
    return std::nullopt;
  }
  auto prepared = actions.Execute(
      PrepareHeader(cxx, include_root, prelude_inputs, *accepted, optimization),
      [&](const std::filesystem::path& pch_path) {
        return BuildAt(cxx, include_root, *accepted, pch_path, optimization);
      });
  if (!prepared) {
    return std::nullopt;
  }
  const std::optional<std::filesystem::path> header =
      LayOutOffer(prepared->path, dir);
  if (!header) {
    return std::nullopt;
  }
  Offer offer{.prepared = *std::move(prepared), .flags = {"-include"}};
  offer.flags.push_back(header->string());
  offer.flags.insert(
      offer.flags.end(), accepted->compiling.begin(),
      accepted->compiling.end());
  return offer;
}

}  // namespace lyra::driver::pch
