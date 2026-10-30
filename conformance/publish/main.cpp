// Publishes one night's records of the suite, one per configuration, for the
// site to show: only a whole run of the suite this program was built with, at
// the revision the caller names, on each configuration, and nothing at all
// where any record falls short.

#include <cstddef>
#include <cstdio>
#include <filesystem>
#include <memory>
#include <optional>
#include <span>
#include <string>
#include <string_view>
#include <utility>

#include <fmt/core.h>

#include "conformance/framework/process.hpp"
#include "conformance/publish/publish.hpp"
#include "tools/cpp/runfiles/runfiles.h"

using bazel::tools::cpp::runfiles::Runfiles;

namespace {

namespace framework = conformance::framework;
namespace publish = conformance::publish;

constexpr std::string_view kUsage =
    "usage: {} --revision REV --site DIR\n"
    "          [--repository URL] [--run URL] record...\n";

// The request, from the command line. `--revision` is the revision of the suite
// this program was built from, which every record has to name; `--site` is
// where the run is written for the page; `--repository` is where the suite's
// files can be read and `--run` where the run that wrote the records can be.
// The rest are the records.
auto ReadRequest(std::span<char* const> args, std::filesystem::path suite)
    -> std::optional<publish::Request> {
  publish::Request request;
  request.suite = std::move(suite);
  for (std::size_t i = 1; i < args.size(); ++i) {
    const std::string_view arg = args[i];
    const bool takes_value = arg == "--revision" || arg == "--site" ||
                             arg == "--repository" || arg == "--run";
    if (!takes_value) {
      request.records.push_back(framework::ResolveCallerPath(arg));
      continue;
    }
    if (i + 1 == args.size()) {
      return std::nullopt;
    }
    const std::string_view value = args[++i];
    if (arg == "--revision") {
      request.revision = value;
    } else if (arg == "--site") {
      request.site = framework::ResolveCallerPath(value);
    } else if (arg == "--repository") {
      request.repository = value;
    } else {
      request.run = value;
    }
  }
  if (request.revision.empty() || request.site.empty()) {
    return std::nullopt;
  }
  return request;
}

}  // namespace

auto main(int argc, char** argv) -> int {
  const std::span<char* const> args{argv, static_cast<std::size_t>(argc)};
  std::string error;
  std::unique_ptr<Runfiles> runfiles{Runfiles::Create(args.front(), &error)};
  if (!runfiles) {
    fmt::print(stderr, "failed to create runfiles: {}\n", error);
    return 1;
  }
  const std::optional<publish::Request> request = ReadRequest(
      args, std::filesystem::absolute(runfiles->Rlocation("_main/conformance"))
                .lexically_normal());
  if (!request) {
    fmt::print(stderr, kUsage, args.front());
    return 1;
  }
  if (const auto published = publish::Publish(*request); !published) {
    fmt::print(stderr, "not published: {}\n", published.error());
    return 1;
  }
  return 0;
}
