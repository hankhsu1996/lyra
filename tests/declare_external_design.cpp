// Writes one case of Verilator's benchmark suite into a directory as Lyra is
// told of it, for whatever measures the case outside the test that holds how
// far it gets: its declaration, the directory its run starts in, and a
// `case.json` naming that directory, the front end's options its design is
// built with, the arguments its run is given and the script that checks the
// run.
//
// Only a case that builds, runs and passes its check is written. What measures
// one is counting the work of a design that works, and a case recorded as
// stopping has no such work to count.
//
// Usage: declare_external_design <case> <directory>

#include <algorithm>
#include <filesystem>
#include <fstream>
#include <iostream>
#include <memory>
#include <nlohmann/json.hpp>
#include <string>
#include <vector>

#include "tests/framework/external_design.hpp"
#include "tools/cpp/runfiles/runfiles.h"

using bazel::tools::cpp::runfiles::Runfiles;

auto main(int argc, char** argv) -> int {
  const std::vector<std::string> args(argv, argv + argc);
  if (args.size() != 3) {
    std::cerr << "usage: declare_external_design <case> <directory>\n";
    return 2;
  }
  const std::string& name = args[1];
  const std::filesystem::path work = args[2];

  std::string error;
  const std::unique_ptr<Runfiles> runfiles{Runfiles::Create(args[0], &error)};
  if (!runfiles) {
    std::cerr << "error: failed to create runfiles: " << error << "\n";
    return 1;
  }
  const lyra::test::DesignCorpus corpus =
      lyra::test::LoadDesignCorpus(*runfiles);
  for (const std::string& problem : corpus.problems) {
    std::cerr << "error: " << problem << "\n";
  }
  if (!corpus.problems.empty()) {
    return 1;
  }

  const auto found =
      std::ranges::find(corpus.cases, name, &lyra::test::DesignCase::name);
  if (found == corpus.cases.end()) {
    std::cerr << "error: the suite has no case '" << name << "'\n";
    return 1;
  }
  if (const auto recorded = corpus.recorded.find(name);
      recorded != corpus.recorded.end()) {
    std::cerr << "error: '" << name << "' is recorded as stopping at its "
              << lyra::test::NameOf(recorded->second.at) << "\n";
    return 1;
  }

  std::filesystem::create_directories(work);
  const std::filesystem::path run = lyra::test::LayOutDesign(*found, work);
  const auto options = corpus.options.find(found->design_name);
  const nlohmann::json described = {
      {"name", found->name},
      {"run", run.lexically_relative(work).string()},
      {"options", options == corpus.options.end() ? std::vector<std::string>{}
                                                  : options->second},
      {"arguments", found->arguments},
      {"check",
       found->check ? nlohmann::json(found->check->string()) : nullptr},
  };
  std::ofstream(work / "case.json") << described.dump(2) << "\n";
  return 0;
}
