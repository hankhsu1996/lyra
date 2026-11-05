#pragma once

// The code other simulators are measured on, as Lyra is told of it. Two kinds
// of suite supply it: Verilator's benchmark suite, which describes each of its
// designs itself, once, for any simulator; and the suites
// `tests/external_design_suites.yaml` describes, which ship sources and no
// description of their own. Either way a case states its sources, what reading
// them needs, its runs and how a run is checked, and what is built from that
// here is a declaration in Lyra's own form, so a case is reproduced by building
// in the directory that declaration is written to.
//
// A case is the one the benchmark suite marks as a design's quick check, or one
// run of a described suite, and what it is recorded as stopping on, and the
// front end's options it is built with, are read beside it.

#include <cstdint>
#include <filesystem>
#include <map>
#include <optional>
#include <string>
#include <string_view>
#include <vector>

#include "tools/cpp/runfiles/runfiles.h"

namespace lyra::test {

enum class DesignStep : std::uint8_t {
  kBuild,
  kRun,
  kCheck,
};

auto NameOf(DesignStep step) -> std::string_view;

// A step a case stops at and what that step says. Of a case that was run it is
// everything the step said; of the record it is words the step says there.
struct DesignStop {
  DesignStep at;
  std::string said;
};

struct DesignCase {
  // Unique across every suite: the design or suite, its configuration or case
  // directory, and its test or run.
  std::string name;
  std::string design_name;
  // The design as Lyra is told of it.
  std::string declaration;
  // What the test reads from the directory it runs in.
  std::vector<std::filesystem::path> files;
  std::vector<std::string> arguments;
  // The check of a finished run, where the design has one: a program and what
  // it is handed, run from the directory the run started in.
  std::optional<std::vector<std::string>> check;
};

// The cases, their record, and the front end's options each design is built
// with, found where the build put them.
struct DesignCorpus {
  std::vector<DesignCase> cases;
  std::map<std::string, DesignStop> recorded;
  std::map<std::string, std::vector<std::string>> options;
  std::vector<std::string> problems;
};

auto LoadDesignCorpus(const bazel::tools::cpp::runfiles::Runfiles& runfiles)
    -> DesignCorpus;

// Writes the case's declaration into `work`, and lays out beside it the
// directory its run starts in, holding what the run reads. Returns that
// directory.
auto LayOutDesign(const DesignCase& design, const std::filesystem::path& work)
    -> std::filesystem::path;

}  // namespace lyra::test
