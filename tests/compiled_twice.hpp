#pragma once

// What the checks that compile one design twice share: the two compilations
// themselves, the design written to show an ordering, and the run over the
// corpus.
//
// The two compilations of a case are deliberately alive at the same time. Each
// owns its arena, so the symbols of one cannot occupy the addresses of the
// other, and an order taken from an address rather than from the design comes
// out as a difference every run. Compiling one and releasing it before
// compiling the other would let the allocator hand back the same addresses, and
// the comparison would pass while testing nothing -- measured across two
// processes on a small design, four of seven pairs agreed by luck.
//
// The corpus is the input because the claim is about designs rather than about
// one design. A leak can only surface through a construct that reaches it, so
// the set of designs to compile is the set the project keeps.

#include <algorithm>
#include <cstddef>
#include <filesystem>
#include <fstream>
#include <gtest/gtest.h>
#include <memory>
#include <string>
#include <string_view>
#include <system_error>
#include <unistd.h>
#include <vector>

#include <fmt/format.h>
#include <slang/driver/Driver.h>

#include "lyra/compiler/compile.hpp"
#include "tests/framework/conformance_case.hpp"
#include "tools/cpp/runfiles/runfiles.h"

namespace lyra::test {

// How many units the second compilation lowers at once. The first lowers one
// at a time, so units finish in a different order, and an answer that depended
// on which unit finished first would come out as a difference.
inline constexpr std::size_t kWide = 4;

// One compilation's inputs, held together because the driver owns the text
// every span points into and the elaboration points into it.
struct Compilation {
  std::unique_ptr<slang::driver::Driver> driver;
  compiler::FrontEndResult front;
};

inline auto Elaborate(const ConformanceCase& test_case) -> Compilation {
  Compilation out;
  out.driver = std::make_unique<slang::driver::Driver>();
  out.driver->addStandardArgs();

  std::vector<std::string> args{"lyra"};
  for (const std::string& top : test_case.tops) {
    args.emplace_back("--top");
    args.push_back(top);
  }
  for (const std::string& arg : test_case.front_end_args) {
    args.push_back(arg);
  }
  for (const std::filesystem::path& supporting : test_case.supporting_sources) {
    args.push_back(supporting.string());
  }
  args.push_back(test_case.entry.string());

  std::vector<const char*> argv;
  argv.reserve(args.size());
  for (const std::string& arg : args) {
    argv.push_back(arg.c_str());
  }
  if (!out.driver->parseCommandLine(
          static_cast<int>(argv.size()), argv.data())) {
    return out;
  }
  out.front = compiler::RunFrontEnd(*out.driver);
  return out;
}

// Where two texts first disagree, named by line. A whole-text mismatch says
// nothing about which of thousands of lines moved.
inline auto FirstDifference(std::string_view left, std::string_view right)
    -> std::string {
  std::size_t line = 1;
  std::size_t left_at = 0;
  std::size_t right_at = 0;
  while (left_at <= left.size() && right_at <= right.size()) {
    const std::size_t left_end = left.find('\n', left_at);
    const std::size_t right_end = right.find('\n', right_at);
    const std::string_view left_line = left.substr(left_at, left_end - left_at);
    const std::string_view right_line =
        right.substr(right_at, right_end - right_at);
    if (left_line != right_line) {
      return fmt::format(
          "line {}\n    first:  {}\n    second: {}", line, left_line,
          right_line);
    }
    if (left_end == std::string_view::npos ||
        right_end == std::string_view::npos) {
      break;
    }
    left_at = left_end + 1;
    right_at = right_end + 1;
    ++line;
  }
  return {};
}

// A directory of this run's own for one design's two compilations, removed once
// they have been compared. Named by process because a sharded run is several
// processes over one corpus, and by case because one process compiles many.
class ScratchDirectory {
 public:
  explicit ScratchDirectory(std::string_view name) {
    std::string flat(name);
    std::ranges::replace(flat, '/', '-');
    root_ = std::filesystem::temp_directory_path() /
            fmt::format("lyra-repeat-{}-{}", ::getpid(), flat);
  }

  ScratchDirectory(const ScratchDirectory&) = delete;
  auto operator=(const ScratchDirectory&) -> ScratchDirectory& = delete;
  ScratchDirectory(ScratchDirectory&&) = delete;
  auto operator=(ScratchDirectory&&) -> ScratchDirectory& = delete;

  ~ScratchDirectory() {
    std::error_code ignored;
    std::filesystem::remove_all(root_, ignored);
  }

  [[nodiscard]] auto Under(std::string_view name) const
      -> std::filesystem::path {
    return root_ / name;
  }

 private:
  std::filesystem::path root_;
};

// A design these checks are known to notice on. What the corpus holds is
// decided by what IEEE 1800 requires, not by what would expose an ordering, and
// only a handful of its cases read enough signals in one place for the order
// among them to show -- with an ordering removed on purpose, nine of six
// hundred failed, and which nine moved from run to run. So the corpus alone
// would lose its grip on this the day those cases were edited, and nothing
// would report it; this design fails every run instead.
//
// Each process reads signals no other reads, because one set of signals is a
// single draw however many processes share it -- the same signals order the
// same way everywhere in one compilation.
//
// The three forms are here because they reach what is being held to by
// different routes: what wakes a procedure and what an expression reads are
// answered separately, and a set reached through instance names (LRM 23.7
// downward name) resolves to a different shape of reference than one naming the
// enclosing scope's own signals. Holding all of them in one design is what puts
// those shapes in the same comparison.
inline constexpr std::string_view kWideSensitivity = R"(
module Leaf;
  logic [31:0] x;
endmodule

module Top;
  logic [31:0] a0, a1, a2, a3;
  logic [31:0] b0, b1, b2, b3;
  logic [31:0] c0, c1, c2, c3;
  logic [31:0] d0, d1, d2, d3;
  logic [31:0] wa, wb, wc, wd, we;

  Leaf i0 ();
  Leaf i1 ();
  Leaf i2 ();
  Leaf i3 ();
  Leaf i4 ();
  Leaf i5 ();
  Leaf i6 ();
  Leaf i7 ();

  always_comb wa = a0 + a1 + a2 + a3;
  always_comb wb = b0 ^ b1 ^ b2 ^ b3;

  assign wc = c0 | c1 | c2 | c3;
  assign wd = d0 + d1 - d2 + d3;
  assign we = i0.x + i1.x + i2.x + i3.x + i4.x + i5.x + i6.x + i7.x;

  initial $display("%0d", wa + wb + wc + wd + we);
endmodule
)";

inline auto WriteWideSensitivityDesign(const std::filesystem::path& dir)
    -> ConformanceCase {
  std::filesystem::create_directories(dir);
  const std::filesystem::path path = dir / "wide_sensitivity.sv";
  std::ofstream out(path);
  out << kWideSensitivity;
  out.close();

  ConformanceCase written;
  written.id = "wide_sensitivity";
  written.directory = dir;
  written.entry = path;
  written.tops = {"Top"};
  return written;
}

// Runs one test per corpus design, each made by `make` from its case, then
// holds the run to having compared most of what it attempted.
//
// The floor is the other direction of the claim. Every comparison is an
// equality, and equality is what a compiler that produced nothing would also
// report, so a run that reached `compared_form` for almost none of the designs
// it attempted has not measured the property whatever it printed. A design that
// reached none skipped, so the count is already kept and is read rather than
// tallied again; a sharded run attempts only its own share, which is what makes
// the ratio the thing to hold rather than the total.
template <typename MakeTest>
auto RunOverCorpus(
    int argc, char** argv, std::string_view compared_form, MakeTest make)
    -> int {
  testing::InitGoogleTest(&argc, argv);

  std::string error;
  const std::unique_ptr<bazel::tools::cpp::runfiles::Runfiles> runfiles{
      bazel::tools::cpp::runfiles::Runfiles::CreateForTest(&error)};
  if (!runfiles) {
    fmt::print(stderr, "{}\n", error);
    return 1;
  }

  static const std::vector<ConformanceCase> kCases =
      LoadConformanceCases(runfiles->Rlocation("_main/tests/conformance"));
  if (kCases.empty()) {
    fmt::print(
        stderr,
        "the corpus holds no designs, so passing would report a property that "
        "was never measured\n");
    return 1;
  }

  for (const ConformanceCase& test_case : kCases) {
    // A design the standard requires to be rejected never reaches a compiled
    // form, so there is nothing of it to compare.
    if (test_case.required_error.has_value()) {
      continue;
    }
    const std::size_t split = test_case.id.rfind('/');
    const std::string group = test_case.id.substr(0, split);
    const std::string name = test_case.id.substr(split + 1);
    testing::RegisterTest(
        group.c_str(), name.c_str(), nullptr, nullptr, __FILE__, __LINE__,
        [&test_case, make]() -> testing::Test* { return make(test_case); });
  }

  const int failures = RUN_ALL_TESTS();
  if (failures != 0) {
    return failures;
  }

  const testing::UnitTest& unit_test = *testing::UnitTest::GetInstance();
  const int attempted = unit_test.test_to_run_count();
  const int compared = attempted - unit_test.skipped_test_count();
  if (compared * 2 < attempted) {
    fmt::print(
        stderr,
        "only {} of {} designs reached {}, which is too few for the comparison "
        "to have measured anything\n",
        compared, attempted, compared_form);
    return 1;
  }
  fmt::print("compared {} of {} designs\n", compared, attempted);
  return 0;
}

}  // namespace lyra::test
