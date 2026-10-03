// Compiling one unchanged design twice produces one program. Nothing the
// design simulates can observe whether it does -- two orderings of a set wake a
// process on exactly the same events -- so no conformance case can state it,
// and it stays invisible until something downstream has to decide from the
// output whether the output must be produced again. A compile cache and a
// record of what a change invalidated both want exactly that, and neither can
// be built on a compiler whose answer moves on its own.
//
// What is compared is the written text, because that is what the claim is
// about: an intermediate form repeating is neither necessary nor sufficient for
// the program repeating. The design is written through the same sink the
// command line writes through, so what counts as the program is decided once,
// where it is already decided, and a file kind added later is covered without
// anything here being told about it. What the sink does not write -- the build
// recipe, the bundled runtime -- carries the output directory's own path and is
// assembled around the text rather than lowered from the design.

#include <cstddef>
#include <filesystem>
#include <fstream>
#include <gtest/gtest.h>
#include <iterator>
#include <map>
#include <string>
#include <utility>

#include <fmt/format.h>

#include "lyra/compiler/compile.hpp"
#include "lyra/compiler/lower_design.hpp"
#include "lyra/diag/sink.hpp"
#include "lyra/driver/cpp_build.hpp"
#include "tests/compiled_twice.hpp"
#include "tests/framework/conformance_case.hpp"

namespace {

using lyra::test::Compilation;
using lyra::test::ConformanceCase;
using lyra::test::Elaborate;
using lyra::test::FirstDifference;
using lyra::test::kWide;
using lyra::test::ScratchDirectory;

auto ReadEveryFileUnder(const std::filesystem::path& dir)
    -> std::map<std::string, std::string> {
  std::map<std::string, std::string> files;
  for (const auto& entry : std::filesystem::recursive_directory_iterator(dir)) {
    if (!entry.is_regular_file()) {
      continue;
    }
    std::ifstream in(entry.path(), std::ios::binary);
    files.emplace(
        std::filesystem::relative(entry.path(), dir).string(),
        std::string(
            std::istreambuf_iterator<char>(in),
            std::istreambuf_iterator<char>()));
  }
  return files;
}

// Every file this compilation's design is written as, lowering `width` units at
// once. Empty where the design did not reach a written form, which is what a
// design this path refuses comes to and is not this test's subject.
auto Emit(
    Compilation& compilation, const std::filesystem::path& dir,
    std::size_t width) -> std::map<std::string, std::string> {
  if (!compilation.front.elaborated.has_value()) {
    return {};
  }

  lyra::diag::DiagnosticSink sink;
  auto design = lyra::compiler::DeclareUnits(
      std::move(compilation.front.elaborated->compilation),
      compilation.front.elaborated->source_mapper,
      lyra::compiler::LoweringPolicy{}, sink);
  if (!design.has_value()) {
    return {};
  }

  std::filesystem::create_directories(dir);
  lyra::driver::CppProjectSink project(
      dir, lyra::driver::SourceFormatting::kOff, sink);
  auto semantic = lyra::compiler::LowerToSemantic(
      *design, compilation.front.elaborated->diag_sources, sink, width,
      [&project](lyra::compiler::SemanticUnit unit) {
        return project.Write(unit.mir);
      },
      [&project](lyra::driver::EmittedUnit unit) {
        project.Collect(std::move(unit));
      });
  if (!semantic.has_value()) {
    return {};
  }
  if (auto finished = project.Finish(semantic->root); !finished) {
    return {};
  }
  return ReadEveryFileUnder(dir);
}

auto Describe(
    const std::map<std::string, std::string>& first,
    const std::map<std::string, std::string>& second) -> std::string {
  for (const auto& [relpath, text] : first) {
    const auto found = second.find(relpath);
    if (found == second.end()) {
      return fmt::format("the second compilation did not write '{}'", relpath);
    }
    if (found->second != text) {
      return fmt::format(
          "'{}' differs at {}", relpath, FirstDifference(text, found->second));
    }
  }
  for (const auto& entry : second) {
    if (!first.contains(entry.first)) {
      return fmt::format(
          "the first compilation did not write '{}'", entry.first);
    }
  }
  return {};
}

class RepeatableEmissionTest : public testing::Test {
 public:
  explicit RepeatableEmissionTest(const ConformanceCase& test_case)
      : case_(&test_case) {
  }

  void TestBody() override {
    const ScratchDirectory scratch(case_->id);
    Compilation first = Elaborate(*case_);
    Compilation second = Elaborate(*case_);

    const std::map<std::string, std::string> first_files =
        Emit(first, scratch.Under("first"), 1);
    const std::map<std::string, std::string> second_files =
        Emit(second, scratch.Under("second"), kWide);
    if (first_files.empty() && second_files.empty()) {
      GTEST_SKIP() << "this path writes no program for '" << case_->id << "'";
    }

    const std::string difference = Describe(first_files, second_files);
    EXPECT_TRUE(difference.empty())
        << "compiling '" << case_->id
        << "' twice wrote two programs: " << difference;
  }

 private:
  const ConformanceCase* case_;
};

// The design written to show an ordering, held to the same claim and to one
// more: it has to reach a written program. A corpus design that does not is a
// path's stated refusal and no concern of this check, but this one exists so
// that something here is known to be sensitive to an ordering, and a skip would
// leave that silently untrue.
TEST(RepeatableEmission, ADesignWrittenToShowAnOrderingWritesOneProgram) {
  const ScratchDirectory scratch("wide-sensitivity");
  const ConformanceCase design =
      lyra::test::WriteWideSensitivityDesign(scratch.Under("source"));

  Compilation first = Elaborate(design);
  Compilation second = Elaborate(design);

  const std::map<std::string, std::string> first_files =
      Emit(first, scratch.Under("first"), 1);
  const std::map<std::string, std::string> second_files =
      Emit(second, scratch.Under("second"), kWide);
  ASSERT_FALSE(first_files.empty())
      << "the design written to exercise this check no longer compiles";

  const std::string difference = Describe(first_files, second_files);
  EXPECT_TRUE(difference.empty())
      << "compiling it twice wrote two programs: " << difference;
}

}  // namespace

auto main(int argc, char** argv) -> int {
  return lyra::test::RunOverCorpus(
      argc, argv, "a written program",
      [](const ConformanceCase& test_case) -> testing::Test* {
        // NOLINTNEXTLINE(cppcoreguidelines-owning-memory)
        return new RepeatableEmissionTest(test_case);
      });
}
