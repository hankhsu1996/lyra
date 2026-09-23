#include <cstdint>
#include <filesystem>
#include <format>
#include <fstream>
#include <gtest/gtest.h>
#include <string>
#include <string_view>
#include <system_error>
#include <vector>

#include "lyra/driver/project_layout.hpp"
#include "lyra/driver/runtime_export.hpp"
#include "lyra/support/runtime_prelude.hpp"
#include "lyra/support/subprocess.hpp"
#include "tests/framework/cli_fixture.hpp"

namespace {

// The whole of what an emitted translation unit adds to the shipped surface:
// one class deriving from a scope, built and released the way a unit builds the
// scopes its design describes. It names none of the storage a scope holds, so
// whatever its object carries of that storage, it was made to carry.
//
// The delimiter is not one the formatter recognizes as C++, deliberately: this
// is data the test writes out, and a formatter reading it as code both reflows
// it and consumes the doubled braces the substitution below needs.
constexpr std::string_view kProbeSource = R"probe(
#include "{}"

namespace {{

class Probe final : public lyra::runtime::Scope {{
 public:
  Probe(
      lyra::runtime::Scope* parent, lyra::runtime::HierarchySegment segment,
      const lyra::runtime::ScopeDefinition* definition)
      : lyra::runtime::Scope(parent, std::move(segment), definition) {{
  }}
}};

}}  // namespace

auto MakeProbe(
    lyra::runtime::Scope* parent, lyra::runtime::HierarchySegment segment,
    const lyra::runtime::ScopeDefinition* definition)
    -> std::unique_ptr<lyra::runtime::Scope> {{
  return std::make_unique<Probe>(parent, std::move(segment), definition);
}}
)probe";

// What such a unit's object may weigh. A ceiling with an order of magnitude of
// room rather than a figure to defend: an object carrying the whole of what the
// library defines weighs 1.6 MB against 1.5 KB of code, so anything near this
// bound is that rather than drift in what a compiler chooses to emit.
constexpr std::uintmax_t kObjectCeiling = std::uintmax_t{256} * 1024;

}  // namespace

// A translation unit pays for what it contains. A unit of an emitted project
// contains one class per scope its design describes, and the operations a scope
// offers are the shipped library's to define -- so what this unit hands the
// linker is its own class and a call into that library, never a copy of what
// the library already holds.
//
// The way that stops being true is silent: a polymorphic class whose virtual
// functions are all written in the header has no translation unit of its own to
// be emitted in, so every unit that builds one instantiates its members and
// copies its dispatch table. Nothing about such a header looks wrong, no
// warning names it, and what it costs is only visible in the object.
TEST(RuntimeSurface, AUnitCarriesNoneOfWhatTheLibraryDefines) {
  const auto lyra_exe = lyra::test::ResolveLyra();
  ASSERT_FALSE(lyra_exe.empty());

  auto loc_or = lyra::driver::ResolveRuntimeLocation(lyra_exe.string());
  ASSERT_TRUE(loc_or) << loc_or.error().primary.message;

  auto cxx_or = lyra::test::FindDefaultCxx();
  if (!cxx_or) {
    GTEST_SKIP() << "measuring an object requires the compiler a project's "
                    "build recipe defaults to";
  }

  auto dir_or = lyra::test::MakeScratchDir();
  ASSERT_TRUE(dir_or) << dir_or.error();

  const auto source = *dir_or / "probe.cpp";
  const auto object = *dir_or / "probe.o";
  {
    std::ofstream out(source);
    out << std::format(
        kProbeSource,
        (loc_or->include_root / lyra::support::kRuntimePreludeHeader).string());
  }

  const std::vector<std::string> args = {
      std::string{lyra::driver::kCxxStandardFlag},
      std::string{
          lyra::driver::OptimizationFlag(lyra::driver::Optimization::kIterate)},
      "-I",
      loc_or->include_root.string(),
      "-c",
      source.string(),
      "-o",
      object.string()};

  auto result_or = lyra::support::RunProcessCaptured(*cxx_or, args);
  ASSERT_TRUE(result_or) << result_or.error();
  ASSERT_EQ(result_or->exit_code, 0)
      << "the probe did not compile against the shipped surface:\n"
      << result_or->stderr_text;

  std::error_code ec;
  const auto size = std::filesystem::file_size(object, ec);
  ASSERT_FALSE(ec) << ec.message();

  EXPECT_LE(size, kObjectCeiling) << std::format(
      "a unit adding one scope class to the shipped surface produced a {} "
      "byte object.\nEvery unit of every design pays this, and a design of a "
      "thousand units pays it a thousand times over on disk.\nThe cause to "
      "look for first is a class in the shipped surface whose virtual "
      "functions are all defined in its header: give it one that is not, and "
      "the library becomes the one place its dispatch table and its members "
      "are emitted.",
      size);
}
