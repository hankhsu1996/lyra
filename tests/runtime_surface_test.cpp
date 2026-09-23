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

// What an emitted translation unit adds to the shipped surface, in the four
// things every design's unit does: derive a class from a scope, give it a
// variable, write that variable, and wait for a change to one. Each is stated
// once, so the object measured below is the floor a unit pays rather than
// anything about how much a design says.
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

  lyra::runtime::Var<lyra::value::PackedArray> field{{}};

  auto Body(
      const lyra::value::PackedArray& edge,
      const lyra::value::PackedArray& mask) -> lyra::runtime::Coroutine<void> {{
    for (;;) {{
      field.Set(field.Get());
      lyra::runtime::Observation observation =
          lyra::runtime::Observation::OnReaching();
      co_await lyra::runtime::Suspension{{lyra::runtime::WaitAny(
          lyra::runtime::current_runtime(),
          std::array<lyra::runtime::Trigger, 1>{{
              lyra::runtime::Trigger(&field, observation, edge, mask)}})}};
    }}
    co_return;
  }}
}};

}}  // namespace

auto MakeProbe(
    lyra::runtime::Scope* parent, lyra::runtime::HierarchySegment segment,
    const lyra::runtime::ScopeDefinition* definition)
    -> std::unique_ptr<lyra::runtime::Scope> {{
  return std::make_unique<Probe>(parent, std::move(segment), definition);
}}

auto StartProbe(
    lyra::runtime::Scope* scope, const lyra::value::PackedArray& edge,
    const lyra::value::PackedArray& mask) -> void {{
  auto* probe = static_cast<Probe*>(scope);
  lyra::runtime::RegisterInitialProcess(probe, probe, probe->Body(edge, mask));
}}
)probe";

// What such a unit's object may weigh. A ceiling rather than a figure to
// defend: one library function defined in a header instead of compiled once
// costs a unit hundreds of kilobytes -- the wait this probe states is worth 400
// KB on its own -- so the distance between this bound and what the probe weighs
// is far wider than any drift in what a compiler emits.
constexpr std::uintmax_t kObjectCeiling = std::uintmax_t{256} * 1024;

}  // namespace

// A translation unit pays for what it contains. A unit of an emitted project
// contains one class per scope its design describes, and the operations a scope
// offers are the shipped library's to define -- so what this unit hands the
// linker is its own class and a call into that library, never a copy of what
// the library already holds.
//
// The ways that stops being true are silent, and there are two. A polymorphic
// class whose virtual functions are all written in the header has no
// translation unit of its own to be emitted in, so every unit that builds one
// instantiates its members and copies its dispatch table. And a function of the
// library's written in a header is compiled again by every unit that calls it,
// along with everything its body reaches -- which for anything touching a
// runtime value is the whole machinery of a variant over every value domain.
// Nothing about either header looks wrong, no warning names one, and what they
// cost is only visible in the object.
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
      "a unit stating one scope, one variable, one write and one wait produced "
      "a {} byte object.\nEvery unit of every design pays this, and a design "
      "of a thousand units pays it a thousand times over on disk.\nTwo causes "
      "to look for, both of them a definition the shipped headers state "
      "themselves:\na class whose virtual functions are all written in its "
      "header, which has no unit of its own to be emitted in;\nand a function "
      "of the library's written in a header, which every caller's unit "
      "compiles again along with everything its body reaches.\nEither is "
      "answered by defining it in the library's own source, or -- for a "
      "template over the value domains -- by stating it once there.",
      size);
}
