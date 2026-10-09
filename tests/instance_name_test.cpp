// What an instance's specialization is called is worked out by the unit naming
// itself and by every unit naming it, each from what it has in hand, so the
// answer cannot depend on which of them asked first. No program a design
// simulates can observe a name, so this is stated against the naming itself.

#include <algorithm>
#include <cstddef>
#include <filesystem>
#include <fstream>
#include <gtest/gtest.h>
#include <optional>
#include <string>
#include <string_view>
#include <utility>
#include <vector>

#include <slang/ast/Compilation.h>
#include <slang/ast/symbols/CompilationUnitSymbols.h>
#include <slang/ast/symbols/InstanceSymbols.h>
#include <slang/driver/Driver.h>

#include "lyra/compiler/compile.hpp"
#include "lyra/frontend/load.hpp"
#include "lyra/lowering/ast_to_hir/unit_identity.hpp"

namespace {

using lyra::lowering::ast_to_hir::SpecializationPolicy;

// A design elaborated from source text. The front end's driver owns what the
// elaboration points into, so the two are held together.
class Elaborated {
 public:
  explicit Elaborated(std::string_view source) {
    const std::filesystem::path path =
        std::filesystem::temp_directory_path() /
        std::filesystem::path{
            ::testing::UnitTest::GetInstance()->current_test_info()->name()}
            .concat(".sv");
    {
      std::ofstream out(path);
      out << source;
    }
    driver_.addStandardArgs();
    const std::string path_text = path.string();
    const std::vector<const char*> args{"lyra", path_text.c_str()};
    EXPECT_TRUE(
        driver_.parseCommandLine(static_cast<int>(args.size()), args.data()));
    lyra::compiler::FrontEndResult front = lyra::compiler::RunFrontEnd(driver_);
    EXPECT_TRUE(front.elaborated.has_value())
        << "the front end rejected the probe design: " << front.diagnostics;
    design_ = std::move(front.elaborated);
  }

  [[nodiscard]] auto Succeeded() const -> bool {
    return design_.has_value();
  }

  [[nodiscard]] auto TopNamed(std::string_view name) const
      -> const slang::ast::InstanceSymbol& {
    const auto tops = design_->compilation->getRoot().topInstances;
    return **std::ranges::find(tops, name, &slang::ast::InstanceSymbol::name);
  }

 private:
  slang::driver::Driver driver_;
  std::optional<lyra::frontend::ParseResult> design_;
};

// Two top-level instances each writing a path from the top that starts at the
// other (LRM 23.6): naming either asks for the other's name while its own is
// still being worked out.
constexpr std::string_view kEachReadsTheOther = R"(
module A;
  int w;
  int got;
  initial got = B.v;
endmodule

module B;
  int v;
  int got;
  initial got = A.w;
endmodule
)";

TEST(InstanceName, IsTheSameWhicheverInstanceIsAskedFirst) {
  const Elaborated design(kEachReadsTheOther);
  ASSERT_TRUE(design.Succeeded());
  const slang::ast::InstanceSymbol& a = design.TopNamed("A");
  const slang::ast::InstanceSymbol& b = design.TopNamed("B");

  const SpecializationPolicy a_first;
  const std::string a_asked_first = a_first.NameOf(a);
  const std::string b_asked_second = a_first.NameOf(b);

  const SpecializationPolicy b_first;
  const std::string b_asked_first = b_first.NameOf(b);
  const std::string a_asked_second = b_first.NameOf(a);

  EXPECT_FALSE(a_asked_first.empty());
  EXPECT_EQ(a_asked_first, a_asked_second);
  EXPECT_EQ(b_asked_first, b_asked_second);
}

// One module twice, its logger naming `sensor` (LRM 23.8): from the first the
// name lands in the sensor holding the logger, and from the spare it lands in
// the board, beside the spare.
constexpr std::string_view kASpareReadsTheMain = R"(
module Logger;
  int seen;
  initial seen = sensor.temperature;
endmodule

module Sensor;
  int temperature;
  Logger logger ();
endmodule

module Backup;
  Sensor spare ();
endmodule

module Board;
  Sensor sensor ();
  Backup backup ();
endmodule
)";

TEST(InstanceName, FollowsWhereANameWrittenBelowLandsAndNotWhoWasAskedFirst) {
  const Elaborated design(kASpareReadsTheMain);
  ASSERT_TRUE(design.Succeeded());
  using slang::ast::InstanceSymbol;
  const InstanceSymbol& board = design.TopNamed("Board");
  const auto& sensor = board.body.find<InstanceSymbol>("sensor");
  const auto& backup = board.body.find<InstanceSymbol>("backup");
  const auto& spare = backup.body.find<InstanceSymbol>("spare");
  const auto& main_logger = sensor.body.find<InstanceSymbol>("logger");
  const auto& spare_logger = spare.body.find<InstanceSymbol>("logger");
  const std::vector<const InstanceSymbol*> outermost_first{
      &board, &sensor, &main_logger, &backup, &spare, &spare_logger};

  const SpecializationPolicy from_the_top;
  std::vector<std::string> asked_from_the_top;
  for (const InstanceSymbol* inst : outermost_first) {
    asked_from_the_top.push_back(from_the_top.NameOf(*inst));
  }

  const SpecializationPolicy from_the_leaves;
  std::vector<std::string> asked_from_the_leaves(outermost_first.size());
  for (std::size_t at = outermost_first.size(); at-- > 0;) {
    asked_from_the_leaves[at] = from_the_leaves.NameOf(*outermost_first[at]);
  }

  EXPECT_EQ(asked_from_the_top, asked_from_the_leaves);
  // The loggers compile apart, so what builds each does too.
  EXPECT_NE(
      from_the_top.NameOf(main_logger), from_the_top.NameOf(spare_logger));
  EXPECT_NE(from_the_top.NameOf(sensor), from_the_top.NameOf(spare));
}

}  // namespace
