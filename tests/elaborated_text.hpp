#pragma once

#include <filesystem>
#include <fstream>
#include <gtest/gtest.h>
#include <memory>
#include <string>
#include <string_view>
#include <vector>

#include <slang/driver/Driver.h>

#include "lyra/compiler/compile.hpp"

namespace lyra::test {

// A design a test writes as text, elaborated by the front end the command line
// runs. The driver owns the text every span of the elaboration points into, so
// the two are held together.
struct ElaboratedText {
  std::unique_ptr<slang::driver::Driver> driver;
  compiler::FrontEndResult front;
};

// Elaborates `source`, written to a file named after the running test so that
// what the front end reports of it names the test, and removed once the front
// end has read it. A design the front end rejects fails the test.
inline auto ElaborateText(std::string_view source) -> ElaboratedText {
  const std::filesystem::path path =
      std::filesystem::temp_directory_path() /
      std::filesystem::path{
          ::testing::UnitTest::GetInstance()->current_test_info()->name()}
          .concat(".sv");
  {
    std::ofstream out(path);
    out << source;
  }
  ElaboratedText out;
  out.driver = std::make_unique<slang::driver::Driver>();
  out.driver->addStandardArgs();
  const std::string path_text = path.string();
  const std::vector<const char*> args{"lyra", path_text.c_str()};
  EXPECT_TRUE(
      out.driver->parseCommandLine(static_cast<int>(args.size()), args.data()));
  out.front = compiler::RunFrontEnd(*out.driver);
  std::filesystem::remove(path);
  EXPECT_TRUE(out.front.elaborated.has_value())
      << "the front end rejected the design: " << out.front.diagnostics;
  return out;
}

}  // namespace lyra::test
