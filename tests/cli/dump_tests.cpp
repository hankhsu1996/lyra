#include <filesystem>
#include <gtest/gtest.h>

#include "tests/cli/cli_test_fixture.hpp"

namespace lyra::test {
namespace {

class DumpTest : public CliTestFixture {};

// Test: lyra dump cpp generates C++ code
TEST_F(DumpTest, DumpCppGeneratesCode) {
  WriteSvModule("test.sv", "Test");

  auto result = Run({"dump", "cpp", "test.sv"});

  EXPECT_TRUE(result.Success()) << result.combined_output;
  // Should contain C++ class definition
  EXPECT_NE(result.combined_output.find("class Test"), std::string::npos);
}

// Test: lyra dump mir generates MIR
TEST_F(DumpTest, DumpMirGeneratesMir) {
  WriteSvModule("test.sv", "Test");

  auto result = Run({"dump", "mir", "test.sv"});

  EXPECT_TRUE(result.Success()) << result.combined_output;
  // Should contain module definition in MIR format
  EXPECT_NE(result.combined_output.find("Test"), std::string::npos);
}

// Test: lyra dump lir generates LIR
TEST_F(DumpTest, DumpLirGeneratesLir) {
  WriteSvModule("test.sv", "Test");

  auto result = Run({"dump", "lir", "test.sv"});

  EXPECT_TRUE(result.Success()) << result.combined_output;
  // Should produce some LIR output
  EXPECT_FALSE(result.combined_output.empty());
}

// Test: lyra dump with invalid format shows error
TEST_F(DumpTest, InvalidFormatShowsError) {
  WriteSvModule("test.sv", "Test");

  auto result = Run({"dump", "invalid", "test.sv"});

  EXPECT_FALSE(result.Success());
  EXPECT_NE(result.combined_output.find("unknown format"), std::string::npos);
}

// Test: lyra dump without files shows error
TEST_F(DumpTest, NoFilesShowsError) {
  auto result = Run({"dump", "cpp"});

  EXPECT_FALSE(result.Success());
  EXPECT_NE(result.combined_output.find("no input files"), std::string::npos);
}

// Test: lyra dump with nonexistent file shows error
TEST_F(DumpTest, NonexistentFileShowsError) {
  auto result = Run({"dump", "cpp", "nonexistent.sv"});

  EXPECT_FALSE(result.Success());
  // Should mention the missing file somehow
  EXPECT_NE(result.combined_output.find("nonexistent"), std::string::npos);
}

// Test: lyra dump accepts multiple files
TEST_F(DumpTest, AcceptsMultipleFiles) {
  WriteSvModule("a.sv", "ModuleA");
  WriteSvModule("b.sv", "ModuleB");

  auto result = Run({"dump", "cpp", "a.sv", "b.sv"});

  EXPECT_TRUE(result.Success()) << result.combined_output;
  // Should contain at least the first module (dump outputs primary module)
  EXPECT_NE(result.combined_output.find("ModuleA"), std::string::npos);
}

// Test: lyra dump with absolute path works
TEST_F(DumpTest, AbsolutePathWorks) {
  WriteSvModule("test.sv", "Test");
  auto abs_path = std::filesystem::absolute(TestDir() / "test.sv");

  auto result = Run({"dump", "cpp", abs_path.string()});

  EXPECT_TRUE(result.Success()) << result.combined_output;
}

}  // namespace
}  // namespace lyra::test
