#include <gtest/gtest.h>

#include "tests/cli/cli_test_fixture.hpp"

namespace lyra::test {
namespace {

class ConfigTest : public CliTestFixture {};

// Test: lyra run fails without config or files
TEST_F(ConfigTest, RunFailsWithoutConfigOrFiles) {
  auto result = Run({"run"});

  EXPECT_FALSE(result.Success());
  EXPECT_NE(result.combined_output.find("no source files"), std::string::npos);
}

// Test: lyra run --interpret fails without config or files
TEST_F(ConfigTest, RunInterpretFailsWithoutConfigOrFiles) {
  auto result = Run({"run", "--interpret"});

  EXPECT_FALSE(result.Success());
  EXPECT_NE(result.combined_output.find("no source files"), std::string::npos);
}

// Test: lyra build fails without config or files
TEST_F(ConfigTest, BuildFailsWithoutConfigOrFiles) {
  auto result = Run({"build"});

  EXPECT_FALSE(result.Success());
  EXPECT_NE(result.combined_output.find("no source files"), std::string::npos);
}

// Test: lyra emit fails without config or files
TEST_F(ConfigTest, EmitFailsWithoutConfigOrFiles) {
  auto result = Run({"emit"});

  EXPECT_FALSE(result.Success());
  EXPECT_NE(result.combined_output.find("no source files"), std::string::npos);
}

// Test: lyra check fails without config or files
TEST_F(ConfigTest, CheckFailsWithoutConfigOrFiles) {
  auto result = Run({"check"});

  EXPECT_FALSE(result.Success());
  EXPECT_NE(result.combined_output.find("no source files"), std::string::npos);
}

// Test: CLI mode requires --top when no lyra.toml
TEST_F(ConfigTest, CliModeRequiresTop) {
  WriteSvModule("test.sv", "Test");

  auto result = Run({"check", "test.sv"});

  EXPECT_FALSE(result.Success());
  EXPECT_NE(
      result.combined_output.find("--top is required"), std::string::npos);
}

// Test: CLI mode works with --top and files
TEST_F(ConfigTest, CliModeWorksWithTopAndFiles) {
  WriteSvModule("test.sv", "Test");

  auto result = Run({"check", "--top", "Test", "test.sv"});

  EXPECT_TRUE(result.Success()) << result.combined_output;
}

// Test: lyra run -i works with valid project
TEST_F(ConfigTest, RunInterpretWorksWithValidProject) {
  WriteSvModule("test.sv", "Test");
  WriteLyraToml("test", "Test", {"test.sv"});

  auto result = Run({"run", "-i"});

  EXPECT_TRUE(result.Success()) << result.combined_output;
  EXPECT_NE(result.combined_output.find("Hello from Test!"), std::string::npos);
}

// Test: lyra check works with valid project
TEST_F(ConfigTest, CheckWorksWithValidProject) {
  WriteSvModule("test.sv", "Test");
  WriteLyraToml("test", "Test", {"test.sv"});

  auto result = Run({"check"});

  EXPECT_TRUE(result.Success()) << result.combined_output;
}

// Test: lyra emit generates output directory
TEST_F(ConfigTest, EmitGeneratesOutput) {
  WriteSvModule("test.sv", "Test");
  WriteLyraToml("test", "Test", {"test.sv"});

  auto result = Run({"emit"});

  EXPECT_TRUE(result.Success()) << result.combined_output;
  EXPECT_TRUE(FileExists("out/main.cpp"));
  EXPECT_TRUE(FileExists("out/CMakeLists.txt"));
}

// Test: Error message for missing source file
TEST_F(ConfigTest, MissingSourceFileError) {
  WriteLyraToml("test", "Test", {"nonexistent.sv"});

  auto result = Run({"check"});

  EXPECT_FALSE(result.Success());
  // Should mention the missing file
  EXPECT_NE(result.combined_output.find("nonexistent.sv"), std::string::npos);
}

// Test: Error message for invalid SystemVerilog
TEST_F(ConfigTest, InvalidSvError) {
  WriteFile("bad.sv", "module Test\n  this is not valid\nendmodule\n");
  WriteLyraToml("test", "Test", {"bad.sv"});

  auto result = Run({"check"});

  EXPECT_FALSE(result.Success());
  // Should show some error diagnostic
  EXPECT_NE(result.combined_output.find("error"), std::string::npos);
}

}  // namespace
}  // namespace lyra::test
