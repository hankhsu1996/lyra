#include <filesystem>
#include <gtest/gtest.h>

#include "tests/cli/cli_test_fixture.hpp"

namespace lyra::test {
namespace {

class InitTest : public CliTestFixture {};

// Test: lyra init <name> creates a new project directory
TEST_F(InitTest, CreatesProjectDirectory) {
  auto result = Run({"init", "myproj"});

  EXPECT_TRUE(result.Success()) << result.combined_output;
  EXPECT_TRUE(FileExists("myproj/lyra.toml"));
  EXPECT_TRUE(FileExists("myproj/myproj.sv"));
}

// Test: lyra init <name> creates correct lyra.toml content
TEST_F(InitTest, CreatesCorrectTomlContent) {
  auto result = Run({"init", "hello"});

  EXPECT_TRUE(result.Success()) << result.combined_output;

  auto toml = ReadFile("hello/lyra.toml");
  EXPECT_NE(toml.find("name = \"hello\""), std::string::npos);
  EXPECT_NE(toml.find("top = \"hello\""), std::string::npos);
  EXPECT_NE(toml.find("hello.sv"), std::string::npos);
}

// Test: lyra init <name> creates correct .sv file
TEST_F(InitTest, CreatesCorrectSvFile) {
  auto result = Run({"init", "test_module"});

  EXPECT_TRUE(result.Success()) << result.combined_output;

  auto sv = ReadFile("test_module/test_module.sv");
  EXPECT_NE(sv.find("module test_module"), std::string::npos);
}

// Test: lyra init fails if directory already exists
TEST_F(InitTest, FailsIfDirectoryExists) {
  std::filesystem::create_directories(TestDir() / "existing");

  auto result = Run({"init", "existing"});

  EXPECT_FALSE(result.Success());
  EXPECT_NE(result.combined_output.find("already exists"), std::string::npos);
}

// Test: lyra init (no args) initializes in current directory
TEST_F(InitTest, InitializesCurrentDirectory) {
  auto result = Run({"init"});

  EXPECT_TRUE(result.Success()) << result.combined_output;
  EXPECT_TRUE(FileExists("lyra.toml"));
  // Should NOT create .sv file when initializing in existing directory
  EXPECT_NE(result.combined_output.find("Initialized"), std::string::npos);
}

// Test: lyra init (no args) fails if lyra.toml already exists
TEST_F(InitTest, FailsIfTomlExistsWithoutForce) {
  WriteFile("lyra.toml", "[package]\nname = \"old\"\n");

  auto result = Run({"init"});

  EXPECT_FALSE(result.Success());
  EXPECT_NE(result.combined_output.find("already exists"), std::string::npos);
}

// Test: lyra init --force overwrites existing lyra.toml
TEST_F(InitTest, ForceOverwritesToml) {
  WriteFile("lyra.toml", "[package]\nname = \"old\"\n");

  auto result = Run({"init", "--force"});

  EXPECT_TRUE(result.Success()) << result.combined_output;
  // Verify the toml was overwritten (won't have "old" name anymore)
  auto toml = ReadFile("lyra.toml");
  EXPECT_EQ(toml.find("name = \"old\""), std::string::npos);
}

// Test: lyra init -f (short flag) works
TEST_F(InitTest, ShortForceFlagWorks) {
  WriteFile("lyra.toml", "[package]\nname = \"old\"\n");

  auto result = Run({"init", "-f"});

  EXPECT_TRUE(result.Success()) << result.combined_output;
}

// Test: Output message for new project
TEST_F(InitTest, OutputMessageForNewProject) {
  auto result = Run({"init", "newproj"});

  EXPECT_TRUE(result.Success()) << result.combined_output;
  EXPECT_NE(result.combined_output.find("Created project"), std::string::npos);
  EXPECT_NE(result.combined_output.find("newproj"), std::string::npos);
}

// Test: Output message for current directory init
TEST_F(InitTest, OutputMessageForCurrentDirInit) {
  auto result = Run({"init"});

  EXPECT_TRUE(result.Success()) << result.combined_output;
  EXPECT_NE(
      result.combined_output.find("Initialized project"), std::string::npos);
}

}  // namespace
}  // namespace lyra::test
