#include "tests/cli/cli_test_fixture.hpp"

#include <array>
#include <cstdio>
#include <cstdlib>
#include <format>
#include <fstream>
#include <random>
#include <sstream>

namespace lyra::test {
namespace {

auto GenerateRandomSuffix() -> std::string {
  static std::random_device rd;
  static std::mt19937 gen(rd());
  static std::uniform_int_distribution<> dis(0, 999999);
  return std::to_string(dis(gen));
}

// Execute command and capture output
// Uses popen to capture combined stdout/stderr
auto ExecuteCommand(const std::string& cmd) -> std::pair<int, std::string> {
  std::string output;
  std::array<char, 4096> buffer{};

  FILE* pipe = popen(cmd.c_str(), "r");
  if (pipe == nullptr) {
    return {-1, "Failed to execute command"};
  }

  while (fgets(buffer.data(), buffer.size(), pipe) != nullptr) {
    output += buffer.data();
  }

  int status = pclose(pipe);
  int exit_code = WIFEXITED(status) ? WEXITSTATUS(status) : -1;

  return {exit_code, output};
}

}  // namespace

void CliTestFixture::SetUp() {
  // Create unique test directory
  auto tmp = std::filesystem::temp_directory_path();
  test_dir_ = tmp / ("lyra_cli_test_" + GenerateRandomSuffix());
  std::filesystem::create_directories(test_dir_);

  // Get lyra binary path from environment or use default
  // Bazel sets TEST_SRCDIR for runfiles
  const char* test_srcdir = std::getenv("TEST_SRCDIR");
  const char* test_workspace = std::getenv("TEST_WORKSPACE");
  if (test_srcdir != nullptr && test_workspace != nullptr) {
    lyra_bin_ = std::filesystem::path(test_srcdir) / test_workspace / "lyra";
  } else {
    // Fallback for running outside Bazel
    lyra_bin_ = "lyra";
  }
}

void CliTestFixture::TearDown() {
  // Clean up test directory
  if (!test_dir_.empty() && std::filesystem::exists(test_dir_)) {
    std::filesystem::remove_all(test_dir_);
  }
}

auto CliTestFixture::Run(std::initializer_list<std::string> args) -> CliResult {
  return RunIn(test_dir_, std::vector<std::string>(args));
}

auto CliTestFixture::Run(const std::vector<std::string>& args) -> CliResult {
  return RunIn(test_dir_, args);
}

auto CliTestFixture::RunIn(
    const std::filesystem::path& dir, std::initializer_list<std::string> args)
    -> CliResult {
  return RunImpl(dir, std::vector<std::string>(args));
}

auto CliTestFixture::RunIn(
    const std::filesystem::path& dir, const std::vector<std::string>& args)
    -> CliResult {
  return RunImpl(dir, args);
}

auto CliTestFixture::RunImpl(
    const std::filesystem::path& working_dir,
    const std::vector<std::string>& args) -> CliResult {
  // Build command string
  std::ostringstream cmd;
  cmd << "cd " << working_dir.string() << " && " << lyra_bin_.string();
  for (const auto& arg : args) {
    // Simple escaping for shell
    cmd << " '" << arg << "'";
  }
  // Redirect stderr to stdout so we capture both
  cmd << " 2>&1";

  auto [exit_code, output] = ExecuteCommand(cmd.str());

  return CliResult{
      .exit_code = exit_code,
      .stdout_output = output,  // Combined since we redirected
      .stderr_output = "",
      .combined_output = output,
  };
}

void CliTestFixture::WriteFile(
    const std::filesystem::path& relative_path, const std::string& content) {
  auto full_path = test_dir_ / relative_path;
  std::filesystem::create_directories(full_path.parent_path());
  std::ofstream out(full_path);
  if (!out) {
    throw std::runtime_error("Failed to create file: " + full_path.string());
  }
  out << content;
}

void CliTestFixture::WriteLyraToml(
    const std::string& name, const std::string& top,
    const std::vector<std::string>& files) {
  std::ostringstream toml;
  toml << "[package]\n";
  toml << "name = \"" << name << "\"\n";
  toml << "top = \"" << top << "\"\n";
  toml << "\n[sources]\n";
  toml << "files = [";
  for (size_t i = 0; i < files.size(); ++i) {
    if (i > 0) {
      toml << ", ";
    }
    toml << "\"" << files[i] << "\"";
  }
  toml << "]\n";
  WriteFile("lyra.toml", toml.str());
}

void CliTestFixture::WriteSvModule(
    const std::string& filename, const std::string& name) {
  std::string sv = std::format(
      R"(module {};
  initial begin
    $display("Hello from {}!");
    $finish;
  end
endmodule
)",
      name, name);
  WriteFile(filename, sv);
}

auto CliTestFixture::FileExists(
    const std::filesystem::path& relative_path) const -> bool {
  return std::filesystem::exists(test_dir_ / relative_path);
}

auto CliTestFixture::ReadFile(const std::filesystem::path& relative_path) const
    -> std::string {
  auto full_path = test_dir_ / relative_path;
  std::ifstream in(full_path);
  if (!in) {
    throw std::runtime_error("Failed to read file: " + full_path.string());
  }
  std::ostringstream ss;
  ss << in.rdbuf();
  return ss.str();
}

}  // namespace lyra::test
