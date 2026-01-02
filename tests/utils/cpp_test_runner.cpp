#include "tests/utils/cpp_test_runner.hpp"

#include <array>
#include <cstdio>
#include <cstdlib>
#include <filesystem>
#include <fstream>
#include <memory>
#include <sstream>

#include <slang/ast/Compilation.h>

#include "lyra/codegen/cpp_codegen.hpp"
#include "lyra/frontend/slang_frontend.hpp"
#include "lyra/lowering/ast_to_mir/ast_to_mir.hpp"

namespace lyra::test {

namespace {

// Inline SDK for generated code (avoids header dependency issues in test)
constexpr const char* kSdkCode = R"(
#include <coroutine>
#include <exception>
#include <functional>
#include <iostream>
#include <string>
#include <vector>

namespace lyra::sdk {

class Task {
 public:
  struct promise_type {
    auto get_return_object() -> Task {
      return Task{std::coroutine_handle<promise_type>::from_promise(*this)};
    }
    static auto initial_suspend() -> std::suspend_never { return {}; }
    static auto final_suspend() noexcept -> std::suspend_always { return {}; }
    static void return_void() {}
    static void unhandled_exception() { std::terminate(); }
  };
  using Handle = std::coroutine_handle<promise_type>;
  explicit Task(Handle handle) : handle_(handle) {}
  ~Task() { if (handle_) handle_.destroy(); }
  Task(const Task&) = delete;
  Task& operator=(const Task&) = delete;
  Task(Task&& other) noexcept : handle_(other.handle_) { other.handle_ = nullptr; }
 private:
  Handle handle_;
};

class Module {
 public:
  explicit Module(std::string name) : name_(std::move(name)) {}
  virtual ~Module() = default;
 protected:
  template <typename T>
  void RegisterInitial(Task (T::*method)()) {
    initial_methods_.push_back([this, method]() {
      return (static_cast<T*>(this)->*method)();
    });
  }
 public:
  void RunInitials() {
    for (auto& method : initial_methods_) { method(); }
  }
 private:
  std::string name_;
  std::vector<std::function<Task()>> initial_methods_;
};

}  // namespace lyra::sdk

using namespace lyra::sdk;
)";

auto ExecuteCommand(const std::string& cmd) -> std::pair<int, std::string> {
  std::array<char, 128> buffer{};
  std::string result;
  std::unique_ptr<FILE, decltype(&pclose)> pipe(
      popen(cmd.c_str(), "r"), pclose);
  if (!pipe) {
    return {-1, "popen failed"};
  }
  while (fgets(buffer.data(), buffer.size(), pipe.get()) != nullptr) {
    result += buffer.data();
  }
  int status = pclose(pipe.release());
  return {WEXITSTATUS(status), result};
}

}  // namespace

auto CppTestResult::ReadVariable(const std::string& name) const -> int64_t {
  auto it = variables_.find(name);
  if (it == variables_.end()) {
    throw std::runtime_error("Variable not found: " + name);
  }
  return it->second;
}

auto CppTestRunner::RunFromSources(
    const std::vector<SourceFile>& files,
    const std::vector<std::string>& variables_to_read) -> CppTestResult {
  CppTestResult result;

  // Create temp directory for SV files
  auto tmp_dir = std::filesystem::temp_directory_path() / "lyra_test";
  std::filesystem::create_directories(tmp_dir);

  // Write SV files to temp directory
  std::vector<std::string> file_paths;
  for (const auto& file : files) {
    auto file_path = tmp_dir / file.name;
    std::ofstream out(file_path);
    out << file.content;
    file_paths.push_back(file_path.string());
  }

  // Parse SV files
  frontend::SlangFrontend frontend;
  auto compilation = frontend.LoadFromFiles(file_paths);
  if (!compilation) {
    result.error_message_ = "Failed to parse SV files";
    return result;
  }

  // Lower to MIR
  const auto& root = compilation->getRoot();
  auto mir = lowering::ast_to_mir::AstToMir(root);
  if (!mir) {
    result.error_message_ = "Failed to lower to MIR";
    return result;
  }

  // Generate C++
  codegen::CppCodegen codegen;
  std::string generated = codegen.Generate(*mir);

  // Build main() that prints variables
  std::ostringstream main_code;
  main_code << "\nint main() {\n";
  main_code << "  " << mir->name << " dut;\n";
  main_code << "  dut.RunInitials();\n";
  for (const auto& var : variables_to_read) {
    main_code << "  std::cout << \"" << var << "=\" << dut." << var
              << " << std::endl;\n";
  }
  main_code << "  return 0;\n";
  main_code << "}\n";

  auto cpp_path = tmp_dir / "test.cpp";
  auto bin_path = tmp_dir / "test";

  // Write complete C++ file
  {
    std::ofstream out(cpp_path);
    out << kSdkCode << "\n";
    auto pos = generated.find("class ");
    if (pos != std::string::npos) {
      out << generated.substr(pos);
    } else {
      out << generated;
    }
    out << main_code.str();
  }

  // Compile
  std::string compile_cmd = "clang++ -std=c++23 -o " + bin_path.string() + " " +
                            cpp_path.string() + " 2>&1";
  auto [compile_status, compile_output] = ExecuteCommand(compile_cmd);
  if (compile_status != 0) {
    result.error_message_ = "Compilation failed: " + compile_output;
    return result;
  }

  // Run
  auto [run_status, run_output] = ExecuteCommand(bin_path.string());
  if (run_status != 0) {
    result.error_message_ =
        "Execution failed with status " + std::to_string(run_status);
    return result;
  }

  // Parse output (format: "var=value\n")
  std::istringstream iss(run_output);
  std::string line;
  while (std::getline(iss, line)) {
    auto eq_pos = line.find('=');
    if (eq_pos != std::string::npos) {
      std::string name = line.substr(0, eq_pos);
      int64_t value = std::stoll(line.substr(eq_pos + 1));
      result.variables_[name] = value;
    }
  }

  result.success_ = true;
  return result;
}

auto CppTestRunner::RunFromSource(
    const std::string& sv_code,
    const std::vector<std::string>& variables_to_read) -> CppTestResult {
  CppTestResult result;

  // Parse SV
  frontend::SlangFrontend frontend;
  auto compilation = frontend.LoadFromString(sv_code);
  if (!compilation) {
    result.error_message_ = "Failed to parse SV";
    return result;
  }

  // Lower to MIR
  const auto& root = compilation->getRoot();
  auto mir = lowering::ast_to_mir::AstToMir(root);
  if (!mir) {
    result.error_message_ = "Failed to lower to MIR";
    return result;
  }

  // Generate C++
  codegen::CppCodegen codegen;
  std::string generated = codegen.Generate(*mir);

  // Build main() that prints variables
  std::ostringstream main_code;
  main_code << "\nint main() {\n";
  main_code << "  " << mir->name << " dut;\n";
  main_code << "  dut.RunInitials();\n";
  for (const auto& var : variables_to_read) {
    main_code << "  std::cout << \"" << var << "=\" << dut." << var
              << " << std::endl;\n";
  }
  main_code << "  return 0;\n";
  main_code << "}\n";

  // Create temp directory and files
  auto tmp_dir = std::filesystem::temp_directory_path() / "lyra_test";
  std::filesystem::create_directories(tmp_dir);

  auto cpp_path = tmp_dir / "test.cpp";
  auto bin_path = tmp_dir / "test";

  // Write complete C++ file
  {
    std::ofstream out(cpp_path);
    out << kSdkCode << "\n";
    // Skip the #include and using lines from generated code
    auto pos = generated.find("class ");
    if (pos != std::string::npos) {
      out << generated.substr(pos);
    } else {
      out << generated;
    }
    out << main_code.str();
  }

  // Compile
  std::string compile_cmd = "clang++ -std=c++23 -o " + bin_path.string() + " " +
                            cpp_path.string() + " 2>&1";
  auto [compile_status, compile_output] = ExecuteCommand(compile_cmd);
  if (compile_status != 0) {
    result.error_message_ = "Compilation failed: " + compile_output;
    return result;
  }

  // Run
  auto [run_status, run_output] = ExecuteCommand(bin_path.string());
  if (run_status != 0) {
    result.error_message_ =
        "Execution failed with status " + std::to_string(run_status);
    return result;
  }

  // Parse output (format: "var=value\n")
  std::istringstream iss(run_output);
  std::string line;
  while (std::getline(iss, line)) {
    auto eq_pos = line.find('=');
    if (eq_pos != std::string::npos) {
      std::string name = line.substr(0, eq_pos);
      int64_t value = std::stoll(line.substr(eq_pos + 1));
      result.variables_[name] = value;
    }
  }

  result.success_ = true;
  return result;
}

}  // namespace lyra::test
