#include "run_llvm.hpp"

#include <cerrno>
#include <cstdlib>
#include <cstring>
#include <filesystem>
#include <fstream>
#include <spawn.h>
#include <sys/wait.h>
#include <unistd.h>
#include <vector>

#include <fmt/color.h>
#include <fmt/core.h>

#include "lyra/common/source_span.hpp"
#include "lyra/common/type.hpp"
#include "lyra/common/unsupported_error.hpp"
#include "lyra/hir/module.hpp"
#include "lyra/hir/statement.hpp"
#include "lyra/lowering/mir_to_llvm/lower.hpp"
#include "pipeline.hpp"
#include "print.hpp"

namespace lyra::driver {

namespace {

class TempFileGuard {
 public:
  explicit TempFileGuard(std::string path) : path_(std::move(path)) {
  }
  ~TempFileGuard() {
    if (!path_.empty()) {
      std::filesystem::remove(path_);
    }
  }

  TempFileGuard(const TempFileGuard&) = delete;
  auto operator=(const TempFileGuard&) -> TempFileGuard& = delete;
  TempFileGuard(TempFileGuard&&) = delete;
  auto operator=(TempFileGuard&&) -> TempFileGuard& = delete;

  void Release() {
    path_.clear();
  }

 private:
  std::string path_;
};

auto CreateTempFile(const std::string& suffix) -> std::string {
  std::string tmpl = std::filesystem::temp_directory_path() / "lyra_XXXXXX";
  tmpl += suffix;

  std::vector<char> buf(tmpl.begin(), tmpl.end());
  buf.push_back('\0');

  int fd = mkstemps(buf.data(), static_cast<int>(suffix.size()));
  if (fd == -1) {
    return "";
  }
  close(fd);
  return {buf.data()};
}

auto FindRuntimeLibrary(std::vector<std::string>& tried_paths) -> std::string {
  constexpr auto kLibName = "liblyra_runtime.so";

  if (const char* env_path = std::getenv("LYRA_RUNTIME_PATH")) {
    if (std::filesystem::exists(env_path)) {
      return env_path;
    }
    tried_paths.emplace_back(fmt::format("LYRA_RUNTIME_PATH={}", env_path));
  }

  std::filesystem::path exe_path;
  try {
    exe_path = std::filesystem::read_symlink("/proc/self/exe");
  } catch (const std::filesystem::filesystem_error& e) {
    tried_paths.emplace_back(
        fmt::format(
            "/proc/self/exe ({}; non-Linux or sandboxed?)",
            e.code().message()));
  }

  if (!exe_path.empty()) {
    auto runfiles_path =
        std::filesystem::path(exe_path.string() + ".runfiles") / "_main" /
        kLibName;
    tried_paths.push_back(runfiles_path.string());
    if (std::filesystem::exists(runfiles_path)) {
      return runfiles_path.string();
    }

    auto sibling_path = exe_path.parent_path() / kLibName;
    tried_paths.push_back(sibling_path.string());
    if (std::filesystem::exists(sibling_path)) {
      return sibling_path.string();
    }
  }

  auto cwd_path = std::filesystem::current_path() / kLibName;
  tried_paths.push_back(cwd_path.string());
  if (std::filesystem::exists(cwd_path)) {
    return cwd_path.string();
  }

  return "";
}

auto RunLli(const std::string& runtime_path, const std::string& ir_path)
    -> int {
  std::string dlopen_arg = fmt::format("--dlopen={}", runtime_path);

  std::vector<char*> argv;
  std::string lli_cmd = "lli";
  argv.push_back(lli_cmd.data());
  argv.push_back(dlopen_arg.data());
  std::string ir_path_copy = ir_path;
  argv.push_back(ir_path_copy.data());
  argv.push_back(nullptr);

  pid_t pid = 0;
  int spawn_result =
      posix_spawnp(&pid, "lli", nullptr, nullptr, argv.data(), environ);

  if (spawn_result != 0) {
    return -1;
  }

  int status = 0;
  if (waitpid(pid, &status, 0) == -1) {
    return -1;
  }

  if (WIFEXITED(status)) {
    return WEXITSTATUS(status);
  }
  return -1;
}

// Slot information for all module variables from HIR
struct SlotData {
  std::vector<lowering::mir_to_llvm::SlotTypeInfo> types;
  std::vector<TypeId> type_ids;
};

// Build slot type info for ALL module variables from HIR
auto BuildSlotData(const CompilationResult& compilation) -> SlotData {
  SlotData result;

  // Find the HIR module
  const hir::Module* hir_module = nullptr;
  for (const auto& element : compilation.hir.design.elements) {
    if (const auto* mod = std::get_if<hir::Module>(&element)) {
      hir_module = mod;
      break;
    }
  }
  if (hir_module == nullptr) {
    return result;
  }

  const auto& type_arena = *compilation.hir.type_arena;
  const auto& symbol_table = *compilation.hir.symbol_table;

  result.types.reserve(hir_module->variables.size());
  result.type_ids.reserve(hir_module->variables.size());

  for (SymbolId sym_id : hir_module->variables) {
    const auto& sym = symbol_table[sym_id];
    const Type& type = type_arena[sym.type];

    // Always store the actual TypeId for LLVM type derivation
    result.type_ids.push_back(sym.type);

    if (type.Kind() == TypeKind::kReal) {
      result.types.push_back({
          .kind = lowering::mir_to_llvm::VarTypeKind::kReal,
          .width = 64,
          .is_signed = true,
      });
    } else if (type.Kind() == TypeKind::kString) {
      result.types.push_back({
          .kind = lowering::mir_to_llvm::VarTypeKind::kString,
          .width = 0,
          .is_signed = false,
      });
    } else if (IsPacked(type)) {
      uint32_t width = PackedBitWidth(type, type_arena);
      bool is_signed = IsPackedSigned(type, type_arena);
      result.types.push_back({
          .kind = lowering::mir_to_llvm::VarTypeKind::kIntegral,
          .width = width > 0 ? width : 32,
          .is_signed = is_signed,
      });
    } else {
      // Unsupported type - use placeholder for SlotTypeInfo
      // The actual TypeId is preserved for LLVM type derivation
      result.types.push_back({
          .kind = lowering::mir_to_llvm::VarTypeKind::kIntegral,
          .width = 32,
          .is_signed = false,
      });
    }
  }
  return result;
}

// Resolve an UnsupportedError origin to a source location string.
// Returns the location if resolvable, otherwise returns empty string.
auto ResolveErrorLocation(
    const common::UnsupportedError& error, const CompilationResult& compilation)
    -> std::string {
  if (!error.origin.IsValid()) {
    return "";
  }

  auto entry = compilation.mir.origin_map.Resolve(error.origin);
  if (!entry) {
    return "";
  }

  // For now, we only handle statement origins
  if (!std::holds_alternative<hir::StatementId>(entry->hir_source)) {
    return "";
  }

  auto stmt_id = std::get<hir::StatementId>(entry->hir_source);
  const hir::Statement& stmt = (*compilation.hir.hir_arena)[stmt_id];

  return FormatSourceLocation(stmt.span, *compilation.hir.source_manager);
}

}  // namespace

auto RunLlvm(const std::vector<std::string>& files) -> int {
  auto compilation = CompileToMir(files);
  if (!compilation) {
    PrintError(compilation.error());
    return 1;
  }

  auto slot_data = BuildSlotData(*compilation);
  lowering::mir_to_llvm::LoweringInput llvm_input{
      .design = &compilation->mir.design,
      .mir_arena = compilation->mir.mir_arena.get(),
      .type_arena = compilation->hir.type_arena.get(),
      .slot_types = std::move(slot_data.types),
      .slot_type_ids = std::move(slot_data.type_ids),
      .variables = {},  // No inspection for CLI
  };

  lowering::mir_to_llvm::LoweringResult llvm_result;
  try {
    llvm_result = lowering::mir_to_llvm::LowerMirToLlvm(llvm_input);
  } catch (const common::UnsupportedErrorException& e) {
    std::string location = ResolveErrorLocation(e.GetError(), *compilation);
    // Format in clang style:
    // - With location: "file:line:col: error: message"
    // - Without location: "lyra: error: message"
    if (!location.empty()) {
      fmt::print(
          stderr, "{}: {}: {}\n",
          fmt::styled(location, fmt::fg(fmt::terminal_color::cyan)),
          fmt::styled("error", fmt::fg(fmt::terminal_color::bright_red)),
          fmt::styled(e.what(), fmt::emphasis::bold));
    } else {
      fmt::print(
          stderr, "{}: {}: {}\n",
          fmt::styled(
              "lyra",
              fmt::fg(fmt::terminal_color::white) | fmt::emphasis::bold),
          fmt::styled("error", fmt::fg(fmt::terminal_color::bright_red)),
          fmt::styled(e.what(), fmt::emphasis::bold));
    }
    return 1;
  } catch (const std::exception& e) {
    // No location available - use clang style: "lyra: error: message"
    fmt::print(
        stderr, "{}: {}: {}\n",
        fmt::styled(
            "lyra", fmt::fg(fmt::terminal_color::white) | fmt::emphasis::bold),
        fmt::styled("error", fmt::fg(fmt::terminal_color::bright_red)),
        fmt::styled(e.what(), fmt::emphasis::bold));
    return 1;
  }

  std::string ir_path = CreateTempFile(".ll");
  if (ir_path.empty()) {
    PrintError(
        fmt::format("failed to create temp file: {}", std::strerror(errno)));
    return 1;
  }
  TempFileGuard temp_guard(ir_path);

  {
    std::ofstream out(ir_path);
    if (!out) {
      PrintError(fmt::format("failed to write to {}", ir_path));
      return 1;
    }
    out << lowering::mir_to_llvm::DumpLlvmIr(llvm_result);
  }

  std::vector<std::string> tried_paths;
  std::string runtime_path = FindRuntimeLibrary(tried_paths);
  if (runtime_path.empty()) {
    std::string msg = "runtime library not found\n       tried:";
    for (const auto& path : tried_paths) {
      msg += fmt::format("\n         - {}", path);
    }
    msg += "\n       hint: set LYRA_RUNTIME_PATH environment variable";
    PrintError(msg);
    return 1;
  }

  int exit_code = RunLli(runtime_path, ir_path);
  if (exit_code == -1) {
    PrintError(
        fmt::format(
            "failed to execute lli: {}\n"
            "       hint: ensure 'lli' is installed and in PATH",
            std::strerror(errno)));
    return 1;
  }

  return exit_code;
}

}  // namespace lyra::driver
