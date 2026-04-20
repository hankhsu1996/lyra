#include "emit_cpp.hpp"

#include <filesystem>
#include <fstream>
#include <optional>

#include <fmt/core.h>

#include "compilation_output.hpp"
#include "driver_output_options.hpp"
#include "frontend.hpp"
#include "lyra/common/diagnostic/diagnostic.hpp"
#include "lyra/common/diagnostic/diagnostic_sink.hpp"
#include "lyra/hir/module.hpp"
#include "lyra/lowering/ast_to_hir/lower.hpp"
#include "lyra/lowering/hir_to_xir/lower.hpp"
#include "lyra/projection/cpp/render.hpp"
#include "runtime_path.hpp"

namespace lyra::driver {

namespace {

namespace fs = std::filesystem;

constexpr std::string_view kProjectionRuntimeLib =
    "liblyra_projection_runtime.a";

auto BuildCompilationUnitInputForPrototype(
    const lowering::ast_to_hir::LoweringResult& hir_result)
    -> Result<lowering::hir_to_xir::CompilationUnitInput> {
  if (hir_result.module_bodies.size() != 1) {
    return std::unexpected(
        Diagnostic::HostError(
            "emit-cpp prototype: requires exactly one module body"));
  }

  if (hir_result.modules.size() != 1) {
    return std::unexpected(
        Diagnostic::HostError(
            "emit-cpp prototype: requires exactly one module in design"));
  }
  const hir::Module* module = hir_result.modules.data();

  if (module->body_id.value != 0) {
    return std::unexpected(
        Diagnostic::HostError(
            "emit-cpp prototype: module body_id does not point to body 0"));
  }

  const auto& body = hir_result.module_bodies[0];
  const auto& sym = (*hir_result.symbol_table)[module->symbol];

  return lowering::hir_to_xir::CompilationUnitInput{
      .body = &body,
      .hir_arena = &body.arena,
      .type_arena = hir_result.type_arena.get(),
      .symbol_table = hir_result.symbol_table.get(),
      .constant_arena = &body.constant_arena,
      .compilation_unit_name = sym.name,
      .variable_symbols = module->variables,
  };
}

auto WriteFile(
    const fs::path& path, std::string_view content, CompilationOutput& output)
    -> bool {
  std::ofstream ofs(path);
  if (!ofs) {
    output.PrintError(std::format("cannot write '{}'", path.string()));
    return false;
  }
  ofs << content;
  return true;
}

auto CopyFile(
    const fs::path& src, const fs::path& dst, CompilationOutput& output)
    -> bool {
  std::error_code ec;
  fs::copy_file(src, dst, fs::copy_options::overwrite_existing, ec);
  if (ec) {
    output.PrintError(
        std::format(
            "cannot copy '{}' to '{}': {}", src.string(), dst.string(),
            ec.message()));
    return false;
  }
  return true;
}

auto GenerateMakefile(std::string_view cu_name) -> std::string {
  std::string out;
  out += "CXX ?= clang++\n";
  out += "CXXFLAGS ?= -std=c++23 -O2\n";
  out += "\n";
  out += std::format("TARGET = {}\n", cu_name);
  out += "SRCS = main.cpp\n";
  out += "RUNTIME_LIB = liblyra_projection_runtime.a\n";
  out += "\n";
  out += "$(TARGET): $(SRCS) $(RUNTIME_LIB)\n";
  out += "\t$(CXX) $(CXXFLAGS) -I. $(SRCS) $(RUNTIME_LIB)";
  out += " -lstdc++ -lm -lpthread -o $(TARGET)\n";
  out += "\n";
  out += "clean:\n";
  out += "\trm -f $(TARGET)\n";
  out += "\n";
  out += ".PHONY: clean\n";
  return out;
}

}  // namespace

auto EmitCpp(const CompilationInput& input, const std::string& output_dir)
    -> int {
  CompilationOutput output(BuildDumpDriverOutputOptions(input));

  std::optional<ParseResult> parse_result;
  {
    PhaseTimer timer(output, Phase::kParse);
    parse_result = ParseFiles(input);
  }
  if (!parse_result) {
    output.Flush();
    return 1;
  }

  {
    PhaseTimer timer(output, Phase::kElaborate);
    if (!Elaborate(*parse_result, input)) {
      output.Flush();
      return 1;
    }
  }

  DiagnosticSink sink;
  lowering::ast_to_hir::AstToHirOutput ast_to_hir;
  {
    PhaseTimer timer(output, Phase::kLowerHir);
    lowering::ast_to_hir::HirLoweringOptions hir_options{
        .disable_assertions = input.disable_assertions,
    };
    ast_to_hir = lowering::ast_to_hir::LowerAstToHir(
        *parse_result->compilation, sink, hir_options);
  }

  auto& hir_result = ast_to_hir.hir;

  if (sink.HasErrors()) {
    output.PrintDiagnostics(sink, hir_result.source_manager.get());
    output.Flush();
    return 1;
  }

  auto cu_input = BuildCompilationUnitInputForPrototype(hir_result);
  if (!cu_input) {
    output.PrintDiagnostic(cu_input.error());
    output.Flush();
    return 1;
  }

  auto xir_cu = lowering::hir_to_xir::LowerToXir(*cu_input);
  if (!xir_cu) {
    output.PrintDiagnostic(xir_cu.error());
    output.Flush();
    return 1;
  }

  auto per_cu = projection::cpp::ProjectPerCU(
      *xir_cu, *hir_result.type_arena,
      hir_result.module_bodies[0].constant_arena);
  if (!per_cu) {
    output.PrintDiagnostic(per_cu.error());
    output.Flush();
    return 1;
  }
  auto host = projection::cpp::ProjectHost(*xir_cu);

  auto header_text = projection::cpp::RenderPerCUHeader(*per_cu);
  auto internal_entry_text = projection::cpp::RenderInternalEntry(host);
  auto host_text = projection::cpp::RenderHostEntry(host);

  // Create output directory and hidden internal directory
  std::error_code ec;
  fs::create_directories(output_dir, ec);
  if (ec) {
    output.PrintError(
        std::format("cannot create '{}': {}", output_dir, ec.message()));
    output.Flush();
    return 1;
  }

  auto out = fs::path(output_dir);
  fs::create_directories(out / ".lyra_internal", ec);
  if (ec) {
    output.PrintError(
        std::format(
            "cannot create '{}': {}", (out / ".lyra_internal").string(),
            ec.message()));
    output.Flush();
    return 1;
  }

  // Write projected module header
  if (!WriteFile(out / (xir_cu->name + ".hpp"), header_text, output)) {
    output.Flush();
    return 1;
  }

  // Write hidden internal entry helper
  if (!WriteFile(
          out / ".lyra_internal" / (xir_cu->name + "_entry.hpp"),
          internal_entry_text, output)) {
    output.Flush();
    return 1;
  }

  // Write entry file
  if (!WriteFile(out / "main.cpp", host_text, output)) {
    output.Flush();
    return 1;
  }

  // Copy runtime support header into output as lyra_projection_runtime.hpp
  constexpr std::string_view kRuntimeHeader =
      "include/lyra/projection/cpp/runtime_bridge.hpp";
  std::vector<std::string> header_tried_paths;
  auto runtime_header =
      FindRuntimeSupportFile(kRuntimeHeader, header_tried_paths);
  if (runtime_header.empty()) {
    std::string msg = "projection runtime header not found\n       tried:";
    for (const auto& path : header_tried_paths) {
      msg += std::format("\n         - {}", path);
    }
    output.PrintError(msg);
    output.Flush();
    return 1;
  }
  if (!CopyFile(runtime_header, out / "lyra_projection_runtime.hpp", output)) {
    output.Flush();
    return 1;
  }

  // Copy pre-built runtime library into output
  std::vector<std::string> tried_paths;
  auto runtime_lib = FindRuntimeLibrary(kProjectionRuntimeLib, tried_paths);
  if (runtime_lib.empty()) {
    std::string msg = "projection runtime library not found\n       tried:";
    for (const auto& path : tried_paths) {
      msg += std::format("\n         - {}", path);
    }
    output.PrintError(msg);
    output.Flush();
    return 1;
  }
  if (!CopyFile(runtime_lib, out / kProjectionRuntimeLib, output)) {
    output.Flush();
    return 1;
  }

  // Generate Makefile
  if (!WriteFile(out / "Makefile", GenerateMakefile(xir_cu->name), output)) {
    output.Flush();
    return 1;
  }

  fmt::print(stderr, "emitted project: {}/\n", output_dir);

  output.Flush();
  return 0;
}

}  // namespace lyra::driver
