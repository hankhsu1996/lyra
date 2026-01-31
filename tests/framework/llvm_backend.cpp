#include "tests/framework/llvm_backend.hpp"

#include <algorithm>
#include <cctype>
#include <cstdint>
#include <expected>
#include <filesystem>
#include <format>
#include <map>
#include <sstream>
#include <string>
#include <string_view>
#include <type_traits>
#include <utility>
#include <variant>
#include <vector>

#include <llvm/IR/Value.h>

#include "lyra/common/diagnostic/diagnostic.hpp"
#include "lyra/common/diagnostic/diagnostic_sink.hpp"
#include "lyra/common/opt_level.hpp"
#include "lyra/common/source_span.hpp"
#include "lyra/common/type.hpp"
#include "lyra/common/type_arena.hpp"
#include "lyra/hir/module.hpp"
#include "lyra/hir/package.hpp"
#include "lyra/llvm_backend/context.hpp"
#include "lyra/llvm_backend/execution.hpp"
#include "lyra/llvm_backend/layout/layout.hpp"
#include "lyra/llvm_backend/lower.hpp"
#include "lyra/lowering/ast_to_hir/lower.hpp"
#include "lyra/lowering/diagnostic_context.hpp"
#include "lyra/lowering/hir_to_mir/lower.hpp"
#include "lyra/lowering/origin_map_lookup.hpp"
#include "lyra/runtime/output_sink.hpp"
#include "tests/framework/runner_common.hpp"
#include "tests/framework/test_case.hpp"
#include "tests/framework/test_value.hpp"

namespace lyra::test {
namespace {

// Test framework hooks for variable inspection and timing.
class TestSimulationHooks : public lowering::mir_to_llvm::SimulationHooks {
 public:
  TestSimulationHooks(
      std::vector<lowering::mir_to_llvm::VariableInfo> variables,
      bool emit_time_report)
      : variables_(std::move(variables)), emit_time_report_(emit_time_report) {
  }

  void OnAfterRunSimulation(
      lowering::mir_to_llvm::Context& context,
      const std::vector<lowering::mir_to_llvm::SlotInfo>& slots,
      llvm::Value* design_state) override {
    lowering::mir_to_llvm::EmitVariableInspection(
        context, variables_, slots, design_state);
    if (emit_time_report_) {
      lowering::mir_to_llvm::EmitTimeReport(context);
    }
  }

 private:
  std::vector<lowering::mir_to_llvm::VariableInfo> variables_;
  bool emit_time_report_;
};

// Build tracked variables for inspection (maps variable names to slot IDs)
// Note: slot_id is the index into the module's variables list, which
// corresponds to the design slot ID (after any package variables).
auto BuildTrackedVariables(
    const hir::Module* hir_module,
    const lowering::hir_to_mir::LoweringInput& mir_input,
    const lowering::ast_to_hir::LoweringResult& hir_result, size_t base_slot_id)
    -> std::vector<lowering::mir_to_llvm::VariableInfo> {
  std::vector<lowering::mir_to_llvm::VariableInfo> variables;
  if (hir_module == nullptr) {
    return variables;
  }

  for (size_t i = 0; i < hir_module->variables.size(); ++i) {
    const auto& sym = (*mir_input.symbol_table)[hir_module->variables[i]];
    const Type& type = (*hir_result.type_arena)[sym.type];

    // Handle real types
    if (type.Kind() == TypeKind::kReal) {
      variables.push_back({.name = sym.name, .slot_id = base_slot_id + i});
      continue;
    }

    // Handle string types (no variable inspection yet)
    if (type.Kind() == TypeKind::kString) {
      continue;
    }

    // Handle packed integral types
    if (!IsPacked(type)) {
      continue;
    }

    variables.push_back({.name = sym.name, .slot_id = base_slot_id + i});
  }
  return variables;
}

// Parse hex string into IntegralValue (unknown bits are all 0)
auto HexToIntegral(const std::string& hex, uint32_t width) -> IntegralValue {
  IntegralValue result;
  result.width = width;
  size_t num_words = (width + 63) / 64;
  result.value.resize(num_words, 0);
  result.unknown.resize(num_words, 0);  // No X/Z from LLVM backend

  // Parse hex string from LSB (rightmost) to MSB
  size_t hex_len = hex.size();
  for (size_t i = 0; i < hex_len; ++i) {
    char c = hex[hex_len - 1 - i];
    uint64_t nibble = 0;
    if (c >= '0' && c <= '9') {
      nibble = c - '0';
    } else if (c >= 'a' && c <= 'f') {
      nibble = 10 + (c - 'a');
    } else if (c >= 'A' && c <= 'F') {
      nibble = 10 + (c - 'A');
    }

    size_t bit_pos = i * 4;
    size_t word_idx = bit_pos / 64;
    size_t bit_in_word = bit_pos % 64;

    if (word_idx < num_words) {
      result.value[word_idx] |= nibble << bit_in_word;
    }
  }

  // Mask top word to actual width
  if (width > 0 && width % 64 != 0) {
    uint64_t mask = (uint64_t{1} << (width % 64)) - 1;
    result.value.back() &= mask;
  }

  return result;
}

// Parse a single __LYRA_VAR entry and extract variable value.
void ParseLyraVarEntry(
    std::string_view entry, std::map<std::string, TestValue>& variables) {
  // New format: "v:i:name=literal" or "v:r:name=value"
  // Old format (deprecated): "i:width:name=value" or "h:width:name=hex" or
  // "r:name=value"
  if (entry.size() < 2 || entry[1] != ':') {
    return;
  }

  char type_tag = entry[0];
  auto rest = entry.substr(2);  // Skip "X:"

  if (type_tag == 'v') {
    // New v: protocol: v:kind:name=literal
    // kind is 'i' for integral (SV literal), 'r' for real (plain decimal)
    if (rest.size() < 2 || rest[1] != ':') {
      return;
    }
    char var_kind = rest[0];
    auto name_literal = rest.substr(2);
    auto eq_pos = name_literal.find('=');
    if (eq_pos == std::string::npos) {
      return;
    }
    std::string name(name_literal.substr(0, eq_pos));
    std::string_view literal = name_literal.substr(eq_pos + 1);

    if (var_kind == 'i') {
      // Integral: parse SV literal (N'b... or N'h...)
      auto result = ParseSvLiteral(literal);
      if (result) {
        variables[name] = *result;
      }
    } else if (var_kind == 'r') {
      // Real: parse as double (not an SV literal)
      variables[name] = std::stod(std::string(literal));
    }
  } else if (type_tag == 'i' || type_tag == 'h') {
    // Old format: i:width:name=value or h:width:name=hex
    auto colon_pos = rest.find(':');
    if (colon_pos == std::string::npos) {
      return;
    }
    auto width = static_cast<uint32_t>(
        std::stoul(std::string(rest.substr(0, colon_pos))));
    auto name_value = rest.substr(colon_pos + 1);
    auto eq_pos = name_value.find('=');
    if (eq_pos == std::string::npos) {
      return;
    }
    std::string name(name_value.substr(0, eq_pos));
    std::string value_str(name_value.substr(eq_pos + 1));

    if (type_tag == 'i') {
      // Narrow integral - parse as unsigned and create IntegralValue
      uint64_t raw = std::stoull(value_str);
      IntegralValue fs;
      fs.width = width;
      fs.value = {raw};
      fs.unknown = {0};
      if (width > 0 && width < 64) {
        fs.value[0] &= (uint64_t{1} << width) - 1;
      }
      variables[name] = fs;
    } else {
      // Wide integral (hex)
      std::string hex(value_str);
      // Normalize to lowercase
      std::ranges::transform(hex, hex.begin(), [](unsigned char c) {
        return static_cast<char>(std::tolower(c));
      });
      variables[name] = HexToIntegral(hex, width);
    }
  } else if (type_tag == 'r') {
    // Old format: r:name=value (no width)
    auto eq_pos = rest.find('=');
    if (eq_pos != std::string::npos) {
      std::string name(rest.substr(0, eq_pos));
      std::string value_str(rest.substr(eq_pos + 1));
      variables[name] = std::stod(value_str);
    }
  }
}

struct ParsedOutput {
  std::string clean;
  std::map<std::string, TestValue> variables;
  uint64_t final_time = 0;
};

// Parse __LYRA_VAR and __LYRA_TIME__ output, stripping protocol lines.
// Preserves all other output exactly, including trailing newlines.
// Handles __LYRA_VAR and __LYRA_TIME__ appearing mid-line (after $write with no
// newline).
auto ParseLyraVarOutput(const std::string& output) -> ParsedOutput {
  ParsedOutput parsed;
  std::string& clean_output = parsed.clean;
  std::map<std::string, TestValue>& variables = parsed.variables;

  // Check if original output ends with newline - we need this to know
  // whether to add a trailing newline to the clean output
  bool output_ends_with_newline = !output.empty() && output.back() == '\n';

  std::istringstream stream(output);
  std::string line;
  bool first_clean_line = true;
  // Track whether user content should have trailing newline.
  // True if the last user content was on its own line (followed by \n),
  // false if user content was followed immediately by __LYRA_VAR (mid-line).
  bool user_ends_with_newline = false;
  constexpr std::string_view kPrefix = "__LYRA_VAR:";
  constexpr std::string_view kTimePrefix = "__LYRA_TIME__=";

  while (std::getline(stream, line)) {
    // Check for __LYRA_TIME__=<N> anywhere in the line
    auto time_pos = line.find(kTimePrefix);
    if (time_pos != std::string::npos) {
      parsed.final_time =
          std::stoull(std::string(line.substr(time_pos + kTimePrefix.size())));
      if (time_pos == 0) {
        // Line starts with __LYRA_TIME__= - no user content on this line
        continue;
      }
      // __LYRA_TIME__= appears mid-line (after $write output with no newline)
      if (!first_clean_line) {
        clean_output += '\n';
      }
      clean_output += line.substr(0, time_pos);
      first_clean_line = false;
      user_ends_with_newline = false;
      continue;
    }

    // Look for __LYRA_VAR: anywhere in the line
    auto var_pos = line.find(kPrefix);

    if (var_pos == std::string::npos) {
      // No __LYRA_VAR: in this line - add whole line to clean output
      if (!first_clean_line) {
        clean_output += '\n';
      }
      clean_output += line;
      first_clean_line = false;
      // Assume this line had user content and ended with \n, but we'll
      // correct this assumption at the end using output_ends_with_newline
      user_ends_with_newline = true;
    } else if (var_pos == 0) {
      // Line starts with __LYRA_VAR: - previous user content ended with \n
      // (Don't change user_ends_with_newline - keep previous value)
      ParseLyraVarEntry(line.substr(kPrefix.size()), variables);
    } else {
      // __LYRA_VAR: appears mid-line (after $write output with no newline)
      if (!first_clean_line) {
        clean_output += '\n';
      }
      clean_output += line.substr(0, var_pos);
      first_clean_line = false;
      // User content was NOT followed by newline (it was followed by
      // __LYRA_VAR)
      user_ends_with_newline = false;
      ParseLyraVarEntry(line.substr(var_pos + kPrefix.size()), variables);
    }
  }

  // Add trailing newline only if:
  // 1. User content should have one (user_ends_with_newline)
  // 2. There was some clean output (!first_clean_line)
  // 3. The original output actually ended with a newline
  if (user_ends_with_newline && !first_clean_line && output_ends_with_newline) {
    clean_output += '\n';
  }

  return parsed;
}

}  // namespace

auto RunLlvmBackend(
    const TestCase& test_case, const std::filesystem::path& work_directory)
    -> TestResult {
  TestResult result;

  // Parse test case using slang
  auto parse_result = ParseTestCase(test_case, work_directory);
  if (!parse_result.Success()) {
    result.error_message = parse_result.error_message;
    return result;
  }

  // Lower AST to HIR
  DiagnosticSink sink;
  auto hir_result =
      lowering::ast_to_hir::LowerAstToHir(*parse_result.compilation, sink);

  if (sink.HasErrors()) {
    std::ostringstream error_stream;
    for (const auto& diagnostic : sink.GetDiagnostics()) {
      if (diagnostic.primary.kind == DiagKind::kError ||
          diagnostic.primary.kind == DiagKind::kUnsupported ||
          diagnostic.primary.kind == DiagKind::kHostError) {
        error_stream << diagnostic.primary.message << "\n";
      }
    }
    result.error_message = "HIR lowering errors:\n" + error_stream.str();
    return result;
  }

  // Lower HIR to MIR
  lowering::hir_to_mir::LoweringInput mir_input{
      .design = &hir_result.design,
      .hir_arena = hir_result.hir_arena.get(),
      .type_arena = hir_result.type_arena.get(),
      .constant_arena = hir_result.constant_arena.get(),
      .symbol_table = hir_result.symbol_table.get(),
      .builtin_types = {},
      .binding_plan = &hir_result.binding_plan,
      .instance_table = &hir_result.instance_table,
  };
  auto mir_result = lowering::hir_to_mir::LowerHirToMir(mir_input);
  if (!mir_result) {
    result.error_message = std::format(
        "MIR lowering error: {}", mir_result.error().primary.message);
    return result;
  }

  // Find the top module (first module in elaboration order) and calculate base
  // slot ID. Slot ordering: packages first, then all modules' variables in
  // element order.
  const hir::Module* top_module = nullptr;
  for (const auto& element : mir_input.design->elements) {
    if (const auto* mod = std::get_if<hir::Module>(&element)) {
      top_module = mod;
      break;  // First module is the top module
    }
  }

  // Count package slots before module variables (packages come first)
  size_t base_slot_id = 0;
  for (const auto& element : mir_input.design->elements) {
    if (const auto* pkg = std::get_if<hir::Package>(&element)) {
      base_slot_id += pkg->variables.size();
    }
  }
  // Top module is first, so its variables start right after packages
  auto tracked_variables =
      BuildTrackedVariables(top_module, mir_input, hir_result, base_slot_id);

  // Create test hooks for variable inspection and timing
  TestSimulationHooks hooks(std::move(tracked_variables), true);

  // Create diagnostic context for LLVM backend error reporting
  lowering::OriginMapLookup origin_lookup(
      &mir_result->origin_map, hir_result.hir_arena.get());
  lowering::DiagnosticContext diag_ctx(origin_lookup);

  // Lower MIR to LLVM IR
  // Use work_directory for file I/O tests, otherwise fall back to CWD
  auto fs_base_dir =
      work_directory.empty()
          ? std::filesystem::absolute(std::filesystem::current_path()).string()
          : std::filesystem::absolute(work_directory).string();
  lowering::mir_to_llvm::LoweringInput llvm_input{
      .design = &mir_result->design,
      .mir_arena = mir_result->mir_arena.get(),
      .type_arena = hir_result.type_arena.get(),
      .diag_ctx = &diag_ctx,
      .hooks = &hooks,
      .fs_base_dir = fs_base_dir,
      .plusargs = test_case.plusargs,
  };

  auto llvm_result = lowering::mir_to_llvm::LowerMirToLlvm(llvm_input);
  if (!llvm_result) {
    const auto& diag = llvm_result.error();
    std::string location;
    std::visit(
        [&](const auto& span) {
          using T = std::decay_t<decltype(span)>;
          if constexpr (std::is_same_v<T, SourceSpan>) {
            location = FormatSourceLocation(span, *hir_result.source_manager);
          }
        },
        diag.primary.span);
    if (!location.empty()) {
      result.error_message =
          std::format("{}: error: {}", location, diag.primary.message);
    } else {
      result.error_message =
          std::format("LLVM lowering error: {}", diag.primary.message);
    }
    return result;
  }

  // Execute using in-process ORC JIT with host process symbols.
  // This ensures JIT code uses the same runtime globals as the test framework.
  // OutputSinkScope captures JIT output and restores previous sink on exit.
  std::string captured_output;
  std::expected<int, std::string> exec_result;
  {
    runtime::OutputSinkScope sink_scope(
        [&captured_output](std::string_view text) {
          captured_output.append(text);
        });
    exec_result = lowering::mir_to_llvm::ExecuteWithOrcJitInProcess(
        *llvm_result, OptLevel::kO0);
  }

  if (!exec_result) {
    result.error_message =
        std::format("JIT execution failed: {}", exec_result.error());
    return result;
  }

  // Parse output to extract variables and time
  auto parsed = ParseLyraVarOutput(captured_output);
  result.success = true;
  result.captured_output = std::move(parsed.clean);
  result.variables = std::move(parsed.variables);
  result.final_time = parsed.final_time;
  return result;
}

}  // namespace lyra::test
