#pragma once

#include <cstddef>
#include <cstdint>
#include <functional>
#include <memory>
#include <optional>
#include <sstream>
#include <string>
#include <string_view>
#include <unordered_map>
#include <unordered_set>
#include <vector>

#include <slang/ast/Symbol.h>

#include "lyra/common/timescale.hpp"
#include "lyra/common/trigger.hpp"
#include "lyra/common/type.hpp"
#include "lyra/mir/expression.hpp"
#include "lyra/mir/module.hpp"
#include "lyra/mir/package.hpp"

namespace lyra::mir {
class CaseStatement;
class WhileStatement;
class DoWhileStatement;
class ForStatement;
class RepeatStatement;
}  // namespace lyra::mir

namespace lyra::compiler {

// Bit flags for tracking which type aliases are used during codegen
enum class TypeAlias : uint8_t {
  kNone = 0,
  kBit = 1 << 0,       // Bit<N> or Bit<N, true> template
  kInt = 1 << 1,       // 32-bit signed
  kLongInt = 1 << 2,   // 64-bit signed
  kShortInt = 1 << 3,  // 16-bit signed
  kByte = 1 << 4,      // 8-bit signed
  kReal = 1 << 5,      // double
  kShortReal = 1 << 6  // float
};

// Bitwise OR for combining TypeAlias flags
inline auto operator|(TypeAlias lhs, TypeAlias rhs) -> TypeAlias {
  return static_cast<TypeAlias>(
      static_cast<uint8_t>(lhs) | static_cast<uint8_t>(rhs));
}

inline auto operator|=(TypeAlias& lhs, TypeAlias rhs) -> TypeAlias& {
  lhs = lhs | rhs;
  return lhs;
}

// Bitwise AND for checking TypeAlias flags
inline auto operator&(TypeAlias lhs, TypeAlias rhs) -> TypeAlias {
  return static_cast<TypeAlias>(
      static_cast<uint8_t>(lhs) & static_cast<uint8_t>(rhs));
}

// Bit flags for tracking which codegen features are used
enum class CodegenFeature : uint16_t {
  kNone = 0,
  kCmath = 1 << 0,                 // #include <cmath> for std::pow
  kTimeDivisor = 1 << 1,           // delays, $time, $stime, $realtime
  kModuleUnitPower = 1 << 2,       // $timeunit, %t format
  kModulePrecisionPower = 1 << 3,  // $timeprecision
  kModuleName = 1 << 4,            // $printtimescale
  kTimescaleStr = 1 << 5,          // $printtimescale
  kDisplay = 1 << 6,               // #include <iostream>, <print> for $display
  kMemIo = 1 << 7,                 // #include <filesystem>, <fstream>
  kPlusargs = 1 << 8,  // plusargs.hpp for $test$plusargs, $value$plusargs
};

// Bitwise OR for combining CodegenFeature flags
inline auto operator|(CodegenFeature lhs, CodegenFeature rhs)
    -> CodegenFeature {
  return static_cast<CodegenFeature>(
      static_cast<uint16_t>(lhs) | static_cast<uint16_t>(rhs));
}

inline auto operator|=(CodegenFeature& lhs, CodegenFeature rhs)
    -> CodegenFeature& {
  lhs = lhs | rhs;
  return lhs;
}

// Bitwise AND for checking CodegenFeature flags
inline auto operator&(CodegenFeature lhs, CodegenFeature rhs)
    -> CodegenFeature {
  return static_cast<CodegenFeature>(
      static_cast<uint16_t>(lhs) & static_cast<uint16_t>(rhs));
}

// Output from GenerateAllModules - one entry per file write operation
struct ModuleOutput {
  std::string filename;  // e.g., "inner.hpp"
  std::string content;   // Generated C++ code
  bool append;           // true for subsequent specializations to same file
};

class Codegen {
 public:
  // Generate module class code (without header guards or includes)
  // @param skip_sdk_aliases If true, skip emitting SDK type aliases (Bit, Int,
  // etc.)
  //        inside the class - used when packages.hpp provides them at file
  //        scope
  // @param emit_file_header If true, emit SDK includes (based on feature
  // usage).
  //        False for subsequent specializations appended to the same file.
  // @param emit_primary_template If true and module is parameterized, emit
  //        the primary template forward declaration before the class.
  auto Generate(
      const mir::Module& module, bool skip_sdk_aliases = false,
      bool emit_file_header = true, bool emit_primary_template = true)
      -> std::string;

  // Generate packages header content (namespaces with type definitions)
  // @param emit_file_header If true, emit #pragma once, SDK includes, and type
  //        aliases at file scope. False for batch compilation.
  auto GeneratePackages(
      const std::vector<std::unique_ptr<mir::Package>>& packages,
      bool emit_file_header = true) -> std::string;

  // Generate complete module header with #pragma once and conditional includes
  // @param module The module to generate code for
  // @param has_packages Whether to include packages.hpp
  // @param emit_file_header True for first module in file (emits #pragma once,
  //        includes). False for appended specializations.
  // @param emit_primary_template True to emit primary template forward decl
  //        for parameterized modules.
  auto GenerateModuleHeader(
      const mir::Module& module, bool has_packages,
      bool emit_file_header = true, bool emit_primary_template = true)
      -> std::string;

  // Generate all module headers with signature-based deduplication.
  // Handles per-instance modules that share signatures, emitting each unique
  // signature once. Returns a list of file writes to perform.
  auto GenerateAllModules(
      const std::vector<std::unique_ptr<mir::Module>>& modules,
      bool has_packages) -> std::vector<ModuleOutput>;

  // Generate batch content for test compilation.
  // Combines packages and modules into a single string wrapped in namespace.
  // SDK includes are emitted outside the namespace, body inside.
  auto GenerateBatchContent(
      std::string_view namespace_name,
      const std::vector<std::unique_ptr<mir::Package>>& packages,
      const std::vector<std::unique_ptr<mir::Module>>& modules) -> std::string;

  // Get global precision power after Generate() has been called
  // Used by main.cpp generation to initialize lyra::sdk::global_precision_power
  [[nodiscard]] auto GetGlobalPrecisionPower() const -> int8_t {
    return global_precision_power_;
  }

  // Check if a codegen feature was used during generation
  [[nodiscard]] auto HasFeature(CodegenFeature feature) const -> bool {
    return (used_features_ & feature) != CodegenFeature::kNone;
  }

 private:
  void EmitHeader(const mir::Module& module, bool uses_arrays);
  void EmitUsingDirectives(const mir::Module& module);
  void EmitPrimaryTemplateDecl(const mir::Module& module);
  void EmitParamsStruct(const mir::Module& module);
  void EmitClass(const mir::Module& module);
  void EmitVariables(const std::vector<mir::ModuleVariable>& variables);
  void EmitGenerateScopeStruct(const mir::GenerateScope& scope);
  void EmitGenerateScopes(const std::vector<mir::GenerateScope>& scopes);
  void EmitProcess(const mir::Process& process);
  void EmitFunction(const mir::FunctionDefinition& function);
  void EmitStatement(const mir::Statement& stmt);
  void EmitConditional(const mir::ConditionalStatement& cond, bool is_else_if);
  void EmitUniquePriorityIf(const mir::ConditionalStatement& root);
  void EmitCaseStatement(const mir::CaseStatement& case_stmt);
  void EmitWhileLoop(const mir::WhileStatement& while_stmt);
  void EmitDoWhileLoop(const mir::DoWhileStatement& do_while);
  void EmitForLoop(const mir::ForStatement& for_stmt);
  void EmitRepeatLoop(const mir::RepeatStatement& repeat_stmt);
  void EmitSystemTask(const mir::SystemCallExpression& syscall);
  void EmitExpression(const mir::Expression& expr, int parent_prec = 0);
  void EmitConstantExpression(
      const mir::Expression& expr);  // For template args
  void EmitAssignmentTarget(const mir::AssignmentTarget& target);
  void EmitPackedBitPosition(
      const mir::Expression& index_expr, int32_t lower_bound,
      size_t element_width);
  void EmitCompositePackedBitPosition(
      const std::vector<std::unique_ptr<mir::Expression>>& indices,
      const common::Type& base_type);
  void EmitSliceShift(
      const mir::Expression& start_expr, int32_t lower_bound,
      int32_t width_offset);
  void EmitHierarchicalPath(
      const std::vector<mir::HierarchicalPathElement>& instance_path,
      mir::SymbolRef target_symbol);
  void EmitHierarchicalPath(const std::vector<std::string>& path);
  void EmitMethodCall(const mir::MethodCallExpression& mc);
  void EmitEnumNavMethod(const mir::MethodCallExpression& mc, bool is_next);
  void EmitEnumNameMethod(const mir::MethodCallExpression& mc);
  void EmitSliceExtract(
      const common::Type& result_type, const mir::Expression& value,
      const std::function<void()>& emit_shift, uint64_t mask, bool is_wide);

  // Emit a string literal argument, handling integral-encoded string literals.
  // Used for system calls like $test$plusargs, $value$plusargs, $readmemh.
  void EmitStringLiteralArg(const mir::Expression& arg);

  // Emit a formatted print call (std::print/println) for
  // $display/$strobe/$monitor
  // @param arguments The argument expressions
  // @param first_arg_idx Index of first value argument (after format string if
  // present)
  // @param sv_fmt SV format string (empty means generate default from arg
  // types)
  // @param print_fn Print function name ("std::print" or "std::println")
  // @param default_format Default format char for non-format-string case
  // ('d','b','o','x')
  void EmitFormattedPrint(
      const std::vector<std::unique_ptr<mir::Expression>>& arguments,
      size_t first_arg_idx, const std::string& sv_fmt,
      std::string_view print_fn, char default_format);

  // Get C++ member access path for a trigger (e.g., "u_child_.value")
  [[nodiscard]] auto GetTriggerPath(const common::Trigger& trigger) const
      -> std::string;

  std::ostringstream out_;
  int indent_ = 0;
  int temp_counter_ = 0;  // For unique temp variable names within a function

  // Timescale info for delay scaling
  std::optional<common::TimeScale> timescale_;
  int8_t global_precision_power_ = common::TimeScale::kDefaultPrecisionPower;

  [[nodiscard]] auto DelayMultiplier() const -> uint64_t;

  // Track port symbols for identifier emission (append _ suffix)
  std::unordered_set<const slang::ast::Symbol*> port_symbols_;

  // Track constructor param symbols for identifier emission (append _ suffix)
  // These are non-template params stored as class members.
  std::unordered_set<const slang::ast::Symbol*> constructor_param_symbols_;

  // Track which type aliases are used for conditional emission
  TypeAlias used_type_aliases_ = TypeAlias::kNone;

  // Track user-defined type aliases (typedef) in insertion order.
  // Dependencies are registered before dependents due to recursive ToCppType().
  std::vector<std::pair<std::string, std::string>> user_type_aliases_;
  std::unordered_set<std::string> user_type_names_;  // For dedup check

  // Track which codegen features are used for conditional emission
  CodegenFeature used_features_ = CodegenFeature::kNone;

  // When true, skip emitting SDK type aliases (they're at file scope from
  // packages.hpp)
  bool skip_sdk_aliases_ = false;

  // Maps C++ keywords to escaped names for collision-free codegen.
  // Built per-module/package before emission.
  std::unordered_map<std::string, std::string> escape_map_;

  // Type conversion helpers (record usage in used_type_aliases_)
  auto ToCppType(const common::Type& type) -> std::string;
  auto ToCppRawType(const common::Type& type)
      -> std::string;  // For template params
  auto ToCppUnsignedType(const common::Type& type) -> std::string;
  static auto IsSigned(const common::Type& type) -> bool;
  void EmitTypeAliases();
  void EmitTimescaleConstants(const mir::Module& module);

  // Build escape map for all identifiers in a module/package.
  // Maps C++ keywords to escaped names, handling collisions by trying
  // alternative suffixes (e.g., double_ -> double__ -> double_0_).
  void BuildEscapeMap(const mir::Module& module);
  void BuildEscapeMap(const mir::Package& package);

  // Get escaped identifier name. Returns original if not a C++ keyword.
  [[nodiscard]] auto Escape(std::string_view name) const -> std::string;

  // Escape qualified names like "MyPkg::double" using the collision-aware map.
  [[nodiscard]] auto EscapeQualified(std::string_view qualified_name) const
      -> std::string;

  void Indent();
  void Line(const std::string& text);
};

}  // namespace lyra::compiler
