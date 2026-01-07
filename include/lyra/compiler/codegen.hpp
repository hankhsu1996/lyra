#pragma once

#include <cstdint>
#include <optional>
#include <sstream>
#include <string>
#include <unordered_set>

#include <slang/ast/Symbol.h>

#include "lyra/common/timescale.hpp"
#include "lyra/mir/expression.hpp"
#include "lyra/mir/module.hpp"

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
enum class CodegenFeature : uint8_t {
  kNone = 0,
  kCmath = 1 << 0,                 // #include <cmath> for std::pow
  kTimeDivisor = 1 << 1,           // delays, $time, $stime, $realtime
  kModuleUnitPower = 1 << 2,       // $timeunit, %t format
  kModulePrecisionPower = 1 << 3,  // $timeprecision
  kModuleName = 1 << 4,            // $printtimescale
  kTimescaleStr = 1 << 5,          // $printtimescale
  kDisplay = 1 << 6,               // #include <iostream>, <print> for $display
};

// Bitwise OR for combining CodegenFeature flags
inline auto operator|(CodegenFeature lhs, CodegenFeature rhs)
    -> CodegenFeature {
  return static_cast<CodegenFeature>(
      static_cast<uint8_t>(lhs) | static_cast<uint8_t>(rhs));
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
      static_cast<uint8_t>(lhs) & static_cast<uint8_t>(rhs));
}

class Codegen {
 public:
  auto Generate(const mir::Module& module) -> std::string;

  // Get global precision power after Generate() has been called
  // Used by main.cpp generation to initialize lyra::sdk::global_precision_power
  [[nodiscard]] auto GetGlobalPrecisionPower() const -> int8_t {
    return global_precision_power_;
  }

 private:
  void EmitHeader(
      const std::vector<mir::SubmoduleInstance>& submodules, bool uses_arrays);
  void EmitClass(const mir::Module& module);
  void EmitVariables(const std::vector<mir::ModuleVariable>& variables);
  void EmitProcess(const mir::Process& process);
  void EmitStatement(const mir::Statement& stmt);
  void EmitConditional(const mir::ConditionalStatement& cond, bool is_else_if);
  void EmitExpression(const mir::Expression& expr, int parent_prec = 0);
  void EmitAssignmentTarget(const mir::AssignmentTarget& target);
  void EmitPackedBitPosition(
      const mir::Expression& index_expr, int32_t lower_bound,
      size_t element_width);
  void EmitSliceShift(
      const mir::Expression& start_expr, int32_t lower_bound,
      int32_t width_offset);
  void EmitHierarchicalPath(const std::vector<std::string>& path);

  std::ostringstream out_;
  int indent_ = 0;

  // Timescale info for delay scaling
  std::optional<common::TimeScale> timescale_;
  int8_t global_precision_power_ = common::TimeScale::kDefaultPrecisionPower;

  [[nodiscard]] auto DelayMultiplier() const -> uint64_t;

  // Track port symbols for identifier emission (append _ suffix)
  std::unordered_set<const slang::ast::Symbol*> port_symbols_;

  // Track which type aliases are used for conditional emission
  TypeAlias used_type_aliases_ = TypeAlias::kNone;

  // Track which codegen features are used for conditional emission
  CodegenFeature used_features_ = CodegenFeature::kNone;

  // Type conversion helpers (record usage in used_type_aliases_)
  auto ToCppType(const common::Type& type) -> std::string;
  auto ToCppUnsignedType(const common::Type& type) -> std::string;
  static auto IsSigned(const common::Type& type) -> bool;
  void EmitTypeAliases();
  void EmitTimescaleConstants(const mir::Module& module);

  void Indent();
  void Line(const std::string& text);
};

}  // namespace lyra::compiler
