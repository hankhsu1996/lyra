#pragma once

#include <array>
#include <cstdint>
#include <string_view>

namespace lyra::common {

/// Category of system function for dispatch
enum class SystemFunctionCategory : uint8_t {
  kSimControl,      // $finish, $stop, $exit
  kDisplay,         // $display (variadic, special handling)
  kTimeFormat,      // $timeformat (mutates global state)
  kPrintTimescale,  // $printtimescale
  kTimeQuery,       // $time, $stime, $realtime, $timeunit, $timeprecision
  kTypeConversion,  // $itor, $rtoi, $signed, $unsigned
  kBitCast,         // $realtobits, $bitstoreal, etc.
  kMathUnary,       // $ln, $sqrt, $sin, etc. (1 arg -> real)
  kMathBinary,      // $pow, $atan2, $hypot (2 args -> real)
  kMathIntegral,    // $clog2 (integral -> integral)
  kMemIo,           // $readmemh/$readmemb/$writememh/$writememb
};

/// Return type specification
enum class SystemFunctionReturnType : uint8_t {
  kVoid,        // System task (no return)
  kReal,        // Returns real (double)
  kIntegral32,  // Returns 32-bit integer
  kIntegral64,  // Returns 64-bit integer
  kSameAsArg,   // Return type matches argument ($signed, $unsigned)
};

/// Metadata for a single system function
struct SystemFunctionInfo {
  std::string_view name;
  SystemFunctionCategory category;
  uint8_t min_args;
  uint8_t max_args;
  SystemFunctionReturnType return_type;
  std::string_view cpp_function;  // Direct C++ mapping (e.g., "std::sin")
};

// Shorter aliases for readability in the table below
using Cat = SystemFunctionCategory;
using Ret = SystemFunctionReturnType;

// NOLINTBEGIN(readability-identifier-naming)
// clang-format off
inline constexpr std::array kSystemFunctions = std::to_array<SystemFunctionInfo>({
  // Simulation Control Tasks
  {.name = "$finish", .category = Cat::kSimControl, .min_args = 0, .max_args = 1, .return_type = Ret::kVoid, .cpp_function = ""},
  {.name = "$stop", .category = Cat::kSimControl, .min_args = 0, .max_args = 1, .return_type = Ret::kVoid, .cpp_function = ""},
  {.name = "$exit", .category = Cat::kSimControl, .min_args = 0, .max_args = 0, .return_type = Ret::kVoid, .cpp_function = ""},

  // Display Tasks
  {.name = "$display", .category = Cat::kDisplay, .min_args = 0, .max_args = 255, .return_type = Ret::kVoid, .cpp_function = ""},
  {.name = "$displayb", .category = Cat::kDisplay, .min_args = 0, .max_args = 255, .return_type = Ret::kVoid, .cpp_function = ""},
  {.name = "$displayo", .category = Cat::kDisplay, .min_args = 0, .max_args = 255, .return_type = Ret::kVoid, .cpp_function = ""},
  {.name = "$displayh", .category = Cat::kDisplay, .min_args = 0, .max_args = 255, .return_type = Ret::kVoid, .cpp_function = ""},
  {.name = "$write", .category = Cat::kDisplay, .min_args = 0, .max_args = 255, .return_type = Ret::kVoid, .cpp_function = ""},
  {.name = "$writeb", .category = Cat::kDisplay, .min_args = 0, .max_args = 255, .return_type = Ret::kVoid, .cpp_function = ""},
  {.name = "$writeo", .category = Cat::kDisplay, .min_args = 0, .max_args = 255, .return_type = Ret::kVoid, .cpp_function = ""},
  {.name = "$writeh", .category = Cat::kDisplay, .min_args = 0, .max_args = 255, .return_type = Ret::kVoid, .cpp_function = ""},
  {.name = "$strobe", .category = Cat::kDisplay, .min_args = 0, .max_args = 255, .return_type = Ret::kVoid, .cpp_function = ""},
  {.name = "$strobeb", .category = Cat::kDisplay, .min_args = 0, .max_args = 255, .return_type = Ret::kVoid, .cpp_function = ""},
  {.name = "$strobeo", .category = Cat::kDisplay, .min_args = 0, .max_args = 255, .return_type = Ret::kVoid, .cpp_function = ""},
  {.name = "$strobeh", .category = Cat::kDisplay, .min_args = 0, .max_args = 255, .return_type = Ret::kVoid, .cpp_function = ""},
  {.name = "$monitor", .category = Cat::kDisplay, .min_args = 0, .max_args = 255, .return_type = Ret::kVoid, .cpp_function = ""},
  {.name = "$monitorb", .category = Cat::kDisplay, .min_args = 0, .max_args = 255, .return_type = Ret::kVoid, .cpp_function = ""},
  {.name = "$monitoro", .category = Cat::kDisplay, .min_args = 0, .max_args = 255, .return_type = Ret::kVoid, .cpp_function = ""},
  {.name = "$monitorh", .category = Cat::kDisplay, .min_args = 0, .max_args = 255, .return_type = Ret::kVoid, .cpp_function = ""},
  {.name = "$monitoron", .category = Cat::kDisplay, .min_args = 0, .max_args = 0, .return_type = Ret::kVoid, .cpp_function = ""},
  {.name = "$monitoroff", .category = Cat::kDisplay, .min_args = 0, .max_args = 0, .return_type = Ret::kVoid, .cpp_function = ""},
  {.name = "$timeformat", .category = Cat::kTimeFormat, .min_args = 0, .max_args = 4, .return_type = Ret::kVoid, .cpp_function = ""},
  {.name = "$printtimescale", .category = Cat::kPrintTimescale, .min_args = 0, .max_args = 1, .return_type = Ret::kVoid, .cpp_function = ""},
  {.name = "$printtimescale_root", .category = Cat::kPrintTimescale, .min_args = 0, .max_args = 0, .return_type = Ret::kVoid, .cpp_function = ""},
  {.name = "$readmemh", .category = Cat::kMemIo, .min_args = 2, .max_args = 4, .return_type = Ret::kVoid, .cpp_function = ""},
  {.name = "$readmemb", .category = Cat::kMemIo, .min_args = 2, .max_args = 4, .return_type = Ret::kVoid, .cpp_function = ""},
  {.name = "$writememh", .category = Cat::kMemIo, .min_args = 2, .max_args = 4, .return_type = Ret::kVoid, .cpp_function = ""},
  {.name = "$writememb", .category = Cat::kMemIo, .min_args = 2, .max_args = 4, .return_type = Ret::kVoid, .cpp_function = ""},

  // Time Query Functions
  {.name = "$time", .category = Cat::kTimeQuery, .min_args = 0, .max_args = 0, .return_type = Ret::kIntegral64, .cpp_function = ""},
  {.name = "$stime", .category = Cat::kTimeQuery, .min_args = 0, .max_args = 0, .return_type = Ret::kIntegral32, .cpp_function = ""},
  {.name = "$realtime", .category = Cat::kTimeQuery, .min_args = 0, .max_args = 0, .return_type = Ret::kReal, .cpp_function = ""},
  {.name = "$timeunit", .category = Cat::kTimeQuery, .min_args = 0, .max_args = 1, .return_type = Ret::kIntegral32, .cpp_function = ""},
  {.name = "$timeprecision", .category = Cat::kTimeQuery, .min_args = 0, .max_args = 1, .return_type = Ret::kIntegral32, .cpp_function = ""},
  {.name = "$timeunit_root", .category = Cat::kTimeQuery, .min_args = 0, .max_args = 0, .return_type = Ret::kIntegral32, .cpp_function = ""},
  {.name = "$timeprecision_root", .category = Cat::kTimeQuery, .min_args = 0, .max_args = 0, .return_type = Ret::kIntegral32, .cpp_function = ""},

  // Type Conversion
  {.name = "$itor", .category = Cat::kTypeConversion, .min_args = 1, .max_args = 1, .return_type = Ret::kReal, .cpp_function = ""},
  {.name = "$rtoi", .category = Cat::kTypeConversion, .min_args = 1, .max_args = 1, .return_type = Ret::kIntegral32, .cpp_function = ""},
  {.name = "$signed", .category = Cat::kTypeConversion, .min_args = 1, .max_args = 1, .return_type = Ret::kSameAsArg, .cpp_function = ""},
  {.name = "$unsigned", .category = Cat::kTypeConversion, .min_args = 1, .max_args = 1, .return_type = Ret::kSameAsArg, .cpp_function = ""},

  // Bit Casting
  {.name = "$realtobits", .category = Cat::kBitCast, .min_args = 1, .max_args = 1, .return_type = Ret::kIntegral64, .cpp_function = ""},
  {.name = "$bitstoreal", .category = Cat::kBitCast, .min_args = 1, .max_args = 1, .return_type = Ret::kReal, .cpp_function = ""},
  {.name = "$shortrealtobits", .category = Cat::kBitCast, .min_args = 1, .max_args = 1, .return_type = Ret::kIntegral32, .cpp_function = ""},
  {.name = "$bitstoshortreal", .category = Cat::kBitCast, .min_args = 1, .max_args = 1, .return_type = Ret::kReal, .cpp_function = ""},

  // Math: Unary Real -> Real (Basic)
  {.name = "$ln", .category = Cat::kMathUnary, .min_args = 1, .max_args = 1, .return_type = Ret::kReal, .cpp_function = "std::log"},
  {.name = "$log10", .category = Cat::kMathUnary, .min_args = 1, .max_args = 1, .return_type = Ret::kReal, .cpp_function = "std::log10"},
  {.name = "$exp", .category = Cat::kMathUnary, .min_args = 1, .max_args = 1, .return_type = Ret::kReal, .cpp_function = "std::exp"},
  {.name = "$sqrt", .category = Cat::kMathUnary, .min_args = 1, .max_args = 1, .return_type = Ret::kReal, .cpp_function = "std::sqrt"},
  {.name = "$floor", .category = Cat::kMathUnary, .min_args = 1, .max_args = 1, .return_type = Ret::kReal, .cpp_function = "std::floor"},
  {.name = "$ceil", .category = Cat::kMathUnary, .min_args = 1, .max_args = 1, .return_type = Ret::kReal, .cpp_function = "std::ceil"},

  // Math: Unary Real -> Real (Trigonometric)
  {.name = "$sin", .category = Cat::kMathUnary, .min_args = 1, .max_args = 1, .return_type = Ret::kReal, .cpp_function = "std::sin"},
  {.name = "$cos", .category = Cat::kMathUnary, .min_args = 1, .max_args = 1, .return_type = Ret::kReal, .cpp_function = "std::cos"},
  {.name = "$tan", .category = Cat::kMathUnary, .min_args = 1, .max_args = 1, .return_type = Ret::kReal, .cpp_function = "std::tan"},
  {.name = "$asin", .category = Cat::kMathUnary, .min_args = 1, .max_args = 1, .return_type = Ret::kReal, .cpp_function = "std::asin"},
  {.name = "$acos", .category = Cat::kMathUnary, .min_args = 1, .max_args = 1, .return_type = Ret::kReal, .cpp_function = "std::acos"},
  {.name = "$atan", .category = Cat::kMathUnary, .min_args = 1, .max_args = 1, .return_type = Ret::kReal, .cpp_function = "std::atan"},

  // Math: Unary Real -> Real (Hyperbolic)
  {.name = "$sinh", .category = Cat::kMathUnary, .min_args = 1, .max_args = 1, .return_type = Ret::kReal, .cpp_function = "std::sinh"},
  {.name = "$cosh", .category = Cat::kMathUnary, .min_args = 1, .max_args = 1, .return_type = Ret::kReal, .cpp_function = "std::cosh"},
  {.name = "$tanh", .category = Cat::kMathUnary, .min_args = 1, .max_args = 1, .return_type = Ret::kReal, .cpp_function = "std::tanh"},
  {.name = "$asinh", .category = Cat::kMathUnary, .min_args = 1, .max_args = 1, .return_type = Ret::kReal, .cpp_function = "std::asinh"},
  {.name = "$acosh", .category = Cat::kMathUnary, .min_args = 1, .max_args = 1, .return_type = Ret::kReal, .cpp_function = "std::acosh"},
  {.name = "$atanh", .category = Cat::kMathUnary, .min_args = 1, .max_args = 1, .return_type = Ret::kReal, .cpp_function = "std::atanh"},

  // Math: Binary Real -> Real
  {.name = "$pow", .category = Cat::kMathBinary, .min_args = 2, .max_args = 2, .return_type = Ret::kReal, .cpp_function = "std::pow"},
  {.name = "$atan2", .category = Cat::kMathBinary, .min_args = 2, .max_args = 2, .return_type = Ret::kReal, .cpp_function = "std::atan2"},
  {.name = "$hypot", .category = Cat::kMathBinary, .min_args = 2, .max_args = 2, .return_type = Ret::kReal, .cpp_function = "std::hypot"},

  // Math: Integral
  {.name = "$clog2", .category = Cat::kMathIntegral, .min_args = 1, .max_args = 1, .return_type = Ret::kIntegral32, .cpp_function = ""},
});
// clang-format on
// NOLINTEND(readability-identifier-naming)

/// Find system function info by name. Returns nullptr if not found.
constexpr auto FindSystemFunction(std::string_view name)
    -> const SystemFunctionInfo* {
  for (const auto& info : kSystemFunctions) {
    if (info.name == name) {
      return &info;
    }
  }
  return nullptr;
}

/// Check if a system function is supported
constexpr auto IsSystemFunctionSupported(std::string_view name) -> bool {
  return FindSystemFunction(name) != nullptr;
}

/// Check if it's a function (returns value) vs task (void)
constexpr auto IsSystemFunction(std::string_view name) -> bool {
  const auto* info = FindSystemFunction(name);
  return info != nullptr &&
         info->return_type != SystemFunctionReturnType::kVoid;
}

/// Check if function has direct C++ mapping (for codegen)
constexpr auto HasDirectCppMapping(std::string_view name) -> bool {
  const auto* info = FindSystemFunction(name);
  return info != nullptr && !info->cpp_function.empty();
}

}  // namespace lyra::common
