#pragma once

// System task/function opcode registry for the "long tail" of simple system
// TFs.
//
// CONTRACT: This file contains ONLY:
//   - Opcode identity (enum values) - stable, append-only, never renumber
//   - Metadata (arity, family, role) - single source of truth
//   - Lookup functions
//
// NO lowering policy, NO type rules, NO backend details.
// Those belong in lowering layers.
//
// DESIGN:
//   - Specialized MIR kinds (DisplayEffect, SeverityEffect, etc.) remain for
//     system TFs with rich payload requirements.
//   - SystemTfRvalueInfo/SystemTfEffect cover the simple cases (fopen, fclose,
//     etc.) where payload is just opcode + operands.

#include <cstdint>
#include <optional>
#include <string_view>

namespace lyra {

// Stable opcode enum. Append-only, never renumber existing values.
// Used by MIR, interpreter, and LLVM backend.
enum class SystemTfOpcode : uint16_t {
  // File I/O
  kFopen = 0,
  kFclose = 1,
  kFflush = 2,
  kFgetc = 3,
  kUngetc = 4,
  kFgets = 8,
  kFread = 9,
  // Query
  kValuePlusargs = 5,
  // Random
  kRandom = 6,
  kUrandom = 7,
};

// Functional category for grouping related system TFs.
enum class SystemTfFamily : uint8_t {
  kFileIO,
  kRandom,
  kDump,
  kMonitor,
  kQuery,
};

// Execution role: does this TF return a value, produce effects, or both?
enum class SystemTfRole : uint8_t {
  kPure,    // No side effects, returns value
  kEffect,  // Side effects only, no return value
  kMixed,   // Side effects + returns value (e.g., $fopen)
};

// Return convention for type-dependent lowering.
enum class ResultConvention : uint8_t {
  kNone,          // No return value
  kIntegral,      // int/longint based on context
  kReal,          // real/shortreal
  kStringHandle,  // Runtime string handle
  kFdHandle,      // File descriptor (int32)
};

// Metadata for a system TF. Used for validation and dispatch.
struct SystemTfMetadata {
  SystemTfOpcode opcode;
  std::string_view name;  // "$fopen", "$fclose", etc.
  SystemTfFamily family;
  SystemTfRole role;
  ResultConvention result_conv;
  int8_t min_args;
  int8_t max_args;       // -1 for variadic
  int8_t out_arg_index;  // -1 if none, else index of by-ref output param
};

// Lookup by opcode (O(1) array index). Used by MIR/backend.
auto GetSystemTfMetadata(SystemTfOpcode op) -> const SystemTfMetadata&;

// Lookup by name (binary search). Used only in AST->HIR lowering.
auto LookupSystemTfOpcode(std::string_view name)
    -> std::optional<SystemTfOpcode>;

// String representation for dumping.
auto ToString(SystemTfOpcode op) -> const char*;

}  // namespace lyra
