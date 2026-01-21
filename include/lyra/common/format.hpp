#pragma once

#include <cstdint>

namespace lyra {

// Format kind for display/write operations.
// Determines how a value should be formatted for output.
enum class FormatKind : int32_t {
  kLiteral = -1,  // Literal string (uses LyraPrintLiteral, not LyraPrintValue)
  kDecimal = 0,   // %d
  kHex = 1,       // %h
  kBinary = 2,    // %b
  kOctal = 3,     // %o
  kString = 4,    // %s
};

// Print kind for display/write operations.
// Determines whether a newline is appended after output.
enum class PrintKind : int32_t {
  kDisplay = 0,  // $display - appends newline
  kWrite = 1,    // $write - no newline
};

}  // namespace lyra
