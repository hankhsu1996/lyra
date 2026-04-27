#pragma once

#include <cstdint>

#include "lyra/runtime/format.hpp"

namespace lyra::runtime {

class Engine;

void LyraPrintStart(Engine& engine, PrintKind print_kind);

void LyraPrintLiteral(Engine& engine, const char* data, std::uint32_t size);

// Compound payloads passed by pointer-to-const for LLVM-friendliness.
void LyraPrintValue(
    Engine& engine, const FormatSpec* spec, const RuntimeValueView* value);

void LyraPrintEnd(Engine& engine, PrintKind print_kind);

}  // namespace lyra::runtime
