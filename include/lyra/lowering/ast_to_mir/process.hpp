#pragma once

#include <cstddef>
#include <memory>

#include "lyra/mir/process.hpp"

namespace lyra::common {
class TypeArena;
}

namespace slang::ast {
class ProceduralBlockSymbol;
}

namespace lyra::lowering::ast_to_mir {

// Counters for generating unique process names within a module.
// Passed by reference so each module maintains independent numbering.
struct ProcessCounters {
  std::size_t initial = 0;
  std::size_t always = 0;
  std::size_t always_comb = 0;
  std::size_t always_latch = 0;
  std::size_t always_ff = 0;
};

// Lowers a slang AST ProceduralBlockSymbol into a MIR Process.
auto LowerProcess(
    const slang::ast::ProceduralBlockSymbol& procedural_block,
    ProcessCounters& counters, common::TypeArena& arena)
    -> std::unique_ptr<mir::Process>;

}  // namespace lyra::lowering::ast_to_mir
