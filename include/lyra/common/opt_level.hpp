#pragma once

namespace lyra {

// Optimization level for JIT compilation.
// Currently affects LLVM codegen optimization only.
// IR optimization passes are not yet threaded through this enum.
enum class OptLevel { kO0, kO1, kO2, kO3 };

}  // namespace lyra
