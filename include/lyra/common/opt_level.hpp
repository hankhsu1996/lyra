#pragma once

namespace lyra {

// Optimization level for compilation.
// Controls both the pre-codegen IR optimization pipeline (OptimizeModule)
// and the backend CodeGen optimization level.
enum class OptLevel { kO0, kO1, kO2, kO3 };

}  // namespace lyra
