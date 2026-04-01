#pragma once

// DPI-C header generation from the design-visible DPI symbol registry.
// Emits a stable C header with prototypes for all DPI exports and imports.

#include <string>

#include "lyra/lowering/hir_to_mir/dpi_registry.hpp"

namespace lyra::lowering::hir_to_mir {

// Render a complete DPI-C header from the design-visible symbol registries.
// Output is deterministically sorted by visible C name.
// Includes both export prototypes (callable from C) and import prototypes
// (implemented by C, declared for reference).
auto RenderDpiHeader(
    const DesignDpiExports& exports, const DesignDpiImports& imports)
    -> std::string;

}  // namespace lyra::lowering::hir_to_mir
