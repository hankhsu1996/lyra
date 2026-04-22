#pragma once

// DPI visible-name collision checking.
// Internal helper for design declaration collection.

#include "lyra/lowering/hir_to_mir/context.hpp"

namespace lyra::lowering::hir_to_mir {

// Check for DPI visible-name collisions across the full foreign-visible
// C namespace. Appends diagnostics to decls.export_diagnostics for any
// import/export or import/import c_name collisions with different symbols.
// Export/export collisions are already checked by DesignDpiExports::Insert.
void CheckDpiVisibleNameCollisions(DesignDeclarations& decls);

}  // namespace lyra::lowering::hir_to_mir
