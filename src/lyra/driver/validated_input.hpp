#pragma once

#include "frontend.hpp"
#include "lyra/common/diagnostic/diagnostic.hpp"

namespace lyra::driver {

// A CompilationInput whose fields have been validated and canonicalized.
// The dpi_link_inputs paths are absolute, deduplicated, and verified to
// exist as regular files. Backend code should only consume this type,
// never raw CompilationInput.
struct ValidatedCompilationInput {
  CompilationInput input;
};

// Validate a raw CompilationInput into its canonical validated form.
// Canonicalizes DPI link input paths (absolute, exists, regular file,
// deduplicated). Over time this becomes the natural home for additional
// input validation.
auto ValidateCompilationInput(CompilationInput raw)
    -> lyra::Result<ValidatedCompilationInput>;

}  // namespace lyra::driver
