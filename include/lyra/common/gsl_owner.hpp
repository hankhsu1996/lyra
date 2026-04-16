#pragma once

// Minimal gsl::owner<T> alias recognized by clang-tidy's
// cppcoreguidelines-owning-memory check. This avoids pulling in
// the full GSL library just for ownership annotations on extern "C"
// create/destroy pairs.

namespace gsl {
template <typename T>
using owner = T;
}  // namespace gsl
