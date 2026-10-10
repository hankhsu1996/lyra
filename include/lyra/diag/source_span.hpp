#pragma once

#include <cstdint>

namespace lyra::diag {

// A piece of the source, as the front end named it: where it starts and where
// it ends, each a value of the front end's own. Whoever holds one hands it
// back; only what stands for the front end reads one, which is what lets a
// piece of text that came out of a macro still be told apart from the macro's
// use when it is shown. Zero at both ends is no place.
struct SourceSpan {
  std::uint64_t start = 0;
  std::uint64_t end = 0;

  auto operator==(const SourceSpan&) const -> bool = default;
};

}  // namespace lyra::diag
