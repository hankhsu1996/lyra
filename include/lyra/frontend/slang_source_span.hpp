#pragma once

#include <cstdint>
#include <string_view>

#include <slang/text/SourceLocation.h>

#include "lyra/diag/source_span.hpp"

namespace lyra::frontend {

// How one of slang's locations is held in a span and taken back out of it. A
// location is a buffer and an offset into it; the two fit one word, and no
// buffer is numbered zero, so zero is no location.
inline constexpr unsigned kOffsetBits = 36;

inline auto PackedLocation(slang::SourceLocation location) -> std::uint64_t {
  if (!location.valid()) {
    return 0;
  }
  return (std::uint64_t{location.buffer().getId()} << kOffsetBits) |
         std::uint64_t{location.offset()};
}

inline auto UnpackedLocation(std::uint64_t packed) -> slang::SourceLocation {
  if (packed == 0) {
    return {};
  }
  return {
      slang::BufferID(
          static_cast<std::uint32_t>(packed >> kOffsetBits),
          std::string_view{}),
      packed & ((std::uint64_t{1} << kOffsetBits) - 1)};
}

// The place slang gave a piece of the source, as slang gave it.
inline auto SpanOf(slang::SourceRange range) -> diag::SourceSpan {
  return {
      .start = PackedLocation(range.start()),
      .end = PackedLocation(range.end())};
}

inline auto PointSpanOf(slang::SourceLocation location) -> diag::SourceSpan {
  return SpanOf(slang::SourceRange{location, location});
}

}  // namespace lyra::frontend
