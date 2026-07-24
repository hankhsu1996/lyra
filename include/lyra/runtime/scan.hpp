#pragma once

#include <cstdint>
#include <initializer_list>
#include <variant>

#include "lyra/value/packed_array.hpp"
#include "lyra/value/string.hpp"

namespace lyra::runtime {

class RuntimeEffects;

// LRM 21.3.4.3 per-slot write target. The runtime parses one value of the
// caller-declared shape and writes it through `dest`; lvalue semantics
// (observable Var<T>, partial selectors, etc.) live entirely at the
// callsite -- the runtime only sees a plain pointer.
struct IntegralScanTarget {
  value::PackedArray* dest;
  std::uint32_t bit_width;
  bool is_signed;
  bool is_four_state;
};

struct StringScanTarget {
  value::String* dest;
};

using ScanTarget = std::variant<IntegralScanTarget, StringScanTarget>;

// LRM 21.3.4.3 `$sscanf`. Returns matched-conversion count, or -1 on EOF
// before any conversion. String scanning is self-contained -- no engine
// runtime access is needed.
auto LyraSScanf(
    const value::String& input, const value::String& format,
    std::initializer_list<ScanTarget> targets) -> value::PackedArray;

// LRM 21.3.4.3 `$fscanf`. Resolves `fd` via `RuntimeEffects::Files()`,
// parks the offending-character pushback in the slot's putback so the
// next read on the same FD sees it. Invalid / closed / MCD descriptors
// stamp `EBADF` on `$ferror` and return -1.
auto LyraFScanf(
    RuntimeEffects& runtime, const value::PackedArray& fd,
    const value::String& format, std::initializer_list<ScanTarget> targets)
    -> value::PackedArray;

}  // namespace lyra::runtime
