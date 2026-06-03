#pragma once

#include <initializer_list>
#include <variant>

#include "lyra/value/packed_array.hpp"
#include "lyra/value/string.hpp"

namespace lyra::runtime {

class RuntimeServices;

// LRM 21.3.4.3 scanner output slot. Each conversion in the format string
// writes through one slot; the slot's variant alternative decides which
// per-spec parser path the runtime takes. The Make factory picks the right
// alternative from the temp's C++ type, so emitted call sites stay
// uniform: `ScanSlot::Make(temp_int)` / `ScanSlot::Make(temp_str)`.
class ScanSlot {
 public:
  static auto Make(value::PackedArray& dest) -> ScanSlot {
    return ScanSlot{&dest};
  }
  static auto Make(value::String& dest) -> ScanSlot {
    return ScanSlot{&dest};
  }

  // Typed accessors for the scanner core. Returns nullptr if the slot does
  // not carry that alternative; the per-spec parser then raises a
  // type-mismatch runtime_error (lowering should have prevented this, so
  // the mismatch path is just defensive).
  [[nodiscard]] auto AsIntegral() const -> value::PackedArray* {
    if (const auto* p = std::get_if<value::PackedArray*>(&dest_)) {
      return *p;
    }
    return nullptr;
  }
  [[nodiscard]] auto AsString() const -> value::String* {
    if (const auto* p = std::get_if<value::String*>(&dest_)) {
      return *p;
    }
    return nullptr;
  }

 private:
  explicit ScanSlot(value::PackedArray* p) : dest_(p) {
  }
  explicit ScanSlot(value::String* p) : dest_(p) {
  }

  std::variant<value::PackedArray*, value::String*> dest_;
};

// LRM 21.3.4.3 $sscanf entry. Parses `input` against `format` and writes
// matched conversions into `slots`. Returns the number of items
// successfully matched and assigned, or -1 on EOF before any conversion
// (matching LRM "code can be 0 ... If the input ends before the first
// matching failure or conversion, EOF is returned"). The result is
// wrapped in a 32-bit PackedArray so emit sites can hand it straight to
// `WriteVar` for an `int` LHS, parallel to the rest of the file IO
// runtime surface. `slots` is taken by initializer_list so the emitted
// call site can build it inline without a named temp; each ScanSlot
// holds a pointer to the caller's lvalue, so const access through the
// list still mutates the user's variable.
auto LyraSScanf(
    const value::String& input, const value::String& format,
    std::initializer_list<ScanSlot> slots) -> value::PackedArray;

// LRM 21.3.4.3 $fscanf entry. Resolves `fd` to the underlying fstream via
// RuntimeServices::Files(), runs the same scanner core as LyraSScanf over
// a FileScanSource, and pushes the offending-character pushback back into
// the fstream's putback buffer before returning so the next $fgetc sees
// it. Invalid / closed / MCD descriptors stamp EBADF on the FD's $ferror
// slot and return -1 (LRM "If the input ends before the first matching
// failure or conversion, EOF (-1) is returned"). The result is wrapped in
// a 32-bit PackedArray to parallel LyraSScanf.
auto LyraFScanf(
    RuntimeServices& services, const value::PackedArray& fd,
    const value::String& format, std::initializer_list<ScanSlot> slots)
    -> value::PackedArray;

}  // namespace lyra::runtime
