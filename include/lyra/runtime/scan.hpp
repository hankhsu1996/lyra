#pragma once

#include <initializer_list>
#include <variant>

#include "lyra/runtime/var.hpp"
#include "lyra/value/packed_array.hpp"
#include "lyra/value/string.hpp"

namespace lyra::runtime {

class RuntimeServices;

// LRM 21.3.4.3 scanner output slot. Integral lvalues ride `Ref<PackedArray>`
// so structural vars route writes through `Var::Set` (firing the LRM 4.3
// update event) while procedural locals fall through to direct assignment.
// String vars are not observable on this pipeline today, so the string
// alternative is a raw pointer with no services threading.
class ScanSlot {
 public:
  static auto Make(Var<value::PackedArray>& dest) -> ScanSlot {
    return ScanSlot{Ref<value::PackedArray>{dest}};
  }
  static auto Make(value::PackedArray& dest) -> ScanSlot {
    return ScanSlot{Ref<value::PackedArray>{dest}};
  }
  static auto Make(value::String& dest) -> ScanSlot {
    return ScanSlot{&dest};
  }

  [[nodiscard]] auto IsIntegral() const -> bool {
    return std::holds_alternative<Ref<value::PackedArray>>(dest_);
  }
  [[nodiscard]] auto IsString() const -> bool {
    return std::holds_alternative<value::String*>(dest_);
  }

  // Per-spec parsers snapshot the current value to read width / sign /
  // 4-state metadata, mutate locally, then commit through `SetIntegral`.
  [[nodiscard]] auto GetIntegral() const -> value::PackedArray {
    return std::get<Ref<value::PackedArray>>(dest_).Get();
  }

  void SetIntegral(
      RuntimeServices& services, const value::PackedArray& new_val) const {
    std::get<Ref<value::PackedArray>>(dest_).Set(services, new_val);
  }
  void SetString(const value::String& new_val) const {
    *std::get<value::String*>(dest_) = new_val;
  }

 private:
  explicit ScanSlot(Ref<value::PackedArray> r) : dest_(r) {
  }
  explicit ScanSlot(value::String* p) : dest_(p) {
  }

  std::variant<Ref<value::PackedArray>, value::String*> dest_;
};

// LRM 21.3.4.3 $sscanf. Returns matched-conversion count, or -1 on EOF
// before any conversion.
auto LyraSScanf(
    RuntimeServices& services, const value::String& input,
    const value::String& format, std::initializer_list<ScanSlot> slots)
    -> value::PackedArray;

// LRM 21.3.4.3 $fscanf. Resolves `fd` via `RuntimeServices::Files()`,
// parks the offending-character pushback in the slot's putback so the
// next read on the same FD sees it. Invalid / closed / MCD descriptors
// stamp `EBADF` on `$ferror` and return -1.
auto LyraFScanf(
    RuntimeServices& services, const value::PackedArray& fd,
    const value::String& format, std::initializer_list<ScanSlot> slots)
    -> value::PackedArray;

}  // namespace lyra::runtime
