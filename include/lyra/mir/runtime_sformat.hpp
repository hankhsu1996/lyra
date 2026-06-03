#pragma once

#include <utility>
#include <vector>

#include "lyra/mir/runtime_print.hpp"

namespace lyra::mir {

// LRM 21.3.3 string-format family ($swrite / $sformat / $sformatf). The
// item sequence is built by the same `BuildRuntimePrintItemsFromCallArgs`
// walker the print family uses, so the conversion-spec set and the
// auto-format vs literal-format branching are identical to $display /
// $write. The backend emits a call to `runtime::LyraSFormat(items)` which
// returns a `value::String`; the enclosing expression context places the
// result (assigned to an output_var for $swrite / $sformat, or used
// directly as an rvalue for $sformatf).
struct RuntimeSFormatCall {
  std::vector<RuntimePrintItem> items;

  explicit RuntimeSFormatCall(std::vector<RuntimePrintItem> i)
      : items(std::move(i)) {
  }
};

}  // namespace lyra::mir
