#pragma once

#include <string>

#include "lyra/xir/fwd.hpp"

namespace lyra::xir {

// First slice: only kInitial. When always/always_comb are introduced,
// this enum will grow to represent source-facing process identity
// before execution-model lifecycle decisions are made.
enum class ProcessEntryKind {
  kInitial,
};

struct ProcessEntry {
  std::string name;
  ProcessEntryKind entry_kind;
  StmtId body;
};

}  // namespace lyra::xir
