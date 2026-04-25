#pragma once

namespace lyra::runtime {

enum class ProcessKind {
  kInitial,
  kAlways,
  kAlwaysComb,
  kAlwaysFf,
  kFinal,
};

}  // namespace lyra::runtime
