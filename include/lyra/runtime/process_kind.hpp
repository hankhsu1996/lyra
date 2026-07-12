#pragma once

namespace lyra::runtime {

enum class ProcessKind {
  kInitial,
  kFinal,
  // A process brought into being during simulation (a fork-join branch),
  // adopted into the lineage of the process that spawned it rather than
  // registered on a scope at startup.
  kSpawned,
};

}  // namespace lyra::runtime
