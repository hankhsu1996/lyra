#pragma once

namespace lyra::runtime {

enum class ProcessKind {
  kInitial,
  kFinal,
  // A process brought into being during simulation (a fork-join branch), owned
  // by the engine's dynamic pool rather than registered at startup.
  kSpawned,
};

}  // namespace lyra::runtime
