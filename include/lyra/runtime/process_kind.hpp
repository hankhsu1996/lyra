#pragma once

namespace lyra::runtime {

enum class ProcessKind {
  kInitial,
  kFinal,
  // A process brought into being during simulation (a fork-join branch),
  // adopted into the lineage of the process that spawned it rather than
  // registered on a scope at startup.
  kSpawned,
  // An execution brought into being during simulation to carry out a deferred
  // effect the standard makes no process of: an update whose event has not
  // happened yet (LRM 9.4.5, 15.5.1). It belongs to no lineage, so `wait fork`
  // does not wait for it and `disable fork` does not reach it (LRM 9.6.1,
  // 9.6.3).
  kDetached,
};

}  // namespace lyra::runtime
