#pragma once

#include <cstddef>
#include <vector>

#include "lyra/runtime/engine_subscriptions.hpp"

namespace lyra::runtime {

struct SuspendRecord;

// First-class runtime process object.
// Single owner of all per-process runtime state. Indexed by process_id
// in Engine::processes_.
struct RuntimeProcess {
  bool is_enqueued = false;
  SuspendRecord* suspend_record = nullptr;
  bool is_comb_kernel = false;

  size_t subscription_count = 0;
  std::vector<LocalSubRef> local_sub_refs;
  std::vector<GlobalSubRef> global_sub_refs;
  IndexPlanPool plan_pool;
  InstalledWaitState installed_wait;
};

}  // namespace lyra::runtime
