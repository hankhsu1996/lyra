#pragma once

#include "lyra/runtime/engine.hpp"

namespace lyra::runtime {

// Internal bridge for process envelope <-> engine collaboration.
// These operations are not part of the public engine API; they exist
// solely to support the envelope's wait lifecycle orchestration.
// Engine declares this class as a friend.
class ProcessEnvelopeAccess {
 public:
  static auto UsesWaitSiteLifecycle(const Engine& engine) -> bool {
    return engine.wait_site_meta_.IsPopulated();
  }

  static auto HasPostActivationReconciliation(const Engine& engine) -> bool {
    return engine.HasPostActivationReconciliation();
  }

  static auto CanRefreshInstalledWait(
      const RuntimeProcess& proc, WaitSiteId wait_site_id) -> bool {
    return Engine::CanRefreshInstalledWait(proc, wait_site_id);
  }

  static auto HasPendingDirtyState(const Engine& engine) -> bool {
    return engine.HasPendingDirtyState();
  }

  static void ResetInstalledWait(Engine& engine, RuntimeProcess& proc) {
    engine.ResetInstalledWait(proc);
  }

  static void InstallTriggers(
      Engine& engine, RuntimeProcess& proc, const WaitRequest& req) {
    engine.InstallTriggers(proc, req);
  }

  static void InstallWaitSite(
      Engine& engine, RuntimeProcess& proc, const WaitRequest& req) {
    engine.InstallWaitSite(proc, req);
  }

  static auto RefreshInstalledSnapshots(Engine& engine, RuntimeProcess& proc)
      -> bool {
    return engine.RefreshInstalledSnapshots(proc);
  }
};

}  // namespace lyra::runtime
