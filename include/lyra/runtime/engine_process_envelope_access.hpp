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
      const Engine& engine, ProcessHandle handle, WaitSiteId wait_site_id)
      -> bool {
    return engine.CanRefreshInstalledWait(handle, wait_site_id);
  }

  static auto HasPendingDirtyState(const Engine& engine) -> bool {
    return engine.HasPendingDirtyState();
  }

  static void ResetInstalledWait(Engine& engine, ProcessHandle handle) {
    engine.ResetInstalledWait(handle);
  }

  static void InstallTriggers(
      Engine& engine, ProcessHandle handle, const WaitRequest& req) {
    engine.InstallTriggers(handle, req);
  }

  static void InstallWaitSite(
      Engine& engine, ProcessHandle handle, const WaitRequest& req) {
    engine.InstallWaitSite(handle, req);
  }

  static auto RefreshInstalledSnapshots(Engine& engine, ProcessHandle handle)
      -> bool {
    return engine.RefreshInstalledSnapshots(handle);
  }
};

}  // namespace lyra::runtime
