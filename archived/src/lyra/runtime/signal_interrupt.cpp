#include "lyra/runtime/signal_interrupt.hpp"

#include <csignal>

namespace lyra::runtime {

namespace {

struct SavedInterruptHandlers {
  struct sigaction sigint_old{};
  struct sigaction sigterm_old{};
  bool installed = false;
};

// NOLINTNEXTLINE(cppcoreguidelines-avoid-non-const-global-variables)
volatile sig_atomic_t g_interrupt_requested = 0;

// NOLINTNEXTLINE(cppcoreguidelines-avoid-non-const-global-variables)
SavedInterruptHandlers g_saved{};

void OnSimulationInterruptSignal(int /*signo*/) {
  g_interrupt_requested = 1;
}

}  // namespace

void InstallSimulationInterruptHandlers() {
  g_interrupt_requested = 0;

  struct sigaction sa{};
  sa.sa_handler = OnSimulationInterruptSignal;
  sa.sa_flags = SA_RESTART;
  sigemptyset(&sa.sa_mask);

  sigaction(SIGINT, &sa, &g_saved.sigint_old);
  sigaction(SIGTERM, &sa, &g_saved.sigterm_old);
  g_saved.installed = true;
}

void RemoveSimulationInterruptHandlers() {
  if (g_saved.installed) {
    sigaction(SIGINT, &g_saved.sigint_old, nullptr);
    sigaction(SIGTERM, &g_saved.sigterm_old, nullptr);
    g_saved.installed = false;
  }
  g_interrupt_requested = 0;
}

auto ConsumeSimulationInterruptRequested() -> bool {
  if (g_interrupt_requested == 0) {
    return false;
  }
  g_interrupt_requested = 0;
  return true;
}

}  // namespace lyra::runtime
