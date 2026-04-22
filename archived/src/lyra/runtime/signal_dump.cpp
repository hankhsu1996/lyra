#include "lyra/runtime/signal_dump.hpp"

#include <atomic>
#include <csignal>
#include <unistd.h>

#include "lyra/runtime/engine.hpp"

namespace lyra::runtime {

namespace {

// NOLINTNEXTLINE(cppcoreguidelines-avoid-non-const-global-variables)
std::atomic<Engine*> g_engine{nullptr};

void HandleSigusr1(int /*sig*/) {
  Engine* e = g_engine.load(std::memory_order_relaxed);
  if (e == nullptr) return;
  e->DumpSchedulerStatusAsyncSignalSafe(STDERR_FILENO);
}

}  // namespace

void InstallSignalDumpHandler(Engine* engine) {
  g_engine.store(engine, std::memory_order_relaxed);

  struct sigaction sa{};
  sa.sa_handler = HandleSigusr1;
  sa.sa_flags = SA_RESTART;
  sigemptyset(&sa.sa_mask);
  sigaction(SIGUSR1, &sa, nullptr);
}

void RemoveSignalDumpHandler() {
  struct sigaction sa{};
  sa.sa_handler = SIG_DFL;
  sigemptyset(&sa.sa_mask);
  sigaction(SIGUSR1, &sa, nullptr);
  g_engine.store(nullptr, std::memory_order_relaxed);
}

}  // namespace lyra::runtime
