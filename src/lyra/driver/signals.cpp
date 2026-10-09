#include "lyra/driver/signals.hpp"

#include <array>
#include <csignal>
#include <filesystem>
#include <functional>
#include <memory>
#include <mutex>
#include <optional>
#include <pthread.h>
#include <sys/types.h>
#include <system_error>
#include <thread>
#include <unistd.h>
#include <vector>

#include "lyra/status/status.hpp"

namespace lyra::driver {

namespace {

constexpr std::array<int, 3> kRequestsToEnd = {SIGINT, SIGTERM, SIGHUP};

struct Held {
  std::mutex lock;
  std::vector<std::filesystem::path> removed_on_signal;
  std::vector<pid_t> children;
};

// Never destroyed: the thread answering a request outlives every static the
// process tears down as it exits.
auto TheHeld() -> Held& {
  static Held& held = *std::make_unique<Held>().release();
  return held;
}

auto RequestsToEnd() -> sigset_t {
  sigset_t requests;
  ::sigemptyset(&requests);
  for (const int signal : kRequestsToEnd) {
    ::sigaddset(&requests, signal);
  }
  return requests;
}

// Waits for a request to end and answers it. The lock is taken and never given
// back, so nothing is added to what is held while it is being given up, and a
// thread that would have unwound part of it waits here until the process is
// gone.
[[noreturn]] void AnswerARequestToEnd(sigset_t requests) {
  int signal = 0;
  while (::sigwait(&requests, &signal) != 0) {
  }
  Held& held = TheHeld();
  held.lock.lock();
  for (const pid_t child : held.children) {
    ::kill(child, signal);
  }
  for (const std::filesystem::path& path : held.removed_on_signal) {
    std::error_code ignored;
    std::filesystem::remove_all(path, ignored);
  }
  status::Clear();
  // Ends the process the way the signal would have ended one that did not
  // answer it, which is what its parent reads the exit status for.
  ::signal(signal, SIG_DFL);
  ::pthread_sigmask(SIG_UNBLOCK, &requests, nullptr);
  ::raise(signal);
  ::_exit(128 + signal);
}

}  // namespace

void EndOnSignal() {
  const sigset_t requests = RequestsToEnd();
  ::pthread_sigmask(SIG_BLOCK, &requests, nullptr);
  std::thread(AnswerARequestToEnd, requests).detach();
}

void RemoveOnSignal(const std::filesystem::path& path) {
  Held& held = TheHeld();
  const std::scoped_lock lock(held.lock);
  held.removed_on_signal.push_back(path);
}

void DontRemoveOnSignal(const std::filesystem::path& path) {
  Held& held = TheHeld();
  const std::scoped_lock lock(held.lock);
  std::erase(held.removed_on_signal, path);
}

auto StartChild(const std::function<std::optional<pid_t>()>& start)
    -> std::optional<pid_t> {
  Held& held = TheHeld();
  const std::scoped_lock lock(held.lock);
  const std::optional<pid_t> child = start();
  if (child) {
    held.children.push_back(*child);
  }
  return child;
}

void ChildReaped(pid_t child) {
  Held& held = TheHeld();
  const std::scoped_lock lock(held.lock);
  std::erase(held.children, child);
}

}  // namespace lyra::driver
