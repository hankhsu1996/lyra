#pragma once

#include <filesystem>
#include <functional>
#include <optional>
#include <sys/types.h>

namespace lyra::driver {

// What this process does when it is asked to end -- interrupted, terminated,
// or hung up on. It passes the request to every child it has running, removes
// every path it was told to, and then ends by that same signal, so whoever
// started it reads the ending it asked for. A process that returns or throws
// unwinds what it holds by itself; this is for the ending that unwinds nothing.
//
// Called once, before the process has a second thread: the request is taken by
// a thread that waits for it, and every other thread is started unable to
// receive one.
void EndOnSignal();

// From here on, `path` and everything under it goes when the process is asked
// to end. A path the process finished with, or removed itself, is taken back
// with the second call.
void RemoveOnSignal(const std::filesystem::path& path);
void DontRemoveOnSignal(const std::filesystem::path& path);

// Starts a child with `start`, which answers the child's process id or nothing
// where none started, and passes a request to end on to that child until it is
// reaped. Once the process is answering such a request no child starts, since
// nothing would pass the request on to it: the call waits, and the process ends
// under it.
auto StartChild(const std::function<std::optional<pid_t>()>& start)
    -> std::optional<pid_t>;

// The child has ended and been waited for, so its number means nothing now.
void ChildReaped(pid_t child);

}  // namespace lyra::driver
