#pragma once

#include <chrono>
#include <filesystem>
#include <gmock/gmock.h>
#include <optional>
#include <ostream>
#include <span>
#include <string>
#include <string_view>
#include <variant>

namespace lyra::test {

// The child was never seen to end: it could not be started, or this process
// lost track of it. Neither says anything about the program it named.
struct CouldNotRun {
  std::string reason;
};

struct Exited {
  int code = 0;
};

struct Signalled {
  int signal = 0;
};

// The child was still running when the time its caller gave it was up, and
// was ended for it.
struct TimedOut {};

// The child was still running when the test running it was about to run out of
// time, and was ended so the test could report it. It says nothing of the
// child, only that the test had no time left to wait.
struct TestTimedOut {};

// How a child ended. An exit code exists only where the child exited, so a
// child a signal or the time limit ended can never read as one that succeeded.
// Nothing having been run comes first, so a value nobody filled in is not a
// success either.
using ProcessEnd =
    std::variant<CouldNotRun, Exited, Signalled, TimedOut, TestTimedOut>;

struct ProcessOutcome {
  ProcessEnd end;
  std::string stdout_text;
  std::string stderr_text;
};

// The code the child exited with, where it exited at all.
auto ExitCodeOf(const ProcessOutcome& outcome) -> std::optional<int>;

// How the child ended, in words: "exited with 3", "ended by signal 9".
auto Describe(const ProcessEnd& end) -> std::string;

// What an assertion shows of an outcome it rejected: how it ended. The streams
// are long, so a caller adds whichever one says why.
void PrintTo(const ProcessOutcome& outcome, std::ostream* out);

// The child exited, with a code `code` accepts; a child that did not exit
// matches no code.
auto ExitedWithCode(const testing::Matcher<int>& code)
    -> testing::Matcher<const ProcessOutcome&>;

// Spawns `exe` with `args` and collects both streams. It starts in
// `working_directory` where one is given, and where this process is otherwise.
//
// A child is out of time shortly before the test running it would be ended for
// its own time: the child is ended first, so the test reports which child it
// was and what it wrote rather than only that the test's time passed. A caller
// gives a `timeout` of its own only where a stop is what the test is about, or
// so that one stuck case does not hold up the others run beside it. Outside a
// test runner that states a limit, a child given no `timeout` has none.
//
// A child out of time is asked to end and killed if it has not shortly after,
// and is reported by whose time ran out whatever it does once asked, an exit
// of 0 included. What it writes when asked is kept as evidence: a program Lyra
// built says the simulation time its run had reached, which only the program
// knows.
auto RunChildProcess(
    const std::filesystem::path& exe, std::span<const std::string> args,
    std::optional<std::chrono::seconds> timeout = std::nullopt,
    const std::optional<std::filesystem::path>& working_directory =
        std::nullopt) -> ProcessOutcome;

// Runs `script` in a shell, which reads `positional` as its own `$0`, `$1` and
// on, so none of them is read as shell text. A machine with no shell answers
// that the script could not be run.
auto RunShellScript(
    std::string_view script, std::span<const std::string> positional = {},
    std::optional<std::chrono::seconds> timeout = std::nullopt)
    -> ProcessOutcome;

// Runs `program` with `args` in a shell that has first limited its own address
// space to `kibibytes`, so a program that asks for more is refused by its host.
auto RunUnderAddressLimit(
    const std::filesystem::path& program, std::span<const std::string> args,
    int kibibytes) -> ProcessOutcome;

}  // namespace lyra::test
