#pragma once

#include <chrono>
#include <expected>
#include <filesystem>
#include <optional>
#include <span>
#include <string>
#include <string_view>
#include <variant>
#include <vector>

namespace conformance::framework {

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

// One command that was run, as someone running it again needs it: what was run,
// where, how it ended, and both streams.
struct Step {
  std::filesystem::path program;
  std::vector<std::string> arguments;
  std::filesystem::path directory;
  ProcessOutcome outcome;
};

// The code the child exited with, where it exited at all.
auto ExitCodeOf(const ProcessOutcome& outcome) -> std::optional<int>;

// How the child ended, in words: "exited with 3", "ended by signal 9".
auto Describe(const ProcessEnd& end) -> std::string;

// A command and everything it did: what was run, how it ended, and both
// streams, as a reader who was not there needs them.
auto Transcript(const Step& step) -> std::string;

// The program of that name as the shell that started this would find it, or
// nothing where no directory on PATH holds one.
auto FindOnPath(std::string_view name) -> std::optional<std::filesystem::path>;

// A path written on this program's command line, as its caller meant it.
// `bazel run` starts a program inside its runfiles and says where it was
// started from, so a relative path is taken from there rather than from where
// the program runs.
auto ResolveCallerPath(std::string_view written) -> std::filesystem::path;

// How long a child started now may run to end by `deadline`: nothing where
// there is no deadline, and none at all where it has passed.
auto TimeLeft(std::optional<std::chrono::steady_clock::time_point> deadline)
    -> std::optional<std::chrono::seconds>;

// How long a program may take to answer a question about itself -- its
// version, where it is installed -- which it does at once. It guards against
// one that never answers, so a run does not wait on it before any case.
inline constexpr std::chrono::seconds kQuestionAnsweredWithin{30};

// The first line `program --version` prints, which is where a command-line
// tool says what release it is, or nothing where it says nothing, fails, or
// does not answer in time.
auto VersionOf(const std::filesystem::path& program)
    -> std::optional<std::string>;

// A directory of the caller's own to run in. The name is drawn rather than
// derived from anything, because a run that repeats must not inherit what an
// earlier one left. Returns why it failed rather than throwing, so a setup
// failure is reported as the run failing rather than as a crash.
auto MakeScratchDir() -> std::expected<std::filesystem::path, std::string>;

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

}  // namespace conformance::framework
