#pragma once

#include <cstdint>

namespace lyra::support {

// What the assertion control tasks select by type (LRM 20.11, Table 20-6),
// each the bit of `assertion_type` that selects it. The last three are the
// violation reports of a `unique`, `unique0` or `priority` statement (LRM
// 12.4.2, 12.5.3), which the tasks control alongside the assertions.
enum class AssertionType : std::uint8_t {
  kConcurrent = 1,
  kSimpleImmediate = 2,
  kObservedDeferredImmediate = 4,
  kFinalDeferredImmediate = 8,
  kExpect = 16,
  kUnique = 32,
  kUnique0 = 64,
  kPriority = 128,
};

// What an assertion states (LRM 16.2, Table 20-7), each the bit of
// `directive_type` that selects it. Only the four assertion types carry one.
// A task's mask is matched against the bit, and which directive it is decides
// nothing else, so it is not a dispatch set.
enum class AssertionDirectiveType : std::uint8_t {
  kAssert = 1,
  kCover = 2,
  kAssume = 4,
};

// The selections LRM 20.11 states where a call writes none: every type of
// Table 20-6 and every directive of Table 20-7 for `$assertcontrol`, the four
// assertion types for `$asserton`, `$assertoff` and `$assertkill`, and those
// and expect statements for the action tasks.
inline constexpr std::uint8_t kEveryAssertionType = 255;
inline constexpr std::uint8_t kEveryDirective = 7;
inline constexpr std::uint8_t kEveryAssertion = 15;
inline constexpr std::uint8_t kEveryAssertionAndExpect = 31;

// What a task does to what it selects (LRM 20.11, Table 20-5), each the value
// of `control_type` that asks for it.
enum class AssertionControlType : std::uint8_t {
  kLock = 1,
  kUnlock = 2,
  kOn = 3,
  kOff = 4,
  kKill = 5,
  kPassOn = 6,
  kPassOff = 7,
  kFailOn = 8,
  kFailOff = 9,
  kNonvacuousOn = 10,
  kVacuousOff = 11,
};

// What one execution of a controlled statement may do, read once where the
// execution begins and kept until it ends, since a change made while it runs
// does not reach it (LRM 20.11): whether it checks at all, and which of its
// actions an outcome may run. Each is a bit of the word the control answers
// with, so the code an execution compiles to tests the bit it needs.
inline constexpr std::uint64_t kAssertionChecks = 1;
inline constexpr std::uint64_t kAssertionPassesVacuously = 2;
inline constexpr std::uint64_t kAssertionPassesNonvacuously = 4;
inline constexpr std::uint64_t kAssertionFails = 8;

}  // namespace lyra::support
