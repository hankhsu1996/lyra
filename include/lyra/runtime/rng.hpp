#pragma once

#include <cstdint>
#include <random>

namespace lyra::runtime {

// The value one generator hands to another. LRM 18.14.1 seeds a newly created
// process from the process that created it, and a static process from the unit
// instance whose declaration it belongs to, so a seed travels between
// generators; the design never observes one directly.
struct RandomSeed {
  std::uint32_t value;
};

// LRM 18.14.1: the source of seeds for one unit instance's static processes and
// static initializers. The language can neither seed it nor read its state, so
// the only thing observable about it is the seeds it hands out, in order. Every
// instance's runs from the same default seed, which is what keeps two instances
// of one module drawing alike and keeps a change elsewhere in the design from
// moving what either of them sees.
class InitializationRng {
 public:
  [[nodiscard]] auto NextSeed() -> RandomSeed {
    return RandomSeed{static_cast<std::uint32_t>(engine_())};
  }

 private:
  // Seeds are consumed only as the starting state of another generator, so the
  // sequence needs to be reproducible and nothing more.
  std::minstd_rand engine_{1U};
};

// LRM 18.14.2: the generator every randomization system call made from one
// process draws from. No two processes share one, which is what makes the
// values a process observes independent of the order in which processes run.
class ProcessRng {
 public:
  explicit ProcessRng(RandomSeed seed) : engine_(seed.value) {
  }

  // The width is the standard's rather than this generator's: `$urandom`
  // returns 32 unsigned bits whatever engine produced them (LRM 18.13.1).
  [[nodiscard]] auto NextValue() -> std::uint32_t {
    return static_cast<std::uint32_t>(engine_());
  }

  // The hierarchical-seeding step: a process created from this one starts from
  // this generator's next value.
  [[nodiscard]] auto NextSeed() -> RandomSeed {
    return RandomSeed{NextValue()};
  }

  void Reseed(RandomSeed seed) {
    engine_.seed(seed.value);
  }

 private:
  std::mt19937 engine_;
};

}  // namespace lyra::runtime
