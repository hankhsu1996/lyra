#pragma once

#include <cstdint>
#include <random>

namespace lyra::runtime {

// The value one generator hands to another. LRM 18.14.1 seeds a newly created
// process from the process that created it, and both a static process and a
// static initialization from the initialization RNG of the container their
// declarations belong to, so a seed travels between generators; the design
// never observes one directly.
struct RandomSeed {
  std::uint32_t value;
};

// LRM 18.14.1: the source of seeds for the static processes and static
// initializers of one module, interface, or program instance, or of one
// package. The language can neither seed it nor read its state, so the only
// thing observable about it is the seeds it hands out, in order. Every
// container's runs from the same default seed, which is what keeps two
// instances of one module drawing alike and keeps a change elsewhere in the
// design from moving what either of them sees.
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

// The generator a randomization system call draws from. A process has one (LRM
// 18.14.2), and so does the static initialization of a container, which is not
// a process (LRM 18.14.1). No two share one, which is what makes the values
// either of them observes independent of the order the rest run in.
class DrawRng {
 public:
  explicit DrawRng(RandomSeed seed) : engine_(seed.value) {
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

// The generator of a static initialization (LRM 18.14.1), which owns no object
// to keep one on. It carries the generator it displaced, so leaving restores
// whatever was drawing before.
struct DisplacingRng {
  DrawRng rng;
  // Null where nothing was drawing before this one started.
  DrawRng* displaced = nullptr;
};

}  // namespace lyra::runtime
