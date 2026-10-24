#pragma once

#include <bit>
#include <cstdint>
#include <optional>
#include <string>
#include <string_view>

namespace lyra::runtime {

// A seed the design states: the argument of `$urandom(seed)` (LRM 18.13.1) or
// of `srandom` (LRM 18.13.3), both of which the standard types as `int`.
struct RandomSeed {
  std::uint32_t value;
};

// The generator a randomization system call draws from. A process has one (LRM
// 18.14.2), and so does the static initialization of a container, which is not
// a process (LRM 18.14.1). No two share one, which is what makes the values
// either of them observes independent of the order the rest run in.
//
// The standard fixes nothing about the algorithm, and what it does fix decides
// one: every thread and every class object has a generator, each started from a
// value drawn out of the one that created it (LRM 18.14.1), so there are as
// many of them as a testbench has objects and each is made by another. That is
// the splittable generator, and this is the L64X128 member of the LXM family
// (Steele and Vigna, "LXM: better splittable pseudorandom number generators",
// 2021): a linear congruential state and a xoroshiro128 state stepped side by
// side, their sum mixed into each output, with the congruential addend chosen
// per generator so that two generators walk different cycles and not two
// stretches of one.
class DrawRng {
 public:
  // The generator a stated seed names. The same seed names the same generator
  // every time, which is what makes it replay (LRM 18.13.1). The seed is
  // spread over the whole state by mixers unlike the generator's own step, so
  // that neighbouring seeds name unrelated generators.
  explicit DrawRng(RandomSeed seed)
      : DrawRng(
            MixMurmur(Spread(seed)) | 1U, 1, MixStafford(Spread(seed)),
            MixStafford(Spread(seed) + kGoldenRatio)) {
  }

  // The width is the standard's rather than this generator's: `$urandom`
  // returns 32 unsigned bits whatever engine produced them (LRM 18.13.1). They
  // are the high half of a step.
  [[nodiscard]] auto NextValue() -> std::uint32_t {
    return static_cast<std::uint32_t>(Step() >> 32U);
  }

  // The hierarchical-seeding step (LRM 18.14.1): the generator of something
  // this one's owner creates. It takes its whole state from here, four steps
  // of it, so that as many distinct generators can be made as the state has
  // values and not as a 32-bit seed has.
  [[nodiscard]] auto Split() -> DrawRng {
    const std::uint64_t addend = (Step() << 1U) | 1U;
    const std::uint64_t congruential = Step();
    const std::uint64_t xor_low = Step();
    const std::uint64_t xor_high = Step();
    return DrawRng{addend, congruential, xor_low, xor_high};
  }

  void Reseed(RandomSeed seed) {
    *this = DrawRng{seed};
  }

  // What `get_randstate` answers (LRM 18.13.4), a text whose length and format
  // the standard leaves to the implementation: the four words of the state in
  // decimal, separated by single spaces.
  [[nodiscard]] auto StateText() const -> std::string;

  // The generator a state text names, for `set_randstate` (LRM 18.13.5). Only
  // a text that writes a state this generator can be in names one; the
  // standard leaves any other undefined, and here it names none.
  [[nodiscard]] static auto FromStateText(std::string_view text)
      -> std::optional<DrawRng>;

 private:
  static constexpr std::uint64_t kGoldenRatio = 0x9e3779b97f4a7c15ULL;
  static constexpr std::uint64_t kSilverRatio = 0x6a09e667f3bcc909ULL;
  static constexpr std::uint64_t kMultiplier = 0xd1342543de82ef95ULL;

  static auto Spread(RandomSeed seed) -> std::uint64_t {
    return std::uint64_t{seed.value} ^ kSilverRatio;
  }

  DrawRng(
      std::uint64_t addend, std::uint64_t congruential, std::uint64_t xor_low,
      std::uint64_t xor_high)
      : addend_(addend),
        congruential_(congruential),
        xor_low_(xor_low),
        xor_high_(xor_high) {
    // The xoroshiro half never leaves the all-zero state.
    if ((xor_low_ | xor_high_) == 0) {
      xor_low_ = MixStafford(congruential_ + kGoldenRatio);
      xor_high_ = MixStafford(congruential_ + (2 * kGoldenRatio));
    }
  }

  static auto MixMurmur(std::uint64_t value) -> std::uint64_t {
    value = (value ^ (value >> 33U)) * 0xff51afd7ed558ccdULL;
    value = (value ^ (value >> 33U)) * 0xc4ceb9fe1a85ec53ULL;
    return value ^ (value >> 33U);
  }

  static auto MixStafford(std::uint64_t value) -> std::uint64_t {
    value = (value ^ (value >> 30U)) * 0xbf58476d1ce4e5b9ULL;
    value = (value ^ (value >> 27U)) * 0x94d049bb133111ebULL;
    return value ^ (value >> 31U);
  }

  static auto MixLea(std::uint64_t value) -> std::uint64_t {
    value = (value ^ (value >> 32U)) * 0xdaba0b6eb09322e3ULL;
    value = (value ^ (value >> 32U)) * 0xdaba0b6eb09322e3ULL;
    return value ^ (value >> 32U);
  }

  auto Step() -> std::uint64_t {
    const std::uint64_t result = MixLea(congruential_ + xor_low_);
    congruential_ = (kMultiplier * congruential_) + addend_;
    std::uint64_t low = xor_low_;
    std::uint64_t high = xor_high_;
    high ^= low;
    low = std::rotl(low, 24) ^ high ^ (high << 16U);
    high = std::rotl(high, 37);
    xor_low_ = low;
    xor_high_ = high;
    return result;
  }

  std::uint64_t addend_;
  std::uint64_t congruential_;
  std::uint64_t xor_low_;
  std::uint64_t xor_high_;
};

// LRM 18.14.1: what the static processes and static initializers of one module,
// interface, or program instance, or of one package, are started from. The
// language can neither seed it nor read its state, so the only thing observable
// about it is the generators it hands out, in order. Every container's runs
// from the same default seed, which is what keeps two instances of one module
// drawing alike and keeps a change elsewhere in the design from moving what
// either of them sees.
class InitializationRng {
 public:
  [[nodiscard]] auto Split() -> DrawRng {
    return source_.Split();
  }

 private:
  DrawRng source_{RandomSeed{1}};
};

// A uniform draw in [0, `span`) from `rng`, for a span of 1 to 2^32.
[[nodiscard]] auto DrawBelow(DrawRng& rng, std::uint64_t span) -> std::uint64_t;

}  // namespace lyra::runtime
