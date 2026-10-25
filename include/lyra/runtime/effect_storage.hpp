#pragma once

#include <array>
#include <cstddef>
#include <vector>

namespace lyra::runtime {

// Where every deferred effect of one run gets its bytes, whichever region and
// time slot holds it: a free list for each size class, refilled a block at a
// time, as SystemC's kernel keeps its timed events. An effect's bytes return to
// their class when it ends, so what is held follows how many effects are
// pending at once and how large each is, never how many times they are due at.
// Blocks stay until the run ends, so what is held once a burst is over is what
// the burst needed. One thread uses it, the one running the scheduler.
class EffectStorage {
 public:
  EffectStorage() = default;
  EffectStorage(const EffectStorage&) = delete;
  auto operator=(const EffectStorage&) -> EffectStorage& = delete;
  EffectStorage(EffectStorage&&) = delete;
  auto operator=(EffectStorage&&) -> EffectStorage& = delete;
  ~EffectStorage();

  // `size` bytes at a multiple of `alignment`, which is a power of two.
  [[nodiscard]] auto Allocate(std::size_t size, std::size_t alignment) -> void*;

  // Gives back what `Allocate(size, alignment)` handed out at `start`.
  void Release(void* start, std::size_t size, std::size_t alignment) noexcept;

 private:
  // The step the classes are sized in, which every class's bytes are also
  // aligned to, and the largest class; anything larger or more strictly
  // aligned comes from the system allocator and goes back to it.
  static constexpr std::size_t kGranule = 16;
  static constexpr std::size_t kLargestClassBytes = 1024;
  // How many bytes a refill asks for, so a class of small effects refills
  // rarely and a class of large ones holds few it never uses.
  static constexpr std::size_t kBlockBytes = 4096;

  struct FreeNode {
    FreeNode* next;
  };

  static constexpr std::size_t kClassCount = kLargestClassBytes / kGranule;
  static constexpr std::size_t kNoClass = kClassCount;

  // The class serving `size` bytes at `alignment`, or `kNoClass`.
  [[nodiscard]] static auto ClassOf(std::size_t size, std::size_t alignment)
      -> std::size_t;

  void Refill(std::size_t size_class);

  std::array<FreeNode*, kClassCount> free_{};
  std::vector<void*> blocks_;
};

}  // namespace lyra::runtime
