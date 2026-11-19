#pragma once

#include <array>
#include <cstddef>
#include <cstdint>
#include <memory>
#include <optional>
#include <span>
#include <utility>
#include <vector>

#include "lyra/base/function_ref.hpp"
#include "lyra/runtime/effect_storage.hpp"
#include "lyra/runtime/intrusive_list.hpp"
#include "lyra/runtime/part_path.hpp"
#include "lyra/runtime/trigger.hpp"
#include "lyra/value/integral_words.hpp"

namespace lyra::runtime {

class Observable;

// What a part of a place held before the open interval first
// modified it. An interval is a span of one execution of an `always_comb` or
// `always_latch` built to coalesce its updates, cut wherever a per-interval
// observer begins or stops listening, and a procedure of the same
// kind is told the net change the interval leaves on what it reads rather than
// each write (LRM 9.2.2.2, 9.2.2.3); the original is what that change is
// measured from when the interval ends. The part is the one a modification
// landed on, so what is kept costs what the write touched; an array whose size
// changes is kept whole. Only the typed storage that wrote it knows how to copy
// and compare what it holds, so each kind of storage keeps its own.
class IntervalOriginal {
 public:
  // Defined in the library: each kind of storage a unit holds builds one, and
  // a definition written here would be compiled again by each such unit.
  IntervalOriginal();
  IntervalOriginal(const IntervalOriginal&) = delete;
  auto operator=(const IntervalOriginal&) -> IntervalOriginal& = delete;
  IntervalOriginal(IntervalOriginal&&) = delete;
  auto operator=(IntervalOriginal&&) -> IntervalOriginal& = delete;
  virtual ~IntervalOriginal();

  // The net change of the part over the interval, measured from the original
  // against what it holds now: the bits that moved where it is a packed value,
  // the whole of it otherwise, and none where it is as it was.
  [[nodiscard]] virtual auto Changed() -> std::optional<Change> = 0;

  // Puts the original back into the part, and keeps what the part held
  // instead. Done twice it changes nothing; between the two, the part holds
  // its original, so a part enclosing it can be kept as it was.
  virtual void Exchange() = 0;
};

// Ends an original and gives its bytes back to the effect storage they came
// from. An interval keeps originals only for the parts it modifies and ends
// them all when it ends, so the bytes are taken again by the next interval
// instead of asked of the system allocator each time.
struct ReleaseToEffectStorage {
  EffectStorage* storage = nullptr;
  std::size_t bytes = 0;
  std::size_t alignment = 0;

  void operator()(IntervalOriginal* original) const noexcept {
    std::destroy_at(original);
    storage->Release(original, bytes, alignment);
  }
};

using OwnedOriginal = std::unique_ptr<IntervalOriginal, ReleaseToEffectStorage>;

// An original of the kind `Kind`, built from `args` in bytes `storage` hands
// out. A copy that raises gives nothing back, as an effect made in the same
// storage does not; the execution it raised in ends.
template <class Kind, class... Args>
[[nodiscard]] auto NewOriginal(EffectStorage& storage, Args&&... args)
    -> OwnedOriginal {
  void* const start = storage.Allocate(sizeof(Kind), alignof(Kind));
  return {
      std::construct_at(static_cast<Kind*>(start), std::forward<Args>(args)...),
      ReleaseToEffectStorage{
          .storage = &storage,
          .bytes = sizeof(Kind),
          .alignment = alignof(Kind)}};
}

// What bits of an integral value held before the open interval first modified
// them, where a write reaches bits rather than the whole value (LRM 11.5.1):
// the words of each plane they lie in, as one run of words that grows as a
// later modification in the interval reaches words outside it. Every
// modification keeps the words it reaches first, so a word outside the run of
// words was not modified in the interval and holds its original: growing takes
// it as it stands.
//
// What is kept costs the words from the first modified to the last, however
// wide the value, and nearly every write reaches one word, which is kept with
// no storage of its own.
class BitsOriginal {
 public:
  explicit BitsOriginal(value::IntegralView live);

  // Keeps what the words `bits`, which lie inside the value, held before the
  // modification that reaches them changes them.
  void Keep(value::BitPositions bits);

  // The net change of the bits kept over the interval: none where they are as
  // they were.
  [[nodiscard]] auto Changed() -> std::optional<Change>;

  // Puts the original words back into the value, and keeps what it held
  // instead, as `IntervalOriginal::Exchange` does of a part.
  void Exchange();

 private:
  // The words of one plane from `first_`, in a room of their own up to its
  // size and in storage of their own past that.
  class Words {
   public:
    [[nodiscard]] auto Size() const -> std::size_t {
      return size_;
    }
    [[nodiscard]] auto Span() -> std::span<std::uint64_t> {
      return size_ <= kRoom ? std::span(inline_).first(size_)
                            : std::span(spilled_);
    }
    // Puts `words` in front of those kept, or after them.
    void Insert(std::span<const std::uint64_t> words, bool before);

   private:
    // One word for a write of bits, two where they straddle a word boundary.
    static constexpr std::size_t kRoom = 2;
    std::array<std::uint64_t, kRoom> inline_{};
    std::vector<std::uint64_t> spilled_;
    std::size_t size_ = 0;
  };

  void Take(std::size_t from, std::size_t to, bool before);

  value::IntegralView live_;
  // The first word kept, and the words kept from it on, of each plane.
  std::size_t first_ = 0;
  Words value_;
  Words unknown_;
};

// Copies what a part holds into the effect storage it is handed, as only the
// write modifying the part knows how: a way across the boundary of the
// library's own source for a copy a unit's write makes of a part of its type.
using CopyOriginal = base::FunctionRef<OwnedOriginal(EffectStorage&)>;

// The same lent from `copy`, which copies the part, for as long as it lives.
template <class Copy>
[[nodiscard]] auto CopyOriginalBy(Copy& copy) -> CopyOriginal {
  return {&copy, [](void* copier, EffectStorage& storage) -> OwnedOriginal {
            return (*static_cast<Copy*>(copier))(storage);
          }};
}

// Orders paths by their steps, so that a path comes before every path it is a
// prefix of, and those come right after it.
struct ByStep {
  [[nodiscard]] auto operator()(const PartPath& a, const PartPath& b) const
      -> bool;
};

// A part the open interval kept the original of, by its path.
struct PartOriginalAt {
  PartPath at;
  OwnedOriginal original;
};

// The bits of an integral value the open interval kept the original of, by the
// value's path.
struct BitsOriginalAt {
  PartPath at;
  BitsOriginal bits;
};

// What a place keeps for the open interval: the original of each
// part the interval modified, made the first time a per-interval observer
// enrols on the place and kept for the place's life. It is linked into the
// interval's list exactly while it keeps something. Each list is ordered by its
// paths, a path before every path inside it, holds no part lying in another,
// and keeps its room from one interval to the next, so an interval modifying
// the place asks for no storage once one has.
struct IntervalAccumulator : IntrusiveListNode<IntervalAccumulator> {
  // Null on the sentinel a list embeds to close its ring.
  Observable* place = nullptr;
  std::vector<PartOriginalAt> parts;
  // The originals of bits of integral values, none lying in a part
  // `parts` holds.
  std::vector<BitsOriginalAt> bits;

  [[nodiscard]] auto Empty() const -> bool {
    return parts.empty() && bits.empty();
  }
};

}  // namespace lyra::runtime
