#pragma once

#include <cstddef>
#include <cstdint>
#include <deque>
#include <map>
#include <memory>
#include <optional>

#include "lyra/base/function_ref.hpp"
#include "lyra/runtime/intrusive_list.hpp"
#include "lyra/runtime/part_path.hpp"
#include "lyra/runtime/trigger.hpp"
#include "lyra/value/integral_words.hpp"

namespace lyra::runtime {

class Observable;

// What a part of a place held before the event's execution under way -- a
// process up to its stop, one deferred update, one timer firing (LRM 4.5) --
// first wrote it, kept for the waits that are triggered by what an event's
// execution leaves rather than by each write it makes (LRM 9.2.2.2, 9.2.2.3).
// The part is the one a write landed on, so what is kept costs what the write
// touched; an array whose size changes is kept whole. Only the typed storage
// that wrote it knows how to copy and compare what it holds, so each kind of
// storage keeps its own.
class KeptOriginal {
 public:
  // Defined in the library: each kind of storage a unit holds builds one, and
  // a definition written here would be compiled again by each such unit.
  KeptOriginal();
  KeptOriginal(const KeptOriginal&) = delete;
  auto operator=(const KeptOriginal&) -> KeptOriginal& = delete;
  KeptOriginal(KeptOriginal&&) = delete;
  auto operator=(KeptOriginal&&) -> KeptOriginal& = delete;
  virtual ~KeptOriginal();

  // What the execution did to the part kept, measured from what it held before
  // the execution against what it holds now: the bits that moved where it is a
  // packed value, the whole of it otherwise, and none where it is as it was.
  [[nodiscard]] virtual auto Changed() -> std::optional<Change> = 0;

  // Puts what was kept back into the part, and keeps what the part held
  // instead. Done twice it changes nothing; between the two, the part holds
  // what it held before the execution, so a part enclosing it can be kept as
  // it was.
  virtual void Exchange() = 0;
};

// What bits of an integral value held before the event's execution under way
// first wrote them, where a write reaches bits rather than the whole value (LRM
// 11.5.1): the words of each plane they lie in, as one run of words that grows
// as a later write of the execution reaches words outside it. Every write of
// the execution keeps the words it reaches first, so a word outside the run was
// not written in it and holds what it held before: growing the run takes it as
// it stands. What is kept costs the words from the first written to the last,
// however wide the value, and grows at either end by what it takes.
class KeptBits {
 public:
  explicit KeptBits(value::IntegralView live);

  // Keeps what the words `bits`, which lie inside the value, held before the
  // write that reaches them changes them.
  void Keep(value::BitPositions bits);

  // What the execution did to the bits kept, measured from what they held
  // before it: none where they are as they were.
  [[nodiscard]] auto Changed() -> std::optional<Change>;

  // Puts what was kept back into the value, and keeps what it held instead,
  // as `KeptOriginal::Exchange` does of a part.
  void Exchange();

 private:
  void Take(std::size_t from, std::size_t to, bool before);

  value::IntegralView live_;
  // The first word kept, and the words kept from it on, of each plane.
  std::size_t first_ = 0;
  std::deque<std::uint64_t> value_;
  std::deque<std::uint64_t> unknown_;
};

// Copies what a part holds, as only the write landing on the part knows how: a
// way across the boundary of the library's own source for a copy a unit's
// write makes of a part of its type.
using MakeOriginal = base::FunctionRef<std::unique_ptr<KeptOriginal>()>;

// The same lent from `make`, which copies the part, for as long as it lives.
template <class Make>
[[nodiscard]] auto OriginalMadeBy(Make& make) -> MakeOriginal {
  return {&make, [](void* maker) -> std::unique_ptr<KeptOriginal> {
            return (*static_cast<Make*>(maker))();
          }};
}

// Orders paths by their steps, so that a path comes before every path it is a
// prefix of, and those come right after it.
struct ByStep {
  [[nodiscard]] auto operator()(const PartPath& a, const PartPath& b) const
      -> bool;
};

// What the event's execution under way kept of a place's parts, by the path of
// each, no part lying in another.
using KeptParts = std::map<PartPath, std::unique_ptr<KeptOriginal>, ByStep>;

// A place an `always_comb` or `always_latch` watches, made the first time such
// a wait enrols on it and kept for the place's life: what the event's execution
// under way keeps of its parts, linked into the run's list of places to judge
// exactly while something is kept.
struct CombinationalWatch : IntrusiveListNode<CombinationalWatch> {
  // Null on the sentinel a list embeds to close its ring.
  Observable* place = nullptr;
  KeptParts kept;
  // What was kept of the bits of integral values, by the path of each value,
  // none lying in a part `kept` holds.
  std::map<PartPath, KeptBits, ByStep> bits;
};

}  // namespace lyra::runtime
