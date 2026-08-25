#pragma once

#include <cstdint>

namespace lyra::base {

// The identities a pool has handed out, in order.
//
// A consumer that walks a pool needs the pool's ids, not positions in it. The
// difference matters because an id is conferred: the pool minted it, so the
// pool is what can hand it back. Rebuilding one from a loop counter reaches the
// same number by asserting how the pool numbers what it holds -- an assertion
// about someone else's decision, and one that has to be spelled with a cast
// because the counter's type is not the id's.
//
// Walking this instead means the raw index never exists, so there is nothing to
// convert and nothing to assert.
template <typename Id>
class IdRange {
 public:
  explicit IdRange(std::uint32_t count) : count_(count) {
  }

  class Iterator {
   public:
    explicit Iterator(std::uint32_t index) : index_(index) {
    }

    auto operator*() const -> Id {
      return Id{index_};
    }

    auto operator++() -> Iterator& {
      ++index_;
      return *this;
    }

    auto operator==(const Iterator& other) const -> bool {
      return index_ == other.index_;
    }

   private:
    std::uint32_t index_;
  };

  [[nodiscard]] auto begin() const -> Iterator {
    return Iterator{0};
  }

  [[nodiscard]] auto end() const -> Iterator {
    return Iterator{count_};
  }

 private:
  std::uint32_t count_;
};

}  // namespace lyra::base
