#pragma once

#include <cstddef>
#include <cstdint>
#include <format>
#include <utility>
#include <vector>

#include "lyra/base/id_range.hpp"
#include "lyra/base/internal_error.hpp"

namespace lyra::base {

// What one pass made for each entity of another pool: keyed by that pool's own
// id, one answer per entity. `Id` is the source pool's id type, a struct
// carrying a single `std::uint32_t value`; `T` is the answer. Key first, like
// any other mapping -- unlike a pool, which leads with what it holds because
// the id is what it hands back.
//
// This mints no identity. The id space belongs to the source pool, so an answer
// is never asked where it goes: it goes to the entity it was built for, and a
// translation is sized by the source's own count. That is what makes it total,
// and totality is what lets a read be a plain index rather than a search that
// can come back empty.
//
// Both ways of building it hold that. Handing over the finished answers states
// the count at the same moment, so a skipped or doubled entity is caught there.
// Opening it and giving one answer at a time serves the fold whose answer for
// one entity reads the answers before it; the count still bounds it, and an
// entity left unanswered surfaces at the read.
//
// `base::Arena` is the counterpart for a pool that does mint, handing out an id
// per append. A container indexed by another pool's ids is not an arena however
// it is spelled: two pools claiming to mint one id type are two authorities for
// one identity, and they drift.
template <typename Id, typename T>
class Translation {
 public:
  Translation() = default;

  // Builds the translation of a pool holding `source_count` entities. A
  // disagreement between that count and the answers built is a caller that
  // skipped an entity or answered one twice. The two numbers reach here from
  // opposite ends -- the source pool's own size, against what the caller
  // produced walking it -- so their agreement is a real fact and not a
  // restatement.
  Translation(std::size_t source_count, std::vector<T> answers)
      : answers_(std::move(answers)), source_count_(source_count) {
    if (answers_.size() != source_count) {
      throw InternalError(
          std::format(
              "Translation: {} answers were built for a pool of {} entities",
              answers_.size(), source_count));
    }
  }

  // Opens a translation of a pool holding `source_count` entities, to be given
  // its answers one at a time.
  explicit Translation(std::size_t source_count) : source_count_(source_count) {
    answers_.reserve(source_count);
  }

  // Gives the answer for the next entity of the source pool. Nothing names that
  // entity, because being next is what identifies it -- which is also why
  // nothing has to check that the answer landed where it belongs.
  void Append(T answer) {
    if (answers_.size() >= source_count_) {
      throw InternalError(
          std::format(
              "Translation::Append: a {}th answer was built for a pool of {} "
              "entities",
              answers_.size() + 1, source_count_));
    }
    answers_.push_back(std::move(answer));
  }

  // The source pool's identities, for a consumer that walks what this answers
  // for. Reading them from here is what keeps a walk from rebuilding an id out
  // of its own loop counter.
  [[nodiscard]] auto Ids() const -> IdRange<Id> {
    return IdRange<Id>{static_cast<std::uint32_t>(source_count_)};
  }

  [[nodiscard]] auto Get(Id id) const -> const T& {
    if (id.value >= answers_.size()) {
      throw InternalError(
          std::format(
              "Translation::Get: id {} has no answer; {} of the pool's {} "
              "entities have been answered for",
              id.value, answers_.size(), source_count_));
    }
    return answers_[id.value];
  }

  [[nodiscard]] auto size() const -> std::size_t {
    return answers_.size();
  }

  [[nodiscard]] auto empty() const -> bool {
    return answers_.empty();
  }

  [[nodiscard]] auto begin() const {
    return answers_.begin();
  }

  [[nodiscard]] auto end() const {
    return answers_.end();
  }

 private:
  std::vector<T> answers_;
  std::size_t source_count_ = 0;
};

}  // namespace lyra::base
