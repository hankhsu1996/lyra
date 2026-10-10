#pragma once

#include <concepts>
#include <cstddef>
#include <cstdint>
#include <iterator>
#include <map>
#include <utility>

#include "lyra/value/element_policy.hpp"
#include "lyra/value/formation.hpp"

namespace lyra::value {

// What an associative array asks of its index type: how a key is held, what a
// lookup is handed, the order the array keeps its keys in (LRM 7.8.2, 7.8.4)
// -- which compares a held key with a handed one -- whether a handed key names
// no entry at all, as one carrying x or z does (LRM 7.8.6), and the key an
// insertion holds of one. Like an element, a key is answered by its C++ type
// where the array is compiled with it, and by its type's table in the library.
template <typename P>
concept KeyPolicy =
    std::copyable<P> && requires(const P& p, const typename P::Probe& key) {
      typename P::Less;
      { p.Order() } -> std::same_as<typename P::Less>;
      { p.Invalid(key) } -> std::same_as<bool>;
      { p.Owned(key) } -> std::same_as<typename P::Stored>;
    };

// An associative array (LRM 7.8), written once over what its index and element
// answer: compiled with their C++ types for the C++ backend, and once in the
// library, with their types' tables, for the execution backend. Every element
// is handed in and out by its address.
//
// Each element is storage of its own that never moves while its key is in the
// array, and the array maps each key, in its index order, to that storage, so
// inserting or removing another key moves no element. What a read of an absent
// or invalid key answers with (LRM 7.8.6) is the array's own persistent
// value, which a `default:` clause names (LRM 7.9.11) and which is otherwise
// the element type's default; it is also what an entry a write allocates starts
// as (LRM 7.8.7).
template <KeyPolicy Key, ElementPolicy Elem>
class BasicAssociativeArray {
 public:
  using Stored = typename Key::Stored;
  using Probe = typename Key::Probe;
  using Map = std::map<Stored, void*, typename Key::Less>;

  BasicAssociativeArray()
    requires std::default_initializable<Key> && std::default_initializable<Elem>
  = default;
  // An empty array whose absent keys read `miss`, or the element default
  // where there is none.
  BasicAssociativeArray(Key key, Elem elem, const void* miss)
      : key_(std::move(key)),
        elem_(std::move(elem)),
        map_(key_.Order()),
        miss_(CopyElement(elem_, miss != nullptr ? miss : elem_.Default())) {
  }

  BasicAssociativeArray(const BasicAssociativeArray& other)
      : BasicAssociativeArray(other.key_, other.elem_, other.Miss()) {
    for (const auto& [stored, slot] : other.map_) {
      map_.emplace_hint(map_.end(), stored, CopyElement(elem_, slot));
    }
  }
  BasicAssociativeArray(BasicAssociativeArray&& other) noexcept
      : key_(std::move(other.key_)),
        elem_(std::move(other.elem_)),
        map_(std::exchange(other.map_, Map(key_.Order()))),
        miss_(std::exchange(other.miss_, nullptr)),
        discard_(std::exchange(other.discard_, nullptr)) {
  }
  auto operator=(const BasicAssociativeArray& other) -> BasicAssociativeArray& {
    if (this != &other) {
      BasicAssociativeArray copy(other);
      Swap(copy);
    }
    return *this;
  }
  auto operator=(BasicAssociativeArray&& other) noexcept
      -> BasicAssociativeArray& {
    if (this != &other) {
      BasicAssociativeArray taken(std::move(other));
      Swap(taken);
    }
    return *this;
  }
  ~BasicAssociativeArray() {
    Clear();
    for (void* slot : {miss_, discard_}) {
      if (slot != nullptr) {
        FreeElement(elem_, slot);
      }
    }
  }

  [[nodiscard]] auto KeyType() const -> const Key& {
    return key_;
  }
  [[nodiscard]] auto Element() const -> const Elem& {
    return elem_;
  }
  [[nodiscard]] auto Entries() const -> const Map& {
    return map_;
  }
  [[nodiscard]] auto Count() const -> std::size_t {
    return map_.size();
  }

  // The array's own value for an absent key (LRM 7.8.6, 7.9.11), the element
  // default where the array was never built.
  [[nodiscard]] auto Miss() const -> const void* {
    return miss_ != nullptr ? miss_ : elem_.Default();
  }

  // LRM 7.9.3.
  [[nodiscard]] auto Exists(const Probe& key) const -> bool {
    return !key_.Invalid(key) && map_.contains(key);
  }

  // LRM 7.8.6: the element `key` names, or the array's own value where it
  // names none, without allocating.
  [[nodiscard]] auto ElementAt(const Probe& key) const -> const void* {
    if (key_.Invalid(key)) {
      return Miss();
    }
    const auto found = map_.find(key);
    return found == map_.end() ? Miss() : found->second;
  }

  // LRM 7.8.7: the element `key` names, allocated holding the array's own
  // value where it names none. An invalid key (LRM 7.8.6) lands where no read
  // reaches, so the write is discarded.
  [[nodiscard]] auto ElementRef(const Probe& key, Formation& formed) -> void* {
    if (key_.Invalid(key)) {
      formed = Formation::kNowhere;
      return DiscardTarget(elem_, discard_);
    }
    const auto found = map_.lower_bound(key);
    if (found != map_.end() && !map_.key_comp()(key, found->first)) {
      formed = Formation::kExisting;
      return found->second;
    }
    void* slot = CopyElement(elem_, Miss());
    map_.emplace_hint(found, key_.Owned(key), slot);
    formed = Formation::kMade;
    return slot;
  }

  // The element `key` names set to a copy of `value`, as a literal's entry
  // states it (LRM 7.9.11).
  void Store(const Probe& key, const void* value) {
    Formation formed{};
    elem_.Assign(ElementRef(key, formed), value);
  }

  // LRM 7.9.2: empties the array, or removes the entry `key` names; an absent
  // or invalid key is no entry to remove.
  void Clear() {
    for (const auto& [stored, slot] : map_) {
      FreeElement(elem_, slot);
    }
    map_.clear();
  }
  void Erase(const Probe& key) {
    if (key_.Invalid(key)) {
      return;
    }
    const auto found = map_.find(key);
    if (found == map_.end()) {
      return;
    }
    FreeElement(elem_, found->second);
    map_.erase(found);
  }

  // LRM 7.9.4 -- 7.9.7: the least and greatest keys, the least after `key`
  // and the greatest before it, each none where there is no such key.
  [[nodiscard]] auto FirstKey() const -> const Stored* {
    return map_.empty() ? nullptr : &map_.begin()->first;
  }
  [[nodiscard]] auto LastKey() const -> const Stored* {
    return map_.empty() ? nullptr : &map_.rbegin()->first;
  }
  [[nodiscard]] auto KeyAfter(const Probe& key) const -> const Stored* {
    const auto found = map_.upper_bound(key);
    return found == map_.end() ? nullptr : &found->first;
  }
  [[nodiscard]] auto KeyBefore(const Probe& key) const -> const Stored* {
    const auto found = map_.lower_bound(key);
    return found == map_.begin() ? nullptr : &std::prev(found)->first;
  }

  // LRM 11.2.2 aggregate equality / 11.4.5: the same key set, each paired
  // element comparing equal by `==` (which propagates X / Z), answering what
  // the element policy's comparison does.
  [[nodiscard]] auto Equal(const BasicAssociativeArray& other) const
      -> FourStateBit {
    if (!SameKeys(other)) {
      return FourStateBit::kZero;
    }
    FourStateBit result = FourStateBit::kOne;
    for (const auto& [key, slot] : map_) {
      result =
          LogicalAnd(result, elem_.Equal(slot, other.map_.find(key)->second));
    }
    return result;
  }
  // LRM 11.4.5 `===`, which is never unknown.
  [[nodiscard]] auto CaseEqual(const BasicAssociativeArray& other) const {
    if (!SameKeys(other)) {
      return elem_.CaseAnswer(false);
    }
    for (const auto& [key, slot] : map_) {
      if (!elem_.CaseEqual(slot, other.map_.find(key)->second)) {
        return elem_.CaseAnswer(false);
      }
    }
    return elem_.CaseAnswer(true);
  }

  // LRM 9.4.2 update event predicate: the array's own value for an absent
  // key, the key set and each paired element all match. That value is part of
  // the array, so changing it alone is an observable change.
  [[nodiscard]] auto IsBitIdentical(const BasicAssociativeArray& other) const
      -> bool {
    if (!elem_.BitIdentical(Miss(), other.Miss()) ||
        map_.size() != other.map_.size()) {
      return false;
    }
    for (const auto& [key, slot] : map_) {
      const auto paired = other.map_.find(key);
      if (paired == other.map_.end() ||
          !elem_.BitIdentical(slot, paired->second)) {
        return false;
      }
    }
    return true;
  }

  // LRM 20.9: an element carrying an unknown bit propagates up; a key cannot
  // carry one, since a write under such a key is discarded (LRM 7.8.6).
  [[nodiscard]] auto HasUnknown() const -> bool {
    for (const auto& [key, slot] : map_) {
      if (elem_.HasUnknown(slot)) {
        return true;
      }
    }
    return false;
  }

  // LRM 20.6.2 `$bits` / LRM 20.9 `$countbits`: the entries' elements laid end
  // to end, a key being no part of the stream.
  [[nodiscard]] auto BitstreamWidth() const -> std::int64_t {
    std::int64_t total = 0;
    for (const auto& [key, slot] : map_) {
      total += elem_.BitstreamWidth(slot);
    }
    return total;
  }
  template <typename Control>
  [[nodiscard]] auto CountBits(const Control& control_bits) const
      -> std::int64_t {
    std::int64_t total = 0;
    for (const auto& [key, slot] : map_) {
      total += elem_.CountBits(slot, control_bits);
    }
    return total;
  }

 private:
  // Whether two arrays hold one key set, which is what pairs their elements;
  // matching empties pair.
  [[nodiscard]] auto SameKeys(const BasicAssociativeArray& other) const
      -> bool {
    if (map_.size() != other.map_.size()) {
      return false;
    }
    for (const auto& [key, slot] : map_) {
      if (!other.map_.contains(key)) {
        return false;
      }
    }
    return true;
  }

  void Swap(BasicAssociativeArray& other) noexcept {
    std::swap(key_, other.key_);
    std::swap(elem_, other.elem_);
    std::swap(map_, other.map_);
    std::swap(miss_, other.miss_);
    std::swap(discard_, other.discard_);
  }

  Key key_;
  Elem elem_;
  Map map_;
  void* miss_ = nullptr;
  void* discard_ = nullptr;
};

}  // namespace lyra::value
