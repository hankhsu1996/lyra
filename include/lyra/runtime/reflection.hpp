#pragma once

#include <cstdint>
#include <utility>
#include <variant>
#include <vector>

#include "lyra/runtime/trigger.hpp"
#include "lyra/value/any_value.hpp"
#include "lyra/value/bit_vec.hpp"
#include "lyra/value/integral_words.hpp"
#include "lyra/value/library_value_types.hpp"
#include "lyra/value/object_ref.hpp"
#include "lyra/value/real.hpp"
#include "lyra/value/string.hpp"
#include "lyra/value/value_type.hpp"

namespace lyra::abi {
struct ScopeDeclarations;
}  // namespace lyra::abi

namespace lyra::runtime {

class GcObject;

// The key an entry of an associative array is kept by (LRM 7.8): a value of
// the array's index type, with that type. Two keys are one where they are of
// one type and the same value.
struct SelectedKey {
  value::AnyValue key;

  [[nodiscard]] auto operator==(const SelectedKey& other) const -> bool;
};

// Which part of a data object a reader reaching the design by name names (LRM
// 37.17 to 37.19, 37.26): the steps from the object to it, in order, each a
// member's index among its structure's or union's members, an element's index
// as the source would write it -- in the range a fixed-size array was declared
// with, from the front of a dynamic array or a queue (LRM 38.19), or into an
// associative array with a wildcard index (LRM 7.8.1) -- or an entry of an
// associative array by its key: the key the array keeps it by, or, until the
// code settling the selection converts it into that key, what foreign code
// named it with -- a number, as a run of bits read signed, or a text. No
// selection names no step and stands for the whole object. The code reaching a
// part takes one step at each depth, mapping an index to its element as the
// design's own select does, and stops at the first value it holds that is no
// aggregate, or where the selection ends; which bits of a packed value a reader
// names is the reader's, never that code's.
class Selection {
 public:
  using Step = std::variant<std::int64_t, SelectedKey>;

  Selection() = default;
  explicit Selection(std::vector<Step> steps);

  // How many steps the selection takes.
  [[nodiscard]] auto Depth() const -> std::int64_t;
  // The step at `depth` as a number: a member's index, an element's index, or
  // an index of an associative array whose index is a wildcard (LRM 7.8.1).
  [[nodiscard]] auto Number(std::int64_t depth) const -> std::int64_t;
  // The step at `depth` as the key of an entry of an associative array whose
  // index type is `type`: the key itself where it is of that type, and
  // otherwise the number or the text it names converted into that type as the
  // design converts an index into it (LRM 7.8.4).
  [[nodiscard]] auto KeyAt(std::int64_t depth, const value::ValueType& type)
      const -> value::AnyValue;
  template <value::LyraValue K>
  auto Key(std::int64_t depth) const -> K {
    const value::AnyValue key = KeyAt(depth, value::LibraryTypeOf<K>());
    return *static_cast<const K*>(key.Bytes());
  }

  [[nodiscard]] auto Steps() const -> const std::vector<Step>& {
    return steps_;
  }

  [[nodiscard]] auto operator==(const Selection&) const -> bool;

 private:
  [[nodiscard]] auto StepAt(std::int64_t depth) const -> const Step&;

  std::vector<Step> steps_;
};

// Where the code settling a selection hands what its steps come to by the
// types alone, before any storage is reached: the key each entry of an
// associative array the selection names is kept by, converted from what
// foreign code named it with as the design converts an index (LRM 7.8.4); how
// many positions a takeover of the variable counts before the part named (LRM
// 10.6.2), none where a step reaches a part no takeover holds at a fixed
// position; and the path a wait on the part watches it by, as the design's own
// wait on that select does (LRM 9.4.2), which stops at a part no step of a
// path names -- an entry of an associative array, a member of an untagged
// union, whose storage is the union's. Defined in the library, which every
// unit describing a data object reaches.
class SelectionSettler {
 public:
  template <class K>
  void Key(std::int64_t depth, const K& key) {
    SettleKey(depth, &key, value::LibraryTypeOf<K>());
  }
  void SettleKey(
      std::int64_t depth, const void* key, const value::ValueType& type);
  void SettlePositions(std::int64_t before);
  void SettleUnfixed();
  void SettleWatchedStep(std::int64_t step);
  void SettleUnwatched();

  // `named` with each entry it names by what foreign code wrote named by the
  // key the array keeps it by.
  [[nodiscard]] auto Settled(const Selection& named) const -> Selection;
  // Whether the part named lies at a fixed position, and how many positions
  // lie before it there.
  [[nodiscard]] auto Fixed() const -> bool;
  [[nodiscard]] auto Positions() const -> std::int64_t;
  // The path from the data object to the part a wait on the selection watches,
  // inexact where that is a part holding the one named.
  [[nodiscard]] auto WatchedPath() const -> PartPath;

 private:
  std::vector<std::pair<std::int64_t, SelectedKey>> keys_;
  std::int64_t positions_ = 0;
  bool fixed_ = true;
  std::vector<std::int64_t> steps_;
  bool unwatched_ = false;
};

// How many elements an array whose size the run sets holds now.
struct SerializedSize {
  std::int64_t count = 0;
};

// A key an associative array keeps an entry by, where it lies, with its type.
struct SerializedKey {
  const void* bytes = nullptr;
  const value::ValueType* type = nullptr;
};

// One thing the code reading a data object hands a reader: the bits of an
// integral value, a real, a string, the object an event or class variable
// names, an array's size, or a key of one of its entries.
using Serialized = std::variant<
    value::ConstIntegralView, double, const value::String*,
    const value::ObjectRef*, SerializedSize, SerializedKey>;

// Where the code reading a data object for a reader reaching the design by
// name hands the value it read (LRM 38.15, 21.7.2.3): the bits of an integral
// value, a real, a string, or the object an event or class variable names.
// Where the selection ends at an array whose size the run sets -- a dynamic
// array, a queue, an associative array -- it hands how many elements it holds
// now (LRM 37.17 detail 9), and an associative array then hands the key of each
// entry it holds, in the order it keeps them (LRM 7.9.4), where the reader asks
// for them. That code is a method of the class declaring the object, compiled
// where the object's type is known, so the reader is handed a value and never
// reads storage itself.
class ValueSerializer {
 public:
  template <class I>
  void Bits(const I& value) {
    const auto words = value.AsWords();
    SerializeBits(words.View());
  }
  void Real(const value::Real& value);
  void Real(const value::ShortReal& value);
  template <class K>
  void Key(const K& key) {
    SerializeKey(&key, value::LibraryTypeOf<K>());
  }

  // Defined in the library, which every unit reading a data object reaches,
  // so no unit compiles one again.
  void SerializeBits(const value::ConstIntegralView& value);
  void SerializeReal(double value);
  void SerializeText(const value::String& value);
  void SerializeObject(const value::ObjectRef& object);
  // Answers whether the reader asks for an associative array's keys too.
  auto SerializeSize(std::int64_t size) -> bool;
  void SerializeKey(const void* key, const value::ValueType& type);

  // Takes what the reading code handed, which the reader says what it means
  // for each kind of.
  virtual void Receive(const Serialized& handed) = 0;
  [[nodiscard]] virtual auto WantsKeys() const -> bool = 0;

 protected:
  ValueSerializer() = default;
  ValueSerializer(const ValueSerializer&) = default;
  ValueSerializer(ValueSerializer&&) = default;
  auto operator=(const ValueSerializer&) -> ValueSerializer& = default;
  auto operator=(ValueSerializer&&) -> ValueSerializer& = default;
  ~ValueSerializer() = default;
};

// Where the code writing a data object for a reader reaching the design by name
// takes the value the write lands (LRM 38.34): the bits of the whole packed
// value it lands on, read into that value's type as a stream of bits is (LRM
// 6.24.3), a real, or a string. That code writes as the design's own write of
// the place does, so what waits on the place sees the write as it would see
// the design's.
class ValueDeserializer {
 public:
  [[nodiscard]] virtual auto Bits() -> value::BitVec = 0;
  [[nodiscard]] virtual auto Real() -> value::Real = 0;
  [[nodiscard]] virtual auto Text() -> value::String = 0;

 protected:
  ValueDeserializer() = default;
  ValueDeserializer(const ValueDeserializer&) = default;
  ValueDeserializer(ValueDeserializer&&) = default;
  auto operator=(const ValueDeserializer&) -> ValueDeserializer& = default;
  auto operator=(ValueDeserializer&&) -> ValueDeserializer& = default;
  ~ValueDeserializer() = default;
};

// Where the code watching a data object for a reader reaching the design by
// name hands the trigger a wait for a change of the object would enrol (LRM
// 9.4.2), so the reader learns of a change as the design's own wait does:
// through what a net shows and a port is bound to, following a force (LRM
// 10.6.2).
class Subscriber {
 public:
  virtual void Subscribe(const Trigger& trigger) = 0;

 protected:
  Subscriber() = default;
  Subscriber(const Subscriber&) = default;
  Subscriber(Subscriber&&) = default;
  auto operator=(const Subscriber&) -> Subscriber& = default;
  auto operator=(Subscriber&&) -> Subscriber& = default;
  ~Subscriber() = default;
};

// The trigger a wait for a change of the data object at `row` among those
// `declarations` describes would enrol, the object `instance` holding it --
// none for a namespace's -- which whatever else learns of its changes enrols
// on: a value dump (LRM 21.7.1.4), a callback (LRM 38.36.1).
[[nodiscard]] auto TriggerOfDataObject(
    const abi::ScopeDeclarations& declarations, GcObject* instance,
    std::int64_t row) -> Trigger;

}  // namespace lyra::runtime
