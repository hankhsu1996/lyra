#pragma once

#include "lyra/runtime/trigger.hpp"
#include "lyra/value/integral_words.hpp"
#include "lyra/value/object_ref.hpp"
#include "lyra/value/real.hpp"

namespace lyra::runtime {

// Where the code reading a data object for a reader reaching the design by
// name hands the value it read (LRM 38.15, 21.7.2.3): the bits of an integral
// value, a real, or the object an event variable names. That code is a method
// of the class declaring the object, compiled where the object's type is
// known, so the reader is handed a value and never reads storage itself.
class ValueSerializer {
 public:
  template <class I>
  void Bits(const I& value) {
    const auto words = value.AsWords();
    SerializeBits(words.View());
  }
  void Real(const value::Real& value);
  void Real(const value::ShortReal& value);

  virtual void SerializeBits(const value::ConstIntegralView& value) = 0;
  virtual void SerializeReal(double value) = 0;
  virtual void SerializeEvent(const value::ObjectRef& event) = 0;

 protected:
  ValueSerializer() = default;
  ValueSerializer(const ValueSerializer&) = default;
  ValueSerializer(ValueSerializer&&) = default;
  auto operator=(const ValueSerializer&) -> ValueSerializer& = default;
  auto operator=(ValueSerializer&&) -> ValueSerializer& = default;
  ~ValueSerializer() = default;
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

}  // namespace lyra::runtime
