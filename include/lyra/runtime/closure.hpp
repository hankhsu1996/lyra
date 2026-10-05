#pragma once

#include <array>
#include <bit>
#include <cstddef>
#include <cstdint>
#include <memory>
#include <new>
#include <utility>

#include "lyra/support/value_domain.hpp"
#include "lyra/value/any_value.hpp"
#include "lyra/value/runtime_value.hpp"
#include "lyra/value/value_type.hpp"

namespace lyra::runtime {

// The immutable definition of one closure: the body a call runs, and how the
// storage its captures live in is laid out. Held once and shared by every value
// built from it, the way a class's definition is shared by its values, and like
// one emitted by the unit declaring it as a constant laid out as C lays a
// structure out.
//
// The body is entered on the closure value its captures live in, under the one
// call protocol its signature states, so exactly one of the entries below is
// set: an ordinary body runs to completion and answers nothing, a coroutine
// body yields the handle whoever entered it drives from there, a per-element
// body is run once for each entry of a container it was handed and results in a
// value, and a value body takes nothing and results in a value each time it is
// run. Whoever runs a closure asks for the protocol it runs it under.
//
// A body that answers a value builds it in storage its caller gives, and states
// the type it comes back in, because a handle carries no type: it is a fact
// only whoever compiled the body holds. A body run per entry also states which
// of the runtime's kinds of value that type is, which is what an entry's answer
// is held as.
//
// The captures lie in the value itself, after what this library keeps there,
// where the code building the closure placed them and fills them. So of the
// value this states only how large it is in all and the body ending what that
// code filled, as a class's definition states the size of its objects and its
// destructor.
struct ClosureDefinition {
  void (*run)(void* self) = nullptr;
  void* (*start)(void* self) = nullptr;
  void* (*run_per_element)(
      void* self, const void* item, const void* index, void* out) = nullptr;
  void* (*run_value)(void* self, void* out) = nullptr;
  const value::ValueType* result_type = nullptr;
  support::ValueDomain result_domain{};
  std::uint64_t size = 0;
  void (*end_captures)(void* self) = nullptr;
};

class ClosureValue;

// Ends a closure value and gives back the storage it and its captures share.
struct EndClosure {
  void operator()(ClosureValue* closure) const noexcept;
};

// A closure value as whoever holds it carries it: the one owner of the closure,
// which stays where it was made while the owner is handed on.
using OwnedClosure = std::unique_ptr<ClosureValue, EndClosure>;

// A callable the runtime runs on the program's behalf -- a non-blocking
// assignment, a postponed print, a deferred assertion's action, the branch a
// `fork` spawns, the `with` expression an array method evaluates per entry, the
// expression an event control is watching for a change in the value of. It
// holds its captures in itself, so a captured value is a copy taken where the
// closure was built and released with the closure, never a handle into the
// body that built it, which may be gone by the time this one runs. What fills
// them is the code building the closure, which laid them out.
//
// The value never moves: a coroutine body's frame reads the captures through
// the value it was entered on for as long as it runs, and whatever keeps the
// closure is handed its owner instead.
class ClosureValue {
 public:
  // A value of the size the definition states, with nothing captured yet.
  [[nodiscard]] static auto Make(const ClosureDefinition* definition)
      -> OwnedClosure;

  ClosureValue(const ClosureValue&) = delete;
  auto operator=(const ClosureValue&) -> ClosureValue& = delete;
  ClosureValue(ClosureValue&&) = delete;
  auto operator=(ClosureValue&&) -> ClosureValue& = delete;
  ~ClosureValue();

  // Runs an ordinary body to completion.
  void Invoke();

  // Builds a coroutine body's frame over these captures and answers its handle,
  // having run no statement of it; the caller drives it from there.
  [[nodiscard]] auto Start() -> void*;

  // Runs a per-element body on one entry (LRM 7.12.4) and answers the value it
  // settled on.
  [[nodiscard]] auto RunPerElement(
      const value::RuntimeValue& item, const value::RuntimeValue& index)
      -> value::RuntimeValue;

  // Runs a body that takes nothing and answers what it settled on.
  [[nodiscard]] auto RunValue() -> value::AnyValue;

  // The same, for a body answering a value of `type`, one of the library's own
  // kinds.
  template <typename T>
  [[nodiscard]] auto RunValueOf(const value::ValueTypeOf<T>& type) -> T {
    alignas(T) std::array<std::byte, sizeof(T)> storage{};
    RunValueInto(type, storage.data());
    T* built = std::launder(std::bit_cast<T*>(storage.data()));
    T answer = std::move(*built);
    std::destroy_at(built);
    return answer;
  }

 private:
  explicit ClosureValue(const ClosureDefinition* definition);

  // Runs a body that takes nothing into `out`, which is sized for `type`.
  void RunValueInto(const value::ValueType& type, void* out);

  const ClosureDefinition* definition_;
};

}  // namespace lyra::runtime
