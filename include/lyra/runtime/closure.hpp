#pragma once

#include <memory>
#include <span>
#include <variant>

#include "lyra/runtime/member_slots.hpp"
#include "lyra/runtime/scope_program.hpp"
#include "lyra/support/tuple_operations.hpp"
#include "lyra/support/value_domain.hpp"
#include "lyra/value/runtime_value.hpp"

namespace lyra::runtime {

// A closure's body, entered on the closure value its captures live in. The
// alternatives are the call protocols a signature states: an ordinary body runs
// to completion and answers nothing, a coroutine body yields the handle whoever
// entered it drives from there, a per-element body is run once for each entry
// of a container it was handed and results in a value, and a value body takes
// nothing and results in a value each time it is run.
//
// A body that answers a value builds it in storage its caller gives, and states
// which representation it comes back in, because a handle carries no type: it
// is a fact only whoever compiled the body holds. A tuple answer also names its
// type, which is what says how much storage it needs.
struct SynchronousBody {
  void (*run)(void* self) = nullptr;
};
struct CoroutineBody {
  void* (*start)(void* self) = nullptr;
};
struct PerElementBody {
  void* (*run)(void* self, const void* item, const void* index, void* out) =
      nullptr;
  support::ValueDomain result_domain{};
  const support::TupleOperations* result_tuple = nullptr;
};
struct ValueBody {
  void* (*run)(void* self, void* out) = nullptr;
  support::ValueDomain result_domain{};
  const support::TupleOperations* result_tuple = nullptr;
};
using ClosureBody =
    std::variant<SynchronousBody, CoroutineBody, PerElementBody, ValueBody>;

// The immutable definition of one closure: the body a call runs, and the
// storage schema its captures need. Held once and shared by every value built
// from it, the way a scope class's definition is shared by its instances.
struct ClosureDefinition {
  ClosureBody body;
  MemberStorageSchema captures;
};

class ClosureValue;

// A closure value as whoever holds it carries it: the one owner of the closure,
// which stays where it was made while the owner is handed on.
using OwnedClosure = std::unique_ptr<ClosureValue>;

// A callable the runtime runs on the program's behalf -- a non-blocking
// assignment, a postponed print, a deferred assertion's action, the branch a
// `fork` spawns, the `with` expression an array method evaluates per entry, the
// expression an event control is watching for a change in the value of. It
// owns one storage object per capture, so a captured value is a copy taken
// where the closure was built and released with the closure, never a handle
// into the body that built it, which may be gone by the time this one runs.
//
// The captures follow the value in its own allocation, so a body reaches one at
// a fixed distance from the value it was entered on. The value never moves: a
// coroutine body's frame reads the captures through that address for as long
// as it runs, and whatever keeps the closure is handed its owner instead.
class ClosureValue {
 public:
  // `captures` supplies one handle per capture, in declaration order. Each is
  // taken as the schema says: a pointer is held, a value is copied.
  [[nodiscard]] static auto Make(
      const ClosureDefinition* definition, std::span<void* const> captures)
      -> OwnedClosure;

  ClosureValue(const ClosureValue&) = delete;
  auto operator=(const ClosureValue&) -> ClosureValue& = delete;
  ClosureValue(ClosureValue&&) = delete;
  auto operator=(ClosureValue&&) -> ClosureValue& = delete;
  ~ClosureValue() = default;

  // Ending a closure returns the whole allocation it was made in, whose size is
  // not the value's own, so the release takes the address alone.
  static void operator delete(void* address);

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
  [[nodiscard]] auto RunValue() -> value::RuntimeValue;

 private:
  ClosureValue(
      const ClosureDefinition* definition, std::span<void* const> captures);

  const ClosureDefinition* definition_;
  MemberSlots captures_;
};

}  // namespace lyra::runtime
