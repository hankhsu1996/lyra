#pragma once

#include <cstdint>
#include <span>
#include <variant>

#include "lyra/runtime/scope_program.hpp"
#include "lyra/runtime/storage_block.hpp"
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
// A body that answers a value states which representation the answer comes back
// in, because a result crosses as a handle and a handle carries no type: it is
// a fact only whoever compiled the body holds.
struct SynchronousBody {
  void (*run)(void* self) = nullptr;
};
struct CoroutineBody {
  void* (*start)(void* self) = nullptr;
};
struct PerElementBody {
  void* (*run)(void* self, const void* item, const void* index) = nullptr;
  support::ValueDomain result_domain{};
};
struct ValueBody {
  void* (*run)(void* self) = nullptr;
  support::ValueDomain result_domain{};
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

// A callable the runtime runs on the program's behalf -- a non-blocking
// assignment, a postponed print, a deferred assertion's action, the branch a
// `fork` spawns, the `with` expression an array method evaluates per entry, the
// expression an event control is watching for a change in the value of. It
// owns one storage object per capture, so a captured value is a copy taken
// where the closure was built and released with the closure, never a handle
// into the stretch that built it, which may be gone by the time the body runs.
class ClosureValue {
 public:
  // `captures` supplies one handle per capture, in declaration order. Each is
  // taken as the schema says: a pointer is held, a value is copied.
  ClosureValue(
      const ClosureDefinition* definition, std::span<void* const> captures);

  // The handle capture `index` crosses back to the body as.
  [[nodiscard]] auto Capture(std::uint32_t index) -> void*;

  // Runs an ordinary body to completion.
  void Invoke();

  // Builds a coroutine body's frame over these captures and answers its handle,
  // having run no statement of it; the caller drives it from there. The body
  // reads its captures through this value's address for as long as it runs, so
  // this may only be called once this value is where it will stay.
  [[nodiscard]] auto Start() -> void*;

  // Runs a per-element body on one entry (LRM 7.12.4) and answers the value it
  // settled on, read out of the scope it was materialized in.
  [[nodiscard]] auto RunPerElement(
      const value::RuntimeValue& item, const value::RuntimeValue& index)
      -> value::RuntimeValue;

  // Runs a body that takes nothing and answers what it settled on, read out of
  // the scope it was materialized in.
  [[nodiscard]] auto RunValue() -> value::RuntimeValue;

 private:
  const ClosureDefinition* definition_;
  StorageBlock captures_;
};

}  // namespace lyra::runtime
