#pragma once

#include <cstdint>
#include <memory>
#include <span>
#include <variant>
#include <vector>

#include "lyra/runtime/member_storage.hpp"
#include "lyra/runtime/scope_program.hpp"

namespace lyra::runtime {

// A closure's body, entered on the closure value its captures live in. The two
// alternatives are the two call protocols a result type states: an ordinary
// body runs to completion and answers nothing, a coroutine body yields the
// handle whoever entered it drives from there.
struct SynchronousBody {
  void (*run)(void* self) = nullptr;
};
struct CoroutineBody {
  void* (*start)(void* self) = nullptr;
};
using ClosureBody = std::variant<SynchronousBody, CoroutineBody>;

// The immutable definition of one closure: the body a call runs, and the
// storage schema its captures need. Held once and shared by every value built
// from it, the way a scope class's definition is shared by its instances.
struct ClosureDefinition {
  ClosureBody body;
  MemberStorageSchema captures;
};

// A callable the runtime holds and runs later -- a non-blocking assignment, a
// postponed print, a deferred assertion's action, the branch a `fork` spawns.
// It owns one storage object per capture, so a captured value is a copy taken
// where the closure was built and released with the closure, never a handle
// into the stretch that built it, which is gone by the time the body runs.
class ClosureValue {
 public:
  // `captures` supplies one handle per capture, in declaration order. Each is
  // taken as the schema says: a pointer is held, a value is copied.
  ClosureValue(
      const ClosureDefinition* definition, std::span<void* const> captures);

  // The handle capture `index` crosses back to the body as.
  [[nodiscard]] auto Capture(std::uint32_t index) -> void*;

  // Runs an ordinary body to completion. A body whose protocol is the
  // coroutine one is entered through `Start` instead.
  void Invoke();

  // Enters a coroutine body and answers the handle it yielded, having run to
  // its first suspension. The caller drives it from there.
  [[nodiscard]] auto Start() -> void*;

 private:
  const ClosureDefinition* definition_;
  std::vector<std::unique_ptr<MemberStorage>> captures_;
};

}  // namespace lyra::runtime
