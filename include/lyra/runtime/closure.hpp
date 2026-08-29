#pragma once

#include <cstdint>
#include <memory>
#include <span>
#include <vector>

#include "lyra/runtime/member_storage.hpp"
#include "lyra/runtime/scope_program.hpp"

namespace lyra::runtime {

// A closure's body, entered on the closure value its captures live in.
using ClosureEntry = void (*)(void* self);

// The immutable definition of one closure: the body a call runs, and the
// storage schema its captures need. Held once and shared by every value built
// from it, the way a scope class's definition is shared by its instances.
struct ClosureDefinition {
  ClosureEntry invoke = nullptr;
  MemberStorageSchema captures;
};

// A callable the runtime holds and runs later -- a non-blocking assignment, a
// postponed print, a deferred assertion's action. It owns one storage object
// per capture, so a captured value is a copy taken where the closure was built
// and released with the closure, never a handle into the stretch that built it,
// which is gone by the time the body runs.
class ClosureValue {
 public:
  // `captures` supplies one handle per capture, in declaration order. Each is
  // taken as the schema says: a pointer is held, a value is copied.
  ClosureValue(
      const ClosureDefinition* definition, std::span<void* const> captures);

  // The handle capture `index` crosses back to the body as.
  [[nodiscard]] auto Capture(std::uint32_t index) -> void*;

  void Invoke();

 private:
  const ClosureDefinition* definition_;
  std::vector<std::unique_ptr<MemberStorage>> captures_;
};

}  // namespace lyra::runtime
