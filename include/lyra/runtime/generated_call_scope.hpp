#pragma once

#include <memory>
#include <utility>
#include <vector>

namespace lyra::runtime {

// A stack-scoped region owning the runtime values a single runtime-to-generated
// call materializes. The generated side sees only opaque handles; their storage
// and destruction belong to the runtime, released together when the call
// returns. Objects are destroyed in reverse construction order; a borrow (a
// print item over a string) stays valid until the whole region drops. Empty
// until the first allocation, so a backend that materializes nothing (a native
// C++ body using real values) pays no storage.
class GeneratedCallArena {
 public:
  GeneratedCallArena() = default;
  ~GeneratedCallArena() = default;
  GeneratedCallArena(const GeneratedCallArena&) = delete;
  auto operator=(const GeneratedCallArena&) -> GeneratedCallArena& = delete;
  GeneratedCallArena(GeneratedCallArena&&) = delete;
  auto operator=(GeneratedCallArena&&) -> GeneratedCallArena& = delete;

  template <typename T, typename... Args>
  auto New(Args&&... args) -> T* {
    auto owner = std::make_shared<T>(std::forward<Args>(args)...);
    T* value = owner.get();
    objects_.push_back(std::move(owner));
    return value;
  }

 private:
  std::vector<std::shared_ptr<void>> objects_;
};

// The ambient context wrapping every runtime-to-generated call boundary: the
// construct entry, each lifecycle body, and each resumed process. It is the
// runtime/generated boundary itself, not a backend-private notion -- every
// backend crosses it; a backend that hands values across as opaque handles
// allocates them into the innermost arena, while a native C++ backend enters
// the scope and allocates nothing. The generated IR never names it; the runtime
// pushes one around each call.
class GeneratedCallScope {
 public:
  GeneratedCallScope();
  ~GeneratedCallScope();
  GeneratedCallScope(const GeneratedCallScope&) = delete;
  auto operator=(const GeneratedCallScope&) -> GeneratedCallScope& = delete;
  GeneratedCallScope(GeneratedCallScope&&) = delete;
  auto operator=(GeneratedCallScope&&) -> GeneratedCallScope& = delete;

  auto Arena() -> GeneratedCallArena& {
    return arena_;
  }

  static auto Current() -> GeneratedCallScope&;

 private:
  GeneratedCallScope* previous_;
  GeneratedCallArena arena_;
};

}  // namespace lyra::runtime
