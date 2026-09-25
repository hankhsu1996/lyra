#pragma once

#include <array>
#include <cstddef>
#include <memory>

#include "lyra/runtime/var.hpp"

namespace lyra::runtime {

// A write in progress into the storage a capability wrapper stands for (LRM
// 11.5.1), held by whoever writes for as long as the write lasts. It is the
// partial-write bracket a wrapper opens, in a form a caller that names no C++
// type can hold: the caller gives it storage, reaches the wrapper's contents
// through it, writes whatever parts it writes in place, and ends it once the
// write is over -- which is when the wrapper is told, once, what the write
// did.
//
// Which wrapper it is, and what that wrapper keeps from before the write, is
// the bracket's own business and is held inside it. So one object serves every
// wrapper and every value, and what it costs is what the bracket costs: a
// before-image only where something is armed to read the answer.
class OpenWrite {
 public:
  template <MutationSink Sink>
  explicit OpenWrite(Sink sink) : bracket_of_(&kBracketOf<Sink>) {
    std::construct_at(Bracket<Sink>(bracket_.data()), sink);
  }

  OpenWrite(const OpenWrite&) = delete;
  auto operator=(const OpenWrite&) -> OpenWrite& = delete;
  OpenWrite(OpenWrite&&) = delete;
  auto operator=(OpenWrite&&) -> OpenWrite& = delete;

  ~OpenWrite() {
    bracket_of_->end(bracket_.data());
  }

  // The wrapper's contents, as storage a part of the write lands in.
  [[nodiscard]] auto Storage() -> void* {
    return bracket_of_->storage(bracket_.data());
  }

 private:
  // Room for the largest bracket any wrapper opens: a reference to the storage,
  // the wrapper, and the value it held before the write.
  static constexpr std::size_t kCapacity = 136;

  // What the room holds, asked of the bracket one wrapper opened there.
  struct BracketOf {
    void* (*storage)(void* room);
    void (*end)(void* room);
  };

  template <MutationSink Sink>
  static auto Bracket(void* room) -> ScopedMutation<Sink>* {
    static_assert(sizeof(ScopedMutation<Sink>) <= kCapacity);
    static_assert(alignof(ScopedMutation<Sink>) <= alignof(void*));
    return static_cast<ScopedMutation<Sink>*>(room);
  }

  template <MutationSink Sink>
  static constexpr BracketOf kBracketOf{
      .storage = [](void* room) -> void* { return &**Bracket<Sink>(room); },
      .end = [](void* room) { std::destroy_at(Bracket<Sink>(room)); }};

  alignas(void*) std::array<std::byte, kCapacity> bracket_{};
  const BracketOf* bracket_of_;
};

}  // namespace lyra::runtime
