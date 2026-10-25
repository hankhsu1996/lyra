#pragma once

#include <cstddef>
#include <memory>
#include <new>
#include <type_traits>
#include <utility>

#include "lyra/runtime/closure.hpp"
#include "lyra/runtime/effect_storage.hpp"
#include "lyra/support/callable_bounds.hpp"

namespace lyra::runtime {

namespace detail {

// An effect the library itself defers, made a closure of the one signature a
// region runs its effects at, so a region holds it as it holds the program's.
template <typename Effect>
class LibraryEffect final : public ClosureValue {
 public:
  explicit LibraryEffect(Effect effect);

  void operator()() {
    effect_();
  }

 private:
  Effect effect_;
};

template <typename Effect>
inline constexpr ClosureDefinition<support::EffectSignature>
    kLibraryEffectDefinition = DefinitionOf<LibraryEffect<Effect>>();

template <typename Effect>
LibraryEffect<Effect>::LibraryEffect(Effect effect)
    : ClosureValue(&kLibraryEffectDefinition<Effect>),
      effect_(std::move(effect)) {
}

// One effect's bytes, as the run's effect storage handed them out, and its
// place in the order of the effects holding it. The place lies right in front
// of the effect, in the same bytes, so keeping an effect once it exists asks
// for nothing that could fail and leave it unended, and the effect's address
// alone finds its place, as the place's alone finds the effect.
struct EffectPlace {
  EffectPlace* next;
  void* start;
  std::size_t bytes;
  std::size_t alignment;
};

}  // namespace detail

// An effect held by something the region runs rather than by the region
// itself: it ends the effect when it is itself ended, and gives the effect's
// bytes back to the run's effect storage, as a box ends its value and frees
// it.
class OwnedEffect {
 public:
  OwnedEffect(const OwnedEffect&) = delete;
  auto operator=(const OwnedEffect&) -> OwnedEffect& = delete;
  OwnedEffect(OwnedEffect&& other) noexcept
      : place_(std::exchange(other.place_, nullptr)), storage_(other.storage_) {
  }
  auto operator=(OwnedEffect&&) -> OwnedEffect& = delete;
  ~OwnedEffect();

  void Call() const;

 private:
  friend class DeferredEffects;
  OwnedEffect(detail::EffectPlace* place, EffectStorage& storage)
      : place_(place), storage_(&storage) {
  }

  detail::EffectPlace* place_;
  EffectStorage* storage_;
};

// The deferred effects one region of one time slot holds pending -- the
// nonblocking updates, the postponed reports, the actions an assertion
// matured -- in the order they were kept. Each is made in bytes the run's
// effect storage hands out, which the effects only borrow, so a later slot's
// effects become the current slot's by joining two orders, with nothing moved.
//
// The effects run in the order they were kept (LRM 4.6). Every effect made
// here is ended exactly once -- after it runs, or, never run, when the effects
// are cleared or these end -- and its bytes go back once it has ended.
class DeferredEffects {
 public:
  explicit DeferredEffects(EffectStorage& storage) : storage_(&storage) {
  }
  DeferredEffects(const DeferredEffects&) = delete;
  auto operator=(const DeferredEffects&) -> DeferredEffects& = delete;
  DeferredEffects(DeferredEffects&&) = delete;
  auto operator=(DeferredEffects&&) -> DeferredEffects& = delete;
  ~DeferredEffects();

  // Where to make one effect of `size` bytes at a multiple of `alignment`,
  // which its maker builds there and then commits. An effect made there and
  // never committed or taken is never ended, and its bytes are never handed
  // out again; only a failed allocation can raise while one is built.
  [[nodiscard]] auto Allocate(std::size_t size, std::size_t alignment) -> void*;

  // Keeps the effect made at `made` -- an address `Allocate` handed out, now
  // holding a closure called at the effect signature -- behind every effect
  // kept before it. It allocates nothing, so an effect that exists is always
  // kept.
  void Commit(void* made);

  // The effect made at `made`, never to be committed: whoever holds what this
  // answers ends it and gives its bytes back.
  [[nodiscard]] auto Take(void* made) -> OwnedEffect;

  // Makes `effect`, an effect the library defers, here and keeps it.
  template <typename Effect>
  void Emplace(Effect&& effect) {
    using Made = detail::LibraryEffect<std::decay_t<Effect>>;
    void* const made = Allocate(sizeof(Made), alignof(Made));
    std::construct_at(static_cast<Made*>(made), std::forward<Effect>(effect));
    Commit(made);
  }

  [[nodiscard]] auto Empty() const noexcept -> bool {
    return first_ == nullptr;
  }

  // Moves every effect `other` holds onto the back of this one, in the order
  // each was kept, leaving `other` holding none. Nothing moves: each effect
  // stays where it was made.
  void Append(DeferredEffects& other);

  // Runs every effect in the order it was kept, each through `run`, which is
  // handed the effect to call, and ends each and gives its bytes back as it
  // leaves it, whether or not `run` raised.
  template <typename Run>
  void Drain(const Run& run) {
    while (detail::EffectPlace* place = PopFront()) {
      const OwnedEffect held(place, *storage_);
      run([&held] { held.Call(); });
    }
  }

 private:
  // Ends every effect held, none of them run, and gives their bytes back.
  void Clear() noexcept;

  auto PopFront() noexcept -> detail::EffectPlace*;

  EffectStorage* storage_;
  detail::EffectPlace* first_ = nullptr;
  detail::EffectPlace* last_ = nullptr;
};

}  // namespace lyra::runtime

// The allocation function a placement new-expression naming a region's
// deferred effects calls, so a closure the program keeps there is made in the
// run's effect storage ([expr.new]); the over-aligned form is the one a type
// whose alignment passes the default asks for. No deallocation function
// matches them, so a construction that raises gives nothing back ([expr.new]),
// as the execution backend's lowering of the same expression does; only a
// failed allocation can raise there.
[[nodiscard]] auto operator new(
    std::size_t size, lyra::runtime::DeferredEffects* effects) -> void*;
[[nodiscard]] auto operator new(
    std::size_t size, std::align_val_t alignment,
    lyra::runtime::DeferredEffects* effects) -> void*;
