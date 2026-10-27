#include "lyra/runtime/deferred_effects.hpp"

#include <algorithm>
#include <cstddef>
#include <iterator>
#include <memory>
#include <new>
#include <span>

#include "lyra/base/internal_error.hpp"
#include "lyra/runtime/closure.hpp"
#include "lyra/runtime/effect_storage.hpp"

namespace lyra::runtime {

namespace {

// The place in front of the effect made at `made`.
auto PlaceOf(void* made) -> detail::EffectPlace* {
  if (made == nullptr) {
    throw InternalError(
        "DeferredEffects: an effect is kept at no address -- please report "
        "this as a bug");
  }
  return std::prev(static_cast<detail::EffectPlace*>(made));
}

// The storage right behind `place`, which is what the place's allocation handed
// out for an effect to be made in.
auto MadeBehind(detail::EffectPlace* place) -> void* {
  return std::next(place);
}

// The effect made behind `place`. Every effect is a closure, which opens with
// the part this library keeps.
auto EffectAt(detail::EffectPlace* place) -> ClosureValue* {
  return static_cast<ClosureValue*>(MadeBehind(place));
}

}  // namespace

OwnedEffect::~OwnedEffect() {
  if (place_ == nullptr) {
    return;
  }
  const detail::EffectPlace place = *place_;
  EffectAt(place_)->End();
  storage_->Release(place.start, place.bytes, place.alignment);
}

void OwnedEffect::Call() const {
  ClosureValue* const effect = EffectAt(place_);
  detail::DefinitionAt<support::EffectSignature>(*effect).call(effect);
}

DeferredEffects::~DeferredEffects() {
  Clear();
}

auto DeferredEffects::Allocate(std::size_t size, std::size_t alignment)
    -> void* {
  // The place lies right in front of the effect, so the effect starts at the
  // first multiple of its alignment, and of the place's, past a whole place.
  const std::size_t aligned = std::max(alignment, alignof(detail::EffectPlace));
  const std::size_t offset =
      (sizeof(detail::EffectPlace) + aligned - 1) / aligned * aligned;
  const std::size_t bytes = offset + size;
  void* const start = storage_->Allocate(bytes, aligned);
  void* const made =
      std::span{static_cast<std::byte*>(start), bytes}.subspan(offset).data();
  std::construct_at(
      PlaceOf(made), detail::EffectPlace{
                         .next = nullptr,
                         .start = start,
                         .bytes = bytes,
                         .alignment = aligned});
  return made;
}

void DeferredEffects::Commit(void* made) {
  detail::EffectPlace* place = PlaceOf(made);
  if (last_ == nullptr) {
    first_ = place;
  } else {
    last_->next = place;
  }
  last_ = place;
}

auto DeferredEffects::Take(void* made) -> OwnedEffect {
  return {PlaceOf(made), *storage_};
}

void DeferredEffects::Append(DeferredEffects& other) {
  if (other.first_ == nullptr) {
    return;
  }
  if (last_ == nullptr) {
    first_ = other.first_;
  } else {
    last_->next = other.first_;
  }
  last_ = other.last_;
  other.first_ = nullptr;
  other.last_ = nullptr;
}

auto DeferredEffects::PopFront() noexcept -> detail::EffectPlace* {
  detail::EffectPlace* place = first_;
  if (place == nullptr) {
    return nullptr;
  }
  first_ = place->next;
  if (first_ == nullptr) {
    last_ = nullptr;
  }
  return place;
}

void DeferredEffects::Clear() noexcept {
  while (detail::EffectPlace* place = PopFront()) {
    const OwnedEffect held(place, *storage_);
  }
}

}  // namespace lyra::runtime

auto operator new(std::size_t size, lyra::runtime::DeferredEffects* effects)
    -> void* {
  return effects->Allocate(size, __STDCPP_DEFAULT_NEW_ALIGNMENT__);
}

auto operator new(
    std::size_t size, std::align_val_t alignment,
    lyra::runtime::DeferredEffects* effects) -> void* {
  return effects->Allocate(size, static_cast<std::size_t>(alignment));
}
