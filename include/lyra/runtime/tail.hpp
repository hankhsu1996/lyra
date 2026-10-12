#pragma once

#include <bit>
#include <cstddef>
#include <cstdint>
#include <memory>
#include <new>
#include <utility>

#include "lyra/value/integral.hpp"

namespace lyra::runtime {

// What a value a holder keeps takes: how many bytes, at which alignment.
struct Layout {
  std::size_t size;
  std::size_t align;
};

// Whether a value lies inline, at the holder's tail, rather than spilled into
// storage of its own: it fits the size a value may be and still lie in what
// holds it, and the holder's own alignment serves it. The one rule a holder,
// the storage made for it, and the code generator laying it out all read.
[[nodiscard]] constexpr auto IsInline(Layout held) -> bool {
  return held.size <= value::kMaxInlineBytes && held.align <= alignof(void*);
}

// What a `Holder` whose tail keeps a value of layout `held` takes: the
// holder, then the value where it lies inline.
template <class Holder>
[[nodiscard]] constexpr auto HolderLayoutFor(Layout held) -> Layout {
  return {
      .size = sizeof(Holder) + (IsInline(held) ? held.size : 0),
      .align = alignof(Holder)};
}

// A `Holder` built with a tail for a value of layout `held`, held by a count
// of its owners.
template <class Holder, class... Args>
auto MakeWithTail(Layout held, Args&&... args) -> std::shared_ptr<Holder> {
  const Layout holder = HolderLayoutFor<Holder>(held);
  const std::align_val_t align{holder.align};
  // The storage is given back if building the holder in it fails.
  const auto give_back = [align](void* storage) {
    ::operator delete(storage, align);
  };
  std::unique_ptr<void, decltype(give_back)> storage(
      ::operator new(holder.size, align), give_back);
  Holder* made = std::construct_at(
      static_cast<Holder*>(storage.get()), std::forward<Args>(args)...);
  storage.release();
  return std::shared_ptr<Holder>(made, [align](Holder* ended) {
    std::destroy_at(ended);
    ::operator delete(ended, align);
  });
}

// What every storage of a value at its holder's tail is, written once: the
// address of the value, which is the holder's tail where the value lies inline
// and storage of its own where it is spilled, kept either way so a reader goes
// through one pointer whichever it is -- as LLVM's `SmallVector` keeps where
// its elements begin while they are inline. `Storage` is that storage itself,
// which is its holder's last part, and states how its value is held: its
// `Owned` form, and whether the value at some bytes is the one it holds.
template <class Storage>
class TailValue {
 public:
  TailValue(const TailValue&) = delete;
  auto operator=(const TailValue&) -> TailValue& = delete;
  TailValue(TailValue&&) = delete;
  auto operator=(TailValue&&) -> TailValue& = delete;

  // Where the value lies, and where the address of it is kept, which is what
  // generated code reads it through.
  [[nodiscard]] auto Bytes() const -> void* {
    return bytes_;
  }
  [[nodiscard]] auto BytesThrough() const -> void* const* {
    return &bytes_;
  }

  // What a holder's protocol asks of its storage over a value of its own
  // form, and over another storage of its kind, each by their bytes.
  template <class Owned>
  [[nodiscard]] auto IsBitIdentical(const Owned& value) const -> bool {
    return Self().IsBitIdenticalTo(value.Bytes());
  }
  template <class Owned>
  void Assign(const Owned& value) {
    Self().AssignBytes(value.Bytes());
  }

 protected:
  // The value lies at the tail, or in `spilled` where it does not, as
  // `IsInline` decides.
  explicit TailValue(void* spilled)
      : bytes_(spilled != nullptr ? spilled : Tail()) {
  }
  ~TailValue() = default;

  // Whether the value is spilled, which only the storage that made the place
  // it lies gives back. It is asked of the value's layout, as where the value
  // lies was decided: an address one past the holder is no answer, since
  // storage allocated apart may begin there.
  [[nodiscard]] auto IsSpilled() const -> bool {
    return !IsInline(Self().ValueLayout());
  }

 private:
  [[nodiscard]] auto Self() const -> const Storage& {
    return static_cast<const Storage&>(*this);
  }
  [[nodiscard]] auto Self() -> Storage& {
    return static_cast<Storage&>(*this);
  }
  // Where the holder ends: the storage is its last part.
  [[nodiscard]] auto Tail() const -> void* {
    return std::bit_cast<void*>(
        std::bit_cast<std::uintptr_t>(this) + sizeof(Storage));
  }

  void* bytes_;
};

}  // namespace lyra::runtime
