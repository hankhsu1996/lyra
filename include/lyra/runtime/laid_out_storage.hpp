#pragma once

#include <memory>
#include <new>

#include "lyra/runtime/tail.hpp"
#include "lyra/value/any_value.hpp"
#include "lyra/value/element_type.hpp"
#include "lyra/value/frame_bound.hpp"
#include "lyra/value/laid_out_value.hpp"
#include "lyra/value/value_type.hpp"

namespace lyra::runtime {

// The storage of a value laid out by its type -- a structure, a union, a tuple,
// a fixed-size unpacked array -- whose type the library is told where the
// holder is built, with how a value of it starts (LRM Table 6-7): the type,
// then the value laid out by it, inline at the holder's tail or spilled into
// storage allocated where the holder is built. So the value has one address
// for as long as the holder lives.
//
// The value is built where the holder is, at its type's default, as words
// storage fills its words; every value assigned afterwards assigns into it.
class LaidOutStorage : public TailValue<LaidOutStorage> {
 public:
  using Owned = value::LaidOutValue;

  explicit LaidOutStorage(const value::ElementType& held)
      : TailValue<LaidOutStorage>(SpilledValueOf(held)), held_(&held) {
    if (!this->IsSpilled()) {
      held.BuildDefault(this->Bytes());
    }
  }
  LaidOutStorage(const LaidOutStorage&) = delete;
  auto operator=(const LaidOutStorage&) -> LaidOutStorage& = delete;
  LaidOutStorage(LaidOutStorage&&) = delete;
  auto operator=(LaidOutStorage&&) -> LaidOutStorage& = delete;
  ~LaidOutStorage() {
    Type().Destroy(this->Bytes());
    if (this->IsSpilled()) {
      ::operator delete(this->Bytes(), std::align_val_t{Type().Align()});
    }
  }

  // Whether the storage holds a value a write may replace, which it does from
  // the moment it is built.
  [[nodiscard]] static auto IsInstalled() -> bool {
    return true;
  }
  [[nodiscard]] auto Type() const -> const value::ValueType& {
    return held_->Type();
  }
  // The type held, with how a value of it starts, which storage made like
  // this one is built from.
  [[nodiscard]] auto Held() const -> const value::ElementType& {
    return *held_;
  }

  // What a value of the type `held` states takes.
  [[nodiscard]] static auto ValueLayoutFor(const value::ElementType& held)
      -> support::Layout {
    return held.Type().Layout();
  }
  [[nodiscard]] auto ValueLayout() const -> support::Layout {
    return ValueLayoutFor(*held_);
  }

  // The value where it lies, which a write replacing the whole of it reads as
  // nothing more than that it changed.
  [[nodiscard]] auto Get() const -> const void* {
    return this->Bytes();
  }

  // Whether the value at `bytes` is the one held, taking it, the value held as
  // the owned type, which this holder keeps only as bytes and so copies, and a
  // copy of the one at `bytes`.
  [[nodiscard]] auto IsBitIdenticalTo(const void* bytes) const -> bool {
    return Type().BitIdentical(this->Bytes(), bytes);
  }
  void AssignBytes(const void* bytes) {
    Type().Assign(this->Bytes(), bytes);
  }
  [[nodiscard]] auto AsOwned() const -> Owned {
    return ToOwned(this->Bytes());
  }
  [[nodiscard]] auto ToOwned(const void* bytes) const -> Owned {
    return Owned(value::AnyValue::CopyOf(Type(), bytes));
  }

 private:
  // Storage of its own holding the default of the type `held` states, where a
  // value of it does not lie inline, and none where it does. The storage is
  // given back if building the value in it fails, since no destructor runs for
  // what was never built.
  static auto SpilledValueOf(const value::ElementType& held) -> void* {
    if (value::LiesInline(ValueLayoutFor(held))) {
      return nullptr;
    }
    const value::ValueType& type = held.Type();
    const auto give_back = [&type](void* spilled) {
      ::operator delete(spilled, std::align_val_t{type.Align()});
    };
    std::unique_ptr<void, decltype(give_back)> spilled(
        ::operator new(type.Size(), std::align_val_t{type.Align()}), give_back);
    held.BuildDefault(spilled.get());
    return spilled.release();
  }

  const value::ElementType* held_;
};

}  // namespace lyra::runtime
