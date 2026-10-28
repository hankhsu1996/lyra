#pragma once

#include <memory>
#include <new>

#include "lyra/runtime/tail.hpp"
#include "lyra/value/any_value.hpp"
#include "lyra/value/frame_bound.hpp"
#include "lyra/value/laid_out_value.hpp"
#include "lyra/value/value_type.hpp"

namespace lyra::runtime {

// The storage of a value laid out by its type -- a structure, a union, a tuple,
// a fixed-size unpacked array -- whose type the library is told where the
// holder is built: the type, then the value laid out by it, inline at the
// holder's tail or spilled into storage allocated where the holder is built. So
// the value has one address for as long as the holder lives.
//
// The value is built where the holder is, at what its type builds before
// anything writes it, as words storage fills its words; every value assigned
// afterwards assigns into it.
class LaidOutStorage : public TailValue<LaidOutStorage> {
 public:
  using Owned = value::LaidOutValue;

  explicit LaidOutStorage(const value::ValueType& type)
      : TailValue<LaidOutStorage>(SpilledValueOf(type)), type_(&type) {
    if (!this->IsSpilled()) {
      type.BuildDefault(this->Bytes());
    }
  }
  LaidOutStorage(const LaidOutStorage&) = delete;
  auto operator=(const LaidOutStorage&) -> LaidOutStorage& = delete;
  LaidOutStorage(LaidOutStorage&&) = delete;
  auto operator=(LaidOutStorage&&) -> LaidOutStorage& = delete;
  ~LaidOutStorage() {
    type_->Destroy(this->Bytes());
    if (this->IsSpilled()) {
      ::operator delete(this->Bytes(), std::align_val_t{type_->Align()});
    }
  }

  // Whether the storage holds a value a write may replace, which it does from
  // the moment it is built.
  [[nodiscard]] static auto IsInstalled() -> bool {
    return true;
  }
  [[nodiscard]] auto Type() const -> const value::ValueType& {
    return *type_;
  }

  // What a value of `type` takes.
  [[nodiscard]] static auto ValueLayoutFor(const value::ValueType& type)
      -> support::Layout {
    return type.Layout();
  }
  [[nodiscard]] auto ValueLayout() const -> support::Layout {
    return ValueLayoutFor(*type_);
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
    return type_->BitIdentical(this->Bytes(), bytes);
  }
  void AssignBytes(const void* bytes) {
    type_->Assign(this->Bytes(), bytes);
  }
  [[nodiscard]] auto AsOwned() const -> Owned {
    return ToOwned(this->Bytes());
  }
  [[nodiscard]] auto ToOwned(const void* bytes) const -> Owned {
    return Owned(value::AnyValue::CopyOf(*type_, bytes));
  }

 private:
  // Storage of its own holding what `type` builds before anything writes it,
  // where a value of it does not lie inline, and none where it does. The
  // storage is given back if building the value in it fails, since no
  // destructor runs for what was never built.
  static auto SpilledValueOf(const value::ValueType& type) -> void* {
    if (value::LiesInline(ValueLayoutFor(type))) {
      return nullptr;
    }
    const auto give_back = [&type](void* spilled) {
      ::operator delete(spilled, std::align_val_t{type.Align()});
    };
    std::unique_ptr<void, decltype(give_back)> spilled(
        ::operator new(type.Size(), std::align_val_t{type.Align()}), give_back);
    type.BuildDefault(spilled.get());
    return spilled.release();
  }

  const value::ValueType* type_;
};

}  // namespace lyra::runtime
