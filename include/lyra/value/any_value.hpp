#pragma once

#include <cstdint>
#include <memory>
#include <new>

#include "lyra/value/concepts.hpp"
#include "lyra/value/integral.hpp"
#include "lyra/value/integral_words.hpp"
#include "lyra/value/value_type.hpp"

namespace lyra::value {

// A value of a type the library was compiled without, in storage of its own,
// held with its type: what Rust's `Box<dyn Trait>` and Swift's existential
// container are. What a holder asks of it -- its copy, its end, whether a
// write changed it -- it asks of that type.
class AnyValue {
 public:
  // Holds no value: storage before whatever declares it installs one.
  AnyValue() = default;

  // A value of `type` that `build` lays out in the storage it is handed, which
  // the value then owns.
  template <typename Build>
  [[nodiscard]] static auto Built(const ValueType& type, Build build)
      -> AnyValue {
    const std::align_val_t align{type.Align()};
    std::unique_ptr<void, Deallocate> storage(
        ::operator new(type.Size(), align), Deallocate{align});
    build(storage.get());
    return {storage.release(), type};
  }

  // A copy of the value of `type` lying at `value`.
  [[nodiscard]] static auto CopyOf(const ValueType& type, const void* value)
      -> AnyValue;

  AnyValue(const AnyValue& other);
  auto operator=(const AnyValue& other) -> AnyValue&;
  AnyValue(AnyValue&&) noexcept = default;
  auto operator=(AnyValue&&) noexcept -> AnyValue& = default;
  ~AnyValue() = default;

  [[nodiscard]] auto Type() const -> const ValueType&;
  [[nodiscard]] auto Bytes() const -> const void* {
    return bytes_.get();
  }
  [[nodiscard]] auto Bytes() -> void* {
    return bytes_.get();
  }

  [[nodiscard]] auto IsBitIdentical(const AnyValue& other) const -> bool;

 private:
  // Frees storage a value was being built in, for a build that did not finish:
  // nothing in it is known to be whole, so nothing is ended.
  struct Deallocate {
    std::align_val_t align;
    void operator()(void* storage) const noexcept {
      ::operator delete(storage, align);
    }
  };

  struct End {
    const ValueType* type;
    void operator()(void* value) const noexcept {
      type->Destroy(value);
      Deallocate{std::align_val_t{type->Align()}}(value);
    }
  };

  AnyValue(void* value, const ValueType& type) : bytes_(value, End{&type}) {
  }

  std::unique_ptr<void, End> bytes_;
};

// Whether two types hold their values the same way: the same type, or two
// integral types of one shape, which generated code and the library may each
// state of their own.
[[nodiscard]] auto SameType(const ValueType& a, const ValueType& b) -> bool;

static_assert(LyraValue<AnyValue>);

}  // namespace lyra::value
