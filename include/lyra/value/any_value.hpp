#pragma once

#include <memory>
#include <new>

#include "lyra/value/value_type.hpp"

namespace lyra::value {

// A value of a type the library was compiled without, in storage of its own,
// held with its type: what Rust's `Box<dyn Trait>` and Swift's existential
// container are. Everything it does it asks of that type.
class AnyValue {
 public:
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

  AnyValue(const AnyValue&) = delete;
  auto operator=(const AnyValue&) -> AnyValue& = delete;
  AnyValue(AnyValue&&) noexcept = default;
  auto operator=(AnyValue&&) noexcept -> AnyValue& = default;
  ~AnyValue() = default;

  [[nodiscard]] auto IsBitIdentical(const AnyValue& other) const -> bool {
    return Type().BitIdentical(bytes_.get(), other.bytes_.get());
  }

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

  [[nodiscard]] auto Type() const -> const ValueType& {
    return *bytes_.get_deleter().type;
  }

  std::unique_ptr<void, End> bytes_;
};

}  // namespace lyra::value
