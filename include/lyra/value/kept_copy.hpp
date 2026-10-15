#pragma once

#include <array>
#include <cstddef>

#include "lyra/value/any_value.hpp"
#include "lyra/value/value_type.hpp"

namespace lyra::value {

// A copy of a value of a type the library is handed, kept aside while the
// library works: in a room of its own where it fits there, and in storage of
// its own past that, so a small value costs no allocation and a large one
// never lies in a frame. It holds the copy from the moment it is built and is
// neither copied nor moved.
class KeptCopy {
 public:
  KeptCopy(const ValueType& type, const void* value) : type_(&type) {
    if (KeptHere()) {
      type.Copy(value, here_.data());
    } else {
      apart_ = AnyValue::CopyOf(type, value);
    }
  }
  KeptCopy(const KeptCopy&) = delete;
  auto operator=(const KeptCopy&) -> KeptCopy& = delete;
  KeptCopy(KeptCopy&&) = delete;
  auto operator=(KeptCopy&&) -> KeptCopy& = delete;
  ~KeptCopy() {
    if (KeptHere()) {
      type_->Destroy(here_.data());
    }
  }

  [[nodiscard]] auto Bytes() const -> const void* {
    return KeptHere() ? static_cast<const void*>(here_.data()) : apart_.Bytes();
  }
  [[nodiscard]] auto Bytes() -> void* {
    return KeptHere() ? static_cast<void*>(here_.data()) : apart_.Bytes();
  }

 private:
  static constexpr std::size_t kRoom = 128;

  [[nodiscard]] auto KeptHere() const -> bool {
    return type_->Size() <= kRoom && type_->Align() <= alignof(void*);
  }

  const ValueType* type_;
  alignas(void*) std::array<std::byte, kRoom> here_{};
  AnyValue apart_;
};

}  // namespace lyra::value
