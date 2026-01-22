#pragma once

#include <cstdint>
#include <cstring>
#include <string>
#include <variant>
#include <vector>

#include "lyra/common/integral_constant.hpp"
#include "lyra/common/type.hpp"

namespace lyra {

struct ConstId {
  uint32_t value = 0;

  auto operator==(const ConstId&) const -> bool = default;
  auto operator<=>(const ConstId&) const = default;
  explicit operator bool() const {
    return value != UINT32_MAX;
  }

  template <typename H>
  friend auto AbslHashValue(H h, ConstId id) -> H {
    return H::combine(std::move(h), id.value);
  }
};

constexpr ConstId kInvalidConstId{UINT32_MAX};

struct StringConstant {
  std::string value;

  auto operator==(const StringConstant&) const -> bool = default;

  template <typename H>
  friend auto AbslHashValue(H h, const StringConstant& c) -> H {
    return H::combine(std::move(h), c.value);
  }
};

struct RealConstant {
  double value;

  auto operator==(const RealConstant&) const -> bool = default;

  template <typename H>
  friend auto AbslHashValue(H h, const RealConstant& c) -> H {
    // Hash the bit representation to avoid NaN/signed-zero issues
    uint64_t bits = 0;
    std::memcpy(&bits, &c.value, sizeof(bits));
    return H::combine(std::move(h), bits);
  }
};

struct StructConstant {
  std::vector<ConstId> fields;

  auto operator==(const StructConstant&) const -> bool = default;

  template <typename H>
  friend auto AbslHashValue(H h, const StructConstant& c) -> H {
    return H::combine(std::move(h), c.fields);
  }
};

struct ArrayConstant {
  std::vector<ConstId> elements;

  auto operator==(const ArrayConstant&) const -> bool = default;

  template <typename H>
  friend auto AbslHashValue(H h, const ArrayConstant& c) -> H {
    return H::combine(std::move(h), c.elements);
  }
};

using ConstantValue = std::variant<
    IntegralConstant, StringConstant, RealConstant, StructConstant,
    ArrayConstant>;

struct Constant {
  TypeId type;
  ConstantValue value;

  auto operator==(const Constant&) const -> bool = default;

  template <typename H>
  friend auto AbslHashValue(H h, const Constant& c) -> H {
    return H::combine(std::move(h), c.type, c.value);
  }
};

}  // namespace lyra
