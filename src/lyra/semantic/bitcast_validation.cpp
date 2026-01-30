#include "lyra/semantic/bitcast_validation.hpp"

#include <cstdint>
#include <format>
#include <optional>
#include <string>

#include "lyra/common/type.hpp"
#include "lyra/common/type_queries.hpp"

namespace lyra::semantic {

namespace {

// Check if type is a real type (64-bit float)
auto IsRealType(const Type& type) -> bool {
  return type.Kind() == TypeKind::kReal;
}

// Check if type is a shortreal type (32-bit float)
auto IsShortRealType(const Type& type) -> bool {
  return type.Kind() == TypeKind::kShortReal;
}

// Check if type is a valid packed integral for bitcast
// Accepts kIntegral or kPackedArray (not kPackedStruct)
auto IsPackedIntegral(const Type& type) -> bool {
  return type.Kind() == TypeKind::kIntegral ||
         type.Kind() == TypeKind::kPackedArray;
}

// Validate packed integral side of bitcast
// Accepts kIntegral or kPackedArray (2-state only, exact width)
// Returns error message or nullopt if valid
auto ValidatePackedIntegralSide(
    const Type& type, uint32_t required_width, const char* context,
    const TypeArena& arena) -> std::optional<std::string> {
  if (!IsPackedIntegral(type)) {
    return std::format(
        "bitcast {}: requires integral type (bit), got {}", context,
        ToString(type));
  }

  if (IsPackedFourState(type, arena)) {
    return std::format(
        "bitcast {}: requires 2-state integral (bit), got 4-state (logic)",
        context);
  }

  uint32_t actual_width = PackedBitWidth(type, arena);
  if (actual_width != required_width) {
    return std::format(
        "bitcast {}: requires {}-bit integral, got {} bits", context,
        required_width, actual_width);
  }

  // Signedness is allowed (ignored for bit reinterpretation)
  return std::nullopt;
}

}  // namespace

auto ValidateBitCast(const Type& src, const Type& dst, const TypeArena& arena)
    -> std::optional<std::string> {
  // Case 1: real -> packed integral (realtobits)
  if (IsRealType(src) && IsPackedIntegral(dst)) {
    return ValidatePackedIntegralSide(dst, 64, "real -> integral", arena);
  }

  // Case 2: packed integral -> real (bitstoreal)
  if (IsPackedIntegral(src) && IsRealType(dst)) {
    return ValidatePackedIntegralSide(src, 64, "integral -> real", arena);
  }

  // Case 3: shortreal -> packed integral (shortrealtobits)
  if (IsShortRealType(src) && IsPackedIntegral(dst)) {
    return ValidatePackedIntegralSide(dst, 32, "shortreal -> integral", arena);
  }

  // Case 4: packed integral -> shortreal (bitstoshortreal)
  if (IsPackedIntegral(src) && IsShortRealType(dst)) {
    return ValidatePackedIntegralSide(src, 32, "integral -> shortreal", arena);
  }

  // Invalid pair - not a supported bitcast
  return std::format(
      "bitcast not supported: {} -> {}", ToString(src), ToString(dst));
}

}  // namespace lyra::semantic
