#include "lyra/value/string.hpp"

#include <cstdint>
#include <format>

namespace lyra::value {

void String::SetDecimal(std::int64_t i) {
  impl_ = std::format("{}", static_cast<std::int32_t>(i));
}

void String::SetHex(std::int64_t i) {
  impl_ = std::format("{:x}", static_cast<std::uint32_t>(i));
}

void String::SetOctal(std::int64_t i) {
  impl_ = std::format("{:o}", static_cast<std::uint32_t>(i));
}

void String::SetBinary(std::int64_t i) {
  impl_ = std::format("{:b}", static_cast<std::uint32_t>(i));
}

void String::Realtoa(const Real& r) {
  impl_ = std::format("{}", r.Value());
}

}  // namespace lyra::value
