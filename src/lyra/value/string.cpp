#include "lyra/value/string.hpp"

#include <cstdint>
#include <format>

namespace lyra::value {

void String::Itoa(std::int32_t i) {
  impl_ = std::format("{}", i);
}

void String::Hextoa(std::int32_t i) {
  impl_ = std::format("{:x}", static_cast<std::uint32_t>(i));
}

void String::Octtoa(std::int32_t i) {
  impl_ = std::format("{:o}", static_cast<std::uint32_t>(i));
}

void String::Bintoa(std::int32_t i) {
  impl_ = std::format("{:b}", static_cast<std::uint32_t>(i));
}

void String::Realtoa(double r) {
  impl_ = std::format("{}", r);
}

}  // namespace lyra::value
