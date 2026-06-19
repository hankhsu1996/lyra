#include "lyra/value/string.hpp"

#include <cstdint>
#include <format>

namespace lyra::value {

void String::Itoa(const PackedArray& i) {
  impl_ = std::format("{}", static_cast<std::int32_t>(i.ToInt64()));
}

void String::Hextoa(const PackedArray& i) {
  impl_ = std::format("{:x}", static_cast<std::uint32_t>(i.ToInt64()));
}

void String::Octtoa(const PackedArray& i) {
  impl_ = std::format("{:o}", static_cast<std::uint32_t>(i.ToInt64()));
}

void String::Bintoa(const PackedArray& i) {
  impl_ = std::format("{:b}", static_cast<std::uint32_t>(i.ToInt64()));
}

void String::Realtoa(double r) {
  impl_ = std::format("{}", r);
}

}  // namespace lyra::value
