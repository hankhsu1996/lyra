#include "lyra/value/dpi_canonical.hpp"

#include <cstdint>

#include "lyra/value/integral_words.hpp"

namespace lyra::value {

DpiBitBuffer::DpiBitBuffer(ConstPlanes sv, std::uint64_t width)
    : groups_(CanonicalGroups(width)) {
  WriteCanonicalBitVec(groups_.data(), sv, width);
}

auto DpiBitBuffer::Data() -> svBitVecVal* {
  return groups_.data();
}

DpiLogicBuffer::DpiLogicBuffer(ConstPlanes sv, std::uint64_t width)
    : groups_(CanonicalGroups(width)) {
  WriteCanonicalLogicVec(groups_.data(), sv, width);
}

auto DpiLogicBuffer::Data() -> svLogicVecVal* {
  return groups_.data();
}

}  // namespace lyra::value
