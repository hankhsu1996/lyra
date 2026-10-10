#include "lyra/value/dpi_canonical.hpp"

#include <cstdint>

#include "lyra/value/integral_words.hpp"

namespace lyra::value {

DpiBitBuffer::DpiBitBuffer(const ConstIntegralView& sv)
    : groups_(CanonicalGroups(sv.width)) {
  WriteCanonicalBitVec(groups_.data(), sv);
}

auto DpiBitBuffer::Data() -> svBitVecVal* {
  return groups_.data();
}

DpiLogicBuffer::DpiLogicBuffer(const ConstIntegralView& sv)
    : groups_(CanonicalGroups(sv.width)) {
  WriteCanonicalLogicVec(groups_.data(), sv);
}

auto DpiLogicBuffer::Data() -> svLogicVecVal* {
  return groups_.data();
}

}  // namespace lyra::value
