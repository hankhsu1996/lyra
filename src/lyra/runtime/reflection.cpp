#include "lyra/runtime/reflection.hpp"

#include "lyra/value/real.hpp"

namespace lyra::runtime {

void ValueSerializer::Real(const value::Real& value) {
  SerializeReal(value.Value());
}

void ValueSerializer::Real(const value::ShortReal& value) {
  SerializeReal(static_cast<double>(value.Value()));
}

}  // namespace lyra::runtime
