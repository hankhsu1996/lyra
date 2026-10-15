#include "lyra/value/runtime_record.hpp"

#include <utility>

#include "lyra/value/any_value.hpp"
#include "lyra/value/laid_out_value.hpp"
#include "lyra/value/value_type.hpp"

namespace lyra::value {

RuntimeRecord::RuntimeRecord(AnyValue value)
    : LaidOutValue<RuntimeRecord>(std::move(value)) {
}

auto RuntimeRecord::CopyOf(const ValueType& type, const void* laid_out)
    -> RuntimeRecord {
  return RuntimeRecord(AnyValue::CopyOf(type, laid_out));
}

}  // namespace lyra::value
