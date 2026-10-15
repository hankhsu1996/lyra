#pragma once

#include "lyra/value/any_value.hpp"
#include "lyra/value/concepts.hpp"
#include "lyra/value/integral.hpp"
#include "lyra/value/integral_words.hpp"
#include "lyra/value/laid_out_value.hpp"
#include "lyra/value/value_type.hpp"
#include "lyra/value/wide.hpp"

namespace lyra::value {

// A record as the runtime keeps one of its own: an unpacked structure (LRM
// 7.2), a union, tagged or untagged (LRM 7.3, 7.3.2), or a call's answer,
// laid out by the type the design is compiled with, which says which it is.
class RuntimeRecord : public LaidOutValue<RuntimeRecord> {
 public:
  // Holds no record: storage before whatever declares it installs one.
  RuntimeRecord() = default;

  // The record `value` holds, which is a value of a record type.
  explicit RuntimeRecord(AnyValue value);

  // A copy of the record of `type` laid out at `laid_out`.
  [[nodiscard]] static auto CopyOf(const ValueType& type, const void* laid_out)
      -> RuntimeRecord;
};

static_assert(LyraValue<RuntimeRecord>);
static_assert(NetResolvable<RuntimeRecord>);
static_assert(CaseEqualComparable<RuntimeRecord>);
static_assert(BitstreamSizable<RuntimeRecord, ConstIntegralView>);
static_assert(TakesBytesInPlace<RuntimeRecord>);

}  // namespace lyra::value
