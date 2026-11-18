#pragma once

#include <cstdint>
#include <string>
#include <string_view>

#include "lyra/support/port_direction.hpp"

namespace lyra::support {

// What a declared type is (LRM 37.25): the predefined type it names, or the
// kind of the type it declares -- a vector of single bits being its bit or
// logic type, a packed array of structures, unions or enumerations a packed
// array (LRM 37.17, 37.18), and an unpacked array one of the four kinds of
// array LRM 7.4 to 7.10 declare. The front end, which reads the declaration,
// states it, and every reader that reaches the design by name reads it, so it
// lives beside both.
enum class TypespecKind : std::uint8_t {
  kBit,
  kLogic,
  kInt,
  kShortInt,
  kLongInt,
  kByte,
  kInteger,
  kTime,
  kReal,
  kShortReal,
  kRealtime,
  kEnum,
  kPackedStruct,
  kPackedUnion,
  kPackedArray,
  kEvent,
  kString,
  kChandle,
  kClass,
  kVirtualInterface,
  kUnpackedStruct,
  kUnpackedUnion,
  kFixedArray,
  kDynamicArray,
  kAssociativeArray,
  kQueue,
};

// The range a declared type states (LRM 37.25): its left and right bounds as
// the source wrote them.
struct Range {
  std::int64_t left;
  std::int64_t right;

  auto operator==(const Range&) const -> bool = default;
};

// The net type of a net data object (LRM 6.6, Table 6-1): it fixes how the
// net's contributions resolve and what the net shows where nothing drives it.
// `wire` and `tri` are one net type under two spellings, as are `wand` and
// `triand`, and `wor` and `trior` (LRM 6.6.1, 6.6.3). The source spelling is
// kept, since a reader by name reports it, and what it states is derived where
// the net type is translated.
enum class NetType : std::uint8_t {
  kWire,
  kTri,
  kWand,
  kTriand,
  kWor,
  kTrior,
  kTri0,
  kTri1,
  kSupply0,
  kSupply1,
  kUwire,
  kTrireg,
};

// The keyword the source declares a net of `net_type` by.
[[nodiscard]] auto NetTypeKeyword(NetType net_type) -> std::string_view;

// A port of a scope connecting a data object the scope declares (LRM
// 23.2.2): the port's name, the direction data flows across it, and its
// position among the scope's ports in the order the declaration gives them,
// a port written as a concatenation being one position for each part (LRM
// 21.7.4.2). Two ports may connect one object, and each is a port of its own.
struct DescribedPort {
  std::string name;
  PortDirection direction;
  std::uint32_t position;

  auto operator==(const DescribedPort&) const -> bool = default;
};

}  // namespace lyra::support
