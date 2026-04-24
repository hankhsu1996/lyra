#pragma once

#include <variant>

#include "lyra/mir/handle.hpp"
#include "lyra/mir/operand.hpp"

namespace lyra::mir {

struct AssocGet {
  PlaceId dest;
  Operand key;
};
struct AssocSet {
  Operand key;
  Operand value;
};
struct AssocExists {
  PlaceId dest;
  Operand key;
};
struct AssocDelete {};
struct AssocDeleteKey {
  Operand key;
};
struct AssocNum {
  PlaceId dest;
};
struct AssocIterFirst {
  PlaceId dest_found;
  PlaceId out_key;
};
struct AssocIterLast {
  PlaceId dest_found;
  PlaceId out_key;
};
struct AssocIterNext {
  PlaceId dest_found;
  PlaceId key_place;
};
struct AssocIterPrev {
  PlaceId dest_found;
  PlaceId key_place;
};
struct AssocSnapshot {
  PlaceId dest_keys;
};

using AssocOpData = std::variant<
    AssocGet, AssocSet, AssocExists, AssocDelete, AssocDeleteKey, AssocNum,
    AssocIterFirst, AssocIterLast, AssocIterNext, AssocIterPrev, AssocSnapshot>;

struct AssocOp {
  PlaceId receiver;
  AssocOpData data;
};

}  // namespace lyra::mir
