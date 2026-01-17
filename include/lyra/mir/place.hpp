#pragma once

#include <variant>
#include <vector>

#include "lyra/common/type.hpp"
#include "lyra/mir/operand.hpp"

namespace lyra::mir {

struct PlaceRoot {
  enum class Kind {
    kLocal,   // function/process local storage (vars, params)
    kTemp,    // compiler-generated local storage
    kDesign,  // design/runtime storage (nets, ports, hierarchy)
  };

  Kind kind;
  int id;       // opaque handle to storage table
  TypeId type;  // type of the value stored at this root
};

struct Projection {
  enum class Kind {
    kField,
    kIndex,
    kSlice,
    kDeref,
  };

  Kind kind;
  std::variant<int, Operand> operand;
};

struct Place {
  PlaceRoot root;
  std::vector<Projection> projections;
};

}  // namespace lyra::mir
