#pragma once

#include <string>

#include "lyra/mir/type_id.hpp"

namespace lyra::mir {

// Mirror of `hir::TypeAliasDecl`. Carried through MIR so backends can render
// SystemVerilog `typedef` declarations as backend-native aliases (e.g., C++
// `using <name> = <target>;`).
struct TypeAliasDecl {
  std::string name;
  TypeId target;
};

}  // namespace lyra::mir
