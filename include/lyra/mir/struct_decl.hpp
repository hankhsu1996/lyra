#pragma once

#include <string>
#include <vector>

#include "lyra/mir/callable_code.hpp"
#include "lyra/mir/type_declaration_ref.hpp"
#include "lyra/mir/type_id.hpp"
#include "lyra/support/value_operation.hpp"

namespace lyra::mir {

// One body a struct answers an operation on its whole value with, defined over
// its members -- two structs are equal when every pair of members is (LRM
// 11.4.5). It takes the parameters the operation takes of any value, the value
// it is asked of first where it is asked of one.
struct StructMethod {
  support::ValueOperation answers;
  CallableCode code;
};

// A struct this unit declares: the name the source declared it under (LRM
// 7.2), which is what another unit reaches it by, its members' types in
// declaration order, which an access reaches by position, and a method for
// every operation on a whole value its type has -- a real member leaves no case
// equality (LRM 11.4.5), a real or a chandle no bit stream (LRM 6.24.3), and a
// member not valid for a net nothing to resolve (LRM 6.7.1).
struct StructDecl {
  std::string name;
  std::vector<TypeId> elements;
  std::vector<StructMethod> methods;
};

// A struct another unit declares, as this unit reads it: the declaration it is
// and its members' types, which is what a value of it needs here.
struct ExternalStruct {
  TypeDeclarationRef declaration;
  std::vector<TypeId> elements;
};

}  // namespace lyra::mir
