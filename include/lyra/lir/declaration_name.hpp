#pragma once

#include <optional>
#include <string>
#include <string_view>

#include "lyra/lir/compilation_unit.hpp"
#include "lyra/lir/type_id.hpp"

namespace lyra::lir {

// The name the declaration `type` names is linked under, program-wide, or
// nothing where the type names no declaration a value is built from. A
// declaration this unit compiles carries the name it was emitted under; one
// another unit declares is composed from the unit and the name a signature
// gave, the same way that unit composed it -- which is what lets the two agree
// with no shared table.
auto DeclarationName(const CompilationUnit& unit, TypeId type)
    -> std::optional<std::string>;

// The name a class is linked under, from the unit that declares it and the name
// that unit gave it. A class's name is unique only inside its unit while the
// whole program links into one name space, so the unit qualifies it. The unit
// that emits the class and every unit that reaches one compose it from the same
// two names, which is what lets them agree with no shared table -- and they
// compose it here, so there is one statement of how.
auto ClassLinkageName(std::string_view unit_name, std::string_view class_name)
    -> std::string;

// The symbol a class's constructor is emitted and called under, given the name
// the class itself is linked under. The word qualifying it is minted here
// rather than written by the source, so the unit that emits the body and the
// unit that enters it both take it from here and cannot drift apart.
auto ConstructorSymbolName(std::string_view declaration_name) -> std::string;

}  // namespace lyra::lir
