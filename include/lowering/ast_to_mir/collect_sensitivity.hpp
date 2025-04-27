#pragma once

#include <vector>

#include <slang/ast/Symbol.h>

namespace slang::ast {
class Statement;
}

namespace lyra::lowering {

// Collects all named signals (identifiers) that appear on the RHS
// of the given statement's expressions.
auto CollectSensitivityList(const slang::ast::Statement& statement)
    -> std::vector<const slang::ast::Symbol*>;

}  // namespace lyra::lowering
