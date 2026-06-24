#pragma once

#include <string>

namespace slang::ast {
class InstanceBodySymbol;
class InstanceSymbol;
}  // namespace slang::ast

namespace lyra::lowering::ast_to_hir {

// The compiled identity of a module specialization: the module definition name,
// plus a content hash of its resolved parameter bindings when the module is
// parameterized (LRM 6.20, 23.10). Instances whose bindings differ get
// distinct names; instances whose bindings match share slang's one canonical
// body and so get the same name; a non-parameterized module keeps its
// definition name. The producer (the unit naming itself) and every consumer
// (a parent naming a child it instantiates) compute the same name from the
// same canonical body, so a cross-unit reference matches by name with no
// shared table.
auto SpecializationName(const slang::ast::InstanceBodySymbol& body)
    -> std::string;

// Resolves the instance to its canonical body and names that specialization.
auto SpecializationName(const slang::ast::InstanceSymbol& inst) -> std::string;

}  // namespace lyra::lowering::ast_to_hir
