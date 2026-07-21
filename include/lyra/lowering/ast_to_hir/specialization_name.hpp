#pragma once

#include <string>

namespace slang::ast {
class ClassType;
class InstanceBodySymbol;
class InstanceSymbol;
class Symbol;
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

// The compiled identity of a SystemVerilog class specialization (LRM 8.25):
// the generic class name plus a content hash of its resolved parameter
// bindings when the class is parameterized, or the bare class name when it
// is not. Two specializations of one generic class denote the same type iff
// every value binding is equal and every type binding is a matching type
// (LRM 8.25 uniqueness rule); slang deduplicates on that rule, so distinct
// bindings arrive as distinct ClassType instances and produce distinct
// names. Bare `C` and empty-override `C #()` resolve to the same slang
// ClassType and thus name the same specialization.
auto SpecializationName(const slang::ast::ClassType& cls) -> std::string;

// The name a compilation unit publishes for itself, so a consumer reaching one
// of its members by name and the unit emitting that member agree with no shared
// table (LRM 26.3). A package publishes its declared name; a module body its
// specialization name. An anonymous compilation-unit scope (the LRM 3.12.1
// `$unit` file-set scope, modeled as a namespace unit with no source name)
// publishes a name derived from its own source-input identity: the only
// property distinguishing two such scopes is which compilation-unit input they
// belong to, which the LRM uses to define the scope boundary itself. Both the
// producer and every consumer compute this from the same slang unit symbol.
auto CompilationUnitName(const slang::ast::Symbol& unit) -> std::string;

}  // namespace lyra::lowering::ast_to_hir
