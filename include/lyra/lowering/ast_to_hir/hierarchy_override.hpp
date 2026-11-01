#pragma once

// What was written elsewhere about an instance's parameters -- a defparam (LRM
// 23.10.1), a configuration rule (LRM 33.4.3), or an override on the command
// line -- stated as what it did to each parameter. The front end settles these
// by elaborating more than once and carries its conclusions into the final
// elaboration keyed by syntax, the one thing its elaborations share; this is
// the one place that account is read.

namespace slang::ast {
class Expression;
class InstanceBodySymbol;
class InstanceSymbol;
class ParameterSymbol;
}  // namespace slang::ast

namespace lyra::lowering::ast_to_hir {

// Whether `param` of `body` holds a value given somewhere other than the
// instantiation. Such a value takes precedence over the instantiation's own
// (LRM 23.10, 33.4.3), and only the elaborated value states it, since the
// expression that gave it belongs to another scope.
[[nodiscard]] auto ValueSetElsewhere(
    const slang::ast::InstanceBodySymbol& body,
    const slang::ast::ParameterSymbol& param) -> bool;

// Whether `param` of `body` holds a value given anywhere other than its own
// declaration: by the instantiation, a defparam, or a configuration (LRM
// 23.10, 33.4.3).
[[nodiscard]] auto GivenAValue(
    const slang::ast::InstanceBodySymbol& body,
    const slang::ast::ParameterSymbol& param) -> bool;

// The expression `inst`'s own instantiation wrote for `param`, where that is
// what the parameter holds; nothing where it was given elsewhere or not at all.
[[nodiscard]] auto ValueWrittenAtInstantiation(
    const slang::ast::InstanceSymbol& inst,
    const slang::ast::ParameterSymbol& param) -> const slang::ast::Expression*;

}  // namespace lyra::lowering::ast_to_hir
