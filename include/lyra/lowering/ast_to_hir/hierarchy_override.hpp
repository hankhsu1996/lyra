#pragma once

// What was written elsewhere about an instance -- a defparam (LRM 23.10.1), a
// bind directive (LRM 23.11), a configuration rule (LRM 33.4), or an override
// on the command line -- stated as what it did to the instance. The front end
// settles these by elaborating more than once and carries its conclusions into
// the final elaboration keyed by syntax, the one thing its elaborations share;
// this is the one place that account is read.

#include <string>
#include <variant>
#include <vector>

namespace slang::ast {
class Expression;
class InstanceBodySymbol;
class InstanceSymbol;
class ParameterSymbol;
class Symbol;
}  // namespace slang::ast

namespace lyra::lowering::ast_to_hir {

// A parameter of the instance whose value was given somewhere other than its
// instantiation.
struct ParameterGivenElsewhere {
  const slang::ast::Symbol* parameter;
};

// The instance is one a bind directive inserted. The directive is named by the
// declaration holding it and its position among that declaration's binds, the
// way a declaration with no name of its own is told apart by where it sits.
// Two binds in one declaration may insert one instance name into different
// targets, connected apart (LRM 23.11), so the instance's own name is not
// enough, and a position survives an edit anywhere else.
struct InsertedByBind {
  std::string directive;
};

// A configuration rule chose which cell the instance is (LRM 33.4.1.6).
struct CellChosenByConfiguration {
  std::string cell;
};

using OverrideEffect = std::variant<
    ParameterGivenElsewhere, InsertedByBind, CellChosenByConfiguration>;

// What was written elsewhere about `inst` itself, in an order that is the same
// for every instance written alike.
[[nodiscard]] auto OverridesOn(const slang::ast::InstanceSymbol& inst)
    -> std::vector<OverrideEffect>;

// Whether something written elsewhere can reach an instance below `inst`: only
// a body an override reached, or one a configuration elaborated, holds one.
[[nodiscard]] auto OverridesMayReachBelow(
    const slang::ast::InstanceSymbol& inst) -> bool;

// Whether `param` of `body` holds a value given somewhere other than the
// instantiation. Such a value takes precedence over the instantiation's own
// (LRM 23.10, 33.4.3), and only the elaborated value states it, since the
// expression that gave it belongs to another scope.
[[nodiscard]] auto ValueSetElsewhere(
    const slang::ast::InstanceBodySymbol& body,
    const slang::ast::ParameterSymbol& param) -> bool;

// The expression `inst`'s own instantiation wrote for `param`, where that is
// what the parameter holds; nothing where it was given elsewhere or not at all.
[[nodiscard]] auto ValueWrittenAtInstantiation(
    const slang::ast::InstanceSymbol& inst,
    const slang::ast::ParameterSymbol& param) -> const slang::ast::Expression*;

}  // namespace lyra::lowering::ast_to_hir
