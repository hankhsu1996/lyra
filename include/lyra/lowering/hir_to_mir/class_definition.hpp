#pragma once

#include <span>
#include <string>
#include <vector>

#include "lyra/mir/class.hpp"
#include "lyra/mir/class_id.hpp"
#include "lyra/mir/class_ref.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/expr_id.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/mir/type_id.hpp"

namespace lyra::lowering::hir_to_mir {

// The address of the definition every object of `of` carries, as a value of
// `block`: the constant the unit declaring the class emits, which a scope's
// constructor hands the part every scope shares.
auto BuildDefinitionRead(
    const mir::CompilationUnit& unit, mir::Block& block,
    const mir::DeclaredClassRef& of) -> mir::ExprId;

// A behavior an interface class introduced that a class extending it reaches
// without stating it (LRM 8.26.2): its name, the behavior, and what a body
// answering it takes after the object and completes with.
struct InheritedBehavior {
  std::string name;
  mir::VirtualSlot slot;
  std::vector<mir::TypeId> params;
  mir::TypeId result;
};

// Each function below states the definition of `cls` -- class `id` of `unit`,
// every body it declares complete -- as the constant its unit emits, adding to
// `cls` the constants and bodies that takes. Which one applies follows from how
// a referrer reaches the class.

// A class every referrer can name -- one a namespace unit declares, or what a
// unit promises of its object -- which answers the library nothing by name.
void StateNamedClassDefinition(
    const mir::CompilationUnit& unit, mir::ClassId id, mir::Class& cls);

// A class a referrer outside the design element declaring it reaches only by
// name (LRM 6.22, 23.9): the body answering where each of its fields is, and
// the body each name a handle of it can call runs -- its own methods, then each
// of `inherited` it does not state itself. For a behavior the object decides
// (LRM 8.20) that body makes the call on the object, viewing it as the class
// that introduced the behavior, so it runs what the object's own class answers.
// Each body is handed the part of the object `cls` is.
void StateNameReachedDefinition(
    const mir::CompilationUnit& unit, mir::ClassId id, mir::Class& cls,
    std::span<const InheritedBehavior> inherited);

// A class an instance of the design hierarchy is built of: its timescale, the
// body each subroutine name it declares reaches (LRM 23.6), `exports`, the
// names it answers a foreign caller with (LRM 35.5.3), and the classes it
// declares. Each body is handed the scope.
void StateScopeDefinition(
    const mir::CompilationUnit& unit, mir::ClassId id, mir::Class& cls,
    std::span<const mir::NamedCallable> exports);

}  // namespace lyra::lowering::hir_to_mir
