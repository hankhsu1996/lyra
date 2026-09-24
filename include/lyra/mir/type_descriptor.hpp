#pragma once

#include <optional>

#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr_id.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/mir/type_descriptor_id.hpp"
#include "lyra/mir/type_descriptor_pool.hpp"
#include "lyra/mir/type_id.hpp"
#include "lyra/mir/value_build.hpp"

namespace lyra::mir {

// What `type`'s declaration says that an operation on a value of it needs, and
// nothing where it says nothing. This is the one place the families are told
// apart: everything below answers by visiting what this returns, so a family
// admitted later fails to build until every answer says what it means for it.
[[nodiscard]] auto DescriptionOf(const CompilationUnit& unit, TypeId type)
    -> std::optional<TypeDescription>;

// Whether `type` has a run-time description at all, for a caller that passes
// one where there is one and nothing where there is not.
[[nodiscard]] auto HasTypeDescriptor(const CompilationUnit& unit, TypeId type)
    -> bool;

// What an operation on a value needs from that value's declaration, stated as
// an operand so it reaches the runtime through the argument list rather than
// being composed by a backend out of type payload. A select uses it to say what
// coordinate system the receiver was declared with, which the value itself
// cannot supply -- a slice of an array carries no declared range, and an
// aggregate's flat base carries no dimension stack.
//
// Naming one is what puts it in the unit, so a caller asks for the description
// of a type whose declaration says something; one whose declaration says
// nothing is a caller that had no operand to pass.
[[nodiscard]] auto BuildTypeDescriptorRef(
    const CompilationUnit& unit, Block& block, TypeId described) -> ExprId;

// The members `enumeration` declares, stated as an operand in the same way.
// This is a second description of the same type: what a value operation takes
// is the base's, and what the questions LRM 6.19.5 and 6.24.2 ask of a value
// are answered against is this one.
[[nodiscard]] auto BuildEnumerationDescriptorRef(
    const CompilationUnit& unit, Block& block, TypeId enumeration) -> ExprId;

// The value type behind `type`, with the indirections a place reaches storage
// through -- a capability wrapper, a pointer -- unwrapped. A declared fact
// belongs to the value type, and a caller asking for one has a place as often
// as a value. Which wrappers exist is asked of the type system rather than
// restated here, so a newly admitted one cannot silently lose the fact.
[[nodiscard]] auto ValueTypeOf(const CompilationUnit& unit, TypeId type)
    -> TypeId;

// The runtime type one description is a value of, which is also what tells two
// descriptions apart at a use.
[[nodiscard]] auto TypeDescriptorTypeOf(
    const CompilationUnit& unit, TypeDescriptorId descriptor) -> TypeId;

// How the description `descriptor` names is built, as the expression that
// builds it. A description is a function of what it says alone: it reads no
// body, and it names no description of its own, so asking for one adds nothing
// to the unit and the set can be walked while the answers are taken.
[[nodiscard]] auto DescribeType(
    const CompilationUnit& unit, TypeDescriptorId descriptor) -> ValueBuild;

}  // namespace lyra::mir
