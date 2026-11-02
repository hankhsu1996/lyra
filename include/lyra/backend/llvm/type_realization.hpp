#pragma once

#include <cstdint>
#include <optional>

#include "lyra/lir/compilation_unit.hpp"
#include "lyra/lir/type.hpp"
#include "lyra/lir/type_id.hpp"
#include "lyra/support/declared_member_storage.hpp"
#include "lyra/support/value_domain.hpp"
#include "lyra/value/integral.hpp"

namespace lyra::backend::llvm_backend {

// An integral type as the value layer states one.
[[nodiscard]] auto ShapeOf(const lir::IntegralType& integral)
    -> value::IntegralShape;

// The domain a LIR type is realized in, absent for a type whose values are not
// values of the design. It is the type's held object where that object is a
// domain's, so the entry a call names and the storage a cell owns read one
// classification.
auto ValueDomainOf(const lir::CompilationUnit& unit, lir::TypeId type)
    -> std::optional<support::ValueDomain>;

// What a member slot is for, which two declarations answer differently for a
// slot of the same type: a variable is written through its own store for as
// long as its owner lives, and a snapshot is filled once where its owner is
// built and only read afterwards.
enum class MemberSlotRole : std::uint8_t { kVariable, kSnapshot };

// What the members a declaration gives its values are for. A closure's are
// copies taken where it is built and only read after; every other
// declaration's are variables of the value that holds them.
auto MemberSlotRoleOf(const lir::TypeDeclaration& declaration)
    -> MemberSlotRole;

// The storage a member of `type` needs, with the value domain it holds where
// it holds values of one, or nothing where this backend has no realization for
// such a member. One arm per LIR type and no catch-all, because the kinds of
// storage differ in what a write has to do: a type gained later fails to
// compile here until someone says which storage it needs. Which type the
// domain is read from follows from the storage -- one wrapping a value names
// the type it wraps, one that is the value names the member's own type -- so
// this is the one place a declaration's type becomes what the runtime builds
// storage from.
auto DeclaredStorageOf(
    const lir::CompilationUnit& unit, lir::TypeId type, MemberSlotRole role)
    -> std::optional<support::DeclaredMemberStorage>;

}  // namespace lyra::backend::llvm_backend
