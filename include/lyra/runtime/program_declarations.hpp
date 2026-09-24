#pragma once

#include <cstdint>
#include <span>

#include "lyra/runtime/class_definition.hpp"
#include "lyra/runtime/closure.hpp"
#include "lyra/runtime/scope_program.hpp"
#include "lyra/support/member_storage_kind.hpp"

namespace lyra::runtime {

// The storage a member needs, realized from what an artifact states about it.
// One arm per kind and no catch-all: a kind gained later fails to compile here
// until someone says what storage it asks for.
auto RealizeMemberStorage(support::DeclaredMemberStorage described)
    -> MemberStorageDescriptor;

// The description of one body's variables, built from what that body's own
// artifact states and kept for as long as anything can enter the body. What a
// body needs is settled before the program starts, so this is built where the
// artifact states it rather than where control first reaches the body.
auto DeclareVariableSchema(
    std::span<const support::DeclaredMemberStorage> described)
    -> const MemberStorageSchema*;

// The definition of one closure a unit declares: the body a call runs, and what
// its captures need, described the same way a body's variables are. Every value
// built from the closure shares it, so it is kept as long as anything can build
// one.
auto DeclareClosure(
    std::span<const support::DeclaredMemberStorage> captures, ClosureBody body)
    -> const ClosureDefinition*;

// The storage a unit shares program-wide, built from what that unit's artifact
// states about it and kept for as long as anything can reach it. What is built
// is what it holds rather than a description of it, so the address answered
// with is the storage itself -- which is what every reference to it names.
auto DeclareSharedStorage(support::DeclaredMemberStorage described) -> void*;

// One class a unit declares, and one whose values stand in the design
// hierarchy. Each answers with the definition every value of it carries, which
// is what the declaring unit leaves in the cell every reference loads. The
// definition is incomplete until the lineage is laid out, which is why nothing
// may be built from it before that.
auto DeclareClass() -> ObjectDefinition*;
auto DeclareScopeClass(
    std::int8_t time_unit_power, std::int8_t time_precision_power)
    -> ScopeDefinition*;

// What one class states it adds to its lineage. A class states nothing about
// what extends it, so the order these arrive in is the order the artifact
// states them and nothing else depends on it.
//
// A class this one reaches is named by the cell holding its definition, never
// by the definition itself. The artifacts are stated in whatever order the
// program was composed in, so a class of another artifact may not have been
// stated yet when this one names it; a cell has an address from the moment the
// program is composed, and what it holds is read once every artifact has
// spoken.
void DeclareBase(ObjectDefinition* cls, const ObjectDefinition* const* base);
void DeclareMembers(
    ObjectDefinition* cls,
    std::span<const support::DeclaredMemberStorage> members);
void DeclareIntroduction(ObjectDefinition* cls, ErasedMethodEntry body);
void DeclareTakeover(
    ObjectDefinition* cls, const ObjectDefinition* const* introduced_by,
    std::uint32_t ordinal, ErasedMethodEntry body);
void DeclarePropertyName(
    ObjectDefinition* cls, AbiStringRef name, std::uint32_t position);
void DeclareBehaviorName(
    ObjectDefinition* cls, AbiStringRef name, std::uint32_t position);
void DeclareBodyName(
    ObjectDefinition* cls, AbiStringRef name, ErasedMethodEntry body);

// What a scope class states beyond that: the entries the runtime drives an
// instance through and the one that builds it, and the names it answers from --
// a subroutine a hierarchical name spells (LRM 23.6), a DPI-C export's
// program-global C identifier (LRM 35.4), and a class it declares (LRM 23.9).
void DeclareScopeProgram(
    ScopeDefinition* scope, ScopeEntry resolve_state,
    ScopeEntry initialize_state, ScopeEntry create_processes,
    ScopeConstructEntry construct);
void DeclareSubroutineName(
    ScopeDefinition* scope, AbiStringRef name, ErasedScopeCallable entry);
void DeclareExportName(
    ScopeDefinition* scope, AbiStringRef name, ErasedScopeCallable entry);
void DeclareClassName(
    ScopeDefinition* scope, AbiStringRef name,
    const ObjectDefinition* const* declared);

// Lays out every class stated so far, each after the one it extends. This is
// the one step that reads the whole program, and it is where a cell naming
// another artifact's class is read -- so it runs once the program is composed
// and before anything is built from a definition.
void RealizeDeclarations();

}  // namespace lyra::runtime
