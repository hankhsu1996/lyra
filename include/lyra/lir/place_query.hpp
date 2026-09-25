#pragma once

#include <optional>
#include <span>
#include <string_view>

#include "lyra/lir/compilation_unit.hpp"
#include "lyra/lir/function.hpp"
#include "lyra/lir/type_id.hpp"

namespace lyra::lir {

// The members a declaration holds, with what a consumer that cannot use one
// can call it -- the declaration's own name, or which kind of declaration it is
// where the source gave it none. A member projection is
// meaningful only against the declaration its base names: an object holds the
// storage its instances own, whether this unit compiles the class or another
// unit published it, and a closure holds the captures its values own.
struct MemberList {
  std::span<const Member> members;
  std::string_view owner;
};

// The members the type names, absent for a type that declares none -- which is
// what makes a member projection over it invalid.
auto DeclaredMembers(const CompilationUnit& unit, TypeId type)
    -> std::optional<MemberList>;

// The type of the storage a place names. The base contributes the storage the
// chain starts from: a place local names its own storage, and any other base is
// a value, which names storage only once dereferenced. Each dereference names
// the storage behind what the chain has reached -- a reference's referent, or
// what a capability wrapper represents; each member step selects a member of
// the declaration it names, and that declaration is the whole of what the step
// is read against: which class the value turns out to be decides nothing about
// where a member sits (LRM 8.14), so the chain the value reached the
// declaration through is neither consulted nor carried.
auto PlaceType(
    const CompilationUnit& unit, const Function& fn, const Place& place)
    -> TypeId;

// Whether the operand names a local that is storage of its own. Such an operand
// already is the address the storage lives at; every other operand is a value,
// which reaches storage only through a dereference.
auto IsPlaceLocal(const Function& fn, const Operand& operand) -> bool;

// Whether an instruction brings a value into existence rather than naming one
// that already exists. A load names the value its place holds, where it lies; a
// cast reads a value as another type; and a library entry that answers with
// storage it was handed names that storage. Every call, build, update and
// operator makes a value of its own. Where that value's type is an owned one,
// what made it owes its end.
auto MakesValue(const InstrData& instr) -> bool;

// The same question of a call, which its target alone answers -- asked of a
// call that may depart as much as of one that returns.
auto CallMakesValue(const CallTarget& target) -> bool;

}  // namespace lyra::lir
