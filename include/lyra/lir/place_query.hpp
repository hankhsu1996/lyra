#pragma once

#include <optional>
#include <span>
#include <string_view>

#include "lyra/lir/compilation_unit.hpp"
#include "lyra/lir/function.hpp"
#include "lyra/lir/type_id.hpp"

namespace lyra::lir {

// The members a declaration holds, with the name it holds them under so a
// consumer that cannot use one can say whose it was. A member projection is
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
// the class the chain has reached.
auto PlaceType(
    const CompilationUnit& unit, const Function& fn, const Place& place)
    -> TypeId;

// Whether the operand names a local that is storage of its own. Such an operand
// already is the address the storage lives at; every other operand is a value,
// which reaches storage only through a dereference.
auto IsPlaceLocal(const Function& fn, const Operand& operand) -> bool;

}  // namespace lyra::lir
