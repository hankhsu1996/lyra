#pragma once

#include <string>
#include <variant>

#include "lyra/lir/class_id.hpp"
#include "lyra/lir/constant_id.hpp"
#include "lyra/lir/function_id.hpp"
#include "lyra/lir/type_id.hpp"

namespace lyra::lir {

// A function of this unit.
struct FunctionReferent {
  FunctionId function;
};

// A function another unit emits, linked under `symbol`: an entry of that
// unit's that answers to no name, declared at `prototype`, the type that unit
// published it with.
struct ExternalFunctionReferent {
  std::string symbol;
  TypeId prototype;
};

// A constant of this unit.
struct ConstantReferent {
  ConstantId constant;
};

// The definition every object of the class `cls` carries, which the unit
// declaring the class emits.
struct DefinitionReferent {
  ClassId cls;
};

// Storage no instance owns, linked under `symbol`, which the unit declaring it
// defines: a variable the whole program shares (LRM 26.2, 3.12.1) or one a
// class shares among its objects (LRM 8.9).
struct SharedStorageReferent {
  std::string symbol;
};

// What ends an object of the class `cls`, of this unit or another: each
// member, then the storage the object was created in.
struct DestructorReferent {
  ClassId cls;
};

// The table an object of the class `cls`, of this unit, lays the bodies of
// its behaviors out in, laid out by the target from what the class states of
// its dispatch.
struct DispatchTableReferent {
  ClassId cls;
};

// The table an object of the class `cls` gives the interface `iface`.
struct InterfaceTableReferent {
  ClassId cls;
  ClassId iface;
};

// A NUL-terminated text this unit holds.
struct TextReferent {
  std::string text;
};

// What an address the program holds before it runs names, wherever the
// address stands -- an operand, the base of a place, a part of a constant, a
// slot of a table -- as rustc's `GlobalAlloc` and clang's `LValueBase` name the
// one set an operand and a constant's pointer both point into.
using Referent = std::variant<
    FunctionReferent, ExternalFunctionReferent, ConstantReferent,
    DefinitionReferent, SharedStorageReferent, DestructorReferent,
    DispatchTableReferent, InterfaceTableReferent, TextReferent>;

// The body a slot of a table holds: code of this unit or of another.
using TableBody = std::variant<FunctionReferent, ExternalFunctionReferent>;

// The referent the body `body` is.
[[nodiscard]] inline auto ReferentOf(const TableBody& body) -> Referent {
  return std::visit([](const auto& code) -> Referent { return code; }, body);
}

}  // namespace lyra::lir
