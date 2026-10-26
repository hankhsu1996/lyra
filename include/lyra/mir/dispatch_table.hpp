#pragma once

#include <optional>
#include <string_view>
#include <variant>
#include <vector>

#include "lyra/mir/class_id.hpp"
#include "lyra/mir/class_ref.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/support/runtime_class.hpp"

namespace lyra::mir {

// The behaviors an object of the class `cls` dispatches on (LRM 8.20), in the
// order its definition holds their bodies: those of the class it extends first,
// in that class's order, then those it introduces, in the order it declares
// them. A class extending another therefore holds each of the other's behaviors
// at the position the other does, which is what lets a call name a position
// knowing only the class its handle is of -- the order clang's `VTableBuilder`
// and rustc's `vtable_entries` give a primary table.
[[nodiscard]] auto DispatchTableOf(const CompilationUnit& unit, ClassId cls)
    -> std::vector<VirtualSlot>;

// The body an object of `cls` runs for `behavior`: the one the nearest class of
// its lineage gives, starting at `cls` itself, or none where that class leaves
// the behavior abstract (LRM 8.21).
[[nodiscard]] auto DispatchedBody(
    const CompilationUnit& unit, ClassId cls, const VirtualSlot& behavior)
    -> std::optional<CallableTarget>;

// The body the nearest class of the lineage of `cls`, starting at `cls`
// itself, answers the library's `callback` with, or none where no class
// answers it.
[[nodiscard]] auto CallbackBodyOf(
    const CompilationUnit& unit, ClassId cls, support::LibraryCallback callback)
    -> std::optional<CallableTarget>;

// Every interface class an object of `cls` is also an object of (LRM 8.26),
// each once however many ways it is reached (LRM 8.26.6.3): those of the class
// it extends first, then each the class names in the order it names them, each
// followed by those it extends.
[[nodiscard]] auto InterfacesOf(const CompilationUnit& unit, ClassId cls)
    -> std::vector<ClassId>;

// The behaviors the interface class `iface` introduces itself, in the order it
// declares them: what one row of an implementing class's definition holds a
// body for each of.
[[nodiscard]] auto RequirementsOf(const CompilationUnit& unit, ClassId iface)
    -> std::vector<VirtualSlot>;

// The name a method taking part in dispatch, or answering what the runtime
// library calls on an object, is reached by in every unit naming it: the name
// the source gave the method introducing its behavior, or the library callback
// it answers. Every body of a behavior is named alike, so each class's table
// names one behavior by one name.
using DispatchName = std::variant<std::string_view, support::LibraryCallback>;

[[nodiscard]] auto DispatchNameOf(
    const CompilationUnit& unit, const Class& cls, CallableId method)
    -> DispatchName;

}  // namespace lyra::mir
