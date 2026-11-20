#pragma once

#include <string>
#include <vector>

#include "lyra/backend/cpp/scope_view.hpp"
#include "lyra/backend/cpp/target_text.hpp"
#include "lyra/backend/cpp/type_operation_bodies.hpp"
#include "lyra/mir/code_source.hpp"
#include "lyra/mir/compilation_unit.hpp"

namespace lyra::backend::cpp {

// What a type MIR builds operations for is named by: the name each of its own
// functions starts with and, for a type implementing the library's traits,
// the struct holding that implementation, the header declaring it and the code
// file defining its items, each named from the type's name as MIR states it, so
// every unit naming the type writes the same files under the same names and
// two types are never one type.
[[nodiscard]] auto TypeSymbolName(
    const mir::CompilationUnit& unit, mir::TypeId type) -> std::string;
[[nodiscard]] auto ImplStructName(
    const mir::CompilationUnit& unit, mir::TypeId type) -> std::string;
[[nodiscard]] auto ImplHeaderFile(
    const mir::CompilationUnit& unit, mir::TypeId type) -> std::string;
[[nodiscard]] auto ImplCodeFile(
    const mir::CompilationUnit& unit, mir::TypeId type) -> std::string;

// Whether the body answering `source` is an item of its type's implementation
// of the library's traits, which the struct holding that implementation
// declares; and whether it is a free function the code file calling it
// defines: any other operation MIR builds for a type -- one over a
// container's core, a fixed-size array's or a tuple's, a passage of an array
// of any kind between its type and the form of it with no queue bounded.
[[nodiscard]] auto IsImplItem(
    const mir::CompilationUnit& unit, const mir::TypeOperationSource& source)
    -> bool;
[[nodiscard]] auto IsNonMemberBody(
    const mir::CompilationUnit& unit, const mir::TypeOperationSource& source)
    -> bool;

// The free function answering `source`, named by its type's name, the
// operation, and the types it is instantiated at.
[[nodiscard]] auto TypeFunctionName(
    const mir::CompilationUnit& unit, const mir::TypeOperationSource& source)
    -> std::string;

// One file the operations of a type are written in: where, and its text.
struct TypeOperationFileText {
  std::string relpath;
  TargetText text;
};

// What the unit writes of the operations MIR builds for its types: the header
// declaring each type's implementation of the library's traits; the code file
// defining its
// items and the free functions they call; and, for the unit's code file, its
// own free functions declared ahead of every body, then defined after them.
struct TypeOperationTexts {
  std::vector<TypeOperationFileText> headers;
  std::vector<TypeOperationFileText> code_files;
  TargetText declarations;
  TargetText definitions;
};

[[nodiscard]] auto RenderTypeOperations(
    const mir::CompilationUnit& unit, const TypeOperationBodies& bodies,
    UnitRenderReport& report) -> TypeOperationTexts;

}  // namespace lyra::backend::cpp
