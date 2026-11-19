#pragma once

#include <optional>

#include "lyra/mir/callable_code.hpp"
#include "lyra/mir/code_source.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/type_id.hpp"
#include "lyra/support/value_operation.hpp"

namespace lyra::lowering::hir_to_mir {

// The body answering the operation `source` names at its type, a type no
// declaration states it for, at the types it is instantiated at,
// built from what the unit's types say and nothing of the source: an operation
// of a fixed-size unpacked array (LRM 7.4.2) or a tuple, an array's passage
// through the form of it with no queue bounded (LRM 7.6), or the default of a
// type some array holds (LRM 7.4.5), or any operation of a run-time-sized
// container (LRM 7.5, 7.8, 7.10). Nothing where the language gives the type no
// such operation.
//
// What is parameterized is instantiated where it is used, as rustc's collector
// instantiates a generic function and clang a template's member, so a body is
// built once something below MIR has found it is needed, as rustc's `make_shim`
// builds one on demand, in the form HIR-to-MIR leaves any body in, before the
// rewrites.
[[nodiscard]] auto MakeTypeOperationBody(
    const mir::CompilationUnit& unit, const mir::TypeOperationSource& source)
    -> std::optional<mir::CallableCode>;

}  // namespace lyra::lowering::hir_to_mir
