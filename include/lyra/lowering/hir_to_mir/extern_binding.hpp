#pragma once

#include "lyra/diag/diagnostic.hpp"
#include "lyra/hir/subroutine.hpp"
#include "lyra/lowering/hir_to_mir/process_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/unit_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/walk_frame.hpp"
#include "lyra/mir/callable_code.hpp"
#include "lyra/mir/callable_id.hpp"
#include "lyra/mir/class_id.hpp"
#include "lyra/mir/field.hpp"
#include "lyra/mir/type_id.hpp"

namespace lyra::lowering::hir_to_mir {

// A subroutine an interface leaves to a connected module (LRM 25.7) is a
// callback: the interface keeps what a definer handed it, and running the
// subroutine calls that. Nothing here knows which module defines it, and no
// caller does either.

// What an object keeps of what its definers hand it: the one a call runs, or,
// where every connected module may define the subroutine (LRM 25.7.4), all of
// them.
[[nodiscard]] auto ExternStorageType(
    mir::CompilationUnit& unit, const ExternBinding& binding) -> mir::TypeId;

// Brings what the object at `frame` keeps in its field `storage` to what it
// holds before any definer hands it anything.
void InitializeExternStorage(
    mir::CompilationUnit& unit, const WalkFrame& frame, mir::ClassId owner,
    mir::FieldId storage, const ExternBinding& binding);

// The code of such a function of class `owner`: a call of what the object keeps
// in its field `storage`, with the function's own formals, answering what that
// answers. A function cannot be disabled, and only one module defines it, so
// that is all it does.
[[nodiscard]] auto ExternFunctionCode(
    UnitLowerer& unit_lowerer, mir::ClassId owner, mir::FieldId storage,
    const ExternBinding& binding) -> mir::CallableCode;

// The body of such a task, in `frame`: it runs what the object keeps in its
// field `storage`, and every definition at once where there may be several
// (LRM 25.7.4), as a body of the task, so a `disable` of the task ends it.
auto WriteExternTaskBody(
    ProcessLowerer& process, const WalkFrame& frame,
    const hir::SubroutineDecl& declared, mir::ClassId owner,
    mir::FieldId storage, const ExternBinding& binding) -> diag::Result<void>;

// The code of the method keeping the definition handed to an object of class
// `owner` in its field `storage`.
[[nodiscard]] auto ExternBindCode(
    UnitLowerer& unit_lowerer, mir::ClassId owner, mir::FieldId storage,
    const ExternBinding& binding) -> mir::CallableCode;

// The class of what the class `owner` hands over for its method `definition`:
// it implements the abstract class `binding` states, holds the object it was
// made for, and its `invoke` runs the method on that object. It is the code of
// the one body defining it, so nothing outside the body names it.
[[nodiscard]] auto DefineExternCallback(
    UnitLowerer& unit_lowerer, mir::ClassId owner, mir::CallableId definition,
    const ExternBinding& binding) -> mir::ClassId;

}  // namespace lyra::lowering::hir_to_mir
