#pragma once

#include <cstdint>

#include "lyra/runtime/import_call_chain.hpp"
#include "lyra/value/string.hpp"

namespace lyra::runtime {

class RuntimeEffects;
class Scope;

// The two ends of the extent a `context` DPI import's call runs inside (LRM
// 35.5.3). Entering makes `decl_scope` -- the instantiated scope of the import
// declaration -- what the foreign side reports, and leaving reports whatever
// was reported before it. The standard's subject is the import call chain: a
// context is created when SystemVerilog calls the import and ends when the
// chain unwinds back to it, so what stands for the chain here is the extent the
// generated body brackets its call with.
//
// The chain is state of whatever is running, so two foreign calls suspended on
// different processes never share one, and a call made before any process
// exists -- a variable declaration assignment runs first (LRM 10.5, 26.2) --
// still has one to enter.
void EnterDpiScope(RuntimeEffects& effects, Scope* decl_scope);
void LeaveDpiScope(RuntimeEffects& effects);

// The same chain read from the other side of the boundary, where the caller is
// foreign code holding nothing to find the run by. Nothing of the design
// running is not a case of its own: such a query is one no imported subroutine
// reached, which reports no scope exactly as a query from outside a context
// import's call does.
[[nodiscard]] auto CurrentDpiScope() -> CurrentDpiHandle;
auto ReplaceDpiScope(CurrentDpiHandle handle) -> CurrentDpiHandle;

// A string an export hands to the foreign code that called it (LRM Annex
// H.8.10), as characters the chain keeps until the import call that let that
// code in returns.
[[nodiscard]] auto HandStringToForeignCaller(
    RuntimeEffects& effects, const value::String& text) -> const char*;

// LRM 35.5.3: foreign code calls an exported subroutine only from inside the
// call of a `context` import. Where an export is reached from anywhere else,
// this raises that as an error of the design, which the region of the function
// foreign code called settles as it settles any error below it -- reported, the
// run ended and the execution stopped -- so nothing of the design runs. A
// non-context import has no boundary where control coming back is checked, so
// the statements after its call run until the execution next suspends.
void RequireAContextImportCall(RuntimeEffects& effects);

// LRM 35.8: the int an exported task's entry hands its foreign caller, and what
// `svIsDisabledState` answers -- 1 while a disable is active on this
// execution thread, 0 otherwise. Active covers everything that stops an
// execution: a disable reaching a block it is inside, its process being
// terminated, and the run ending. The standard names only the first, because
// it is the only one it lets reach a foreign call and return to it; the others
// reach the foreign side the same way here, and they are one answer because
// foreign code can do only one thing about any of them -- stop calling in and
// return. An execution running none of the design's own code has nothing to
// stop, and answers 0.
[[nodiscard]] auto DisableIsActive(RuntimeEffects& effects) -> std::int32_t;

// `svAckDisabledState` (LRM 35.9 item c): the foreign frame now running says it
// is following the protocol, which is what its boundary holds an imported
// function to before it returns.
void AcknowledgeStop(RuntimeEffects& effects);

// LRM 35.9's checks on the foreign side, each reported where its evidence is
// and none of them leaving by an effect -- each holds only once the execution
// has been told to stop, so what is left is to report and end the run, and the
// execution departs, as it already must, at the boundary where control comes
// back.
//
// Item b: an imported task that returns while its execution must stop returns
// 1. Item c: an imported function in that state calls `svAckDisabledState`
// before returning. Item d: no exported subroutine is called at all once the
// state is entered, which is checked where such a call arrives. Outside that
// state the same check holds the call to LRM 35.5.3, which lets an export be
// called only from inside a `context` import's call; that breach finds the
// execution told nothing, so it is raised, and the region of the function
// foreign code called settles it.
void CheckImportTaskAcknowledged(
    RuntimeEffects& effects, std::int32_t returned);
void CheckImportFunctionAcknowledged(RuntimeEffects& effects);
void CheckExportReachable(RuntimeEffects& effects);

}  // namespace lyra::runtime
