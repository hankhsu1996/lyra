#pragma once

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
[[nodiscard]] auto CurrentDpiScope() -> Scope*;
auto ReplaceDpiScope(Scope* scope) -> Scope*;

}  // namespace lyra::runtime
