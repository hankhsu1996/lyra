#pragma once

namespace lyra::hir {

struct CompilationUnit;

// Checks what this layer states about a finished unit, and throws InternalError
// naming every place that breaks it.
//
// Each thing the source wrote is said once, so within one body or scope every
// expression is reached from exactly one place: the expression it is an operand
// of, or the statement or declaration that holds it. An expression reached from
// two places is one piece of source standing at two positions, and every
// consumer below evaluates it at each of them (LRM 11.4.1 has an operand
// evaluated once).
//
// A place is where something evaluates the expression. A read set or a
// sensitivity names expressions the body holds in order to describe what it
// reads or waits on, and an expression nothing holds is evaluated by nothing,
// so neither is a place; a description is still held to naming an expression
// its arena has.
//
// A unit is reported whole rather than at its first violation, one line per
// expression, so one run over a design says everything that shares.
void Verify(const CompilationUnit& unit);

}  // namespace lyra::hir
