# Sequencing in expression position is a block expression, not an invoked closure

Date: 2026-08-28. Status: accepted.

## Why this decision matters

Some evaluations take several steps and still stand where the grammar admits only an expression. A
subroutine call that writes back to its actuals yields a result and performs writes; a scan yields a
matched-conversion count and parses into its output arguments (LRM 21.3.4.3); a foreign call
marshals across a boundary and hands back a value (LRM 35.5). Each is a run of statements that ends
in a value, sitting inside a larger expression:

```systemverilog
n = $sscanf(s, fmt, a, b) + 1;
```

MIR is a generic software programming language IR, so the question is not what SystemVerilog needs
but what a language of that family does with several steps in value position. This entry settles
that, and settles what the resulting node is allowed to contain.

## What the requirement is, in language terms

MIR separates statements from expressions: statements sequence, expressions produce values. That is
the C, C++, GENERIC, and Clang-AST family. The family's characteristic gap is exactly this
requirement -- the language has no way to say "these steps, then this value".

Three languages, three answers:

| Language   | Has the construct | How several-steps-then-a-value is written               |
| ---------- | ----------------- | ------------------------------------------------------- |
| Rust       | yes               | `let x = { let a = f(); a + 1 };`                       |
| C++        | no                | `const auto x = [&] { auto a = f(); return a + 1; }();` |
| TypeScript | no                | `const x = (() => { const a = f(); return a + 1; })();` |

The two that lack it reach for the same substitute: a closure, invoked where it is written. That
substitute is not free. A closure is a **value** -- it has identity, it can be stored, passed, and
invoked later or twice -- and none of that is true of the thing being expressed, which runs once, in
place, and is held by nobody. The substitute also introduces a function boundary, which changes what
a `return` among the steps means.

The languages without the construct are not settled in that position: GCC carries `({ ...; e; })` as
an extension, and JavaScript has a long-standing `do` expression proposal whose motivation is the
cost of the workaround. The requirement is universal; the construct is its direct spelling and the
invoked closure is a substitute for it.

## The model

```text
callable, closure   -- a function. A value. Holds a return.
block expression    -- a run of statements and the value it ends with. Not a value-producing
                       entity of its own, not a function, and it holds nothing.
```

A block expression is the node that lifts a statement sequence into value position. That is its
whole content: it sequences, and it yields.

### One node, not one per construct

Rust makes each control construct an expression: `if`, `match`, `loop`, and `unsafe` each carry a
value. In a statement / expression-split language the same reach is bought with one node, because
any statement form becomes value-producing by standing inside a block expression:

```text
Rust  let x = if p { ...; a } else { ...; b };
MIR   ConditionalExpr{ p, BlockExpr{ ..., a }, BlockExpr{ ..., b } }

Rust  let x = match k { ... };
MIR   BlockExpr{ [ the multi-way statement, writing a local ], that local }
```

The conditional's arms are already expressions and a block expression is one, so the composition
needs nothing new; the conditional evaluates only the arm its condition selects, which is what the
Rust form means. A construct MIR has not met yet gains a value the same way, with no further node.
Adding value-hood construct by construct would be N changes reaching less far.

## The decisions

```text
D1. MIR has a block expression: a child scope of the enclosing block, plus the value the whole
    yields. Its type is that value's type. It is the one node that puts a statement sequence in
    value position.

D2. A block expression is not a callable boundary. Its steps are lowered through the enclosing
    callable's binding context: a local declared among them is an ordinary local of the enclosing
    body, and a reference out of them resolves with no capture.

D3. A block expression is pure sequencing: it has no control-flow effect. Its steps do not return.
    A construct whose answer depends on a test states that answer as a value the steps settle.

D4. A closure is the other concept and keeps its own job: a callable value, for a body someone else
    holds and invokes later. A body invoked where it is written is not a closure.

D5. A block expression always yields a value. A run of steps that settles none is a statement, and
    a block statement over the same scope is what says so.
```

### Why D5 admits no valueless form

The node kind exists to put a statement sequence in value position. Remove the value and that reason
is gone, and what is left is a run of statements -- which the language already has a statement for.

The tempting counter is that a void-typed expression is ordinary. It is, but not for this node, and
the difference is worth stating because it decides the shape. A call is an expression **because
calls are expressions**: void is a property of the callee, and one node kind has to cover a void and
a non-void one alike, so it survives the callee returning nothing. A block expression is an
expression **because it yields**. Nothing else is holding it in the category.

So a valueless block expression describes not the node but a caller with no way to say "this lowered
to statements". A lowering exists to make a class of knowledge stop being needed, and one that hands
its consumers "did this produce a value?" has left that class alive. Where that came up -- a call
whose completion only writes back to its actuals, and a foreign boundary whose foreign side returns
nothing -- the lowering answers with a statement, which is what the source wrote.

### Why D3 is available here and not in Rust

Rust allows a `return` among a block's steps, and cannot do otherwise: in Rust a **function body is
itself a block expression**, so a rule against `return` inside one would remove `return` from the
language. Its `return` is therefore defined against the nearest enclosing function-like boundary --
a `fn` or a closure -- and passes through every block on the way.

MIR draws the two apart. A callable's body is a block of statements; a block expression is a
separate node that appears only in value position. A rule against `return` inside a block expression
therefore reaches only the steps of a value-producing run: a `return` in a callable body, in a
conditional's arm, in a loop body, is untouched, because none of those is a block expression.

So Rust's permissiveness is not evidence that permissiveness is correct. It is the only option a
language has when its blocks and its function bodies are one construct.

### What D3 buys

The node's purpose is sequencing that ends in a value. A non-local exit is a second capability, and
D3 declines to bundle it in. That is the whole of the argument: nothing needs it, so putting it in
the node would give every consumer a second thing to handle for no program's sake.

The consequence is that every consumer may treat a block expression as "run the steps, then take the
value", with no question about whether control leaves from the middle. A target whose only spelling
of the node is a function realizes it faithfully, which matters because a backend exists so that one
target's spelling never becomes a fact another consumer has to know -- and a node whose meaning some
target cannot realize is exactly that fact, leaking upward.

Nothing is given up. A `return` reaches MIR from a source-level return statement, which is a
statement of some callable's body and never a step of an expression's evaluation. Permitting it
would be IR surface with no producer and no test.

The cost of getting this wrong is already on record from the other direction:
[foreach-lowering](foreach-lowering.md) rejected spelling a loop's break as a `return` out of an
invoked lambda, because it corrupts a source-level `return` written inside the body. That is the
same boundary, met while designing a different construct.

## Rejected alternatives

- **A closure invoked where it is built.** The substitute C++ and TypeScript are forced into. It
  pays a capture boundary, an environment, and a call for a body that escapes to nobody, and it
  makes every sequencing site structurally indistinguishable from a body that suspends, since a
  closure carries coroutine-ness in its result type. It also spends the closure's function boundary
  on something that is not a function, which is what silently reinterprets a `return`.

- **Flattening at HIR-to-MIR: hoist the steps before the enclosing statement and leave a temporary
  in expression position.** This is the standard technique in a compiler whose middle IR is
  three-address (GCC's gimplification, Clang's CodeGen), and it needs no new node. It is not
  available here, because hoisting one subexpression reorders it against its siblings, so the
  transformation is only correct applied to every subexpression at once -- which is normalizing MIR
  into three-address form, and MIR's identity is a structured program that still reads as software.
  The flattening happens one layer down, where it is that layer's whole purpose.

- **A `return` among the steps, read as leaving the enclosing callable.** Rejected by D3 above.

- **A `return` among the steps, read as yielding the block's value.** One node would mean two things
  by context, and the meaning would differ from every language the reader knows.

- **Making each control construct value-producing instead.** Reaches less far for more changes, and
  each construct's value form is then a second thing to keep correct beside its statement form.

## Consequences

- The block expression is in MIR's primitive set, and it is the only node that puts statements in
  value position. Lowerings that need one build it through a single builder, whose frame is handed
  to expression lowering -- which has no return to emit, so D3 holds by what reaches the steps
  rather than by a check over them. Like MIR's other forbidden shapes it is stated, not scanned for.
- Sequencing and escaping are separate concepts with separate nodes: a body invoked in place is a
  block expression, a body someone else holds is a closure. No lowering builds a closure to
  sequence.
- A call whose lowering settles no value answers with a statement rather than an expression, so the
  two call families that can do so -- a subroutine writing back to its actuals alone, and a foreign
  boundary whose foreign side returns nothing -- have a statement form beside their expression one.
  Each is reached from statement position, which is the only position the frontend admits them in.
- The node does not survive lowering to the machine model. Below MIR it is the steps' instructions
  followed by the value's, which is what a flattened form would have produced -- so the node costs
  its expressive power and nothing else.
- A backend whose target lacks the construct spells it with a function, and D3 is what keeps that
  spelling faithful.

## Cross-references

- `../architecture/mir.md` -- the primitive set the block expression belongs to, and the forbidden
  shape D3 states.
- `../architecture/backend_contract.md` -- what a backend may and may not do with the node.
- `closure-environment-and-activation-frame.md` -- the closure as a concrete callable value, the
  concept D4 keeps separate from this one.
- `foreach-lowering.md` -- the same function boundary met from the other direction, where a lambda's
  `return` was rejected as the spelling of a loop's break.
- `unified-callable-model.md` -- callable code versus callable value; a block expression is neither.
- `closure-value-realization.md` -- how a closure value is realized where one is genuinely wanted.
