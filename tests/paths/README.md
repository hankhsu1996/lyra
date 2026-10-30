# How Lyra holds itself to the conformance suite

The cases are the conformance suite's (`../../conformance/README.md`), which decides whether a run
conforms to IEEE 1800. Nothing here changes a verdict: what this directory holds says only which
verdict Lyra is known to give, so a result off that baseline fails, whichever way it moved. Every
case is run with Lyra's own checks of itself switched on.

## What a path cannot do

A path is the artifact a run produces -- the C++ project, or the LLVM module -- not the way that
artifact is executed. What a path currently refuses is recorded once for the path, in `<path>.yaml`
here, never on a case:

```yaml
16_assertions/immediate_assertion_severity: "assertions are not yet supported"
```

A case listed there is expected to fall short with output containing that text. One that conforms
instead fails until its entry is removed, so the file only ever shrinks. That is what makes it the
coverage report: the cases absent from it are the cases the path runs.

Absence of an entry is therefore a claim, which is why no case carries a per-path tag. A tag's
absence cannot tell "this path refuses the construct" from "nobody has tagged it yet".

A compiler bug is neither. A run that reports one fails whatever the records say, because "not
implemented yet" and "implemented wrongly" recorded the same way would leave nothing able to report
coverage. Every case is lowered with Lyra's verification of its middle form after each rewrite, and
also reading each instance through a body of its own, each held to the compiled unit it shares; one
that lowers differently fails as a compiler bug. A foreign source is compiled after the header Lyra
generates for the design, so a prototype that header gets wrong is a failure of the case even though
the case never includes it.

## Where a path answers wrongly

A wrong answer is not a refusal, so it is recorded separately, in `<path>.defects.yaml`, holding
text the failing run produces:

```yaml
11_operators/packed_power_operator: "zero_base_negative_exponent was"
```

The case keeps every check it makes. It is expected to fall short, and to say that; the day the
answer becomes right the case conforms, and the run then fails until the entry goes. That is the
point of recording a defect here rather than commenting the check out inside the case, where the
check stops running and nothing notices when the behaviour is fixed.

A run whose programs are built to check their memory (`--sanitize address`, in the run that checks
Lyra's own) sees what no other run does, so what it finds unfixed is recorded apart, in
`<path>.address.defects.yaml`, in the same form. Only that run reads it, since the same case passes
in every other.

A case appears in at most one of the records -- refused and wrong are different outcomes and a case
recorded twice leaves no saying which it is held to. No record is a place for a failure that is
merely inconvenient. What goes in is a defect someone has diagnosed, with an entry in the matching
progress doc saying what the standard requires.

## A case no path can run

A case Lyra cannot run without harm -- one that never ends, or takes the machine's memory with it --
is named in `parked.yaml` with why, and is held only to elaborating. The case itself is left as it
is, so every other tool runs it. This is the last resort, because a parked case is the one shape
here that nothing watches.

## Lyra's choices

Where the standard leaves an answer to the tool and Lyra has chosen one, a case holding Lyra to that
choice lives in `../choices/`, in the same shape and under the same clause, and only Lyra runs it.
It is how a choice is kept from changing by accident, and it is never a statement of the standard.
Where two clauses disagree, the choices case holds the reading the ruling in
`docs/architecture/language_conformance.md` gives, and the conformance case beside it keeps what
both readings require.

## Running

```bash
bazel test //tests:llvm_tests
bazel test //tests:cpp_tests
```

To run one case, filter by its id -- its path under the suite's `cases/` -- with `/` written as `.`.
Nothing names the path that runs it, because the target already does:

```bash
bazel test //tests:llvm_tests \
  --test_filter='27_generate_constructs.loop_increment_forms'
```

A parked case's elaboration check answers to `Parked.` and then the same name, that being the one
thing the target does not already say.

Adding a SystemVerilog feature means adding or extending a case of the suite first, then
implementing until it passes. A record names a case by its id, and an entry naming an id that
matches no case fails the run, so moving a case is where its entries are renamed.
