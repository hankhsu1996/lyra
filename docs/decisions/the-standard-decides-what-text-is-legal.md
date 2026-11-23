# The standard decides what text is legal

Status: Accepted, 2026-10-11; revised 2026-10-16. Replaces the choice of 2026-10-09 that made the
front end's most tolerant reading the default.

## Requirement

A design means what the standard says it means, and its author is told where it relies on something
else. On 2026-10-09 Lyra began asking the front end for the union of every tolerance it knows, on
the argument that a design brought to a new simulator already runs elsewhere, so its text "works
everywhere else". The designs argued from were testbenches written for one open simulator and run on
nothing else; the forbidden texts in them were ones that simulator takes, which is why nobody had
noticed them. And a forbidden text has no meaning to adopt, only a tool to copy. The standard says
nothing of what `addr[2:3]` selects from a `[63:0]` vector, and the front end, asked to tolerate it,
warned and then built no expression at all.

## Choice

- **The default reading is the front end's own, which is meant to be the standard's.** A tolerant
  reading is selected only by name (`--compat`).
- **Legality is judged once, by the front end, and corrected there.** Where its reading is wrong it
  is corrected in the front end, never in Lyra's settings, and Lyra keeps no list of diagnostics
  deciding what is legal. Lyra's policy chooses the front end and its defaults, the front end
  analyzes, and Lyra carries out what was analyzed, deciding only what the analysis leaves open.
- **Since 2026-10-16, the front end's severity is shipped as given.** A forbidden text the front end
  accepts with a warning runs with that warning, lowering judges no legality again, and an argument
  that the text should be an error is made upstream. The exception is a choice the standard leaves
  to the tool, which Lyra makes itself.

## Alternatives turned down

- **The tolerant default, refusing only what the front end gives no meaning.** It fails a design
  meaning what the standard says, since wherever the front end does produce a meaning, which is most
  of the time, the design means one tool's choice, and its author is not told.
- **A list in Lyra of single diagnostics to raise or lower.** It fails one statement of the
  standard. The failure rests on a second statement beside the front end's drifting from it with no
  place where a difference is argued and settled. A choice the standard leaves to the tool is not
  such a judgement.
- **Accepting a forbidden text that every tool takes and gives one meaning**, such as assigning a
  non-enumerated value to an enumerated variable without a cast. It fails a rule anyone can state in
  advance, since an exception for agreement has no boundary.
- **Having the front end give a tolerated text the meaning one tool gives it.** It fails the same
  requirement as the tolerant default, making one tool's choice the definition of a text the
  standard left undefined.
- **Keeping "a forbidden text is an error" in Lyra.** It fails one judgement of legality, needing
  either the list rejected above or lowering refusing what the front end evaluated; `$fatal` with a
  message first (LRM 20.10) is accepted with a warning by the front end, deliberately.

## Consequences across layers

- The rule as it holds, how a disputed text is ruled on, and every ruling so far are
  [`language_conformance.md`](../architecture/language_conformance.md)'s.
- A correction of the front end is a commit on its fork and a move of Lyra's pin
  ([the-front-end-fork-keeps-a-branch-per-release](the-front-end-fork-keeps-a-branch-per-release.md)).
- Name resolution is the front end's too
  ([front-end-semantic-boundary](front-end-semantic-boundary.md)).
