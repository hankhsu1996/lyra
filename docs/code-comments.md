# Code comments

The rules for any comment in `src/` or `include/`. When and how often to re-read this doc is
specified in `CLAUDE.local.md`.

## The three kinds (Clean Code framing)

- **Why** -- motivation, hidden constraint, non-obvious choice. The code shows what; this explains
  why. **Write these.**
- **How** -- short algorithm sketch for a procedure complex enough that line-by-line reading is
  painful. A summary, not a play-by-play. **Write these when genuinely non-trivial.**
- **What** -- restates what the code already says. **Forbidden.** The fix is renaming the symbol
  until the code is self-explanatory, not writing a comment to compensate.

Comments fill what the language cannot express -- intent, context, the algorithm's shape.

## Hard bans

1. **No project-documentation references.** No `docs/` path, no decision-doc / architecture-doc /
   progress-doc name, no "invariant N", no "see the decision on X". The **LRM** (IEEE 1800
   SystemVerilog spec) is the one exception -- it is the external spec the code implements, and
   `LRM 21.3.4.3`-style citations are required for any SV-grounded comment. Why: docs rename and
   move; back-references rot and lie.

2. **No implementation-name coupling.** A comment that names a sister helper, accessor, or member
   couples to a rename. State the stable contract instead. Architecture-level vocabulary (a MIR
   primitive, a runtime type, an LRM concept) is fine; internal helper names are not.

3. **No What-comments restating the code.** Rename the symbol.

4. **No section-separator comments.** `// === X ===`, `// --- title ---`, any region-marker line.
   Use blank lines.

5. **No migration / progress narration.** `// Cut 1`, `// migrated in phase 3`,
   `// later cuts will extend this`. Source comments are timeless; progress lives in
   `docs/progress/`, in PRs, and in conversation.

6. **No `/*param=*/value` at call sites.** IDE inlay hints do this.

7. **No inline comments on struct fields or variable declarations.** Put the comment on the line
   above.

8. **No defence of the change.** A comment that only makes sense read against the version it
   replaced is changelog: `// keyed rather than parallel`, `// the alternative would ...`,
   `// never a mix of X and Y`. It can be technically correct, cite the LRM, and name no sister
   symbol -- passing every ban above -- and still be wrong. Changelog belongs in the PR, the commit
   message, and the conversation. Two consequences make this the ban that is hardest to see: it
   grows only where the code was just edited, so it shows up as one commented member in a block
   where nothing else carries a comment; and no search finds it, because the words it uses are the
   same ones a legitimate comment uses to contrast what the code is against what a reader would
   assume.

## Litmus checklist

Run per comment before keeping it:

1. **Cite check** -- names any project doc, decision-doc, architecture-doc, progress-doc, or
   "invariant N"? Cut. (LRM is the only exception.)
2. **Rename check** -- would renaming any sister symbol named in this comment force an edit? Cut the
   name; rewrite to the stable contract.
3. **Why/How check** -- is this Why, How, or LRM citation? If none, it is a What. Cut and rename the
   symbol so the code self-explains.
4. **Drift check** -- does the comment still match the code as it stands now? Update or cut.
5. **Changelog check** -- would this comment exist if the code had always been this way? If no, it
   is a defence of the edit. Cut.
