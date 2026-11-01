# Lowering

**Definition.** One elaborated body the front end did not share, lowered to HIR for the instance it
was elaborated for under that instance's environment, and named by that instance's hierarchical path
(LRM 23.6).

**Contrast.** Not a specialization (LRM 8.25), which is a definition applied to arguments: the front
end may elaborate two applications with equal arguments apart, and then they are two lowerings. Not
a [body](body.md): lowerings whose HIR is equal are one body. Not an instance: an instance the front
end found alike an earlier one is served by that one's lowering.

**Usage notes.** A lowering's name is internal to one run and spelled in nothing compiled; the
design root's construction tables and the parameter values each lowering publishes are keyed by it.
`specialization_model.md` owns the rule.
