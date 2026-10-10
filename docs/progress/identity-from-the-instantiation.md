# An instance identified where it is instantiated

Tracks moving the question "which unit is this instance" from the instance's body to the place that
instantiates it, and making what answers it the whole of what a unit's lowering can know about the
instance it is lowered from.

Done when naming an instance reads nothing of that instance's body; when a unit's lowering takes
every fact about another unit from what that unit published; when two instances of one unit cannot
lower differently by construction, so nothing is lowered a second time to check; and when a build
where nothing changed reads no instance's body at all.

## Contracts

This workstream reasons from these and does not restate them:

- `../architecture/north_star.md` -- compile per unit, elaborate at run time; artifacts follow
  distinct specializations.
- `../architecture/specialization_model.md` -- a specialization is a definition applied to
  arguments, and what counts as an argument.
- `../architecture/incremental_build.md` -- compilation as memoized queries; a unit depends on
  another only through its signature.
- `../decisions/specialization-identity.md` -- the key, what it holds, and that both sides derive it
  independently.
- `../decisions/a-parameter-read-as-a-value-is-supplied-at-construction.md` -- which parameters
  enter the key, and that a comparison decides where a classification predicts.
- `../decisions/the-front-end-has-one-reader.md` -- what reading the front end costs and why it is
  read one unit at a time.

## Where this differs from a compiler of generics, and why it is not free

A C++ or Rust compiler names an instance from the line that uses it: a definition and the arguments
written or deduced there. It opens the body once, to compile it. Two things make that harder here,
and each item below removes part of one of them.

**Some of what tells an instance apart is not written where it is instantiated.** A hierarchical
name resolves per instance (LRM 23.8), and what is written elsewhere reaches one instance and not
another (LRM 23.10.1, 23.11, 33.4). One module text reading `cfg.g` under two parents is two
programs when the two `cfg` differ in type, so the type a name reaches is an argument the source
does not write as one. That much is the language's.

**A unit's lowering can reach past what identifies it.** It is lowered from one instance's body, and
that body leads to the instance, to what the instance is connected to, and to the bodies of other
instances. Whatever it reads there and the key does not hold makes two instances of one key lower
differently, which is why every instance with a body of its own is lowered and held to its unit.

## The steps

- [x] **What a body's text says of its parameters is asked once per body.** Which parameters decide
      what is compiled is a fact of the elaborated text; which of them an instance was handed a
      value for is a fact of the instance.
- [x] **An instance whose shared body no hierarchical name leaves is named where it is written.**
      The front end elaborates one body for instances it finds alike and leaves the rest
      unelaborated. Such an instance is one application with an earlier one when its values were
      seen before, by this compiler's bit-exact comparison, and nothing of it is read: not its body,
      and nothing below it. Every other instance is read through its own body and held to its unit,
      as before.
- [x] **Every corpus design is lowered both ways and held to one answer**: reading only what the
      front end elaborated, and reading every instance's body with each held to its unit.
- [ ] **A name written from the top does not enter a key.** It names one object from every instance
      (LRM 23.6), so it tells no two instances apart; today it is counted with the names that
      resolve per instance, and a body holding one is read per instance.
- [ ] **A parameter read through a hierarchical name is read from what its unit published.** Today
      its value is folded into the reader from the front end's tree, and the key holds only the
      other unit's name.
- [ ] **A type declared in another instance is taken from what its unit published.**
- [ ] **A port of a child is read from the child's signature**: what the port stands for inside the
      child, and whether a connection is the child's default.
- [ ] **A route below another instance is spelled from what the units on it published**, and not by
      walking that instance's subtree in the front end's tree.
- [ ] **"Is this declaration mine" is answered by what a declaration is**, and not by whether it
      stands in the very body the unit was lowered from.
- [ ] **The remaining reaches named in the audit**: the class scopes a name passes through, whether
      a class's scope encloses the reader, the unit an enclosing instance is where the key holds
      only how far out it is, and the front end's rendering of a type name.
- [ ] **A unit's lowering is handed the body and one value saying everything it may know of the
      instance**, and the key is that value. From here two instances of one key cannot lower
      differently, and the per-instance lowering that checks it goes; reading every instance stays
      as what the tests do.
- [ ] **Which names leave a body is taken from the front end's own record of it**, which it keeps
      per body and notes on every body a name passes through. Naming then reads no body.
- [ ] **A holder does not write the class of what it builds into its own code.** A name written deep
      down then tells apart only the module writing it, and not every module between it and where it
      lands.
- [ ] **The front end elaborates a body for a reader and releases it.**

## Blockers and open questions

- The steps on what the lowering reads land in files more than one clone has open; each is a change
  of its own.
- Whether a name written from the top is compiled relative to the instance writing it is not read.
  If it is, the fourth step changes how such a name is lowered and not only what the key holds.
- The front end's record of leaving names holds upward names only, is not exposed, and its
  definition has not been held against this compiler's own walk. Its instance cache has been wrong
  twice (an interface-array port left out of its key, fixed upstream and in the pin; 0.0 and -0.0 as
  one value, standing), so nothing here takes the front end's word that two instances are alike
  without a condition of its own.
- A module writing an upward name is elaborated once per instance by the front end itself. What such
  a module costs follows its instances, and the most these steps reach is not adding to it.
- A holder's code naming its child's class is an object-model question, and the alternative (the
  class handed over at construction) is not designed.
