# Performance

Tracks performance work that is intentionally deferred behind functionality. An item here is a known
optimization with a defined target shape, not a correctness gap: the feature already behaves
correctly without it, and the item only makes the same behavior cheaper. Split into two domains that
have different cost models and different measurement methods.

Two kinds of item depart from that and each says so where it sits. The information a write discards
is what makes it expensive here and what makes one construct answer wrongly elsewhere, so the same
change closes both; the wrong answer is tracked with the construct it affects rather than here. And
a cost that grows with something the design chooses stops being a cost at some size and becomes a
wall, because the host compiler or the stack is what runs out -- which is what the generate item and
the packed-width item below were.

What a change measured is recorded in the pull request that made it, never here: a figure in this
file is stale the next change and invites re-measuring to keep it true.

## Runtime performance

Simulation execution speed: the cost of running processes, scheduling events, and reading and
writing state once the object graph is built.

How hard a program is compiled is settled on one axis and open on another. The runtime library an
emitted program links is optimized as shipped, independent of how the compiler that ships it was
built, because a user recompiles none of it. The design's own code is compiled unoptimized by
default and optimized under `--release`, on either backend, which is the one place the build-time /
run-time trade is a choice: iterating pays the compile on every edit, while a long run earns it
back. Which level `--release` asks the host compiler for is the open one, and is an entry below.

Runtime performance is tracked by the benchmark over the corpus under `tests/benchmark/`, which
builds every case under both Lyra and Verilator and puts the two rates side by side. It runs nightly
and gates its own run on a case building, running, and finishing inside a time limit -- never on a
rate itself, which would be a claim about the machine that ran it.

What a case reports is a **rate**, and no amount of work is written down anywhere: the harness
raises the amount until a measurement reaches a target duration, separately for each tool, and
divides it back out. So a case is never sized by hand, an engine that gets a thousand times faster
is followed rather than re-tuned around, and a reference simulator hundreds of times quicker is
measured over hundreds of times the work instead of over a millisecond of process startup. Beside
each rate the report says how the cost grew with the amount, because a rate holds at every amount
only where that growth is a line, and a case whose work is the size of a design may not be one.
`../decisions/benchmark-case-shape.md` carries why.

Two things still decide how a number is read. Every case that times a simulation is built optimized,
because the default unoptimized translation unit measures the build-time trade rather than the
simulation, so an unoptimized reading is not a slower version of the same answer. And the Verilator
column is **not** a like-for-like comparison: there is no two-state mode, so every factor carries
whatever X/Z tracking costs.

A ratio against the reference column is the one figure that can move with nothing on Lyra's side
having changed: the reference column moves between readings, and so does the amount of work each
tool is handed. So where a gain is recorded, it is recorded as Lyra's own rate, which can be read
again any day, and a ratio is never the evidence that a case got faster.

Read that way, most cost families sit in one band against the reference, and a family far outside it
is a defect rather than slowness: a factor of a few hundred is an engine that is slow everywhere,
and a factor orders of magnitude past that in one place is one operation doing the wrong amount of
work. Two such outliers have been found and closed, both below: writing an unpacked array element by
element, which copied the array, and wide bitwise work, which moved bits one at a time. Closing the
first is also the record of what such a factor is worth chasing: what the operation gains, a design
gains only in the share of its run it spends there.

**There is no single top.** Profiles over the corpus agree about most of what a run costs and
disagree only about what sits on top of it, so one case's profile is not the engine's answer. What
they have found, in the order it was closed: a value coming into existence -- blanking one, zeroing
its words, constructing it, masking the bits above its width -- rather than arithmetic; a fact the
compiler settled reaching the run as a value to work out again, such as a part-select whose bounds
are literals; every operation building a fresh value and every store comparing the old one against
the new; and, where the design waits on a clock, every wait allocating. The design's own body is a
small share of every one. What remains waiting on most of it is the open entry below: a value
operation is a call into a separately compiled runtime, and the optimizer cannot see into it.

- [x] An integral value's dimension stack no longer allocates for the single-dimension case. Every
      declared integral carried its stack in a growable container, so constructing or copying any
      value -- which generated code does at every operation, since the descriptor is built at the
      use site -- reached the allocator, on scalar arithmetic with no arrays and no scheduling as
      much as anywhere, which places the cost per value rather than per aggregate. The value grew in
      exchange, so a whole-array copy moves more; on a design whose arrays dominate the trade could
      go the other way.

- [x] A partial write to a cell no longer materializes the whole value. It used to reuse the
      whole-variable commit path, which reads the owner's whole value so that LRM 4.3 change
      detection and the subscriber wake could be answered by comparing it against the whole new
      value. For a packed value that is cheap and the edge decision needs the old bits anyway; for
      an aggregate the premise fails, because whether the value changed is answerable at the part
      being written. A write now lands in the owner's storage where it happens, and describes itself
      only where something is armed to read the description -- LRM 4.3 makes an update event matter
      to what is considered for evaluation, so a cell nothing observes has nothing to report and
      takes a plain store. The model is
      [../decisions/owner-transition-and-observation.md](../decisions/owner-transition-and-observation.md).

      Writing an element of a large unpacked array cost what copying the array costs -- the write
      tracked the size of the array it landed in rather than the size of the element, at the
      machine's memory bandwidth, so only not copying could recover anything. A write and a read of
      one element are now the same order, which is what "the write touches one element" means and
      is the thing to re-measure if this ever regresses. The unpacked-array read case, whose setup
      fills the array one element at a time, reports a read rate for the first time; while the
      write copied, its setup outweighed every read pass and it reported its own setup.

      What the correctness half of this cost is not fixed by it, and is fixed separately: a
      value-change event control now decides for itself, by comparing the value of its own
      expression, so what a leaf's bit window does is pass over a wait a write cannot have reached
      rather than decide one. Tracked with the construct it affects, in
      [processes.md](processes.md).

- [x] A net one driver decides is no longer resolved. Its driver's write lands in the value the net
      shows and wakes on what it changed, as a variable's does; a net with several drivers, a pull,
      a supply or a charge, and one a connection joined to another, resolve as before. An input port
      written with no data type is a net (LRM 23.2.2.3), so a design whose modules declare their
      inputs that way resolved one net per port per change.

- [x] A net whose one driver is a connection or continuous assignment from the whole of a variable,
      another net, or an input port declared as a variable, of its type, shows that object, and
      nothing runs after time zero; a wait on the net is a wait on the object. Such a port costs
      about what a port declared `input logic` does.

- [x] A wait whose watched storage cannot change while its process lives -- an `always_comb`, an
      `always_ff`, an event control on fixed storage anywhere in a body, a continuous assignment and
      every port connection, a sampler, an assertion's clock tick -- is built once and no longer
      rebuilt and re-registered at every stop. It stands on what it watches for its whole life, and
      a stop only says that the process is waiting there now; nothing is allocated and no watched
      expression is evaluated per stop. Every other wait is held by the body the same way, built at
      its stop, and a process inside a named block joins the block's disable target once rather than
      at every stop.

- [x] A closure the execution backend builds -- the expression an event control watches, the effect
      a nonblocking assignment defers -- is made once, with its captures in its own allocation, and
      whatever keeps it takes its owner rather than a moved copy; a capture is read at a fixed
      distance from the closure instead of through a runtime call. A wait reads its leaves where the
      waiting body built them instead of copying them into a list first.

- [x] Converting a packed value between widths or between the two- and four-state domains no longer
      walks it one bit at a time. It was the single largest cost in both profiled programs, and the
      reason is that the front end inserts such a conversion wherever an expression's type differs
      from its context's, so a design pays it on ordinary assignments rather than on anything a
      reader would call a conversion. One conversion now costs about what its words do.

      The separate pass that cleared the destination first is gone rather than made cheaper: it
      existed only because the per-bit copy wrote a subset of the destination, and a word-wise one
      writes every word. [refactor.md](refactor.md) R91 holds the shape and what was left out.

- [x] Taking bits out of a value and writing them back move them a word at a time, where both moved
      them one bit at a time; wide bitwise work was the outlier at every reading until then.

- [x] The sequence type behind a value's words and dimensions no longer holds its inline storage and
      its spill container at the same time. A value that never spills used to carry the spill
      container anyway and copy it on every copy; both of the type's uses want capacity one, so the
      general form was a cost with no consumer.

      What the size bought was not what made it faster, and the two came close to cancelling. The
      first cut reached the short case through a plain pointer and a run-time length, which costs
      the compiler exactly what an inline buffer is for -- it no longer knows how much room there is
      or that the length cannot exceed it -- and that measured slower while the value was already
      smaller. Settling which side a write is on before naming the room, rather than after, is what
      turned it around. The case that spills lost nothing by the narrower value.

- [x] An integral value no longer carries how its declaration divides its bits, and no longer
      re-establishes while running what the compiler settled before it ran. A value now states how
      many bits it has, whether they are read as signed and whether a position may hold x or z; an
      operation that names a position inside one takes the declared division as an argument from the
      site that wrote the position, which is what every other selectable family already did. The
      width had been stored two or three times over and the state domain twice, once as a flag and
      once as a discriminant kept in step by a runtime check.

      Two checks went with it, and the reason they could go is the same one: a view over a value's
      words was bounds-checked against those same words on every construction, where the relation
      holds by construction, and it also carried a bit offset that every construction set to zero
      and every site checked was zero. Operand-shape checks between two values stay, because a
      mismatch there is a missing conversion rather than something construction excludes. An
      operation now writes its result's bits into the result instead of into a buffer that is then
      validated and copied, which also removes a second pass masking bits the first pass had
      already masked. The model is
      [../decisions/packed-shape-belongs-to-the-type.md](../decisions/packed-shape-belongs-to-the-type.md).

- [x] A value the source fixed is no longer built wherever the program reaches it. The middle layer
      had literal forms for machine scalars and none for a value of the language it expresses, so an
      integral constant lowered to a call asking a library to make one -- which is the one form an
      optimizer cannot fold, in a layer whose primitives are chosen for an optimizer to fold. A unit
      now holds the constants it was written with, interned on the bits and the type together, and
      an occurrence names the entry; each backend gives that entry the storage its target has for
      something the whole run shares. The gain held with the value layer's definitions made visible
      to the optimizer, so this is not a cost an optimizer was going to recover. The model is
      [../decisions/a-constant-is-stated-not-computed.md](../decisions/a-constant-is-stated-not-computed.md).

- [x] Making a value no longer pays for its planes twice, and the runtime no longer calls its own
      functions as though another library might replace them. A four-state value that fits one word
      held each plane in a sequence of its own, each with its own length and room; both planes now
      share one array of words that holds them in place. Clearing the bits above the width after an
      operation, one instruction of work, had been a call into another file; it is now in view of
      every operation that ends with it.

      That half bought little, because a value fitting one word had not been reaching the allocator
      in the first place. Finding why found the rest: the shipped runtime is compiled
      position-independent, and under that mode one of the two compilers treats every function the
      runtime exports as replaceable by a same-named one loaded at run time, so it neither inlines
      one into another nor calls it directly -- a four-instruction function in the same file was
      reached through the procedure linkage table. The runtime ships as a static archive linked into
      the program, where nothing can replace anything, so the assumption only costs; it is now
      stated off for every target.

- [x] A select no longer hands the run the source's coordinates to resolve again. The bounds of
      `v[2:0]` used to reach the run as two four-state values, a form and the declaration's shape,
      and the run recovered where the selected bits start with four-state arithmetic on every
      evaluation. Every select now names a position in the selected value's own numbering and a
      count, the declaration is read where the select is written, and an index is moved by it only
      where the declaration does not number from the index's own zero. An integral operation whose
      operands are all constants is itself a constant, so a conversion of a literal is no longer
      performed each time control reaches it.

      What had looked like a conversion between two known types turned out to be two things. A
      conversion of a constant was a fact the compiler settled, and folds away with the rest. A
      conversion of a value the program computes -- `longint'(a)` -- has nothing left to settle;
      its cost was a general word loop where one word suffices, and a conversion between two values
      of one word now takes a straight-line path. The model is
      [../decisions/a-select-names-a-position.md](../decisions/a-select-names-a-position.md).

- [ ] An emitted program is optimized without the runtime library it spends its time in. The
      design's translation units and the runtime are compiled separately and linked as native
      objects, so every value operation a design performs is a call the optimizer cannot see into --
      which is what the middle layer's own contract already says about a runtime helper call, adding
      that link-time optimization mitigates it but does not eliminate it. Letting the optimizer see
      the value layer's definitions runs the representative block markedly faster, and on a design
      of a few units it costs no build time, because each unit then emits only an intermediate form
      and the optimization happens once at the link.

      **What is not measured is the only thing that decides it**: what that single link step costs
      on a design of a thousand units, where it is one serial whole-program pass rather than three
      files. That is the end-to-end trade `north_star.md` puts first, and it is the number to get
      before this is switched on rather than offered.

      Nothing pulls the other way any more. The value layer's operations are compiled once, in the
      shipped runtime, and a unit calls them; the copies a unit's object once carried, which an
      earlier reading attributed to those operations, were the runtime's own entries and families,
      and those are compiled once too now. So what decides this is the link cost above and nothing
      else.

      **Target shape**: the runtime ships in a form the optimizer can read, and a design build says
      whether to use it. It is the run-time half of the same axis `--release` already names, so it
      belongs to that flag's question rather than to a new one.

- [ ] Which optimization level a release build asks the host compiler for, and whether a user names
      it. `--release` asks for the second level on the C++ backend. An operation on a value of one
      word is reduced to a few instructions by the host compiler folding the value library's short
      functions into the design's code, and how large a function a compiler folds in differs by
      compiler and by level. Every one-word operation counted folds at the second level under both
      clang and gcc, so what the level decides is the operations larger than those. Undecided: what
      `--release` asks for, and whether an invoker names the level. Not measured: a benchmark
      design's build time and rate at each level under clang and under gcc.

- [x] On the execution backend an operation on a value within the in-place size is instructions at
      the value's own width, where past 64 bits it was a call into the library at a width the
      library learned from its arguments. A wide concatenation now runs at the C++ backend's rate,
      where through the library it ran far below it.

- [ ] What is still a call on the execution backend past 64 bits: a product, a quotient, a remainder
      and a power, whose work grows with the square of the width, and every operation over a value
      past the in-place size. Each call lays its operands and its answer out around the work. Not
      measured on a design that spends its time there.

- [ ] A concatenation of several operands is carried out two at a time on both backends. An
      optimized build folds the steps together; an unoptimized one makes a value as wide as
      everything joined so far at each step. Not measured apart from the entry above.

- [ ] An array's `unique` and `unique_index` (LRM 7.12.1) compare each element against every
      distinct one seen before it, so they cost the square of the element count. Two elements are
      the same where they are bit-identical (`===`), and the relational order that `sort` uses is no
      order over values carrying x or z, so it cannot stand in. The field keeps the keys seen in a
      set: Verilator's queue holds them in an ordered set, which its two-state values make possible.
      Here it needs a hash, or a total order, that agrees with `===` for every type an element or a
      `with` clause can have, on both backends. Not blocked.

- [x] A waiting process holds what it has: the place it resumes at, a record for each place it
      watches and each disable target it is inside, and a small random generator. A test holds a
      design of many waiting processes to an address limit.

- [ ] How many processes a design makes is not looked at: a port connection is still one.

## Construction / compile-time performance

The cost of producing compiled artifacts and of building the object graph at time zero. The primary
constraint is `north_star.md`: compile-time work scales with the number of distinct unit
specializations, not with instance count.

- [x] Specialization dedup for a value parameter read only as a value. Instances of a module handed
      different values of such a parameter are one unit, and the value reaches each instance when it
      is built (LRM 23.10), so a loop generate handing a child its index emits one child.
      [../decisions/a-parameter-read-as-a-value-is-supplied-at-construction.md](../decisions/a-parameter-read-as-a-value-is-supplied-at-construction.md)
      holds how a parameter qualifies and why a miss is reported as a compiler defect.

- [x] A parameter that only says what an instance builds is one unit for every value that builds the
      same blocks: one read by a conditional generate's condition, a case generate's selector, or a
      loop generate's bound, directly or through a parameter the module writes from it (LRM 27.4,
      27.5). Which blocks an instance built is part of its unit's name, so instances selecting
      different alternatives are a unit per set of blocks, and a loop counting more or fewer blocks
      of one kind is the same unit.

- [ ] What stays in the key although it might not. A parameter choosing which element a hierarchical
      name reaches, or read through its own hierarchical name, because either is settled when the
      unit is lowered rather than when the instance is built; a parameter an instantiation may
      override declared with no type, whose type is the type of the value it is given; and a
      parameter a class or one of its methods declares, with what it is written from, since the
      class's code holds it as a constant. Each still compiles once per value.

- [x] Which parameters decide is read off the front end's record of what each constant evaluation
      read, so no position is listed and none can be missed. A unit is lowered from the one instance
      it is named for; a check that lowers every other instance too reports one that lowers apart as
      a compiler bug, and nothing compiles the definition once per value instead. A module holding a
      class is a unit like any other, its class one class of the unit. An instantiation a `bind`
      directive inserts states its connections in the directive and is told apart by the directive
      that inserted it, which a case exercises.

- [x] The generate axis of that same sharing. A `generate for` used to lower concretely: N
      iterations became N scope classes and N construction statements, so the artifact grew
      one-for-one with the iteration count even when no parameter varied and every body was
      identical, in the declarations header every referrer parses as much as in the bodies. This is
      the one item here that was not a cost but a wall: past some width the host C++ compiler does
      not finish, and a design that does not compile has no behaviour to be correct.

      A loop's blocks now compile to one class the constructor builds once per index it counts out,
      and the index reaches the block as a value that construction supplies rather than as a constant
      folded into its body -- which is what lets the body of a loop that uses its index be one body
      at all, and a loop emits the same however many blocks it counts. Where the index instead fixes
      something a class settles once -- a width, a specialization, which members exist -- separate
      compilation is the correct answer and stays, which is `specialization_model.md` invariant 1. It
      is one class per distinct body rather than per index: blocks whose widths alternate between two
      compile to two classes, and the loop builds at each index the one that index takes.
      [../decisions/one-body-built-at-every-index.md](../decisions/one-body-built-at-every-index.md)
      holds how a body qualifies, why the question deciding that is checked rather than trusted, and
      what a scope's construction receives.

      **A block naming a constant worked out from its own index used to fall outside that and no
      longer does.** It reads as a case the sentence above already covers and was not: the index
      reached the block as a supplied value, and a constant derived from it was still folded, so
      one level of naming put the block back to one class per index, as a block declaring
      `localparam int K = i * 3 + 1` did. Such a parameter is now a declaration of the block holding
      the expression the source wrote, so blocks that wrote the same expression state the same
      thing. The positions where the value reaches a **type** -- a declared width, an unpacked
      extent -- are unchanged and correct: two types are two classes, and that is the sentence above
      rather than an exception to it.

      **A block or an instance reaching part of a vector through its index or its parameter used to
      fall outside both axes, and no longer does.** The bits a continuous assignment, an event
      control, `always_comb` or `wait` watches were stated as the bits one construction settled them
      to, and so were the range an indexed part-select yields and the positions a bidirectional port
      joins. A loop of `assign d[e] = v[e]` was therefore one class per index, and a module reading
      `v[N]` one unit per value, the second caught only by comparing instances and reported as lost
      sharing. Each is now stated as the select the source wrote (LRM 11.5.3), which every
      construction evaluates with its own index or value, so the loop's block is one class however
      many iterations it counts.
      [../decisions/a-part-is-the-select-the-source-wrote.md](../decisions/a-part-is-the-select-the-source-wrote.md)
      holds why which bits are watched stays the front end's answer.

      **A block whose index or parameter settles a condition its body writes used to fall outside
      them too, and no longer does.** What a body waits on was computed with every path its
      constants exclude left out, so `always_comb if ((i % 2) == 0) y[i] = a[i]; else y[i] = 1'b0;`
      waited on `a[i]` at one index and on nothing at the next, and blocks written alike were one
      set of classes per index. The same held under a `case`, a conditional operator, a logical
      operator's second operand, a procedural loop the index bounds, `@*`, `wait`, and a continuous
      assignment. What a body waits on is now what its text reads (LRM 9.2.2.2.1, 9.4.2.2), so each
      of those is one set of classes however many iterations it counts.
      [../decisions/read-set-inference.md](../decisions/read-set-inference.md) holds why, and what
      it costs: a procedure wakes on a read in a branch its constants exclude, as the clause states.

      Still one class per index, for two causes of their own. A constant select that lands outside
      its object at some index, which is how a chain is written inside a procedure
      (`if (i > 0) y[i] = a[i - 1];`): at the index where it lands nowhere the read reaches no bit
      and is not listed, so that block differs from its neighbours, and one block differing keeps
      every block apart. And a read inside a function the procedure calls (`return a[i];`), which is
      stated as the bits it reached and not as the select written there. Naming it by that select
      was tried: it shares the body, and it also watches the same bits as every select that makes
      it up, so a design watched more entries than its text reads.

      **A body waiting on two or more variables of its own used to fall outside them as well, for
      a reason no construct explains.** What a body waits on arrived in the order the front end's
      table of reads enumerated them, which follows where each variable was allocated, so two
      copies of one body stated the same reads at different positions and compared unequal. A loop
      whose blocks each declare three variables and read them in an `always_comb` was a set of
      classes per iteration and is one set at any count; a module instantiated with different
      values of a parameter it only reads reported lost sharing and was compiled once per value.
      Reads now arrive in the order the text makes them.
      [../decisions/read-set-inference.md](../decisions/read-set-inference.md) holds why the order
      is settled there. Every corpus design is lowered twice and held to stating one thing, and a
      check reads every instance through a body of its own and holds each to the unit it shares; an
      ordinary build lowers each unit from one instance.

      What it costs where nothing is gained: a block that cannot share either way now carries the
      declaration instead of the folded literal, paid only by blocks compiled apart for a reason of
      their own -- far less than an iteration removed where the block does share.

      **How many classes a loop publishes used to follow from comparing its blocks, and no longer
      does.** Each block was published under the path to it and the blocks found alike were merged
      afterwards, the others' names kept as further names of one class, so the C++ backend wrote a
      file per block and a module's forward header changed when one body did. A block instance is
      now an application of its block, named from the source: one published class per application,
      and blocks of one application whose code differs are that class realized more than once, each
      realization named from the index's value where the block's code uses it before any block
      lowers, so no block is compared with another to find which body it is. A child naming the
      block that holds it is one unit where it was one per block, and a struct a block declares is
      one type where it was one per block. On the C++ backend the classes of a module's scopes are
      nested classes in the one header that carries the module's name, so file count and emitted
      bytes no longer grow with the iteration count for any loop design the growth check holds. A
      loop whose blocks declare a class is still one class per index.
      [../decisions/a-generate-block-is-a-nested-definition.md](../decisions/a-generate-block-is-a-nested-definition.md)
      holds what an index decides and why.

      What this does **not** touch is the front end's own elaboration, which still produces a symbol
      per block, so the lowering phase stays linear in the iteration count and its wall clock and
      peak memory stay roughly where they were. What goes away is the emit and the host compile.

      `runtime_model.md` states this one as an absolute ("if you find compile-time work that scales
      with instance count, the design has been violated") where `specialization_model.md` invariant
      1 admits the concrete form as correct and scopes the requirement to the optimized steady
      state. The two are reconciled by the entry above, with the specialization model governing: the
      concrete form stays the baseline and the shared form is what a program gets when a proof
      exists for it.

- [x] What a generate block costs where it is not shared is held, kind by kind. The growth designs
      used to state only repetitions of one thing, so the check asked whether what should share does
      and never what a block unlike its neighbours adds. A run now reports how many of each kind of
      thing every unit holds in each form it passes through, a design says what one more step of its
      size adds, and every kind reported is held to that at two sizes.

- [ ] A generate block that is not shared costs a fixed apparatus, where it should cost about its
      own statements. One continuous assignment in such a block adds a class, a construction, the
      bodies running it, three start-up bodies and the constants describing them, where a simulator
      that writes the block's statements into its module adds those statements. So every way two
      blocks can differ that sharing does not see costs a multiple of that, and a loop of a thousand
      such blocks is where a design stops building. The growth record names what follows the block
      count on each path.

- [ ] Blocks that are one text are still told apart by what selects them. A declaration sized by a
      block parameter set from the loop index is one class per block where the sizes are few; a
      block holding nothing sized is one class per block where a block it holds is; and every block
      under a block told apart is written once per such block. Same growth record.

- [ ] A block reaching its neighbour by a name indexed from its own index is compiled once per
      block, and lowering such a loop takes more than proportional time, held in both paths' growth
      records.

- [ ] What remains of the instance array on that same axis. The member and the construction
      statement no longer scale: `Child c[0:N-1]` is one member whose type carries the multiplicity
      and one statement composing the whole sequence, so widening an array changes neither. Two
      things still do. The composition names every element, so the constructor holds N element
      expressions; reaching one loop needs a way to build a sequence from a count rather than from
      its elements, which the value model does not have -- a sequence is composed where it is built.
      And a port connection on an array is still distributed per element by the frontend, so its
      endpoint members and its implied continuous assignments stay one per element; collapsing those
      needs the peer to be a function of the index, which is exactly what the distribution spent.

- [ ] What a port binding costs at time zero. Each binding adds a fixed time to elaboration, linear
      in the number of bindings and independent of whether the port is driven across the boundary.
      About half of it is kernel time, and that half grows with the binding count and with nothing
      else the design holds, which points at an allocation per binding rather than at the work of
      settling one. What makes it worth an entry is the scale: a design with very many ports spends
      a noticeable time before any process runs, and elaboration is paid on every run rather than
      once per edit.

      Asking the declaring unit for a published member is not what this is. That is one indirect call
      per binding, orders of magnitude under what a measurement at this size can separate, so the
      reading bounds the call from above and says nothing else about it.

- [x] Writing out the target text no longer costs more the deeper a design nests its expressions.
      Every part of an emitted artifact used to be built as a value and copied into the value
      enclosing it, so a byte near the bottom of an expression was copied once per level above it,
      and the punctuation between the parts was a format description read again at run time for
      every part written. Each byte is now written once, where it goes, names and type spellings
      included, and the text produced is identical, which is what the whole conformance corpus was
      emitted twice to establish. [refactor.md](refactor.md) R137 holds the reading and the
      prediction it falsified.

      This is a rate rather than a count, so no coverage record holds it and the corpus cannot see
      it: every conformance case is one small design, and what it measures only shows at a scale
      none of them reach.

- [x] What a design costs to build and to start no longer follows the width of a packed type it
      declares. This was a wall and not a cost, on three counts. A value lay in the frame of
      whatever evaluated it, so a program declaring a vector of some millions of bits ended on a
      fault with no message, and at the width every tool must take (LRM 6.9.1) a body of five
      hundred statements did the same on the C++ backend. A fill, a default and a small number
      widened were each folded into data as large as the type, so a module of twenty lines took
      minutes to build at a width of some millions of bits. And an integer parameter was spelled
      into a unit's name as digits, which cost the square of its width.

      A value past a fixed size now owns its words and a frame keeps their address, folding makes
      no data the source did not write, and a parameter is spelled from its words. The growth check
      holds a design that fills a vector, adds to it and writes part of it, and its build time and
      program size no longer follow the width. `../decisions/a-packed-value-is-its-words.md` holds
      why the line is drawn by size and what folding declines.

- [x] A process that waits only at the head of its loop is no longer compiled as a coroutine. A
      continuous assignment, a port connection, an `always_comb` and an `always @(...)` or
      `always_ff` whose event control opens its statement each became one, and splitting a coroutine
      into its resumable parts is most of what code generation spends on a design made of them. Each
      is now two plain bodies -- what runs before the loop, and one pass of it -- which one
      coroutine of the runtime calls, on both backends. Where the loop's wait is built inside it, or
      a process waits anywhere else, it stays a coroutine.
      `../decisions/a-process-stopping-only-at-its-loop-head-is-called.md` holds why this is a
      rewrite of MIR rather than a second kind of process.

### Open questions

- How thin the specialization key goes. The fat-value runtime representation carries packed width
  and unpacked size as runtime fields rather than as distinct types
  (`decisions/integral-representation.md`, `decisions/unpacked-array-representation.md`), so for the
  C++ backend a width or size parameter does not change generated code and could be a constructor
  input. A width-templated backend (the future LLVM `iN` lowering) does specialize on width. Whether
  per-width specialization is a key axis or a backend-internal monomorphization downstream of one
  width-generic artifact is unresolved, and it conflicts with `specialization_model.md` invariant 3
  as written (which lists packed width as code-shape-affecting). Resolve before keying on width.
