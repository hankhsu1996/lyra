# Performance

Tracks performance work that is intentionally deferred behind functionality. An item here is a known
optimization with a defined target shape, not a correctness gap: the feature already behaves
correctly without it, and the item only makes the same behavior cheaper. Split into two domains that
have different cost models and different measurement methods.

Two items depart from that and each says so where it sits. The information a write discards is what
makes it expensive here and what makes one construct answer wrongly elsewhere, so the same change
closes both; the wrong answer is tracked with the construct it affects rather than here. And an
artifact that grows with something the design chooses stops being a cost at some width and becomes a
wall, because the host compiler is what runs out -- which is what the generate item below was.

## Runtime performance

Simulation execution speed: the cost of running processes, scheduling events, and reading and
writing state once the object graph is built.

How hard a program is compiled is settled and is not a tracked gap. The runtime library an emitted
program links is optimized as shipped, independent of how the compiler that ships it was built,
because a user recompiles none of it. The design's own code is compiled unoptimized by default and
optimized under `--release`, on either backend, which is the one place the build-time / run-time
trade is a choice: iterating pays the compile on every edit, while a long run earns it back.

Runtime performance is tracked by the benchmark over the corpus under `tests/benchmark/`, which
builds every case under both Lyra and Verilator and puts the two rates side by side. It runs nightly
and gates its own run on a case building, running, and finishing inside a time limit -- never on a
rate itself, which would be a claim about the machine that ran it.

What a case reports is a **rate**, and no amount of work is written down anywhere: the harness
raises the amount until a measurement reaches a target duration, separately for each tool, and
divides it back out. So a case is never sized by hand, an engine that gets a thousand times faster
is followed rather than re-tuned around, and a reference simulator hundreds of times quicker is
measured over hundreds of times the work instead of over a millisecond of process startup.
`../decisions/benchmark-case-shape.md` carries why.

Two things still decide how a number is read. Every case that times a simulation is built optimized,
because the default unoptimized translation unit measures the build-time trade rather than the
simulation -- 4.5x on the NBA-heavy case, so an unoptimized reading is not a slower version of the
same answer. And the Verilator column is **not** a like-for-like comparison: there is no two-state
mode, so every factor carries whatever X/Z tracking costs.

Read that way, and re-measured over the whole corpus on 2026-09-23, most cost families put Lyra
between 21x and 706x Verilator's rate, with the narrowest a subscription scan that fires nothing and
a dense change subscription beside it, and the widest a packed-slice read. One still sits outside
that band: wide bitwise work on a 256-bit value, which has been the outlier at every reading and was
**3,716x** at that one. A factor of a few hundred is an engine that is slow everywhere; a factor
orders of magnitude past that in one place is a defect rather than slowness.

The rates behind that band, so a later reader has something that can be read again rather than a
ratio against a reference column that moves: 36,302 cycles/s on the subscription scan, 14,720 on the
dense one, 2,568 array passes/s on the packed-slice read, 367,261 iterations/s on the wide bitwise
case, and 1,636 table passes/s on the representative block.

The wide bitwise case has moved since, to **469,661 iterations/s**, which is the entry below on
moving a run of bits a word at a time. Its ratio read 2,683x on the same run -- but the reference
column also moved between the two readings, so the ratio is not what says the case got faster and
the rate is.

One figure this paragraph used to carry widened and has since come back: the subscription scan that
fires nothing read 15x, then 114x, and reads 21x today. Nothing landed in that window could widen a
ratio from Lyra's side -- its rate only improved -- so the 114x was the reference column or the
amount of work each tool was handed, and the reading that followed it says so rather than leaving
the question open. **What the episode is worth keeping for is the shape of it**: a ratio moved by a
factor of seven with nothing on this side of it having changed, and a ratio is the only figure that
can do that. This is why an absolute rate is recorded beside one.

Writing an unpacked array element by element was the other such outlier, at **three million times**,
and read 2,345x when that fix landed -- inside the band, and no longer the thing to look at; today
it reads 380x, at 133 array passes/s. It is the one entry here whose fix has been measured twice, so
it is also the record of what such a factor is worth chasing: what it bought was a thousandfold on
the operation, and the design that spends two thirds of its run there gains the two thirds, not a
thousandfold.

The unpacked-array _read_ case could not report a read rate while that defect stood, and the way it
failed is worth keeping. It fills the array before reading it, one element at a time, so its setup
ran the write path once over the whole array -- more than a minute, against a second for each read
pass that followed. Separating a fixed cost from a marginal one needs the marginal part to clear the
noise between two readings, and there it did not, so what the case reported was its own setup. It
reported 1,069x, which is a read rate, and it did so with no change to the case: nothing in the
write work touched the read path, which is what makes this the independent check that the write
stopped moving the array. It reads 259x today, at 263 array passes/s.

A profile of the integration design says where the time goes there, and it is not where the
pre-reset engine spent it -- propagation does not appear at all. Two thirds of a run was a single
nonblocking write into the design's memory, and a further quarter is spent constructing and
range-checking views over bit vectors, which every bit access pays whatever its storage. That
profile predates the write fix below and has not been retaken, so the two thirds is what the fix was
sized against rather than what a run costs now; the quarter is untouched by it and is what the next
profile should show at the top.

Nearly. A profile of the mixed arithmetic-and-control case, once the width-conversion entry below
stopped hiding everything behind it, had building a value from its word planes and range-checking a
view tied at the top, near a tenth of the run each -- two halves of the same thing, since a value
built from words was then read through a window that revalidated what the builder had just
established. That is the last entry below, and it is closed; the design's profile has not been
retaken since, because retaking it needs the design.

**Retaken over the corpus instead, on 2026-09-23, and the result is that there is no single top.**
Two cases profiled under callgrind on the compiled program, both `--release`. On the wide bitwise
case -- the only one outside the band -- taking a run of bits out of a value was 23.3% of the run's
instructions on its own and writing one back a further 6.6%, because both moved a run one bit at a
time; that was [refactor.md](refactor.md) R128, now closed, and it took the case from 367,261 to
469,661 iterations/s. On the representative block those same two are 3.4% together and the top is
**making a value at all**: blanking one 12.4%, zeroing its words 5.7%, its constructor 4.3%,
building one from an integer 3.9%, and masking the bits above its declared width a further 8.4%. The
design's own body is **3.3%** of its own run.

With the run-moving gone, the wide case's top is the same one: blanking a value, zeroing its words,
masking above its width. The two profiles agreed about the rest all along and disagreed only about
what sat on top of it, which is the reading to keep -- one case's profile is not the engine's
answer. What both are waiting on is the open entry below: a value operation is a call into a
separately compiled runtime, the optimizer cannot see into it, and letting it see runs the same case
in half the time. What the profile adds is that the calls it cannot see into are mostly not
arithmetic -- they are a value coming into existence.

**Retaken after making a value cheaper, over three cases, and the top is no longer one family.**
What is left sorts into three. The largest is **a fact the compiler settled reaching the run as a
value to work out again**: a part-select whose bounds are literals is resolved while running through
four-state value arithmetic, at about 2,100 instructions a select and 43% of the representative
block; a conversion between two types the compiler already knows costs about 360 instructions and is
28% of scalar arithmetic. Second is **every operation building a fresh value and every store
comparing the old one against the new**, which is 13% and 5% of the representative block and 15% and
12% of scalar arithmetic. Third, and only where the design waits on a clock, is **every wait
allocating**: about 12% of the clocked pipeline is the allocator, beside building a trigger list and
subscribing per wait. The design's own body stays under 5% in every one. The first of the three is
closed below.

- [x] An integral value's dimension stack no longer allocates for the single-dimension case. Every
      declared integral carried its stack in a growable container, so constructing or copying any
      value -- which generated code does at every operation, since the descriptor is built at the
      use site -- reached the allocator. Measured at 17% on the clocked-pipeline case and 26% on the
      NBA-heavy one, and 14% on a case that is scalar arithmetic with no arrays and no scheduling,
      which is what places the cost per value rather than per aggregate. The value grew from 144 to
      168 bytes in exchange, so a whole-array copy moves more; the trade was measured rather than
      assumed, and on a design whose arrays dominate it could go the other way.

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

      Writing each element of a 32768-element unpacked array of 32-bit elements in turn cost 1.55 ms
      per element, against 1.16 us to read one back: the cost of a write tracked the size of the
      array it landed in rather than the size of the element, which is the signature of the copy.
      That figure is 5.5 MB of value moved in 1.55 ms, or 3.5 GB/s, the measuring machine's memory
      bandwidth -- so the write was bounded by copying the array and by nothing else, and only not
      copying could recover anything. It is now 1.5 us per element, a thousandfold, and reading one
      back is 0.48 us: a write and a read are the same order, which is what "the write touches one
      element" means and is the thing to re-measure if this ever regresses.

      Two of the four predictions written down before the work landed as stated -- the write family
      left the outlier band, and a write came down to a read's order. The third was the independent
      check and it held with a number higher than predicted: the unpacked-array read case reports a
      read rate for the first time, at 1,069x rather than the few hundred expected. The fourth,
      about three times on the integration design, is not yet measured.

      What the correctness half of this cost is not fixed by it, and is fixed separately: a
      value-change event control now decides for itself, by comparing the value of its own
      expression, so what a leaf's bit window does is pass over a wait a write cannot have reached
      rather than decide one. Tracked with the construct it affects, in
      [processes.md](processes.md).

- [x] Converting a packed value between widths or between the two- and four-state domains no longer
      walks it one bit at a time. It was the single largest cost in both profiled programs, and the
      reason is that the front end inserts such a conversion wherever an expression's type differs
      from its context's, so a design pays it on ordinary assignments rather than on anything a
      reader would call a conversion.

      Measured before and after at the same amount of work. On the mixed arithmetic-and-control
      case, conversion was over four tenths of the run's instructions and is now under one tenth; on
      the clocked-pipeline case it was about a sixth and is now a fiftieth. One conversion of a value
      in the tens of bits cost thousands of instructions and now costs a couple of hundred, and the
      first case as a whole runs in about two thirds of the instructions it did.

      The separate pass that cleared the destination first is gone rather than made cheaper: it
      existed only because the per-bit copy wrote a subset of the destination, and a word-wise one
      writes every word. [refactor.md](refactor.md) R91 holds the shape and what was left out.

- [x] The sequence type behind a value's words and dimensions no longer holds its inline storage and
      its spill container at the same time. A value that never spills used to carry the spill
      container anyway and copy it on every copy; both of the type's uses want capacity one, so the
      general form was a cost with no consumer. An integral value is now 120 bytes rather than 168.

      What the size bought was not what made it faster, and the two came close to cancelling. The
      first cut reached the short case through a plain pointer and a run-time length, which costs
      the compiler exactly what an inline buffer is for -- it no longer knows how much room there is
      or that the length cannot exceed it -- and that measured **9% slower** on scalar arithmetic
      while the value was already 48 bytes smaller. Settling which side a write is on before naming
      the room, rather than after, is what turned it around.

      Measured by interleaving both builds inside each round, five rounds, against the benchmark
      corpus: **6.8% on scalar arithmetic with no arrays and no scheduling, 5.5% on the mixed
      arithmetic-and-control case, and 6.4% on wide bitwise work over 256-bit values**. The last of
      those is the case that spills, so narrowing the value costs the wide path nothing.

- [x] An integral value no longer carries how its declaration divides its bits, and no longer
      re-establishes while running what the compiler settled before it ran. A value now states how
      many bits it has, whether they are read as signed and whether a position may hold x or z; an
      operation that names a position inside one takes the declared division as an argument from the
      site that wrote the position, which is what every other selectable family already did. The
      value went from 120 bytes to 64 -- the width had been stored two or three times over and the
      state domain twice, once as a flag and once as a discriminant kept in step by a runtime check.

      Two checks went with it, and the reason they could go is the same one: a view over a value's
      words was bounds-checked against those same words on every construction, where the relation
      holds by construction, and it also carried a bit offset that every construction set to zero
      and forty-four sites checked was zero. Operand-shape checks between two values stay, because a
      mismatch there is a missing conversion rather than something construction excludes. An
      operation now writes its result's bits into the result instead of into a buffer that is then
      validated and copied, which also removes a second pass masking bits the first pass had
      already masked.

      Measured by interleaving both builds inside each round, five rounds per case: **1.71x on
      scalar arithmetic, 1.95x on wide bitwise work over 256-bit values, 2.01x on a tight
      call-and-loop case, and 2.09x on the representative compute block**. Each column's own spread
      runs from 1.2% to 15.4% depending on how quiet the machine was, and every gap clears its own
      round's spread several times over. The model is
      [../decisions/packed-shape-belongs-to-the-type.md](../decisions/packed-shape-belongs-to-the-type.md).

- [x] A value the source fixed is no longer built wherever the program reaches it. The middle layer
      had literal forms for machine scalars and none for a value of the language it expresses, so an
      integral constant lowered to a call asking a library to make one -- which is the one form an
      optimizer cannot fold, in a layer whose primitives are chosen for an optimizer to fold. A unit
      now holds the constants it was written with, interned on the bits and the type together, and
      an occurrence names the entry; each backend gives that entry the storage its target has for
      something the whole run shares.

      Measured by interleaving both emitted programs inside each round, five rounds, one runtime
      archive and one compiler throughout, on the representative compute block: **1.21x with the
      design compiled unoptimized, which is the default the edit loop uses, and 1.23x compiled
      optimized**. The reading that decides it is the third: with the value layer's definitions made
      visible to the optimizer, the same comparison is **1.29x** -- so this is not a cost an
      optimizer was going to recover, and the entry it is recorded under says why. The model is
      [../decisions/a-constant-is-stated-not-computed.md](../decisions/a-constant-is-stated-not-computed.md).

      The execution backend reads **1.15x** on the clocked-pipeline case, over four rounds after a
      cold first one. It is a different case and the two figures do not compare: the case the other
      column measures cannot run on that backend at a length worth timing, for the reason the
      refactor queue now records, so what each column says is that its own backend moved and by how
      much.

      The same case now sustains **1,634 table passes per second, 67x off Verilator** (2026-09-22).
      That is an absolute rate rather than a comparison, recorded because the ratios above compare
      against a shape the tree no longer contains: re-deriving one needs a second toolchain built
      from before the change, while a rate can be read again any day. What a later reading of it
      answers is whether the gain held, which the ratios on their own cannot say.

- [x] Making a value no longer pays for its planes twice, and the runtime no longer calls its own
      functions as though another library might replace them. A four-state value that fits one word
      held each plane in a sequence of its own, each with its own length and room; both planes now
      share one run of words that holds them in place, and the value is 48 bytes rather than 64.
      Clearing the bits above the width after an operation, one instruction of work, had been a call
      into another file; it is now in view of every operation that ends with it.

      That half bought **2.6%** of the representative block's instructions, because a value fitting
      one word had not been reaching the allocator in the first place. Finding why it bought so
      little found the rest: the shipped runtime is compiled position-independent, and under that
      mode one of the two compilers treats every function the runtime exports as replaceable by a
      same-named one loaded at run time, so it neither inlines one into another nor calls it directly
      -- a four-instruction function in the same file was reached through the procedure linkage
      table. The runtime ships as a static archive linked into the program, where nothing can replace
      anything, so the assumption only costs; it is now stated off for every target.

      Measured on the representative block at the same amount of work: **1,004,760,115 instructions
      before, 978,301,527 with the value change, 659,009,314 with both** -- a third gone, with the
      same result printed. The rate went from **1,408 to 2,976 table passes per second**, 37x off
      Verilator, 2026-09-23.

- [x] A select no longer hands the run the source's coordinates to resolve again. The bounds of
      `v[2:0]` used to reach the run as two four-state values, a form and the declaration's shape,
      and the run recovered where the selected bits start with four-state arithmetic on every
      evaluation -- about 2,100 instructions a select and 43% of the representative block. Every
      select now names a position in the selected value's own numbering and a count, the declaration
      is read where the select is written, and an index is moved by it only where the declaration
      does not number from the index's own zero. An integral operation whose operands are all
      constants is itself a constant, so a conversion of a literal is no longer performed each time
      control reaches it.

      What the entry called a conversion between two known types turned out to be two things. A
      conversion of a constant was a fact the compiler settled, and folds away with the rest. A
      conversion of a value the program computes -- `longint'(a)`, which scalar arithmetic does
      three times an iteration -- has nothing left to settle; its cost was a general word loop
      where one word suffices, and a conversion between two values of one word now takes a
      straight-line path.

      Measured 2026-09-23 against `212505b0`, by instruction count at the same work: the
      representative block 1,200,065,357 -> 829,304,434 (-30.9%), and **3,237 -> 4,711 table
      passes per second, 24x off Verilator** (from 35x); scalar arithmetic 7,380,008,890 ->
      6,408,004,232 (-13.2%), and 6,684,983 -> 6,804,700 iterations per second -- the instructions
      the one-word path removed did not move the rate by much. The model is
      [../decisions/a-select-names-a-position.md](../decisions/a-select-names-a-position.md).

- [ ] An emitted program is optimized without the runtime library it spends its time in. The
      design's translation units and the runtime are compiled separately and linked as native
      objects, so every value operation a design performs is a call the optimizer cannot see into --
      which is what the middle layer's own contract already says about a runtime helper call, adding
      that link-time optimization mitigates it but does not eliminate it.

      Measured 2026-09-17 on the representative compute block, interleaved, five rounds, the same
      runtime and the same compiler throughout: compiling the design optimized and letting the
      optimizer see the value layer's definitions runs the case in **half the time** of the same
      design optimized without them. At this scale it also costs no build time -- 11.6 s against
      13.7 s, because each unit then emits only an intermediate form and the optimization happens
      once at the link.

      **What is not measured is the only thing that decides it**: what that single link step costs
      on a design of a thousand units, where it is one serial whole-program pass rather than three
      files. That is the end-to-end trade `north_star.md` puts first, and it is the number to get
      before this is switched on rather than offered.

      Nothing pulls the other way any more. The value layer's operations are compiled once, in the
      shipped runtime, and a unit calls them; the copies a unit's object once carried, which an
      earlier reading put at three quarters of its symbol bytes and attributed to those
      operations, were the runtime's own entries and families, and those are compiled once too
      now. So what decides this is the link cost above and nothing else.

      **Target shape**: the runtime ships in a form the optimizer can read, and a design build says
      whether to use it. It is the run-time half of the same axis `--release` already names, so it
      belongs to that flag's question rather than to a new one.

## Construction / compile-time performance

The cost of producing compiled artifacts and of building the object graph at time zero. The primary
constraint is `north_star.md`: compile-time work scales with the number of distinct unit
specializations, not with instance count.

- [ ] Specialization dedup. Today every distinct parameter binding produces its own compiled
      artifact, because unit identity is keyed on the frontend's per-parameter-set elaboration. This
      over-forks: two instances whose generated code is identical except for a folded constant
      compile twice. The target is `specialization_model.md`: classify each parameter as a
      code-shape-affecting input (enters the specialization key) or a constructor/config input
      (flows in at construction), emit one artifact per distinct code shape, and let value-only
      parameters differ per instance without forking the artifact (LRM 23.10). Functionality does
      not depend on this -- a correctly-identified per-binding artifact already behaves correctly
      (see `hierarchy.md` Stage A); dedup only reduces how many artifacts exist. The identity
      mechanism is unchanged from the functional case: the same canonical binding serializer
      (`docs/decisions/specialization-identity.md`) is fed only the code-shape-affecting subset, and
      value-only parameters are demoted to constructor inputs.

- [x] The generate axis of that same sharing. A `generate for` used to lower concretely: N
      iterations became N scope classes and N construction statements, so the artifact grew
      one-for-one with the iteration count even when no parameter varied and every body was
      identical. Measured over 16, 64, and 256 iterations of one trivial child, emitted lines and
      generated classes both scaled linearly with N -- about 4.2 KB per elaborated block in the
      bodies file and 1.85 KB in the declarations header, the second of which every referrer parses.
      This is the one item here that was not a cost but a wall: past some width the host C++
      compiler does not finish, and a design that does not compile has no behaviour to be correct.

      A loop's blocks now compile to one class the constructor builds once per index it counts out,
      and the index reaches the block as a value that construction supplies rather than as a constant
      folded into its body -- which is what lets the body of a loop that uses its index be one body
      at all. Measured at 256 iterations of a block declaring one variable and writing `sink[i] = i`:
      9,666 bytes of bodies and 7,072 of declarations, which is what four iterations emit, against
      1,064,336 and 765,273 before; the host compile of that emitted project goes from 16.8 seconds
      to 3.5. Where the index instead fixes something a class settles once -- a width, a
      specialization, which members exist -- separate compilation is the correct answer and stays,
      which is `specialization_model.md` invariant 1.
      [../decisions/one-body-built-at-every-index.md](../decisions/one-body-built-at-every-index.md)
      holds how a body qualifies, why the question deciding that is checked rather than trusted, and
      what a scope's construction receives.

      **A block naming a constant worked out from its own index used to fall outside that and no
      longer does.** It reads as a case the sentence above already covers and was not: the index
      reached the block as a supplied value, and a constant derived from it was still folded, so
      one level of naming put the block back to one class per index. Measured at 256 iterations of
      a block declaring `localparam int K = i * 3 + 1` and adding it to a signal: 2,236,192 bytes of
      design C++ before and 21,111 after, which is what four iterations emit. Such a parameter is
      now a declaration of the block holding the expression the source wrote, so blocks that wrote
      the same expression state the same thing. The positions where the value reaches a **type** --
      a declared width, an unpacked extent -- are unchanged and correct: two types are two classes,
      and that is the sentence above rather than an exception to it.

      What it costs where nothing is gained, measured rather than waved at: a block that cannot
      share either way now carries the declaration instead of the folded literal, which is **359
      bytes per block** -- 15 blocks declaring a width through a named constant emit 154,362 bytes
      against 148,978 writing the index straight into the width. Against 8,693 bytes an iteration
      removed where the block does share, and the blocks paying it are the ones compiled apart for a
      reason of their own.

      What this does **not** touch is the front end's own elaboration, which still produces a symbol
      per block, so the lowering phase stays linear in the iteration count and its wall clock and
      peak memory stay roughly where they were. What goes away is the emit and the host compile.

      `runtime_model.md` states this one as an absolute ("if you find compile-time work that scales
      with instance count, the design has been violated") where `specialization_model.md` invariant
      1 admits the concrete form as correct and scopes the requirement to the optimized steady
      state. The two are reconciled by the entry above, with the specialization model governing: the
      concrete form stays the baseline and the shared form is what a program gets when a proof
      exists for it.

- [ ] What remains of the instance array on that same axis. The member and the construction
      statement no longer scale: `Child c[0:N-1]` is one member whose type carries the multiplicity
      and one statement composing the whole sequence, so widening an array changes neither. Two
      things still do. The composition names every element, so the constructor holds N element
      expressions; reaching one loop needs a way to build a sequence from a count rather than from
      its elements, which the value model does not have -- a sequence is composed where it is built.
      And a port connection on an array is still distributed per element by the frontend, so its
      endpoint members and its implied continuous assignments stay one per element; collapsing those
      needs the peer to be a function of the index, which is exactly what the distribution spent.

- [ ] What a port binding costs at time zero. Measured 2026-09-17 over 500 instances of one trivial
      child, built optimized, against an otherwise identical design whose child takes no ports and
      whose parent holds the same cells: each binding adds about 7 microseconds to elaboration. It
      is linear in the number of bindings -- two per instance and three per instance give the same
      per-binding figure -- and does not depend on whether the port is driven across the boundary.
      About half of it is kernel time, and that half grows with the binding count and with nothing
      else the design holds, which points at an allocation per binding rather than at the work of
      settling one. What makes it worth an entry is the scale: a design with a hundred thousand
      ports spends most of a second before any process runs, and elaboration is paid on every run
      rather than once per edit.

      Asking the declaring unit for a published member is not what this is. That is one indirect call
      per binding, three orders of magnitude under what a measurement at this size can separate --
      the instrument here resolves a couple of microseconds per binding, so the reading bounds the
      call from above and says nothing else about it.

- [x] Writing out the target text no longer costs more the deeper a design nests its expressions.
      Every part of an emitted artifact used to be built as a value and copied into the value
      enclosing it, so a byte near the bottom of an expression was copied once per level above it,
      and the punctuation between the parts was a format description read again at run time for
      every part written. Each byte is now written once, where it goes.

      Measured as instructions over one emission, because the host this was taken on is shared: 15.6%
      fewer over 1.3 MB of C++ from many small units, and 17.2% fewer over 2.0 MB whose expressions
      nest two hundred deep. The text produced is identical, which is what the whole conformance
      corpus was emitted twice to establish. [refactor.md](refactor.md) R137 holds the reading and
      the prediction it falsified.

      Names and type spellings were left out of that at first, on the ground that a name has
      readers besides the artifact, and they were the larger half: over one optimized emission of
      256 distinct unit specializations, the format calls composing them were 30.5% of the run.
      They are written the same way now. That emission costs 25.7% fewer instructions --
      683,818,727 to 507,852,755 -- and the one nesting two hundred deep 17.3% fewer, with the text
      identical over the whole conformance corpus. Measured as a rate on the same host, an
      optimized build writes about 43 MB of design C++ a second.

      This is a rate rather than a count, so no coverage record holds it and the corpus cannot see
      it: every conformance case is one small design, and what it measures only shows at a scale
      none of them reach.

### Open questions

- How thin the specialization key goes. The fat-value runtime representation carries packed width
  and unpacked size as runtime fields rather than as distinct types
  (`decisions/integral-representation.md`, `decisions/unpacked-array-representation.md`), so for the
  C++ backend a width or size parameter does not change generated code and could be a constructor
  input. A width-templated backend (the future LLVM `iN` lowering) does specialize on width. Whether
  per-width specialization is a key axis or a backend-internal monomorphization downstream of one
  width-generic artifact is unresolved, and it conflicts with `specialization_model.md` invariant 3
  as written (which lists packed width as code-shape-affecting). Resolve before keying on width.
