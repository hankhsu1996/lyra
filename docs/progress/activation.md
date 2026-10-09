# Activation model: gaps to the contract

Tracks where the runtime's execution code differs from the golden activation model in
`../architecture/activation.md`. Each entry names the current shape, the contract shape it must
reach, and what (if anything) blocks it.

The runtime already realizes the load-bearing core of the contract: an activation is a coroutine
frame; the scheduler holds a payload-neutral activation token and never sees the completion type; a
suspending callable's result type is `Coroutine<T>` and a typed await consumes its value; a spawned
process is attached to its spawner's lineage, which is both the ownership and the
cancellation-domain relation and outlives the execution it named while any descendant is live; and a
join waits on each branch's termination. The gaps below are where the current shape is still wrong
or incomplete relative to the contract.

## Items

- [x] **The terminal outcome is unified.** Contract invariant 2: a body is left in exactly two ways,
      so an activation settles one completion slot holding either the value it produced or the
      departure that reached its landing, and the scheduler-visible execution core carries none of
      it. A consumer reads the whole outcome once, which yields the value or carries the departure
      on past a frame that is not the landing. The execution core the scheduler sees is purely
      suspend / resume / wait state / identity. Killed-ness is separately a persistent fact of the
      process node, read by `status()` / `await()`, because a process killed while parked is
      released without its slot ever being read.

- [x] **Every reference to a parked activation ends with its owner.** Contract invariant 4: every
      external reference to a parked activation -- a region queue, a delay slot, an event, a value
      change, a process's termination, a disable target -- is a membership recorded once, owned by
      what shares its life and merely linked by the target: the activation for its queue place, the
      wait its frame holds for what that wait watches, the process for its being inside a target.
      Because the relation has a single record rather than a copy on each side, leaving is a detach
      -- neither end searches the other, and neither can hold a belief the other has abandoned.
      Releasing an activation releases its frame, and with it every wait it held and its queue
      place, so nothing is left able to resume it; waking only queues, so a bulk kill can wake as it
      goes even where a later step releases one it woke. Adding a kind of wait means giving a target
      a list, not teaching the scheduler a new way to be searched.

- [x] **Cancellation over the dynamic domain settles a reader.** Contract: a cancellation domain --
      a relation distinct from ownership and continuation -- decides which activations are cancelled
      together; disabling takes each activation off whatever could resume it, cancels the owned
      descendants, then releases the frame. Current shape: `disable fork` (LRM 9.6.3) and `kill`
      (LRM 9.7) both walk the process lineage, which is that domain, and terminate the whole
      descendant subtree -- including the descendants of subprocesses that have already terminated.
      Both funnel through one terminal transition: each terminated node is marked KILLED and its
      frame released, and releasing a frame ends every wait it held and its queue place, so nothing
      -- queue, wait, or a handle held past the kill -- is left able to resume it.

      Cancellation now has a reader. `process::status()` reports a killed process as KILLED through a
      handle that outlives it, and `process::await()` (LRM 9.7) suspends a process until another
      terminates -- normally or forcibly -- then reads that outcome. KILLED is realized as a
      persistent fact of the process node, a terminal cause distinguishing a finished process from a
      killed one, not as a `Cancelled` value in the completion slot: a cancelled activation is
      released while parked, so its slot is never read. Cancellation is a lifetime event observed
      through status / await, not a third terminal value the consumer reads from the slot.

      Killing the calling process or one of its ancestors is a deferred safe-boundary termination: a
      running body cannot destroy the frame it executes in, so the target is marked, the calling body
      unwinds to the scheduler's resume boundary, and the frame is torn down there, while every
      off-stack subtree is released at once.

- [x] **Pause and resume settle on the disposition model.** `process::suspend()` / `resume()` (LRM
      9.7) pause and restart a process. A blocked activation is parked on a wait its frame holds,
      whose memberships stand for the wait's whole life: a suspend takes the activation off that
      wait and keeps the wait, and a resume asks it afresh -- parking there again, or running in the
      current time step where what it waits for has already happened. Each suspending construct
      supplies that uniformly, so its own resume rule reads in one place: an event control or named
      event measures from what it is worth again (an occurrence during the stop is missed), a delay
      compares the absolute moment it is waiting for (one that has passed continues at once), a
      `wait` condition is read by the body's own loop (a condition that became true during the stop
      continues at once), and a monotonic condition (join, wait fork, await) is re-checked. Holding
      nothing is how an activation that was already runnable says it has nothing left to wait for.
      `status()` reports SUSPENDED. The four positions run on both backends. See the activation
      contract and the waiting-is-an-operation decision.

- [x] **Disable of a named block or task.** `disable` (LRM 9.6.2) selects its target by static block
      or task identity and reaches every execution currently inside it, without regard to the
      process lineage: each such execution resumes after the scope, and every activity enabled
      within it terminates. Unlike `disable fork`, the cancellation domain is static declaration
      identity, not the dynamic lineage. The target is invalidated and every affected execution
      leaves it through one control effect the naming region consumes (see the
      disable-scope-invalidation decision). Implemented for a named block, a named fork, and a task,
      at any nesting depth, reached from the same process or a concurrent one, and across every
      activation of a reentrant task. Membership is carried by the running process rather than
      derived from where a body is written, so it spans a call: a task called from inside a disabled
      scope terminates with it instead of running to completion. An activity spawned inside the
      scope takes that membership at the spawn, so a fork child terminates whether it was spawned
      directly, spawned by a task called from the scope, or not yet started when the disable landed;
      such a child reports KILLED (LRM 9.7), the same status a `disable fork` or `kill` gives it.
      The effect arises where an execution regains control -- a wait resuming, the `disable`
      statement itself -- and a spawned activity disabled before it ever runs is terminated without
      executing a statement. What a body carries is the region alone, and both backends run the
      construct the same way, by leaving the region the way the target they emit leaves a scope from
      within. Which targets an execution is inside is the execution's own state, so a spawned branch
      is enclosed by the targets its spawner was inside even though its body states no region, and a
      `disable` of a named `fork` reaches a branch parked on a delay. Where the target lives follows
      what replicates it: a scope of the design hierarchy is replicated per instance and keeps one
      target per instance, while a class method's block is one target for the class -- a method is
      automatic (LRM 8.6) and LRM 9.6.2 disables a block inside an automatic task for every
      concurrent execution of it, so one object's `disable` ends the block in every other object
      running it -- and a package subroutine's is one for the program. Disabling a task runs on both
      backends, the one without exceptions included, since a task enable there is an await on a body
      that completes as a coroutine and the region is built from the body's own ways out.
  - [x] A `disable` whose target another module instance, generate scope, or interface declares --
        `disable u.blk`, `disable c.tk`, `disable g[0].blk`, and the same through an interface
        instance or an interface port -- naming it by a hierarchical path (LRM 23.9). What a name
        reaches there is an object on the design hierarchy, which every other cross-instance
        reference already reaches through one route mechanism, and the target took its own place in
        that vocabulary rather than a second way to address one. Nothing about the model moved:
        which source a `disable` invalidates and what leaving it does are as they were.

        The same route serves a target in a generate block of the reader's own module, named from
        the module body or from a sibling generate block, so what the statement can reach is decided
        by the target it names and not by the position it is written from. A target the writing
        body's own declaration scope declares needs no route and keeps the identity it always had.
        Both backends carry it, because what a route seals here is an address.

        `disable pkg::t` is not among them and is refused with a located diagnostic. A package has
        no instance and so no object on the hierarchy a route walks, which is the same reason a
        package variable is reached by name rather than by a route; what a target there would need
        is that by-name form, and it is a target family of its own.

- [ ] **Runtime vocabulary trails the model.** The execution code names the activation and its core
      in coroutine-implementation terms; the contract's vocabulary is activation / completion slot /
      cancellation domain / join, with the coroutine mechanics as one realization. The activation,
      its execution-state axis -- a process's states are execution states, and its end state is
      outcome-neutral termination rather than "completed" -- the terminal outcome's completion slot,
      and the join as a wait on its branches' termination are named for the model. Rename the rest
      where it clarifies the boundary between execution control and completion. Low priority;
      unblocked.
