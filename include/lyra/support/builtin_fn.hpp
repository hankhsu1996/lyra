#pragma once

#include <cstddef>
#include <cstdint>
#include <optional>
#include <string_view>
#include <variant>

namespace lyra::support {

// Closed namespace of compiler-recognized runtime entries. Same identity at
// HIR and MIR; lives in the support layer so neither layer's vocabulary
// imports the other's. The receiver type at the call site names the runtime
// library type whose method is being invoked (value-layer containers,
// observable storage cells, runtime effects, scope handle); this enum
// carries only the method identity.
enum class BuiltinFn : std::uint16_t {
  // LRM 7.4 / 7.8 / 7.10 / 11.5 access into a value. The plain form answers
  // with the part's value; the `Ref` form answers with the part itself, so what
  // stands there may be written and a further access composes onto it.
  //
  // Two entries rather than one entry read two ways: which of them a source
  // position calls is settled where the source is read, and a consumer that had
  // to work it out from the position could work it out differently.
  // Composition is the receiver -- an access whose receiver is another access
  // is the descent -- so a descent of any depth is these entries applied one
  // per level and nothing states a path.
  kElement,
  kSlice,
  kElementRef,
  kSliceRef,
  // The same two directions over a part named by its declaration-order
  // position rather than by a coordinate. One pair covers a product and an
  // active-member value alike: what differs is whether every part is live at
  // once or one at a time, and whether reaching one for writing settles which
  // that is -- all of which is the value's own semantics, reached through the
  // domain its type names, exactly as a coordinate step reaches a queue's rules
  // or an associative array's.
  kPart,
  kPartRef,
  // Which member an active-member value holds, and building one that holds a
  // given member. Only a value carrying an observable tag has the first, and a
  // product has no counterpart for either.
  kTagMatches,
  kMakeActiveMember,
  // Yields the receiver when a condition holds and raises the given message as
  // a simulation error when it does not. The shape for a check the language
  // requires to run as part of evaluating an access rather than ahead of it
  // (LRM 11.3.5): passing the receiver through lets the access compose onto the
  // guard, so a guarded access stays the ordinary one above.
  kRequire,
  // LRM 7.4.3 / 7.5 / 7.9 / 7.10.2. AA's `num` is an alias of `size`;
  // String's LRM 6.16.1 `len` is its own mandated spelling.
  kSize,
  kLen,
  // LRM 20.6.2 `$bits` over a dynamically sized value: the bit count of what it
  // currently holds. Sums each element's own bit count, so an element that is
  // itself dynamically sized contributes its current width. The fixed-size case
  // folds at elaboration and never reaches this entry.
  kBitstreamWidth,
  // The bits themselves, in the order LRM 6.24.3 fixes for the value's type:
  // the first item occupies the most significant bits, an associative array
  // contributes in index-sorted order, and a class contributes its base's
  // members before its own. One entry reads a value out as that sequence and
  // one builds a value from it, and both recurse the way the width query above
  // does -- a part reports its own bits, so no caller inspects the part's
  // shape.
  //
  // Building takes a prototype because a sequence of bits carries no shape: the
  // result is the prototype's type, held at the prototype's representation, and
  // the sequence is consumed left to right. The caller brings the sequence to
  // exactly the width the prototype reports, so neither widening nor truncation
  // is this entry's business.
  kToBitstream,
  kFromBitstream,
  // Reversing the order of the fixed-size blocks a vector divides into, from
  // its least significant bit up, leaving the bits inside each block where they
  // are (LRM 11.4.14.2). The last block is whatever is left over and is not
  // padded. A generic bit operation -- LLVM's `llvm.bswap` is this at a block
  // size of eight -- so the block size reaches it as a machine count.
  kReverseBlocks,
  // LRM 7.12 / 7.5 / 7.10 container ops. Emptying a container and dropping
  // the one entry an index names are two operations the source spells with one
  // word (LRM 7.9.3 / 7.10.2.3), so each takes its own identity here and the
  // spelling is resolved where the source is read.
  kToOwned,
  kDelete,
  kDeleteIndex,
  // LRM 7.12 ordering. `Sort` / `Rsort` take a `with`-clause closure as
  // the second argument.
  kReverse,
  kSort,
  kRsort,
  // LRM 7.12.3 reductions. All take a `with`-clause closure.
  kSum,
  kProduct,
  kAnd,
  kOr,
  kXor,
  // LRM 7.12.1 search.
  kFind,
  kFindIndex,
  kFindFirst,
  kFindFirstIndex,
  kFindLast,
  kFindLastIndex,
  kMin,
  kMax,
  kUnique,
  kUniqueIndex,
  // LRM 7.12.5.
  kMap,
  // LRM 7.10 queue insertion / removal.
  kInsert,
  kPopFront,
  kPopBack,
  kPushFront,
  kPushBack,
  // LRM 7.9. The traversal family (LRM 7.9.4 -- 7.9.7) writes the
  // visited key through a `ref` index argument and returns 0 / 1 / -1.
  kExists,
  kAssocFirst,
  kAssocLast,
  kAssocNext,
  kAssocPrev,
  // The smallest and largest currently allocated index (LRM 20.7 `$low` /
  // `$high` over an associative dimension). Each takes the value to report when
  // no index is allocated -- the index type's default, which is `'x` for a
  // 4-state index, as LRM 20.7 requires.
  kAssocMinIndex,
  kAssocMaxIndex,
  // LRM 6.16 string methods.
  kGetc,
  kPutc,
  kToupper,
  kTolower,
  kCompare,
  kIcompare,
  kSubstr,
  // LRM 6.16.9 -- 6.16.13 string parse.
  kAtoi,
  kAtohex,
  kAtooct,
  kAtobin,
  kAtoreal,
  // LRM 6.16.14 -- 6.16.18 string format. Mutates the receiver.
  kItoa,
  kHextoa,
  kOcttoa,
  kBintoa,
  kRealtoa,
  // LRM 15.5 named-event operations. `Trigger` arises from `-> e;` collapsing
  // at HIR -> MIR; waiting for one is the ordinary wait, naming the event as a
  // leaf. `Triggered` is the LRM 15.5.3 same-time-step query.
  kTrigger,
  kTriggered,
  // LRM 16.9.3 sampled value history operations, all reached through the
  // history's own address. `Install` fills it with the expression's default
  // sampled value and fixes how far back it reaches, which the design does
  // where it activates; `Push` records what a tick settled; `At` answers with
  // what the tick a read names settled, counting back from the most recent.
  kSampledHistoryInstall,
  kSampledHistoryPush,
  kSampledHistoryAt,
  // LRM 16.14.1 concurrent assertion evaluation, all reached through the
  // storage that holds one assertion's attempts. `Install` fixes how wide a
  // position set is, what a pending attempt is owed when the run ends, and the
  // statements an outcome selects. `BeginTick` opens the attempt this tick
  // starts and makes every live evaluation one the tick has not stepped;
  // `DisableTick` is the same tick with the disable condition true, which
  // discards every attempt and starts none (LRM 16.12). `LiveWord` bounds the
  // Boolean expressions the tick has to read, `NextUnstepped` walks the
  // evaluations the tick still owes a step, and `BitsAt` / `SetWord` / `Step`
  // read a position set, replace it with its successor, and record what the
  // tick left it in. `Seed` starts an evaluation of an implication's
  // consequent in the attempt whose antecedent matched. `Settle` answers every
  // attempt the sweep resolved and submits its statements to Reactive.
  kEvaluationAttemptsInstall,
  kEvaluationAttemptsSeedWord,
  kEvaluationAttemptsBeginTick,
  kEvaluationAttemptsDisableTick,
  kEvaluationAttemptsLiveWord,
  kEvaluationAttemptsNextUnstepped,
  kEvaluationAttemptsBitsAt,
  kEvaluationAttemptsSetWord,
  kEvaluationAttemptsStep,
  kEvaluationAttemptsSeed,
  kEvaluationAttemptsSettle,
  // LRM 20.9 / 21.3.4.3. 2-state packed types return false; downstream
  // constant-folds those calls.
  kIsUnknown,
  // LRM 20.9 bit counting. The argument carries the control bits, one per bit
  // position; a receiver bit counts when it matches any of them, and a bit
  // value named by several control bits still counts once. Returns the SV
  // `int` shape.
  kCountBits,
  // LRM 20.8.1. ceil(log2) of the operand read as unsigned; $clog2(0) is 0.
  // A constant argument is folded downstream, never in lowering.
  kClog2,
  // LRM 20.8.2 Table 20-4: real-valued mathematics, each entry cross-listed
  // with the C standard math library function whose behavior the standard
  // defines it to have. Each dispatches on the real it operates on, and the
  // two-argument forms carry the second operand as an argument. The table's
  // `$pow` row has no entry of its own: it asks for exactly what LRM 11.4.3
  // `**` asks for, and one entry serves both spellings.
  kLn,
  kLog10,
  kExp,
  kSqrt,
  kFloor,
  kCeil,
  kSin,
  kCos,
  kTan,
  kAsin,
  kAcos,
  kAtan,
  kAtan2,
  kHypot,
  kSinh,
  kCosh,
  kTanh,
  kAsinh,
  kAcosh,
  kAtanh,
  // Installs a capability wrapper's declared representation once at
  // construction. It acts on the wrapper rather than on the storage the wrapper
  // represents, which is why it is a call at all. No runtime handle: it runs
  // before any process, so nothing is subscribed yet. Every later store
  // requires its value to already be at the installed representation.
  kInitialize,
  // Installing what a net's declaration gives it, once at construction: the
  // representation its data type fixes, and what its declared net type states
  // -- the contribution the net type itself makes, as the value the net shows
  // where nothing drives it and the strength it holds that value at (LRM
  // 6.6.5, 6.7.1). One entry per resolution -- tri-state for `wire` / `tri`,
  // wired-and for `wand` / `triand`, wired-or for `wor` / `trior` (LRM 6.6.1,
  // 6.6.3), and one that resolves tri-state and leaves its own contribution
  // holding what the drivers last decided, which is how a net stores a value
  // (LRM 6.6.4) -- because a truth table is applied rather than carried, and
  // an operation is named here. What the contribution is stays a value the
  // call carries.
  kNetInitializeTriState,
  kNetInitializeWiredAnd,
  kNetInitializeWiredOr,
  kNetInitializeRetaining,
  // Reading what a cell holds, and replacing it. Both act on the wrapper rather
  // than name its storage: a read answers with a value the cell decides how to
  // produce, and a write publishes the change to whatever the wrapper relates
  // to in the object graph (LRM 4.3). Naming the wrapper itself is the bare
  // place, which is how rebinding a reference stays a different program from
  // writing through one.
  //
  // Neither carries a runtime handle. The wrapper reaches the ambient one,
  // which has the standing of a stack pointer rather than of program data. A
  // stored value must already be at the cell's installed representation, which
  // is the store boundary's job upstream, not this entry's.
  kLoad,
  kStore,
  // Reading what a cell held in the Preponed region of the current time slot --
  // its value before anything in that slot ran (LRM 4.4.2.1, 16.5.1). The same
  // operation on the wrapper as an ordinary read and it carries no handle for
  // the same reason; the two differ only in which of the values the cell holds
  // is asked for.
  kSampledLoad,
  // Arming a cell to answer for a sampled value at all, and installing the one
  // every read answers with until some later slot first changes the cell (LRM
  // 16.5.1). A cell nothing samples is never armed and carries neither the
  // storage nor the work of maintaining it.
  kArmSampling,
  // Asking a cell for its storage as somewhere to write, which is an operation
  // on the wrapper for the same reason the two above are: which storage it
  // currently stands for is a fact about the wrapper, not about the place
  // naming it. It answers with a borrowed pointer, so the ordinary dereference
  // names the storage through it and a write that reaches one part of a value
  // costs that part rather than the whole.
  kOpenForWrite,
  // Attaching a driver to a net (LRM 6.5), at the strength its source drives at
  // (LRM 28.11): a `ResolvedNet` method returning the driver handle the drive
  // capability is reached through. The strength is fixed when the driver
  // attaches rather than restated on every update, because it is a property of
  // the source and not of the value it puts on the net.
  kAttachDriver,
  // Joining two nets into one resolution (LRM 23.3.3.7): a `ResolvedNet`
  // method taking the other net, after which every driver of either is a
  // contribution to the same fold at the strength it drives at, and both nets
  // show what that fold produces. It states no direction, because the
  // connection it realizes has none (LRM 23.3.3).
  kNetJoin,
  // Putting a cell under a procedural continuous assignment and taking it back
  // out (LRM 10.6). Beginning one answers with the generation its evaluation
  // carries; driving states what that evaluation produced and answers whether
  // it is still the one in effect, which is how an evaluation superseded by a
  // later takeover, or ended by a `deassign` or `release`, learns to stop.
  kBeginTakeover,
  kDriveTakeover,
  kEndTakeover,
  // The ambient `RuntimeEffects` accessor. Zero-argument free function in
  // `lyra::runtime`. Every body kind -- module process, class method, package
  // function, class static method -- reaches the runtime through this,
  // uniformly and without a receiver. Backed by a per-thread pointer the
  // attached Runtime publishes for its lifetime.
  kCurrentRuntime,
  // Runtime scheduler submit operations, each a `RuntimeEffects` method taking
  // a closure: the NBA (LRM 4.4.2), postponed (LRM 4.4.2 / 21.2.2), and
  // Observed (LRM 12.4.2.1 violation report maturing) region commits
  // respectively. A submitted violation report is pending on behalf of the
  // executing process, which the runtime reads ambiently, so the call carries
  // only the closure.
  kSubmitNba,
  // LRM 9.4.5: the same NBA commit, into the region of the slot a delay control
  // names (LRM 4.4.2.4). The delay crosses as an amount in the scope's time
  // unit with that scope's unit and precision powers, the way a delay control's
  // does, because LRM 9.4.1 reads the amount before any scaling. One entry per
  // amount representation.
  kSubmitNbaAfter,
  kSubmitNbaAfterReal,
  // LRM 9.4.5, 15.5.1: an effect whose control is an event cannot say, where
  // the statement is reached, which slot it lands in, so it is carried by an
  // execution that waits for the event and then applies it. `RunDetached` takes
  // that execution as a coroutine and runs it apart from every lineage -- the
  // standard makes no process of the update, so `wait fork` does not wait for
  // it and `disable fork` does not reach it -- and `ResumeInNbaRegion` is how
  // it reaches the region the update is due in (LRM 4.4.2.4) once the event has
  // named the slot.
  kRunDetached,
  kResumeInNbaRegion,
  kSubmitPostponed,
  // LRM 16.5: a concurrent assertion's tick is evaluated in the Observed
  // region, and nothing withdraws it -- what it reads is sampled, so a flush
  // point of whatever submitted it cannot change the answer.
  kSubmitObserved,
  // LRM 12.4.2.1: a `unique` / `unique0` / `priority` violation report matures
  // in the Observed region unless the process that raised it reaches a flush
  // point first.
  kSubmitViolationReport,
  // LRM 16.4 deferred immediate assertion action commits, each a
  // `RuntimeEffects` method taking the action closure. The observed (`#0`) form
  // matures in Observed and runs its action in Reactive; the final form matures
  // and runs in Postponed. Both ride the executing process's deferred report
  // queue, which the runtime reads ambiently, so the call carries only the
  // closure.
  kSubmitDeferredObserved,
  kSubmitDeferredFinal,
  // File-IO subsystem accessor and cancellation token operations. `Files`
  // is a `RuntimeEffects` method returning the `FileTable` broker.
  // `CancellationFor` is a `FileTable` method taking a file descriptor and
  // returning a `ChannelCancellation` token snapshotted to the channel
  // currently bound at the descriptor. `IsCancelled` queries that token
  // (LRM 21.3.1 cancel-on-close), returning an SV bit.
  kFiles,
  kCancellationFor,
  kIsCancelled,
  // Print decomposes into a pure-value format step and a sink-write step.
  // `Format` is a `lyra::value` free function that walks the items and yields
  // an SV `string`; it takes the runtime's `$timeformat` state (for `%t`) as an
  // explicit operand the caller supplies from `TimeFormat`, so the format step
  // holds no runtime state of its own. `Write` / `Writeln` are `FileTable`
  // methods that emit that string to the descriptor's sink (LRM 21.2.1 /
  // 21.3.1); the `ln` variant appends a trailing newline.
  kFormat,
  // LRM 21.3.3 format string known only at simulation time. Like `Format` a
  // `lyra::value` free function yielding an SV `string`, but it parses the
  // format string and binds each operand as it goes, so it takes the format
  // text and a bare operand array in place of the pre-bound print items. The
  // hierarchical name a `%m` renders and the scope's time unit for a `%t` are
  // call-site facts absent from the format text, so they ride as operands too.
  kFormatRuntime,
  kWrite,
  kWriteln,
  // Diagnostic subsystem accessor and severity-fixed emit operations.
  // `Diagnostic` is a `RuntimeEffects` method returning the
  // `DiagnosticDispatcher` broker. `EmitInfo` / `EmitWarning` / `EmitError` /
  // `EmitFatal` are dispatcher methods taking a pre-formatted text (LRM 20.10),
  // one method per severity rather than a single emit-with-tag, mirroring the
  // `Write` / `Writeln` split. `EmitFatal` pairs with a subsequent `Finish`
  // call to realize the LRM 20.10 "implicit $finish" requirement.
  kDiagnostic,
  kEmitInfo,
  kEmitWarning,
  kEmitError,
  kEmitFatal,
  // LRM 16.3 immediate cover result. One entry records an evaluation together
  // with whether it succeeded, rather than one entry per outcome, because the
  // two counts the standard asks a tool to report are not independent: a
  // success is also an evaluation. The site operand is the statement's source
  // location, which is what tells one coverage goal from another.
  kRecordCoverage,
  // LRM 20.4.3 `$timeformat` display state on `RuntimeEffects`. `TimeFormat`
  // reads the current state, threaded into `Format` as the `%t` operand. The
  // setter takes the four `%t` display arguments (units power, precision,
  // suffix, minimum field width) as SV values; the reset form takes none and
  // restores the LRM Table 20-3 defaults. One entry per form, so which form a
  // call means is settled here rather than by counting its arguments.
  kTimeFormat,
  kSetTimeFormat,
  kResetTimeFormat,
  // LRM 21.3.4.3 scan primitives. The two parse entries are pure value-layer
  // parsers that differ only in whether a null character separates input
  // fields (LRM 21.3.4.3(a) grants that to `$sscanf` alone); one entry per
  // policy rather than one entry taking a mode operand, so which policy a call
  // means is settled here. The remaining two are the file-side bytes-and-
  // position operations a `$fscanf` lowering composes with the file parse.
  kScanString,
  kScanFile,
  kPeekBuffered,
  kAdvanceFd,
  // LRM 21.3 file-IO subsystem methods on the `files` broker. Each is a
  // FileTable instance method whose receiver is `runtime.Files()`; the
  // descriptor / FD operands are SV-typed packed values, so the lowered
  // call carries the same shapes the user wrote. `Open` returns the
  // descriptor value (LRM 21.3.1); the read family yields byte counts;
  // `Close` / `Flush` are void. The `kFile` prefix disambiguates from
  // string's `kGetc` (LRM 6.16) which targets a different receiver.
  //
  // Opening without a mode yields a multichannel descriptor and opening with
  // one yields a file descriptor (LRM 21.3.1); flushing an addressed channel
  // and flushing every open one (LRM 21.3.6) are likewise two requests; and
  // reading binary data into a packed variable is a different request from
  // reading it into a memory, which LRM 21.3.4.4 states as two variants and
  // gives different addressing. Each form is its own entry, so which one a
  // call means is settled here rather than by counting its arguments or by
  // reading the type of one of them.
  kFileOpen,
  kFileOpenMode,
  kFileClose,
  kFileGetc,
  kFileUngetc,
  kFileGets,
  kFileRead,
  kFileReadMemory,
  kFileSeek,
  kFileRewind,
  kFileTell,
  kFileEof,
  kFileError,
  kFileFlush,
  kFileFlushAll,
  // LRM 21.6 command-line plusargs. Free functions on `lyra::runtime` that
  // take the runtime handle plus SV `string` operands; the value form also
  // takes the output lvalue by reference (a `PackedArray` or `String`,
  // selected by C++ overload from the SV lvalue's declared type). Both
  // return an SV `int` (1 on prefix match, 0 otherwise).
  kTestPlusargs,
  kValuePlusargs,
  // LRM 20.17.1 $system. Running a command and asking after the processor are
  // different requests, so each is its own entry rather than one whose meaning
  // depends on how many arguments it was given. The commanded form takes the
  // runtime handle and the SV `string` to execute, and publishes whatever the
  // design has written before the command inherits the output. The null form
  // runs nothing, so it observes the host and needs no handle at all. Both
  // return an SV `int` carrying what the host reported.
  kRunHostCommand,
  kRunNullHostCommand,
  // LRM 21.4 $readmemh / $readmemb and LRM 21.5 $writememh / $writememb. Free
  // functions on `lyra::runtime` taking the runtime handle, the memory, the
  // file name, whatever addressing that memory's own kind states, the digit
  // radix (16 / 2), and the address the run starts from. A load completes with
  // the memory it filled, since a word the file does not address keeps what it
  // held.
  //
  // Running upward from a start and running within a start-and-finish window
  // are two requests: the window bounds the addresses the file may name, lets
  // the run descend, and obliges the file to fill the whole of it. Each is its
  // own entry, so which one a call means is settled here rather than by
  // counting arguments.
  kReadMem,
  kReadMemWithin,
  kWriteMem,
  kWriteMemWithin,
  // LRM 9.4.1 `#N`. The runtime free functions the scheduler suspends on. Each
  // call takes the runtime handle, the amount of time the design asked to wait,
  // and the calling scope's time unit and precision powers; the runtime rounds
  // that amount to the scope's precision (LRM 3.14.1) and scales it to the
  // design-global tick (LRM 3.14.3). There are two because the language gives a
  // program two ways to write such an amount and reads them differently -- an
  // integral expression, whose unknown and negative values carry meanings of
  // their own, and a real expression, which can name a fraction of a unit.
  kDelay,
  kDelayReal,
  // What decides whether reaching a wait is an event for it (LRM 9.4.2), built
  // where the wait begins and held for as long as it lasts. Two halves, each
  // present exactly where the source put one: an event control watches an
  // expression's value at a stated edge, and a named event's trigger is the
  // event itself so it watches nothing; either may carry an `iff` qualifier
  // (LRM 9.4.2.3, 15.5). Four entries because the four combinations are what
  // the language distinguishes and an absent half has no value to stand in for
  // it, so which one a wait carries is settled where the source is read rather
  // than by counting arguments.
  kObservationOnReaching,
  kObservationOfValue,
  kObservationOfValueQualified,
  kObservationQualified,
  // LRM 9.4.2 / 9.4.2.2 / 9.4.3 value-change wait. The runtime free function
  // every wait on a signal suspends on -- an `@(...)`, an `@*`, an
  // `always_comb` / `always_latch` body, a `wait (cond)`, a continuous
  // assignment. The call takes the runtime handle and the trigger set, one
  // entry per watched leaf; the process resumes when a change to one of them
  // is an event for the wait.
  kWaitAny,
  // LRM 20.3 simulation-time read functions. Each takes the runtime handle
  // and the calling scope's unit power; the runtime scales the design-global
  // tick down to that unit. `$time` rounds and yields a 64-bit `time`,
  // `$stime` yields the low 32 bits as an `int`, `$realtime` keeps the
  // fractional part as a `realtime`.
  kSimTime,
  kSTime,
  kRealTime,
  // LRM 18.13.1 -- 18.13.2 unconstrained random number functions. Free
  // functions on `lyra::runtime` taking the runtime handle; each draws from the
  // calling process's own generator, which the runtime reads ambiently, so no
  // generator reaches the call as an operand. `$urandom`'s optional seed
  // re-seeds that generator before the draw, which is a whole different
  // operation rather than an extra operand, so it is a separate entry and no
  // consumer of one has to ask whether a seed was written. `kUrandomRange`
  // always takes both bounds; an omitted low bound is the zero LRM 18.13.2
  // defines it to be, and the lowering supplies it.
  kUrandom,
  kUrandomSeeded,
  kUrandomRange,
  // `$random` called with no seed (LRM 20.14.1): the same process draw as
  // `kUrandom`, read as a signed 32-bit value.
  kRandom,
  // LRM 20.14.2 probabilistic distribution functions, generating by the
  // algorithm LRM Annex N states. Pure functions on `lyra::runtime` reached
  // with no runtime handle: their whole state is the seed operand. Each answers
  // with a product of the value drawn and the seed that draw advanced, because
  // the seed is an `inout` argument and the advanced one goes back to the
  // design's own variable.
  kDistUniform,
  kDistNormal,
  kDistExponential,
  kDistPoisson,
  kDistChiSquare,
  kDistT,
  kDistErlang,
  // LRM 20.2 simulation control. Takes the runtime handle, the call's origin,
  // and the diagnostic level (0 / 1 / 2) that selects what the tool prints
  // about it (Table 20-1). The call suspends and never resumes; the runtime
  // drops the process at the next dispatch. `$stop` suspends the simulation
  // where `$finish` exits it, and a run nothing can resume tells the two apart
  // only in what it prints, so each names the entry that prints for it.
  kFinish,
  kStop,
  // Ancestor-scope resolution for a hierarchical reference whose route starts
  // above the referrer (LRM 23.6 / 23.8). Called once per reference in the
  // resolve phase, dispatching on the referrer's own scope handle.
  // `kResolveRoot` climbs to the parent-less `$root` anchor; the descent suffix
  // starts strictly below it. `kResolveVisibleChild` walks the enclosing chain
  // and matches a child by the canonical instance name and per-axis index it
  // carries as arguments; the descent suffix starts below the matched child.
  kResolveRoot,
  kResolveVisibleChild,
  // The scope handle's runtime ABI, each entry dispatching on it. A constructor
  // registers a signal by name, or hands a freshly-built child to its parent to
  // own; `kAddOwnedChild` consumes the built child (a unique pointer) and
  // returns the parent-owned handle -- the child's own `Segment()` supplies
  // both the by-name key and the LRM display form, so the parent never
  // re-states them.
  kRegisterSignal,
  kAddOwnedChild,
  // A constructor also hands a scope what a `disable` naming it terminates
  // (LRM 9.6.2), which is unnamed because a scope carries exactly one.
  kRegisterDisableTarget,
  // What a scope answers with, one entry per kind of declaration a hierarchical
  // name may end at (LRM 23.6): the cell of a signal, the owned child at a name
  // and per-axis index, the entry of a subroutine, or what a `disable` naming
  // the scope terminates. Each is reached once in the resolve phase and each
  // fails rather than answering with nothing, because the name was resolved to
  // a declaration of that scope before anything was emitted for it.
  kFindSignal,
  kFindChild,
  kFindSubroutine,
  kFindDisableTarget,
  // The definition of a class the scope's unit declares. A class declared
  // inside a design element is a distinct type per instance of that element
  // (LRM 6.22) and nameable only inside the scope declaring it (LRM 23.9), so
  // which definition a name reaches is the scope's to answer and no referrer's
  // to assume. Reached in the resolve phase like the four above.
  kFindClass,
  // Where a name lands on a class, asked of the class rather than of a scope:
  // the storage a property occupies among what its declaring class declares, or
  // the dispatch position a behavior holds among what its introducing class
  // introduces (LRM 8.14, 8.20). Both are asked once while a reference
  // resolves, and what they answer is applied at each access with no name in
  // hand.
  kClassFindProperty,
  kClassFindBehavior,
  // Fork-join branch dispatch. Each entry spawns every branch as its own
  // coroutine and yields the parent's wait shape per LRM 9.3.2: `kForkWaitAll`
  // for `join` (resume after the last branch), `kForkWaitFirst` for
  // `join_any` (resume after the first), `kSpawnAll` for `join_none` (no
  // wait; the call's result is `void` so the caller never awaits it). The
  // mode lives in the callee identity rather than as an enum operand so MIR
  // never carries a join-mode datum and the call's result type is what
  // selects await vs not. Each takes the runtime handle followed by a
  // variadic branch list -- the runtime entry is a variadic template that
  // assembles the move-only branches into the internal coroutine vector.
  kForkWaitAll,
  kForkWaitFirst,
  kSpawnAll,
  // LRM 9.6.1 `wait fork`: suspends the executing process until every immediate
  // child it spawned has terminated. Takes only the runtime handle; the child
  // set is the executing process's, read at runtime. The call's result is
  // `void` and the caller awaits it, the same await shape as `join`.
  kWaitFork,
  // LRM 9.6.3 `disable fork`: terminates every descendant of the executing
  // process, including the descendants of subprocesses that have already
  // terminated. Takes only the runtime handle; the descendant set is the
  // executing process's, read at runtime. The caller does not block, so its
  // `void` result is never awaited.
  kDisableFork,
  // LRM 9.6.2 `disable <named block or task>`. `kDisable` is the statement
  // itself: it invalidates the named target, wakes the executions blocked
  // inside it, and leaves the disabling execution when that execution is inside
  // the target too. `kEnterTarget` and `kLeaveTarget` bracket the target's
  // extent, recording on the running process which targets its execution is
  // inside and the generation each held on entry; a region naming a target
  // tests the effect in hand with `kEffectNamesTarget` and raises it again when
  // it names another.
  kDisable,
  kEnterTarget,
  kLeaveTarget,
  kEffectNamesTarget,
  // LRM 9.7's `process` methods. The class is one the runtime library defines
  // and every unit imports rather than declares, so no per-unit declaration
  // names these and the library carries each of them out. `kProcessSelf` names
  // the running process, so it acts on no object; the other five act on the
  // handle one of those answered with.
  kProcessSelf,
  kProcessStatus,
  kProcessKill,
  kProcessAwait,
  kProcessSuspend,
  kProcessResume,
  // Lifecycle activation registration (LRM 9.2): binds a process body's
  // coroutine to the scope's startup (`kRegisterInitial`) or shutdown
  // (`kRegisterFinal`) lifecycle. Distinct callees, not one tagged call --
  // initial and final are different registrations. Each dispatches on the
  // scope handle; the coroutine to register is a regular argument.
  kRegisterInitial,
  kRegisterFinal,
  // The extent a static initializer draws inside (LRM 18.14.1): entered before
  // the initializers a body runs and left on every way out of them, so a
  // randomization call made before any procedure starts has the generator the
  // standard gives it. A scope names the instance holding its seeds; a
  // namespace has none to name, so the two are distinct callees rather than
  // one taking an absent operand.
  kEnterScopeStaticInit,
  kEnterNamespaceStaticInit,
  kLeaveStaticInit,
  // The inner step of a value conversion: reading the source out as a machine
  // integer. HIR-to-MIR dispatches on the (src, dst) type pair and emits a
  // `CallExpr` to the matching entry, which the backend renders with no
  // type-driven branching of its own. `kToInt64` is the `PackedArray` accessor
  // the integral-to-real path reads through; `kRound` is the `Real` /
  // `ShortReal` accessor that rounds to int64 per LRM 6.12.1, which the
  // real-to-integral path reads through.
  kToInt64,
  kRound,
  // LRM 20.5 real conversions. `kTruncate` is `$rtoi`'s reading of a real as an
  // integer, which drops the fraction where the LRM 6.12.1 conversion rounds
  // it. `kToBits` and `kFromBits` carry the IEEE 754 pattern itself rather than
  // the number it encodes, as `$realtobits` and `$bitstoreal` do; both name the
  // pattern as a machine int64, whose low half the shortreal pair occupies.
  kTruncate,
  kToBits,
  kFromBits,
  // Machine-value accessors used by DPI-C marshaling (LRM 35.5.6):
  // `kRealValue` reads a `Real` / `ShortReal` out as its machine float;
  // `kStringCStr` borrows a `String` as a NUL-terminated C string valid for the
  // owning string's lifetime; `kChandlePtr` reads a `Chandle` out as the opaque
  // pointer it carries. Each takes that value as its receiver.
  kRealValue,
  kStringCStr,
  kChandlePtr,
  // Canonical DPI-C packed and 1-bit-4-state marshaling (LRM Annex H.10.1).
  // `kToSvLogic` reads a 1-bit 4-state value out as its `svLogic` scalar
  // encoding (`value | unknown << 1`); `kFromSvLogic` builds it back in the
  // destination's declared representation (`args[0]` the byte, `args[1]` the
  // type). The `kReadCanonical*` helpers build an SV value from a canonical
  // buffer in that representation (`args[0]` the buffer pointer, `args[1]` the
  // type); the `kWriteCanonical*` helpers are their inverse, writing an SV
  // value out into a canonical buffer (`args[0]` the buffer pointer, `args[1]`
  // the SV value), as an export's C entry point does through the foreign
  // caller's pointer. These are free functions in `lyra::value`. An import's
  // packed copy-in is not a builtin: the boundary buffer is a `DpiBitBuffer` /
  // `DpiLogicBuffer` value constructed from the SV value, and the
  // `kDpi*BufferData` pair reads its writable chunk pointer for the foreign
  // call and the copy-back read, taking that buffer as its receiver. `Bit`
  // carries a 2-state (aval-only) buffer, `Logic` a 4-state buffer whose `aval`
  // is the value plane and `bval` the unknown plane. The two are separate
  // library types the foreign side reaches through separate C pointer types
  // (Annex H.10.2), so which of them a read is on is named here.
  kToSvLogic,
  kFromSvLogic,
  kReadCanonicalBitVec,
  kReadCanonicalLogicVec,
  kWriteCanonicalBitVec,
  kWriteCanonicalLogicVec,
  kDpiBitBufferData,
  kDpiLogicBufferData,
  // The open-array boundary image (LRM 35.5.6.1, Annex H.12).
  // `kDpiOpenArrayHandle` reads the opaque handle the foreign side receives in
  // place of the actual; `kDpiOpenArrayValue` reads the image back as an SV
  // value shaped like the prototype it carries as an argument, which is what an
  // `output` / `inout` open array stores into its actual. Each takes as its
  // receiver the image, which the call site builds from the actual before the
  // foreign call.
  kDpiOpenArrayHandle,
  kDpiOpenArrayValue,
  // Runs a DPI-C import task's foreign call (LRM 35.5.2) on a fiber whose
  // native stack can be parked while simulation time advances, and yields the
  // awaitable that suspends the caller until the call returns. The call itself
  // is `args[0]`, a closure of the whole boundary; a foreign task may consume
  // time by calling back an exported task that suspends, and the fiber is what
  // lets that suspension cross a native stack the runtime does not own. A free
  // function over that closure.
  kRunForeignTaskOnFiber,
  // Runs an exported SV task's coroutine body to completion synchronously and
  // yields its completion payload. A foreign C caller of an exported task (LRM
  // 35.8) is not a coroutine and cannot await the task body, so the C entry
  // point enters the body through this runtime driver instead of the `co_await`
  // an SV enabler uses. A free function over the task's coroutine value.
  kRunExportedTaskToCompletion,
  // The scope a subroutine exported to foreign code runs against, and the entry
  // that scope publishes for a given foreign name (LRM 35.5.3). A
  // program-global export symbol reaches its subroutine through these: the
  // foreign call chain establishes the scope, and the scope names the entry.
  // Free functions over the run.
  kCurrentExportScope,
  kFindExportEntry,
  // The outer step of a value conversion: the static factory that lands a
  // machine-level result in the destination's declared representation, which is
  // the type the call answers with. `kFromInt` builds a target-shape vector
  // from a machine integer and `kConvertFrom` reshapes another packed vector;
  // `kFromPackedArray` and `kFromByteArray` build a string from packed bits
  // (LRM 6.16) or from a byte unpacked array (LRM 21.3.4.3).
  kFromInt,
  kFromWords,
  kConvertFrom,
  kFromPackedArray,
  kFromByteArray,
  // The opposite direction, under the LRM 5.9 string-literal assignment rules:
  // an integral destination takes the text right-justified, an unpacked byte
  // array takes it left-justified. One factory named on whichever destination
  // the call answers with; that destination's declared representation reaches
  // it as an operand naming its type (plus an element count for the array).
  kFromString,
  // LRM 7.6: one unpacked array kind taking another's elements. The three kinds
  // differ in what the destination declares and the source cannot supply -- a
  // fixed-size array its element count, a queue its bound -- so the call states
  // those as operands, while the elements themselves cross unchanged because
  // the clause admits the assignment only where the element types are
  // equivalent. Named rather than left to the
  // type's own construction because building over an element list carries the
  // same operand count.
  kFromArray,
  // Conforms a queue value to a destination's LRM 7.10.5 bound (a negative
  // argument means unbounded): the store boundary brings a differently-bounded
  // source to the destination's declared bound. An instance method on the
  // queue value.
  kConformBound,
  // The two steps an unpacked concatenation (LRM 10.10) folds into: appending
  // one element to the accumulating array, and appending every element of a
  // spread part in order. A part contributes itself unless the program spreads
  // a container, so which step it takes is the program's fact, not the part's
  // type. Both are instance methods on the accumulator, so a concatenation of
  // any number of parts is the left-to-right chain of them, folded where the
  // join is built -- no entry composes an operand list of arbitrary length. One
  // entry each serves every growable array family, the accumulator's own domain
  // (a queue enforcing its bound, a dynamic array unbounded) selecting the
  // realization. A spread part crosses erased; its own container representation
  // is no concern of the appending entry.
  kArrayConcatElement,
  kArrayConcatSpread,
  // The step that fits a concatenation's parts to a fixed-size unpacked array
  // (LRM 10.10): the parts, accumulated into a dynamic array by the two steps
  // above, adopted into a target whose element count is fixed, which is an
  // error when the counts differ. The call answers with that target, as it does
  // for any static factory, so the entry is named for the array it builds and
  // not for the dynamic array it reads.
  kArrayConformSize,
  // A dynamic array sized at run time: empty at its declared element shape,
  // `new[N]`, and `new[N](src)` (LRM 7.5.1). Each is named because the
  // argument list does not tell them apart -- a sized `new` and an element
  // list both carry two operands -- so a target that cannot resolve overloads
  // would have nothing to read. Building one from an element list is not among
  // them: every container builds from a list the same way, so its own type
  // names that factory. The call answers with the array, as it does for any
  // static factory.
  kMakeDynamicArrayDefault,
  kMakeDynamicArrayNew,
  kMakeDynamicArrayNewCopy,
  // LRM 11.4.12 concatenation and replication. What the two operators join --
  // bit planes or characters -- follows the operand's value domain, so one
  // entry each serves both and the domain names the realization. Each joins
  // two operands: a longer source-level join folds into a chain, because an
  // operand list of arbitrary length has no single entry to call.
  kConcat,
  kReplicate,
  // Operator realizations on `PackedArray`. HIR-to-MIR lifts the
  // method-style SV operators (LRM 11.4) into `CallExpr` against these
  // entries, so the backend renders every operator mechanically: native
  // forms (`+`, `==`, ...) collapse to a single formatter, method forms
  // route through the call path. The shift / power / xnor / wildcard /
  // case / implication / equivalence ids dispatch on their left operand,
  // and the reduction ids likewise dispatch on the operand they fold.
  // `kFromBool` is a static factory that wraps a host bool
  // into a 1-bit `PackedArray` (used to shape the result of a real /
  // string comparison or logical operator into the LRM 11.3 / 11.4 1-bit
  // integral result type).
  kPow,
  kShiftLeft,
  kLogicalShiftRight,
  kArithmeticShiftRight,
  // The same three shifts applied to what a place holds, rather than computed
  // from two values (LRM 11.4.1 `<<=` / `>>=` / `>>>=`). A compound assignment
  // reads its destination exactly once, and an entry that answers with a value
  // leaves the reading to whoever calls it -- so applying is its own operation,
  // and it is the one a compound assignment names. Every other compound
  // assignment operator is one a target applies to two values of one type and
  // names no entry at all.
  kShiftLeftAssign,
  kLogicalShiftRightAssign,
  kArithmeticShiftRightAssign,
  kBitwiseXnor,
  kLogicalImplication,
  kLogicalEquivalence,
  kWildcardEquals,
  kCaseEqual,
  kCasezEquals,
  kCasexEquals,
  kMergeConditional,
  kReductionAnd,
  kReductionOr,
  kReductionXor,
  kReductionNand,
  kReductionNor,
  kReductionXnor,
  kFromBool,
  // Typed parent navigation: `scope->Parent()` returns the enclosing scope as
  // the runtime `Scope` base pointer. An intra-unit upward member access casts
  // the result to the enclosing class and reads the member directly (the unit
  // owns the enclosing class's layout); distinct from the by-name cross-unit
  // navigation above.
  kParent,
  // LRM 8.11 `this`: the handle referring to the object the running subroutine
  // was invoked on. A body reaches its own object through a borrowed pointer,
  // which serves every member access; the source asks for a handle instead when
  // it returns, passes, or compares the object itself, and how a target answers
  // with one follows from how that target realizes a handle.
  kSelfHandle,
  // LRM 21.2.1.5 `%m` source: yields the receiver scope's hierarchical name as
  // an SV `string`. Walks `Parent()` from the receiver up to the implicit root
  // and joins the name of each scope a path can reach with `.`. `%m` lowering
  // reads the result as an ordinary value-string print item rather than as a
  // context-dependent format kind.
  kHierarchicalPath,
};

// A function the library declares in a scope of its own, named here as a call
// site writes it. The object the entry acts on, where it has one, is its
// leading argument, because a free function binds nothing.
struct FreeFunction {
  std::string_view qualified_name;
};

// A method on the object the entry acts on, reached through that object.
struct Method {
  std::string_view identifier;
};

// A factory on the type the entry builds. There is no object to act on, and
// the type it is reached on is the type of the value the call answers with.
struct StaticFactory {
  std::string_view identifier;
};

// How a call site reaches the entry. Every entry is one of these, because an
// operation no runtime library carries out is not named here at all: it is
// answered where the source is read and no layer below meets it.
using EntryDeclaration = std::variant<FreeFunction, Method, StaticFactory>;

// Every property of one runtime entry: what the library calls it, how a call
// site reaches it, and what it does with the operands it is given. A consumer
// asking any of those reads the field for it, never a list of its own, so an
// entry gains a property by saying so here and nowhere else.
struct RuntimeEntry {
  // The entry's stable spelling. It aligns with the SV method spelling where
  // one exists (LRM 6.16 / 7.9 / 7.10 / 7.12 / 15.5) and with what the library
  // calls it where there is no SV-side surface (`get` / `set` / `initialize`).
  //
  // This is an interface contract, not a display string. It names the entry in
  // a dump and in a diagnostic, and it is the suffix of the runtime-library
  // symbol a generated module calls, so changing it renames a linked symbol.
  // Change it only to correct the entry's identity, never to improve how a
  // dump reads.
  std::string_view name;
  // Where the library declares the entry, and what a call site writes to reach
  // it.
  EntryDeclaration declaration;
  // Whether the entry's implementation takes the engine handle -- to reach the
  // scheduler, or to identify the process that is running. It is an ordinary
  // operand, riding immediately after the object the entry acts on and first
  // where there is none, so a call site composing the operands has to know
  // whether to supply it and only the entry knows.
  bool takes_the_runtime_handle = false;
  // Whether a call to the entry parks the caller until something other than
  // the call settles, so a statement calling it awaits (LRM 9.7 `await`, LRM
  // 9.4 a delay, LRM 9.4.2 a value-change wait). Distinct from a callee that
  // completes as a coroutine, which the call's own type states: the entry
  // answers with an ordinary value and nothing about that value says the
  // caller stopped.
  bool parks_the_caller = false;
  // Whether the entry updates the object it acts on, so that object names a
  // place rather than a value -- which is what makes a receiver reaching
  // through a capability wrapper reach its write access. Where the update
  // lands and how it gets there is each target's own answer.
  bool mutates_receiver = false;
  // Whether the entry answers with the part it reaches rather than with that
  // part's value, so what stands at the call may be written and a further
  // access composes onto it. A target whose values have no reachable interior
  // realizes such a call as a read of the whole and a rebuild instead; which
  // entries it must do that for is this, so the property stays with the entry
  // and not with each target that has to know it.
  bool answers_with_the_part = false;
  // Whether the LRM 7.12 method takes a `with`-clause closure as its second
  // argument. The other LRM 7.5 / 7.10 array entries (`size`, `delete`,
  // `reverse`) take none.
  bool takes_closure = false;
  // Whether the entry answers with an index by writing it into the variable
  // the source named (LRM 7.9.4 -- 7.9.7), so the call lowers to a block
  // expression: binding the answer and writing it back are steps rather than
  // one expression.
  bool writes_the_index_back = false;
  // Which operand is an index, absent for an entry that names none. It covers
  // the index a traversal searches from as well as the one a select names,
  // since both cross the boundary the same way.
  std::optional<std::size_t> index_operand = std::nullopt;
  // Which operand is a whole container crossing erased -- a spread
  // concatenation part (LRM 10.10) whose own domain the entry cannot name, so
  // it boxes into a runtime value in that domain and is read back element by
  // element. Absent for an entry that has none.
  std::optional<std::size_t> spread_operand = std::nullopt;
  // Which operand is the prototype the entry's result takes its shape from,
  // absent for an entry whose result the object it acts on already shapes. A
  // prototype stands for the result before there is one -- an empty
  // reduction's zero, a locator's key, a map's chosen element (LRM 7.12), the
  // index an unallocated dimension reports (LRM 20.7), the element a container
  // is seeded with (LRM 7.5.1) -- so the call site supplies it and it names a
  // representation the entry has no other way to know.
  std::optional<std::size_t> result_prototype_operand = std::nullopt;
};

// The one declaration of `id`. Total over the entry set, so an entry added
// anywhere in the pipeline fails to build until every property it has is
// written down here.
[[nodiscard]] auto RuntimeEntryOf(BuiltinFn id) -> RuntimeEntry;

}  // namespace lyra::support
