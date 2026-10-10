#pragma once

#include <algorithm>
#include <array>
#include <cstddef>
#include <cstdint>
#include <initializer_list>
#include <optional>
#include <span>
#include <string_view>
#include <variant>

#include "lyra/base/internal_error.hpp"
#include "lyra/support/integral_operation.hpp"

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
  //
  // Several parts in a row are three operations, each with operands of its
  // own. Bits of an integral value are named by where they start, and are as
  // many as the type they are read at is wide (LRM 11.5.1). Elements of a
  // fixed-size or dynamic array are named by where they start and how many
  // there are (LRM 7.4.6). A queue's slice is bounded by two positions the
  // running program can move, and is a queue the slice builds rather than a
  // part that may be written (LRM 7.10.1).
  kElement,
  kSlice,
  kElementSlice,
  kQueueSlice,
  kElementRef,
  kSliceRef,
  kElementSliceRef,
  // The same two directions over a part named by its declaration-order
  // position rather than by a coordinate. One pair covers a product and an
  // active-member value alike: what differs is whether every part is live at
  // once or one at a time, and whether reaching one for writing settles which
  // that is -- all of which is the value's own semantics, reached through the
  // domain its type names, exactly as a step by position reaches a queue's
  // rules or a dynamic array's.
  kComponent,
  kComponentRef,
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
  // Building takes a value of the type it builds, because a sequence of bits
  // carries no shape: the answer has that value's shape -- how many elements
  // each array in it holds -- and the sequence is consumed left to right. The
  // caller brings the sequence to exactly the width that shape takes, so
  // neither widening nor truncation is this entry's business.
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
  // The accesses and the removal above as an associative array takes them (LRM
  // 7.8, 7.9.3): by the key the source wrote, a value of the type it was
  // written in, where every other container is reached by a position. A key
  // and a position are different operands, so each access is an entry of its
  // own, named where the source is read and the array's type is in hand.
  kAssocElement,
  kAssocElementRef,
  kAssocDesignateElement,
  kAssocReferElement,
  kAssocDeleteIndex,
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
  // Whether two values are the same bits -- an unknown bit matching only the
  // same unknown, a real its own pattern -- which is what decides whether a
  // write changed a variable (LRM 4.3, 9.4.2); and whether a value holds an
  // unknown bit at all, which a value with no four-state part never does (LRM
  // 20.9). Each answers with a machine boolean, being a predicate the runtime
  // asks of a value it holds as well as one the program writes.
  kBitIdentical,
  kHasUnknown,
  // Folding two contributions of equal strength to a net, one entry per truth
  // table for the reason a net's installation is one entry per resolution (LRM
  // 6.6.1, 6.6.3); what a stronger contribution leaves a weaker one (LRM
  // 28.12.1); and a value shaped as another is, with every bit set to one fill
  // (LRM 6.7.1).
  kResolveTriState,
  kResolveWiredAnd,
  kResolveWiredOr,
  kDominating,
  kFilledLike,
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
  // A declaration's own write on a capability wrapper: the first installs the
  // declared representation, and a declaration reached again writes at that
  // representation. It acts on the wrapper rather than on the storage the
  // wrapper represents, which is why it is a call at all. It carries no runtime
  // handle, so it raises no update event -- which holds for what reaches it: a
  // declaration whose storage is built with its owner, and one whose storage
  // the body it sits in owns. Every later store requires its value to already
  // be at the installed representation.
  kInitialize,
  // Installing what a net's declaration gives it, once at construction: what
  // its data type fixes, and what its declared net type states -- the
  // contribution the net type itself makes, as the scalar the net shows where
  // nothing drives it and the strength it holds that scalar at (LRM 6.6.5,
  // 6.7.1). One entry per resolution -- tri-state for `wire` / `tri`,
  // wired-and for `wand` / `triand`, wired-or for `wor` / `trior` (LRM 6.6.1,
  // 6.6.3), and one that resolves tri-state and leaves its own contribution
  // holding what the drivers last decided, which is how a net stores a value
  // (LRM 6.6.4) -- because a truth table is applied rather than carried, and
  // an operation is named here. What the contribution is stays a value the
  // call carries.
  //
  // A net of an integral type is told how many positions that type has, which
  // is all a connection can name of it (LRM 23.3.3.7). A net of an unpacked
  // aggregate is handed a value of its data type, whose shape it takes (LRM
  // 6.7.1), so its install is an entry of its own.
  kNetInitializeTriState,
  kNetInitializeWiredAnd,
  kNetInitializeWiredOr,
  kNetInitializeRetaining,
  kAggregateNetInitializeTriState,
  kAggregateNetInitializeWiredAnd,
  kAggregateNetInitializeWiredOr,
  kAggregateNetInitializeRetaining,
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
  // Opening a cell's storage for a write, which is an operation on the wrapper
  // for the same reason the two above are: which storage it currently stands
  // for is a fact about the wrapper, not about the place naming it. It answers
  // with the write in progress, which ends with the full-expression and names
  // no place itself.
  kOpenForWrite,
  // The whole of what a write in progress was opened on, designated within
  // it: where the steps into parts start, and, dereferenced, where a write of
  // the whole lands.
  kDesignateWhole,
  // A step taken within a write in progress, into a part of what it designates
  // that the write lands on where it lies: an element, a component, bits of an
  // integral value, a slice of elements. It answers with the part designated
  // within the same write -- a value of its own that borrows the write -- which
  // is not what the step answering with the part alone answers. The write hears
  // what the step did to the variable where the part cannot say so itself -- an
  // element made by being written, an index naming none (LRM 7.8.7, 7.10.1,
  // 7.4.6), bits or elements the write moved; a component is formed by nothing,
  // so its step has nothing to tell. Dereferencing what the steps designate is
  // where the write lands.
  kDesignateElement,
  kDesignateComponent,
  kDesignateSlice,
  kDesignateElementSlice,
  // A step taken on a reference, into a part of what it refers to that may be
  // passed by reference -- an element or a component (LRM 13.5.2). It answers
  // with a reference to that part, which belongs to whatever the reference it
  // was taken on belongs to, so a write through it is a write of the same
  // variable. Forming an element can change that variable (LRM 7.8.7), and the
  // step says so where it is taken.
  kReferElement,
  kReferComponent,
  // A reference to a property of an object (LRM 13.5.2, 8.4), a step taken on
  // the object. A write through it tells the object as it lands (LRM 9.4.2), so
  // the object travels with the reference, and the property is named as the
  // part the call reaches.
  kReferProperty,
  // What a wait on the storage a reference names enrols on: whatever a
  // write through the reference is told to -- the variable, or the object a
  // property belongs to (LRM 13.5.2, 9.4.2) -- as an erased pointer, the form
  // an object's event source takes too.
  kReferenceReportsTo,
  // Binding a member that owns no storage to what its connection drives it
  // with (LRM 23.3.3): the member names that storage from then on, and is
  // recorded as bound from whatever member the bound reference came from.
  kBindMember,
  // Stating, as the design is built, that a continuous assignment drives the
  // storage a reference names (LRM 10.3), which decides what a variable shows
  // once nothing overrides it (LRM 10.6.2).
  kDrivesContinuously,
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
  // it and `disable fork` does not reach it -- and `ResumeInNbaRegion` is the
  // wait it stops at to reach the region the update is due in (LRM 4.4.2.4)
  // once the event has named the slot.
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
  // An operand of such a format string whose type decides its whole rendering
  // (LRM 21.2.1.6): it carries the text and nothing else, because the clause
  // defines no other conversion for it and so there is no value reading to
  // carry. A factory rather than a constructor, an operand that reads by
  // conversion and one that reads only as its pattern being two ways of making
  // one thing out of the same argument type.
  kMakeRenderedFormatArg,
  // An operand of such a format string that reads by conversion and also
  // carries the text its type renders it as (LRM 21.2.1.6): an enumeration,
  // which is its base integral under every other conversion. A factory of its
  // own, so which of the two operations a construction is never depends on how
  // many operands it was handed.
  kMakePatternedFormatArg,
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
  // takes the destination's value and completes with it beside the answer. A
  // destination is an integral or a string, and converting a plusarg into each
  // is a different request, so each is its own entry and which one a call
  // means is settled here rather than by reading the destination's type. All
  // answer an SV `int` (1 on prefix match, 0 otherwise).
  kTestPlusargs,
  kValuePlusargs,
  kValuePlusargsString,
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
  // LRM 9.4.1 `#N`. The runtime free functions answering the wait of a delay.
  // Each call takes the runtime handle, the amount of time the design asked to
  // wait, and the calling scope's time unit and precision powers; the runtime
  // rounds
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
  // Building an observation evaluates nothing. Asking whether a candidacy was
  // an event evaluates it, by the waiting process for a wait that process
  // decides (LRM 4.5), and answers one or zero.
  kObservationFires,
  // LRM 9.4.2 / 9.4.2.2 value-change wait -- an `@(...)`, an `@*`, an
  // implicit list, a continuous assignment. The wait is built on one trigger
  // per watched leaf, or on the implicit list a report settled (LRM
  // 9.2.2.2.1), where what it watches stops changing, and held across the
  // body's stops there. Each answers the wait.
  kWaitOn,
  kWaitOnImplicitList,
  // LRM 9.4 every stop: it takes the runtime handle and the wait the body holds
  // -- whichever construct built it -- and answers whether the caller must give
  // up control; the execution continues when what the wait waits for happens.
  kParkAt,
  // LRM 9.4.2 / 9.4.3 the waits on what an evaluation the process made
  // reached, each taking one read report per expression evaluated and built at
  // the stop. The process decides these by evaluating again, so an event
  // control resumes on every candidacy and asks its observations, which it
  // also hands over for a restart (LRM 9.7); a `wait (cond)` resumes for its
  // loop to test the condition, which is also what a restart does, since a
  // condition is a state the body reads rather than an occurrence. Each takes
  // what its reports hold.
  kWaitRecollecting,
  kWaitUntil,
  // What an evaluation states the places it reached in (LRM 9.4.2): an empty
  // report, or one no evaluation makes, a procedure's implicit list, whose
  // calls report and never run; a place it reads and the bits it reads there;
  // a place reached through a handle; every object at once; the bracket around
  // a call made on a handle, while which everything reported is reached
  // through it; a place written and its bits; settling what was reported as a
  // procedure's implicit list (LRM 9.2.2.2.1), which keeps only what was read
  // directly, less what was written; the bracket a function takes around
  // reporting into it, which answers whether to go on at all; and whether the
  // function, having reported, runs -- the one an evaluation called does. A
  // function reaching a read no leaf watches yet refuses the report instead.
  kReadReportEmpty,
  kReadReportForImplicitList,
  kReadReportAdd,
  kReadReportAddThroughHandle,
  kReadReportAddEveryObject,
  kReadReportEnterCallOnHandle,
  kReadReportLeaveCallOnHandle,
  kReadReportAddWrite,
  kReadReportSettleAsImplicitList,
  kReadReportEnter,
  kReadReportLeave,
  kReadReportRunsTheBody,
  kRefuseReport,
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
  // about it (Table 20-1). The call ends the run and never returns: the calling
  // execution departs, so no statement after it runs, in any body -- a
  // function included (LRM 13.4). `$stop` suspends the simulation where
  // `$finish` exits it, and a run nothing can resume tells the two apart only
  // in what it prints, so each names the entry that prints for it.
  kFinish,
  kStop,
  // Where a hierarchical name leaving an instance starts (LRM 23.6 / 23.8):
  // the nearest scope above the receiver that is of the class handed in, an
  // instance or a generate block, or past the topmost a top-level instance of
  // it. Called once per reference in the resolve phase.
  kEnclosingScope,
  // Whether the receiver, a scope of the design hierarchy, is an object of the
  // class handed in or of one extending it. A class a unit published of a
  // scope may be realized by several classes of that unit, and a body entered
  // on the object as the published class asks which of them it is.
  kIsOfClass,
  // A constructor hands a freshly-built child to its parent to own: this
  // consumes the built child (a unique pointer) and returns the parent-owned
  // handle -- the child's own `Segment()` supplies the LRM display form, so the
  // parent never re-states it.
  kAddOwnedChild,
  // Extends a sequence still being composed by one element, answering with
  // what it became. A declaration standing for several objects counts them out
  // as it builds them (LRM 23.3.2), and what the member finally holds is the
  // last answer -- so nothing a member holds is ever partly composed, and the
  // count never has to be known before the objects are.
  kExtendSequence,
  // The part of the object a handle reaches it through. A body runs on that
  // part rather than on a reference to it, and a handle refers to one without
  // being one.
  kViewOf,
  // What reports a change to an object's properties (LRM 9.4.2): the event
  // source a wait reaching the object enrols on; a write into its
  // properties, opened on the object alone, open while the full-expression
  // doing it lasts and telling the object when it ends; and the object a write
  // answers, which is how a dereference of the write is reached below MIR.
  kObjectEventSource,
  kOpenObjectWrite,
  kWrittenObject,
  // Fork-join branch dispatch. Each entry spawns every branch as its own
  // coroutine and answers per LRM 9.3.2: `kForkWaitAll` for `join` (the wait
  // for the last branch), `kForkWaitFirst` for `join_any` (the wait for the
  // first), `kSpawnAll` for `join_none` (no wait; the call's result is `void`
  // so the caller never stops). The mode lives in the callee identity rather
  // than as an enum operand so MIR never carries a join-mode datum and the
  // call's result type is what selects whether the caller stops. Each takes the
  // runtime handle followed by a variadic branch list -- the runtime entry is a
  // variadic template that assembles the move-only branches into the internal
  // coroutine vector.
  kForkWaitAll,
  kForkWaitFirst,
  kSpawnAll,
  // LRM 9.6.1 `wait fork`: the wait for every immediate child the executing
  // process spawned to have terminated. Takes only the runtime handle; the
  // child set is the executing process's, read at runtime. The caller stops at
  // what it answers, the same shape as `join`.
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
  // What a region binds as the effect it caught, asked of whatever unwound into
  // it: an effect is itself, a run-time error is the departure it becomes, and
  // anything that is not the design's passes on from inside the question. No
  // call in MIR names it, because the binding is part of what a region is;
  // each target calls it where it realizes one, and the thing it asks about is
  // whatever that target holds in flight -- a C++ handler holds it implicitly,
  // while a target that unwinds by hand hands the entry the pointer it caught.
  kReceiveDeparture,
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
  // The extent a `context` DPI import's foreign call runs inside (LRM 35.5.3):
  // entered before the call so the foreign side reports the instantiated scope
  // of the import declaration, and left on every way out of it so what was
  // reported before is reported again. A declaration in a namespace is never
  // instantiated and names no scope, which it enters as a null one rather than
  // leaving the chain to report an enclosing entry that is not its own.
  kEnterDpiScope,
  kLeaveDpiScope,
  // The DPI-C disable protocol (LRM 35.9), which is what a boundary states
  // instead of letting a departure cross a frame this compiler did not emit.
  // The answer is the int LRM 35.8 gives an exported task's entry: 1 while a
  // disable is active on this execution thread -- one reached a block it is
  // inside, its process was terminated, or the run ended -- and 0 otherwise.
  //
  // The three checks are the ones the clause makes the simulator's, each placed
  // where its evidence is: what an imported task returned, whether an imported
  // function acknowledged, and whether an exported subroutine was reached at
  // all once the state was entered. None of them leaves by an effect, because
  // the frame each stands in was reached from foreign code.
  kDisableIsActive,
  kCheckImportTaskAcknowledged,
  kCheckImportFunctionAcknowledged,
  kCheckExportReachable,
  // Where a foreign call hands control back, this execution takes the departure
  // owed to it, if one is (LRM 9.6.2, 9.7). Every other point an execution
  // regains control is a resumption, which each backend already asks its own
  // way; a foreign call that consumed no simulation time suspended nothing, so
  // this is the one such point a body has to state for itself.
  kTakeDepartureIfDue,
  // Whether this call is the one bring-up a namespace's initializers get (LRM
  // 26.2). A namespace is reached both by the design's own bring-up and by
  // every namespace whose initializers read its cells, so the entry answers
  // true once and false afterwards, and taking the claim before descending is
  // what ends a cycle among them.
  kClaimNamespaceInitialize,
  // The inner step of a value conversion: reading the source out as a machine
  // integer. HIR-to-MIR dispatches on the (src, dst) type pair and emits a
  // `CallExpr` to the matching entry, which the backend renders with no
  // type-driven branching of its own. `kToInt64` is the integral accessor the
  // integral-to-real path reads through; `kRound` is the `Real` / `ShortReal`
  // accessor that rounds to int64 per LRM 6.12.1, which the real-to-integral
  // path reads through.
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
  // destination's declared representation, which is the type the call answers
  // with (`args[0]` the byte). The `kReadCanonical*` helpers build an SV value
  // of the type the call answers with from a canonical buffer (`args[0]` the
  // buffer pointer); the `kWriteCanonical*` helpers are their inverse, writing
  // an SV value out into a canonical buffer (`args[0]` the buffer pointer,
  // `args[1]` the SV value), as an export's C entry point does through the
  // foreign caller's pointer. An import's packed copy-in is not a builtin: the
  // boundary buffer is a `DpiBitBuffer` / `DpiLogicBuffer` value constructed
  // from the SV value, and the `kDpi*BufferData` pair reads its writable chunk
  // pointer for the foreign call and the copy-back read, taking that buffer as
  // its receiver. `Bit` carries a 2-state (aval-only) buffer, `Logic` a
  // 4-state buffer whose `aval` is the value plane and `bval` the unknown
  // plane. The two are separate library types the foreign side reaches through
  // separate C pointer types (Annex H.10.2), so which of them a read is on is
  // named here.
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
  // native stack can be parked while simulation time advances. It is where the
  // import's caller stops, so it answers whether the caller must give up
  // control, exactly as stopping at a wait does. The call itself is
  // `args[1]`, a closure of the whole boundary; a foreign task may consume time
  // by calling back an exported task that suspends, and the fiber is what lets
  // that suspension cross a native stack the runtime does not own. A free
  // function over the run and that closure.
  kRunForeignTaskOnFiber,
  // Runs an exported SV task's coroutine body to completion synchronously and
  // yields its completion payload. A foreign C caller of an exported task (LRM
  // 35.8) is not a coroutine and cannot await the task body, so the C entry
  // point enters the body through this runtime driver instead of the `co_await`
  // an SV enabler uses. A free function over the execution the body runs as;
  // where a target takes the value the body completes with through storage the
  // caller supplies, it is that storage the value lands in rather than the
  // call's own answer.
  kRunExportedTaskToCompletion,
  // The scope a subroutine exported to foreign code runs against, and the entry
  // that scope publishes for a given foreign name (LRM 35.5.3). A
  // program-global export symbol reaches its subroutine through these: the
  // foreign call chain establishes the scope, and the scope names the entry.
  // Free functions over the run.
  kCurrentExportScope,
  kFindExportEntry,
  // The outer step of a value conversion: what lands a machine-level result in
  // the destination's declared representation, which is the type the call
  // answers with. `kFromInt` builds a value of that type from a machine integer
  // and `kConvertFrom` reshapes another value of the same family into it.
  kFromInt,
  kConvertFrom,
  // The position an index names, as a value position arithmetic can be done in
  // without wrapping, and unknown where the index names none: every select,
  // and every ordinal a method takes, reaches the part it names through this.
  // It answers with the position type.
  kToPosition,
  // A string out of an integral value's bytes (LRM 6.16) and out of an unpacked
  // array of byte (LRM 21.3.4.3).
  kStringFromBits,
  kFromByteArray,
  // The opposite direction, under the LRM 5.9 string-literal assignment rules:
  // an integral destination takes the text right-justified, and is as wide as
  // the type the call answers with.
  kIntegralFromString,
  // An unpacked array of byte out of text and out of an integral value's bytes,
  // each left-justified from the array's first element (LRM 5.9). A string
  // literal is an integral constant, so an assignment of one arrives as bits,
  // whole: a NUL among them is a byte like any other. Each takes how many
  // elements the array has.
  kByteArrayFromString,
  kByteArrayFromBits,
  // LRM 7.6: one unpacked array kind taking another's elements, one entry per
  // destination kind. The kinds differ in what the destination declares and the
  // source cannot supply -- a fixed-size array its element count, a queue its
  // bound (a negative one meaning none, LRM 7.10.5) -- so each entry takes what
  // its own destination declares, after the source and the default an element
  // of the destination starts from. The elements themselves cross unchanged,
  // because the clause admits the assignment only where the element types are
  // equivalent.
  kDynamicArrayFromArray,
  kUnpackedArrayFromArray,
  kQueueFromArray,
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
  // LRM 11.4.12 concatenation: two operands joined, bits or characters as the
  // operands are. A longer source-level join folds into a chain, because an
  // operand list of arbitrary length has no single entry to call.
  kConcat,
  // LRM 11.4.12.1 replication. Bits are repeated as many times as fill the
  // integral type the call answers with, which is a constant of the source.
  // Characters are repeated as many times as an operand says, since a string's
  // multiplier may be a value the program computes.
  kReplicateBits,
  kReplicateString,
  // Operator realizations on integral values. HIR-to-MIR lifts the
  // method-style SV operators (LRM 11.4) into `CallExpr` against these
  // entries, so the backend renders every operator mechanically: native
  // forms (`+`, `==`, ...) collapse to a single formatter, method forms
  // route through the call path. The shift / power / xnor / wildcard /
  // case / implication / equivalence ids dispatch on their left operand,
  // and the reduction ids likewise dispatch on the operand they fold.
  // `kFromBool` carries a machine predicate into the integral type the call
  // answers with, which is how the answer of a real or string comparison takes
  // the one-bit integral type LRM 11.3 and 11.4 give it.
  kPow,
  kShiftLeft,
  kLogicalShiftRight,
  kArithmeticShiftRight,
  kBitwiseXnor,
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
  // An operation the source states over values of several kinds, as it is over
  // integral values. The source names the entry above that serves every kind
  // -- a join of bits and a join of characters are one operator -- and the
  // lowering to MIR, which knows what the operation is applied to, names one
  // of these where that is integral, so every entry below MIR is over values
  // of one kind and has one declaration.
  kConcatBits,
  kIntegralPow,
  kIntegralCaseEqual,
  kIntegralBitIdentical,
  kIntegralHasUnknown,
  kIntegralIsUnknown,
  kIntegralCountBits,
  kIntegralResolveTriState,
  kIntegralResolveWiredAnd,
  kIntegralResolveWiredOr,
  kIntegralDominating,
  kIntegralMergeConditional,
  kIntegralFromInt,
  kIntegralConvert,
  // LRM 6.19.5 / 6.24.2: what an enumeration's member list answers about a
  // value -- whether it is a member, its name, and the member a step away from
  // it. The receiver is the member list the unit states once for the
  // enumeration, and each question is one library routine serving every
  // enumeration.
  kEnumerationHas,
  kEnumerationName,
  kEnumerationNext,
  kEnumerationPrev,
  // Typed parent navigation: `scope->Parent()` returns the enclosing scope as
  // the runtime `Scope` base pointer. An intra-unit upward member access casts
  // the result to the enclosing class and reads the member directly, the unit
  // owning the enclosing class's layout.
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

// A function the library declares in a scope of its own: the scope, and the
// function's name in it. The object the entry acts on, where it has one, is its
// leading argument, because a free function binds nothing. A value of a class
// type answers such an entry through its member of the same name.
struct FreeFunction {
  std::string_view scope;
  std::string_view identifier;
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

// The ways a call can end, which decide what follows it: a call that returns
// is followed by the statement after it, one that departs by the cleanups
// between it and the landing that claims the departure, and one that can do
// either by both. A call that only departs is followed by nothing, so what
// stands after it in the source is never reached.
enum class CallEnding : std::uint8_t {
  kReturns,
  kReturnsOrDeparts,
  kDeparts,
};

// What a call to an entry answers with -- the C++ distinction between returning
// `T` and returning `T&`. Most entries build a value, which the caller then
// owns and ends. The others answer with storage that already exists and that
// the caller owns nothing of: the object the call acted on, handed back so an
// access composes onto the call (a guard, LRM 11.3.5); or a part reached inside
// that object, which may be written and descended into further.
enum class EntryAnswer : std::uint8_t {
  kNewValue,
  kTheReceiver,
  kPartOfTheReceiver,
};

// How an entry that reaches one part of the value it acts on names that part:
// by a coordinate the program computes (LRM 7.4.5, 7.8, 7.10.1), by a
// declaration-order position (LRM 7.2, 7.3), or as a slice of consecutive
// elements (LRM 7.4.6). Reading the part and writing into it name it the same
// way, so the entry for each shares the answer.
enum class PartSelection : std::uint8_t {
  kElement,
  kComponent,
  kSlice,
};

// How an entry reads one operand it is handed, which is the whole of what a
// caller composing a call has to know of that operand: what crosses for it
// follows from this and from nothing about the value in hand. Each names one
// way of crossing. What one realization of an entry is told beyond it -- how
// wide the words a holder keeps are, the type a memory's keys are built at --
// is that realization's to state, where the realizations are named.
enum class OperandReading : std::uint8_t {
  // A value that goes into, or comes out of, something that already knows its
  // layout -- an element of the container the entry acts on, what the holder
  // it acts on holds, a value the entry hands back unread -- so the value
  // crosses alone.
  kHeld,
  // An integral value the entry reads the planes of, so it is told how wide
  // the value is and whether it can hold x or z.
  kBits,
  // An integral value the entry reads as a number, so it is told whether the
  // value is signed as well (LRM 11.4.3).
  kNumber,
  // An ordinal the layer above brought to the position type where the source
  // wrote it (LRM 7.4.5, 11.5.1), so its bytes are all that crosses.
  kPosition,
  // A value of a type the entry keeps and acts on afterwards, crossing with
  // that type: the value an answer starts from, a spread part of a
  // concatenation (LRM 10.10).
  kTyped,
  // The key an associative array is reached by, crossing with its type,
  // since the array holds no index type of its own. Only an entry acting on
  // an associative array reads an operand so (LRM 7.8).
  kKey,
  // A count, a flag, a code address, storage the entry acts on, or a value or
  // an object of one of the library's own types: what the entry reads as the
  // thing it is.
  kMachine,
};

// How the operands of one entry are read, in the order the entry takes them:
// the object it acts on where it has one, the engine handle where it takes
// it, then the rest. An operand past the last one stated is read as the thing
// it is, so an entry that states none takes no integral value and nothing that
// crosses with a type. An entry that is an operation over integral values
// states none: that operation's own declaration says what it takes.
class OperandReadings {
 public:
  constexpr OperandReadings() = default;
  constexpr OperandReadings(std::initializer_list<OperandReading> readings)
      : count_(readings.size()) {
    if (readings.size() > kMost) {
      throw InternalError(
          "a runtime entry states more operand readings than an entry holds "
          "-- please report this as a bug");
    }
    std::ranges::copy(readings, readings_.begin());
  }

  [[nodiscard]] constexpr auto All() const -> std::span<const OperandReading> {
    return std::span<const OperandReading>(readings_).first(count_);
  }

  // How operand `index` is read, whether or not it is one of those stated.
  [[nodiscard]] constexpr auto At(std::size_t index) const -> OperandReading {
    return index < count_ ? readings_.at(index) : OperandReading::kMachine;
  }

 private:
  static constexpr std::size_t kMost = 6;
  std::array<OperandReading, kMost> readings_{};
  std::size_t count_ = 0;
};

// What an entry is told of the type it answers at, beyond the storage its
// answer is laid out in.
enum class AnswerTold : std::uint8_t {
  // Nothing: the operands, or the library's own types, settle the layout.
  kNothing,
  // The element type of the array it builds, out of text or bits that state
  // none (LRM 5.9).
  kElementType,
  // How wide the integral value it answers is and whether that value holds x
  // or z, where its operands fix neither: a stream of bits as wide as what the
  // value it is asked of holds (LRM 6.24.3).
  kIntegralExtent,
  // How wide the integral type it is called at is, and nothing else of it:
  // how many bits a write placed into a designated value (LRM 11.5.1).
  kIntegralWidth,
};

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
  // Whether the entry is generic over a type its operands do not settle: a
  // conversion's destination, the bits of a part, as many copies as fill the
  // answer, the union a member goes into. A call to such an entry states that
  // type on its callee, as a call to a generic function states its type
  // arguments, and each target tells the entry the way it tells a type -- one
  // that resolves types where the call is written names it there, and one
  // calling code compiled once for every type hands over what that code needs
  // of it.
  bool takes_a_type_argument = false;
  // Whether the entry acts on an object, its first operand being whatever
  // reaches that object -- a class handle, the running method's own object, or
  // a write in progress into it -- as an access's receiver is. Reaching the
  // object from it is each target's, as it is for a member access: the C++
  // library takes each kind as it is, and below MIR the object is opened from
  // it the way a member access opens its receiver.
  bool reaches_an_object = false;
  // Whether the entry's implementation takes the engine handle -- to reach the
  // scheduler, or to identify the process that is running. It is an ordinary
  // operand, riding immediately after the object the entry acts on and first
  // where there is none, so a call site composing the operands has to know
  // whether to supply it and only the entry knows.
  bool takes_the_runtime_handle = false;
  // Whether the entry answers a wait its caller then stops at, whatever the
  // source-level call it lowers yields (LRM 9.7 `await`, a void task). The
  // entry builds what is waited for; the stop is the caller's.
  bool answers_a_wait = false;
  // How a call to the entry ends. An entry can depart unless its definition
  // cannot raise a departure: a run-time error it raises is a departure too,
  // and it may run the design's own code, which a `disable` anywhere reaches
  // (LRM 9.6.2), so what is owed between the call site and the frame's edge has
  // to get its turn before the departure carries on past it. Two kinds of entry
  // raise none, and their definitions say so by being `noexcept`: one that runs
  // on a way out of a body -- a cleanup, or a region's test of what it caught
  // -- where a raise would have nowhere to go, and one that only moves memory,
  // whose one possible failure is running out of it, which ends the program
  // rather than the design.
  CallEnding ending = CallEnding::kReturnsOrDeparts;
  // Whether the entry updates the object it acts on, so that object names a
  // place rather than a value -- which is what makes a receiver reaching
  // through a capability wrapper reach its write access. Where the update
  // lands and how it gets there is each target's own answer.
  bool mutates_receiver = false;
  // What the entry answers with. One answering with a part, rather than with
  // that part's value, is one where what stands at the call may be written and
  // a further access composes onto it; a target realizes such a call over a
  // part that is storage of its own as a step into it, and over a part that is
  // a view of its whole as a read of the whole and a rebuild.
  // Which entries it must do that for, and which answers a caller owns and
  // ends, are this, so the property stays with the entry and not with each
  // target that has to know it.
  EntryAnswer answer = EntryAnswer::kNewValue;
  // How the entry names the part it reaches, absent for an entry that reaches
  // none.
  std::optional<PartSelection> selects = std::nullopt;
  // Whether the LRM 7.12 method takes a `with`-clause closure as its second
  // argument. The other LRM 7.5 / 7.10 array entries (`size`, `delete`,
  // `reverse`) take none.
  bool takes_closure = false;
  // Whether the entry answers with an index by writing it into the variable
  // the source named (LRM 7.9.4 -- 7.9.7), so the call lowers to a block
  // expression: binding the answer and writing it back are steps rather than
  // one expression.
  bool writes_the_index_back = false;
  // How the entry reads each operand it is handed. A value the entry's answer
  // starts from is one of them, read with its type: it stands for the answer
  // before there is one -- an empty reduction's zero, a locator's key, a map's
  // chosen element (LRM 7.12), the index an unallocated dimension reports (LRM
  // 20.7), the element a container is seeded with (LRM 7.5.1), the value whose
  // shape a stream of bits is read into (LRM 6.24.3) -- and its contents are
  // part of what the entry reads.
  OperandReadings operands = {};
  // Which operand is the value the entry's answer starts from, for an entry
  // whose answer starts from one. The source writes no such operand, so
  // whoever builds the call supplies it, at the default of the type the call
  // answers with.
  std::optional<std::size_t> answer_starts_from = std::nullopt;
  // What the entry is told of the type it answers at.
  AnswerTold answer_told = AnswerTold::kNothing;
  // The operation over integral values the entry is, absent for an entry over
  // any other kind of value. Such an entry is that operation and nothing
  // else, so its spelling and whether it is generic over the type it answers
  // at are the operation's.
  std::optional<IntegralOp> integral = std::nullopt;
  // The entry that carries this one out over integral values, for an entry
  // the source states over values of several kinds; absent for an entry over
  // one kind. A call below the source's own layer names that entry where its
  // first operand is integral, and this one everywhere else.
  std::optional<BuiltinFn> over_integral_values = std::nullopt;
};

// The one declaration of `id`. Total over the entry set, so an entry added
// anywhere in the pipeline fails to build until every property it has is
// written down here.
[[nodiscard]] auto RuntimeEntryOf(BuiltinFn id) -> RuntimeEntry;

}  // namespace lyra::support
