#pragma once

#include <cstdint>
#include <string_view>

namespace lyra::support {

// Closed namespace of compiler-recognized runtime entries. Same identity at
// HIR and MIR; lives in the support layer so neither layer's vocabulary
// imports the other's. The receiver type at the call site names the runtime
// library type whose method is being invoked (value-layer containers,
// observable storage cells, runtime services, scope handle); this enum
// carries only the method identity.
enum class BuiltinFn : std::uint16_t {
  // LRM 7.4 / 7.8 / 7.10 / 11.5 positional access. Bare returns value
  // form; `Ref` returns write-through reference. `Slice` is read,
  // `SliceRef` is write.
  kElement,
  kElementRef,
  kSlice,
  kSliceRef,
  // LRM 7.4.3 / 7.5 / 7.9 / 7.10.2. AA's `num` is an alias of `size`;
  // String's LRM 6.16.1 `len` is its own mandated spelling.
  kSize,
  kLen,
  // LRM 7.12 / 7.5 / 7.10 container ops.
  kToOwned,
  kDelete,
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
  // LRM 15.5 named-event operations. `Trigger` / `Await` arise from
  // `-> e;` / `@e;` collapsing at HIR -> MIR.
  kTrigger,
  kAwait,
  kTriggered,
  // LRM 6.19.5 enum type-static queries. `constexpr` in the runtime so
  // downstream optimizers fold the call.
  kEnumFirst,
  kEnumLast,
  kEnumNum,
  // LRM 6.19.5 enum instance methods.
  kEnumName,
  kEnumNext,
  kEnumPrev,
  // LRM 20.9 / 21.3.4.3. 2-state packed types return false; downstream
  // constant-folds those calls.
  kIsUnknown,
  // Observable storage cell operations. `Set` / `Mutate` thread services
  // as the second argument.
  kGet,
  kSet,
  kMutate,
  kServices,
  // Engine submit operations. `SubmitNba` and `SubmitPostponed` are
  // `RuntimeServices` methods taking a closure -- the NBA region commit
  // (LRM 4.4.2) and the postponed region commit (LRM 4.4.2 / 21.2.2)
  // respectively. `SubmitObserved` is a `Scope` method taking the
  // deferred-check site id and a closure (LRM 16.14.6 last-write-wins
  // settle).
  kSubmitNba,
  kSubmitPostponed,
  kSubmitObserved,
  // File-IO subsystem accessor and cancellation token operations. `Files`
  // is a `RuntimeServices` method returning the `FileTable` broker.
  // `CancellationFor` is a `FileTable` method taking a file descriptor and
  // returning a `ChannelCancellation` token snapshotted to the channel
  // currently bound at the descriptor. `IsCancelled` queries that token
  // (LRM 21.3.1 cancel-on-close), returning an SV bit.
  kFiles,
  kCancellationFor,
  kIsCancelled,
  // Print decomposes into a pure-value format step and a sink-write step.
  // `Format` is a `RuntimeServices` method that walks the items, honoring
  // the engine's `$timeformat` for `%t`, and yields an SV `string`.
  // `Write` / `Writeln` are `FileTable` methods that emit that string to
  // the descriptor's sink (LRM 21.2.1 / 21.3.1); the `ln` variant appends
  // a trailing newline.
  kFormat,
  kWrite,
  kWriteln,
  // Diagnostic subsystem accessor and severity-fixed emit operations.
  // `Diagnostic` is a `RuntimeServices` method returning the
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
  // LRM 20.4.3 `$timeformat` state setter / reset on `RuntimeServices`. The
  // setter takes the four `%t` display arguments (units power, precision,
  // suffix, minimum field width) as SV values; the reset form takes none and
  // restores the LRM Table 20-3 defaults. Two distinct methods rather than
  // one with arity-driven branching, mirroring the print `Write` / `Writeln`
  // and diagnostic `EmitX` splits.
  kSetTimeFormat,
  kResetTimeFormat,
  // LRM 21.3.4.3 scan primitives. `Scan` is a pure value-layer parser;
  // `PeekBuffered` / `AdvanceFd` are the file-side bytes-and-position
  // operations a `$fscanf` lowering composes with `Scan`.
  kScan,
  kPeekBuffered,
  kAdvanceFd,
  // LRM 21.3 file-IO subsystem methods on the `files` broker. Each is a
  // FileTable instance method whose receiver is `services.Files()`; the
  // descriptor / FD operands are SV-typed packed values, so the lowered
  // call carries the same shapes the user wrote. `Open` returns the
  // descriptor value (LRM 21.3.1); the read family yields byte counts;
  // `Close` / `Flush` are void. The `kFile` prefix disambiguates from
  // string's `kGetc` (LRM 6.16) which targets a different receiver.
  kFileOpen,
  kFileClose,
  kFileGetc,
  kFileUngetc,
  kFileGets,
  kFileRead,
  kFileSeek,
  kFileRewind,
  kFileTell,
  kFileEof,
  kFileError,
  kFileFlush,
  // LRM 9.4.1 `#N`. The runtime free function the scheduler suspends on.
  // The call takes the engine handle, the duration in the calling scope's
  // precision steps, and the calling scope's precision power; the runtime
  // scales to the design-global tick (LRM 3.14.3).
  kDelay,
  // LRM 20.3 simulation-time read functions. Each takes the engine handle
  // and the calling scope's unit power; the runtime scales the design-global
  // tick down to that unit. `$time` rounds and yields a 64-bit `time`,
  // `$stime` yields the low 32 bits as an `int`, `$realtime` keeps the
  // fractional part as a `realtime`.
  kSimTime,
  kSTime,
  kRealTime,
  // LRM 20.2 simulation termination. Takes the engine handle and the LRM
  // diagnostic level (0 / 1 / 2). The call suspends and never resumes; the
  // engine drops the process at the next dispatch. `kFatalFinish` is the
  // $fatal sibling (LRM 20.10): same shutdown protocol, but marks the
  // termination as a fatal error so the engine returns a non-zero exit code.
  kFinish,
  kFatalFinish,
  // Resolves an `ExternUp<T>` member into a borrowed pointer to the
  // observable cell it currently refers to. Used by sensitivity-leaf
  // lowering so the wait expression carries an explicit observable handle
  // rather than letting the backend derive one from the member's type.
  kAsObservable,
  // By-name scope navigation: a constructor walks a sibling unit's
  // interface, looks up an owned child by name (and per-dimension index),
  // looks up a signal by name, or registers its own signal under a name.
  // All three are instance methods on the scope handle (`args[0]`); the
  // name (and the index array for `kGetChild`) is a regular argument.
  kRegisterSignal,
  kGetSignal,
  kGetChild,
};

// True iff `id` is a type-namespace-qualified static call -- no receiver,
// the qualifier is part of the symbol identity (e.g. `MyEnum::first()`).
// Used by HIR-to-MIR to pick `BuiltinStaticCallee` vs `BuiltinFnCallee`
// without re-deriving the family axis.
[[nodiscard]] constexpr auto IsStaticBuiltinFn(BuiltinFn id) -> bool {
  return id == BuiltinFn::kEnumFirst || id == BuiltinFn::kEnumLast ||
         id == BuiltinFn::kEnumNum;
}

// True iff the function modifies its receiver argument's storage in place.
// Used to route observable cells through `Mutate` so the receiver is the
// commit snapshot, not a Get'd value.
[[nodiscard]] auto IsMutatingBuiltinFn(BuiltinFn id) -> bool;

// True iff the call's `args[0]` is the container being indexed or sliced
// (rather than a value receiver). Used by LHS-chain walkers to reach the
// root primary.
[[nodiscard]] auto IsContainerAccessFn(BuiltinFn id) -> bool;

// True iff the LRM 7.12 method takes a `with`-clause closure as its second
// argument. The other LRM 7.5 / 7.10 array entries (`size`, `delete`,
// `reverse`) take no closure.
[[nodiscard]] auto ArrayMethodTakesClosure(BuiltinFn id) -> bool;

// True iff the LRM 7.12 method yields a new value whose result shape the
// producer supplies (the reduction, locator, and map families). The ordering
// family (`sort` / `rsort` / `reverse`) mutates in place and yields none.
[[nodiscard]] auto ArrayMethodProducesValue(BuiltinFn id) -> bool;

// True iff `id` is an associative-array traversal entry (LRM 7.9.4 -- 7.9.7).
// The traversal family lowers to an immediately-invoked closure (mutates the
// index argument and runs the write-back inline).
[[nodiscard]] auto IsAssociativeTraversalFn(BuiltinFn id) -> bool;

// True iff the file-IO entry writes through one of its argument slots
// (LRM 21.3.4: `$fgets` writes its first arg, `$fread` writes its first
// arg, `$ferror` writes its second arg). The lowering routes such calls
// through a copy-out wrapper at statement position.
[[nodiscard]] auto IsFileOutputArgBuiltinFn(BuiltinFn id) -> bool;

// Short snake-case name for the id. Used for HIR / MIR dumps and any
// diagnostic that names the runtime entry; aligns with the SV method
// spelling where one exists (LRM 6.16 / 7.9 / 7.12 / 6.19.5) and a
// descriptive name where there is no SV-side surface (`get` / `set` /
// `mutate` / `services`).
[[nodiscard]] auto BuiltinFnName(BuiltinFn id) -> std::string_view;

}  // namespace lyra::support
