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
  // LRM 20.6.2 `$bits` over a dynamically sized value: the bit count of what it
  // currently holds. Sums each element's own bit count, so an element that is
  // itself dynamically sized contributes its current width. The fixed-size case
  // folds at elaboration and never reaches this entry.
  kBitstreamWidth,
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
  // LRM 20.8.1. ceil(log2) of the operand read as unsigned; $clog2(0) is 0.
  // A constant argument is folded downstream, never in lowering.
  kClog2,
  // Observable storage cell operations. `Set` / `Mutate` thread services
  // as the second argument. `Initialize` installs the cell's declared
  // representation once at construction (no services -- no subscribers yet);
  // every later `Set` then requires the right-hand side to already be at that
  // representation.
  kGet,
  kInitialize,
  kSet,
  kMutate,
  // Net driver operations (LRM 6.5). `AttachDriver` is a `ResolvedNet` method
  // returning a driver handle; `UpdateDriver` is a `Driver` method that
  // threads services and the new contribution value.
  kAttachDriver,
  kUpdateDriver,
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
  // `Format` is a `lyra::value` free function that walks the items and yields
  // an SV `string`; it takes the engine's `$timeformat` state (for `%t`) as an
  // explicit operand the caller supplies from `TimeFormat`, so the format step
  // holds no engine state of its own. `Write` / `Writeln` are `FileTable`
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
  // LRM 20.4.3 `$timeformat` display state on `RuntimeServices`. `TimeFormat`
  // reads the current state, threaded into `Format` as the `%t` operand. The
  // setter takes the four `%t` display arguments (units power, precision,
  // suffix, minimum field width) as SV values; the reset form takes none and
  // restores the LRM Table 20-3 defaults. Distinct methods rather than one with
  // arity-driven branching, mirroring the print `Write` / `Writeln` and
  // diagnostic `EmitX` splits.
  kTimeFormat,
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
  // LRM 21.6 command-line plusargs. Free functions on `lyra::runtime` that
  // take the services handle plus SV `string` operands; the value form also
  // takes the output lvalue by reference (a `PackedArray` or `String`,
  // selected by C++ overload from the SV lvalue's declared type). Both
  // return an SV `int` (1 on prefix match, 0 otherwise).
  kTestPlusargs,
  kValuePlusargs,
  // LRM 9.4.1 `#N`. The runtime free function the scheduler suspends on.
  // The call takes the engine handle, the duration in the calling scope's
  // precision steps, and the calling scope's precision power; the runtime
  // scales to the design-global tick (LRM 3.14.3).
  kDelay,
  // LRM 9.4.2 / 9.4.2.2 / 9.4.3 value-change wait. The runtime free function
  // every wait on a signal suspends on -- an `@(...)`, an `@*`, an
  // `always_comb` / `always_latch` body, a `wait (cond)`, a continuous
  // assignment. The call takes the engine handle and the trigger set, one
  // entry per observed leaf; the process resumes when any leaf changes as its
  // edge demands.
  kWaitAny,
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
  // Ancestor-scope resolution for a hierarchical reference whose route starts
  // above the referrer (LRM 23.6 / 23.8). Called once per reference in the
  // resolve phase against the referrer's own scope handle (`args[0]`).
  // `kResolveRoot` climbs to the parent-less `$root` anchor; the descent
  // suffix (`kGetChild` / `kGetSignal`) starts strictly below it.
  // `kResolveVisibleChild` walks the enclosing chain and matches a child by
  // its canonical instance name and per-axis index (`args[1]`, `args[2]`);
  // the descent suffix starts below the matched child.
  kResolveRoot,
  kResolveVisibleChild,
  // The scope handle's runtime ABI, reached on `args[0]`. A constructor
  // registers a signal by name, looks a signal or child up by name, or hands
  // a freshly-built child to its parent to own. `kRegisterSignal` and
  // `kGetSignal` carry the signal name as a regular argument; `kGetChild`
  // carries the lookup name and per-axis index array. `kAddOwnedChild`
  // consumes the built child (a unique pointer) and returns the parent-owned
  // handle -- the child's own `Segment()` supplies both the by-name lookup
  // key and the LRM display form, so the parent never re-states them.
  kRegisterSignal,
  kAddOwnedChild,
  kGetSignal,
  kGetChild,
  // Fork-join branch dispatch. Each entry spawns every branch as its own
  // coroutine and yields the parent's wait shape per LRM 9.3.2: `kForkWaitAll`
  // for `join` (resume after the last branch), `kForkWaitFirst` for
  // `join_any` (resume after the first), `kSpawnAll` for `join_none` (no
  // wait; the call's result is `void` so the caller never awaits it). The
  // mode lives in the callee identity rather than as an enum operand so MIR
  // never carries a join-mode datum and the call's result type is what
  // selects await vs not. Each takes the services handle followed by a
  // variadic branch list -- the runtime entry is a variadic template that
  // assembles the move-only branches into the internal coroutine vector.
  kForkWaitAll,
  kForkWaitFirst,
  kSpawnAll,
  // LRM 9.6.1 `wait fork`: suspends the executing process until every immediate
  // child it spawned has terminated. Takes only the services handle; the child
  // set is the executing process's, read at runtime. The call's result is
  // `void` and the caller awaits it, the same await shape as `join`.
  kWaitFork,
  // LRM 9.6.3 `disable fork`: terminates every descendant of the executing
  // process, including the descendants of subprocesses that have already
  // terminated. Takes only the services handle; the descendant set is the
  // executing process's, read at runtime. The caller does not block, so its
  // `void` result is never awaited.
  kDisableFork,
  // Lifecycle activation registration (LRM 9.2): binds a process body's
  // coroutine to the scope's startup (`kRegisterInitial`) or shutdown
  // (`kRegisterFinal`) lifecycle. Distinct callees, not one tagged call --
  // initial and final are different registrations. Instance methods on the
  // scope handle (`args[0]`); the coroutine to register is a regular argument.
  kRegisterInitial,
  kRegisterFinal,
  // Value-layer conversion factories. HIR-to-MIR dispatches on the (src, dst)
  // type pair and emits a `CallExpr` to the matching factory; the C++ backend
  // renders each as the corresponding `lyra::value::T::Method(...)` static call
  // or instance method, with no type-driven branching. `kToInt64` is the
  // `PackedArray` accessor that yields a machine int64 (used as an inner step
  // of
  // the integral-to-real path); `kRound` is the `Real` / `ShortReal` accessor
  // that rounds to int64 per LRM 6.12.1 (used as an inner step of the
  // real-to-integral path). `kFromInt` / `kConvertFrom` are the static
  // `PackedArray` factories that build a target-shape vector from an integer
  // (rounded real value) or reshape another packed vector. `kFromPackedArray`
  // / `kFromByteArray` are the static `String` factories that build a string
  // from packed bits (LRM 6.16) or from a byte unpacked array (LRM 21.3.4.3).
  kToInt64,
  kRound,
  // Machine-value accessors used by DPI-C marshaling (LRM 35.5.6):
  // `kRealValue` reads a `Real` / `ShortReal` out as its machine float;
  // `kStringCStr` borrows a `String` as a NUL-terminated C string valid for the
  // owning string's lifetime; `kChandlePtr` reads a `Chandle` out as the opaque
  // pointer it carries. Instance methods on the receiver (`args[0]`).
  kRealValue,
  kStringCStr,
  kChandlePtr,
  // Canonical DPI-C packed and 1-bit-4-state marshaling (LRM Annex H.10.1).
  // `kToSvLogic` reads a 1-bit 4-state value out as its `svLogic` scalar
  // encoding (`value | unknown << 1`); `kFromSvLogic` builds it back in a
  // prototype's shape (`args[0]` the byte, `args[1]` the prototype). The
  // `kReadCanonical*` helpers build an SV value from a canonical buffer in a
  // prototype's shape (`args[0]` the buffer pointer, `args[1]` the prototype);
  // the `kWriteCanonical*` helpers are their inverse, writing an SV value out
  // into a canonical buffer (`args[0]` the buffer pointer, `args[1]` the SV
  // value), as an export wrapper does through the foreign caller's pointer.
  // These are free functions in `lyra::value`. An import's packed copy-in is
  // not
  // a builtin: the boundary buffer is a `DpiBitBuffer` / `DpiLogicBuffer` value
  // constructed from the SV value, and `kDpiBufferData` (an instance method)
  // reads its writable chunk pointer for the foreign call and the copy-back
  // read. `Bit` carries a 2-state (aval-only) buffer, `Logic` a 4-state buffer
  // whose `aval` is the value plane and `bval` the unknown plane.
  kToSvLogic,
  kFromSvLogic,
  kReadCanonicalBitVec,
  kReadCanonicalLogicVec,
  kWriteCanonicalBitVec,
  kWriteCanonicalLogicVec,
  kDpiBufferData,
  kFromInt,
  kConvertFrom,
  kFromPackedArray,
  kFromByteArray,
  // The opposite direction, under the LRM 5.9 string-literal assignment rules:
  // an integral destination takes the text right-justified, an unpacked byte
  // array takes it left-justified. One factory named on whichever destination
  // the call qualifies it with; that destination's declared representation
  // reaches it as a prototype operand (plus an element count for the array).
  kFromString,
  // Conforms a queue value to a destination's LRM 7.10.5 bound (a negative
  // argument means unbounded): the store boundary brings a differently-bounded
  // source to the destination's declared bound. An instance method on the
  // queue value.
  kConformBound,
  // Builds an unpacked-queue concatenation value (LRM 10.10). The first two
  // arguments are a default element of the queue's element type and its LRM
  // 7.10.5 bound; the remaining arguments are the concatenation parts, each
  // spliced or appended by its own type. A free function over the element type.
  kMakeQueueConcat,
  // Operator realizations on `PackedArray`. HIR-to-MIR lifts the
  // method-style SV operators (LRM 11.4) into `CallExpr` against these
  // entries, so the backend renders every operator mechanically: native
  // forms (`+`, `==`, ...) collapse to a single formatter, method forms
  // route through the call path. The shift / power / xnor / wildcard /
  // case / implication / equivalence ids are instance methods on the
  // receiver (`args[0]`); the reduction ids likewise take the operand as
  // `args[0]`. `kFromBool` is a static factory that wraps a host bool
  // into a 1-bit `PackedArray` (used to shape the result of a real /
  // string comparison or logical operator into the LRM 11.3 / 11.4 1-bit
  // integral result type).
  kPow,
  kShiftLeft,
  kLogicalShiftRight,
  kArithmeticShiftRight,
  kBitwiseXnor,
  kLogicalImplication,
  kLogicalEquivalence,
  kWildcardEquals,
  kCaseEqual,
  kCasezEquals,
  kCasexEquals,
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
  // owns the enclosing class's layout); distinct from the by-name `kGetSignal`
  // / `kGetChild` cross-unit navigation.
  kParent,
  // LRM 21.2.1.1 `%m` source: yields the receiver scope's hierarchical name
  // as an SV `string`. Walks `Parent()` from the receiver up to the implicit
  // root and joins each scope's own name with `.`; `%m` lowering reads it as
  // an ordinary value-string print item rather than as a context-dependent
  // format kind.
  kHierarchicalPath,
};

// True iff `id` is a type-namespace-qualified static call -- no receiver,
// the qualifier rides on the call site as the `Direct::qualification`
// (e.g. `MyEnum::first()`). Used by HIR-to-MIR to decide whether to attach
// a qualification to the call.
[[nodiscard]] constexpr auto IsStaticBuiltinFn(BuiltinFn id) -> bool {
  return id == BuiltinFn::kEnumFirst || id == BuiltinFn::kEnumLast ||
         id == BuiltinFn::kEnumNum || id == BuiltinFn::kFromInt ||
         id == BuiltinFn::kConvertFrom || id == BuiltinFn::kFromPackedArray ||
         id == BuiltinFn::kFromByteArray || id == BuiltinFn::kFromBool ||
         id == BuiltinFn::kFromString;
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

// True iff the entry yields a value whose shape the call site must supply as a
// trailing prototype argument, because the receiver does not determine it: the
// LRM 7.12 reduction, locator, and map families (an index locator's key, a
// map's chosen element, an empty reduction's zero) and the associative index
// queries (the value an unallocated dimension reports). The LRM 7.12 ordering
// family
// (`sort` / `rsort` / `reverse`) mutates in place and yields no value.
[[nodiscard]] auto BuiltinFnTakesResultPrototype(BuiltinFn id) -> bool;

// True iff `id` is an associative-array traversal entry (LRM 7.9.4 -- 7.9.7).
// The traversal family lowers to an immediately-invoked closure (mutates the
// index argument and runs the write-back inline).
[[nodiscard]] auto IsAssociativeTraversalFn(BuiltinFn id) -> bool;

// True iff the file-IO entry writes through one of its argument slots
// (LRM 21.3.4: `$fgets` writes its first arg, `$fread` writes its first
// arg, `$ferror` writes its second arg). The lowering routes such calls
// through a copy-out wrapper at statement position.
[[nodiscard]] auto IsFileOutputArgBuiltinFn(BuiltinFn id) -> bool;

// The id's stable spelling. It aligns with the SV method spelling where one
// exists (LRM 6.16 / 7.9 / 7.12 / 6.19.5) and is descriptive where there is no
// SV-side surface (`get` / `set` / `mutate` / `services`).
//
// This is an interface contract, not a display string. It names the entry in a
// dump and in a diagnostic, and it is the suffix of the runtime-library symbol
// a generated module calls, so changing it renames a linked symbol. Change it
// only to correct the entry's identity, never to improve how a dump reads.
[[nodiscard]] auto BuiltinFnName(BuiltinFn id) -> std::string_view;

}  // namespace lyra::support
