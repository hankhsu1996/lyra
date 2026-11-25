#pragma once

#include <cstddef>
#include <cstdint>
#include <optional>
#include <span>
#include <string_view>
#include <variant>

#include "lyra/support/callable_bounds.hpp"
#include "lyra/support/integral_operation.hpp"
#include "lyra/support/per_operand.hpp"

namespace lyra::support {

// Closed namespace of compiler-recognized runtime entries. Same identity at
// HIR and MIR; lives in the support layer so neither layer's vocabulary
// imports the other's. The receiver type at the call site names the runtime
// library type whose method is being invoked (value-layer containers,
// observable storage cells, runtime effects, scope handle); this enum
// carries only the method identity.
enum class BuiltinFn : std::uint16_t {
  // LRM 7.4 / 7.8 / 7.10 element of an array, reached for reading and for
  // writing. The two have different outcomes where the position names no
  // element -- a read answers the element default and creates nothing, a write
  // lands nowhere or creates the element (LRM 7.4.5, 7.8.6, 7.8.7) -- so they
  // are two entries, each answering a reference to the element whose
  // dereference is the element's place, as rustc's `Index::index` and
  // `IndexMut::index_mut` are. Which a source position calls is settled where
  // the source is read.
  kElement,
  kElementMut,
  kQueueElementMut,
  // LRM 7.4 / 7.10 several elements in a row of a container, read as a value.
  //
  // They are two operations, each with operands of its own. Elements of a
  // fixed-size or dynamic array are named by where they start, and are as many
  // as the array type they are read at holds (LRM 7.4.6). A queue's slice is
  // bounded by two positions the running program can move, and is read as a
  // queue the slice builds (LRM 7.10.1). Bits of an integral value are no
  // entry: they are a step of the place the value is.
  kElementSlice,
  kQueueSlice,
  // LRM 7.6: a value written into several elements of a fixed-size or dynamic
  // array in a row, one assignment to the whole window, answering the
  // positions of the elements it changed (LRM 4.3). A window is several
  // elements rather than one storage, so it is written by the array type's own
  // operation on the place the array lies in.
  kAssignElementSlice,
  // LRM 7.6, 7.10.1: the same for a queue's slice. It is bounded, as a read of
  // it is, by two positions the running program can move, so the queue clamps
  // them and checks the value holds as many elements as they name.
  kAssignQueueSlice,
  // Storage for one element of a queue, holding no value and leased by no
  // call, which a placement new-expression builds the element in: what the
  // queue's recycler was given back, or fresh storage from it.
  kQueueAllocate,
  // The queue held to its bound, which drops what lies past it and says so
  // (LRM 7.10.5); a queue of no bound holds whatever it is given.
  kQueueHoldToBound,
  // The same for the array a write in progress designates, taken on the
  // designation, as `putc` writes a character: the array type's own slice
  // write on the part the designation answers, whose answer -- the positions
  // it changed -- the write is told and this answers (LRM 4.3). Its operands
  // are what that slice write takes, each by reference, handed on unread.
  kAssignDesignatedRun,
  // LRM 11.5.1: an integral value with the bits a position names replaced,
  // which is how bits of a value lying in no storage are written.
  kWithSlice,
  // Raises the given message as a simulation error unless a machine boolean
  // holds: a check the language requires before an access (LRM 11.9, 25.9),
  // stated ahead of it as rustc's `Assert` stands before the projection it
  // guards.
  kAssert,
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
  // A sequence of bits carries no shape, so building is told the type it
  // builds, which fixes how many elements each array in it holds, and the
  // sequence is consumed left to right. The caller brings the sequence to
  // exactly the width that type takes, so neither widening nor truncation is
  // this entry's business.
  kToBitstream,
  kFromBitstream,
  // The same two where the value's bit count is the running program's: its
  // stream as a run of bits whose length is the run's own (LRM 6.24.3,
  // 11.4.14.1), and the value of the type it builds that a run holds exactly,
  // every part whose width the program decides taking what the parts after it
  // leave (LRM 11.4.14.4). A string is built from a run by an entry of its own,
  // since it is no array whose elements an entry is told the type of.
  kToBitVec,
  kFromBitVec,
  kStringFromBitVec,
  // What a run of bits is made of and taken apart into (LRM 11.4.14): a run of
  // an integral value's bits; another value's bits laid after the run's, an
  // integral value's and another run's being two entries; another run's bits
  // written into the run's own from a position, which builds a run whose
  // width is known first in one pass; its blocks reversed (LRM 11.4.14.2); how
  // many bits it holds, as a machine count; the run of `count` bits `at`
  // positions from its most significant end; the bits there as a value of the
  // integral type the call is made at; and the run widened to a width by zero
  // bits on its right, which left-aligns it in a target that wide.
  kBitVecOf,
  kBitVecExtend,
  kBitVecAppend,
  kBitVecInsertBits,
  kBitVecReverseBlocks,
  kBitVecLen,
  kBitVecExtractBits,
  kBitVecLoad,
  kBitVecPaddedTo,
  // LRM 11.4.14.4 `with [...]`: the stream of `count` elements of an array
  // from a position, an element outside the array streaming as the element
  // default; and the array with those elements taking the values of another,
  // a dynamic array or a queue grown first to hold them and a fixed-size one
  // refusing a range outside it.
  kWindowToBitVec,
  kWithWindow,
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
  // LRM 7.12.2 reverse: the array's elements in the opposite order.
  kReverse,
  // An array whose elements a position names is sorted by its keys instead:
  // the positions its keys come from once in ascending order, or descending,
  // which a dynamic array of positions answers by a body of its own for the
  // keys' type, comparing them by that type's own operators -- integral, real
  // or string, the only types the relational operators order (LRM 11.4.4,
  // 6.16) -- and the array's elements put in an order of positions where they
  // lie, which the array type's own body does.
  kSortOrder,
  kApplyOrder,
  // LRM 7.10 queue insertion / removal.
  kInsert,
  kPopFront,
  kPopBack,
  kPushFront,
  kPushBack,
  // LRM 7.9: whether an index names an entry, and where the least, the
  // greatest, the next greater and the next lesser index the array holds lies
  // (LRM 7.9.4 -- 7.9.7), none where there is no such index; the use writes
  // the one visited back and answers 1 or 0 (LRM 7.9.8).
  kExists,
  kAssocFirst,
  kAssocLast,
  kAssocNext,
  kAssocPrev,
  // The smallest and largest index an associative dimension holds (LRM 20.7
  // `$low` / `$high`), each the array's first or last index read at the answer
  // type, which holds its default where none is held: the `'x` LRM 20.7
  // requires, since the query answers a 4-state type.
  kAssocMinIndex,
  kAssocMaxIndex,
  // LRM 7.9.11: where the value a `default:` clause gave an associative array
  // lies, none where no clause gave one, which every index naming no entry
  // reads; and that value set.
  kAssocStatedDefault,
  kAssocSetStatedDefault,
  // The accesses and the removal above as an associative array takes them (LRM
  // 7.8, 7.9.2): by the index the source wrote, where every other container is
  // reached by a position. An index and a position are different operands, so
  // each access is an entry of its own, named where the source is read and the
  // array's type is in hand.
  kAssocElement,
  kAssocElementMut,
  kAssocDesignateElement,
  kAssocReferElement,
  kAssocDeleteIndex,
  // What an associative array's core does with its entries (LRM 7.8), each a
  // function of the library compiled once: an array of no entries told its
  // index kind and the records of its index and element types; finding the
  // entry an index of a kind whose order the core knows names; storage for an
  // entry, where its index lies, and committing it; removing one or every
  // entry; the entries in order, and the one after or before an index; the
  // value a `default:` clause gives, its storage and setting it; ending the
  // whole.
  kAssociativeArrayConstruct,
  kAssociativeArrayFind,
  kAssociativeArrayPrepare,
  kAssociativeArrayIndexOf,
  kAssociativeArrayCommit,
  kAssociativeArrayRemove,
  kAssociativeArrayClear,
  kAssociativeArrayFirst,
  kAssociativeArrayLast,
  kAssociativeArrayNext,
  kAssociativeArrayPrevious,
  kAssociativeArrayAfter,
  kAssociativeArrayBefore,
  kAssociativeArrayPrepareDefault,
  kAssociativeArraySetDefault,
  kAssociativeArrayStatedDefault,
  kAssociativeArrayEnd,
  kAssociativeArrayAdopt,
  // LRM 7.8.6: a read through an index holding x or z, or naming no entry, of
  // an array with no `default:` value, which reads the element default, and a
  // write through an index holding x or z, which is ignored; each said where
  // it was written.
  kAssociativeArrayReadAtAnInvalidIndex,
  kAssociativeArrayReadAtAMissingEntry,
  kAssociativeArrayWrittenAtAnInvalidIndex,
  // LRM 7.8.1: whether a wildcard index holds an x or a z, which names no
  // entry (LRM 7.8.6).
  kWildcardIndexHasUnknown,
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
  // LRM 15.5 operations on an event, each handed the event's value, a
  // reference to a synchronization object or to none (LRM 6.17). `EventNew`
  // makes the object a declaration with no initializer names; `Trigger` is
  // `-> e;`; `Triggered` the LRM 15.5.3 same-time-step query. A wait on an
  // event is the ordinary wait, its leaf put where `EventReportsTo` says and
  // decided by `EventOccurrences` moving from where the wait began.
  kEventNew,
  kTrigger,
  kTriggered,
  kEventOccurrences,
  kEventReportsTo,
  // LRM 16.9.3 sampled value history operations, all reached through the
  // history's own address. `Install` fills it with the expression's default
  // sampled value and fixes how far back it reaches, which the design does
  // where it activates; `Push` records what a tick settled; `At` answers with
  // what the tick a read names settled, counting back from the most recent.
  kSampledHistoryInstall,
  kSampledHistoryPush,
  kSampledHistoryAt,
  // LRM 16.14.1 concurrent assertion evaluation, reached through the storage
  // that holds one property's attempts. `Install` fixes the positions the
  // whole property starts at, what a tick runs and the assertion's control
  // (LRM 20.11). `Allocate` hands out storage for one attempt's closure and
  // `Commit` says the attempt it begins has matured; `Tick` says the leading
  // clock ticked and runs the tick, which a commit later in the same time step
  // runs again for the attempt it opens (LRM 16.14.5, 16.14.6). What the tick
  // runs reads, bounded by `LiveWord`, the Booleans every attempt shares, and
  // `Run` steps each attempt's closure with them, settling those that answered
  // and starting the statement each answer selects in Reactive where its
  // control allowed it. `TestDisable` asks each closure whether its disable
  // condition holds between ticks (LRM 16.12).
  // `Answer` and `ActionRuns` read what the one attempt an expect statement
  // holds came to, and whether the arm that answer selects may run.
  kEvaluationAttemptsInstall,
  kEvaluationAttemptsAllocate,
  kEvaluationAttemptsCommit,
  kEvaluationAttemptsTick,
  kEvaluationAttemptsRun,
  kEvaluationAttemptsTestDisable,
  kEvaluationAttemptsLiveWord,
  kEvaluationAttemptsAnswer,
  kEvaluationAttemptsActionRuns,
  // One attempt, reached through the pointer its closure is handed.
  // `SharedWord` reads the Booleans every attempt shares at this tick, as the
  // step was handed them, and `LiveWord` bounds the Boolean expressions the
  // step has to read. `NextUnstepped` walks the evaluations the tick still
  // owes a step, and `BitsAt` / `SetWord` / `Step` read a position set,
  // replace it with its successor, and mark it stepped. `SeedWord` stages a
  // set and `Seed` starts on it an instance of what an implication, a
  // followed-by or a selection asks, as an instance of one implication of the
  // evaluation whose antecedent matched. `Count` / `PartOf` /
  // `ImplicationAnswer` / `ImplicationNonvacuous` / `Resolve` answer every
  // evaluation from the last back: how many there are, which part of the
  // property one answers for, the least answer an implication's instances
  // reached and whether any was nonvacuous (LRM 16.14.8), and its own answer
  // and vacuity.
  kEvaluationAttemptSharedWord,
  kEvaluationAttemptLiveWord,
  kEvaluationAttemptNextUnstepped,
  kEvaluationAttemptBitsAt,
  kEvaluationAttemptSetWord,
  kEvaluationAttemptStep,
  kEvaluationAttemptSeedWord,
  kEvaluationAttemptSeed,
  kEvaluationAttemptCount,
  kEvaluationAttemptPartOf,
  kEvaluationAttemptImplicationAnswer,
  kEvaluationAttemptImplicationNonvacuous,
  kEvaluationAttemptResolve,
  // LRM 16.17 the wait an expect statement stops at, which watches the clock
  // and the disable condition and holds the one attempt the statement began;
  // that attempt, reached through the wait; and the tick the wait runs and the
  // attempt's closure, closures of the waiting body and so handed to it
  // once it exists.
  kWaitOnPropertyEvaluation,
  kAttemptsOf,
  kBindPropertyEvaluation,
  // LRM 10.3.3 the timer a delayed continuous assignment propagates through.
  // `Bind` names, once, what firing does; `Arm` schedules the one event
  // pending, some steps of the scope's precision from now, in place of the one
  // before.
  kTimerBind,
  kTimerArm,
  // LRM 20.9 bit counting. The argument carries the control bits, one per bit
  // position; a receiver bit counts when it matches any of them, and a bit
  // value named by several control bits still counts once. Returns the SV
  // `int` shape.
  kCountBits,
  // Whether two values are the same bits -- an unknown bit matching only the
  // same unknown, a real its own pattern -- which is what decides whether a
  // write changed a variable (LRM 4.3, 9.4.2); and whether a value holds an
  // unknown bit at all, which a value with no four-state part never does and
  // `$isunknown` answers as a bit (LRM 20.9). Each answers with a machine
  // boolean, being a predicate the runtime asks of a value it holds as well as
  // one the program writes.
  kBitIdentical,
  kHasUnknown,
  // The positions a procedural continuous assignment may hold some of (LRM
  // 10.6.2) and a net resolves (LRM 6.6.7): an integral value's bits, a
  // real's IEEE 754 bits, an unpacked structure's and a fixed-size unpacked
  // array's parts' positions one part after another, an untagged union's
  // storage's, a tagged union's held member's and then its tag's, and one for
  // any other value, which is held whole. How many a type has, asked of the
  // type, and a value with the run of positions a first one and a count name
  // taken from another of its type.
  kPositionCount,
  kTakePositions,
  // The positions a net resolves of a value whose every part is an integral
  // value (LRM 6.7.1, 6.6.7), numbered as a procedural continuous assignment
  // numbers them: all of them as one integral value of as many bits, the first
  // position least significant, which a driver of such a value gives; and the
  // value with the positions a view of resolved ones reaches replaced in place,
  // the view's first position standing at a position of the value, answering
  // whether any changed (LRM 9.4.2) and reaching only the parts they lie in.
  // The positions of every type with a fixed sequence of bits are numbered
  // so, a real's being its IEEE 754 bits, which is how a member of an untagged
  // union is a view of the union's storage (LRM 7.3); the value of the type
  // that one integral value of its positions holds is the inverse of the
  // first, which a read of such a member makes.
  kToPositions,
  kWritePositions,
  kFromPositions,
  // The value a variable of an aggregate starts as (LRM Table 7-1): a
  // structure's each member at the initial value its declaration assigns (LRM
  // 7.2.2), and at its own type's default where it assigns none; a union's its
  // first member's; an array's each element's. A type with operations of its
  // own answers it with its own body, and no value the library holds does.
  kDeclaredDefault,
  // A value of a type holding a bounded queue below its outermost dimension, as
  // a value of the same type with no queue in it bounded, and back (LRM 6.22.2
  // e, 7.6,
  // 7.10.5). Every type equivalent to it has that unbounded form, so a value is
  // assigned across types that differ only in their queues' bounds by taking it
  // into that form and out of it again. Into it nothing is lost; out of it each
  // queue keeps what its own bound admits. The first is asked of a value and
  // answers its unbounded form; the second is asked of the type it builds.
  kToBoundFree,
  kFromBoundFree,
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
  // A net of an integral type or of an unpacked aggregate a net type resolves
  // was told its data type where it was built, which counts its positions, so
  // its install carries only what the net type states (LRM 6.7.1).
  //
  // A net of a type no net type resolves -- a real, a string, a class handle,
  // or an aggregate holding one, which only an input port written with no port
  // kind declares (LRM 23.2.2.3) -- is a net of a nettype with no resolution
  // function (LRM 6.6.7): its net type contributes nothing, its one driver
  // gives it whole, and where none does it holds the value it is handed, its
  // type's default. So its install takes that value alone.
  kNetInitializeTriState,
  kNetInitializeWiredAnd,
  kNetInitializeWiredOr,
  kNetInitializeRetaining,
  kUnresolvedNetInitialize,
  // Reading what a cell holds, and replacing it. Both act on the wrapper rather
  // than name its storage: a read answers with a reference to what the cell
  // shows, and a write publishes the change to whatever the wrapper relates
  // to in the object graph (LRM 4.3). Naming the wrapper
  // itself is the bare place, which is how rebinding a reference stays a
  // different program from writing through one.
  //
  // Neither carries a runtime handle. The wrapper reaches the ambient one,
  // which has the standing of a stack pointer rather than of program data. A
  // stored value must already be at the cell's installed representation, which
  // is the store boundary's job upstream, not this entry's.
  kGet,
  kSet,
  // Reading what a cell held in the Preponed region of the current time slot --
  // its value before anything in that slot ran (LRM 4.4.2.1, 16.5.1). It acts
  // on the wrapper and carries no handle for the same reason as a read, and
  // answers a reference to that value where the cell keeps it apart from its
  // storage.
  kSampledGet,
  // Arming a cell to answer for a sampled value at all, starting from its
  // default sampled value (LRM 16.5.1). A cell nothing samples is never armed
  // and carries neither the storage nor the work of maintaining it.
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
  // that a write lands on: an element or a component, a tagged union's member
  // among the components. It answers with the part designated within the same
  // write -- a value of its own that borrows the write -- which is not what the
  // step answering with the part alone answers, and the write keeps only what
  // it lands on. The write hears what the step did to the variable where the
  // part
  // cannot say so itself -- an element made by being written, an index naming
  // none (LRM 7.8.7, 7.10.1, 7.4.5); a component is formed by nothing, so its
  // step has nothing to tell. A tagged union's member lies where every other
  // member does (LRM 7.3.2), so the change is reported at the union.
  // Dereferencing what the steps designate is where the write lands. Bits and a
  // run of elements are no storage of their own, and are
  // written within the write by the value's own operations taken on its
  // designation. Each kind of array has its own, which the call names by the
  // type it is over: a fixed-size array's, a dynamic array's and a queue's.
  kDesignateElement,
  kDynamicArrayDesignateElement,
  kQueueDesignateElement,
  kDesignateComponent,
  // A step into a member of a tagged union a write designates. Every member
  // lies at the union's own start (LRM 7.3.2), so no step of a path stands for
  // one, and the path the write reports stops at the union.
  kDesignateMember,
  // A step taken on a reference, into a part of what it refers to that may be
  // passed by reference -- an element or a component (LRM 13.5.2). It answers
  // with a reference to that part, which belongs to whatever the reference it
  // was taken on belongs to, so a write through it is a write of the same
  // variable. Forming an element can change that variable (LRM 7.8.7), and the
  // step says so where it is taken. Each kind of array has its own, which the
  // call names by the type it is over: a fixed-size array's, a dynamic array's
  // and a queue's.
  kReferElement,
  kDynamicArrayReferElement,
  kQueueReferElement,
  kReferComponent,
  // A reference to a property of an object (LRM 13.5.2, 8.4), a step taken on
  // the object. A write through it tells the object as it lands (LRM 9.4.2), so
  // the object travels with the reference, and the property is named as the
  // part the call reaches.
  kReferProperty,
  // What a call's binding of a reference leases where the reference reaches
  // through something the program can let go of while the call runs, each
  // answering the reference together with a lease that keeps that thing in
  // existence until the call returns (LRM 13.5.2, 8.29): the object a
  // property reference names storage of, which the reference itself states;
  // and an element of a queue or an associative array, or a dynamic array's
  // elements, which only the step into the element can lease, so that step
  // takes the lease, each kind its own. Then the reference a leased one
  // carries, which is what the call is handed.
  kLeaseObject,
  kReferElementLeased,
  kQueueReferElementLeased,
  kAssocReferElementLeased,
  kLeasedReference,
  // Where occurrences at the storage a capability wrapper stands for are
  // reported, which is what a wait on it enrols on and what a read report
  // names (LRM 9.4.2): a variable answers itself; a net whatever it shows its
  // value through, its own value or the object its one driver carries; and a
  // reference whatever a write through it is told to -- the variable, or the
  // object a property belongs to (LRM 13.5.2, 23.3.3) -- named by the member
  // it was bound into where it was, so a wait follows a force (LRM 10.6.2).
  kReportsTo,
  // Binding a member that owns no storage to what its connection drives it
  // with (LRM 23.3.3): the member names that storage from then on, and is
  // recorded as bound from whatever member the bound reference came from.
  kBindMember,
  // Binding a `ref` port to what its connection names (LRM 23.3.3.2): another
  // name of that, so where it names an input port's member the port goes on
  // naming whatever that member names, and a takeover through it acts on that
  // member.
  kBindAlias,
  // Binding a reference an object holds as a member to what a construction
  // was handed (LRM 13.5.2): it names that from then on, and nothing else is
  // told, since what names it is the object and ends with it -- a
  // covergroup's `ref` argument (LRM 19.3).
  kBindReference,
  // Stating, as the design is built, that a continuous assignment drives the
  // storage a reference names (LRM 10.3), which decides what a variable shows
  // once nothing overrides it (LRM 10.6.2).
  kDrivesContinuously,
  // Attaching a driver to a net (LRM 6.5), at the strength its source drives at
  // (LRM 28.11): a net's method returning the driver handle the drive
  // capability is reached through. The strength is fixed when the driver
  // attaches rather than restated on every update, because it is a property of
  // the source and not of the value it puts on the net. So are the positions
  // it drives, which its target names by constant selects (LRM 10.3.2,
  // A.8.5): they are operands of the attach, and the driver gives them as one
  // integral value, its type the call's type argument: the part itself where it
  // is integral, and its positions otherwise. So is the run of them the net
  // has, which a select's position may leave short of the part (LRM 7.4.5,
  // 11.5.1), and the driver drives only that.
  kAttachDriver,
  // Attaching the one driver of a net of a nettype with no resolution function
  // (LRM 6.6.7): an `UnresolvedNet` method returning the driver handle. Such a
  // net has no strength levels (LRM 28.12), so the attach names none.
  kAttachWholeDriver,
  // Stating, as the design is built, that what a net's driver of the whole of
  // it gives is the whole of a variable, another net, or an input port declared
  // as a variable, of the net's own type, unchanged: a `Net` method taking the
  // variable's cell, the other net, or the port's reference. A net that driver
  // alone decides shows that object where it lies (LRM 23.3.3, 4.8).
  kDriveFromVariable,
  kDriveFromNet,
  kDriveFromReference,
  // That driver's turn at time zero (LRM 4.9.1): a `Net` method
  // answering whether the net shows the object its driver carries from here
  // on, which leaves the drive nothing to do.
  kShowSource,
  // Joining two nets into one resolution (LRM 23.3.3.7): a net's method taking
  // the other net, after which every driver of either is a contribution to the
  // same fold at the strength it drives at, and both nets show what that fold
  // produces. It states no direction, because the connection it realizes has
  // none (LRM 23.3.3). Each side states the run of the part it names that its
  // net has, and only positions both sides have are joined.
  kNetJoin,
  // Putting a variable or a net, reached through its cell or a reference naming
  // it, under a procedural continuous assignment and taking it back out (LRM
  // 10.6), over the positions a first one and a count name, as the target's
  // type numbers them. Beginning one answers with the generation its
  // evaluation carries; driving states what that evaluation produced, as a
  // value of what the target holds, and answers whether it still holds any
  // positions, which is how an evaluation superseded by a later takeover, or
  // ended by a `deassign` or `release`, learns to stop.
  kBeginTakeover,
  kDriveTakeover,
  kEndTakeover,
  // The ambient `RuntimeEffects` accessor. Zero-argument free function in
  // `lyra::runtime`. Every body kind -- module process, class method, package
  // function, class static method -- reaches the runtime through this,
  // uniformly and without a receiver. Backed by a per-thread pointer the
  // attached Runtime publishes for its lifetime.
  kCurrentRuntime,
  // LRM 4.4.2: the NBA region's effects of the current slot, a `RuntimeEffects`
  // method answering the keeper a closure the program defers there is emplaced
  // into, as each entry below answering a keeper is.
  kNbaEffects,
  // LRM 9.4.5: the same NBA region, of the slot a delay control names (LRM
  // 4.4.2.4). The delay crosses as an amount in the scope's time unit with that
  // scope's unit and precision powers, the way a delay control's does, because
  // LRM 9.4.1 reads the amount before any scaling. One entry per amount
  // representation.
  kNbaEffectsAfter,
  kNbaEffectsAfterReal,
  // A region the construct placing the effect names itself, of the slot a
  // delay names, whichever region set the writing thread runs in: the region
  // crosses as an operand, a position of `support::Region`. A clocking block
  // keeps there, in Active, an input's value that skew behind its signal, and
  // takes there the sample an event reads of it (LRM 14.13). One entry per
  // amount representation.
  kRegionEffectsAfter,
  kRegionEffectsAfterReal,
  // What runs once the pass of the region running now has run everything it
  // held: a clocking block puts there onto an output's signal the one value
  // the pass's drives of it left (LRM 14.16.2).
  kPassEndEffects,
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
  // The wait for a region the standard names itself, of the slot a delay
  // names, whichever region set the waiting thread runs in, the region an
  // operand as above: a synchronous drive is carried by an execution that
  // waits for the clocking event to mature it, then stops here to reach the
  // Re-NBA region of the slot its output skew names (LRM 14.4, 14.16). One
  // entry per amount representation.
  kResumeInRegionAfter,
  kResumeInRegionAfterReal,
  // LRM 19.3: a covergroup sampled on its clocking event samples at each
  // occurrence, as though the execution making it occur called `sample`, so
  // the execution that waits for the event runs inside each occurrence.
  kRunObserving,
  // LRM 4.4.2, 21.2.2: the Postponed region's effects of the current slot.
  kPostponedEffects,
  // LRM 19.9: the coverage database is written once the run has ended, after
  // the final procedures.
  kSubmitAtRunEnd,
  // LRM 16.5: the Observed region's effects of the current slot, where a
  // concurrent assertion's tick is evaluated.
  kObservedEffects,
  // What each keeper of deferred effects implements keeping by: the
  // allocation function a placement new-expression on the keeper names, which
  // hands out storage for one effect, and the commit of the effect made in it,
  // which takes it over. Each keeper type has its own pair. A statement's
  // assertion control keeps its violation reports (LRM 12.4.2.1, 20.11).
  kDeferredEffectsAllocate,
  kDeferredEffectsCommit,
  kAssertionControlAllocate,
  kAssertionControlCommit,
  // LRM 20.11 what the assertion control tasks set for one statement.
  // `Describe` states which statement it controls, where its storage is
  // constructed; `RegisterIn` and `Register` make it one the tasks select, held
  // by an
  // instance or by none, `RegisterWithin` one a task naming a block it stands
  // in selects; `Allowed` is what an execution beginning now may do, as
  // `support::kAssertion*` bits. `SubmitObserved` and `SubmitFinal` queue a
  // deferred immediate assertion's action (LRM 16.4), taking the coroutine the
  // action runs as: the observed (`#0`) form matures in Observed and runs its
  // action in Reactive, the final form matures and runs in Postponed, and both
  // ride the executing process's deferred report queue, which the runtime
  // reads ambiently. The observed form takes where it is written, and is
  // refused once the slot is in its Postponed region.
  kAssertionControlDescribe,
  kAssertionControlRegisterIn,
  kAssertionControlRegister,
  kAssertionControlRegisterWithin,
  kAssertionControlAllowed,
  kAssertionControlSubmitObserved,
  kAssertionControlSubmitFinal,
  // LRM 20.11 an assertion control task, applied to the whole design, to an
  // instance or a generate block, to a named block or a subroutine, or to
  // every block a loop generate construct built.
  kAssertControlOfDesign,
  kAssertControlOfScope,
  kAssertControlOfBlock,
  kAssertControlOfLoop,
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
  // hierarchical name a `%m` renders, the library and cell a `%l` renders and
  // the scope's time unit for a `%t` are call-site facts absent from the format
  // text, so they ride as operands too.
  kFormatRuntime,
  // An operand of such a format string whose type decides its whole rendering
  // (LRM 21.2.1.6): it carries the text and nothing else, because the clause
  // defines no other conversion for it and so there is no value reading to
  // carry. A factory rather than a constructor, an operand that reads by
  // conversion and one that reads only as its pattern being two ways of making
  // one thing out of the same argument type.
  kRenderedFormatArgMake,
  // An operand of such a format string that reads by conversion and also
  // carries the text its type renders it as (LRM 21.2.1.6): an enumeration,
  // which is its base integral under every other conversion. A factory of its
  // own, so which of the two operations a construction is never depends on how
  // many operands it was handed.
  kPatternedFormatArgMake,
  // A print item writing the text it is handed as it is (LRM 21.2.1). A
  // factory rather than a constructor, an item being built over a value and
  // its specification the other way, so which of the two an item is never
  // depends on how many operands it was handed.
  kTextPrintItemMake,
  kWrite,
  kWriteln,
  // Diagnostic subsystem accessor and severity-fixed emit operations.
  // `Diagnostic` is a `RuntimeEffects` method returning the
  // `DiagnosticDispatcher` broker. `EmitInfo` / `EmitWarning` / `EmitError` /
  // `EmitFatal` are dispatcher methods taking a pre-formatted text and the name
  // of the scope the call is written in (LRM 20.10), one method per severity
  // rather than a single emit-with-tag, mirroring the `Write` / `Writeln`
  // split. `EmitFatal` pairs with a subsequent `Finish` call to realize the LRM
  // 20.10 "implicit $finish" requirement.
  kDiagnostic,
  kEmitInfo,
  kEmitWarning,
  kEmitError,
  kEmitFatal,
  // The same reports, made by a body that reports at its caller, whose scopes
  // mean nothing to the design's author: each names the procedure that was
  // running, as a failure the tool raises does.
  kEmitInfoAtCaller,
  kEmitWarningAtCaller,
  kEmitErrorAtCaller,
  kEmitFatalAtCaller,
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
  // LRM 21.3.4.3 `$sscanf`, a value-layer parse of a string.
  kScanString,
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
  kFileScan,
  kFileRead,
  kFileReadWords,
  kFileSeek,
  kFileRewind,
  kFileTell,
  kFileEof,
  kFileError,
  kFileFlush,
  kFileFlushAll,
  // LRM 21.7 value dumps. Free functions on `lyra::runtime` that take the
  // runtime handle and act on the run's one dump: naming its file, selecting
  // what it records -- the whole design, an instance and the levels below it,
  // or one variable, as the trigger a wait for a change of it enrols -- and
  // switching it off, on, checkpointing, bounding and flushing it. Each form
  // `$dumpvars` takes is its own entry, so which a call means is settled here
  // rather than by counting its arguments.
  kDumpFile,
  kDumpVarsLevels,
  kDumpVarsOf,
  kDumpVarsOfVariable,
  kDumpOff,
  kDumpOn,
  kDumpAll,
  kDumpLimit,
  kDumpFlush,
  // LRM 21.7.3 extended dumps of ports, each acting on the file it names, or
  // on every file where the name is empty: an instance's ports going to a
  // file, and each file switched off, on, checkpointed, bounded and flushed.
  kDumpPortsOf,
  kDumpPortsOff,
  kDumpPortsOn,
  kDumpPortsAll,
  kDumpPortsLimit,
  kDumpPortsFlush,
  // What the code reading and the code watching a data object hand a reader
  // reaching the design by name, each a method of what the reader handed in:
  // an integral value's bits, a real, a string, the object an event or class
  // variable names, how many elements an array whose size the run sets holds,
  // the key
  // of an entry of an associative array, and the trigger a wait for a change
  // of the object would enrol (LRM 38.15, 21.7.2.3, 37.17 detail 9, 9.4.2). An
  // integral value crosses with its width, so one library function takes every
  // integral value; a real is handed at the width it is held at; a key crosses
  // with its type.
  kSerializeBits,
  kSerializeReal,
  kSerializeText,
  kSerializeObject,
  kSerializeSize,
  kSerializeKey,
  kSubscribe,
  // What the code reaching a part of a data object asks of the selection a
  // reader handed it: how many steps it takes, and the step at a depth, as a
  // number or as the key of an entry at the index type it asks it at (LRM
  // 37.19, 37.26).
  kSelectionDepth,
  kSelectionNumber,
  kSelectionKey,
  // What the code settling a selection hands the settler a reader handed it,
  // by the types alone: the key an entry of an associative array at a depth is
  // kept by, with its type, how many positions a takeover counts before the
  // part a step reaches, that a step reaches a part no takeover holds at a
  // fixed position (LRM 7.8.4, 10.6.2), the step a wait's path takes there, and
  // that the path stops there (LRM 9.4.2); and, read back by a takeover,
  // whether the part lies at a fixed position and how many positions lie
  // before it.
  kSettleKey,
  kSettlePositions,
  kSettleUnfixed,
  kSettleWatchedStep,
  kSettleUnwatched,
  kSettledFixed,
  kSettledPositions,
  // What the code writing a data object takes from the deserializer a reader
  // handed it: the bits of the whole packed value it lands on, a real, or a
  // string (LRM 38.34).
  kDeserializeBits,
  kDeserializeReal,
  kDeserializeText,
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
  // functions on `lyra::runtime` taking the runtime handle, the file name, the
  // declared ranges of a memory of fixed size and none of any other, the digit
  // radix (16 / 2), and the address the run starts from, each answering the
  // words the task moves between the file and the memory, which the memory
  // type's own code then reads or writes through the entries below.
  //
  // Running upward from a start and running within a start-and-finish window
  // are two requests: the window bounds the addresses the file may name, lets
  // the run descend, and obliges the file to fill the whole of it. Each is its
  // own entry, so which one a call means is settled here rather than by
  // counting arguments.
  //
  // A load into a memory of an enumerated type takes only the members' values
  // as words (LRM 21.4.2), so it is a load of its own, handed the
  // enumeration's member list before the file name.
  kReadMem,
  kReadMemWithin,
  kReadEnumeratedMem,
  kReadEnumeratedMemWithin,
  kWriteMem,
  kWriteMemWithin,
  // What a memory's own code asks of the words a task moves. A load reads the
  // file by the addressing of the memory's kind -- its declared ranges, a count
  // of elements from 0, or keys -- and a dump plans the order it writes in;
  // then each word is asked its position in each dimension or its key, a
  // load's word at the type of the memory's words, and a dump's word or entry
  // is handed over, before the dump is written.
  kMemoryWordsReadGrid,
  kMemoryWordsReadDense,
  kMemoryWordsReadKeyed,
  kMemoryWordsPlanGrid,
  kMemoryWordsPlanDense,
  kMemoryWordsCount,
  kMemoryWordsOrdinal,
  kMemoryWordsKey,
  kMemoryWordsBytesRead,
  kMemoryWordsWord,
  kMemoryWordsPut,
  kMemoryWordsPutKeyed,
  kMemoryWordsCommit,
  // A memory's own code for a memory task (LRM 21.4, 21.5): its words taken
  // from what a load read, each landing on the element its address names, and
  // its words handed to a dump in the order the dump writes them.
  kLoadWords,
  kStoreWords,
  // An array's own code for a DPI-C open array (LRM 35.5.6.1, Annex H.12): its
  // elements written into the image in the image's order before the foreign
  // call, and read back out of it after.
  kToImage,
  kFromImage,
  // An array's own code walking it for a call of `randomize()` (LRM 18.4),
  // after the call began the walk on the variable it is or lies in.
  kWalkRandom,
  // LRM 9.4.1 `#N`. The runtime free functions answering the wait of a delay.
  // Each call takes the runtime handle, the amount of time the design asked to
  // wait, and the calling scope's time unit and precision powers; the runtime
  // rounds that amount to the scope's precision (LRM 3.14.1) and scales it to
  // the design-global tick (LRM 3.14.3). There are two because the language
  // gives a program two ways to write such an amount and reads them differently
  // -- an integral expression, whose unknown and negative values carry meanings
  // of their own, and a real expression, which can name a fraction of a unit.
  kDelay,
  kDelayReal,
  // The same amounts as a count of the scope's precision steps, for a delay
  // that is compared with others before it is due -- the rise, fall and
  // turn-off delays of a continuous assignment (LRM 10.3.3, 28.16) -- and the
  // one of those a change to a value takes, by the rule of a scalar net or of a
  // vector one.
  kDelayTicks,
  kDelayTicksReal,
  kScalarPropagationDelay,
  kVectorPropagationDelay,
  // What decides whether reaching a wait is an event for it (LRM 9.4.2), built
  // where the wait begins and held for as long as it lasts: being reached, for
  // a wait sensitive to what it reads; or the watch the compiler builds for an
  // entry of an event control -- an expression's value, or the triggers of the
  // object an event names (LRM 15.5.2) -- with or without an `iff` qualifier
  // (LRM 9.4.2.3). Three entries because the three are what the language
  // distinguishes and an absent half has no value to stand in for it, so which
  // one a wait carries is settled where the source is read rather than by
  // counting arguments.
  kObservationOnReaching,
  kObservationOfWatch,
  kObservationOfWatchQualified,
  // Building an observation evaluates nothing. Asking whether a candidacy was
  // an event evaluates it, by the waiting process for a wait that process
  // decides (LRM 4.5), and answers one or zero.
  kObservationFires,
  // LRM 9.4.2 / 9.4.2.2 value-change wait -- an `@(...)`, an `@*` -- built on
  // one trigger per watched leaf, so a stop reached again stops at the same
  // wait; and the wait of an `always_comb` or `always_latch` on the implicit
  // list a report settled (LRM 9.2.2.2.1), woken by each change of what it
  // watches, or, coalescing, woken by the net change an execution of another
  // such procedure leaves on it and by each write no such execution
  // accumulated. Each answers the wait.
  kWaitOn,
  kWaitPerChangeOnImplicitList,
  kWaitPerIntervalOnImplicitList,
  // LRM 4.9.1 / 4.9.2 the wait of a continuous assignment or a procedural
  // continuous assignment, on one trigger per source element of its
  // expression: a change the process makes itself while it runs evaluates it
  // again. Answers the wait.
  kWaitContinuouslyOn,
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
  // calls state what they read and never run; a place it reads and the bits it
  // reads there; an object a property is read on, which the wait watches
  // without keeping it; a place written and its bits; settling what was
  // reported as a procedure's implicit list (LRM 9.2.2.2.1), which keeps
  // what was read less
  // what was written; the bracket a function takes around collecting into the
  // list, which answers whether to go on at all; whether a function handed a
  // report runs -- one an evaluation handed it does; and the extent of the
  // evaluation itself, while which an exported function foreign code calls
  // reports into it, together with the report that exported function asks for
  // (LRM 35.5.3). A body reading something no wait can watch yet refuses the
  // report instead.
  kReadReportEmpty,
  kReadReportForImplicitList,
  kReadReportAdd,
  kReadReportAddObject,
  kReadReportAddWrite,
  kReadReportSettleAsImplicitList,
  kReadReportEnter,
  kReadReportLeave,
  kReadReportRunsTheBody,
  kReadReportBeginEvaluation,
  kReadReportEndEvaluation,
  kEvaluationInProgress,
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
  // LRM 20.2 simulation control. Takes the runtime handle and the finish number
  // that selects what the tool prints about it (Table 20-1). The call ends the
  // run and never returns: the calling execution departs, so no statement after
  // it runs, in any body -- a function included (LRM 13.4). `$stop` suspends
  // the simulation where `$finish` exits it, and a run nothing can resume tells
  // the two apart only in what it prints, so each names the entry that prints
  // for it.
  kFinish,
  kStop,
  // LRM 24.7 `$exit`: ends the program the running thread originates in, the
  // running thread with it, or, called from any other thread, is ignored.
  kExit,
  // Where a hierarchical name leaving an instance starts (LRM 23.6 / 23.8):
  // the nearest scope above the receiver that is of the class handed in, an
  // instance or a generate block, or past the topmost a top-level instance of
  // it. Called once per reference in the resolve phase.
  kEnclosingScope,
  // The blocks one loop generate built, the receiver (LRM 27.4): filing the
  // block built where the index stood at the value handed in, as the object
  // holding the loop builds it, and the block a hierarchical name reaching
  // that value asks for (LRM 23.6). Where a block stands among the loop's is
  // the holding object's own answer, so a name carries only the value.
  kLoopBlocksAdd,
  kLoopBlocksAt,
  // A scope's constructor gives the part every scope shares its place, first,
  // before anything of the scope runs: its parent, its segment, and the
  // definition of the class the object is of. The part itself was built
  // without them when the object was created.
  kSetScopePlace,
  // A constructor hands a freshly-built child to the tree: this consumes the
  // built child (a unique pointer), files it under the parent the child was
  // given when it was built, and returns the handle the parent owns -- the
  // child's own `Segment()` supplies the LRM display form, so the parent never
  // re-states it.
  kAttachScope,
  // How the instance at one instantiation is made, read off the construction
  // table the object being built was handed: the entry making it, as a code
  // address whose prototype the construction restores, and the table it is
  // constructed with. Which body of which unit is made there is decided where
  // the program is assembled, so no compiled body names it.
  kInstantiationEntry,
  kInstantiationTable,
  // The same for the block at one place of one of a scope's generates, read
  // by the generate's position and the place's. Which body is built there is
  // the object's, so no compiled body names it either.
  kBlockEntry,
  kBlockTable,
  // Extends a sequence still being composed by one element, answering with
  // what it became. A declaration standing for several objects counts them out
  // as it builds them (LRM 23.3.2), and what the member finally holds is the
  // last answer -- so nothing a member holds is ever partly composed, and the
  // count never has to be known before the objects are.
  kExtendSequence,
  // The object a handle names, as the class the call states the handle views
  // it as. A body runs on the object rather than on a reference to it, and a
  // handle refers to one without being one; a handle naming none fails the
  // design (LRM 8.4).
  kObjectOf,
  // The handle its first operand is, where the object it refers to is an
  // object of the class whose definition its second operand points at, and a
  // handle referring to nothing otherwise: what `$cast` asks of a class handle
  // toward a class it may not be (LRM 8.16), the class's definition answering
  // as clang's `__dynamic_cast` reads a class's type information.
  kDynamicCast,
  // Whether the object its first operand refers to is a value of the copy of
  // the interface class whose definition its second operand points at that
  // belongs to the instance its third operand is: what `$cast` asks of an
  // object toward an interface class an instance declares, once the object is
  // of it (LRM 6.22, 8.26.5).
  kIsOfInterfaceOf,
  // A box or a borrow of a closure made one of a closure of no known type, the
  // one the type the call is at holds, as rustc's unsizing coercion makes a
  // `Box<dyn FnMut>` of a `Box<T>` and a `&dyn FnMut` of a `&T`. The closure
  // stays where it was made, and is called, and for a box ended, from then on
  // through the definition it opens with.
  kUnsize,
  // The value the storage a raw pointer points at holds, taken out of it and
  // leaving it holding none, as rustc's `ptr::read` is a call in THIR that its
  // MIR then states as a read of `*p`. It is how a container's own code moves
  // an element out of storage it keeps, which a move of a whole local cannot
  // state; nothing but such code calls it.
  kPtrRead,
  // The allocation function answering the storage a raw pointer points at,
  // which holds no value, as C++'s reserved placement `operator new(size_t,
  // void*)` answers its argument: a placement new-expression naming it builds
  // its value there. It is how a container's own code puts an element into
  // storage it took; nothing but such code names it.
  kDefaultPlacementNew,
  // Fresh storage from the run's recycler of storage of one size, whose free
  // list is empty, as LLVM's `RecyclingAllocator` asks its allocator for
  // storage it has none of; a container's own code takes what was given back
  // first.
  kRecyclerAllocate,
  // Where a read through a position naming no element of an array finds the
  // element default, and where a write through one lands (LRM 7.4.5, 7.10.1),
  // each at the element type it is called at: the addresses an array's own
  // element step answers such a position with.
  kElementDefault,
  kDiscardedWrite,
  // Where the record the library keeps of the type the call is at lies, which
  // a container's core is told its element type by, as Rust's `type_id::<T>()`
  // answers the type it is called at.
  kTypeRecordOf,
  // Where a write through a position into a run-time-sized array lands, and
  // whether the write made the element there (LRM 7.10.1): the address of the
  // element the position names, or none where it names none, and whether the
  // array appended it for the write. The array type's own body answers it,
  // told where the write was written: a queue's, which can grow past its bound
  // or be written through an invalid index, says so there.
  kElementPlace,
  // What a library template holding a value of any type asks of a
  // run-time-sized array, each taking the value read or written over as its
  // receiver: the element a machine index names, by reference, the index
  // naming one, which an array type's own bodies ask of every array whose
  // elements they walk, a fixed-size one's included, as Rust's core reaches a
  // slice's elements by `get_unchecked` in its own loops; and the array made a
  // count of elements long, kept as far as
  // it reaches and grown by element defaults (LRM 7.5.1), a queue then held to
  // its bound (LRM 7.10.5). And the empty array of the call's type a core
  // answers, told its element type's record, which the type's default asks for
  // and which does the same for every element type.
  kGetUnchecked,
  kGetUncheckedMut,
  kResized,
  kQueueConstruct,
  kDynamicArrayConstruct,
  // A value of any type (LRM 15.4.9), holding a copy of its operand, kept with
  // the operand's type and the identity the call's second operand names.
  kAnyMake,
  // A reference to a value of any type, as Rust's unsizing coercion makes a
  // `&mut dyn Any` of a `&mut T`: a value of the identity its second operand
  // names is written into the storage its first operand refers to, of the
  // type the call is at, and then the closure its third operand borrows is
  // called, which carries that value on to wherever the reference stands for
  // (LRM 13.5.2).
  kSeeAsAny,
  // Writes a value of any type into what a reference to one names, where the
  // two are of one identity, and answers whether it did: a message is received
  // only into a variable of a type equivalent to its own (LRM 15.4.5, 6.22.2).
  kReceiveAny,
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
  // LRM 13.4.4: what a statement that schedules what takes effect after its
  // function returns -- a nonblocking assignment or event trigger, a
  // fork-join_none -- asks first, where it runs during a function call. It is
  // an error unless an initial or always procedure, or a fork of one, created
  // the running thread. Takes only the runtime handle.
  kRequireCallerMayScheduleAfterReturn,
  // What a deferred body that evaluates what the source wrote takes where it
  // is made -- the context it runs in on no thread once its region runs it,
  // and the generator what is running hands over to it (LRM 21.2.2, 18.14) --
  // and the entry the body opens with, which takes that context on until the
  // effect running the body returns. The first takes the runtime handle, and
  // the second is entered on the context.
  kTakeNoThreadContext,
  kEnterNoThread,
  // LRM 9.6.2 `disable <named block or task>`. `kDisable` is the statement
  // itself: it invalidates the named target, wakes the executions blocked
  // inside it so each resumes past it, ends the activities enabled within it,
  // and leaves the disabling execution when that execution is inside the target
  // too. `kEnterTarget` and `kLeaveTarget` bracket the target's extent,
  // recording on the running process which targets its execution is inside and
  // the generation each held on entry. A region of a target asks
  // `kRegionLandsEffect` whether the effect in hand lands there, and raises it
  // again where it does not.
  kDisable,
  kEnterTarget,
  kLeaveTarget,
  kRegionLandsEffect,
  // What a region binds as the effect it caught, asked of whatever unwound into
  // it: an effect is itself, a run-time error is the departure it becomes, and
  // anything that is not the design's passes on from inside the question. No
  // call in MIR names it, because the binding is part of what a region is;
  // each target calls it where it realizes one, once its handler has begun
  // handling what arrived, which is the thing it asks about.
  kReceiveDeparture,
  // LRM 9.7's `process` methods that only a process has. The class is one the
  // runtime library defines and every unit imports rather than declares, so no
  // per-unit declaration names these and the library carries each of them out.
  // `kProcessSelf` names the running process, so it acts on no object; the
  // others act on a handle it answered with. What a process has as every class
  // object does -- its generator's methods among them -- it has through the
  // entries every class object is reached by.
  kProcessSelf,
  kProcessStatus,
  kProcessKill,
  kProcessAwait,
  kProcessSuspend,
  kProcessResume,
  // LRM 18.13.3 -- 18.13.5 on a class object's own generator (LRM 18.14.3),
  // acting on whatever reaches the object: a handle, or a method's own object.
  kObjectSrandom,
  kObjectGetRandstate,
  kObjectSetRandstate,
  // LRM 8.30's weak reference to an object: one made from a handle, the object
  // it answers while the object has not expired, and an object's identity. The
  // answer tells a wait evaluating it to watch the object's expiry.
  kWeakDowngrade,
  kWeakUpgrade,
  kWeakRefersTo,
  kObjectIdentity,
  // The built-in `pre_randomize()` and `post_randomize()` (LRM 18.6.2), which
  // a call reaches where no class of its lineage writes its own: the part every
  // object starts with gives them, and they do nothing, so neither is handed
  // the object. A call of one is still a method reached through the handle it
  // is made on, which is opened before it (LRM 8.4).
  kBuiltinPreRandomize,
  kBuiltinPostRandomize,
  // The two hooks the object's own class runs, which a call naming them
  // through an interface class handle runs (LRM 8.26.9, as Lyra reads it).
  kObjectPreRandomize,
  kObjectPostRandomize,
  // A call of `randomize()` (LRM 18.6, 18.7, 18.11, 18.12), as the steps its
  // caller runs: begin on the object -- choosing what its declarations and
  // modes choose, or only what the call lists -- or on the running thread for
  // `std::randomize()`; list each variable the call names; let the object state
  // its own; add what the call writes with it; and finish, answering 1 or 0.
  kRandomizeObject,
  kRandomizeObjectListed,
  kRandomizeScope,
  kRandomizeList,
  kRandomizeConstrain,
  kRandomizeFinish,
  // What a class states to the call's problem in the functions the class
  // object declares for it (LRM 18.4, 18.5): a variable a constraint reads,
  // by how the source declared it, after the members of the enumeration it
  // is where it is one; a constant; a random object a handle reaches; whether
  // a constraint block is on; and the operations, constraints and orderings a
  // constraint is made of, each over the problem's terms.
  kConstraintMembers,
  kConstraintRand,
  kConstraintRandc,
  // A random variable walked by its own type's code after a call above, or a
  // listing, a reach through handles or a uniqueness set, begins the walk on
  // the whole of it (LRM 18.4): an array of a fixed size it holds, one whose
  // size the program chooses, and a value, the last two named by a reference
  // and the type a record names, and each element's position entered before
  // it and left after it, until the walk ends.
  kRandomWalkFixedArray,
  kRandomWalkResizableArray,
  kRandomWalkEnter,
  kRandomWalkLeave,
  kRandomWalkLeaf,
  kRandomWalkEnd,
  kConstraintVariable,
  kConstraintConstant,
  kRandomizeNested,
  kConstraintBlockEnabled,
  kStaticConstraintBlockEnabled,
  kConstraintNegate,
  kConstraintBitwiseNot,
  kConstraintReduceAnd,
  kConstraintReduceOr,
  kConstraintReduceXor,
  kConstraintLogicalNot,
  kConstraintAdd,
  kConstraintSubtract,
  kConstraintMultiply,
  kConstraintDivide,
  kConstraintRemainder,
  kConstraintBitwiseAnd,
  kConstraintBitwiseOr,
  kConstraintBitwiseXor,
  kConstraintShiftLeft,
  kConstraintShiftRight,
  kConstraintEqual,
  kConstraintLess,
  kConstraintLogicalAnd,
  kConstraintLogicalOr,
  kConstraintConcatenate,
  kConstraintSelect,
  kConstraintResize,
  kConstraintSlice,
  kConstraintRequire,
  kConstraintPrefer,
  kConstraintDisableSoft,
  kConstraintSolveBefore,
  // LRM 18.5.3: a distribution of a value, answered as which one it is, then
  // its items and its default. LRM 18.5.4: a set of values that differ, its
  // members, an array standing for each of its elements, and its requirement.
  kConstraintDistribution,
  kConstraintDistributionItem,
  kConstraintDistributionDefault,
  kConstraintUniqueSet,
  kConstraintUniqueMember,
  kConstraintUniqueArray,
  kConstraintUniqueRequire,
  // LRM 18.5.7.1 and 18.5.11: the size of an array a constraint reads, a
  // variable a function a constraint calls is handed, and settling those
  // before the rest, answering whether the description is stated again.
  kConstraintArraySize,
  kConstraintWant,
  kRandomizeSettleFirst,
  // LRM 18.8 and 18.9: a random variable's mode, through the variable the
  // class declares, its value and a reference to the part named, or every one
  // of an object's at once; a constraint block's, by its name on an
  // object, every one of an object's, or a static block's by the key naming it
  // and the class that introduced it.
  kRandModeSet,
  kRandModeGet,
  kObjectRandModeSet,
  kConstraintModeSet,
  kConstraintModeGet,
  kObjectConstraintModeSet,
  kStaticConstraintModeSet,
  kStaticConstraintModeGet,
  // A static process handed to the run to start with its instance (LRM 9.2,
  // 10.3, Annex P): the scope it belongs to, the instance whose text holds it,
  // the instance whose code it evaluates, and its definition, a constant of the
  // unit saying what kind of process it is and where the source writes it. The
  // two entries are one operation over the two ways a body is compiled: a
  // coroutine the run resumes, and a body that runs to its end each time it is
  // triggered, whose setup and pass a second constant names and which the run
  // calls on the object it belongs to, erased.
  kRegisterCoroutine,
  kRegisterReaction,
  // A final procedure is a function the run calls at the end of simulation
  // (LRM 9.2.3), so what registers it is a closure calling it on its scope.
  kRegisterFinal,
  // A concurrent assertion's pass or fail statement, started as an execution of
  // its own when an attempt's outcome selects it (LRM 16.14.1), from what a
  // registered process is started from.
  kRunAssertionAction,
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
  // The first is the int LRM 35.8 gives an exported task's entry to hand its
  // foreign caller: 1 while the execution thread is disabled -- a disable
  // reached a block it is inside, or its process was terminated -- and 0
  // otherwise. Where the execution ended the run instead, the entry does not
  // hand it back at all: it waits on the foreign stack until the run's end
  // releases it, so foreign code calling in for ever cannot keep the run from
  // ending (LRM 20.2).
  //
  // Where a `context` import's foreign call returns, what the foreign side left
  // for that return is settled: the clause's checks of what an imported task
  // returned and whether an imported function acknowledged, and a control task
  // foreign code asked for through `vpi_control`, which runs upon the return
  // (LRM 38.4), ending the run there, so the execution leaves through the gate
  // after the call. Where
  // an exported subroutine is reached, whether it may be reached at all once
  // the state was entered. No check leaves by an effect, because each holds
  // only once the execution is disabled, so the departure is already due where
  // control comes back. Outside that state the last also holds the
  // call to LRM 35.5.3, which lets an export be called only from inside a
  // `context` import's call; that breach finds the execution told nothing, so
  // it is raised, and the region of the function foreign code called settles
  // it.
  kExportedTaskReturnValue,
  kContextImportTaskReturned,
  kContextImportFunctionReturned,
  kCheckExportReachable,
  // Foreign code is about to run (LRM 35) -- at an import call that may write,
  // and where an export returns to its foreign caller -- so each side hands
  // over the standard output it holds and the two print in the order written.
  kBeforeForeignCodeRuns,
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
  // A namespace's storage is being installed, so the run holds it among the
  // objects the design brought up (LRM 37.10), with what it states of itself.
  kRegisterNamespace,
  // An integral value read out as a machine integer, which a position, a
  // count or a C argument is computed in.
  kToInt64,
  // A `Real` or `ShortReal` as a value of the integral type the call names,
  // of any width: rounded to the nearest integer, ties away from zero, and
  // kept to that width (LRM 6.12.1). HIR-to-MIR dispatches on the (src, dst)
  // type pair and emits a `CallExpr` to the matching entry, which the backend
  // renders with no type-driven branching of its own.
  kToIntegral,
  // LRM 20.5 real conversions. `kTruncate` is `$rtoi`'s reading of a real as an
  // integer, which drops the fraction where the LRM 6.12.1 conversion rounds
  // it. `kToBits` and `kFromBits` carry the IEEE 754 pattern itself rather than
  // the number it encodes, as `$realtobits` and `$bitstoreal` do; both name the
  // pattern as a machine int64, whose low half the shortreal pair occupies.
  kTruncate,
  kToBits,
  kFromBits,
  // The machine-value accessor DPI-C marshaling reads a string through (LRM
  // 35.5.6): it borrows a `String` as a NUL-terminated C string valid for the
  // owning string's lifetime, and takes that string as its receiver.
  kStringCStr,
  // A `String` an export hands to the foreign code that called it -- its
  // result, an output or an inout string (LRM Annex H.8.10) -- as a C string
  // the runtime keeps until the import call that let that code in returns,
  // since the value the entry holds ends with the entry and the foreign side
  // reads the text after it.
  kHandStringToForeignCaller,
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
  // place of the actual. The rest are what the actual's own code asks of the
  // image as it fills it before the foreign call and reads it back after: how
  // many elements it holds, the ordinal in the actual's unpacked dimension
  // where its element at a position lies, and that element written from a
  // value and read back at the type of the actual's elements. Each takes the
  // image as its receiver.
  kDpiOpenArrayHandle,
  kDpiOpenArrayElementCount,
  kDpiOpenArrayOrdinal,
  kDpiOpenArrayPut,
  kDpiOpenArrayWord,
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
  // A value converted into the destination's declared representation, which
  // is the type the call answers with. `kFromIntegral` builds the real nearest
  // the number an integral value of any width is (LRM 6.12.1), and
  // `kConvertFrom` reshapes another value of the same family into it.
  kFromIntegral,
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
  // whole: a NUL among them is a byte like any other. How many elements the
  // array has is the type the call answers with.
  kByteArrayFromString,
  kByteArrayFromBits,
  // LRM 7.6: an unpacked array taking another array's elements, one entry per
  // destination kind, the source of any of the three kinds -- a queue among
  // them, which is how a queue takes another of a different bound. Each takes
  // the source alone: what the destination's declaration states -- a
  // fixed-size array's count, a queue's bound (LRM 7.10.5) -- is the type the
  // call answers at. The elements cross as they are, so the call answers at a
  // type whose element type is the source's.
  kDynamicArrayFromArray,
  kUnpackedArrayFromArray,
  kQueueFromArray,
  // A dynamic array sized at run time: `new[N]` and `new[N](src)` (LRM 7.5.1).
  // Each is named because the argument list does not tell them apart -- a
  // `new` copying a source and an element list both carry two operands -- so a
  // target that cannot resolve overloads would have nothing to read. Building
  // one from an element list, the empty one included, is not among them: every
  // container builds from a list the same way, so its own type names that
  // factory. The call answers with the array, as it does for any static
  // factory.
  kDynamicArrayNew,
  kDynamicArrayNewCopy,
  // What the core keeping a dynamic array's elements does (LRM 7.5): storage
  // for as many elements as a count, holding no value; the place one of them
  // lies, where a placement new-expression builds it; that storage made the
  // array's elements, once it let go of the ones it held; letting go of its
  // elements, answering whether the array's own code ends them, which it does
  // not where a call leases them (LRM 13.5.2); and the storage of elements so
  // ended given back.
  kDynamicArrayGeneration,
  kDynamicArrayPlace,
  kDynamicArrayAdopt,
  kDynamicArrayLetGo,
  kDynamicArrayFree,
  // What the core keeping a queue's elements does (LRM 7.10): a ring long
  // enough for as many elements as a count; the element built in storage the
  // queue's own allocation answers put at a position; the report that a queue
  // grew past its bound, where its own code dropped what lay past it; the ring
  // of a queue holding no element given back; another queue's elements made
  // its own, once it let go of the ones it held (LRM 7.10.3); the element at a
  // position taken out of the order, answering where it still lies; and its
  // elements put in an order of positions a sort answered, or in the opposite
  // order (LRM 7.12.2).
  kQueueReserve,
  kQueueCommit,
  kQueueGrewPastItsBound,
  kQueueWrittenAtAnInvalidIndex,
  kQueueFreeRing,
  kQueueAdopt,
  kQueueUnlink,
  kQueueApplyOrder,
  kQueueReverse,
  // The values the types the library declares start as, each built out of no
  // operands and called at the type it builds: a string of no characters, a
  // value of any type holding none and a run of no bits (LRM Table 6-7,
  // 15.4.9, 11.4.14.1); and a weak reference referring to nothing (LRM 8.30).
  kDefault,
  kWeakReferenceDefault,
  // The constructors of the types the library declares, each reached by a
  // construction of the type it builds and by no call. A string is built from
  // the host text it holds, and a wildcard index from the integral value it
  // is, which keeps the type it was written in (LRM 7.8.1).
  kStringFromText,
  kWildcardIndexMake,
  // What a print, a wait and a call across the DPI-C boundary are built from:
  // the identity a child of the hierarchy carries (LRM 23.3.3.5), a leaf of a
  // value-change wait (LRM 9.4.2), a format specification, a print item and a
  // format argument over a value each reads (LRM 21.2.1), the last two with an
  // entry of their own serving every integral value, and the images of a value
  // in the form the C side reads (LRM 35.5.6.1, Annex H.7.7).
  kHierarchySegmentMake,
  kTriggerMake,
  kFormatSpecMake,
  kPrintItemMake,
  kIntegralPrintItemMake,
  kFormatArgMake,
  kIntegralFormatArgMake,
  kDpiBitBufferMake,
  kDpiLogicBufferMake,
  kDpiOpenArrayMake,
  // LRM 11.4.12.2 concatenation of strings: every part the source lists, one
  // list of strings, joined in order into one string sized once.
  kConcatStrings,
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
  // -- `===` over integers and over structures is one operator -- and the
  // lowering to MIR, which knows what the operation is applied to, names one
  // of these where that is integral, so every entry below MIR is over values
  // of one kind and has one declaration.
  kIntegralPow,
  kIntegralCaseEqual,
  kIntegralBitIdentical,
  kIntegralHasUnknown,
  kIntegralCountBits,
  kIntegralWithPositions,
  kIntegralTakePositions,
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
  // LRM 8.11 `this`: the handle referring to the object the running subroutine
  // was invoked on. A body reaches its own object through a borrowed pointer,
  // which serves every member access; the source asks for a handle instead when
  // it returns, passes, or compares the object itself, and how a target answers
  // with one follows from how that target realizes a handle.
  kSelfHandle,
  // LRM 21.2.1.5 `%m` source: yields, as an SV `string`, the hierarchical name
  // of what its argument names inside the receiver scope. Walks `Parent()`
  // from the receiver up to the implicit root, joins the name of each scope
  // below the root with `.`, and joins the argument after them where it names
  // anything. `%m` lowering reads the result as an ordinary value-string print
  // item rather than as a context-dependent format kind.
  kHierarchicalPathOf,
};

// The two entries of the diagnostic broker a report at one severity is made
// through (LRM 20.10): one naming the scope the call is written in, for the
// design's own code, and one naming the procedure that was running, for a body
// that reports at its caller, none of whose scopes the design's author wrote.
struct ReportEntries {
  BuiltinFn in_scope;
  BuiltinFn at_caller;
};

inline constexpr ReportEntries kInfoReport{
    .in_scope = BuiltinFn::kEmitInfo,
    .at_caller = BuiltinFn::kEmitInfoAtCaller};
inline constexpr ReportEntries kWarningReport{
    .in_scope = BuiltinFn::kEmitWarning,
    .at_caller = BuiltinFn::kEmitWarningAtCaller};
inline constexpr ReportEntries kErrorReport{
    .in_scope = BuiltinFn::kEmitError,
    .at_caller = BuiltinFn::kEmitErrorAtCaller};
inline constexpr ReportEntries kFatalReport{
    .in_scope = BuiltinFn::kEmitFatal,
    .at_caller = BuiltinFn::kEmitFatalAtCaller};

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

// An allocation function a placement new-expression names, as C++'s
// `operator new(std::size_t, Args...)` is: no call reaches it, the expression
// does, and it answers storage it owns ([expr.new]). A target whose
// new-expression finds the function by the placement arguments writes the
// expression; one that calls the function itself hands it the placement
// arguments, then the size and the alignment of the value placed. The
// alignment is the least the storage meets, not an exact one, since C++ hands
// it only for an over-aligned type and otherwise the storage meets the default
// alignment a new-expression assumes, which no other type's exceeds.
struct AllocationFunction {};

// C++'s reserved placement form, `operator new(std::size_t, void*)`: the
// storage its one argument points at, holding no value; it allocates nothing
// ([new.delete.placement]). A target that calls the function itself hands it
// that address alone, since where the value goes is all it answers.
struct ReservedPlacementForm {};

// A constructor of the type the entry builds, as a C++ class's is: no call
// reaches it, a construction of that type does, naming the type. A target whose
// construction names the type writes it; one that calls the entry itself is
// handed it by the declaration of the type built.
struct Constructor {};

// How code reaches the entry -- a call in one of three forms, a placement
// new-expression naming it as its allocation function or placing its value
// where it points, or a construction of the type it builds. Every entry is one
// of these, because an operation no runtime library carries out is not named
// here at all: it is answered where the source is read and no layer below
// meets it.
using EntryDeclaration = std::variant<
    FreeFunction, Method, StaticFactory, AllocationFunction,
    ReservedPlacementForm, Constructor>;

// What a call to an entry answers with -- the C++ distinction between returning
// `T` and returning `T&`. Most entries build a value, which the caller then
// owns and ends. The others answer with storage that already exists and that
// the caller owns nothing of: a reference to a part of the object the call
// acted on that is storage of its own, read-only or mutable, whose
// dereference is the part, as rustc's `Index::index` answers `&Self::Output`
// and `IndexMut::index_mut` `&mut Self::Output`; a read-only reference to what
// a capability wrapper holds, whose dereference is the storage the wrapper
// represents, as a `std::cell::Ref` dereferences to the value its cell holds;
// and a read-only reference to what that storage held when it was sampled
// (LRM 16.5.1), which the library keeps apart from it and alone can find.
enum class EntryAnswer : std::uint8_t {
  kNewValue,
  kReadOnlyReferenceIntoTheReceiver,
  kMutableReferenceIntoTheReceiver,
  kReferenceToContents,
  kReferenceToSampledContents,
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
// wide the words a holder keeps are, the type a memory's keys are built at,
// the type of a fixed-size unpacked array, whose bytes state none -- is that
// realization's to state, where the realizations are named.
enum class OperandReading : std::uint8_t {
  // A value that goes into, or comes out of, something that already knows its
  // layout -- an element of the container the entry acts on, what the holder
  // it acts on holds, a value the entry hands back unread, a value of the one
  // type the entry takes it at -- so the value crosses alone.
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
  // that type, since the entry was compiled without it.
  kTyped,
  // The key an associative array is reached by, brought to the array's index
  // type where the source wrote it (LRM 7.8.4), so its bytes are all that
  // crosses: the array's type states its index type. Only an entry acting on
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
class OperandReadings : public PerOperand<OperandReading, 6> {
 public:
  using PerOperand::PerOperand;

  // How operand `index` is read, whether or not it is one of those stated.
  [[nodiscard]] constexpr auto At(std::size_t index) const -> OperandReading {
    const std::span<const OperandReading> stated = All();
    return index < stated.size() ? stated[index] : OperandReading::kMachine;
  }
};

// What an entry asks of each closure or started body it is handed, how long
// it keeps each and how it enters each, in the order of the operands holding
// them -- an operand gathering several, the branches a fork starts, is one --
// as a Rust callee states its bounds on each callable parameter. An entry
// handed none states none.
using CallableOperands = PerOperand<CallableBounds, 3>;

// What an entry is told of the type it answers at, beyond the storage its
// answer is laid out in.
enum class AnswerTold : std::uint8_t {
  // Nothing: the operands, or the library's own types, settle the layout.
  kNothing,
  // The type of the array it builds: the array's own where that type states
  // more than its elements -- a fixed-size unpacked array's count, which its
  // bytes do not state, or a queue's bound (LRM 7.10.5) -- and otherwise its
  // element type, which says what a position of the array holds before
  // anything is written to it (LRM 7.4.5, Table 7-1). Which of these it is is
  // the type the call answers at to say.
  kBuiltArray,
  // The type of the value it answers, which nothing it is handed states: what
  // it answers where it finds nothing to answer with -- the 'x an associative
  // dimension holding no index reports (LRM 20.7) -- is a value of that type,
  // and a tuple it lays its answers out in states nothing of it in its bytes.
  kAnswerType,
  // The type of the value it answers with how a value of it starts, and the
  // operations of that type it reads the value by: a value read out of a
  // stream of bits (LRM 6.24.3), which only the type's own operations read,
  // into one at its default.
  kAnswerTypeAndOperations,
  // The element type of the element it answers with where no array is in hand
  // to state one: what a read of a position naming no element yields, and
  // storage a write to one lands in (LRM 7.4.5).
  kElementType,
  // How wide the integral value it answers is and whether that value holds x
  // or z, where its operands fix neither: a stream of bits as wide as what the
  // value it is asked of holds (LRM 6.24.3).
  kIntegralExtent,
  // The type the call is made at, which a value it answers keeps and nothing
  // it is handed states: the type of the variable a reference names, which a
  // value of any type sees it as (LRM 15.4.9).
  kTypeCalledAt,
};

// What an entry generic over a type is told of it where the call is written:
// the type itself, or the signature a closure of no known type is called at,
// which is what a box or a borrow of one is named by.
enum class TypeArgumentNamed : std::uint8_t {
  kTheType,
  kTheCallSignature,
};

// The declarations of the library's types, which name the classes of the
// runtime's an entry may view an object as.
enum class LibraryDecl : std::uint8_t;

// The class an entry acting on an object views it as: the class the program
// reaches it as, where the entry reaches its members, or a class of the
// runtime's the library declares, where that is all the entry asks of it -- as
// a C++ function taking a base class takes an object of any class extending
// it.
struct TheClassReached {};
using ObjectView = std::variant<TheClassReached, LibraryDecl>;

// How an entry hands the run a static process whose body it starts: which of
// its operands is the started body, its last, and the entry handing the run
// the same process as a reaction once a rewrite compiles the body that way,
// which takes the operands before the body, then the reaction's definition and
// the object its parts run on.
struct HandsTheRunABody {
  std::size_t body_at;
  BuiltinFn as_a_reaction;
};

// Every property of one runtime entry: what the library calls it, how code
// reaches it, and what it does with the operands it is given. A consumer
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
  // Where the library declares the entry, and what the code reaching it
  // writes.
  EntryDeclaration declaration;
  // Whether the entry is generic over a type its operands do not settle: a
  // conversion's destination, the bits of a part, as many copies as fill the
  // answer. A call to such an entry states that type on its callee, as a call
  // to a generic function states its type arguments, and each target tells the
  // entry the way it tells a type -- one that resolves types where the call is
  // written names it there, and one calling code compiled once for every type
  // hands over what that code needs of it.
  bool takes_a_type_argument = false;
  // What a target resolving types where the call is written names for that
  // type. A target laying values out itself reads what it needs off the type.
  TypeArgumentNamed type_argument_named = TypeArgumentNamed::kTheType;
  // Whether the entry acts on an object, and as which class: its first operand
  // is whatever reaches that object -- a class handle, the running method's own
  // object, or a write in progress into it -- as an access's receiver is. A
  // handle is opened by the library's call before the entry is handed it, as
  // the class the entry views the object as, and below MIR the object is
  // opened from the rest the way a member access opens its receiver.
  std::optional<ObjectView> acts_on_object_as = std::nullopt;
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
  // Whether the entry is handed the report the evaluation calling it states
  // what it reaches in (LRM 9.4.2), null where that evaluation makes none. It
  // rides after the engine handle and ahead of the operands the source wrote.
  bool takes_the_read_report = false;
  // Whether the entry is handed where the operation calling it was written:
  // rustc's `track_caller`, as the default. It rides last, after every operand
  // the source wrote, as a value of the library's location type, and the entry
  // binds it for the extent of the call. An entry takes none only where nothing
  // it does reads that binding: it cannot fail the design (LRM 20.10), it only
  // runs the design's own code, what it reports is the run's own, or it reports
  // at a location a library object kept.
  bool takes_the_location = true;
  // Whether a call to the entry comes back to the point after it, as a C++
  // function not declared `[[noreturn]]` does and a Rust function whose
  // result type is not `!`. One that never does -- an end of the run -- is
  // followed by nothing, so what stands after it in the source is never
  // reached.
  bool returns = true;
  // Whether a call to the entry can be unwound out of, as a C++ function not
  // declared `noexcept` can and LLVM's `nounwind` and rustc's
  // `fn_can_unwind` say. An entry can be unwound out of unless its definition
  // cannot raise: a run-time error it raises is a departure too, and it may
  // run the design's own code, which a `disable` anywhere reaches (LRM
  // 9.6.2), so what is owed between the call site and the frame's edge has to
  // get its turn before the departure carries on past it. A call is unwound
  // out of by the release of the stack it waits on as well, so one no
  // departure leaves can still be. Two kinds of entry raise none, and their
  // definitions say so by being `noexcept`: one that runs on a way out of a
  // body -- a cleanup, or a region's test of what it caught -- where a raise
  // would have nowhere to go, and one that only moves memory, whose one
  // possible failure is running out of it, which ends the program rather than
  // the design.
  bool can_unwind = true;
  // Whether the entry updates the object it acts on, so that object names a
  // place rather than a value -- which is what makes a receiver reaching
  // through a capability wrapper reach its write access. Where the update
  // lands and how it gets there is each target's own answer.
  bool mutates_receiver = false;
  // What the entry answers with. One answering with a reference into its
  // receiver is realized as the address of the part where it lies. Which
  // answers a caller owns and ends is this, so the property stays with the
  // entry and not with each target that has to know it.
  EntryAnswer answer = EntryAnswer::kNewValue;
  // How the entry names the part it reaches, absent for an entry that reaches
  // none.
  std::optional<PartSelection> selects = std::nullopt;
  // Whether the entry answers with an index by writing it into the variable
  // the source named (LRM 7.9.4 -- 7.9.7), so the call lowers to a block
  // expression: binding the answer and writing it back are steps rather than
  // one expression.
  bool writes_the_index_back = false;
  // How the entry reads each operand it is handed.
  OperandReadings operands = {};
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
  // The same over a queue and over a dynamic array, whose element is found by
  // its type's own body where a fixed-size array's lies at its position; a
  // queue's write of an element can also report: a position naming neither an
  // element nor the one past the last ignores the write (LRM 7.10.1), and
  // growing one past its bound drops what it wrote (LRM 7.10.5).
  std::optional<BuiltinFn> over_queues = std::nullopt;
  std::optional<BuiltinFn> over_dynamic_arrays = std::nullopt;
  // What the entry asks of each closure and started body it is handed, how
  // long it keeps each, which bounds how long it can run and so what it may
  // refer to, and the signature it calls each closure at.
  CallableOperands callables = {};
  // How the entry hands the run a static process whose body it starts (LRM
  // Annex P), for an entry that hands it one.
  std::optional<HandsTheRunABody> hands_the_run_a_body = std::nullopt;
};

// The one declaration of `id`. Total over the entry set, so an entry added
// anywhere in the pipeline fails to build until every property it has is
// written down here.
[[nodiscard]] auto RuntimeEntryOf(BuiltinFn id) -> RuntimeEntry;

// Whether an entry declared as `declared` hands out storage a new-expression
// forms its value in, holding no value yet: an allocation function, or the
// reserved placement form.
[[nodiscard]] auto HandsOutStorage(const EntryDeclaration& declared) -> bool;
// Whether that storage is where the entry's one argument points, as the
// reserved placement form's is, rather than what an allocation hands out.
[[nodiscard]] auto PlacesAtItsArgument(const EntryDeclaration& declared)
    -> bool;
// Whether an entry declared as `declared` acts on an object -- the receiver of
// a method, the first argument of a free function, the object a constructor
// builds -- rather than answering a value from its
// arguments alone, as a factory does, or storage, as an allocation function
// and the reserved placement form do.
[[nodiscard]] auto ActsOnAnObject(const EntryDeclaration& declared) -> bool;

}  // namespace lyra::support
