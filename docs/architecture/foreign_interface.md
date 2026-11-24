# Foreign Interface

The compiler does not yet carry every value across the DPI-C boundary this doc describes. It refuses
a formal that is a sized unpacked array throughout, an unpacked structure passed on its own, an open
array whose element type Annex H.7.3 puts in C-compatible form, an open array of more than three
unpacked dimensions, and an actual of an open array formal holding a dynamic array or a queue below
its outermost dimension; the rules below for the C layout and the open array bind each regardless.
Through VPI the runtime names no entry of an associative array keyed by a class handle or a
`chandle` and lists no entry of one with a wildcard index, and it accepts and never calls a signal
callback registered once the last time slot has run, or an end-of-simulation callback registered
from inside one; the rules below for names, iterations and refused callbacks bind each regardless.

Foreign code is C, written against the standard's headers, compiled apart from the design and linked
into the program that runs it. Through DPI-C (LRM 35, Annex H) the design calls it and it calls the
design back; through VPI (LRM 36 to 38) it reaches the design's objects, reads and writes them, and
asks the run to call it back. The library it calls is the tool's, and Lyra provides it routine by
routine. Every question here comes back with each routine, argument type and object kind added, so
each is answered once: what a definition is checked against, how foreign code meets the program,
what crosses the boundary, what is in force while foreign code runs, and what a handle names.

A DPI-C import or export as a callable -- its signature, its carriers, its marshaling, an export's
entry per scope -- is [callable.md](callable.md)'s. How foreign code finds the run and the scope it
runs in is [runtime_model.md](runtime_model.md)'s, and the rows a scope describes its data objects
in, which VPI reads, are stated there too. A departure stopping at a foreign frame is
[nonlocal_control_flow.md](nonlocal_control_flow.md)'s, and the regions a callback is placed in are
[scheduling.md](scheduling.md)'s.

## The library is defined against the standard's headers

**The headers are the standard's listings, shipped beside every program, and the runtime defines
each routine against them** -- `svdpi.h` (Annex I), `vpi_user.h` and `sv_vpi_user.h` (Annexes K and
M), and the compatibility names of Annex L. They are corrected only where the typesetting broke a
line or a character, or a listing contradicts its own clause, and each correction is recorded in the
headers' own README. Defining against them holds every definition to the prototype the application
was compiled against, which no copy of the declarations could promise. `svdpi.h` is among the
runtime's own headers as well, since the runtime holds a packed value's canonical representation
(Annex H.10.1.2) in the types it declares and in no copy of them.

**A routine is defined only once what it acts on exists in the runtime.** One not provided is not
defined at all, so an application calling it fails to link and the linker names the routine. A
definition that compiled and answered "not supported" would let the program build and lose the
answer at run time, since the routine's own error report (LRM 38.2) is a status the application may
never read. So:

- Save and restart (`vpi_get_data`, `vpi_put_data`) are never defined: LRM 38.36.3 lets a tool lack
  them, and Lyra has neither. `vpi_control` asked for `vpiReset` or `vpiSetInteractiveScope` answers
  0 and reports an error, since there is no `$reset` and no interactive mode.
- The SV3.1a routines Annex H.14 leaves out of what an IEEE 1800 tool owes are not defined.
- `veriuser.h`, the header of the TF routines, ships declaring nothing. IEEE 1800 does not include
  those routines (LRM 36.1 NOTE, C.2.1), and foreign code written for older tools still includes the
  header and calls nothing from it, so a call of one fails to compile or link and names it.
- Every version-bound name Annex L gives a routine Lyra defines is defined beside it, so an
  application compiled for any compatibility mode (LRM 36.12.2.1) links unmodified; and
  `vpi_free_object`, the routine's name before IEEE 1800-2009 renamed it to `vpi_release_handle`
  (LRM C.2.4), is defined beside the new name and does what it does.

**The routines whose prototype is variadic -- the printing routines and `vpi_control` -- are defined
in C**, and hand what they read to the runtime as ordinary arguments. Reading a variable argument
list is C's own facility, which every compiler builds as it builds the application's code.

**A string foreign code hands over that is a null pointer reads as the empty string** -- an import's
result, an output or inout formal, an argument of an export it calls (LRM 35.5.6), a `vpiStringVal`
written through VPI. Annex H.8.10 asks for a null-terminated C string and gives a null pointer no
meaning; the UVM library returns one to say "none" and reads it back as the empty string, so a
reading that refused it would stop every program built on UVM. The rule is the value library's,
where a string is built from C text, so every crossing reads it alike.

`svGetCallerInfo` answers where the design wrote the context import whose call is running, which the
call chain keeps for every such call; it answers false, writing nothing, where no context import's
call is running or the call has no place in the source (Annex H.9.3 leaves to the tool whether it
has one). `svDpiVersion` answers "1800-2005", the string Annex H.10.1.3 gives the canonical
representation "DPI-C" passes packed values in.

## A program has one linker

**Whatever resolves a name between two of a program's artifacts resolves every name between them, in
both directions.** A DPI-C import resolves to a global symbol the user's C defines, and an export
defines one the user's C calls (LRM 35.4); the standard fixes the symbol and the user's C is
compiled against it, so the program's link is the one party that can bind both. A design's foreign
code is therefore an artifact of the program, not an environment around it: its sources are compiled
to objects by the host compiler, under the macros and include directories its library declares
([project_model.md](project_model.md)), and every object, archive and shared library is a direct
input of the program's one link, on either backend. Nothing links the inputs to each other; the
program is where they meet the design.

**The program's entry is the run's.** Foreign code enters the design only from inside a call the
design or the run made into it: an export is called within an import call chain, which begins with a
SystemVerilog subroutine calling a context import (LRM 35.5.3), and a VPI routine within an
application routine the run called. So a program built from a design holds the `main` that starts
the run, and a foreign `main` that drives a design as a library is not a program Lyra builds: no
clause gives the calls it would make a meaning, and no import call chain would be current in them.

**Foreign code handed over compiled is linked as it is.** "All SystemVerilog applications shall
support the integration of foreign language code in object code form" (LRM J.4), as a shared library
named by `-sv_lib` (a path the tool completes with the platform's extension), by a bootstrap file
`-sv_liblist` names, or under a root `-sv_root` sets. The standard speaks of loading; here a library
is one more input of the link, so the dynamic loader binds it to the program in both directions when
the program starts, and an export it calls needs nothing more.

- **Lyra decides only what the linker and the loader cannot decide for it.** A source is told by its
  extension, as clang's driver tells one; anything else is read by its contents, so `libfoo.so.1` is
  a shared library whatever it is called. Whether a file suits the machine, whether the linker can
  read it and whether the loader finds what it needs are theirs to say. What Lyra refuses is what
  would break a promise of its own: a file the linker would read as a script, or a thin archive,
  brings files into the program that neither the kept program's key nor an emitted project's copies
  would know of; and two inputs known by one name would stand for each other where they are gathered
  into one directory.
- **The program is linked against the very file it was handed**, by its path, never found by a
  search across directories, where one input's directory could shadow another. A library declaring
  no soname is recorded by that path; one declaring a soname is recorded by it, with its own
  directory on the program's run path, so the loader finds it under its ordinary rules. What a
  library itself depends on is its provider's (LRM J.4).
- **A library is kept as a dependency whether or not the link sees it referred to**, so which
  libraries a program loads does not change with the host linker's default.
- **The order is the standard's**: each bootstrap file's entries, then each `-sv_lib`, every file
  once (LRM J.4). The loader loads a program's libraries in the order of its link, which is the
  order a symbol several define is taken from.
- **A program is kept by every file its foreign code reads**, in order, and by where each shared
  library lies, since that is what it loads when it runs.

**A bundled project carries a copy of every foreign input it was given** -- sources, objects,
archives and shared libraries -- so it builds where neither Lyra nor the originals are reachable. A
source brings every file its compile reads, itself and each header it includes, which the compiler
is asked to list as make asks it for a rule's prerequisites, each laid out as it lay so every
include finds what it found before. A shared library is copied under the file name the loader looks
for, and the program finds it among the copies beside wherever it is moved to; a library whose
soname is a path, which the loader would look for there and not among the copies, is refused when
the project is emitted. The in-place build produces the same boundary in its work directory and
copies nothing else.

## The boundary a design publishes

The user's own C sources are a second consumer of the design's output. They need the prototype of
every foreign name the design takes part in -- the imports they must define and the exports they may
call -- and the standard header those prototypes are spelled in; and the design owes a definition of
every exported name, since the C side calls a symbol nothing else defines. Both are produced beside
the design's sources, for every design, so a foreign source compiles against one include path and
links against one boundary it never restates.

**Each unit states its own part of the boundary, and the union is assembled by whatever collects the
files.** A DPI-C name is program-global and lives in a name space of its own rather than in any
compilation unit's (LRM 35.4, 35.7): the name space belongs to no unit, which does not stop a unit
from stating a name. Every unit taking part in a name writes that name's prototype, and the header a
foreign source includes names those fragments and states nothing itself. Repeating an identical C
prototype is what every C header does. That every declaration of one name publishes the same
prototype (LRM 35.5.4) is checked, for a name crossing units, by the user's own C compiler, the one
party that sees them together; LRM 35.4 requires C naming and forbids overloading, so the mangling
that makes a cross-unit disagreement a C++ link error is not on offer. Within one unit the front end
rejects declarations of one name that disagree.

**A symbol the design must define is defined by every unit that declares it, and the link keeps
one** ([emission_model.md](emission_model.md) states the merge rule). Each backend spells it as a
weak definition, which the linker keeps one of and never drops for being unreferenced, since a
foreign entry point is reached only from outside the emitting language. The surface is
target-language-neutral -- the same prototypes whichever backend runs the design -- so a foreign
source compiled against it stays correct across backends.

## An open array crosses as a canonical image

A DPI-C open array (LRM 35.5.6.1) is a formal with one or more unsized dimensions, so one foreign
function serves actuals of different sizes, and the C side reaches it through a handle and the
library surface of Annex H.12. Annex H.11.4 exempts it from the C layout every other unpacked array
crossing the boundary must have, and Annex H.7.3 states its representation instead: canonical form
where the element type is a 2- or 4-state scalar or packed type, C-compatible otherwise.

**The call site builds a contiguous canonical image of the actual, owning its own storage, and hands
the foreign side a handle to it.** Every element is in its Annex H.7.7 form, the dimensions in the
LRM 7.6 left-to-right order. A direction that writes back reconstructs one value from the image
after the call and stores it through the actual's ordinary write path, so nothing aliases live
storage and the image is an ABI temporary like any other carrier. Filling the image and reading it
back is the actual's array type's own code -- a body of the type that walks its elements in the
image's order and hands each one over by its ordinal, at the element's type -- so the image itself
is monomorphic and needs no accessor table. Its two-state and four-state forms are one type
distinguished by a field, since the C side spells both `svOpenArrayHandle`. What the image needs of
the element type -- its width and state domain, which size each element's canonical form -- comes
from the actual's declaration as an operand, never from an element, which an empty actual does not
have.

**The whole-array and element pointers are served where an element's canonical form is also how an
individual value of its type crosses, and are NULL otherwise** (Annex H.12.4). A `bit [7:0]` crosses
alone as `svBitVecVal*`, its canonical form, so its pointer is valid; a `byte` crosses alone as a C
`char` and its canonical element is a 32-bit group, so its pointer is NULL. The canonical and scalar
element accessors are the paths the standard makes unconditional, and they serve every element type
an open array takes.

**The formal's open shape lives on its ABI carrier, not in the type system.** LRM 35.6.1.1 has the
unsized ranges determined at the call, so the formal's unsized shape is the type of no value that
crosses: the boundary temporary, the type a value is marshaled at and the write-back destination all
need the actual's type. The carrier records only what the formal's type does not -- whether its one
packed dimension is unsized, and for each unpacked dimension the range the declaration fixes or its
absence -- and the element's type stays on the formal. Each unsized dimension reports the actual's
range and each sized one its declared range, unnormalized (Annex H.7.5, H.7.6).

**The actual's own shape is stated where the call is, because the image is built before the call:**
a fixed-size dimension's range from its declaration, and a dynamic array's or a queue's, which is an
actual of an open array formal as it is of any formal (LRM 7.7, 35.6.1.1), as `[0:size-1]` with its
size read as the call begins. The foreign side cannot change the image's extent, so a formal that is
written back gives the actual its value at the size it had when the call began. An exported
subroutine takes no open array (LRM 35.5.6.1, H.8.2), and the front end rejects the declaration.

A formal that is a sized unpacked array throughout, and an open array whose element type Annex H.7.3
puts in C-compatible form, cross in the C layout Annex H.11.4 requires.

## What is in force while foreign code runs is an extent

LRM 35.5.3 gives a `context` import a scope the foreign side observes, stated over the import call
chain rather than over the subroutine: the context "is created that is equal to the instantiated
scope of the import declaration" when the import is called, `svSetScope` replaces it, a call to an
export restores it on return, and it ends when the chain returns to its root.

**The scope is installed by an entering call and given back by a leaving one, and the body states
both**, as a region with a cleanup on every way out
([nonlocal_control_flow.md](nonlocal_control_flow.md)). Nothing is left for a target language to run
at scope exit, since only one of the two backends has destructors, and the cleanup is what makes
"however the call ends" true: an export the foreign side calls back can raise a control effect or
end the run.

**The chain belongs to whatever is running, not to a process**, as the generator a draw uses does
([runtime_model.md](runtime_model.md)): a subroutine body is reached from a process and from code
that runs on no thread alike, so a context import is legal wherever the language allows a call,
every position that initializes static-lifetime state before any procedure starts included. Two
foreign calls suspended on different processes never share a chain, because a process owns what is
in force while it runs and a resume installs it.

**The extent covers the statement that hands control to the foreign side**: for a function import
the boundary the call sits in, and for a task import the await of the stack the call runs on, since
the scope has to be current before that stack is entered and still current when it resumes. A value
settled inside the extent and read after it lands in a binding declared ahead of it. A plain import
carries no extent at all, since LRM 35.5.3 instruments only a `context` one. What a context import a
package or `$unit` declares observes is the standard's silence, ruled under "What the standard
leaves to the tool" below.

## A foreign call that can wait runs on a stack of its own

A task import may consume time (LRM 35.5.1.1), so its foreign call runs on a stack the runtime
allocates, and the SystemVerilog side can park while the foreign frame stands. Generated code
reached from that stack -- an export the foreign code calls -- is entered as generated code is
entered anywhere: the runtime runs it in stretches between parks and names, around each stretch,
where it allocates the cells its awaited calls answer into. Those cells may have to survive a park,
the storage an exported subroutine's entry completes into among them, so the foreign call owns one
store of them for its whole life. A store per stretch is released too early, and no frame outside
the call lives long enough.

A thread that blocked under a foreign call is resumed by driving the innermost foreign call it has
entered, never its frame directly, or the scheduler would resume a frame whose continuation is
buried under a native stack. A call that returns goes on in the frame that made it, on the stack
that frame runs on, which for a call made inside another is that call's.

**The runtime tells the address sanitizer of every switch that runs code on a foreign call's
stack**, from both sides, as the sanitizer's interface asks of a fiber library
(`__sanitizer_start_switch_fiber`), including the two the fiber library makes on its own: leaving a
call's stack for good, and unwinding a waiting call when it is destroyed. The sanitizer keeps its
own account of which stack runs, and a throw on one it was not told of is unwound against the wrong
bounds. One source serves both builds of the runtime, through weak references a program linking no
sanitizer leaves empty. The frames the sanitizer keeps for a stack are kept until the call's stack
is destroyed, since the fiber library's own frames still run on them before its last switch.

## A control foreign code asks for is carried out where its routine returns

`vpi_control(vpiFinish)` and `vpi_control(vpiStop)` are carried out upon return of the application
routine (LRM 38.4): the context import that called them, innermost where imports nest through an
export, or the callback the run called, innermost where a callback is called from inside a foreign
call. The request is recorded on that call and done where it returns, as the design's own `$finish`
would be done where the call is written, and a callback's, which no statement wrote, at no place;
nothing is raised among the foreign frames. A callback's call is no import's: no DPI scope is
current in it and no export may be called from it (Annex H.9), even where it runs on the stack of an
import whose write it was called for. A plain import called from inside a context import's call is
uninstrumented (LRM 35.5.3), so its request is recorded on that call and done where it returns; a
request with no such call running answers 0 with an error, LRM 35.5 leaving what VPI does from a
plain import called from SystemVerilog unpredictable.

## A VPI handle is a reference to an object

The design hierarchy holds more objects than the run constructs: a task declared in a loop's block
is a task of each block the loop built (LRM 36.10), and an instance array is an object beside its
elements. So what an object is, and what a handle holds, are stated apart from what the run built.

**An object is one of these:** a scope the run constructed; a package or a compilation unit's scope;
the set one declaration builds in a constructed scope -- an instance array, or the blocks a loop
generate counts out (LRM 37.11, 37.85); a scope the run makes no object for -- a block, a fork, a
loop, a task, a function, one imported from C among them (LRM 37.41), a class or a clocking block
(LRM 37.12) -- as it stands in the nearest constructed scope or namespace around it, named by that
holder and the declaration's record, so one declaration held by two scopes is two objects; a
variable or a part of one (LRM 37.15 to 37.19, 37.26); a constant of a unit a relation answers -- a
typespec, a member of one, an enumeration's constant, a range and a bound of one (LRM 37.22, 37.25);
and, of the simulation rather than the design, a callback and a time queue (LRM 37.80, 37.81).

**A handle refers to an object and holds nothing else of it.** Two handles compare equal where they
refer to one object, however each was reached (LRM 38.3), and releasing one leaves the other. A
handle is the address of a word holding its own number in the run's table, and one foreign code
passes back is looked for among the addresses the table issued before anything is read through it,
so a pointer that is no handle is reported as an error (LRM 38.2) rather than followed. An iteration
holds the objects its relation answered when it was made and the object it was made on (LRM 37.84),
is released when stepping reaches its end (LRM 38.40), and a relation that answers nothing gives no
iterator (LRM 38.23). A routine asked for a relation or a property Lyra does not reach answers
nothing and says so through `vpi_chk_error` (LRM 38.2). A compatibility mode changes what is a
variable and an array of them and what a scope's relations to them hold, as LRM 36.12.1 Table 36-10
says for the mode, and nothing else: the routines answering a type or a relation take the mode the
application was compiled for (LRM 36.12.2.1), and an application compiled for none is read in the
one the run's `+lyra+vpi+compatibility=<version>` names, else the latest (LRM 36.12.2.2), a run
option so that choosing one needs no rebuild.

**A variable's object is what holds it, the position of its row among the data objects the holder
describes, and its selection**: the steps from the variable to the part, each a member's index, an
element's index as the source would write it, or an associative entry by the key the array keeps it
by, then the steps into the packed value the selection ends at. It names an index, never an address,
so a handle to an element of a dynamic array is whatever the array holds there when it is used (LRM
37.17 detail 27), and two handles reaching one part by a name and by an index are one object; within
a packed value a part is the bits it is, so a bit named through a member and by its index into the
whole are one object too (LRM 38.3). An index foreign code hands an associative array is the index
of the select it would write (LRM 7.8.4, 38.19): code compiled for the array's type converts it into
the index type by the conversion the design's own casts take, and the handle is made from the key
that code hands back, settled whole when it is made by the types alone, so 300 into a `byte` index
names the entry at 44. A wildcard index, which no one type holds, stays the number it was named by,
read unsigned (LRM 7.8.1). A `ref` port's name is a reference object of the same shape (LRM 37.15),
whose actual object is the variable whose watch is the reference's own, found in a table of every
variable by its storage that the run makes once, at the first such question.

**A value is read, written, forced and released by code compiled where its type is known**: one body
per type and operation, taking the one step a selection states at its depth -- a member, an element
through the declared range, an entry by its key, a tagged member with its tag checked -- and handing
the rest to the bodies of the part's type, as a type's derived serialization hands each field to the
field type's own; the class declaring the variable states one body per operation that finds it by
its row. So the code compiled grows with the types and the data objects, never with the paths
through them, and how each type is laid out and each select maps an index is stated once, by the
design's own steps. A read takes each step as the design's own read does, so an element it does not
hold reads as its type's default and is not made (LRM 7.4.6, 7.8.6); a write takes each step as a
reference to the part is taken, so it lands as the design's own write through a `ref` does (LRM
13.5.2, 9.4.2) and an entry it names is made (LRM 7.8.7). A member of a union is never lent by
reference (LRM 13.5.2), so a read reads the member's value and a write writes the union whole with
the member laid in, as the design's own write of the member lands. Which bits of a packed value a
part is, is the runtime's: a write of some bits is the value read, those bits laid in and the value
written whole, so a wait on its other bits sees no change of them. Where a selection ends at an
array whose size the run sets, a read hands how many elements it holds and, asked for, an
associative array's indices in the order it keeps them (LRM 7.9.4), which its elements and the range
of its first dimension are read from; a handle to an entry keyed by a class handle holds that object
as the key it names, so the object lives while the handle does. A variable declared `const` is
neither written nor forced (LRM 6.20.6).

**Foreign code enters the design on the terms a call it makes into the design does** (LRM 35.9): no
code of the design is entered where what called into foreign code was disabled or ended the run,
while what the records alone answer is answered. A read or a write the design cannot do -- a member
of a tagged union other than the one its tag names, an operation Lyra does not carry out -- is that
request's error, answered through `vpi_chk_error` (LRM 38.2), and the run goes on as it would had
the request not been made, since what failed is what foreign code asked, not the design; a debugger
interface keeps the program it reflects running and answers the request with an error. A departure
raised under it was recorded where it was raised and is taken as the import returns, and nothing is
raised into foreign code.

**A force from foreign code is a procedural force** (LRM 38.34) and takes over what one takes under
the reading the build was given, which is the front end's: the severity it gives a procedural force
of a part of a variable, read once where the design is loaded, so the two cannot disagree. Under the
standard's reading only a whole singular variable is taken and anything less answers that it is
forbidden (LRM 10.6.2); under the tolerant reading of a forced part
([language_conformance.md](language_conformance.md)) any part is taken as a procedural force of it
would be. A release hands back the value the object holds after it.

**A callback is named by its identity among those the run registered**, so a handle to it is
released like any other and two registrations of the same data are two callbacks; one removed or
ended names none, and a handle to it then answers that. A time queue is named by its time, answers
it through `vpi_get_time`, and is listed in increasing time -- the current slot only while it holds
events ahead of its Postponed region, and only slots the run will still run. An object relates to
the callbacks placed on it, a time queue to those waiting in it, and no object to a callback related
to none of the diagram's objects -- one for a simulator action, on a signal, on a task or a
function, or due when time next moves.

## A callback runs where its reason places it

Foreign code asks the run to call it back with `vpi_register_cb` (LRM 36.9.2, 38.36): after a change
of an object, at a point of a time slot, or for a simulator action, until it withdraws the request
or the moment is spent. Every write already tells the observers enrolled on what it wrote as it
lands (LRM 4.3), and no code of the design runs while one of those lists is walked; foreign code is
code of that kind. So a change is pushed to the callback, and its foreign code is called once the
telling has ended, never inside it.

**A callback is a registration attached to the source its reason names**, kept as copies of its own
since what it was handed belongs to the caller (LRM 38.36): a change of a variable to the place a
wait on it enrols on (LRM 9.4.2), a disable to the target it ends (LRM 9.6.2), a point of a time
slot to a timer in that slot's region, the next advance of time to that advance, a simulator action
to the run, a signal to the one owner of the process's signals. Nothing else watches storage, so a
run that registers no callback pays nothing to have its storage watched, and a source the run holds
rather than storage -- a disable target, an action, a signal -- lists the callbacks waiting for it,
so an occurrence finds its own without walking the rest. Removal takes a callback out of the
registry and of its source at once, so it is called no more and keeps no time slot (LRM 38.39); it
is destroyed when the last of its owners -- the registry while it stands, each call of it while that
call runs -- lets it go, taking its handle to the object it was registered with.

**A time callback is a timer the scheduler runs in the PLI region LRM Table 4-1 places it in**, as
it runs any other ([scheduling.md](scheduling.md), Region structure).

**A callback runs as an application routine of no thread.** Its foreign code runs on no thread of
the design and as no procedure, inside an application routine call of its own, so a `vpi_control` it
makes is carried out as it returns. A value-change, size-change or disable callback runs at the
occurrence, once every observer the occurrence reached has been told and before the execution that
made it goes on, as a covergroup sampled on its event does. A disable is the occurrence once it has
run: it calls back each named block and task it ended activity in -- a block nested in the one
named, a task down the chain of enables from it -- once however many activations it ended, read by
the disable itself before it ends them (LRM 9.6.2). An execution an earlier disable or a kill
already ended is no longer active, so a disable of a target nothing active is inside calls nothing
back. No `disable` ends a function (LRM 9.6.2), so a disable callback placed on one is kept and
never called.

**A call is handed what its occurrence was.** A change is read as it is made, and the callbacks it
made due are called once what was due before them has been, so each is handed the changes in the
order they were made, each its own value; a size change is called ahead of a value change one
operation made with it (LRM 37.17 detail 14). A callback on a part of a variable is enrolled where
the design's own wait on that select is, so a write is told to it where it may have changed the
part, and whether the part changed the callback decides: it keeps what it last read -- a value, a
size, or which object a class variable names -- and is called where what it reads now is another,
whatever the write said of what it did. A class variable's object is told apart by its ownership
record, held weakly, so an object made where an ended one lay is another and a callback keeps no
object alive. Time and value come in the forms the registration asked for (LRM 38.36.1.1), and each
callback for an error reads that error through `vpi_chk_error`, whatever a callback called before it
for the same error cleared.

**A moment the run will not reach is refused.** Whether a moment is one the run still reaches is the
scheduler's one answer for any moment, which a callback asks rather than working out from the
scheduler's state. A callback for a point of the current slot that the slot does not run again is
refused, the read-write error LRM 38.36.2 lists, as is a start-of-time one of no delay once the slot
is under way, unless asked from inside a start-of-time callback, which runs where it would; so is a
callback for a simulator action the run has passed, and a time callback for a later slot once none
will run. Writing a value in the read-only Postponed region is refused too, whoever asks (LRM
4.4.2.9, 4.4.3.10). A reason for a feature Lyra lacks -- save, restart, reset, the interactive mode
-- is refused as absent, and a reason Lyra owes and does not carry out as not supported, never
accepted to wait silently.

## What the standard leaves to the tool

- **A namespace's context import.** LRM 35.5.3 defines a context import's scope as the instance its
  declaration is in, and says nothing of a package's or `$unit`'s. A namespace is never
  instantiated, so such an import observes no scope wherever it is called from: `svGetScope` answers
  null, an export a package declares runs, and one a module declares finds no scope and is reported
  where it looks up its entry.
- **Names and scopes.** A scope the source did not name answers no name and no full name, but a
  scope named inside one answers, as its full name, what `%m` prints inside it, though no
  hierarchical name reaches it (LRM 9.3.4, 37.10 detail 5). `vpiInternalScope` from an instance or a
  generate scope answers every scope directly inside it, each element of an array or a loop on its
  own and no array itself. A package is reached by its name alone where no top-level instance has
  that name. The built-in package `std` is no object, its classes being Lyra's own definitions. A
  compilation unit's scope answers `wire` as its net type and no default lifetime, and one that
  declares nothing is no object.
- **Definitions.** The delay mode and decay time directives are informative (Annex E), so a
  definition answers no delay mode and an undefined decay time. Lyra reads no configuration (LRM
  33.4), so a definition answers none, the cell its text lies in as its cell (a module declared
  inside another lies in that one's, as `%l` prints it, LRM 33.7), and the library its text was read
  into (LRM 33.3), `work` where the command line names the design.
- **Time.** Asked of no object, the time unit and the time precision are the simulation time unit,
  as LRM 38.6 says and DPI's own queries answer (Annex H.13), where LRM 37.10 detail 7 says the
  smallest precision of the modules.
- **Values.** An array variable states no sign; the size of a real, a handle or an event is
  `vpiUndefined`; a `longint`'s own format is a vector, since an integer format holds 32 bits; a
  hexadecimal or octal digit whose bits hold both x and z is an X, as `%h` prints one (LRM
  21.2.1.3); an element of a dynamic container is allocated by the run, `vpiDynamicScheme`; a
  typespec no typedef names answers no name, but a class's, which is named after the class (LRM
  37.25, 37.32). A key of a class handle or a `chandle` has no literal to be written with, and no
  operation of the language lists the entries of an associative array with a wildcard index (LRM
  7.9); every entry still answers a name that reaches it again, and iterating an associative array's
  entries lists every one it holds.
- **Callbacks.** A read-write synchronization runs in Post-NBA, since it runs "after execution of
  events for a specified time" (LRM 38.36.2). An after-delay callback of no delay asked for once
  Pre-Active has run runs in the slot's Active region, the earliest the slot still runs. An
  end-of-simulation callback runs once the final procedures and what the design asked for at its end
  have run, and not where the tool could not carry the run on. An error callback is called for each
  run-time error the run reports of the design, and a routine-error callback for each error a VPI
  routine reports at the level `vpiError` or above; neither is called again from inside a call of
  its own. A signal callback traps the signal and is called between two region passes, before the
  next region is chosen; a signal the system lets no program catch, or that reports a fault of the
  instruction raising it, is refused.

## Forbidden shapes

- **A routine defined to answer "not supported".** It fails the requirement that a missing
  capability is reported where no caller can miss it: the failure moves from the link, where the
  routine is named, to a run, where only a caller that checks `vpi_chk_error` learns of it.
- **The runtime's own copy of the standard's declarations or canonical types.** It fails holding
  every definition to the prototype the application was compiled against, which only the one header
  can.
- **A second party resolving names between a program's artifacts -- a shared library opened beside
  the program, or a resolver for the inward direction only.** It fails the requirement that both
  directions of the boundary resolve, since the standard fixes the symbol on both sides and only the
  one linker sees both.
- **A shared library input found by a search across directories.** It fails linking the program
  against the very file it was handed, which the kept program's key and an emitted project's copies
  depend on.
- **An open-array handle aliasing the actual's live storage.** It fails the value model, which lends
  no aggregate's interior across a call the compiler cannot see ([storage.md](storage.md)), and it
  loses the pointer accessors, since a value's own form is not the canonical one.
- **An open-array type in the HIR or MIR type systems.** It fails the requirement that a type is the
  type of some value: the formal's unsized shape is the type of none that crosses (LRM 35.6.1.1).
- **The context scope held by a value whose destructor gives it back.** It fails the requirement
  that both backends realize one MIR, since only one target has destructors to run at scope exit.
- **The import call chain kept on the process.** It fails a context import reached from code running
  on no thread, which every static initialization is.
- **A VPI handle that is a constructed scope's address, as `svScope` is.** It fails reaching what
  the run made no object for -- a task, a block, an instance array.
- **A variable's handle holding the address of the part it reaches.** It fails LRM 37.17 detail 27:
  an element of a dynamic array moves as the array is resized, and the handle names whatever the
  array holds there when it is used.
- **The runtime reading a part's storage by an offset it computes, or converting a foreign index by
  its own reading of a type's record.** It fails the requirement that a type's layout and each
  select's mapping are stated once, by the design's own compiled steps.
- **A body per class unrolling each data object's type into every path through it.** It fails the
  requirement that compiled code grows with the types and the data objects: `m` members nested `k`
  deep are `m^k` paths from `k*m` declarations.
- **Ending the run where a foreign read or write fails.** It fails LRM 38.2, which gives such a
  failure an error channel; foreign code's mistake would end a simulation the design did not ask to
  end.
- **Comparing every watched variable against a copy at points of the loop.** It fails the
  requirement that a callback is handed each change as it was made: it costs a pass whether or not
  anything changed, and loses a change made and undone between two passes.
- **Calling foreign code while the observers of a write are being told.** It fails the requirement
  that no code runs while the list it was reached through is walked, since foreign code may remove
  an observer or write again.
- **Calling a value-change callback on what a write says it did, without the callback's own
  comparison.** It fails calling a callback exactly where what it reads changed: a write's account
  of what it did is wrong for some position, and a foreign program would see it.
- **Accepting a callback whose occurrence never happens here.** It fails the requirement that a
  reason Lyra lacks is refused: it reads as a feature the run has and silently never calls back.
