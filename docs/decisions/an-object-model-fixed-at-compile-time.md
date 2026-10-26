# An object model fixed at compile time, and a library reached through constants

Date: 2026-09-29 Status: accepted; D4 and D5 revised and D9 added 2026-10-02, D7 revised 2026-10-17;
how an object is built (D7, D9's last clause) superseded 2026-10-22 by
[an-object-is-created-whole-then-constructed](an-object-is-created-whole-then-constructed.md); D1,
D2, D3, D5 and D9 rewritten 2026-10-25, when the layout stopped following the C++ ABI (see "What the
2026-10-25 revision found" under Why)

**D8 is reversed by
[a-design-element-publishes-its-declarations](a-design-element-publishes-its-declarations.md)**, and
with it D4's tables a name is answered from: a class a design element declares is published by that
element, so a referrer names it and calls its methods as it would any published class's, and no
class or scope carries a table of names. What D4's constant still holds is what a class of the
design hierarchy states of its instances and a scope's DPI-C export table.

## Context

A SystemVerilog class dispatches its virtual methods on the object (LRM 8.20), an interface class
may be reached along several paths and is still one type (LRM 8.26.6.3), and `$cast` asks whether an
object is of a class (LRM 8.16, 8.26.5). Separately, the library the compiler ships drives what the
program builds. It builds a scope, drives its phases, ends an object, and finds a body a name
reaches.

Both were answered through records the library composed while the program started. Each unit called
into the library to declare its classes, the library laid every lineage out into flat tables, and a
dispatch on the execution backend asked the library which body a position held. The C++ backend
answered the same questions a second time with C++ `virtual`. So one program had two dispatch
mechanisms on one backend and one on the other.

## Decision

**D1. Where each part of an object sits and which body answers each behavior are fixed when the
program is compiled, on both backends.** Every object starts with the definition of its class,
written once when the object is created and never changed, then the part the library defines, then
the members of each class of its lineage, base first. A class's definition names a table of the
bodies its objects answer each behavior with, in the order the lineage introduced them, and one
table for each interface class its objects are values of, in the order that interface class
introduced its behaviors. MIR states which body answers each behavior of each table (D2); where a
table lies, and which slot a behavior has, is decided below MIR. A call dispatching on a behavior
loads the definition, its table and the slot, and calls; a call through an interface class's handle
asks the library for the table the object's class gives that interface class, then loads the slot.
Each table is to be stated once, by a step both backends consume after the semantic lowering: an
ordinary record of entry points per class, the record of the class it extends first, a constant of
it with every entry stated, and a dispatching call stated as a read of it, so that the C++ render
translates the record, the constant and the call as it does any other, and neither backend works a
table out. This is not yet how it is built. Today the C++ backend works out each table's structure
while rendering -- a structure of function pointers, one member per behavior a class introduces,
extending the structure of the class it extends, which the host compiler lays out -- and the
execution backend lays the same tables out by its own rule in code generation, so a table's order,
member types and initializer are each settled once per backend, and the C++ output holds a
declaration no IR states. A closure's captures are laid out the same way, by the code that builds
it, in the closure value itself after what the library keeps there, as a lambda's are by the C++
compiler; a capture is then read as a member is.

**D2. An interface class is one type however many paths reach it, and is no part of an object.** It
adds no storage (LRM 8.26), so every class of an object's lineage and every interface class it is a
value of starts where the object does, and a handle is the object's address and its share of
ownership, whatever class it is held as. Which body of a class's lineage answers each behavior of
every interface class the class is a value of is stated in MIR, as the class's interface tables,
because a table indexed by an interface's behaviors is built from exactly that relation and no
consumer may recover it by name. An interface class's behavior a class answers with one it inherits
names that inherited body directly; nothing forwards to it. A call through an interface class's
handle is the same MIR call as any dispatching one, naming the behavior; that the behavior's class
is an interface class, and so its table is found among the object's interface rows rather than read
off the definition, is read below MIR, as clang has one member call and decides how it is made only
in code generation.

**D3. Whether an object is of a class is answered by the object's definition.** The library walks
the definition the object starts with: the class itself, the classes it extends through each
definition's base, and the interface classes the object's own definition lists, every one it is a
value of. `$cast` toward a class asks that, on both backends, and answers the handle it was handed
or one naming no object; nothing is converted, because every class the object is of starts where it
does.

**D4. What the library reads by name comes from one constant per class, which the declaring unit
emits.** The constant is a structure the library defines, holding the tables a name is answered
from, each entry taking the object first, and for a class of the design hierarchy its timescale. A
closure's definition is such a constant too, stating its body and the body ending its captures. A
unit names another unit's class's constant by symbol, which its signature already makes an explicit
dependency. Nothing is declared to the library at run time, and the library composes no record. The
constant is a value MIR states like any other: the class holds it, and the arrays its tables point
at, as constants whose values are ordinary expressions -- a structure composed from its members, an
array from its elements, a name, an address, a body named as code. A table is the address of its
array and a count, as a table is in C and a slice is in Rust. A body the library enters is a body of
the class that takes no receiver, stated in MIR too. So a backend translates the constant with what
it translates every expression with and writes nothing of its own; the execution backend emits it as
data, each member at the offset the library's structure gives it, which is read off the structure
itself where the compiler is built rather than restated.

**D5. The compiler does not link a C++ front end.** The only C++ a program meets is the library Lyra
ships and is built with, so every fact generated code needs about it is a fact of that library read
when Lyra is built: the size of the part every object starts with, where each member of the
structures it reads lies, and the symbols of the entries that begin and end that part. Generated
code calls those entries, and nothing of a C++ compiler's own ABI -- a virtual table, type
information, a constructor variant -- is something it must match.

**D9. Every object the library holds starts with the library's root, and the library ends and drives
it through entries its class's definition holds.** The definition names the entry ending an object
of the class and giving back its storage, the entries of each phase a scope is driven through, and
those `randomize()` asks of a class. Each of the last two kinds is a body MIR states, which takes
the object as the root and calls the body of the class on it; the entry ending an object is the
class's deleting destructor, handed the object's storage. The library's records state each entry at
its prototype, so the C++ host compiler checks every one against the body it names, and nothing
restores an entry the library calls to a type written by hand. So whoever holds an object holds it
as the root, and no class an object is of has a virtual function. The execution backend gives each
class a base object destructor, which ends the class's members and then its base's part, and a
deleting destructor, which the definition names; and a base object creation, which begins the base's
part and then the class's members, and an allocating creation, which takes the storage, runs that,
and gives the object its class. On the execution backend a `new` in another unit calls the
allocating creation, so it reads nothing of how large an object of the class is; the C++ backend
reads the class's definition from its header, as any C++ `new` does. How an object is built is
[an-object-is-created-whole-then-constructed](an-object-is-created-whole-then-constructed.md)'s.

**D6. The signature of a class a unit publishes states the class's declaration whole.** The
properties another unit may name, by name and type in the order they hold their slots, then the type
of each `local` one (LRM 8.18), placed after them; the properties of the class itself that another
unit may name (LRM 8.9), by name and type; its constructor, with the prototype a construction is
entered through (LRM 8.7), unless it is an interface class; every method it declares, with its
prototype and what a call to it is made on -- no object for a method of the class itself (LRM 8.10),
and otherwise an object, the method being not virtual, introducing a virtual method, or overriding
one, and whether it is pure (LRM 8.20, 8.21); and the interface classes its declaration names (LRM
8.26.2), in the order written. A class of another unit extending it then places its own storage and
builds its tables at compile time, and a call to one of its methods reads what it passes and awaits
off the signature. A change to a `local` property re-emits the units extending the class and no unit
that only reaches its other properties, since those keep their places ahead of it.

Nothing that follows from another class's declaration is on it. A value of a class is also a value
of every interface class the class it extends is (LRM 8.26) and of every one an interface class it
names extends, and each of those is read off the class that names it. Every layer states the class
it extends and the interface classes it names and no more, and the order an object's interface class
parts are placed in is decided where the rest of its layout is (D1).

**D7. An instance of the design hierarchy is built the way a `new` object is.** It is created and
then constructed, as
[an-object-is-created-whole-then-constructed](an-object-is-created-whole-then-constructed.md) states
for every object, which replaced this decision's account of how an object is built (2026-10-22):
that account had each constructor build its base and then take its class's tables and build its
members, so a base being constructed dispatched as itself, which LRM 8.17 NOTE 2 does not allow. The
design root is built through its unit's object entry, as an instance of another unit is. No
construction is entered through a constant, and the library allocates no object.

**D8. A name reaching a method of a class the referrer cannot name is settled to a body while the
design elaborates, whether or not the object decides which body runs.** (Reversed, as the status
above says, and written while an interface class still had a part of its own in an object, which D2
removed; kept as the reasoning it was.) A class a design element declares is a type of each instance
(LRM 6.22) and nameable only inside it (LRM 23.9), so a call on one from outside walks to the
declaring scope, asks it for the class, and asks the class for the body the method's name runs --
once, before the simulation starts. For a virtual method (LRM 8.20) that body is one the declaring
unit synthesizes, which makes the virtual call on the object it is handed, so the object still
decides. The body is entered on the part of the object the handle reaches it through, so the class
asked is the one the handle is of. Every class of a lineage starts where the object does, so a body
of an ancestor serves as it stands; an interface class's part is its own, so an interface class
answers by name every behavior an interface class it extends introduced (LRM 8.26.2), each with a
body that views the object as the introducer and makes the call -- the adjusting entry the Itanium
ABI places in a secondary table (section 2.5.2), and the supertrait entries rustc places in a
subtrait's table. Only the declaring scope can make that conversion, so a call through an interface
class's handle goes this way even to a method a class the referrer can name declares. Nothing is
looked up by name on the simulation path, and the library holds no entry answering a dispatch
position by name.

## Why

**The promise states storage whole, because nothing Lyra compiles is distributed as a binary.**
Objective-C's non-fragile ivars, Swift's resilient layout and the JVM's link-time field offsets each
load an offset at run time so a library shipped compiled can grow without its clients recompiling
(https://alwaysprocessing.blog/2023/03/12/objc-ivar-abi). Swift's library evolution document says
code using a declaration of a non-resilient module "is permitted to know all the details of how the
entity is declared" (https://github.com/swiftlang/swift/blob/main/docs/LibraryEvolution.rst). Lyra
compiles every unit of a design itself, so that load would buy a saved recompile at the price of
more work per access than the same C++ program does.

**Every method is on the signature, because a call depends on its callee's prototype.** A call
passes and awaits what the callee's formals and protocol say (LRM 13.3, 13.5). Where a caller in
another unit takes that from the front end's view of the callee, a change to the prototype changes
the caller's output while no signature changed, which is a dependency nothing records. Clang and
rustc both carry every method's signature to the consumer, in the header and in crate metadata. How
a method takes part in dispatch is stated as the front end resolved it, since what a name overrides
follows from every name the classes above declare (LRM 8.20, the `virtual` qualifier being optional
on an override). Clang stores that relation on the method where its front end resolved it
(`CXXMethodDecl::overridden_methods`) and rustc stores which trait item an impl item implements;
neither re-derives it by name downstream, and both recompute only the layout.

**The constructor is on the signature for the same reason.** The arguments a construction passes are
bound against the constructor's formals -- how many, the conversion each takes, the defaults filled
in (LRM 8.7, 13.5) -- so a unit entering another unit's constructor depends on them whether or not
its lowering names them. A constructor's formals follow the conventions of any subroutine, other
directions included (LRM 8.7, and the `output` formal in LRM 8.17's example), so nothing about a
construction makes it the one call whose callee is unstated. The JVM's `new` only allocates, and the
constructor is a method `invokespecial` names by "a symbolic reference to a method ... which gives
the name and descriptor" (JVMS SE21 6.5, `new` and `invokespecial`); a CLR `newobj` takes a token
naming "the name, class and signature of the constructor to call" (ECMA-335 III.4.21, as .NET's
`OpCodes.Newobj` reference states it); and a C++ constructor is declared in the class like any
member function.

**Only the interface classes a declaration names are stated, because what those extend is another
class's to state.** A signature listing every interface class a value is also a value of would
repeat what the signature of each interface class already says it extends, and would change when
only that other unit's source did. A referrer reads those signatures anyway, since each interface
class's table is built from its own methods. The base is handled the same way, as the one class
extended, with the chain walked by whoever needs it. Clang and rustc both state direct relations and
walk them in the layout step: clang's record layout walks a class's direct bases recursively with a
set of the virtual bases already placed (`ItaniumRecordLayoutBuilder::LayoutVirtualBases`), and
rustc's vtable layout walks direct supertraits the same way (`prepare_vtable_segments`).

**Properties another unit may name stay ahead of the `local` ones, because iteration time is the
objective.** Clang and rustc lay fields out in declaration order and accept that a private field
moves the public ones after it, so every user recompiles. Placing the nameable properties first
costs nothing at run time and keeps a unit that only reaches them unchanged when a `local` property
is added.

**Compile time, because nothing about a SystemVerilog hierarchy changes after compilation.** Every
system with dynamic binding and a fixed hierarchy assigns positions at compile time, clang in a
vtable-layout query beside its record layout, rustc per (type, trait), D for its C++ classes
("matches C++ virtual function table layout for single inheritance",
https://dlang.org/spec/cpp_interface.html). Systems that assign later -- a JVM linking a class, the
Objective-C runtime -- do so because classes arrive or change while the program runs, which
SystemVerilog does not allow.

**What the 2026-10-25 revision found.** From 2026-09-29 to 2026-10-25 this record had the layout
follow the Itanium C++ ABI: an interface class was a virtual base with a part and a table of its own
in every object, a handle carried the address of the part it was held as beside the object's
identity, `$cast` was the host's `__dynamic_cast`, and the library ended and drove an object through
C++ virtual functions. None of that is something SystemVerilog asks for. The language asks that an
interface class add no storage (LRM 8.26), that one reached along two paths be one type (LRM
8.26.6.3), and that a cast between a class and an interface class test the object (LRM 8.16,
8.26.5). What the C++ shape cost, measured on the execution backend before the change: a handle of
three words where two say which object it is; eight bytes more in every object for each interface
class its class is a value of; a unit holding only a handle of a class made again when the class
came to implement another interface class, and a unit creating an object of another unit's class
made again when that class gained a property; a forwarding method synthesized for each interface
method a class answered with an inherited one, which could not carry an `output`, `inout` or `ref`
argument; and a method every class has, called through an interface class's handle, needing two
library functions because neither MIR nor the C++ rendering could dispatch the library's hook
through one. The Itanium layout exists for C++'s multiple and virtual inheritance, where a base's
part need not start where the object does; SystemVerilog has neither, so nothing it bought was used.

**One header naming the class, because every system with single inheritance and interfaces does
it.** HotSpot embeds a class's vtable in its class metadata, read by index, and lays an interface's
methods out in an itable after it: an offset table of interface and offset pairs, then a method
table per interface (`klassVtable` and the "Format of an itable" comment,
https://github.com/openjdk/jdk/blob/master/src/hotspot/share/oops/klassVtable.hpp). CoreCLR gives
every type a vtable for its virtual methods and finds an interface method by mapping the call's
token to a slot of the object's type, caching the answer per call site and in a global cache keyed
by token and type (Book of the Runtime, "Virtual Stub Dispatch"). Go keeps one `itab` per pair of an
interface and a concrete type, holding the method pointers, found by `getitab` and cached in
`itabTable` (https://github.com/golang/go/blob/master/src/runtime/iface.go). Swift stores in a
class's metadata "a function pointer to the implementation of every method of the class in
declaration order", once per class of the lineage, and finds a protocol's witness table through the
conformance records `swift_conformsToProtocol()` scans
(https://github.com/swiftlang/swift/blob/main/docs/ABI/TypeMetadata.rst). rustc makes one vtable per
pair of a type and a trait, keyed by both (`vtable_allocation_provider`,
`rustc_middle/src/ty/vtable.rs`; its entries from `vtable_entries`,
`rustc_trait_selection/src/traits/vtable.rs`). The class's own table indexed by position, and a
table per pair of a class and an interface found through the class, is the shape every one of them
has. Lyra's conditions match theirs: single inheritance, interfaces adding no storage, and every
class known when the program is compiled, so each table is a constant rather than one built at run
time. Since every class a value is of starts where the object does, a handle is the object's address
and its share; nothing is formed when a handle is converted.

**Entries in the definition, because the library is the only reader that does not know the class.**
Whoever holds an object -- a handle's last share, a scope's owner, the constraint solver -- calls
what its class's definition names, which is what a C++ virtual table was doing under the C++ ABI's
rules, stated instead in a structure the library declares and both backends fill from MIR. rustc
reaches the same place: every trait object's table opens with the type's drop function, size and
alignment (`VtblEntry::MetadataDropInPlace`, `rustc_middle/src/ty/vtable.rs`).

**The allocating creation in the defining unit, because the size is that unit's fact.** Swift calls
a class's allocating initializer, emitted by the module defining the class, wherever its layout may
change without the caller being rebuilt, and the Itanium ABI names the same entry the complete
object allocating constructor. A `new` that allocates the size itself reads every property of the
class, so a unit creating an object of another unit's class would be made again whenever that class
gained one.

**Constants for what is read by name, because a name table is data.** A table of names and
object-first entries is filled from the class's own bodies with nothing decided. Swift's runtime
hands compiled code C-shaped metadata
(https://github.com/swiftlang/swift/blob/main/docs/ABI/TypeMetadata.rst), and Objective-C's holds
`IMP`s with the receiver first.

**The constant is an MIR value, because a backend entry writes one node and nothing else.** This
record first had MIR state only what the constant said -- which bodies, which names -- and had each
backend write the library's structures itself. The C++ backend then held a family of functions
composing those structures member by member, and a function writing the lambda that enters a method
object first, none of which any MIR node stated; the execution backend held the same structures a
second time. Stated as expressions, the constant needs nothing a backend does not already have: a
composite, a literal, an address, a cast. What MIR gained is a reference naming a body as code and a
class's own constants. A type's run-time description was already stated this way.

**It is emitted as data, because the earlier objections to initialized data no longer hold.** The
record this replaces rejected emitting the records as data on two grounds, that the storage a member
needs was a library object constructed at run time, and that a backend writing a C++ structure's
offsets would turn a wrong one into a silent wrong read. The first went when the execution backend
began laying every declaration out itself, so a definition is plain data. The second is answered by
where the offsets come from. The compiler is built against the library, so the execution backend
places each member at the `offsetof` the library's declaration has and pads to its `sizeof`, and a
structure the two read differently is not one the build can produce. It also rejected stating each
record's contents in MIR, on the ground that MIR would then hold a library structure's shape. MIR
names the structure and gives its members in the order the library declares them, which is what
naming any library type and constructing it already is; the offsets stay the backend's, read off the
library. The same record rejected flattening a lineage's table at compile time because where an
introducer's positions begin is stated by another unit; D6 makes every promise state the behaviors
its class introduces and overrides, so that count is in hand where the extending class is compiled.

**A construction is entered typed, because only a constant's own consumer needs an erased entry.**
The library entered an instance's construction through its class's definition, so every class's
constructor took one prototype and its own arguments arrived erased in an array. That served a
referrer holding nothing of a scope but its definition, which no referrer is. An instance of another
unit is built through that unit's object entry, and one of this unit through its own constructor,
which is what clang does with `new T(args)`: `operator new` for the storage, then the constructor,
called like any other.

**A virtual call by name is settled to a forwarding body, because the name is fixed by elaboration
and only the object is not.** Asking the library for a dispatch position by name at each call would
look a name up wherever the design wrote the call, which no other reference does. C++ meets the same
shape in a pointer to a virtual member function. The Itanium ABI (section 2.3) stores the table
offset plus one there, so every call through such a pointer tests which kind it holds; Microsoft's
ABI stores the address of a thunk the compiler emits to make the virtual call
(`MicrosoftCXXABI::EmitVirtualMemPtrThunk`, `clang/lib/CodeGen/MicrosoftCXXABI.cpp`), so a call
through any such pointer is one indirect call. The by-name entry is read by calls that must not
branch on what they hold, which is the condition the second answers. The synthesized body is that
thunk, the table holds an address like any other entry, and the call site treats a virtual method
exactly as a non-virtual one.

**No C++ front end, because we import no C++ we did not write.** Swift and Carbon link clang
in-process because they import arbitrary user headers whose layout only a C++ compiler knows, and
Swift still needed a forked clang entry to emit tables for bodies it defines. Lyra's foreign
boundary is C (LRM 35), so that need does not arise, and parsing headers per unit would be paid on
every compile against the iteration-time objective.

## Rejected

- **Tables composed by the library at program start, and a call asking the library for a body.**
  Answers at run time what is fixed at compile time, adds a library call to every dispatch, and left
  the execution backend unable to dispatch through an interface class at all.
- **Linking clang to lay out generated classes.** Pays per unit for a need we do not have.
- **Interface classes as ordinary bases.** Two copies of an interface reached along two paths make a
  view to it ambiguous, which LRM 8.26.6.3 rules out; the C++ backend's output failed to compile on
  exactly that until this record.
- **Interface classes as virtual bases, laid out by the Itanium C++ ABI.** This record's own answer
  until 2026-10-25; what it cost is under Why.
- **A handle carrying a table pointer for the interface class it is held as, as a `dyn` reference
  does.** It makes an interface call one load shorter, and makes a handle three words again, with a
  conversion at every assignment between a class handle and an interface class handle, for a call
  that is not on any path the measured designs spend time in.
- **A backend writing the function a table holds.** A table holds a body that takes the object as
  the root, and a method takes it as its own class, so something must take one and call the other.
  Where a backend wrote it, it would be a function MIR did not state. MIR states it instead, a body
  of the class that takes no receiver, is handed the object first, reads it as the class, and calls
  the method, and both backends translate it like any other.

## Supersedes

- [dispatch-position-is-a-lineage-coordinate](dispatch-position-is-a-lineage-coordinate.md) D3. The
  library no longer answers which body a position holds. D1 and D2 stand, so a behavior is still
  named by its introducing class and an ordinal, and the layout step is where that becomes a slot.
- [generated-behavior-boundary](generated-behavior-boundary.md). The library drives a scope through
  entries its class's definition names (D9), a constant the declaring unit emits rather than a
  program of entry points, and what is left of the program is the name tables and timescale D4
  states.
- [object-identity-is-carried-not-derived](object-identity-is-carried-not-derived.md) D5. Every
  object starts with the library's root (D9), so it starts where the object does and nothing records
  where the object was created.
- [a-unit-states-what-it-declares](a-unit-states-what-it-declares.md), in part. What a unit states
  is still stated once in MIR and translated by both backends, as data rather than as calls into the
  library. Its rejections of initialized data, of a lineage table flattened at compile time and of
  record contents stated in MIR are answered above, and its program entry builds the root through
  the root unit's object entry.
- [interface-conformance-realization](interface-conformance-realization.md) D3. Its trigger, a
  backend dispatching through an interface handle, is this.
- [inherited-member-reference](inherited-member-reference.md), where the execution backend resolves
  the pair. A member is still named by its declaring class and its slot, and the execution backend's
  layout step resolves the pair at compile time, across units too, rather than against a schema the
  library builds. Its rejection of a flattened list as the fragile base class problem rests on
  binary distribution, which Lyra does not have.
- [a-dynamic-cast-asks-the-type-or-the-object](a-dynamic-cast-asks-the-type-or-the-object.md), its
  object half, from 2026-09-29 to 2026-10-25, when D3 returned it to the library's walk of the
  object's definition that record had.
- [a-settled-access-is-ordinary-operations](a-settled-access-is-ordinary-operations.md), in part. A
  virtual behavior is answered by name with a body too (D8), so its table's second row is gone, and
  D4's "the entry is handed the most-derived object" gives way to the root every object is. That an
  erased entry takes the object rather than the handle stands.
- [structural-access-on-an-opaque-object](structural-access-on-an-opaque-object.md), D2's virtual
  behavior coordinate. A behavior reached past a signature settles to a body (D8).
- [a-referrer-calls-rather-than-navigates](a-referrer-calls-rather-than-navigates.md), D4a's
  premise. The definition a promise passes on is a constant its unit emits (D4).
- [a-published-class-is-emitted-once](a-published-class-is-emitted-once.md), its list of classes.
  The garbage-collected base, a value of a class and a scope have no virtual function (D9), so four
  classes are published that way.
- [member-slot-storage](member-slot-storage.md), the execution backend's realization. Its members
  sit in the value's own bytes at offsets fixed at compile time.
- [a-member-is-reached-at-a-derived-offset](a-member-is-reached-at-a-derived-offset.md). A member is
  still reached at an offset derived below the execution IR, but the offset is the layout step's
  constant on every path: there are no uniform slots after a kind of value, and a lineage through
  another unit's class is laid out from its promise, which states storage whole (D6), instead of
  reading a count the runtime recorded.
- [closure-value-realization](closure-value-realization.md) D2 and D4. A closure is built by an
  instruction of its own and its captures are filled by the building code (D1, D4).
- [constructing-another-units-class](constructing-another-units-class.md) D2's claim that a
  construction reads nothing about its constructor, D4, and its rejection of the constructor on the
  promise. The signature states the constructor (D6).
- [reaching-past-a-published-class](reaching-past-a-published-class.md) D1's list of what a promise
  states. The promise states storage whole (D6). Its rule that nothing inherited is restated stands.
- [entering-a-class-construction](entering-a-class-construction.md), that allocation is the
  runtime's and that the handle opens before the constructor runs (D7).
- [root-unit-elaboration](root-unit-elaboration.md), how the root is built. It is built through its
  unit's object entry (D7).
