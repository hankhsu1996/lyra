# An object model fixed at compile time, and a library reached through constants

Date: 2026-09-29 Status: accepted; D4 and D5 revised and D9 added 2026-10-02

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

**D1. Where each part of an object sits and which body each view dispatches to are fixed when the
program is compiled, on both backends.** The C++ backend states classes, and the host compiler lays
them out. The execution backend lays them out itself, below MIR, in one layout step asked by code
generation, which places the object's parts, one table of bodies for the class and one for each
interface class view of it, and the offset from each view to the object. It follows the Itanium C++
ABI's rules for the shapes SystemVerilog has -- one class base, and interface classes that hold no
storage and may be reached along several paths -- so both backends do the same work at run time. A
dispatch is a load of the table, a load of the slot, and a call. A closure's captures are laid out
the same way, by the code that builds it, in the closure value itself after what the library keeps
there, as a lambda's are by the C++ compiler; a capture is then read as a member is.

**D2. An interface class is one type however many paths reach it.** The C++ backend renders it as a
virtual base; the execution backend gives it one view per object. Which behavior of a class's
lineage answers each behavior of every interface class it reaches is stated in MIR, because a table
indexed by an interface's slots is built from exactly that relation and no consumer may recover it
by name.

**D3. Whether an object is of a class is answered by the object's type descriptor.** Each class has
a descriptor in the format Itanium 2.9.4 defines, and `$cast` calls the host's `__dynamic_cast`
(2.9.7) on the execution backend, which is what the host compiler emits for the C++ backend's
`dynamic_cast`. The cast forms the view and the view's presence is the answer; nothing asks twice.

**D4. What the library reads by name comes from one constant per class, which the declaring unit
emits.** The constant is a structure the library defines, holding the tables a name is answered
from, each entry taking the object first, and for a class of the design hierarchy its timescale. A
closure's definition is such a constant too, stating its body, the size of a value holding its
captures, and the body ending them. A unit names another unit's class's constant by symbol, which
its signature already makes an explicit dependency. Nothing is declared to the library at run time,
and the library composes no record. The constant is a value MIR states like any other: the class
holds it, and the arrays its tables point at, as constants whose values are ordinary expressions --
a structure composed from its members, an array from its elements, a name, an address, a body named
as code. A table is the address of its array and a count, as a table is in C and a slice is in Rust.
A body the library enters is a body of the class that takes no receiver, stated in MIR too. So a
backend translates the constant with what it translates every expression with and writes nothing of
its own; the execution backend emits it as data, each member at the offset the library's structure
gives it, which is read off the structure itself where the compiler is built rather than restated.

**D5. The compiler does not link a C++ front end.** The only C++ a program meets is the library Lyra
ships and is built with, so every fact generated code needs about it is a fact of that library read
when Lyra is built: a class's size, the order it declares its virtual functions in, and the symbols
of its type information, its constructor, its destructor and its own virtual bodies. Generated code
calls those symbols as a class compiled by clang calls its base's, and the library offers no entry
standing in for any of them.

**D9. Every object the library holds is of a class derived from the library's root, and the library
ends and drives it through C++ virtual functions.** The root has a virtual destructor, so whoever
holds an object holds it as the root and `delete` ends the class it was built as. The scope adds one
virtual function per phase the library drives it through, private and called from the scope's own
public entry, and named with `sv_`, which no name of the source can take once it reaches C++. The
C++ backend overrides them as C++ does. The execution backend produces what clang produces for that
output by the Itanium rules: the root is the primary base, so its table pointer is at every object's
start; each class has a base object, complete object and deleting destructor (D2, D1, D0), the first
ending the class's members and then its base's part; its table holds the destructor first, then the
library's virtual functions, then the source's; and a constructor builds its base, takes its class's
table and builds its members, in that order, before its body runs.

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

**D7. An instance of the design hierarchy is built the way a `new` object is.** On the execution
backend the host's `operator new` is asked for storage of the size of a complete object of the
class, and the class's constructor is then entered on it directly, with its own typed arguments. The
constructor's chain builds the part the library defines first -- the part every scope shares, from
the arguments the class states for its base, or the part every object starts with -- through the
library's constructor of that part, as the C++ backend's base initializer does. Each constructor
then runs its class's prologue: the value takes the class's tables, including those of the interface
class parts, and the class's members come into existence. A base being constructed thus dispatches
as itself, as C++ gives it through construction virtual tables (Itanium 2.6). A `new` object is
handed to the handle owning it only once its constructor has run. The design root is built through
its unit's object entry, as an instance of another unit is. No construction is entered through a
constant, and the library allocates no object.

**D8. A name reaching a method of a class the referrer cannot name is settled to a body while the
design elaborates, whether or not the object decides which body runs.** A class a design element
declares is a type of each instance (LRM 6.22) and nameable only inside it (LRM 23.9), so a call on
one from outside walks to the declaring scope, asks it for the class, and asks the class for the
body the method's name runs -- once, before the simulation starts. For a virtual method (LRM 8.20)
that body is one the declaring unit synthesizes, which makes the virtual call on the object it is
handed, so the object still decides. The body is entered on the part of the object the handle
reaches it through, so the class asked is the one the handle is of. Every class of a lineage starts
where the object does, so a body of an ancestor serves as it stands; an interface class's part is
its own, so an interface class answers by name every behavior an interface class it extends
introduced (LRM 8.26.2), each with a body that views the object as the introducer and makes the call
-- the adjusting entry the Itanium ABI places in a secondary table (section 2.5.2), and the
supertrait entries rustc places in a subtrait's table. Only the declaring scope can make that
conversion, so a call through an interface class's handle goes this way even to a method a class the
referrer can name declares. Nothing is looked up by name on the simulation path, and the library
holds no entry answering a dispatch position by name.

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
construction makes it the one call whose callee is unstated. A JVM `new` names `<init>` by a method
reference carrying its descriptor, a CLR `newobj` takes a method token, and a C++ constructor is
declared in the class like any member function; the three were recalled for this entry rather than
opened.

**Only the interface classes a declaration names are stated, because what those extend is another
class's to state.** A signature listing every interface class a value is also a value of would
repeat what the signature of each interface class already says it extends, and would change when
only that other unit's source did. A referrer reads those signatures anyway, since each interface
class's table is built from its own methods. The base is handled the same way, as the one class
extended, with the chain walked by whoever needs it. Clang and rustc both state direct relations and
walk them in the layout step: clang's record layout walks a class's direct bases recursively with a
set of the virtual bases already placed (`ItaniumRecordLayoutBuilder::LayoutVirtualBases`), and
rustc's vtable layout walks direct supertraits the same way (`prepare_vtable_segments`). The C++
backend reads only the direct list, which it renders as virtual bases.

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

**The Itanium rules, because they are a published platform ABI and the host's `__dynamic_cast` reads
them.** An earlier record rejected having the execution backend encode a C++ compiler's vtable
layout, type descriptors and base offsets, as a foreign responsibility specific to a platform and a
compiler version. What it rejected was generated code posing as the library's own C++ classes --
defining a class the library declares -- which stays ruled out. The tables of the classes Lyra
generates follow the Itanium C++ ABI (sections 2.4, 2.5, 2.9), which every compiler targeting the
platform implements identically and no compiler version changes, and following it is what lets
`$cast` be the host's own `__dynamic_cast` rather than a second implementation of one.

**Virtual functions of the library, because the program written in C++ by hand has them.** Whoever
holds an object holds it as the root and ends it with `delete`; the scope's phases are member
functions a scope's class overrides. Before D9 the same two things were a structure of function
pointers each class emitted -- a release entry and three phase entries -- which is a virtual table
under other names, and which a reader of the C++ could only recognize by being told. The reason
given for it was that a C++ virtual of the library is filled only by a producer reproducing the
library compiler's layout of the library's class: its slot order, primary base, data size and base
construction. Each of those is either an Itanium rule this record already follows for every
generated class -- slot order, primary base -- or a fact of the library read when Lyra is built --
its size, and the symbol of its base object constructor. rustc reaches the same place from the other
side: every trait object's table opens with the type's drop function, size and alignment
(`VtblEntry::MetadataDropInPlace`, `rustc_middle/src/ty/vtable.rs`), and that drop function is the
virtual destructor.

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
ABI stores the address of a thunk the compiler emits to make the virtual call (recalled rather than
read here), so a call through any such pointer is one indirect call. The by-name entry is read by
calls that must not branch on what they hold, which is the condition the second answers. The
synthesized body is that thunk, the table holds an address like any other entry, and the call site
treats a virtual method exactly as a non-virtual one.

**No C++ front end, because we import no C++ we did not write.** Swift and Carbon link clang
in-process because they import arbitrary user headers whose layout only a C++ compiler knows, and
Swift still needed a forked clang entry to emit tables for bodies it defines. Lyra's foreign
boundary is C (LRM 35), so that need does not arise, and parsing headers per unit would be paid on
every compile against the iteration-time objective.

## Rejected

- **Tables composed by the library at program start, and a call asking the library for a body.**
  Answers at run time what is fixed at compile time, adds a library call to every dispatch, and left
  the execution backend unable to dispatch through an interface class at all.
- **The library ending and driving an object through function pointers its class's constant holds.**
  This record's own first answer, replaced by D9: it is the virtual destructor and three virtual
  functions under names of their own, and the objection that led to it is answered above.
- **Linking clang to lay out generated classes.** Pays per unit for a need we do not have.
- **Interface classes as ordinary bases.** Two copies of an interface reached along two paths make a
  view to it ambiguous, which LRM 8.26.6.3 rules out; the C++ backend's output failed to compile on
  exactly that until this change.
- **Rendering every C++ method as a static function taking the object first, with a `virtual` member
  forwarding to it.** It would let the C++ backend hand the library a method's own address, but D1
  has that backend dispatch through the host compiler's `virtual`, so every virtual method would
  gain a forwarder instead of every entry the library holds -- the same function, written for a
  different table. Where the library holds a method, MIR states a body of the class that takes no
  receiver: it is handed the object first, reads it as the class, and calls the method. Both
  backends translate that body like any other and hand the library its address, so no backend writes
  a function MIR did not state.

## Supersedes

- [dispatch-position-is-a-lineage-coordinate](dispatch-position-is-a-lineage-coordinate.md) D3. The
  library no longer answers which body a position holds. D1 and D2 stand, so a behavior is still
  named by its introducing class and an ordinal, and the layout step is where that becomes a slot.
- [generated-behavior-boundary](generated-behavior-boundary.md). The library drives a scope through
  the C++ virtual functions of its class (D9), not through a program of entry points, and what is
  left of the program is the name tables and timescale D4 states.
- [object-identity-is-carried-not-derived](object-identity-is-carried-not-derived.md) D5. The root
  is the primary base of every object (D9), so it starts where the object does and nothing records
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
  object half. That half is answered by the descriptor through the view the cast forms.
- [a-settled-access-is-ordinary-operations](a-settled-access-is-ordinary-operations.md), in part. A
  virtual behavior is answered by name with a body too (D8), so its table's second row is gone, and
  D4's "the entry is handed the most-derived object" gives way to the part the handle reaches. That
  an erased entry takes the object rather than the handle stands.
- [structural-access-on-an-opaque-object](structural-access-on-an-opaque-object.md), D2's virtual
  behavior coordinate. A behavior reached past a signature settles to a body (D8).
- [a-referrer-calls-rather-than-navigates](a-referrer-calls-rather-than-navigates.md), D4a's
  premise. The definition a promise passes on is a constant its unit emits (D4).
- [a-published-class-is-emitted-once](a-published-class-is-emitted-once.md), its list of classes. A
  value of a class is the generated class itself, so the library publishes no class for it.
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
