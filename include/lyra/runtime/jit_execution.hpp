#pragma once

#include <cstdint>

// The execution-strategy-neutral ABI the generated module calls. Every runtime
// value crosses as an opaque pointer; the runtime owns its type and lifetime.
//
// A `bool` is never one of those values. It is a machine predicate -- a
// condition read off a value, a question about the running execution, a
// constant of the program the entry is told rather than shown -- so it carries
// no width and no unknown state. Every other answer is a handle or nothing at
// all.
//
// Definitions wrap the runtime; a host resolves these symbols when it loads a
// generated module (JIT-compiled, AOT-linked, or interpreted).
extern "C" {

struct LyraSpan {
  void* data;
  std::uint64_t count;
};

// A code address whose prototype is erased, which is how a body the runtime
// looks up crosses back: one lookup serves bodies of every signature, and the
// asking code restores the exact type the body was generated with. It stays a
// function pointer rather than becoming a data pointer, because converting
// between the two is not something the language guarantees.
using LyraMethodEntry = void (*)();

auto lyra_rt_current_runtime() -> void*;
auto lyra_rt_files(void* runtime) -> void*;
auto lyra_rt_time_format(void* runtime) -> const void*;

// Writes the `$timeformat` state a formatted time is rendered against, which is
// one setting the whole design shares rather than a per-scope one (LRM 20.4.3).
// The two powers and the minimum width cross as opaque packed values and the
// suffix as an opaque string, like every scalar. Spelling the arguments and
// omitting them are different requests -- the second restores the defaults
// rather than passing them -- so each is its own entry.
void lyra_rt_set_time_format(
    void* runtime, const void* units_power, const void* precision,
    const void* suffix, const void* min_width);
void lyra_rt_reset_time_format(void* runtime);

// The file operations, reached on the broker the runtime hands out rather than
// on the runtime itself (LRM 21.3). Every descriptor, byte count and position
// crosses as an opaque packed value and every name and mode as an opaque
// string, like every scalar, so no host file handle crosses the boundary. Where
// the source may spell an argument or leave it out -- a mode on open, a
// descriptor on flush -- each form is its own entry, because the two are
// different requests rather than one carrying a default.
auto lyra_rt_file_open(void* files, const void* name) -> void*;
auto lyra_rt_file_open_mode(void* files, const void* name, const void* mode)
    -> void*;
void lyra_rt_file_close(void* files, const void* descriptor);
auto lyra_rt_file_getc(void* files, const void* fd) -> void*;
auto lyra_rt_file_ungetc(void* files, const void* c, const void* fd) -> void*;
// A read that answers through an argument the call names completes with how
// many bytes it read and the destination those bytes filled (LRM 21.3.4.2,
// 21.3.4.4, 21.3.7). A binary read is handed the destination as well, because
// its shape decides how much is read and what the file does not reach keeps
// what it held; reading into a packed variable and reading into a memory are
// two requests, and a memory's bounds and window reach the second as operands
// of their own.
auto lyra_rt_file_gets(void* files, const void* fd) -> void*;
auto lyra_rt_file_error(void* files, const void* fd) -> void*;
auto lyra_rt_file_read(void* files, const void* dest, const void* fd) -> void*;
auto lyra_rt_file_read_memory(
    void* files, const void* dest, const void* fd, const void* left,
    const void* right, const void* start, const void* count) -> void*;
auto lyra_rt_file_seek(
    void* files, const void* fd, const void* offset, const void* operation)
    -> void*;
auto lyra_rt_file_rewind(void* files, const void* fd) -> void*;
auto lyra_rt_file_tell(void* files, const void* fd) -> void*;
auto lyra_rt_file_eof(void* files, const void* fd) -> void*;
void lyra_rt_file_flush(void* files, const void* descriptor);
void lyra_rt_file_flush_all(void* files);

// The bytes a scan may read without consuming them, and the commit of how many
// it used (LRM 21.3.4.3). A scan parses out of what it can see and only then
// says how far it got, so looking and consuming are two operations rather than
// one read that has to guess the length first.
auto lyra_rt_peek_buffered(void* files, const void* fd) -> void*;
void lyra_rt_advance_fd(void* files, const void* fd, const void* count);

// The joint cancel state of the channels a descriptor names (LRM 21.3.2), as a
// transient runtime value owned by the current call scope. A deferred write
// snapshots it so the write short-circuits if any of those channels is closed
// before the region that performs it runs.
auto lyra_rt_cancellation_for(void* files, const void* descriptor) -> void*;

// Whether any channel that cancel state covers has been closed since it was
// taken (LRM 21.3.2), as an opaque packed value like every scalar.
auto lyra_rt_is_cancelled(const void* cancellation) -> void*;

auto lyra_rt_string_make(void* cstr) -> void*;
auto lyra_rt_make_print_literal_item(void* string_value) -> void*;
auto lyra_rt_format(LyraSpan items, const void* time_format) -> void*;
// A packed constant crosses as its own word planes so that no part of its value
// is lost at the boundary: the value plane holds every word of the constant,
// and the unknown plane the X / Z mask a 4-state constant carries (empty when
// it carries none, and always empty for a 2-state one). It also carries its
// full dimension stack (a flat `{left, right}` pair array of `dims_count`
// ranges) so a multi-dim packed value keeps its shape into element / slice
// access. Whether the planes span the width those dimensions describe is
// checked here, where the width is a concrete size.
void lyra_rt_writeln(void* files, void* descriptor, void* text);
void lyra_rt_write(void* files, void* descriptor, void* text);

// The severity-fixed diagnostic channel (LRM 20.10). The dispatcher is reached
// from the runtime once, then emitted to; `origin` locates the call site and
// keys its per-site rate limit, and `text` is already formatted. One entry per
// severity, so the generated module names the severity it means and no severity
// tag crosses the boundary.
auto lyra_rt_diagnostic(void* runtime) -> void*;
void lyra_rt_emit_info(void* dispatcher, const void* origin, const void* text);
void lyra_rt_emit_warning(
    void* dispatcher, const void* origin, const void* text);
void lyra_rt_emit_error(void* dispatcher, const void* origin, const void* text);
void lyra_rt_emit_fatal(void* dispatcher, const void* origin, const void* text);

// LRM 16.3 immediate cover result: one evaluation of the cover statement at
// `site`, and whether it succeeded. Reached on the runtime rather than through
// a broker, because a coverage goal has one verb.
void lyra_rt_record_coverage(void* runtime, const void* site, bool succeeded);

// Makes an execution the engine can schedule out of a generated body's own
// frame, which is built with its arguments in place and stopped before its
// first statement. The runtime owns the coroutine the engine schedules and
// drives the generated one through its handle; the generated body never owns
// the scheduler's coroutine, and every stretch of it -- the first included --
// runs under that driver.
//
// The two differ only in how long the environment the body reads outlives it,
// never in what construct it came from. A receiver is borrowed: it outlives
// every execution reading it, so the frame already carries everything and
// nothing else crosses. A closure is taken, supplying both the entry and the
// captures, because the body runs after the stretch that built them has
// returned (LRM 9.3.2).
auto lyra_rt_enter_coroutine_borrowed_environment(void* frame) -> void*;
auto lyra_rt_enter_coroutine_owned_environment(void* closure) -> void*;

// Calling a task (LRM 13.3, where the call is also named a task enable).
// `await_coroutine` gives the calling thread to `activation` and runs it there,
// so it executes in the caller's process (LRM 9.5) rather than as one of its
// own, and answers whether the caller must park -- which a task that consumed
// no time makes unnecessary. `release_coroutine` takes the thread back, ends
// that activation, and raises into the caller a run-time error the call was
// left by, since the call is one statement of the calling thread.
//
// Neither names the activation once it is handed over: a thread is inside one
// called activation at a time, so the runtime knows which without being told,
// and no scheduling identity reaches generated code.
auto lyra_rt_await_coroutine(void* runtime, void* activation) -> bool;
void lyra_rt_release_coroutine(void* runtime);

void lyra_rt_register_initial(void* self, void* unit_instance, void* coroutine);
void lyra_rt_register_final(void* self, void* unit_instance, void* coroutine);

// LRM 9.3.2 Table 9-1. Each takes the branches one `fork` spawned, in source
// order, and hands them to the engine, which does not run any of them until the
// spawning process blocks or terminates. `spawn_all` is `join_none`, whose
// parent never waits and so answers nothing; the other two park the parent
// unless the fork spawned no branch at all.
void lyra_rt_spawn_all(void* runtime, LyraSpan branches);
auto lyra_rt_fork_wait_all(void* runtime, LyraSpan branches) -> bool;
auto lyra_rt_fork_wait_first(void* runtime, LyraSpan branches) -> bool;

// LRM 9.6.1 `wait fork` and 9.6.3 `disable fork`. Both read the executing
// process, so neither names the children it reaches. `wait fork` parks the
// caller unless every immediate child has already terminated; `disable fork`
// never blocks.
auto lyra_rt_wait_fork(void* runtime) -> bool;
void lyra_rt_disable_fork(void* runtime);

// LRM 9.7 process control. The receiver is a handle of the managed-reference
// domain naming a process node; `self` builds one for the calling process,
// which the engine already owns, so nothing is constructed here. `await` is the
// one that blocks, and it answers the way every registration does -- whether
// the caller must park at all -- since a target that has already terminated
// leaves nothing to wait for.
auto lyra_rt_process_self(void* runtime) -> void*;
auto lyra_rt_process_status(const void* self) -> void*;
void lyra_rt_process_kill(const void* self, void* runtime);
auto lyra_rt_process_await(const void* self, void* runtime) -> bool;
void lyra_rt_process_suspend(const void* self, void* runtime);
void lyra_rt_process_resume(const void* self, void* runtime);

// Builds a callable the runtime runs later: `definition` is an opaque
// cross-artifact reference naming both the body and the storage its captures
// need, and `captures` supplies one handle per capture in declaration order,
// each taken into that storage as the schema says -- a pointer held, a value
// copied. The value is transient, owned by the current call scope until
// something that outlives the stretch takes it: a region a deferred effect is
// submitted to, the coroutine a spawned branch is entered as, or the array
// method that runs a per-element body over its receiver.
auto lyra_rt_closure_make(const void* definition, LyraSpan captures) -> void*;

// Brings an object into existence on the managed heap (LRM 8.3):
// `definition` is an opaque cross-artifact reference naming the storage its
// properties need. The object's properties hold their storage's default until
// the construction the program asked for runs on it, which is the asking
// code's own to enter. The handle answered is a reference to the object,
// transient like every value the boundary hands back and owned by the current
// call scope until a store takes a copy of it.
auto lyra_rt_object_make(const void* definition) -> void*;

// The object a class handle refers to (LRM 8.3). Which object that is, is a
// fact the handle holds rather than is, so reaching it is an operation; a
// handle referring to no object fails the run here rather than further in.
auto lyra_rt_object_deref(void* handle) -> void*;

// The address of a property's storage on an object, named by the class that
// declares the property and the slot that class gave it. A class carries what
// its bases declare before what it declares itself, so a property keeps one
// slot in the class that declares it and in every class extending that one, and
// where that class's own properties begin is a fact of the class rather than of
// the access -- which is why the access states a pair and this side adds.
auto lyra_rt_object_member_addr(
    void* object, const void* declared_by, std::uint32_t slot) -> void*;

// The body an object's class answers one behavior with (LRM 8.20), the code
// axis of the coordinate rule above and named the same way: the class that
// introduced the behavior, and which of that class's introductions it is. What
// class an object is, is a fact only this side holds, while entering a body
// with the right arguments is only the asking code's to do -- so this answers
// with the address and calls nothing.
auto lyra_rt_object_method(
    void* object, const void* introduced_by, std::uint32_t ordinal)
    -> LyraMethodEntry;

// Where a name lands on a class, for a referrer with no name for that class and
// so no way to count a position out of it. Both run while a reference to such a
// class resolves and neither is reached from the simulation path; each answers
// with a coordinate the two entries below then apply, once per access, with no
// name in hand.
auto lyra_rt_class_find_property(const void* definition, const void* name)
    -> const void*;
auto lyra_rt_class_find_behavior(const void* definition, const void* name)
    -> const void*;

// The same two answers as the pair above, with the coordinate arriving whole
// instead of in parts. What the access states is the only difference: one reads
// the pair off a declaration it can name, the other reads it out of a value.
auto lyra_rt_object_member_addr_at(void* object, const void* coordinate)
    -> void*;
auto lyra_rt_object_method_at(void* object, const void* coordinate)
    -> LyraMethodEntry;

// The handle one capture crosses back to the body as, by declaration index. A
// captured pointer answers the pointer it holds; a captured value answers the
// storage the closure owns, which outlives every read of it. A body reaches its
// captures the same way whatever it is called with, so this is one entry for
// every body.
auto lyra_rt_closure_capture(void* self, std::uint32_t index) -> void*;

// Hands a callable to the region that will run it (LRM 4.4): the write a
// non-blocking assignment defers, the print a `$strobe` postpones, and the
// report a deferred assertion leaves for the observed region. Each takes
// ownership of the closure, which is what lets the closure outlive the stretch
// that built it.
void lyra_rt_submit_nba(void* runtime, void* closure);
void lyra_rt_submit_postponed(void* runtime, void* closure);
void lyra_rt_submit_observed(void* runtime, void* closure);
void lyra_rt_submit_violation_report(void* runtime, void* closure);
void lyra_rt_submit_deferred_observed(void* runtime, void* closure);
void lyra_rt_submit_deferred_final(void* runtime, void* closure);

// The NBA commit of an effect carrying a delay control (LRM 9.4.5): the region
// is the same, the slot is the one `duration` steps of the scope's time unit
// (`unit_power`) away, read and rounded exactly as a delay control's amount is.
void lyra_rt_submit_nba_after(
    void* runtime, const void* duration, const void* unit_power,
    const void* precision_power, void* closure);
void lyra_rt_submit_nba_after_real(
    void* runtime, const void* duration, const void* unit_power,
    const void* precision_power, void* closure);

// The NBA commit of an effect carrying an event control (LRM 9.4.5, 15.5.1),
// which cannot name its slot where the statement is reached. What crosses is
// the execution that waits for the event and then makes the commit in whichever
// slot it landed in: a handle to a coroutine the building stretch owns, taken
// the way a fork branch is. It runs apart from every lineage, being an update
// the standard makes no process of.
void lyra_rt_run_detached(void* runtime, void* carrier);

// The region that update is due in, reached by the carrier once the event has
// named the slot (LRM 4.4.2.4). Which execution suspends is the running one,
// read from the runtime, so nothing about it crosses; the answer is the park
// flag every registration returns, and this one always parks.
auto lyra_rt_resume_in_nba_region(void* runtime) -> bool;

// Registers the running process to wake after `duration` steps of its scope's
// time unit (`unit_power`), the registration a delay's suspend edge is preceded
// by (LRM 9.4.1). The runtime rounds that amount to the scope's precision
// (`precision_power`) and scales it to the engine's global tick; a zero wait
// re-enqueues on the current slot's inactive region. The counts cross as opaque
// packed values, like every scalar. The wakeup source is the running process
// itself, read from the runtime; no token crosses the boundary. A delay always
// parks.
auto lyra_rt_delay(
    void* runtime, const void* duration, const void* unit_power,
    const void* precision_power) -> bool;

// The same for a delay the design wrote as a real expression, which can name a
// fraction of a time unit and is rounded to the precision (LRM 3.14.1).
auto lyra_rt_delay_real(
    void* runtime, const void* duration, const void* unit_power,
    const void* precision_power) -> bool;

// Builds one leaf of a wait: the place it watches, what decides whether what
// happens there is an event -- which the leaves watching for one event share --
// and which bits of that place's packed encoding it reads, as a
// `(lsb_bit_offset, bit_width)` pair, a width of zero being the whole of it and
// what a named event's leaf carries. The scalars cross as opaque packed values,
// like every scalar. The leaf is a transient runtime value owned by the current
// call scope.
auto lyra_rt_make_trigger(
    void* observable, const void* observation, const void* lsb_bit_offset,
    const void* bit_width) -> void*;

// What decides whether reaching a wait is an event for it (LRM 9.4.2). Two
// halves, and one entry per combination of them, so the call states which form
// it is building. A watched half is a closure answering what the event
// expression is worth now together with the edge specifier written on it,
// crossing as an opaque packed value like every scalar, and it is armed here
// with what the expression is worth at this moment. A qualifying half is an
// `iff` condition, answering as a one-bit value already reduced to LRM 12.4
// truth (LRM 9.4.2.3). Watching nothing is what an implicit sensitivity carries
// (LRM 9.2.2.2.1) and what an unqualified named-event wait carries, the trigger
// there being the event itself (LRM 15.5.1). Like a trigger these are
// transient, and the waits built from them hold them for as long as they last.
auto lyra_rt_observation_on_reaching() -> void*;
auto lyra_rt_observation_of_value(void* expression, const void* edge) -> void*;
auto lyra_rt_observation_of_value_qualified(
    void* expression, const void* edge, void* condition) -> void*;
auto lyra_rt_observation_qualified(void* condition) -> void*;

// Registers the running process to wake when what happens at one of `triggers`
// is an event for the wait, the registration such a wait's suspend edge is
// preceded by (LRM 9.4.2 / 9.4.2.2 / 9.4.3 / 15.5.2). An empty span means
// "never wake up". The wakeup source is the running process itself, read from
// the runtime; no token crosses the boundary. Such a wait always parks.
auto lyra_rt_wait_any(void* runtime, LyraSpan triggers) -> bool;

// A named event (LRM 15.5). Triggering records the instant and ends the wait of
// every process the trigger is an event for; waiting for one is an ordinary
// wait naming the event, since the event is a place a wait registers on like
// any other. `triggered` answers whether the most recent trigger happened in
// this time step, which is a comparison of instants rather than a state the
// event clears.
void lyra_rt_trigger(void* event, void* runtime);
auto lyra_rt_triggered(const void* event, void* runtime) -> void*;

// LRM 9.6.2 `disable`. A target crosses as its address, and a control effect as
// the target it names, since that is all one carries.
//
// The two brackets record on the running process which targets its execution is
// inside, and the generation each held on entry; `lyra_rt_disable` advances the
// named target's generation and wakes the executions blocked inside it, and
// leaves who lands where to each of them. The two queries answer which target
// this execution is inside has been disabled since it entered, and whether one
// has -- each computed by comparing generations, so nothing is stored and
// nothing has to be cleared. A body asks them where it regains control, because
// a simulated process cannot be made to run code partway through a statement.
// `lyra_rt_settle_cancelled` reports that an effect left the body with no
// region of it claiming the effect, so the activation settles cancelled (LRM
// 9.7 KILLED) rather than completing normally.
void lyra_rt_enter_target(void* runtime, void* target);
void lyra_rt_leave_target(void* runtime, void* target);
void lyra_rt_disable(void* target, void* runtime);
auto lyra_rt_effect_names_target(void* effect, void* target) -> void*;
auto lyra_rt_invalidated_target(void* runtime) -> void*;
auto lyra_rt_has_invalidated_target(void* runtime) -> bool;
void lyra_rt_settle_cancelled(void* effect);

// Reads the current simulation time, scaled to the time unit of the design
// element the call sits in (LRM 20.3). That unit is the caller's property
// rather than the runtime's, so its power of ten crosses as an opaque packed
// value, like every scalar, and so do the first two answers; the third is an
// opaque real, keeping whatever fraction of a unit the instant falls on.
auto lyra_rt_sim_time(void* runtime, const void* unit_power) -> void*;
auto lyra_rt_stime(void* runtime, const void* unit_power) -> void*;
auto lyra_rt_realtime(void* runtime, const void* unit_power) -> void*;

// Records a request to tear the simulation down once the current time slot
// completes, and prints what the level selects about it (LRM 20.2, Table 20-1).
// Each arranges no resumption at all, so each parks, and the recorded request
// is what keeps the process from ever being dispatched again. The origin
// crosses as an opaque string value and the level as an opaque packed value,
// like every scalar.
auto lyra_rt_finish(void* runtime, const void* origin, const void* level)
    -> bool;
auto lyra_rt_stop(void* runtime, const void* origin, const void* level) -> bool;

// Runs a command line through the host's command processor and yields what it
// answered; the null form runs nothing and yields whether a command processor
// exists at all (LRM 20.17.1). The command crosses as an opaque string value
// and the answer as an opaque packed value, like every scalar.
auto lyra_rt_run_host_command(void* runtime, const void* command) -> void*;
auto lyra_rt_run_null_host_command() -> void*;

// Whether the simulation's own arguments carry a plusarg with the given prefix
// (LRM 21.6). Those arguments are the runtime's, so only the prefix crosses, as
// an opaque string; the answer is an opaque packed value, like every scalar.
auto lyra_rt_test_plusargs(void* runtime, const void* user_string) -> void*;

// The value a plusarg carries, converted as the user string's format specifier
// asks (LRM 21.6). It completes with whether one matched and the value the
// destination now holds; the destination crosses in because a miss leaves it as
// it was and its size decides how a match is fitted, and the entry is named by
// the representation that destination takes.
auto lyra_rt_packed_value_plusargs(
    void* runtime, const void* user_string, const void* destination) -> void*;
auto lyra_rt_string_value_plusargs(
    void* runtime, const void* user_string, const void* destination) -> void*;

// Draws from the calling process's generator (LRM 18.13.1 -- 18.13.2). The
// generator is the running process's, read from the runtime, so none crosses
// the boundary; the seed and the two bounds cross as opaque packed values, as
// every scalar does, and so does the result.
auto lyra_rt_urandom(void* runtime) -> void*;
auto lyra_rt_urandom_seeded(void* runtime, const void* seed) -> void*;
auto lyra_rt_urandom_range(
    void* runtime, const void* maxval, const void* minval) -> void*;

// `$random` with no seed (LRM 20.14.1): the same process draw, read signed.
auto lyra_rt_random(void* runtime) -> void*;

// Draws by the algorithm LRM Annex N states (LRM 20.14.2). The seed is the
// whole state, so no runtime crosses the boundary; each answers with a product
// of the value drawn and the seed that draw advanced, which the caller stores
// back into the design's own seed variable.
auto lyra_rt_dist_uniform(const void* seed, const void* start, const void* end)
    -> void*;
auto lyra_rt_dist_normal(
    const void* seed, const void* mean, const void* standard_deviation)
    -> void*;
auto lyra_rt_dist_exponential(const void* seed, const void* mean) -> void*;
auto lyra_rt_dist_poisson(const void* seed, const void* mean) -> void*;
auto lyra_rt_dist_chi_square(const void* seed, const void* degrees_of_freedom)
    -> void*;
auto lyra_rt_dist_t(const void* seed, const void* degrees_of_freedom) -> void*;
auto lyra_rt_dist_erlang(const void* seed, const void* stages, const void* mean)
    -> void*;

// Builds a scope's structural identity from its base label and per-dimension
// indices (a span of 32-bit index values, empty for a scalar). The segment is
// a transient runtime value owned by the current call scope.
auto lyra_rt_make_segment(void* label, LyraSpan indices) -> void*;

// Allocates a generic instance of `definition`, runs its construct entry to
// build its subtree, and returns the owning handle to the caller, which hands
// it on to be attached. `definition` is an opaque cross-unit reference the
// generated code never inspects.
auto lyra_rt_make_scope(const void* definition, void* parent, void* segment)
    -> void*;

// The scope's hierarchical name (LRM 21.2.1.5; the `%m` source), as a transient
// string owned by the current call scope.
auto lyra_rt_hierarchical_path(void* self) -> void*;

// The scope one step out. A name written in a generate block and declared in
// the module around it is reached by climbing to that scope and reading the
// member there, which the referring artifact can do directly because it owns
// the enclosing scope's layout.
auto lyra_rt_parent(void* self) -> void*;

// Attaches a freshly built child to its parent, transferring ownership into the
// runtime tree; returns the child as a borrowed scope handle.
auto lyra_rt_add_owned_child(void* parent, void* child) -> void*;

// Walks the scope tree a hierarchical reference names (LRM 23.6 / 23.8): the
// nearest enclosing child a name matches, then a descent by name from there. A
// name crosses as a plain C string, since it is fixed where the reference is
// compiled, and its per-axis indices as a span of machine integers, since one
// name may stand for an array of instances. A step matching nothing fails the
// run, naming the scope and the name.
auto lyra_rt_resolve_visible_child(
    void* self, const void* head_name, LyraSpan head_indices) -> void*;
auto lyra_rt_find_child(void* self, const void* name, LyraSpan indices)
    -> void*;

// The address of a generic instance's member storage, by its position in the
// storage that instance owns.
auto lyra_rt_member_addr(void* self, std::uint32_t index) -> void*;

// The sequence of handles a declaration standing for several objects builds,
// in the order its coordinates count, and the handle at a position in one. A
// sequence is built once where its owner is built and held by address for the
// rest of the run, which is what lets a dimension of a multidimensional
// declaration be an ordinary handle in the dimension above it.
auto lyra_rt_sequence_make(LyraSpan handles) -> const void*;
auto lyra_rt_sequence_element(const void* sequence, std::int64_t index)
    -> void*;

// Publishes a member cell under its source-level name for by-name navigation,
// and reads one back. The read answers an untyped address because the reader is
// the artifact a hierarchical reference is written in, which does not know the
// layout of the body the name lives in (LRM 23.6). Both names cross as a plain
// C string, since a source-level name is fixed at compile time.
void lyra_rt_register_signal(void* self, const void* name, void* cell);
auto lyra_rt_find_signal(void* self, const void* name) -> void*;

// Reads back the entry a scope answers a subroutine name with (LRM 23.8.1).
// It answers a code address rather than a data one, which the language does not
// guarantee to be interconvertible, so the two lookups cannot share a return
// type. What a caller does with the answer is restore it to the prototype its
// own call site was compiled against.
auto lyra_rt_find_subroutine(void* self, const void* name) -> void (*)();
auto lyra_rt_find_class(void* self, const void* name) -> const void*;

// Publishes what a `disable` naming this scope terminates, and reads it back
// (LRM 9.6.2). Neither carries a name, a scope having exactly one.
void lyra_rt_register_disable_target(void* self, void* target);
auto lyra_rt_find_disable_target(void* self) -> void*;

// Observable storage cell operations, reached through the cell's address. The
// entry names the cell's value domain; the runtime never inspects a type tag.
// A read yields a value of its own rather than a view of the cell's contents,
// so it stays valid across a later write to that cell -- generated code holds
// what it loaded, and nothing tells it when a store invalidates a view.
//
// `alloc` builds a cell for a local whose storage is lent by reference: a
// reference reaches storage through a cell and through nothing else, and it is
// this cell kind because a cell's address crosses as one `void*` every entry
// here reads alike. The cell is owned by the current generated call, which
// outlives the declaration that built it; nothing subscribes to a procedural
// local, so the update event a write raises wakes no one.
//
// `arm_sampling` and `sampled_load` are the same access to the same storage,
// differing only in which of the two values a cell holds answers: the current
// one, or the one the current time slot found there before anything in it ran
// (LRM 4.4.2.1, 16.5.1). Only an armed cell keeps the second, so a cell nothing
// samples carries neither the storage nor the work of maintaining it.
auto lyra_rt_packed_cell_alloc() -> void*;
auto lyra_rt_packed_cell_get(void* cell) -> void*;
void lyra_rt_packed_cell_initialize(void* cell, const void* prototype);
void lyra_rt_packed_cell_set(void* cell, const void* value);
void lyra_rt_packed_cell_arm_sampling(void* cell);
auto lyra_rt_packed_cell_sampled_load(void* cell) -> void*;
// Putting a cell under a procedural continuous assignment and taking it back
// out (LRM 10.6). Beginning one answers with the generation the evaluation
// driving it carries; driving answers whether that evaluation is still the one
// in effect, which is what stops one a later takeover superseded.
auto lyra_rt_packed_cell_begin_takeover(void* cell, const void* level) -> void*;
auto lyra_rt_packed_cell_drive_takeover(
    void* cell, const void* level, const void* generation, const void* value)
    -> bool;
void lyra_rt_packed_cell_end_takeover(void* cell, const void* level);
auto lyra_rt_string_cell_alloc() -> void*;
auto lyra_rt_string_cell_get(void* cell) -> void*;
void lyra_rt_string_cell_initialize(void* cell, const void* prototype);
void lyra_rt_string_cell_set(void* cell, const void* value);
void lyra_rt_string_cell_arm_sampling(void* cell);
auto lyra_rt_string_cell_sampled_load(void* cell) -> void*;
auto lyra_rt_real_cell_alloc() -> void*;
auto lyra_rt_real_cell_get(void* cell) -> void*;
void lyra_rt_real_cell_initialize(void* cell, const void* prototype);
void lyra_rt_real_cell_set(void* cell, const void* value);
void lyra_rt_real_cell_arm_sampling(void* cell);
auto lyra_rt_real_cell_sampled_load(void* cell) -> void*;
auto lyra_rt_shortreal_cell_alloc() -> void*;
auto lyra_rt_shortreal_cell_get(void* cell) -> void*;
void lyra_rt_shortreal_cell_initialize(void* cell, const void* prototype);
void lyra_rt_shortreal_cell_set(void* cell, const void* value);
void lyra_rt_shortreal_cell_arm_sampling(void* cell);
auto lyra_rt_shortreal_cell_sampled_load(void* cell) -> void*;

// What the ticks of one clocking event settled for one expression (LRM
// 16.9.3), reached only through the history's own address. The entry names the
// representation every value in it is realized in.
//
// `install` fills it with the expression's default sampled value and fixes how
// far back it reaches, so a read has no empty case and nothing counts ticks;
// `push` records what a tick settled, dropping the entry no read can name; and
// `at` answers with what the tick a read names settled, counting back from 1
// for the most recent.
void lyra_rt_packed_sampled_history_install(
    void* history, const void* default_value, const void* depth);
void lyra_rt_packed_sampled_history_push(void* history, const void* value);
auto lyra_rt_packed_sampled_history_at(
    const void* history, const void* ticks_back) -> void*;
void lyra_rt_string_sampled_history_install(
    void* history, const void* default_value, const void* depth);
void lyra_rt_string_sampled_history_push(void* history, const void* value);
auto lyra_rt_string_sampled_history_at(
    const void* history, const void* ticks_back) -> void*;
void lyra_rt_real_sampled_history_install(
    void* history, const void* default_value, const void* depth);
void lyra_rt_real_sampled_history_push(void* history, const void* value);
auto lyra_rt_real_sampled_history_at(
    const void* history, const void* ticks_back) -> void*;
void lyra_rt_shortreal_sampled_history_install(
    void* history, const void* default_value, const void* depth);
void lyra_rt_shortreal_sampled_history_push(void* history, const void* value);
auto lyra_rt_shortreal_sampled_history_at(
    const void* history, const void* ticks_back) -> void*;
void lyra_rt_tuple_sampled_history_install(
    void* history, const void* default_value, const void* depth);
void lyra_rt_tuple_sampled_history_push(void* history, const void* value);
auto lyra_rt_tuple_sampled_history_at(
    const void* history, const void* ticks_back) -> void*;
void lyra_rt_union_sampled_history_install(
    void* history, const void* default_value, const void* depth);
void lyra_rt_union_sampled_history_push(void* history, const void* value);
auto lyra_rt_union_sampled_history_at(
    const void* history, const void* ticks_back) -> void*;
void lyra_rt_tagged_union_sampled_history_install(
    void* history, const void* default_value, const void* depth);
void lyra_rt_tagged_union_sampled_history_push(
    void* history, const void* value);
auto lyra_rt_tagged_union_sampled_history_at(
    const void* history, const void* ticks_back) -> void*;
void lyra_rt_dynarray_sampled_history_install(
    void* history, const void* default_value, const void* depth);
void lyra_rt_dynarray_sampled_history_push(void* history, const void* value);
auto lyra_rt_dynarray_sampled_history_at(
    const void* history, const void* ticks_back) -> void*;
void lyra_rt_unpackedarray_sampled_history_install(
    void* history, const void* default_value, const void* depth);
void lyra_rt_unpackedarray_sampled_history_push(
    void* history, const void* value);
auto lyra_rt_unpackedarray_sampled_history_at(
    const void* history, const void* ticks_back) -> void*;
void lyra_rt_queue_sampled_history_install(
    void* history, const void* default_value, const void* depth);
void lyra_rt_queue_sampled_history_push(void* history, const void* value);
auto lyra_rt_queue_sampled_history_at(
    const void* history, const void* ticks_back) -> void*;
void lyra_rt_assocarray_sampled_history_install(
    void* history, const void* default_value, const void* depth);
void lyra_rt_assocarray_sampled_history_push(void* history, const void* value);
auto lyra_rt_assocarray_sampled_history_at(
    const void* history, const void* ticks_back) -> void*;

// What one concurrent assertion has in flight (LRM 16.14.1), reached only
// through the storage's own address. These carry machine words rather than
// values of the design, which is why one entry serves every assertion.
//
// `install` fixes how wide a position set is, what a pending attempt is owed
// when the run ends, and the statements an outcome selects. A tick opens with
// `begin_tick`, or with `disable_tick` where the disable condition held;
// `live_word` bounds the Boolean expressions the tick has to read, and
// `next_unstepped` walks the evaluations it still owes a step, answering -1
// when none is left. `bits_at`, `set_word` and `step` read one evaluation's
// position set, replace it with its successor, and record what the tick left
// it in; `seed_word` stages the set a new evaluation starts from and `seed`
// opens one on it. `settle` answers every attempt the sweep resolved.
void lyra_rt_evaluation_attempts_install(
    void* attempts, void* effects, std::uint64_t words, bool pending_holds,
    void* pass_action, void* fail_action);
void lyra_rt_evaluation_attempts_seed_word(
    void* attempts, std::uint64_t word, std::uint64_t bits);
void lyra_rt_evaluation_attempts_begin_tick(void* attempts);
void lyra_rt_evaluation_attempts_disable_tick(void* attempts);
auto lyra_rt_evaluation_attempts_live_word(
    const void* attempts, std::uint64_t word) -> std::uint64_t;
auto lyra_rt_evaluation_attempts_next_unstepped(void* attempts) -> std::int64_t;
auto lyra_rt_evaluation_attempts_bits_at(
    const void* attempts, std::int64_t index, std::uint64_t word)
    -> std::uint64_t;
void lyra_rt_evaluation_attempts_set_word(
    void* attempts, std::int64_t index, std::uint64_t word, std::uint64_t bits);
void lyra_rt_evaluation_attempts_step(
    void* attempts, std::int64_t index, std::uint64_t outcome);
void lyra_rt_evaluation_attempts_seed(
    void* attempts, std::int64_t index, bool this_tick);
void lyra_rt_evaluation_attempts_settle(void* attempts, void* effects);

// A procedural local whose value crosses a suspension (LRM 9.4). The cell lives
// in the running activation's frame, so the handle a generated frame holds
// across a suspension points into activation-lifetime storage rather than the
// per-stretch scope. `store` overwrites the cell -- the first store installs
// the declared representation -- and `load` copies the current value back into
// the per-stretch scope. No runtime handle and no subscriber wakeup: a
// procedural local is not observable.
auto lyra_rt_packed_value_cell_alloc() -> void*;
auto lyra_rt_string_value_cell_alloc() -> void*;
void lyra_rt_packed_value_cell_store(void* cell, const void* value);
void lyra_rt_string_value_cell_store(void* cell, const void* value);
auto lyra_rt_packed_value_cell_load(const void* cell) -> void*;
auto lyra_rt_string_value_cell_load(const void* cell) -> void*;

// One entry per operator per value domain: the generated module names the entry
// it means, so no operator code crosses the boundary. Each is the library peer
// of the C++ operator a native target would emit. The result is a transient
// value owned by the current call scope.
// Joining values and laying one down a stated number of times (LRM 11.4.12).
// What is joined follows the operand's domain, so one entry each serves both
// spellings. A join takes two operands: a longer source-level one folds into a
// chain, since an operand list of arbitrary length has no single entry to call.
auto lyra_rt_packed_concat(const void* lhs, const void* rhs) -> void*;
auto lyra_rt_packed_replicate(const void* operand, std::int64_t count) -> void*;

auto lyra_rt_packed_add(const void* lhs, const void* rhs) -> void*;
auto lyra_rt_packed_sub(const void* lhs, const void* rhs) -> void*;
auto lyra_rt_packed_mul(const void* lhs, const void* rhs) -> void*;
auto lyra_rt_packed_div(const void* lhs, const void* rhs) -> void*;
auto lyra_rt_packed_mod(const void* lhs, const void* rhs) -> void*;
auto lyra_rt_packed_and(const void* lhs, const void* rhs) -> void*;
auto lyra_rt_packed_or(const void* lhs, const void* rhs) -> void*;
auto lyra_rt_packed_xor(const void* lhs, const void* rhs) -> void*;
auto lyra_rt_packed_eq(const void* lhs, const void* rhs) -> void*;
auto lyra_rt_packed_ne(const void* lhs, const void* rhs) -> void*;
auto lyra_rt_packed_lt(const void* lhs, const void* rhs) -> void*;
auto lyra_rt_packed_le(const void* lhs, const void* rhs) -> void*;
auto lyra_rt_packed_gt(const void* lhs, const void* rhs) -> void*;
auto lyra_rt_packed_ge(const void* lhs, const void* rhs) -> void*;
auto lyra_rt_packed_logical_and(const void* lhs, const void* rhs) -> void*;
auto lyra_rt_packed_logical_or(const void* lhs, const void* rhs) -> void*;
auto lyra_rt_packed_neg(const void* operand) -> void*;
auto lyra_rt_packed_not(const void* operand) -> void*;
auto lyra_rt_packed_logical_not(const void* operand) -> void*;
auto lyra_rt_packed_inc(const void* operand) -> void*;
auto lyra_rt_packed_dec(const void* operand) -> void*;
auto lyra_rt_packed_to_bool(const void* operand) -> bool;

// Value builtins: the operations the source language spells as a call rather
// than an operator. Named `lyra_rt_<domain>_<builtin>`, the same way an
// operator entry is, so the generated module derives the symbol it means.
auto lyra_rt_packed_convert_from_packed(const void* src, const void* prototype)
    -> void*;
auto lyra_rt_packed_from_bool(bool value) -> void*;
auto lyra_rt_packed_from_int(std::int64_t value, const void* prototype)
    -> void*;
auto lyra_rt_packed_to_int64(const void* value) -> std::int64_t;
auto lyra_rt_packed_is_unknown(const void* value) -> void*;
auto lyra_rt_packed_count_bits(const void* value, const void* control_bits)
    -> void*;
auto lyra_rt_packed_clog2(const void* value) -> void*;
auto lyra_rt_packed_pow(const void* base, const void* exponent) -> void*;
auto lyra_rt_packed_shift_left(const void* value, const void* amount) -> void*;
auto lyra_rt_packed_logical_shift_right(const void* value, const void* amount)
    -> void*;
auto lyra_rt_packed_arithmetic_shift_right(
    const void* value, const void* amount) -> void*;
auto lyra_rt_packed_shift_left_assign(const void* value, const void* amount)
    -> void*;
auto lyra_rt_packed_logical_shift_right_assign(
    const void* value, const void* amount) -> void*;
auto lyra_rt_packed_arithmetic_shift_right_assign(
    const void* value, const void* amount) -> void*;
auto lyra_rt_packed_bitwise_xnor(const void* lhs, const void* rhs) -> void*;
auto lyra_rt_packed_logical_implication(const void* lhs, const void* rhs)
    -> void*;
auto lyra_rt_packed_logical_equivalence(const void* lhs, const void* rhs)
    -> void*;
auto lyra_rt_packed_case_equal(const void* lhs, const void* rhs) -> void*;
auto lyra_rt_packed_wildcard_equals(const void* lhs, const void* rhs) -> void*;
auto lyra_rt_packed_casez_equals(const void* lhs, const void* rhs) -> void*;
auto lyra_rt_packed_casex_equals(const void* lhs, const void* rhs) -> void*;
auto lyra_rt_packed_merge_conditional(const void* lhs, const void* rhs)
    -> void*;
auto lyra_rt_packed_reduction_and(const void* value) -> void*;
auto lyra_rt_packed_reduction_or(const void* value) -> void*;
auto lyra_rt_packed_reduction_xor(const void* value) -> void*;
auto lyra_rt_packed_reduction_nand(const void* value) -> void*;
auto lyra_rt_packed_reduction_nor(const void* value) -> void*;
auto lyra_rt_packed_reduction_xnor(const void* value) -> void*;
auto lyra_rt_packed_to_owned(const void* value) -> void*;
// Positional access (LRM 11.5.1). `element` copies the bit-select / element
// out; `with_element` returns a copy with that element replaced -- the
// functional write the execution backend uses because it cannot mutate a packed
// value in place.
auto lyra_rt_packed_element(const void* value, const void* index) -> void*;
auto lyra_rt_packed_with_element(
    const void* value, const void* index, const void* replacement) -> void*;
// A part-select states the shape its result takes through `shape`, a value of
// the result's declared type: the bounds decide which bits are selected, that
// type decides how they are structured.
auto lyra_rt_packed_slice(
    const void* value, const void* a, const void* b, const void* form,
    const void* shape) -> void*;
auto lyra_rt_packed_with_slice(
    const void* value, const void* a, const void* b, const void* form,
    const void* shape, const void* replacement) -> void*;

auto lyra_rt_string_from_packed_array(const void* bits) -> void*;
// LRM 21.3.4.3: an unpacked array of byte read as text, in element order.
auto lyra_rt_string_from_byte_array(const void* bytes) -> void*;
// The C string a `string` crosses the DPI-C boundary as (LRM 35.5.6). It points
// into the SV value, which outlives the call, so the foreign side may read it
// for the call's duration.
auto lyra_rt_string_cstr(const void* value) -> const char*;
auto lyra_rt_string_len(const void* value) -> void*;
auto lyra_rt_string_getc(const void* value, const void* index) -> void*;
// Positional access (LRM 6.16.2). `element` reads the character; `with_element`
// returns a copy with one character replaced -- the functional write the
// execution backend uses because it cannot mutate a string in place.
auto lyra_rt_string_element(const void* value, const void* index) -> void*;
auto lyra_rt_string_with_element(
    const void* value, const void* index, const void* replacement) -> void*;
auto lyra_rt_string_toupper(const void* value) -> void*;
auto lyra_rt_string_tolower(const void* value) -> void*;
auto lyra_rt_string_compare(const void* lhs, const void* rhs) -> void*;
auto lyra_rt_string_icompare(const void* lhs, const void* rhs) -> void*;
auto lyra_rt_string_substr(
    const void* value, const void* first, const void* last) -> void*;
auto lyra_rt_string_atoi(const void* value) -> void*;
auto lyra_rt_string_atohex(const void* value) -> void*;
auto lyra_rt_string_atooct(const void* value) -> void*;
auto lyra_rt_string_atobin(const void* value) -> void*;
auto lyra_rt_string_atoreal(const void* value) -> void*;
// LRM 6.16.14 -- 6.16.18 format the receiver from a number. The source language
// spells them as mutations of the receiver; the execution backend cannot mutate
// a string in place, so each returns the formatted string and the call site
// stores it back.
auto lyra_rt_string_putc(
    const void* value, const void* index, const void* character) -> void*;
auto lyra_rt_string_itoa(const void* value, const void* number) -> void*;
auto lyra_rt_string_hextoa(const void* value, const void* number) -> void*;
auto lyra_rt_string_octtoa(const void* value, const void* number) -> void*;
auto lyra_rt_string_bintoa(const void* value, const void* number) -> void*;
auto lyra_rt_string_realtoa(const void* value, const void* number) -> void*;

// LRM 21.3.4.3 `$sscanf` / `$fscanf`, resolved through the domain of the text
// they read. `prototypes` is the product of one value per conversion, stating
// the shape each parses into; the completion leads with the matched-conversion
// count and how far the parse advanced, then carries one value per prototype.
auto lyra_rt_string_scan_string(
    const void* input, const void* format, const void* prototypes) -> void*;
auto lyra_rt_string_scan_file(
    const void* input, const void* format, const void* prototypes) -> void*;

auto lyra_rt_string_add(const void* lhs, const void* rhs) -> void*;
auto lyra_rt_string_concat(const void* lhs, const void* rhs) -> void*;
auto lyra_rt_string_replicate(const void* operand, std::int64_t count) -> void*;
auto lyra_rt_string_eq(const void* lhs, const void* rhs) -> void*;
auto lyra_rt_string_ne(const void* lhs, const void* rhs) -> void*;
auto lyra_rt_string_case_equal(const void* lhs, const void* rhs) -> void*;
auto lyra_rt_string_lt(const void* lhs, const void* rhs) -> void*;
auto lyra_rt_string_le(const void* lhs, const void* rhs) -> void*;
auto lyra_rt_string_gt(const void* lhs, const void* rhs) -> void*;
auto lyra_rt_string_ge(const void* lhs, const void* rhs) -> void*;

// The `real` / `realtime` host-double value domain. A relational or equality
// entry yields a packed 1-bit; the arithmetic entries yield a real. `const`
// builds a real from a host-precision immediate, `from_int64` from an integer
// already read out of a packed value, and `from_shortreal` / `from_real`
// reshape the other real precision. The cell entries hold a real in storage
// that outlives the stretch that wrote it.
auto lyra_rt_real_add(const void* lhs, const void* rhs) -> void*;
auto lyra_rt_real_sub(const void* lhs, const void* rhs) -> void*;
auto lyra_rt_real_mul(const void* lhs, const void* rhs) -> void*;
auto lyra_rt_real_div(const void* lhs, const void* rhs) -> void*;
auto lyra_rt_real_neg(const void* operand) -> void*;
auto lyra_rt_real_inc(const void* operand) -> void*;
auto lyra_rt_real_dec(const void* operand) -> void*;
auto lyra_rt_real_eq(const void* lhs, const void* rhs) -> void*;
auto lyra_rt_real_ne(const void* lhs, const void* rhs) -> void*;
auto lyra_rt_real_lt(const void* lhs, const void* rhs) -> void*;
auto lyra_rt_real_le(const void* lhs, const void* rhs) -> void*;
auto lyra_rt_real_gt(const void* lhs, const void* rhs) -> void*;
auto lyra_rt_real_ge(const void* lhs, const void* rhs) -> void*;
auto lyra_rt_real_to_bool(const void* operand) -> bool;

// The LRM 20.8.2 Table 20-4 mathematics, whose behavior the standard defines
// to be that of the C library function each is cross-listed with. The
// two-argument rows take their second operand after the receiver, and `pow` is
// the row LRM 11.4.3 `**` on real operands asks for as well.
auto lyra_rt_real_pow(const void* base, const void* exponent) -> void*;
auto lyra_rt_real_ln(const void* value) -> void*;
auto lyra_rt_real_log10(const void* value) -> void*;
auto lyra_rt_real_exp(const void* value) -> void*;
auto lyra_rt_real_sqrt(const void* value) -> void*;
auto lyra_rt_real_floor(const void* value) -> void*;
auto lyra_rt_real_ceil(const void* value) -> void*;
auto lyra_rt_real_sin(const void* value) -> void*;
auto lyra_rt_real_cos(const void* value) -> void*;
auto lyra_rt_real_tan(const void* value) -> void*;
auto lyra_rt_real_asin(const void* value) -> void*;
auto lyra_rt_real_acos(const void* value) -> void*;
auto lyra_rt_real_atan(const void* value) -> void*;
auto lyra_rt_real_atan2(const void* y, const void* x) -> void*;
auto lyra_rt_real_hypot(const void* x, const void* y) -> void*;
auto lyra_rt_real_sinh(const void* value) -> void*;
auto lyra_rt_real_cosh(const void* value) -> void*;
auto lyra_rt_real_tanh(const void* value) -> void*;
auto lyra_rt_real_asinh(const void* value) -> void*;
auto lyra_rt_real_acosh(const void* value) -> void*;
auto lyra_rt_real_atanh(const void* value) -> void*;

// Reading a real out as an integer: LRM 6.12.1 rounds, LRM 20.5 `$rtoi`
// truncates, and the bit-pattern pair carries the IEEE 754 encoding itself.
auto lyra_rt_real_round(const void* value) -> std::int64_t;
auto lyra_rt_real_real_value(const void* value) -> double;
auto lyra_rt_real_truncate(const void* value) -> std::int64_t;
auto lyra_rt_real_to_bits(const void* value) -> std::int64_t;
auto lyra_rt_real_from_bits(std::int64_t bits) -> void*;

auto lyra_rt_real_const(double value) -> void*;
auto lyra_rt_real_from_int(std::int64_t value) -> void*;
auto lyra_rt_real_convert_from_shortreal(const void* value) -> void*;
auto lyra_rt_real_convert_from_real(const void* value) -> void*;
auto lyra_rt_real_value_cell_alloc() -> void*;
void lyra_rt_real_value_cell_store(void* cell, const void* value);
auto lyra_rt_real_value_cell_load(const void* cell) -> void*;
auto lyra_rt_real_make_print_value_item(const void* value, const void* spec)
    -> void*;
auto lyra_rt_real_make_format_arg(const void* value) -> void*;

// The `shortreal` host-float value domain, the single-precision peer of the
// real domain above.
auto lyra_rt_shortreal_add(const void* lhs, const void* rhs) -> void*;
auto lyra_rt_shortreal_sub(const void* lhs, const void* rhs) -> void*;
auto lyra_rt_shortreal_mul(const void* lhs, const void* rhs) -> void*;
auto lyra_rt_shortreal_div(const void* lhs, const void* rhs) -> void*;
auto lyra_rt_shortreal_neg(const void* operand) -> void*;
auto lyra_rt_shortreal_inc(const void* operand) -> void*;
auto lyra_rt_shortreal_dec(const void* operand) -> void*;
auto lyra_rt_shortreal_eq(const void* lhs, const void* rhs) -> void*;
auto lyra_rt_shortreal_ne(const void* lhs, const void* rhs) -> void*;
auto lyra_rt_shortreal_lt(const void* lhs, const void* rhs) -> void*;
auto lyra_rt_shortreal_le(const void* lhs, const void* rhs) -> void*;
auto lyra_rt_shortreal_gt(const void* lhs, const void* rhs) -> void*;
auto lyra_rt_shortreal_ge(const void* lhs, const void* rhs) -> void*;
auto lyra_rt_shortreal_to_bool(const void* operand) -> bool;
auto lyra_rt_shortreal_pow(const void* base, const void* exponent) -> void*;
auto lyra_rt_shortreal_round(const void* value) -> std::int64_t;
auto lyra_rt_shortreal_real_value(const void* value) -> float;
auto lyra_rt_shortreal_to_bits(const void* value) -> std::int64_t;
auto lyra_rt_shortreal_from_bits(std::int64_t bits) -> void*;
auto lyra_rt_shortreal_const(float value) -> void*;
auto lyra_rt_shortreal_from_int(std::int64_t value) -> void*;
auto lyra_rt_shortreal_convert_from_real(const void* value) -> void*;
auto lyra_rt_shortreal_value_cell_alloc() -> void*;
void lyra_rt_shortreal_value_cell_store(void* cell, const void* value);
auto lyra_rt_shortreal_value_cell_load(const void* cell) -> void*;
auto lyra_rt_shortreal_make_print_value_item(
    const void* value, const void* spec) -> void*;
auto lyra_rt_shortreal_make_format_arg(const void* value) -> void*;

// The `chandle` domain (LRM 6.14). A chandle is a pointer, so the domain
// carries its value inline: each operand IS the chandle value, not a handle to
// a runtime-owned value object. LRM 6.14 admits only the equality family (which
// yields a packed 1-bit) and the boolean test; there is no arithmetic, no
// ordering and no format entry. A null chandle is the host null pointer, a
// native constant; a chandle that names something came from a foreign call, and
// both directions of that crossing are entries so that which bits the value is
// stays the runtime's own answer.
auto lyra_rt_chandle_make(void* pointer) -> void*;
auto lyra_rt_chandle_ptr(void* operand) -> void*;
auto lyra_rt_chandle_eq(void* lhs, void* rhs) -> void*;
auto lyra_rt_chandle_ne(void* lhs, void* rhs) -> void*;
auto lyra_rt_chandle_case_equal(void* lhs, void* rhs) -> void*;
auto lyra_rt_chandle_to_bool(void* operand) -> bool;
auto lyra_rt_chandle_value_cell_alloc() -> void*;
void lyra_rt_chandle_value_cell_store(void* cell, void* value);
auto lyra_rt_chandle_value_cell_load(const void* cell) -> void*;

// The managed-reference domain (LRM 8.3, and the LRM 9.7 `process` a handle
// names). Unlike a chandle, an operand here is a handle to a runtime-owned
// value: what a handle carries is the object's address together with a share of
// its ownership, and a share cannot be recovered from an address alone, so the
// two travel together and a store copies both. LRM Table 11-1's "Any data type"
// row is the whole operator surface -- the equality family, which yields a
// packed 1-bit, and the boolean test. A null handle is the domain's default
// value rather than a null pointer, because a null pointer would be a second
// shape for the operand every entry would then have to tell apart.
auto lyra_rt_managedref_default() -> void*;
auto lyra_rt_managedref_eq(const void* lhs, const void* rhs) -> bool;
auto lyra_rt_managedref_ne(const void* lhs, const void* rhs) -> bool;
auto lyra_rt_managedref_case_equal(const void* lhs, const void* rhs) -> void*;
auto lyra_rt_managedref_to_bool(const void* operand) -> bool;
auto lyra_rt_managedref_value_cell_alloc() -> void*;
void lyra_rt_managedref_value_cell_store(void* cell, const void* value);
auto lyra_rt_managedref_value_cell_load(const void* cell) -> void*;

// Boxes a value-domain handle into a type-erased `RuntimeValue`, the form in
// which an aggregate holds its parts. A value crosses this way exactly where it
// states a representation the entry receiving it has no other way to know: a
// product's components, each of its own domain, and a container construction's
// element prototype, which is what every element beside it is then erased
// against. A value that conforms to a representation its entry already fixes
// crosses as the bare handle of its own domain instead. The domain rides in the
// symbol name, as every other domain-parametric entry does.
auto lyra_rt_packed_value_box(const void* value) -> void*;
auto lyra_rt_string_value_box(const void* value) -> void*;
auto lyra_rt_real_value_box(const void* value) -> void*;
auto lyra_rt_shortreal_value_box(const void* value) -> void*;
auto lyra_rt_chandle_value_box(void* value) -> void*;
auto lyra_rt_tuple_value_box(const void* value) -> void*;
auto lyra_rt_dynarray_value_box(const void* value) -> void*;

// The unpacked-struct domain (LRM 7.2), MIR's product type. A struct value is a
// runtime-owned product carried behind an opaque handle. It owns its components
// by value, so construction copies each component in and access copies out; the
// generated side only ever holds handles, never the product's internal storage.
//
// `make` collects the boxed components into the product value. `extract` copies
// component `index` back out; `update` returns a copy of the product with
// component `index` replaced -- a value operation, never an in-place write, so
// value semantics hold even when the product is shared.
auto lyra_rt_tuple_make(LyraSpan components) -> void*;
auto lyra_rt_tuple_extract(const void* tuple, std::int64_t index) -> void*;
auto lyra_rt_tuple_update(const void* tuple, std::int64_t index, void* value)
    -> void*;
auto lyra_rt_tuple_eq(const void* lhs, const void* rhs) -> void*;
auto lyra_rt_tuple_ne(const void* lhs, const void* rhs) -> void*;
auto lyra_rt_tuple_case_equal(const void* lhs, const void* rhs) -> void*;
auto lyra_rt_tuple_is_unknown(const void* value) -> void*;
auto lyra_rt_tuple_cell_alloc() -> void*;
auto lyra_rt_tuple_cell_get(void* cell) -> void*;
void lyra_rt_tuple_cell_initialize(void* cell, const void* prototype);
void lyra_rt_tuple_cell_set(void* cell, const void* value);
void lyra_rt_tuple_cell_arm_sampling(void* cell);
auto lyra_rt_tuple_cell_sampled_load(void* cell) -> void*;
auto lyra_rt_tuple_value_cell_alloc() -> void*;
void lyra_rt_tuple_value_cell_store(void* cell, const void* value);
auto lyra_rt_tuple_value_cell_load(const void* cell) -> void*;

// The untagged-union domain (LRM 7.3), MIR's `UnionType`. An active-member
// value carried behind an opaque handle: it stores the one live member and its
// index. `make` builds it from an index and a boxed member value; `extract`
// returns the member at `index`, which must be the live one -- a cross-member
// read is undefined (LRM 7.3) and, since only the active member is stored,
// reported rather than defaulted on this backend; `update` returns a copy whose
// live member is `index` carrying the boxed replacement. All are value
// operations, never in-place writes.
auto lyra_rt_union_value_box(const void* value) -> void*;
auto lyra_rt_union_make(std::int64_t index, void* value) -> void*;
auto lyra_rt_union_extract(const void* value, std::int64_t index) -> void*;
auto lyra_rt_union_update(const void* value, std::int64_t index, void* member)
    -> void*;
auto lyra_rt_union_eq(const void* lhs, const void* rhs) -> void*;
auto lyra_rt_union_ne(const void* lhs, const void* rhs) -> void*;
auto lyra_rt_union_case_equal(const void* lhs, const void* rhs) -> void*;
auto lyra_rt_union_is_unknown(const void* value) -> void*;
auto lyra_rt_union_cell_alloc() -> void*;
auto lyra_rt_union_cell_get(void* cell) -> void*;
void lyra_rt_union_cell_initialize(void* cell, const void* prototype);
void lyra_rt_union_cell_set(void* cell, const void* value);
void lyra_rt_union_cell_arm_sampling(void* cell);
auto lyra_rt_union_cell_sampled_load(void* cell) -> void*;
auto lyra_rt_union_value_cell_alloc() -> void*;
void lyra_rt_union_value_cell_store(void* cell, const void* value);
auto lyra_rt_union_value_cell_load(const void* cell) -> void*;

// The tagged-union domain (LRM 7.3.2 / 11.9), MIR's `TaggedUnionType`. The
// tagged sibling of the untagged union: the tag is observable, so `extract` and
// `update` fault when `index` is not the live tag rather than returning a
// fallback, and `tag_matches` answers whether the active tag is a given one,
// the packed guard a pattern match tests (LRM 12.6). `make` builds it from a
// tag and a boxed payload; re-tagging goes through `make`, never `update`.
auto lyra_rt_tagged_union_value_box(const void* value) -> void*;
auto lyra_rt_tagged_union_make(std::int64_t tag, void* payload) -> void*;
auto lyra_rt_tagged_union_extract(const void* value, std::int64_t index)
    -> void*;
auto lyra_rt_tagged_union_update(
    const void* value, std::int64_t index, void* member) -> void*;
auto lyra_rt_tagged_union_tag_matches(const void* value, std::int64_t index)
    -> bool;
auto lyra_rt_tagged_union_eq(const void* lhs, const void* rhs) -> void*;
auto lyra_rt_tagged_union_ne(const void* lhs, const void* rhs) -> void*;
auto lyra_rt_tagged_union_case_equal(const void* lhs, const void* rhs) -> void*;
auto lyra_rt_tagged_union_is_unknown(const void* value) -> void*;
auto lyra_rt_tagged_union_cell_alloc() -> void*;
auto lyra_rt_tagged_union_cell_get(void* cell) -> void*;
void lyra_rt_tagged_union_cell_initialize(void* cell, const void* prototype);
void lyra_rt_tagged_union_cell_set(void* cell, const void* value);
void lyra_rt_tagged_union_cell_arm_sampling(void* cell);
auto lyra_rt_tagged_union_cell_sampled_load(void* cell) -> void*;
auto lyra_rt_tagged_union_value_cell_alloc() -> void*;
void lyra_rt_tagged_union_value_cell_store(void* cell, const void* value);
auto lyra_rt_tagged_union_value_cell_load(const void* cell) -> void*;

// The empty domain: a tagged union's `void` member (LRM 7.3.2), a value with no
// bits. `default` builds the one value it has; `value_box` erases it for a
// build's payload the way every other domain does.
auto lyra_rt_empty_default() -> void*;
auto lyra_rt_empty_value_box(const void* value) -> void*;

// The dynamic-array domain (LRM 7.5), MIR's `DynamicArrayType`. A
// run-time-sized homogeneous container carried behind an opaque handle, owning
// its elements by value. `default` / `new` / `new_copy` are the LRM 7.5.1
// constructors (empty, sized, sized-from-source); `from_literal` collects the
// elements of an assignment pattern. The element default rides every
// constructor -- the shape source for out-of-range reads (LRM 7.4.5) and resize
// fills -- and it crosses erased, because it is what states the element's
// representation and nothing here knows that representation before it arrives.
// A literal's elements then cross as bare handles: the prototype beside them
// names their domain, so the entry erases them itself. `element` copies an
// element out; `with_element` returns a copy of the array with one element
// replaced (LRM 7.4.6), and `delete` a copy emptied (LRM 7.5.3) -- value
// operations, never in-place writes, so value semantics hold even when the
// array is shared.
auto lyra_rt_make_dynamic_array_default(void* prototype) -> void*;
auto lyra_rt_make_dynamic_array_new(const void* size, void* prototype) -> void*;
auto lyra_rt_make_dynamic_array_new_copy(
    const void* size, void* prototype, const void* src) -> void*;
auto lyra_rt_dynarray_from_literal(
    void* prototype, LyraSpan unit, std::int64_t count) -> void*;
// LRM 7.6: one unpacked array kind taking another's elements. The entry names
// both representations because the source is read through the one it has and
// the result is built in the one the destination declares.
auto lyra_rt_dynarray_from_array_unpackedarray(
    const void* source, void* prototype) -> void*;
auto lyra_rt_dynarray_from_array_queue(const void* source, void* prototype)
    -> void*;
auto lyra_rt_dynarray_element(const void* array, const void* index) -> void*;
auto lyra_rt_dynarray_concat_element(const void* array, void* item) -> void*;
auto lyra_rt_dynarray_concat_spread(const void* array, const void* part)
    -> void*;
auto lyra_rt_dynarray_with_element(
    const void* array, const void* index, void* value) -> void*;
auto lyra_rt_dynarray_delete(const void* array) -> void*;
auto lyra_rt_dynarray_slice(
    const void* array, const void* a, const void* b, const void* form) -> void*;
auto lyra_rt_dynarray_with_slice(
    const void* array, const void* a, const void* b, const void* form,
    const void* replacement) -> void*;
auto lyra_rt_dynarray_size(const void* array) -> void*;
auto lyra_rt_dynarray_eq(const void* lhs, const void* rhs) -> void*;
auto lyra_rt_dynarray_ne(const void* lhs, const void* rhs) -> void*;
auto lyra_rt_dynarray_case_equal(const void* lhs, const void* rhs) -> void*;
auto lyra_rt_dynarray_cell_alloc() -> void*;
auto lyra_rt_dynarray_cell_get(void* cell) -> void*;
void lyra_rt_dynarray_cell_initialize(void* cell, const void* prototype);
void lyra_rt_dynarray_cell_set(void* cell, const void* value);
void lyra_rt_dynarray_cell_arm_sampling(void* cell);
auto lyra_rt_dynarray_cell_sampled_load(void* cell) -> void*;
auto lyra_rt_dynarray_value_cell_alloc() -> void*;
void lyra_rt_dynarray_value_cell_store(void* cell, const void* value);
auto lyra_rt_dynarray_value_cell_load(const void* cell) -> void*;

// A fixed-size unpacked array (LRM 7.4.2). Its payload is ordinal-only: the
// declared range is the receiver's static type's, so every coordinate-consuming
// entry takes it as a `[left:right]` operand pair rather than reading it off
// the value.
auto lyra_rt_unpackedarray_from_literal(
    void* prototype, LyraSpan unit, std::int64_t count) -> void*;
auto lyra_rt_unpackedarray_conform_size(const void* parts, std::int64_t count)
    -> void*;
auto lyra_rt_unpackedarray_from_array_dynarray(
    const void* source, void* prototype, std::int64_t declared) -> void*;
auto lyra_rt_unpackedarray_from_array_queue(
    const void* source, void* prototype, std::int64_t declared) -> void*;
auto lyra_rt_unpackedarray_element(
    const void* array, const void* index, const void* left, const void* right)
    -> void*;
auto lyra_rt_unpackedarray_with_element(
    const void* array, const void* index, const void* left, const void* right,
    void* value) -> void*;
auto lyra_rt_unpackedarray_slice(
    const void* array, const void* a, const void* b, const void* form,
    const void* left, const void* right) -> void*;
auto lyra_rt_unpackedarray_with_slice(
    const void* array, const void* a, const void* b, const void* form,
    const void* left, const void* right, const void* replacement) -> void*;
auto lyra_rt_unpackedarray_size(const void* array) -> void*;
auto lyra_rt_unpackedarray_eq(const void* lhs, const void* rhs) -> void*;
auto lyra_rt_unpackedarray_ne(const void* lhs, const void* rhs) -> void*;
auto lyra_rt_unpackedarray_case_equal(const void* lhs, const void* rhs)
    -> void*;
auto lyra_rt_unpackedarray_is_unknown(const void* value) -> void*;
// The value a conditional whose arms disagree yields (LRM 11.4.11): each
// element takes the two arms' merge, so an element the arms agree on survives
// and one they differ on becomes unknown.
auto lyra_rt_unpackedarray_merge_conditional(const void* lhs, const void* rhs)
    -> void*;
// The LRM 6.24.1 bit-stream cast of a packed value into an unpacked array: the
// bits are cut into `count` elements of the stated element type.
auto lyra_rt_unpackedarray_from_packed_array(
    const void* bits, const void* element_type, const void* count) -> void*;
auto lyra_rt_unpackedarray_value_box(const void* value) -> void*;
auto lyra_rt_unpackedarray_cell_alloc() -> void*;
auto lyra_rt_unpackedarray_cell_get(void* cell) -> void*;
void lyra_rt_unpackedarray_cell_initialize(void* cell, const void* prototype);
void lyra_rt_unpackedarray_cell_set(void* cell, const void* value);
void lyra_rt_unpackedarray_cell_arm_sampling(void* cell);
auto lyra_rt_unpackedarray_cell_sampled_load(void* cell) -> void*;
auto lyra_rt_unpackedarray_value_cell_alloc() -> void*;
void lyra_rt_unpackedarray_value_cell_store(void* cell, const void* value);
auto lyra_rt_unpackedarray_value_cell_load(const void* cell) -> void*;

// Nets and their drivers (LRM 6.5, 6.6). A net is storage of its own, like a
// cell: one `net_initialize` entry per resolution fixes, once, the net's
// declared type and the contribution its net type makes to its own resolution
// -- the value it shows where nothing drives it, as a scalar filling the
// declared type, and the strength it holds that value at (LRM 6.7.1). `net_get`
// answers with the resolution of every contribution. It takes no store -- a
// value reaches a net only through a driver.
//
// `attach_driver` issues one at the strength its source drives at, and the
// handle it answers with is the net's to own, so a source may hold it for as
// long as the net lives. `driver_set` publishes that driver's whole
// contribution, after which the net re-resolves and wakes its subscribers only
// on a real change; `driver_get` reads the contribution back, which is what a
// source driving part of a net updates part of and leaves the rest of at high
// impedance (LRM 6.6.1).
//
// `net_join` makes two nets one resolution, over the contributions of both
// (LRM 23.3.3.7). It takes the other net rather than a value, and states no
// direction; both nets then answer with what that one resolution produces.
//
// LRM 6.7.1 fixes which domains these exist for: a 4-state integral net, and a
// fixed-size unpacked array, struct, or union whose elements are themselves
// valid for a net.
auto lyra_rt_packed_net_get(void* net) -> void*;
void lyra_rt_packed_net_initialize_tri_state(
    void* net, const void* prototype, const void* fill, const void* strength);
void lyra_rt_packed_net_initialize_wired_and(
    void* net, const void* prototype, const void* fill, const void* strength);
void lyra_rt_packed_net_initialize_wired_or(
    void* net, const void* prototype, const void* fill, const void* strength);
void lyra_rt_packed_net_initialize_retaining(
    void* net, const void* prototype, const void* fill, const void* strength);
// Forcing a net and releasing it (LRM 10.6.2). What these change is the value
// the net shows; its drivers go on updating their contributions underneath,
// which is what the net answers with again once it is released.
auto lyra_rt_packed_net_begin_takeover(void* net, const void* level) -> void*;
auto lyra_rt_packed_net_drive_takeover(
    void* net, const void* level, const void* generation, const void* value)
    -> bool;
void lyra_rt_packed_net_end_takeover(void* net, const void* level);
auto lyra_rt_packed_attach_driver(void* net, const void* strength) -> void*;
void lyra_rt_packed_net_join(void* net, void* other);
auto lyra_rt_packed_driver_get(void* driver) -> void*;
void lyra_rt_packed_driver_set(void* driver, const void* value);
auto lyra_rt_tuple_net_get(void* net) -> void*;
void lyra_rt_tuple_net_initialize_tri_state(
    void* net, const void* prototype, const void* fill, const void* strength);
void lyra_rt_tuple_net_initialize_wired_and(
    void* net, const void* prototype, const void* fill, const void* strength);
void lyra_rt_tuple_net_initialize_wired_or(
    void* net, const void* prototype, const void* fill, const void* strength);
void lyra_rt_tuple_net_initialize_retaining(
    void* net, const void* prototype, const void* fill, const void* strength);
auto lyra_rt_tuple_attach_driver(void* net, const void* strength) -> void*;
void lyra_rt_tuple_net_join(void* net, void* other);
auto lyra_rt_tuple_driver_get(void* driver) -> void*;
void lyra_rt_tuple_driver_set(void* driver, const void* value);
auto lyra_rt_union_net_get(void* net) -> void*;
void lyra_rt_union_net_initialize_tri_state(
    void* net, const void* prototype, const void* fill, const void* strength);
void lyra_rt_union_net_initialize_wired_and(
    void* net, const void* prototype, const void* fill, const void* strength);
void lyra_rt_union_net_initialize_wired_or(
    void* net, const void* prototype, const void* fill, const void* strength);
void lyra_rt_union_net_initialize_retaining(
    void* net, const void* prototype, const void* fill, const void* strength);
auto lyra_rt_union_attach_driver(void* net, const void* strength) -> void*;
void lyra_rt_union_net_join(void* net, void* other);
auto lyra_rt_union_driver_get(void* driver) -> void*;
void lyra_rt_union_driver_set(void* driver, const void* value);
auto lyra_rt_unpackedarray_net_get(void* net) -> void*;
void lyra_rt_unpackedarray_net_initialize_tri_state(
    void* net, const void* prototype, const void* fill, const void* strength);
void lyra_rt_unpackedarray_net_initialize_wired_and(
    void* net, const void* prototype, const void* fill, const void* strength);
void lyra_rt_unpackedarray_net_initialize_wired_or(
    void* net, const void* prototype, const void* fill, const void* strength);
void lyra_rt_unpackedarray_net_initialize_retaining(
    void* net, const void* prototype, const void* fill, const void* strength);
auto lyra_rt_unpackedarray_attach_driver(void* net, const void* strength)
    -> void*;
void lyra_rt_unpackedarray_net_join(void* net, void* other);
auto lyra_rt_unpackedarray_driver_get(void* driver) -> void*;
void lyra_rt_unpackedarray_driver_set(void* driver, const void* value);

// The queue domain (LRM 7.10): a run-time-sized ordered container whose
// elements are added and removed at either end, carried behind an opaque handle
// and owning its elements by value. A queue is built over an element list,
// empty or not, and a declared bound (LRM 7.10.5) is a value its constructor
// takes rather than one it can derive -- so a bounded queue has an entry of its
// own. The bound belongs to the variable, not to the value written, so
// `conform_bound` is what a semantic store into a bounded queue passes its
// right-hand side through. An element write appends when its index is the
// queue's size and is discarded at any other invalid index (LRM 7.10.1); every
// apparent mutation -- an element write, a push, an insert, a delete -- yields
// a new queue rather than writing in place, so value semantics hold even when
// the queue is shared.
auto lyra_rt_queue_from_literal(
    void* prototype, LyraSpan unit, std::int64_t count) -> void*;
auto lyra_rt_queue_from_literal_bounded(
    void* prototype, LyraSpan unit, std::int64_t count, const void* max_bound)
    -> void*;
auto lyra_rt_queue_conform_bound(const void* queue, const void* max_bound)
    -> void*;
auto lyra_rt_queue_from_array_unpackedarray(
    const void* source, void* prototype, const void* max_bound) -> void*;
auto lyra_rt_queue_from_array_dynarray(
    const void* source, void* prototype, const void* max_bound) -> void*;
auto lyra_rt_queue_element(const void* queue, const void* index) -> void*;
auto lyra_rt_queue_with_element(
    const void* queue, const void* index, void* value) -> void*;
auto lyra_rt_queue_slice(
    const void* queue, const void* anchor, const void* extent, const void* form)
    -> void*;
auto lyra_rt_queue_size(const void* queue) -> void*;
auto lyra_rt_queue_push_back(const void* queue, void* item) -> void*;
auto lyra_rt_queue_push_front(const void* queue, void* item) -> void*;
auto lyra_rt_queue_concat_element(const void* queue, void* item) -> void*;
auto lyra_rt_queue_concat_spread(const void* queue, const void* part) -> void*;
auto lyra_rt_queue_insert(const void* queue, const void* index, void* item)
    -> void*;
// LRM 7.10.2.4 / 7.10.2.5 pop. Each completes with the queue left once the
// element goes and the element itself, because the two are one call's two
// answers.
auto lyra_rt_queue_pop_front(const void* queue) -> void*;
auto lyra_rt_queue_pop_back(const void* queue) -> void*;
// LRM 7.10.2.3 `delete`: with no index the whole queue empties, with one only
// the entry it names goes, so the two spellings are two entries.
auto lyra_rt_queue_delete(const void* queue) -> void*;
auto lyra_rt_queue_delete_index(const void* queue, const void* index) -> void*;
auto lyra_rt_queue_eq(const void* lhs, const void* rhs) -> void*;
auto lyra_rt_queue_ne(const void* lhs, const void* rhs) -> void*;
auto lyra_rt_queue_case_equal(const void* lhs, const void* rhs) -> void*;
auto lyra_rt_queue_bitstream_width(const void* queue) -> void*;
auto lyra_rt_queue_count_bits(const void* queue, const void* control_bits)
    -> void*;
auto lyra_rt_queue_value_box(const void* value) -> void*;
auto lyra_rt_queue_cell_alloc() -> void*;
auto lyra_rt_queue_cell_get(void* cell) -> void*;
void lyra_rt_queue_cell_initialize(void* cell, const void* prototype);
void lyra_rt_queue_cell_set(void* cell, const void* value);
void lyra_rt_queue_cell_arm_sampling(void* cell);
auto lyra_rt_queue_cell_sampled_load(void* cell) -> void*;
auto lyra_rt_queue_value_cell_alloc() -> void*;
void lyra_rt_queue_value_cell_store(void* cell, const void* value);
auto lyra_rt_queue_value_cell_load(const void* cell) -> void*;

// The associative-array domain (LRM 7.8): a sparse lookup table allocated entry
// by entry and held in index order, carried behind an opaque handle. Its
// element default carries the element shape and crosses erased at construction
// like every other container's; what a read of an index with no entry yields
// (LRM 7.8.6) is a second value the construction takes. An index
// crosses erased too, and for a reason of its own: the array holds no prototype
// for one, so nothing here could know the representation the program wrote it
// in. An element beside an index still crosses bare, since the element default
// names its domain. Every apparent mutation yields a new array rather than
// writing in place, so value semantics hold even when the array is shared.
auto lyra_rt_assocarray_from_entries_default(
    void* prototype, LyraSpan entries, void* user_default) -> void*;
auto lyra_rt_assocarray_element(const void* array, const void* index) -> void*;
auto lyra_rt_assocarray_with_element(
    const void* array, const void* index, void* value) -> void*;
auto lyra_rt_assocarray_exists(const void* array, const void* index) -> void*;
auto lyra_rt_assocarray_size(const void* array) -> void*;
// LRM 7.9.3 `delete`: with no index the whole array empties, with one only the
// entry it names goes, so the two spellings are two entries.
auto lyra_rt_assocarray_delete(const void* array) -> void*;
auto lyra_rt_assocarray_delete_index(const void* array, const void* index)
    -> void*;
auto lyra_rt_assocarray_eq(const void* lhs, const void* rhs) -> void*;
auto lyra_rt_assocarray_ne(const void* lhs, const void* rhs) -> void*;
auto lyra_rt_assocarray_case_equal(const void* lhs, const void* rhs) -> void*;
auto lyra_rt_assocarray_bitstream_width(const void* array) -> void*;
// LRM 20.7 `$low` / `$high` over an associative dimension: the smallest and
// largest index the array holds, or `unallocated` where it holds none. That
// answer is an index, so it crosses erased for the same reason a probe does.
auto lyra_rt_assocarray_assoc_min_index(const void* array, void* unallocated)
    -> void*;
auto lyra_rt_assocarray_assoc_max_index(const void* array, void* unallocated)
    -> void*;
// LRM 7.9.4 -- 7.9.7 traversal. Each completes with the SV int it answers with
// and the index it visited, which is the probe unchanged when there is no such
// index; the probe crosses erased because an index states its own
// representation.
auto lyra_rt_assocarray_assoc_first(const void* array, void* probe) -> void*;
auto lyra_rt_assocarray_assoc_last(const void* array, void* probe) -> void*;
auto lyra_rt_assocarray_assoc_next(const void* array, void* probe) -> void*;
auto lyra_rt_assocarray_assoc_prev(const void* array, void* probe) -> void*;
auto lyra_rt_assocarray_count_bits(const void* array, const void* control_bits)
    -> void*;
auto lyra_rt_assocarray_value_box(const void* value) -> void*;
auto lyra_rt_assocarray_cell_alloc() -> void*;
auto lyra_rt_assocarray_cell_get(void* cell) -> void*;
void lyra_rt_assocarray_cell_initialize(void* cell, const void* prototype);
void lyra_rt_assocarray_cell_set(void* cell, const void* value);
void lyra_rt_assocarray_cell_arm_sampling(void* cell);
auto lyra_rt_assocarray_cell_sampled_load(void* cell) -> void*;
auto lyra_rt_assocarray_value_cell_alloc() -> void*;
void lyra_rt_assocarray_value_cell_store(void* cell, const void* value);
auto lyra_rt_assocarray_value_cell_load(const void* cell) -> void*;

// LRM 7.12 array manipulation. The body a `with` clause states is a closure run
// over each of the receiver's entries, handed the element and that entry's
// index and taking back what it settled on; a result whose shape the receiver
// does not determine takes the prototype the call supplies, which crosses
// erased because the shape it states varies with the clause rather than with
// the receiver. The ordering family reorders the receiver and produces no
// element it did not already hold, so it takes no prototype, and `reverse`
// projects nothing, so it runs no body. The clause defines ordering on the
// ordinally indexed containers alone.
auto lyra_rt_unpackedarray_sum(
    const void* receiver, void* body, void* prototype) -> void*;
auto lyra_rt_unpackedarray_product(
    const void* receiver, void* body, void* prototype) -> void*;
auto lyra_rt_unpackedarray_and(
    const void* receiver, void* body, void* prototype) -> void*;
auto lyra_rt_unpackedarray_or(const void* receiver, void* body, void* prototype)
    -> void*;
auto lyra_rt_unpackedarray_xor(
    const void* receiver, void* body, void* prototype) -> void*;
auto lyra_rt_unpackedarray_find(
    const void* receiver, void* body, void* prototype) -> void*;
auto lyra_rt_unpackedarray_find_index(
    const void* receiver, void* body, void* prototype) -> void*;
auto lyra_rt_unpackedarray_find_first(
    const void* receiver, void* body, void* prototype) -> void*;
auto lyra_rt_unpackedarray_find_first_index(
    const void* receiver, void* body, void* prototype) -> void*;
auto lyra_rt_unpackedarray_find_last(
    const void* receiver, void* body, void* prototype) -> void*;
auto lyra_rt_unpackedarray_find_last_index(
    const void* receiver, void* body, void* prototype) -> void*;
auto lyra_rt_unpackedarray_min(
    const void* receiver, void* body, void* prototype) -> void*;
auto lyra_rt_unpackedarray_max(
    const void* receiver, void* body, void* prototype) -> void*;
auto lyra_rt_unpackedarray_unique(
    const void* receiver, void* body, void* prototype) -> void*;
auto lyra_rt_unpackedarray_unique_index(
    const void* receiver, void* body, void* prototype) -> void*;
auto lyra_rt_unpackedarray_map(
    const void* receiver, void* body, void* prototype) -> void*;
auto lyra_rt_dynarray_sum(const void* receiver, void* body, void* prototype)
    -> void*;
auto lyra_rt_dynarray_product(const void* receiver, void* body, void* prototype)
    -> void*;
auto lyra_rt_dynarray_and(const void* receiver, void* body, void* prototype)
    -> void*;
auto lyra_rt_dynarray_or(const void* receiver, void* body, void* prototype)
    -> void*;
auto lyra_rt_dynarray_xor(const void* receiver, void* body, void* prototype)
    -> void*;
auto lyra_rt_dynarray_find(const void* receiver, void* body, void* prototype)
    -> void*;
auto lyra_rt_dynarray_find_index(
    const void* receiver, void* body, void* prototype) -> void*;
auto lyra_rt_dynarray_find_first(
    const void* receiver, void* body, void* prototype) -> void*;
auto lyra_rt_dynarray_find_first_index(
    const void* receiver, void* body, void* prototype) -> void*;
auto lyra_rt_dynarray_find_last(
    const void* receiver, void* body, void* prototype) -> void*;
auto lyra_rt_dynarray_find_last_index(
    const void* receiver, void* body, void* prototype) -> void*;
auto lyra_rt_dynarray_min(const void* receiver, void* body, void* prototype)
    -> void*;
auto lyra_rt_dynarray_max(const void* receiver, void* body, void* prototype)
    -> void*;
auto lyra_rt_dynarray_unique(const void* receiver, void* body, void* prototype)
    -> void*;
auto lyra_rt_dynarray_unique_index(
    const void* receiver, void* body, void* prototype) -> void*;
auto lyra_rt_dynarray_map(const void* receiver, void* body, void* prototype)
    -> void*;
auto lyra_rt_queue_sum(const void* receiver, void* body, void* prototype)
    -> void*;
auto lyra_rt_queue_product(const void* receiver, void* body, void* prototype)
    -> void*;
auto lyra_rt_queue_and(const void* receiver, void* body, void* prototype)
    -> void*;
auto lyra_rt_queue_or(const void* receiver, void* body, void* prototype)
    -> void*;
auto lyra_rt_queue_xor(const void* receiver, void* body, void* prototype)
    -> void*;
auto lyra_rt_queue_find(const void* receiver, void* body, void* prototype)
    -> void*;
auto lyra_rt_queue_find_index(const void* receiver, void* body, void* prototype)
    -> void*;
auto lyra_rt_queue_find_first(const void* receiver, void* body, void* prototype)
    -> void*;
auto lyra_rt_queue_find_first_index(
    const void* receiver, void* body, void* prototype) -> void*;
auto lyra_rt_queue_find_last(const void* receiver, void* body, void* prototype)
    -> void*;
auto lyra_rt_queue_find_last_index(
    const void* receiver, void* body, void* prototype) -> void*;
auto lyra_rt_queue_min(const void* receiver, void* body, void* prototype)
    -> void*;
auto lyra_rt_queue_max(const void* receiver, void* body, void* prototype)
    -> void*;
auto lyra_rt_queue_unique(const void* receiver, void* body, void* prototype)
    -> void*;
auto lyra_rt_queue_unique_index(
    const void* receiver, void* body, void* prototype) -> void*;
auto lyra_rt_queue_map(const void* receiver, void* body, void* prototype)
    -> void*;
auto lyra_rt_assocarray_sum(const void* receiver, void* body, void* prototype)
    -> void*;
auto lyra_rt_assocarray_product(
    const void* receiver, void* body, void* prototype) -> void*;
auto lyra_rt_assocarray_and(const void* receiver, void* body, void* prototype)
    -> void*;
auto lyra_rt_assocarray_or(const void* receiver, void* body, void* prototype)
    -> void*;
auto lyra_rt_assocarray_xor(const void* receiver, void* body, void* prototype)
    -> void*;
auto lyra_rt_assocarray_find(const void* receiver, void* body, void* prototype)
    -> void*;
auto lyra_rt_assocarray_find_index(
    const void* receiver, void* body, void* prototype) -> void*;
auto lyra_rt_assocarray_find_first(
    const void* receiver, void* body, void* prototype) -> void*;
auto lyra_rt_assocarray_find_first_index(
    const void* receiver, void* body, void* prototype) -> void*;
auto lyra_rt_assocarray_find_last(
    const void* receiver, void* body, void* prototype) -> void*;
auto lyra_rt_assocarray_find_last_index(
    const void* receiver, void* body, void* prototype) -> void*;
auto lyra_rt_assocarray_min(const void* receiver, void* body, void* prototype)
    -> void*;
auto lyra_rt_assocarray_max(const void* receiver, void* body, void* prototype)
    -> void*;
auto lyra_rt_assocarray_unique(
    const void* receiver, void* body, void* prototype) -> void*;
auto lyra_rt_assocarray_unique_index(
    const void* receiver, void* body, void* prototype) -> void*;
auto lyra_rt_assocarray_map(const void* receiver, void* body, void* prototype)
    -> void*;
auto lyra_rt_unpackedarray_sort(const void* receiver, void* body) -> void*;
auto lyra_rt_unpackedarray_rsort(const void* receiver, void* body) -> void*;
auto lyra_rt_dynarray_sort(const void* receiver, void* body) -> void*;
auto lyra_rt_dynarray_rsort(const void* receiver, void* body) -> void*;
auto lyra_rt_queue_sort(const void* receiver, void* body) -> void*;
auto lyra_rt_queue_rsort(const void* receiver, void* body) -> void*;
auto lyra_rt_unpackedarray_reverse(const void* receiver) -> void*;
auto lyra_rt_dynarray_reverse(const void* receiver) -> void*;
auto lyra_rt_queue_reverse(const void* receiver) -> void*;

// LRM 21.4 / 21.5 memory load and dump. The memory names the entry, since what
// an address means is its own: an unpacked memory reads the declared bounds of
// every dimension, which ride as a run of packed values with the addressed one
// first; a dynamic array or queue is the dense space its current size spans;
// and an associative memory is addressed by key, so a load takes a key
// prototype to build each key at the width an ordinary access uses. Running
// upward from an address and running within a window are two requests, so each
// is its own entry. A load answers through its completion, because a word the
// file does not address keeps what it held.
auto lyra_rt_unpackedarray_read_mem(
    void* runtime, const void* memory, const void* name, LyraSpan dims,
    const void* base, const void* start) -> void*;
auto lyra_rt_unpackedarray_read_mem_within(
    void* runtime, const void* memory, const void* name, LyraSpan dims,
    const void* base, const void* start, const void* finish) -> void*;
void lyra_rt_unpackedarray_write_mem(
    void* runtime, const void* memory, const void* name, LyraSpan dims,
    const void* base, const void* start);
void lyra_rt_unpackedarray_write_mem_within(
    void* runtime, const void* memory, const void* name, LyraSpan dims,
    const void* base, const void* start, const void* finish);
auto lyra_rt_dynarray_read_mem(
    void* runtime, const void* memory, const void* name, const void* base,
    const void* start) -> void*;
auto lyra_rt_dynarray_read_mem_within(
    void* runtime, const void* memory, const void* name, const void* base,
    const void* start, const void* finish) -> void*;
void lyra_rt_dynarray_write_mem(
    void* runtime, const void* memory, const void* name, const void* base,
    const void* start);
void lyra_rt_dynarray_write_mem_within(
    void* runtime, const void* memory, const void* name, const void* base,
    const void* start, const void* finish);
auto lyra_rt_queue_read_mem(
    void* runtime, const void* memory, const void* name, const void* base,
    const void* start) -> void*;
auto lyra_rt_queue_read_mem_within(
    void* runtime, const void* memory, const void* name, const void* base,
    const void* start, const void* finish) -> void*;
void lyra_rt_queue_write_mem(
    void* runtime, const void* memory, const void* name, const void* base,
    const void* start);
void lyra_rt_queue_write_mem_within(
    void* runtime, const void* memory, const void* name, const void* base,
    const void* start, const void* finish);
auto lyra_rt_assocarray_read_mem(
    void* runtime, const void* memory, const void* name,
    const void* key_prototype, const void* base, const void* start) -> void*;
auto lyra_rt_assocarray_read_mem_within(
    void* runtime, const void* memory, const void* name,
    const void* key_prototype, const void* base, const void* start,
    const void* finish) -> void*;
void lyra_rt_assocarray_write_mem(
    void* runtime, const void* memory, const void* name, const void* base,
    const void* start);
void lyra_rt_assocarray_write_mem_within(
    void* runtime, const void* memory, const void* name, const void* base,
    const void* start, const void* finish);

// LRM 21.3.3 / 5.9: text conformed to a destination's declared shape. An
// integral destination takes it right-justified and an unpacked array of bytes
// left-justified, which is why only the array form carries an element count.
auto lyra_rt_make_packed_range(std::int64_t left, std::int64_t right) -> const
    void*;
auto lyra_rt_make_packed_type(LyraSpan dims, bool is_signed, bool is_four_state)
    -> const void*;
auto lyra_rt_packed_from_words(
    LyraSpan value_words, LyraSpan unknown_words, const void* type) -> void*;
auto lyra_rt_packed_from_string(const void* text, const void* prototype)
    -> void*;
auto lyra_rt_unpackedarray_from_string(
    const void* text, const void* element_type, const void* count) -> void*;

// LRM 20.6.2 `$bits` over the domains whose value is a bit stream: how many
// bits the value currently holds, which for an aggregate is its parts' streams
// laid end to end. A packed value answers from its own shape and needs no entry
// here.
auto lyra_rt_string_bitstream_width(const void* value) -> void*;
auto lyra_rt_tuple_bitstream_width(const void* value) -> void*;
auto lyra_rt_dynarray_bitstream_width(const void* value) -> void*;
auto lyra_rt_unpackedarray_bitstream_width(const void* value) -> void*;

// LRM 6.24.3: the bits a value makes, and a value read back from them at the
// shape a prototype states. The prototype crosses boxed, as every operand
// naming a representation the entry cannot otherwise know does. One pair per
// domain a fixed-size stream is built from; a domain whose width only the
// running program fixes has no entry, because no stream over one is nameable.
auto lyra_rt_packed_to_bitstream(const void* value) -> void*;
auto lyra_rt_tuple_to_bitstream(const void* value) -> void*;
auto lyra_rt_unpackedarray_to_bitstream(const void* value) -> void*;
auto lyra_rt_packed_from_bitstream(const void* bits, void* prototype) -> void*;
auto lyra_rt_tuple_from_bitstream(const void* bits, void* prototype) -> void*;
auto lyra_rt_unpackedarray_from_bitstream(const void* bits, void* prototype)
    -> void*;

// LRM 11.4.14.2: a vector's `block`-wide blocks in reversed order, the bits
// inside each block left where they are.
auto lyra_rt_packed_reverse_blocks(const void* value, std::int64_t block)
    -> void*;

// LRM 20.9 `$countbits` over the domains whose value is a bit stream. An
// aggregate reduces over its parts, so each of these is the same fold seen at a
// different element type.
auto lyra_rt_string_count_bits(const void* value, const void* control_bits)
    -> void*;
auto lyra_rt_tuple_count_bits(const void* value, const void* control_bits)
    -> void*;
auto lyra_rt_dynarray_count_bits(const void* value, const void* control_bits)
    -> void*;
auto lyra_rt_unpackedarray_count_bits(
    const void* value, const void* control_bits) -> void*;

// Builds one conversion's format specification, and the print item that pairs a
// value with it. Each field arrives as a packed value, as the value model
// routes every compile-time scalar.
auto lyra_rt_make_format_spec(
    const void* kind, const void* width, const void* precision,
    const void* zero_pad, const void* left_align, const void* timeunit_power)
    -> void*;
auto lyra_rt_packed_make_print_value_item(const void* value, const void* spec)
    -> void*;
auto lyra_rt_string_make_print_value_item(const void* value, const void* spec)
    -> void*;

// A format performed at run time (LRM 21.3.3), where the format string is not a
// literal and so no print item could be built for it at compile time: the text
// is parsed against the arguments as it is rendered. Each argument borrows the
// value it formats, which holds because both are transients of the generated
// entry that performs the format. The hierarchical name a `%m` renders and the
// time scale a `%t` is read against are facts of the call site, so they arrive
// as operands rather than being reached from here.
auto lyra_rt_format_runtime(
    const void* format, LyraSpan args, const void* scope_path,
    const void* time_format, const void* timeunit_power) -> void*;
auto lyra_rt_packed_make_format_arg(const void* value) -> void*;
auto lyra_rt_string_make_format_arg(const void* value) -> void*;

// The DPI-C boundary temporaries (LRM 35.5.6, Annex H.7.7, H.10). None of these
// is an SV value: each exists inside one lowered call window, holding an image
// of an actual in the canonical form the C side reads and writes. So each entry
// is one function rather than a family -- what a canonical buffer, an `svLogic`
// scalar and an open-array image hold is fixed by the C ABI, and the SV value
// on the far side of one is always a packed value.
//
// A buffer sizes itself from the value it images and hands the foreign side the
// writable chunk pointer; the read entries rebuild an SV value, in the declared
// shape `type` names, from what the call left there. `bit` carries the value
// plane only, `logic` both planes. What the foreign side writes through points
// into the buffer, so it stays writable for exactly as long as the buffer does.
auto lyra_rt_make_dpi_bit_buffer(const void* sv) -> void*;
auto lyra_rt_make_dpi_logic_buffer(const void* sv) -> void*;
auto lyra_rt_dpi_bit_buffer_data(void* buffer) -> void*;
auto lyra_rt_dpi_logic_buffer_data(void* buffer) -> void*;
auto lyra_rt_read_canonical_bit_vec(const void* src, const void* type) -> void*;
auto lyra_rt_read_canonical_logic_vec(const void* src, const void* type)
    -> void*;

// A 1-bit 4-state value's `svLogic` scalar encoding (Annex H.10.1.1), which
// crosses as the machine byte the C side declares rather than as a handle.
auto lyra_rt_to_sv_logic(const void* sv) -> std::uint8_t;
auto lyra_rt_from_sv_logic(std::uint8_t encoded, const void* type) -> void*;

// The open-array image (LRM 35.5.6.1, Annex H.12). The value it images crosses
// erased, because an image is element-type-independent and nothing on this side
// could read that representation off anything else; `bounds` is the declared
// `(left, right)` pair of each unpacked dimension, outermost first, and
// `addressable_elements` says an individual value of the element type crosses
// in the same canonical form the image holds it in (Annex H.12.4). The handle
// is what the foreign side receives in place of the actual, and the value entry
// rebuilds one SV value shaped like the prototype a write-back direction hands
// it.
auto lyra_rt_make_dpi_open_array(
    void* sv, LyraSpan bounds, bool addressable_elements) -> void*;
auto lyra_rt_dpi_open_array_handle(void* image) -> void*;
auto lyra_rt_dpi_open_array_value(const void* image, void* prototype) -> void*;
}
