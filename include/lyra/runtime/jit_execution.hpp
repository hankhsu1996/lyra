#pragma once

#include <cstdint>

// The execution-strategy-neutral ABI the generated module calls. Every runtime
// value crosses as an opaque pointer; the runtime owns its type and lifetime.
//
// A `bool` is never one of those values. It is a machine predicate the
// generated code branches on -- a condition read off a value, a question about
// the running execution, or whether that execution must park before its suspend
// edge -- so it carries no width and no unknown state. Every other answer is a
// handle or nothing at all.
//
// Definitions wrap the runtime; a host resolves these symbols when it loads a
// generated module (JIT-compiled, AOT-linked, or interpreted).
extern "C" {

struct LyraSpan {
  void* data;
  std::uint64_t count;
};

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
// that activation, and raises into the caller any fault the call settled with,
// since the call is one statement of the calling thread.
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

// Builds a callable the runtime runs later: `definition` is an opaque
// cross-artifact reference naming both the body and the storage its captures
// need, and `captures` supplies one handle per capture in declaration order,
// each taken into that storage as the schema says -- a pointer held, a value
// copied. The value is transient, owned by the current call scope until
// something that outlives the stretch takes it: a region a deferred effect is
// submitted to, the coroutine a spawned branch is entered as, or the array
// method that runs a per-element body over its receiver.
auto lyra_rt_closure_make(const void* definition, LyraSpan captures) -> void*;

// Builds an object on the managed heap and runs its constructor on it (LRM
// 8.3): `definition` is an opaque cross-artifact reference naming both the
// storage its properties need and the body that initializes them. The handle
// answered is a reference to the object, transient like every value the
// boundary hands back and owned by the current call scope until a store takes
// a copy of it.
auto lyra_rt_object_make(const void* definition) -> void*;

// The object a class handle refers to (LRM 8.3). Which object that is, is a
// fact the handle holds rather than is, so reaching it is an operation; a
// handle referring to no object fails the run here rather than further in.
auto lyra_rt_object_deref(void* handle) -> void*;

// The address of a property's storage on an object, by its position in the
// storage that object owns. A class carries what its bases declare before what
// it declares itself, so a property keeps one position in the class that
// declares it and in every class extending that one.
auto lyra_rt_object_member_addr(void* object, std::uint32_t index) -> void*;

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

// Registers the running process to wake after `ticks` steps of its scope's
// precision (`precision_power`), the registration a delay's suspend edge is
// preceded by (LRM 9.4.1). A zero delay re-enqueues on the current slot's
// inactive region; a positive one scales to the engine's global tick. The
// counts cross as opaque packed values, like every scalar. The wakeup source is
// the running process itself, read from the runtime; no token crosses the
// boundary. A delay always parks.
auto lyra_rt_delay(
    void* runtime, const void* ticks, const void* precision_power) -> bool;

// Builds one leaf of a value-change wait: the observable cell it watches, the
// bit projection of that cell's packed encoding it watches as a
// `(lsb_bit_offset, bit_width)` pair, and the edge polarity it watches for (LRM
// 9.4.2). The scalars cross as opaque packed values, like every scalar. The
// trigger is a transient runtime value owned by the current call scope.
auto lyra_rt_make_trigger(
    void* observable, const void* edge, const void* lsb_bit_offset,
    const void* bit_width) -> void*;

// Registers the running process to wake when any leaf of `triggers` changes as
// its edge demands, the registration a value-change wait's suspend edge is
// preceded by (LRM 9.4.2 / 9.4.2.2 / 9.4.3). An empty span means "never wake
// up". The wakeup source is the running process itself, read from the runtime;
// no token crosses the boundary. A value-change wait always parks.
auto lyra_rt_wait_any(void* runtime, LyraSpan triggers) -> bool;

// A named event (LRM 15.5). Triggering records the instant and releases every
// process parked on the event at once, since the event carries no per-waiter
// condition to evaluate; awaiting parks the running process, which the runtime
// knows without being told, so nothing but the event crosses; and `triggered`
// answers whether the most recent trigger happened in this time step, which is
// a comparison of instants rather than a state the event clears.
void lyra_rt_trigger(void* event, void* runtime);
void lyra_rt_await(void* event);
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
// completes (LRM 20.2); the fatal form (LRM 20.10) additionally makes the run
// report a non-zero exit code. Each arranges no resumption at all, so each
// parks, and the recorded request is what keeps the process from ever being
// dispatched again. The level crosses as an opaque packed value, like every
// scalar.
auto lyra_rt_finish(void* runtime, const void* level) -> bool;
auto lyra_rt_fatal_finish(void* runtime, const void* level) -> bool;

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
// name may stand for an array of instances. A step matching nothing answers
// null.
auto lyra_rt_resolve_visible_child(
    void* self, const void* head_name, LyraSpan head_indices) -> void*;
auto lyra_rt_get_child(void* self, const void* name, LyraSpan indices) -> void*;

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
auto lyra_rt_get_signal(void* self, const void* name) -> void*;

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
auto lyra_rt_packed_cell_alloc() -> void*;
auto lyra_rt_packed_cell_get(void* cell) -> void*;
void lyra_rt_packed_cell_initialize(void* cell, const void* prototype);
void lyra_rt_packed_cell_set(void* cell, const void* value);
auto lyra_rt_string_cell_alloc() -> void*;
auto lyra_rt_string_cell_get(void* cell) -> void*;
void lyra_rt_string_cell_initialize(void* cell, const void* prototype);
void lyra_rt_string_cell_set(void* cell, const void* value);
auto lyra_rt_real_cell_alloc() -> void*;
auto lyra_rt_real_cell_get(void* cell) -> void*;
void lyra_rt_real_cell_initialize(void* cell, const void* prototype);
void lyra_rt_real_cell_set(void* cell, const void* value);
auto lyra_rt_shortreal_cell_alloc() -> void*;
auto lyra_rt_shortreal_cell_get(void* cell) -> void*;
void lyra_rt_shortreal_cell_initialize(void* cell, const void* prototype);
void lyra_rt_shortreal_cell_set(void* cell, const void* value);

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
auto lyra_rt_string_string_cstr(const void* value) -> const char*;
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

// The `chandle` domain (LRM 6.14). A chandle is a pointer, so the domain
// carries its value inline: each operand IS the chandle value, not a handle to
// a runtime-owned value object. LRM 6.14 admits only the equality family (which
// yields a packed 1-bit) and the boolean test; there is no arithmetic, no
// ordering, no format entry, and no runtime constructor -- a null chandle is
// the host null pointer, a native constant.
auto lyra_rt_chandle_eq(void* lhs, void* rhs) -> void*;
auto lyra_rt_chandle_ne(void* lhs, void* rhs) -> void*;
auto lyra_rt_chandle_case_equal(void* lhs, void* rhs) -> void*;
auto lyra_rt_chandle_to_bool(void* operand) -> bool;

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
auto lyra_rt_dynarray_element(const void* array, const void* index) -> void*;
auto lyra_rt_dynarray_with_element(
    const void* array, const void* index, void* value) -> void*;
auto lyra_rt_dynarray_delete(const void* array) -> void*;
auto lyra_rt_dynarray_size(const void* array) -> void*;
auto lyra_rt_dynarray_eq(const void* lhs, const void* rhs) -> void*;
auto lyra_rt_dynarray_ne(const void* lhs, const void* rhs) -> void*;
auto lyra_rt_dynarray_case_equal(const void* lhs, const void* rhs) -> void*;
auto lyra_rt_dynarray_cell_alloc() -> void*;
auto lyra_rt_dynarray_cell_get(void* cell) -> void*;
void lyra_rt_dynarray_cell_initialize(void* cell, const void* prototype);
void lyra_rt_dynarray_cell_set(void* cell, const void* value);
auto lyra_rt_dynarray_value_cell_alloc() -> void*;
void lyra_rt_dynarray_value_cell_store(void* cell, const void* value);
auto lyra_rt_dynarray_value_cell_load(const void* cell) -> void*;

// A fixed-size unpacked array (LRM 7.4.2). Its payload is ordinal-only: the
// declared range is the receiver's static type's, so every coordinate-consuming
// entry takes it as a `[left:right]` operand pair rather than reading it off
// the value.
auto lyra_rt_unpackedarray_from_literal(
    void* prototype, LyraSpan unit, std::int64_t count) -> void*;
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
auto lyra_rt_unpackedarray_value_cell_alloc() -> void*;
void lyra_rt_unpackedarray_value_cell_store(void* cell, const void* value);
auto lyra_rt_unpackedarray_value_cell_load(const void* cell) -> void*;

// Nets and their drivers (LRM 6.5, 6.6). A net is storage of its own, like a
// cell: `net_initialize` fixes its declared type once, and `net_get` answers
// with the fold of its drivers' contributions. It takes no store -- a value
// reaches a net only through a driver.
//
// `attach_driver` issues one, and the handle it answers with is the net's to
// own, so a source may hold it for as long as the net lives. `driver_set`
// publishes that driver's whole contribution, after which the net re-resolves
// and wakes its subscribers only on a real change; `driver_get` reads the
// contribution back, which is what a source driving part of a net updates part
// of and leaves the rest of at high impedance (LRM 6.6.1).
//
// LRM 6.7.1 fixes which domains these exist for: a 4-state integral net, and a
// fixed-size unpacked array, struct, or union whose elements are themselves
// valid for a net.
auto lyra_rt_packed_net_get(void* net) -> void*;
void lyra_rt_packed_net_initialize(void* net, const void* prototype);
auto lyra_rt_packed_attach_driver(void* net) -> void*;
auto lyra_rt_packed_driver_get(void* driver) -> void*;
void lyra_rt_packed_driver_set(void* driver, const void* value);
auto lyra_rt_tuple_net_get(void* net) -> void*;
void lyra_rt_tuple_net_initialize(void* net, const void* prototype);
auto lyra_rt_tuple_attach_driver(void* net) -> void*;
auto lyra_rt_tuple_driver_get(void* driver) -> void*;
void lyra_rt_tuple_driver_set(void* driver, const void* value);
auto lyra_rt_union_net_get(void* net) -> void*;
void lyra_rt_union_net_initialize(void* net, const void* prototype);
auto lyra_rt_union_attach_driver(void* net) -> void*;
auto lyra_rt_union_driver_get(void* driver) -> void*;
void lyra_rt_union_driver_set(void* driver, const void* value);
auto lyra_rt_unpackedarray_net_get(void* net) -> void*;
void lyra_rt_unpackedarray_net_initialize(void* net, const void* prototype);
auto lyra_rt_unpackedarray_attach_driver(void* net) -> void*;
auto lyra_rt_unpackedarray_driver_get(void* driver) -> void*;
void lyra_rt_unpackedarray_driver_set(void* driver, const void* value);

// The queue domain (LRM 7.10): a run-time-sized ordered container whose
// elements are added and removed at either end, carried behind an opaque handle
// and owning its elements by value. `default` and `from_literal` mirror the
// dynamic array's constructors, and each has a bounded form because a declared
// bound (LRM 7.10.5) is a value the constructor takes rather than one it can
// derive. The bound belongs to the variable, not to the value written, so
// `conform_bound` is what a semantic store into a bounded queue passes its
// right-hand side through. An element write appends when its index is the
// queue's size and is discarded at any other invalid index (LRM 7.10.1); every
// apparent mutation -- an element write, a push, an insert, a delete -- yields
// a new queue rather than writing in place, so value semantics hold even when
// the queue is shared.
auto lyra_rt_queue_default(void* prototype) -> void*;
auto lyra_rt_queue_default_bounded(void* prototype, const void* max_bound)
    -> void*;
auto lyra_rt_queue_from_literal(
    void* prototype, LyraSpan unit, std::int64_t count) -> void*;
auto lyra_rt_queue_from_literal_bounded(
    void* prototype, LyraSpan unit, std::int64_t count, const void* max_bound)
    -> void*;
auto lyra_rt_queue_conform_bound(const void* queue, const void* max_bound)
    -> void*;
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
auto lyra_rt_queue_value_cell_alloc() -> void*;
void lyra_rt_queue_value_cell_store(void* cell, const void* value);
auto lyra_rt_queue_value_cell_load(const void* cell) -> void*;

// The associative-array domain (LRM 7.8): a sparse lookup table allocated entry
// by entry and held in index order, carried behind an opaque handle. Its
// element default is what a read of an index with no entry yields (LRM 7.8.6),
// and it crosses erased at construction like every other container's. An index
// crosses erased too, and for a reason of its own: the array holds no prototype
// for one, so nothing here could know the representation the program wrote it
// in. An element beside an index still crosses bare, since the element default
// names its domain. Every apparent mutation yields a new array rather than
// writing in place, so value semantics hold even when the array is shared.
auto lyra_rt_assocarray_default(void* prototype) -> void*;
auto lyra_rt_assocarray_from_entries(void* prototype, LyraSpan entries)
    -> void*;
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
// largest index the array holds. An array with no entries reports the value the
// call supplied for that case, which is the caller's own handle -- a value
// handle is immutable, so answering with it aliases nothing observable.
auto lyra_rt_assocarray_assoc_min_index(const void* array, void* empty)
    -> void*;
auto lyra_rt_assocarray_assoc_max_index(const void* array, void* empty)
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
auto lyra_rt_make_format_spec_of_kind(const void* kind) -> void*;
auto lyra_rt_make_format_spec(
    const void* kind, const void* width, const void* precision,
    const void* zero_pad, const void* left_align, const void* timeunit_power)
    -> void*;
auto lyra_rt_packed_make_print_value_item(const void* value, const void* spec)
    -> void*;
auto lyra_rt_string_make_print_value_item(const void* value, const void* spec)
    -> void*;
}
