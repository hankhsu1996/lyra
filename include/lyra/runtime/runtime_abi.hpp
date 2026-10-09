#pragma once

#include <cstdint>

// The execution-strategy-neutral ABI the generated module calls. Every runtime
// value crosses as an opaque pointer. The runtime owns its type; its storage
// belongs to whoever made the value, which also ends it.
//
// A `bool` is never one of those values. It is a machine predicate -- a
// condition read off a value, a question about the running execution, a
// constant of the program the entry is told rather than shown -- so it carries
// no width and no unknown state. Every other answer is a handle or nothing at
// all.
//
// A value crosses erased exactly where it states a representation the entry
// receiving it has no other way to know -- the member a union is to hold, a
// container's element prototype, an associative array's index. It then crosses
// as two pointers, where it lies and then the type it is a value of, the pair a
// Rust trait-object reference and a Swift existential carry, and is borrowed
// for the call. A value that conforms to a representation its entry already
// fixes crosses as the bare handle of its own domain instead.
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

auto lyra_rt_current_runtime() noexcept -> void*;
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
auto lyra_rt_file_open(void* files, const void* name, void* out) -> void*;
auto lyra_rt_file_open_mode(
    void* files, const void* name, const void* mode, void* out) -> void*;
void lyra_rt_file_close(void* files, const void* descriptor);
auto lyra_rt_file_getc(void* files, const void* fd, void* out) -> void*;
auto lyra_rt_file_ungetc(void* files, const void* c, const void* fd, void* out)
    -> void*;
// A read that answers through an argument the call names completes with how
// many bytes it read and the destination those bytes filled (LRM 21.3.4.2,
// 21.3.4.4, 21.3.7). A binary read is handed the destination as well, because
// its shape decides how much is read and what the file does not reach keeps
// what it held; reading into a packed variable and reading into a memory are
// two requests, and a memory's bounds and window reach the second as operands
// of their own.
auto lyra_rt_file_gets(void* files, const void* fd, void* out) -> void*;
auto lyra_rt_file_error(void* files, const void* fd, void* out) -> void*;
auto lyra_rt_file_read(void* files, const void* dest, const void* fd, void* out)
    -> void*;
auto lyra_rt_file_read_memory(
    void* files, const void* dest, const void* fd, const void* declared,
    const void* start, const void* count, void* out) -> void*;
auto lyra_rt_file_seek(
    void* files, const void* fd, const void* offset, const void* operation,
    void* out) -> void*;
auto lyra_rt_file_rewind(void* files, const void* fd, void* out) -> void*;
auto lyra_rt_file_tell(void* files, const void* fd, void* out) -> void*;
auto lyra_rt_file_eof(void* files, const void* fd, void* out) -> void*;
void lyra_rt_file_flush(void* files, const void* descriptor);
void lyra_rt_file_flush_all(void* files);

// The bytes a scan may read without consuming them, and the commit of how many
// it used (LRM 21.3.4.3). A scan parses out of what it can see and only then
// says how far it got, so looking and consuming are two operations rather than
// one read that has to guess the length first.
auto lyra_rt_peek_buffered(void* files, const void* fd, void* out) -> void*;
void lyra_rt_advance_fd(void* files, const void* fd, const void* count);

// The joint cancel state of the channels a descriptor names (LRM 21.3.2), built
// in storage the caller gives. A deferred write snapshots it so the write
// short-circuits if any of those channels is closed before the region that
// performs it runs.
auto lyra_rt_cancellation_for(void* files, const void* descriptor, void* out)
    -> void*;

// Whether any channel that cancel state covers has been closed since it was
// taken (LRM 21.3.2), as an opaque packed value like every scalar.
auto lyra_rt_is_cancelled(const void* cancellation, void* out) -> void*;

auto lyra_rt_string_make(void* cstr, void* out) -> void*;
auto lyra_rt_make_print_literal_item(void* string_value, void* out) -> void*;
auto lyra_rt_format(LyraSpan items, const void* time_format, void* out)
    -> void*;
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
// captures, because the body runs after the one that built them has returned
// (LRM 9.3.2).
auto lyra_rt_enter_coroutine_borrowed_environment(
    void* frame, void* out) noexcept -> void*;
auto lyra_rt_enter_coroutine_owned_environment(
    void* closure, void* out) noexcept -> void*;

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

void lyra_rt_enter_scope_static_init(void* runtime, void* unit_instance);
void lyra_rt_enter_namespace_static_init(void* runtime);
void lyra_rt_leave_static_init(void* runtime) noexcept;

void lyra_rt_enter_dpi_scope(void* runtime, void* decl_scope);
void lyra_rt_leave_dpi_scope(void* runtime) noexcept;

// The DPI-C disable protocol (LRM 35.9). The question an exported task's entry
// answers as its int, and the three checks the clause makes the simulator's.
// None of them raises: each stands in a frame foreign code reached, so what it
// does is report and end the run.
auto lyra_rt_disable_is_active(void* runtime) -> std::int32_t;
void lyra_rt_check_import_task_acknowledged(
    void* runtime, std::int32_t returned);
void lyra_rt_check_import_function_acknowledged(void* runtime);
void lyra_rt_check_export_reachable(void* runtime);

auto lyra_rt_claim_namespace_initialize(void* runtime, const char* name)
    -> std::int64_t;

// What one program-global export symbol resolves against (LRM 35.4, 35.5.3):
// the scope the foreign call chain currently holds, and the entry that scope
// publishes under the exported name. The symbol takes only the C arguments its
// prototype states, so it recovers both from the run rather than from its
// caller. The entry answers as a code address with its prototype erased, which
// the call site restores to the one it was compiled against.
auto lyra_rt_current_export_scope() -> void*;
auto lyra_rt_find_export_entry(void* scope, const void* subroutine)
    -> LyraMethodEntry;

// The two directions of a DPI-C task across the boundary. Going out, the call
// is carried on a stack of the runtime's own so an exported task it reaches can
// suspend while simulation time advances (LRM 35.5.1.1); it is what an import
// task's suspend edge is preceded by, so it answers whether the caller must
// give up control, as a stop at a wait does. Coming in, a foreign caller is not
// a coroutine and cannot await (LRM 35.8), so the entry it called drives the
// body to completion on the stack that call is running on, and what the body
// completes with lands in the storage its caller supplied rather than coming
// back through the call.
auto lyra_rt_run_foreign_task_on_fiber(void* runtime, void* closure) -> bool;
void lyra_rt_run_exported_task_to_completion(void* activation);

// LRM 9.3.2 Table 9-1. Each takes the branches one `fork` spawned, in source
// order, and hands them to the engine, which does not run any of them until the
// spawning process blocks or terminates. `spawn_all` is `join_none`, whose
// parent never waits and so answers nothing; the other two answer the parent's
// wait for its join, built in storage the caller gives.
void lyra_rt_spawn_all(void* runtime, LyraSpan branches);
auto lyra_rt_fork_wait_all(void* runtime, LyraSpan branches, void* out)
    -> void*;
auto lyra_rt_fork_wait_first(void* runtime, LyraSpan branches, void* out)
    -> void*;

// LRM 9.6.1 `wait fork` and 9.6.3 `disable fork`. Both read the executing
// process, so neither names the children it reaches. `wait fork` answers the
// wait for every immediate child to have terminated; `disable fork` never
// blocks.
auto lyra_rt_wait_fork(void* runtime, void* out) -> void*;
void lyra_rt_disable_fork(void* runtime);

// LRM 9.7 process control. The receiver is a handle of the managed-reference
// domain naming a process node; `self` builds one for the calling process,
// which the engine already owns, so nothing is constructed here. `await` is the
// one that waits, and it answers the wait for the target to terminate, which
// the caller then stops at.
auto lyra_rt_process_self(void* runtime, void* out) -> void*;
auto lyra_rt_process_status(const void* self, void* out) -> void*;
void lyra_rt_process_kill(const void* self, void* runtime);
auto lyra_rt_process_await(const void* self, void* runtime, void* out) -> void*;
void lyra_rt_process_suspend(const void* self, void* runtime);
void lyra_rt_process_resume(const void* self, void* runtime);

// Builds a callable the runtime runs later: `definition` is an opaque
// cross-artifact reference naming both the body and how the storage its
// captures live in is laid out. The closure is made once and never moves; what
// the caller's storage receives is its owner, which is what something
// outliving the caller's statement takes from there: a region a deferred
// effect is submitted to, the coroutine a spawned branch is entered as, or an
// observation of the value it answers. An array method running a per-element
// body borrows it for the call. It answers the closure itself, whose captures
// the code building it fills where it laid them out in it.
auto lyra_rt_closure_make(const void* definition, void* out) -> void*;

// The handle the program's `new` answers (LRM 8.3), owning `object`: a whole
// value of a class extending the part every object starts with, allocated and
// constructed by the caller and held by nothing yet. The handle ends the value
// through its virtual destructor when the last reference to it goes, and is
// built in storage the caller gives like every value the boundary hands back.
auto lyra_rt_object_adopt(void* object, void* out) -> void*;

// A counted hold on a new, empty cell of the named domain -- what a block's
// local lives in when a branch the block spawns can outlive it (LRM 6.21). The
// hold is built in storage the frame that names it gives, and the cell ends
// with the last hold on it.
auto lyra_rt_packed_shared_cell_make(void* out) -> void*;
auto lyra_rt_string_shared_cell_make(void* out) -> void*;
auto lyra_rt_real_shared_cell_make(void* out) -> void*;
auto lyra_rt_shortreal_shared_cell_make(void* out) -> void*;
auto lyra_rt_chandle_shared_cell_make(void* out) -> void*;
auto lyra_rt_managedref_shared_cell_make(void* out) -> void*;
auto lyra_rt_tuple_shared_cell_make(void* out) -> void*;
auto lyra_rt_union_shared_cell_make(void* out) -> void*;
auto lyra_rt_tagged_union_shared_cell_make(void* out) -> void*;
auto lyra_rt_dynarray_shared_cell_make(void* out) -> void*;
auto lyra_rt_unpackedarray_shared_cell_make(void* out) -> void*;
auto lyra_rt_queue_shared_cell_make(void* out) -> void*;
auto lyra_rt_assocarray_shared_cell_make(void* out) -> void*;

// Where the cell a hold names lies. A hold is a value rather than the address
// of what it names, exactly as a class handle is, so reaching the storage
// behind one is an operation.
auto lyra_rt_shared_pointer_deref(void* handle) -> void*;

// The part of the object a class handle refers to that the class it was formed
// as reaches, or null for a handle referring to no object. Forming the handle
// of another class is generated code's own, since where each part of an object
// sits is fixed where the object's class is compiled.
auto lyra_rt_handle_view(const void* handle) -> void*;

// A handle referring to the object `handle` refers to, through the part at
// `view`; one referring to no object where `view` is null, which is how a
// conversion that found no such part answers.
auto lyra_rt_handle_with_view(const void* handle, void* view, void* out)
    -> void*;

// The part of the object a class handle reaches it through (LRM 8.3), which is
// what a member access is applied to. Which object that is, is a fact the
// handle holds rather than is, so reaching it is an operation; a handle
// referring to no object fails the run here rather than further in.
auto lyra_rt_view_of(const void* handle) -> void*;

// The handle referring to the object a body runs on (LRM 8.11), given the
// object's start, which is also the part a handle of any class of its lineage
// reaches it through.
auto lyra_rt_self_handle(void* self, void* out) -> void*;

// What reports a change to an object's properties (LRM 9.4.2), each taking the
// object as the root every object shares: the source a wait on the object
// enrols on, and a write into its properties, opened on the object alone
// in storage the writing body gives, which answers the object the properties
// are reached through and whose end tells the object.
auto lyra_rt_object_event_source(void* object) -> void*;
auto lyra_rt_open_object_write(void* object, void* out) -> void*;
auto lyra_rt_written_object(const void* write) -> void*;

// What an enumeration's member list answers about a value (LRM 6.19.5,
// 6.24.2). Whether it is a member crosses as the machine integer every computed
// answer crosses as; a host `bool` here would say the call parks its caller.
auto lyra_rt_enumeration_has(const void* enumeration, const void* value)
    -> std::int64_t;
auto lyra_rt_enumeration_name(
    const void* enumeration, const void* value, void* out) -> void*;
auto lyra_rt_enumeration_next(
    const void* enumeration, const void* value, const void* count, void* out)
    -> void*;
auto lyra_rt_enumeration_prev(
    const void* enumeration, const void* value, const void* count, void* out)
    -> void*;

// Hands a callable to the region that will run it (LRM 4.4): the write a
// non-blocking assignment defers, the print a `$strobe` postpones, and the
// report a deferred assertion leaves for the observed region. Each takes
// ownership of the closure, which is what lets the closure outlive the body
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
// slot it landed in: an execution the building body made in its own frame,
// taken the way a fork branch is. It runs apart from every lineage, being an
// update the standard makes no process of.
void lyra_rt_run_detached(void* runtime, void* carrier);

// The wait for the region that update is due in, which the carrier stops at
// once the event has named the slot (LRM 4.4.2.4).
auto lyra_rt_resume_in_nba_region(void* out) -> void*;

// The wait of a delay (LRM 9.4.1): `duration` steps of its scope's time unit
// (`unit_power`), from now. The runtime rounds that amount to the scope's
// precision (`precision_power`) and scales it to the engine's global tick; a
// zero wait lands on the current slot's inactive region. The counts cross as
// opaque packed values, like every scalar.
auto lyra_rt_delay(
    void* runtime, const void* duration, const void* unit_power,
    const void* precision_power, void* out) -> void*;

// The same for a delay the design wrote as a real expression, which can name a
// fraction of a time unit and is rounded to the precision (LRM 3.14.1).
auto lyra_rt_delay_real(
    void* runtime, const void* duration, const void* unit_power,
    const void* precision_power, void* out) -> void*;

// Builds one leaf of a wait: the place it watches, what decides whether what
// happens there is an event -- which the leaves watching for one event share --
// and which bits of that place's packed encoding it reads, as a
// `(lsb_bit_offset, bit_width)` pair, a width of zero being the whole of it and
// what a named event's leaf carries. The scalars cross as opaque packed values,
// like every scalar. The leaf is built in storage the caller gives.
auto lyra_rt_make_trigger(
    void* observable, const void* observation, const void* lsb_bit_offset,
    const void* bit_width, void* out) -> void*;

// What decides whether reaching a wait is an event for it (LRM 9.4.2). Two
// halves, and one entry per combination of them, so the call states which form
// it is building. A watched half is a closure answering what the event
// expression is worth now together with the edge specifier written on it,
// crossing as an opaque packed value like every scalar. A qualifying half is an
// `iff` condition, answering as a one-bit value already reduced to LRM 12.4
// truth (LRM 9.4.2.3). Watching nothing is what an implicit sensitivity carries
// (LRM 9.2.2.2.1) and what an unqualified named-event wait carries, the trigger
// there being the event itself (LRM 15.5.1). Like a trigger these are
// transient, and the waits built from them hold them for as long as they last.
//
// Building one evaluates nothing. The wait holding it arms it with what the
// expression is worth where the wait begins, and the waiting process asks it
// where the process decides -- the answer crossing as the machine integer
// every computed answer crosses as, since a host `bool` here would say the
// call parks its caller.
auto lyra_rt_observation_on_reaching(void* out) -> void*;
auto lyra_rt_observation_of_value(void* expression, const void* edge, void* out)
    -> void*;
auto lyra_rt_observation_of_value_qualified(
    void* expression, const void* edge, void* condition, void* out) -> void*;
auto lyra_rt_observation_qualified(void* condition, void* out) -> void*;
auto lyra_rt_observation_fires(const void* observation) -> std::int64_t;

// The waits on the places an evaluation the process made reached, one read
// report per expression evaluated: an event control its process decides, with
// the observations that decide it, and a `wait (cond)` whose loop tests the
// condition (LRM 9.4.2, 9.4.3). The two part company where a stopped process is
// started again: the condition is the loop's to read, while an event control
// waits for the next occurrence (LRM 9.7). Each takes what its reports hold,
// leaving them empty for the next evaluation.
auto lyra_rt_wait_recollecting(
    LyraSpan reports, LyraSpan observations, void* out) -> void*;
auto lyra_rt_wait_until(LyraSpan reports, void* out) -> void*;

// A wait for what happens at one of `triggers` to be an event for it (LRM
// 9.4.2 / 9.4.2.2 / 15.5.2), or at one leaf of the implicit list a report
// settled (LRM 9.2.2.2.1). An empty span means "never wake up".
auto lyra_rt_wait_on(LyraSpan triggers, void* out) -> void*;
auto lyra_rt_wait_on_implicit_list(const void* report, void* out) -> void*;

// Every wait above is built in storage the caller gives, and every stop is
// this, at one of them, answering whether the caller must give up control.
// Which execution is waiting is the runtime's own to know, so no token crosses
// the boundary.
auto lyra_rt_park_at(void* runtime, void* wait) -> bool;

// What an evaluation states the places it reached in: an empty report; a place
// read and the bits of it read; a place reached through a handle; every object
// at once; the bracket around a call made on a handle; a place written;
// settling it as a procedure's implicit list once all is reported; the bracket
// a function reporting into it takes, which answers one or zero for whether it
// goes on; and one or zero for whether the function then runs. A function
// meeting a read no leaf watches yet refuses the report, and the design fails
// there.
auto lyra_rt_read_report_empty(void* out) -> void*;
auto lyra_rt_read_report_for_implicit_list(void* out) -> void*;
void lyra_rt_read_report_add(
    void* report, void* place, const void* lsb_bit_offset,
    const void* bit_width);
void lyra_rt_read_report_add_through_handle(
    void* report, void* place, const void* lsb_bit_offset,
    const void* bit_width);
void lyra_rt_read_report_enter_call_on_handle(void* report);
void lyra_rt_read_report_leave_call_on_handle(void* report);
void lyra_rt_read_report_add_every_object(void* report);
void lyra_rt_read_report_add_write(
    void* report, void* place, const void* lsb_bit_offset,
    const void* bit_width);
void lyra_rt_read_report_settle_as_implicit_list(void* report);
auto lyra_rt_read_report_enter(void* report) -> std::int64_t;
void lyra_rt_read_report_leave(void* report);
auto lyra_rt_read_report_runs_the_body(const void* report) -> std::int64_t;
void lyra_rt_refuse_report(const void* why);

// A named event (LRM 15.5). Triggering records the instant and ends the wait of
// every process the trigger is an event for; waiting for one is an ordinary
// wait naming the event, since the event is a place a wait enrols on like
// any other. `triggered` answers whether the most recent trigger happened in
// this time step, which is a comparison of instants rather than a state the
// event clears.
void lyra_rt_trigger(void* event, void* runtime);
auto lyra_rt_triggered(const void* event, void* runtime, void* out) -> void*;

// Takes a copy of a value the run then holds by address from here on. Every
// value crossing this boundary lives in storage the generated body gave it and
// ends with the evaluation that made it; a constant is built once and read for
// the rest of the run, so generated code hands the built value here before
// keeping an address. It is an entry of this target's own ABI rather than an
// operation any layer above states: the lifetime it answers exists because
// values cross here by address.
auto lyra_rt_retain_constant(const void* value) -> const void*;

// LRM 9.6.2 `disable`. A target crosses as its address, and a control effect as
// the target it names, since that is all one carries.
//
// The two brackets record on the running process which targets its execution is
// inside, and the generation each held on entry; `lyra_rt_disable` advances the
// named target's generation and wakes the executions blocked inside it, and
// leaves who lands where to each of them. The two queries answer whether this
// execution has to leave where it stands and which target its departure names
// -- the first computed from the generations and from a termination this
// execution has yet to unwind for, the second from the generations alone, so
// nothing is stored and nothing has to be cleared. A departure naming no target
// is one no region may claim. A body asks them where it regains control,
// because a simulated process cannot be made to run code partway through a
// statement.
void lyra_rt_enter_target(void* runtime, void* target);
void lyra_rt_leave_target(void* runtime, void* target) noexcept;
void lyra_rt_disable(void* target, void* runtime);
auto lyra_rt_effect_names_target(void* effect, void* target, void* out) noexcept
    -> void*;
void lyra_rt_take_departure_if_due(void* runtime);

// A departure that arrived at a landing, in the steps the platform's unwinding
// protocol takes. A landing stops whatever the platform is carrying, and
// receiving turns it into the departure it carries -- a run-time error is
// settled there and becomes the departure no region may claim -- and answers
// the target that departure names, which is what the landing tests; anything
// else is carried on from inside the receive, unchanged. Finishing releases
// the departure, which a landing does when it continues past its own region;
// declining carries the one the landing holds on outward. Settling hands that
// one to the activation a suspendable body completes instead, which is how such
// a body is left by one: it then completes as it would by returning, and
// whoever drives it carries the departure on. They are entries of this ABI
// rather than calls a body makes for itself, so generated code names no
// unwinding symbol and no raised type, and each target's own protocol stays
// inside the runtime.
auto lyra_rt_receive_departure(void* exception) -> void*;
void lyra_rt_finish_departure();
[[noreturn]] void lyra_rt_decline_departure(void* target);
void lyra_rt_settle_departure(void* target);

// Reads the current simulation time, scaled to the time unit of the design
// element the call sits in (LRM 20.3). That unit is the caller's property
// rather than the runtime's, so its power of ten crosses as an opaque packed
// value, like every scalar, and so do the first two answers; the third is an
// opaque real, keeping whatever fraction of a unit the instant falls on.
auto lyra_rt_sim_time(void* runtime, const void* unit_power, void* out)
    -> void*;
auto lyra_rt_stime(void* runtime, const void* unit_power, void* out) -> void*;
auto lyra_rt_realtime(void* runtime, const void* unit_power, void* out)
    -> void*;

// Records a request to tear the simulation down once the current time slot
// completes, prints what the level selects about it (LRM 20.2, Table 20-1), and
// departs from the calling execution, so neither returns. The origin crosses as
// an opaque string value and the level as an opaque packed value, like every
// scalar.
[[noreturn]] void lyra_rt_finish(
    void* runtime, const void* origin, const void* level);
[[noreturn]] void lyra_rt_stop(
    void* runtime, const void* origin, const void* level);

// Runs a command line through the host's command processor and yields what it
// answered; the null form runs nothing and yields whether a command processor
// exists at all (LRM 20.17.1). The command crosses as an opaque string value
// and the answer as an opaque packed value, like every scalar.
auto lyra_rt_run_host_command(void* runtime, const void* command, void* out)
    -> void*;
auto lyra_rt_run_null_host_command(void* out) -> void*;

// Whether the simulation's own arguments carry a plusarg with the given prefix
// (LRM 21.6). Those arguments are the runtime's, so only the prefix crosses, as
// an opaque string; the answer is an opaque packed value, like every scalar.
auto lyra_rt_test_plusargs(void* runtime, const void* user_string, void* out)
    -> void*;

// The value a plusarg carries, converted as the user string's format specifier
// asks (LRM 21.6). It completes with whether one matched and the value the
// destination now holds; the destination crosses in because a miss leaves it as
// it was and its size decides how a match is fitted, and the entry is named by
// the representation that destination takes.
auto lyra_rt_packed_value_plusargs(
    void* runtime, const void* user_string, const void* destination, void* out)
    -> void*;
auto lyra_rt_string_value_plusargs(
    void* runtime, const void* user_string, const void* destination, void* out)
    -> void*;

// Draws from the calling process's generator (LRM 18.13.1 -- 18.13.2). The
// generator is the running process's, read from the runtime, so none crosses
// the boundary; the seed and the two bounds cross as opaque packed values, as
// every scalar does, and so does the result.
auto lyra_rt_urandom(void* runtime, void* out) -> void*;
auto lyra_rt_urandom_seeded(void* runtime, const void* seed, void* out)
    -> void*;
auto lyra_rt_urandom_range(
    void* runtime, const void* maxval, const void* minval, void* out) -> void*;

// `$random` with no seed (LRM 20.14.1): the same process draw, read signed.
auto lyra_rt_random(void* runtime, void* out) -> void*;

// Draws by the algorithm LRM Annex N states (LRM 20.14.2). The seed is the
// whole state, so no runtime crosses the boundary; each answers with a product
// of the value drawn and the seed that draw advanced, which the caller stores
// back into the design's own seed variable.
auto lyra_rt_dist_uniform(
    const void* seed, const void* start, const void* end, void* out) -> void*;
auto lyra_rt_dist_normal(
    const void* seed, const void* mean, const void* standard_deviation,
    void* out) -> void*;
auto lyra_rt_dist_exponential(const void* seed, const void* mean, void* out)
    -> void*;
auto lyra_rt_dist_poisson(const void* seed, const void* mean, void* out)
    -> void*;
auto lyra_rt_dist_chi_square(
    const void* seed, const void* degrees_of_freedom, void* out) -> void*;
auto lyra_rt_dist_t(const void* seed, const void* degrees_of_freedom, void* out)
    -> void*;
auto lyra_rt_dist_erlang(
    const void* seed, const void* stages, const void* mean, void* out) -> void*;

// Builds a scope's structural identity from its base label and per-dimension
// indices (a span of 32-bit index values, empty for a scalar), in storage the
// caller gives.
auto lyra_rt_make_segment(void* label, LyraSpan indices, void* out) -> void*;

// The scope's hierarchical name (LRM 21.2.1.5; the `%m` source), as a string
// built in storage the caller gives.
auto lyra_rt_hierarchical_path(void* self, void* out) -> void*;

// The scope one step out. A name written in a generate block and declared in
// the module around it is reached by climbing to that scope and reading the
// member there, which the referring artifact can do directly because it owns
// the enclosing scope's layout.
auto lyra_rt_parent(void* self) -> void*;

// Attaches a freshly built child to its parent, transferring ownership into the
// runtime tree; returns the child as a borrowed scope handle.
auto lyra_rt_add_owned_child(void* parent, void* child) -> void*;

// Where a hierarchical name leaving the instance `self` stands in starts (LRM
// 23.6 / 23.8): the nearest scope above it of the class `definition` describes,
// or past the topmost a top-level instance of it.
auto lyra_rt_enclosing_scope(void* self, const void* definition) -> void*;

// Whether the scope `self` is an object of the class `definition` describes or
// of one extending it.
auto lyra_rt_is_of_class(void* self, const void* definition) -> bool;

// The sequence of handles a declaration standing for several objects builds,
// in the order its coordinates count, and the handle at a position in one. A
// sequence is held by address for the rest of the run, which is what lets a
// dimension of a multidimensional declaration be an ordinary handle in the
// dimension above it. A declaration counts its objects out as it builds them,
// so a sequence starts from the handles already in hand and is extended once
// per object after that; what the owner keeps is the last answer, and until it
// does nothing else names the sequence being extended.
auto lyra_rt_sequence_make(LyraSpan handles) -> void*;
auto lyra_rt_sequence_extend(void* sequence, void* element) -> void*;
auto lyra_rt_sequence_element(const void* sequence, std::int64_t index)
    -> void*;

// Where a program starts, answering its exit status: the arguments it was
// started with, the entry its design root's unit builds its object through --
// given the scope it is built under and the identity it is reached by, an
// instance built with `new` that this library then owns -- and the label the
// root carries as the artifact's own NUL-terminated constant.
auto lyra_rt_run_program(
    std::int32_t argc, char** argv, void* (*make)(void*, const void*),
    const void* name) -> std::int32_t;

// A reference, built in the storage the caller gives: where a value lies, and
// what holds that storage, if anyone is told about it. A body holding a
// reference is lowered once for every caller and cannot ask what it was lent
// (LRM 13.5.2), while a write through one has to wake whoever waits on the
// variable or the object holding the storage at the moment it lands (LRM 4.3,
// 9.4.2), so the holder travels with the reference. One over a subscribable
// variable's cell names the whole of it, and is named by the domain the cell
// holds since where the value lies inside the cell is the cell's type's to
// say; one over storage nothing is told about is handed the value's handle and
// names nothing; and one over a class property is held by the object and given
// the property's storage. A property reached through a handle naming no object
// is the design's own failure (LRM 8.4).
auto lyra_rt_refer_storage(void* storage, void* out) -> void*;
auto lyra_rt_refer_property(void* object, void* storage, void* out) -> void*;
// What a wait on the storage a reference names enrols on: the variable or
// the object's event source, and null for storage nothing is told about.
auto lyra_rt_reference_reports_to(const void* reference) -> void*;
auto lyra_rt_packed_cell_refer(void* cell, void* out) -> void*;
auto lyra_rt_string_cell_refer(void* cell, void* out) -> void*;
auto lyra_rt_real_cell_refer(void* cell, void* out) -> void*;
auto lyra_rt_shortreal_cell_refer(void* cell, void* out) -> void*;
auto lyra_rt_chandle_cell_refer(void* cell, void* out) -> void*;
auto lyra_rt_managedref_cell_refer(void* cell, void* out) -> void*;
auto lyra_rt_tuple_cell_refer(void* cell, void* out) -> void*;
auto lyra_rt_union_cell_refer(void* cell, void* out) -> void*;
auto lyra_rt_tagged_union_cell_refer(void* cell, void* out) -> void*;
auto lyra_rt_dynarray_cell_refer(void* cell, void* out) -> void*;
auto lyra_rt_unpackedarray_cell_refer(void* cell, void* out) -> void*;
auto lyra_rt_queue_cell_refer(void* cell, void* out) -> void*;
auto lyra_rt_assocarray_cell_refer(void* cell, void* out) -> void*;

// A step a reference takes into an element or a component of what it names
// (LRM 13.5.2), answering with a reference to that part in the storage the
// caller gives. The part belongs to the variable the reference it was taken on
// belongs to; forming an element can change that variable (LRM 7.8.7), and the
// variable is told so where the step is taken.
auto lyra_rt_dynarray_refer_element(
    const void* reference, const void* index, void* out) -> void*;
auto lyra_rt_unpackedarray_refer_element(
    const void* reference, const void* position, void* out) -> void*;
auto lyra_rt_queue_refer_element(
    const void* reference, const void* index, void* out) -> void*;
auto lyra_rt_assocarray_refer_element(
    const void* reference, const void* index, const void* index_type, void* out)
    -> void*;
auto lyra_rt_tuple_refer_component(
    const void* reference, std::int64_t index, void* out) -> void*;

// Observable storage cell operations, reached through the cell's address. The
// entry names the cell's value domain; the runtime never inspects a type tag.
// A read of what a cell, a net, a driver or a reference holds answers with the
// value where it lies; a reader copies it only to keep it.
//
// `arm_sampling` and `sampled_load` are the same access to the same storage,
// differing only in which of the two values a cell holds answers: the current
// one, or the one the current time slot found there before anything in it ran
// (LRM 4.4.2.1, 16.5.1). Only an armed cell keeps the second, so a cell nothing
// samples carries neither the storage nor the work of maintaining it.
auto lyra_rt_packed_cell_get(void* cell) -> const void*;
void lyra_rt_packed_cell_initialize(void* cell, const void* prototype) noexcept;
void lyra_rt_packed_cell_set(void* cell, const void* value);
void lyra_rt_packed_cell_arm_sampling(void* cell);
auto lyra_rt_packed_cell_sampled_load(void* cell, void* out) -> void*;
// Putting a cell under a procedural continuous assignment and taking it back
// out (LRM 10.6). Beginning one answers with the generation the evaluation
// driving it carries; driving answers whether that evaluation is still the one
// in effect, which is what stops one a later takeover superseded.
auto lyra_rt_packed_cell_begin_takeover(
    void* cell, const void* level, void* out) -> void*;
auto lyra_rt_packed_cell_drive_takeover(
    void* cell, const void* level, const void* generation, const void* value)
    -> bool;
void lyra_rt_packed_cell_end_takeover(void* cell, const void* level);
// Reading and writing storage a caller lent, which answers through the form
// the reference carries: a watchable variable's own access, or a plain read
// and write where nothing can watch.
auto lyra_rt_packed_ref_get(void* reference) -> const void*;
void lyra_rt_packed_ref_set(void* reference, const void* value);
void lyra_rt_packed_ref_arm_sampling(void* reference);
auto lyra_rt_packed_ref_sampled_load(void* reference, void* out) -> void*;
auto lyra_rt_string_ref_get(void* reference) -> const void*;
void lyra_rt_string_ref_set(void* reference, const void* value);
void lyra_rt_string_ref_arm_sampling(void* reference);
auto lyra_rt_string_ref_sampled_load(void* reference, void* out) -> void*;
auto lyra_rt_real_ref_get(void* reference) -> const void*;
void lyra_rt_real_ref_set(void* reference, const void* value);
void lyra_rt_real_ref_arm_sampling(void* reference);
auto lyra_rt_real_ref_sampled_load(void* reference, void* out) -> void*;
auto lyra_rt_shortreal_ref_get(void* reference) -> const void*;
void lyra_rt_shortreal_ref_set(void* reference, const void* value);
void lyra_rt_shortreal_ref_arm_sampling(void* reference);
auto lyra_rt_shortreal_ref_sampled_load(void* reference, void* out) -> void*;
auto lyra_rt_chandle_ref_get(void* reference) -> const void*;
void lyra_rt_chandle_ref_set(void* reference, const void* value);
void lyra_rt_chandle_ref_arm_sampling(void* reference);
auto lyra_rt_chandle_ref_sampled_load(void* reference, void* out) -> void*;
auto lyra_rt_managedref_ref_get(void* reference) -> const void*;
void lyra_rt_managedref_ref_set(void* reference, const void* value);
void lyra_rt_managedref_ref_arm_sampling(void* reference);
auto lyra_rt_managedref_ref_sampled_load(void* reference, void* out) -> void*;
auto lyra_rt_tuple_ref_get(void* reference) -> const void*;
void lyra_rt_tuple_ref_set(void* reference, const void* value);
void lyra_rt_tuple_ref_arm_sampling(void* reference);
auto lyra_rt_tuple_ref_sampled_load(void* reference, void* out) -> void*;
auto lyra_rt_union_ref_get(void* reference) -> const void*;
void lyra_rt_union_ref_set(void* reference, const void* value);
void lyra_rt_union_ref_arm_sampling(void* reference);
auto lyra_rt_union_ref_sampled_load(void* reference, void* out) -> void*;
auto lyra_rt_tagged_union_ref_get(void* reference) -> const void*;
void lyra_rt_tagged_union_ref_set(void* reference, const void* value);
void lyra_rt_tagged_union_ref_arm_sampling(void* reference);
auto lyra_rt_tagged_union_ref_sampled_load(void* reference, void* out) -> void*;
auto lyra_rt_dynarray_ref_get(void* reference) -> const void*;
void lyra_rt_dynarray_ref_set(void* reference, const void* value);
void lyra_rt_dynarray_ref_arm_sampling(void* reference);
auto lyra_rt_dynarray_ref_sampled_load(void* reference, void* out) -> void*;
auto lyra_rt_unpackedarray_ref_get(void* reference) -> const void*;
void lyra_rt_unpackedarray_ref_set(void* reference, const void* value);
void lyra_rt_unpackedarray_ref_arm_sampling(void* reference);
auto lyra_rt_unpackedarray_ref_sampled_load(void* reference, void* out)
    -> void*;
auto lyra_rt_queue_ref_get(void* reference) -> const void*;
void lyra_rt_queue_ref_set(void* reference, const void* value);
void lyra_rt_queue_ref_arm_sampling(void* reference);
auto lyra_rt_queue_ref_sampled_load(void* reference, void* out) -> void*;
auto lyra_rt_assocarray_ref_get(void* reference) -> const void*;
void lyra_rt_assocarray_ref_set(void* reference, const void* value);
void lyra_rt_assocarray_ref_arm_sampling(void* reference);
auto lyra_rt_assocarray_ref_sampled_load(void* reference, void* out) -> void*;
auto lyra_rt_string_cell_get(void* cell) -> const void*;
void lyra_rt_string_cell_initialize(void* cell, const void* prototype) noexcept;
void lyra_rt_string_cell_set(void* cell, const void* value);
void lyra_rt_string_cell_arm_sampling(void* cell);
auto lyra_rt_string_cell_sampled_load(void* cell, void* out) -> void*;
auto lyra_rt_real_cell_get(void* cell) -> const void*;
void lyra_rt_real_cell_initialize(void* cell, const void* prototype) noexcept;
void lyra_rt_real_cell_set(void* cell, const void* value);
void lyra_rt_real_cell_arm_sampling(void* cell);
auto lyra_rt_real_cell_sampled_load(void* cell, void* out) -> void*;
auto lyra_rt_shortreal_cell_get(void* cell) -> const void*;
void lyra_rt_shortreal_cell_initialize(
    void* cell, const void* prototype) noexcept;
void lyra_rt_shortreal_cell_set(void* cell, const void* value);
void lyra_rt_shortreal_cell_arm_sampling(void* cell);
auto lyra_rt_shortreal_cell_sampled_load(void* cell, void* out) -> void*;

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
    const void* history, const void* ticks_back, void* out) -> void*;
void lyra_rt_string_sampled_history_install(
    void* history, const void* default_value, const void* depth);
void lyra_rt_string_sampled_history_push(void* history, const void* value);
auto lyra_rt_string_sampled_history_at(
    const void* history, const void* ticks_back, void* out) -> void*;
void lyra_rt_real_sampled_history_install(
    void* history, const void* default_value, const void* depth);
void lyra_rt_real_sampled_history_push(void* history, const void* value);
auto lyra_rt_real_sampled_history_at(
    const void* history, const void* ticks_back, void* out) -> void*;
void lyra_rt_shortreal_sampled_history_install(
    void* history, const void* default_value, const void* depth);
void lyra_rt_shortreal_sampled_history_push(void* history, const void* value);
auto lyra_rt_shortreal_sampled_history_at(
    const void* history, const void* ticks_back, void* out) -> void*;
void lyra_rt_tuple_sampled_history_install(
    void* history, const void* default_value, const void* depth);
void lyra_rt_tuple_sampled_history_push(void* history, const void* value);
auto lyra_rt_tuple_sampled_history_at(
    const void* history, const void* ticks_back, void* out) -> void*;
void lyra_rt_union_sampled_history_install(
    void* history, const void* default_value, const void* depth);
void lyra_rt_union_sampled_history_push(void* history, const void* value);
auto lyra_rt_union_sampled_history_at(
    const void* history, const void* ticks_back, void* out) -> void*;
void lyra_rt_tagged_union_sampled_history_install(
    void* history, const void* default_value, const void* depth);
void lyra_rt_tagged_union_sampled_history_push(
    void* history, const void* value);
auto lyra_rt_tagged_union_sampled_history_at(
    const void* history, const void* ticks_back, void* out) -> void*;
void lyra_rt_dynarray_sampled_history_install(
    void* history, const void* default_value, const void* depth);
void lyra_rt_dynarray_sampled_history_push(void* history, const void* value);
auto lyra_rt_dynarray_sampled_history_at(
    const void* history, const void* ticks_back, void* out) -> void*;
void lyra_rt_unpackedarray_sampled_history_install(
    void* history, const void* default_value, const void* depth);
void lyra_rt_unpackedarray_sampled_history_push(
    void* history, const void* value);
auto lyra_rt_unpackedarray_sampled_history_at(
    const void* history, const void* ticks_back, void* out) -> void*;
void lyra_rt_queue_sampled_history_install(
    void* history, const void* default_value, const void* depth);
void lyra_rt_queue_sampled_history_push(void* history, const void* value);
auto lyra_rt_queue_sampled_history_at(
    const void* history, const void* ticks_back, void* out) -> void*;
void lyra_rt_assocarray_sampled_history_install(
    void* history, const void* default_value, const void* depth);
void lyra_rt_assocarray_sampled_history_push(void* history, const void* value);
auto lyra_rt_assocarray_sampled_history_at(
    const void* history, const void* ticks_back, void* out) -> void*;
void lyra_rt_managedref_sampled_history_install(
    void* history, const void* default_value, const void* depth);
void lyra_rt_managedref_sampled_history_push(void* history, const void* value);
auto lyra_rt_managedref_sampled_history_at(
    const void* history, const void* ticks_back, void* out) -> void*;

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

// A value a body holds across a suspension (LRM 9.4) -- what a call it awaits
// completes with. The cell lives in the running activation's store, so the
// handle a generated frame holds across a suspension points into
// activation-lifetime storage. `store` overwrites the cell -- the first store
// installs the declared representation -- and `load` answers with the value
// where the cell holds it. No runtime handle and no subscriber wakeup: it is no
// variable, and nothing waits on it.
auto lyra_rt_packed_value_cell_alloc() noexcept -> void*;
auto lyra_rt_string_value_cell_alloc() noexcept -> void*;
void lyra_rt_packed_value_cell_store(void* cell, const void* value) noexcept;
void lyra_rt_string_value_cell_store(void* cell, const void* value) noexcept;
auto lyra_rt_packed_value_cell_load(void* cell) noexcept -> void*;
auto lyra_rt_string_value_cell_load(void* cell) noexcept -> void*;

// A guard the language requires to run as part of evaluating an access rather
// than ahead of it (LRM 11.3.5): it raises `message` unless `condition` is a
// definite one, and otherwise yields what it was handed, so the access composes
// onto it. What is guarded decides nothing here -- the guard reads it not at
// all -- so this is one entry rather than one per value domain, and the same
// one serves a cell the access reached through. `message` is a literal and
// crosses as the constant itself, which is what every literal operand does.
auto lyra_rt_require(void* value, const void* condition, const char* message)
    -> void*;

// One entry per operator per value domain: the generated module names the entry
// it means, so no operator code crosses the boundary. Each is the library peer
// of the C++ operator a native target would emit, and builds its result in the
// storage the caller gives, as that operator's result is built in the caller's
// frame.

// Joining values and laying one down a stated number of times (LRM 11.4.12).
// What is joined follows the operand's domain, so one entry each serves both
// spellings. A join takes two operands: a longer source-level one folds into a
// chain, since an operand list of arbitrary length has no single entry to call.
auto lyra_rt_packed_concat(const void* lhs, const void* rhs, void* out)
    -> void*;
auto lyra_rt_packed_replicate(
    const void* operand, std::int64_t count, void* out) -> void*;

auto lyra_rt_packed_add(const void* lhs, const void* rhs, void* out) -> void*;
auto lyra_rt_packed_sub(const void* lhs, const void* rhs, void* out) -> void*;
auto lyra_rt_packed_mul(const void* lhs, const void* rhs, void* out) -> void*;
auto lyra_rt_packed_div(const void* lhs, const void* rhs, void* out) -> void*;
auto lyra_rt_packed_mod(const void* lhs, const void* rhs, void* out) -> void*;
auto lyra_rt_packed_and(const void* lhs, const void* rhs, void* out) -> void*;
auto lyra_rt_packed_or(const void* lhs, const void* rhs, void* out) -> void*;
auto lyra_rt_packed_xor(const void* lhs, const void* rhs, void* out) -> void*;
auto lyra_rt_packed_eq(const void* lhs, const void* rhs, void* out) -> void*;
auto lyra_rt_packed_ne(const void* lhs, const void* rhs, void* out) -> void*;
auto lyra_rt_packed_lt(const void* lhs, const void* rhs, void* out) -> void*;
auto lyra_rt_packed_le(const void* lhs, const void* rhs, void* out) -> void*;
auto lyra_rt_packed_gt(const void* lhs, const void* rhs, void* out) -> void*;
auto lyra_rt_packed_ge(const void* lhs, const void* rhs, void* out) -> void*;
auto lyra_rt_packed_logical_and(const void* lhs, const void* rhs, void* out)
    -> void*;
auto lyra_rt_packed_logical_or(const void* lhs, const void* rhs, void* out)
    -> void*;
auto lyra_rt_packed_neg(const void* operand, void* out) -> void*;
auto lyra_rt_packed_not(const void* operand, void* out) -> void*;
auto lyra_rt_packed_logical_not(const void* operand, void* out) -> void*;
auto lyra_rt_packed_to_bool(const void* operand) -> bool;

// Value builtins: the operations the source language spells as a call rather
// than an operator. Named `lyra_rt_<domain>_<builtin>`, the same way an
// operator entry is, so the generated module derives the symbol it means.
auto lyra_rt_packed_convert_from_packed(
    const void* src, const void* type, void* out) -> void*;
auto lyra_rt_packed_from_bool(bool value, void* out) -> void*;
auto lyra_rt_packed_from_int(std::int64_t value, const void* type, void* out)
    -> void*;
auto lyra_rt_packed_to_int64(const void* value) -> std::int64_t;
auto lyra_rt_packed_is_unknown(const void* value, void* out) -> void*;
auto lyra_rt_packed_count_bits(
    const void* value, const void* control_bits, void* out) -> void*;
auto lyra_rt_packed_clog2(const void* value, void* out) -> void*;
auto lyra_rt_packed_pow(const void* base, const void* exponent, void* out)
    -> void*;
auto lyra_rt_packed_shift_left(const void* value, const void* amount, void* out)
    -> void*;
auto lyra_rt_packed_logical_shift_right(
    const void* value, const void* amount, void* out) -> void*;
auto lyra_rt_packed_arithmetic_shift_right(
    const void* value, const void* amount, void* out) -> void*;
void lyra_rt_packed_shift_left_assign(void* value, const void* amount);
void lyra_rt_packed_logical_shift_right_assign(void* value, const void* amount);
void lyra_rt_packed_arithmetic_shift_right_assign(
    void* value, const void* amount);
auto lyra_rt_packed_bitwise_xnor(const void* lhs, const void* rhs, void* out)
    -> void*;
auto lyra_rt_packed_logical_equivalence(
    const void* lhs, const void* rhs, void* out) -> void*;
auto lyra_rt_packed_case_equal(const void* lhs, const void* rhs, void* out)
    -> void*;
auto lyra_rt_packed_wildcard_equals(const void* lhs, const void* rhs, void* out)
    -> void*;
auto lyra_rt_packed_casez_equals(const void* lhs, const void* rhs, void* out)
    -> void*;
auto lyra_rt_packed_casex_equals(const void* lhs, const void* rhs, void* out)
    -> void*;
auto lyra_rt_packed_merge_conditional(
    const void* lhs, const void* rhs, void* out) -> void*;
auto lyra_rt_packed_reduction_and(const void* value, void* out) -> void*;
auto lyra_rt_packed_reduction_or(const void* value, void* out) -> void*;
auto lyra_rt_packed_reduction_xor(const void* value, void* out) -> void*;
auto lyra_rt_packed_reduction_nand(const void* value, void* out) -> void*;
auto lyra_rt_packed_reduction_nor(const void* value, void* out) -> void*;
auto lyra_rt_packed_reduction_xnor(const void* value, void* out) -> void*;
auto lyra_rt_packed_to_owned(const void* value, void* out) -> void*;
// `width` bits from `position` (LRM 11.5.1): `slice` copies them out, and
// `with_slice` returns a copy with them replaced -- the functional write the
// execution backend uses because it cannot mutate a packed value in place. A
// bit-select and a packed aggregate's member are the same bits, one bit or one
// member wide.
auto lyra_rt_packed_slice(
    const void* value, const void* position, std::int64_t width, void* out)
    -> void*;
auto lyra_rt_packed_with_slice(
    const void* value, const void* position, std::int64_t width,
    const void* replacement, void* out) -> void*;
// The position an index names, as the value position arithmetic is done in.
auto lyra_rt_packed_to_position(const void* index, void* out) -> void*;

auto lyra_rt_string_from_packed_array(const void* bits, void* out) -> void*;
// LRM 21.3.4.3: an unpacked array of byte read as text, in element order.
auto lyra_rt_string_from_byte_array(const void* bytes, void* out) -> void*;
// The C string a `string` crosses the DPI-C boundary as (LRM 35.5.6). It points
// into the SV value, which outlives the call, so the foreign side may read it
// for the call's duration.
auto lyra_rt_string_cstr(const void* value) -> const char*;
auto lyra_rt_string_len(const void* value, void* out) -> void*;
auto lyra_rt_string_getc(const void* value, const void* index, void* out)
    -> void*;
// Positional access (LRM 6.16.2). `element` reads the character; `with_element`
// returns a copy with one character replaced -- the functional write the
// execution backend uses because it cannot mutate a string in place.
auto lyra_rt_string_element(const void* value, const void* index, void* out)
    -> void*;
auto lyra_rt_string_with_element(
    const void* value, const void* index, const void* replacement, void* out)
    -> void*;
auto lyra_rt_string_toupper(const void* value, void* out) -> void*;
auto lyra_rt_string_tolower(const void* value, void* out) -> void*;
auto lyra_rt_string_compare(const void* lhs, const void* rhs, void* out)
    -> void*;
auto lyra_rt_string_icompare(const void* lhs, const void* rhs, void* out)
    -> void*;
auto lyra_rt_string_substr(
    const void* value, const void* first, const void* last, void* out) -> void*;
auto lyra_rt_string_atoi(const void* value, void* out) -> void*;
auto lyra_rt_string_atohex(const void* value, void* out) -> void*;
auto lyra_rt_string_atooct(const void* value, void* out) -> void*;
auto lyra_rt_string_atobin(const void* value, void* out) -> void*;
auto lyra_rt_string_atoreal(const void* value, void* out) -> void*;
// LRM 6.16.3 writes one character of the receiver, and LRM 6.16.14 -- 6.16.18
// format the receiver from a number; each changes the string where it lies.
void lyra_rt_string_putc(void* value, const void* index, const void* character);
void lyra_rt_string_itoa(void* value, const void* number);
void lyra_rt_string_hextoa(void* value, const void* number);
void lyra_rt_string_octtoa(void* value, const void* number);
void lyra_rt_string_bintoa(void* value, const void* number);
void lyra_rt_string_realtoa(void* value, const void* number);

// LRM 21.3.4.3 `$sscanf` / `$fscanf`, resolved through the domain of the text
// they read. `prototypes` is the product of one value per conversion, stating
// the shape each parses into; the completion leads with the matched-conversion
// count and how far the parse advanced, then carries one value per prototype.
auto lyra_rt_string_scan_string(
    const void* input, const void* format, const void* prototypes, void* out)
    -> void*;
auto lyra_rt_string_scan_file(
    const void* input, const void* format, const void* prototypes, void* out)
    -> void*;

auto lyra_rt_string_add(const void* lhs, const void* rhs, void* out) -> void*;
auto lyra_rt_string_concat(const void* lhs, const void* rhs, void* out)
    -> void*;
auto lyra_rt_string_replicate(
    const void* operand, std::int64_t count, void* out) -> void*;
auto lyra_rt_string_eq(const void* lhs, const void* rhs, void* out) -> void*;
auto lyra_rt_string_ne(const void* lhs, const void* rhs, void* out) -> void*;
auto lyra_rt_string_case_equal(const void* lhs, const void* rhs, void* out)
    -> void*;
auto lyra_rt_string_lt(const void* lhs, const void* rhs, void* out) -> void*;
auto lyra_rt_string_le(const void* lhs, const void* rhs, void* out) -> void*;
auto lyra_rt_string_gt(const void* lhs, const void* rhs, void* out) -> void*;
auto lyra_rt_string_ge(const void* lhs, const void* rhs, void* out) -> void*;

// The `real` / `realtime` host-double value domain. A relational or equality
// entry yields a packed 1-bit; the arithmetic entries yield a real. `const`
// builds a real from a host-precision immediate, `from_int64` from an integer
// already read out of a packed value, and `from_shortreal` / `from_real`
// reshape the other real precision. The cell entries hold a real in storage
// that outlives the body that wrote it.
auto lyra_rt_real_add(const void* lhs, const void* rhs, void* out) -> void*;
auto lyra_rt_real_sub(const void* lhs, const void* rhs, void* out) -> void*;
auto lyra_rt_real_mul(const void* lhs, const void* rhs, void* out) -> void*;
auto lyra_rt_real_div(const void* lhs, const void* rhs, void* out) -> void*;
auto lyra_rt_real_neg(const void* operand, void* out) -> void*;
auto lyra_rt_real_eq(const void* lhs, const void* rhs, void* out) -> void*;
auto lyra_rt_real_ne(const void* lhs, const void* rhs, void* out) -> void*;
auto lyra_rt_real_lt(const void* lhs, const void* rhs, void* out) -> void*;
auto lyra_rt_real_le(const void* lhs, const void* rhs, void* out) -> void*;
auto lyra_rt_real_gt(const void* lhs, const void* rhs, void* out) -> void*;
auto lyra_rt_real_ge(const void* lhs, const void* rhs, void* out) -> void*;
auto lyra_rt_real_to_bool(const void* operand) -> bool;

// The LRM 20.8.2 Table 20-4 mathematics, whose behavior the standard defines
// to be that of the C library function each is cross-listed with. The
// two-argument rows take their second operand after the receiver, and `pow` is
// the row LRM 11.4.3 `**` on real operands asks for as well.
auto lyra_rt_real_pow(const void* base, const void* exponent, void* out)
    -> void*;
auto lyra_rt_real_ln(const void* value, void* out) -> void*;
auto lyra_rt_real_log10(const void* value, void* out) -> void*;
auto lyra_rt_real_exp(const void* value, void* out) -> void*;
auto lyra_rt_real_sqrt(const void* value, void* out) -> void*;
auto lyra_rt_real_floor(const void* value, void* out) -> void*;
auto lyra_rt_real_ceil(const void* value, void* out) -> void*;
auto lyra_rt_real_sin(const void* value, void* out) -> void*;
auto lyra_rt_real_cos(const void* value, void* out) -> void*;
auto lyra_rt_real_tan(const void* value, void* out) -> void*;
auto lyra_rt_real_asin(const void* value, void* out) -> void*;
auto lyra_rt_real_acos(const void* value, void* out) -> void*;
auto lyra_rt_real_atan(const void* value, void* out) -> void*;
auto lyra_rt_real_atan2(const void* y, const void* x, void* out) -> void*;
auto lyra_rt_real_hypot(const void* x, const void* y, void* out) -> void*;
auto lyra_rt_real_sinh(const void* value, void* out) -> void*;
auto lyra_rt_real_cosh(const void* value, void* out) -> void*;
auto lyra_rt_real_tanh(const void* value, void* out) -> void*;
auto lyra_rt_real_asinh(const void* value, void* out) -> void*;
auto lyra_rt_real_acosh(const void* value, void* out) -> void*;
auto lyra_rt_real_atanh(const void* value, void* out) -> void*;

// Reading a real out as an integer: LRM 6.12.1 rounds, LRM 20.5 `$rtoi`
// truncates, and the bit-pattern pair carries the IEEE 754 encoding itself.
auto lyra_rt_real_round(const void* value) -> std::int64_t;
auto lyra_rt_real_real_value(const void* value) -> double;
auto lyra_rt_real_truncate(const void* value) -> std::int64_t;
auto lyra_rt_real_to_bits(const void* value) -> std::int64_t;
auto lyra_rt_real_from_bits(std::int64_t bits, void* out) -> void*;

auto lyra_rt_real_const(double value, void* out) -> void*;
auto lyra_rt_real_from_int(std::int64_t value, void* out) -> void*;
auto lyra_rt_real_convert_from_shortreal(const void* value, void* out) -> void*;
auto lyra_rt_real_convert_from_real(const void* value, void* out) -> void*;
auto lyra_rt_real_value_cell_alloc() noexcept -> void*;
void lyra_rt_real_value_cell_store(void* cell, const void* value) noexcept;
auto lyra_rt_real_value_cell_load(void* cell) noexcept -> void*;
auto lyra_rt_real_make_print_value_item(
    const void* value, const void* spec, void* out) -> void*;
auto lyra_rt_real_make_format_arg(const void* value, void* out) -> void*;

// The `shortreal` host-float value domain, the single-precision peer of the
// real domain above.
auto lyra_rt_shortreal_add(const void* lhs, const void* rhs, void* out)
    -> void*;
auto lyra_rt_shortreal_sub(const void* lhs, const void* rhs, void* out)
    -> void*;
auto lyra_rt_shortreal_mul(const void* lhs, const void* rhs, void* out)
    -> void*;
auto lyra_rt_shortreal_div(const void* lhs, const void* rhs, void* out)
    -> void*;
auto lyra_rt_shortreal_neg(const void* operand, void* out) -> void*;
auto lyra_rt_shortreal_eq(const void* lhs, const void* rhs, void* out) -> void*;
auto lyra_rt_shortreal_ne(const void* lhs, const void* rhs, void* out) -> void*;
auto lyra_rt_shortreal_lt(const void* lhs, const void* rhs, void* out) -> void*;
auto lyra_rt_shortreal_le(const void* lhs, const void* rhs, void* out) -> void*;
auto lyra_rt_shortreal_gt(const void* lhs, const void* rhs, void* out) -> void*;
auto lyra_rt_shortreal_ge(const void* lhs, const void* rhs, void* out) -> void*;
auto lyra_rt_shortreal_to_bool(const void* operand) -> bool;
auto lyra_rt_shortreal_pow(const void* base, const void* exponent, void* out)
    -> void*;
auto lyra_rt_shortreal_round(const void* value) -> std::int64_t;
auto lyra_rt_shortreal_real_value(const void* value) -> float;
auto lyra_rt_shortreal_to_bits(const void* value) -> std::int64_t;
auto lyra_rt_shortreal_from_bits(std::int64_t bits, void* out) -> void*;
auto lyra_rt_shortreal_const(float value, void* out) -> void*;
auto lyra_rt_shortreal_from_int(std::int64_t value, void* out) -> void*;
auto lyra_rt_shortreal_convert_from_real(const void* value, void* out) -> void*;
auto lyra_rt_shortreal_value_cell_alloc() noexcept -> void*;
void lyra_rt_shortreal_value_cell_store(void* cell, const void* value) noexcept;
auto lyra_rt_shortreal_value_cell_load(void* cell) noexcept -> void*;
auto lyra_rt_shortreal_make_print_value_item(
    const void* value, const void* spec, void* out) -> void*;
auto lyra_rt_shortreal_make_format_arg(const void* value, void* out) -> void*;

// The `chandle` domain (LRM 6.14). LRM 6.14 admits only the equality family
// (which yields a packed 1-bit) and the boolean test; there is no arithmetic,
// no ordering and no format entry. A chandle that names something came from a
// foreign call, and both directions of that crossing are entries so that which
// bits the value is stays the runtime's own answer.
auto lyra_rt_chandle_default(void* out) -> void*;
auto lyra_rt_chandle_make(void* pointer, void* out) -> void*;
auto lyra_rt_chandle_ptr(const void* operand) -> void*;
auto lyra_rt_chandle_eq(const void* lhs, const void* rhs, void* out) -> void*;
auto lyra_rt_chandle_ne(const void* lhs, const void* rhs, void* out) -> void*;
auto lyra_rt_chandle_case_equal(const void* lhs, const void* rhs, void* out)
    -> void*;
auto lyra_rt_chandle_to_bool(const void* operand) -> bool;
auto lyra_rt_chandle_value_cell_alloc() noexcept -> void*;
void lyra_rt_chandle_value_cell_store(void* cell, const void* value) noexcept;
auto lyra_rt_chandle_value_cell_load(void* cell) noexcept -> void*;
auto lyra_rt_chandle_cell_get(void* cell) -> const void*;
void lyra_rt_chandle_cell_initialize(
    void* cell, const void* prototype) noexcept;
void lyra_rt_chandle_cell_set(void* cell, const void* value);
void lyra_rt_chandle_cell_arm_sampling(void* cell);
auto lyra_rt_chandle_cell_sampled_load(void* cell, void* out) -> void*;

// The managed-reference domain (LRM 8.3, and the LRM 9.7 `process` a handle
// names). What a handle carries is the object's address together with a share
// of its ownership, and a share cannot be recovered from an address alone, so
// the two travel together and a store copies both. LRM Table 11-1's "Any data
// type" row is the whole operator surface -- the equality family, which yields
// a packed 1-bit, and the boolean test. A null handle is the domain's default
// value.
auto lyra_rt_managedref_default(void* out) -> void*;
auto lyra_rt_managedref_eq(const void* lhs, const void* rhs, void* out)
    -> void*;
auto lyra_rt_managedref_ne(const void* lhs, const void* rhs, void* out)
    -> void*;
auto lyra_rt_managedref_case_equal(const void* lhs, const void* rhs, void* out)
    -> void*;
auto lyra_rt_managedref_to_bool(const void* operand) -> bool;
auto lyra_rt_managedref_value_cell_alloc() noexcept -> void*;
void lyra_rt_managedref_value_cell_store(
    void* cell, const void* value) noexcept;
auto lyra_rt_managedref_value_cell_load(void* cell) noexcept -> void*;
auto lyra_rt_managedref_cell_get(void* cell) -> const void*;
void lyra_rt_managedref_cell_initialize(
    void* cell, const void* prototype) noexcept;
void lyra_rt_managedref_cell_set(void* cell, const void* value);
void lyra_rt_managedref_cell_arm_sampling(void* cell);
auto lyra_rt_managedref_cell_sampled_load(void* cell, void* out) -> void*;

// The tuple domain, an unpacked struct (LRM 7.2) among them. A tuple is laid
// out by the program that uses it, opening with its type's operation table, and
// crosses as the address of those bytes; building one, reaching a component,
// comparing two and every other operation of the type are the program's own,
// compiled for the type. What remains here is what a storage wrapper does with
// a tuple it holds.
auto lyra_rt_tuple_cell_get(void* cell) -> const void*;
void lyra_rt_tuple_cell_initialize(void* cell, const void* prototype) noexcept;
void lyra_rt_tuple_cell_set(void* cell, const void* value);
void lyra_rt_tuple_cell_arm_sampling(void* cell);
auto lyra_rt_tuple_cell_sampled_load(void* cell, void* out) -> void*;
auto lyra_rt_tuple_value_cell_alloc() noexcept -> void*;
void lyra_rt_tuple_value_cell_store(void* cell, const void* value) noexcept;
auto lyra_rt_tuple_value_cell_load(void* cell) noexcept -> void*;

// The untagged-union domain (LRM 7.3), MIR's `UnionType`. An active-member
// value carried behind an opaque handle: it stores the one live member and its
// index. `make` builds it from an index and an erased member value;
// `component` returns the member at `index`, which must be the live one;
// `with_component`
// returns a copy whose live member is `index` carrying the erased replacement.
// All are value operations, never in-place writes.
auto lyra_rt_union_make(
    std::int64_t index, const void* value, const void* value_type, void* out)
    -> void*;
auto lyra_rt_union_component(const void* value, std::int64_t index, void* out)
    -> void*;
auto lyra_rt_union_with_component(
    const void* value, std::int64_t index, const void* member,
    const void* member_type, void* out) -> void*;
auto lyra_rt_union_eq(const void* lhs, const void* rhs, void* out) -> void*;
auto lyra_rt_union_ne(const void* lhs, const void* rhs, void* out) -> void*;
auto lyra_rt_union_case_equal(const void* lhs, const void* rhs, void* out)
    -> void*;
auto lyra_rt_union_is_unknown(const void* value, void* out) -> void*;
auto lyra_rt_union_cell_get(void* cell) -> const void*;
void lyra_rt_union_cell_initialize(void* cell, const void* prototype) noexcept;
void lyra_rt_union_cell_set(void* cell, const void* value);
void lyra_rt_union_cell_arm_sampling(void* cell);
auto lyra_rt_union_cell_sampled_load(void* cell, void* out) -> void*;
auto lyra_rt_union_value_cell_alloc() noexcept -> void*;
void lyra_rt_union_value_cell_store(void* cell, const void* value) noexcept;
auto lyra_rt_union_value_cell_load(void* cell) noexcept -> void*;

// The tagged-union domain (LRM 7.3.2 / 11.9), MIR's `TaggedUnionType`. The
// tagged sibling of the untagged union: the tag is observable, so `component`
// and `with_component` fault when `index` is not the live tag rather than
// returning a fallback, and `tag_matches` answers whether the active tag is a
// given one, the packed guard a pattern match tests (LRM 12.6). `make` builds
// it from a tag and an erased payload; re-tagging goes through `make`, never
// `with_component`.
auto lyra_rt_tagged_union_make(
    std::int64_t tag, const void* payload, const void* payload_type, void* out)
    -> void*;
auto lyra_rt_tagged_union_component(
    const void* value, std::int64_t index, void* out) -> void*;
auto lyra_rt_tagged_union_with_component(
    const void* value, std::int64_t index, const void* member,
    const void* member_type, void* out) -> void*;
auto lyra_rt_tagged_union_tag_matches(const void* value, std::int64_t index)
    -> bool;
auto lyra_rt_tagged_union_eq(const void* lhs, const void* rhs, void* out)
    -> void*;
auto lyra_rt_tagged_union_ne(const void* lhs, const void* rhs, void* out)
    -> void*;
auto lyra_rt_tagged_union_case_equal(
    const void* lhs, const void* rhs, void* out) -> void*;
auto lyra_rt_tagged_union_is_unknown(const void* value, void* out) -> void*;
auto lyra_rt_tagged_union_cell_get(void* cell) -> const void*;
void lyra_rt_tagged_union_cell_initialize(
    void* cell, const void* prototype) noexcept;
void lyra_rt_tagged_union_cell_set(void* cell, const void* value);
void lyra_rt_tagged_union_cell_arm_sampling(void* cell);
auto lyra_rt_tagged_union_cell_sampled_load(void* cell, void* out) -> void*;
auto lyra_rt_tagged_union_value_cell_alloc() noexcept -> void*;
void lyra_rt_tagged_union_value_cell_store(
    void* cell, const void* value) noexcept;
auto lyra_rt_tagged_union_value_cell_load(void* cell) noexcept -> void*;

// The empty domain: a tagged union's `void` member (LRM 7.3.2), a value with no
// bits. `default` builds the one value it has.
auto lyra_rt_empty_default(void* out) -> void*;

// The dynamic-array domain (LRM 7.5), MIR's `DynamicArrayType`. A
// run-time-sized homogeneous container carried behind an opaque handle, owning
// its elements by value. `default` / `new` / `new_copy` are the LRM 7.5.1
// constructors (empty, sized, sized-from-source); `from_literal` collects the
// elements of an assignment pattern. The element default rides every
// constructor -- the shape source for out-of-range reads (LRM 7.4.5) and resize
// fills -- and it crosses erased, because it is what states the element's
// representation and nothing here knows that representation before it arrives.
// A literal's elements then cross as bare handles: the prototype beside them
// names their type, so the entry copies each as a value of it. An element is
// storage of its own: `element` answers with it where it lies, for reading, and
// `element_ref` with the same storage for a write to land in (LRM 7.4.6);
// `slice_ref` writes a window into the elements already there (LRM 7.6).
auto lyra_rt_make_dynamic_array_default(
    const void* prototype, const void* prototype_type, void* out) -> void*;
auto lyra_rt_make_dynamic_array_new(
    const void* size, const void* prototype, const void* prototype_type,
    void* out) -> void*;
auto lyra_rt_make_dynamic_array_new_copy(
    const void* size, const void* prototype, const void* prototype_type,
    const void* src, void* out) -> void*;
auto lyra_rt_dynarray_from_literal(
    const void* prototype, const void* prototype_type, LyraSpan unit,
    std::int64_t count, void* out) -> void*;
// LRM 7.6: one unpacked array kind taking another's elements. The entry names
// both representations because the source is read through the one it has and
// the result is built in the one the destination declares.
auto lyra_rt_dynarray_from_array_unpackedarray(
    const void* source, const void* prototype, const void* prototype_type,
    void* out) -> void*;
auto lyra_rt_dynarray_from_array_queue(
    const void* source, const void* prototype, const void* prototype_type,
    void* out) -> void*;
auto lyra_rt_dynarray_element(const void* array, const void* index) -> const
    void*;
auto lyra_rt_dynarray_element_ref(void* array, const void* index) -> void*;
auto lyra_rt_dynarray_concat_element(
    const void* array, const void* item, void* out) -> void*;
auto lyra_rt_dynarray_concat_spread(
    const void* array, const void* part, const void* part_type, void* out)
    -> void*;
void lyra_rt_dynarray_delete(void* array);
auto lyra_rt_dynarray_slice(
    const void* array, const void* start, std::int64_t count, void* out)
    -> void*;
void lyra_rt_dynarray_slice_ref(
    void* array, const void* start, std::int64_t count,
    const void* replacement);
auto lyra_rt_dynarray_size(const void* array, void* out) -> void*;
auto lyra_rt_dynarray_eq(const void* lhs, const void* rhs, void* out) -> void*;
auto lyra_rt_dynarray_ne(const void* lhs, const void* rhs, void* out) -> void*;
auto lyra_rt_dynarray_case_equal(const void* lhs, const void* rhs, void* out)
    -> void*;
auto lyra_rt_dynarray_cell_get(void* cell) -> const void*;
void lyra_rt_dynarray_cell_initialize(
    void* cell, const void* prototype) noexcept;
void lyra_rt_dynarray_cell_set(void* cell, const void* value);
void lyra_rt_dynarray_cell_arm_sampling(void* cell);
auto lyra_rt_dynarray_cell_sampled_load(void* cell, void* out) -> void*;
auto lyra_rt_dynarray_value_cell_alloc() noexcept -> void*;
void lyra_rt_dynarray_value_cell_store(void* cell, const void* value) noexcept;
auto lyra_rt_dynarray_value_cell_load(void* cell) noexcept -> void*;

// A fixed-size unpacked array (LRM 7.4.2). Its payload is ordinal-only, and
// every access names an element by its ordinal: the declared range is the
// static type's, and a select has read it before any of these is reached. An
// element is storage of its own, reached as the dynamic array's is.
auto lyra_rt_unpackedarray_from_literal(
    const void* prototype, const void* prototype_type, LyraSpan unit,
    std::int64_t count, void* out) -> void*;
auto lyra_rt_unpackedarray_conform_size(
    const void* parts, std::int64_t count, void* out) -> void*;
auto lyra_rt_unpackedarray_from_array_dynarray(
    const void* source, const void* prototype, const void* prototype_type,
    std::int64_t declared, void* out) -> void*;
auto lyra_rt_unpackedarray_from_array_queue(
    const void* source, const void* prototype, const void* prototype_type,
    std::int64_t declared, void* out) -> void*;
auto lyra_rt_unpackedarray_element(const void* array, const void* position)
    -> const void*;
auto lyra_rt_unpackedarray_element_ref(void* array, const void* position)
    -> void*;
auto lyra_rt_unpackedarray_slice(
    const void* array, const void* start, std::int64_t count, void* out)
    -> void*;
void lyra_rt_unpackedarray_slice_ref(
    void* array, const void* start, std::int64_t count,
    const void* replacement);
auto lyra_rt_unpackedarray_size(const void* array, void* out) -> void*;
auto lyra_rt_unpackedarray_eq(const void* lhs, const void* rhs, void* out)
    -> void*;
auto lyra_rt_unpackedarray_ne(const void* lhs, const void* rhs, void* out)
    -> void*;
auto lyra_rt_unpackedarray_case_equal(
    const void* lhs, const void* rhs, void* out) -> void*;
auto lyra_rt_unpackedarray_is_unknown(const void* value, void* out) -> void*;
// The value a conditional whose arms disagree yields (LRM 11.4.11): each
// element takes the two arms' merge, so an element the arms agree on survives
// and one they differ on becomes unknown.
auto lyra_rt_unpackedarray_merge_conditional(
    const void* lhs, const void* rhs, void* out) -> void*;
// The LRM 6.24.1 bit-stream cast of a packed value into an unpacked array: the
// bits are cut into `count` elements of the stated element type.
auto lyra_rt_unpackedarray_from_packed_array(
    const void* bits, const void* element_type, const void* count, void* out)
    -> void*;
auto lyra_rt_unpackedarray_cell_get(void* cell) -> const void*;
void lyra_rt_unpackedarray_cell_initialize(
    void* cell, const void* prototype) noexcept;
void lyra_rt_unpackedarray_cell_set(void* cell, const void* value);
void lyra_rt_unpackedarray_cell_arm_sampling(void* cell);
auto lyra_rt_unpackedarray_cell_sampled_load(void* cell, void* out) -> void*;
auto lyra_rt_unpackedarray_value_cell_alloc() noexcept -> void*;
void lyra_rt_unpackedarray_value_cell_store(
    void* cell, const void* value) noexcept;
auto lyra_rt_unpackedarray_value_cell_load(void* cell) noexcept -> void*;

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
// `net_join` states that some positions of one net and as many positions of
// another are one physical net, whose contributions resolve together (LRM
// 23.3.3.7, 10.11). It takes the other net rather than a value, the position
// each side starts at in its own net, and how many positions they cover; it
// states no direction, and both nets then answer over those positions with what
// that one resolution produces. A connection between two whole nets states
// every position of them.
//
// LRM 6.7.1 fixes which domains these exist for: a 4-state integral net, and a
// fixed-size unpacked array, struct, or union whose elements are themselves
// valid for a net.
auto lyra_rt_packed_net_get(void* net) -> const void*;
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
auto lyra_rt_packed_net_begin_takeover(void* net, const void* level, void* out)
    -> void*;
auto lyra_rt_packed_net_drive_takeover(
    void* net, const void* level, const void* generation, const void* value)
    -> bool;
void lyra_rt_packed_net_end_takeover(void* net, const void* level);
auto lyra_rt_packed_attach_driver(void* net, const void* strength) -> void*;
void lyra_rt_packed_net_join(
    void* net, void* other, const void* here, const void* there,
    const void* width);
auto lyra_rt_packed_driver_get(void* driver) -> const void*;
void lyra_rt_packed_driver_set(void* driver, const void* value);
auto lyra_rt_tuple_net_get(void* net) -> const void*;
void lyra_rt_tuple_net_initialize_tri_state(
    void* net, const void* prototype, const void* fill, const void* strength);
void lyra_rt_tuple_net_initialize_wired_and(
    void* net, const void* prototype, const void* fill, const void* strength);
void lyra_rt_tuple_net_initialize_wired_or(
    void* net, const void* prototype, const void* fill, const void* strength);
void lyra_rt_tuple_net_initialize_retaining(
    void* net, const void* prototype, const void* fill, const void* strength);
auto lyra_rt_tuple_attach_driver(void* net, const void* strength) -> void*;
void lyra_rt_tuple_net_join(
    void* net, void* other, const void* here, const void* there,
    const void* width);
auto lyra_rt_tuple_driver_get(void* driver) -> const void*;
void lyra_rt_tuple_driver_set(void* driver, const void* value);
auto lyra_rt_union_net_get(void* net) -> const void*;
void lyra_rt_union_net_initialize_tri_state(
    void* net, const void* prototype, const void* fill, const void* strength);
void lyra_rt_union_net_initialize_wired_and(
    void* net, const void* prototype, const void* fill, const void* strength);
void lyra_rt_union_net_initialize_wired_or(
    void* net, const void* prototype, const void* fill, const void* strength);
void lyra_rt_union_net_initialize_retaining(
    void* net, const void* prototype, const void* fill, const void* strength);
auto lyra_rt_union_attach_driver(void* net, const void* strength) -> void*;
void lyra_rt_union_net_join(
    void* net, void* other, const void* here, const void* there,
    const void* width);
auto lyra_rt_union_driver_get(void* driver) -> const void*;
void lyra_rt_union_driver_set(void* driver, const void* value);
auto lyra_rt_unpackedarray_net_get(void* net) -> const void*;
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
void lyra_rt_unpackedarray_net_join(
    void* net, void* other, const void* here, const void* there,
    const void* width);
auto lyra_rt_unpackedarray_driver_get(void* driver) -> const void*;
void lyra_rt_unpackedarray_driver_set(void* driver, const void* value);

// The queue domain (LRM 7.10): a run-time-sized ordered container whose
// elements are added and removed at either end, carried behind an opaque handle
// and owning its elements by value. A queue is built over an element list,
// empty or not, and a declared bound (LRM 7.10.5) is a value its constructor
// takes rather than one it can derive -- so a bounded queue has an entry of its
// own. The bound belongs to the variable, not to the value written, so
// `conform_bound` is what a semantic store into a bounded queue passes its
// right-hand side through. An element write appends when its index is the
// queue's size and is discarded at any other invalid index (LRM 7.10.1); an
// element write, a push, an insert and a delete each change the queue where it
// lies.
auto lyra_rt_queue_from_literal(
    const void* prototype, const void* prototype_type, LyraSpan unit,
    std::int64_t count, void* out) -> void*;
auto lyra_rt_queue_from_literal_bounded(
    const void* prototype, const void* prototype_type, LyraSpan unit,
    std::int64_t count, const void* max_bound, void* out) -> void*;
auto lyra_rt_queue_conform_bound(
    const void* queue, const void* max_bound, void* out) -> void*;
auto lyra_rt_queue_from_array_unpackedarray(
    const void* source, const void* prototype, const void* prototype_type,
    const void* max_bound, void* out) -> void*;
auto lyra_rt_queue_from_array_dynarray(
    const void* source, const void* prototype, const void* prototype_type,
    const void* max_bound, void* out) -> void*;
auto lyra_rt_queue_element(const void* queue, const void* index) -> const void*;
auto lyra_rt_queue_element_ref(void* queue, const void* index) -> void*;
auto lyra_rt_queue_slice(
    const void* queue, const void* lo, const void* hi, void* out) -> void*;
auto lyra_rt_queue_size(const void* queue, void* out) -> void*;
void lyra_rt_queue_push_back(void* queue, const void* item);
void lyra_rt_queue_push_front(void* queue, const void* item);
auto lyra_rt_queue_concat_element(
    const void* queue, const void* item, void* out) -> void*;
auto lyra_rt_queue_concat_spread(
    const void* queue, const void* part, const void* part_type, void* out)
    -> void*;
void lyra_rt_queue_insert(void* queue, const void* index, const void* item);
// LRM 7.10.2.4 / 7.10.2.5 pop: the element leaves the queue and is what the
// call answers with.
auto lyra_rt_queue_pop_front(void* queue, void* out) -> void*;
auto lyra_rt_queue_pop_back(void* queue, void* out) -> void*;
// LRM 7.10.2.3 `delete`: with no index the whole queue empties, with one only
// the entry it names goes, so the two spellings are two entries.
void lyra_rt_queue_delete(void* queue);
void lyra_rt_queue_delete_index(void* queue, const void* index);
auto lyra_rt_queue_eq(const void* lhs, const void* rhs, void* out) -> void*;
auto lyra_rt_queue_ne(const void* lhs, const void* rhs, void* out) -> void*;
auto lyra_rt_queue_case_equal(const void* lhs, const void* rhs, void* out)
    -> void*;
auto lyra_rt_queue_bitstream_width(const void* queue, void* out) -> void*;
auto lyra_rt_queue_count_bits(
    const void* queue, const void* control_bits, void* out) -> void*;
auto lyra_rt_queue_cell_get(void* cell) -> const void*;
void lyra_rt_queue_cell_initialize(void* cell, const void* prototype) noexcept;
void lyra_rt_queue_cell_set(void* cell, const void* value);
void lyra_rt_queue_cell_arm_sampling(void* cell);
auto lyra_rt_queue_cell_sampled_load(void* cell, void* out) -> void*;
auto lyra_rt_queue_value_cell_alloc() noexcept -> void*;
void lyra_rt_queue_value_cell_store(void* cell, const void* value) noexcept;
auto lyra_rt_queue_value_cell_load(void* cell) noexcept -> void*;

// The associative-array domain (LRM 7.8): a sparse lookup table allocated entry
// by entry and held in index order, carried behind an opaque handle. Its
// element default carries the element shape and crosses erased at construction
// like every other container's; what a read of an index with no entry yields
// (LRM 7.8.6) is a second value the construction takes. An index crosses
// erased too, and for a reason of its own: the array holds no index type, so an
// index crosses with the type the program wrote it in, and a lookup reads it
// where it lies. An element beside an index still crosses bare, since the
// element default names its type. An element write and a delete change the
// array where it lies. The order the entries are held in is the one the
// declared index type imposes (LRM 7.8), so which of these two builds an array
// follows from the type being built. Every declared index type's order is the
// one its own values carry; a wildcard index (LRM 7.8.1) is the one they cannot
// carry, so it is a construction of its own rather than an operand the
// generated code computes.
auto lyra_rt_assocarray_from_entries_default(
    const void* prototype, const void* prototype_type, LyraSpan entries,
    const void* user_default, void* out) -> void*;
auto lyra_rt_assocarray_from_entries_default_wildcard(
    const void* prototype, const void* prototype_type, LyraSpan entries,
    const void* user_default, void* out) -> void*;
auto lyra_rt_assocarray_element(
    const void* array, const void* index, const void* index_type) -> const
    void*;
auto lyra_rt_assocarray_element_ref(
    void* array, const void* index, const void* index_type) -> void*;
auto lyra_rt_assocarray_exists(
    const void* array, const void* index, const void* index_type, void* out)
    -> void*;
auto lyra_rt_assocarray_size(const void* array, void* out) -> void*;
// LRM 7.9.3 `delete`: with no index the whole array empties, with one only the
// entry it names goes, so the two spellings are two entries.
void lyra_rt_assocarray_delete(void* array);
void lyra_rt_assocarray_delete_index(
    void* array, const void* index, const void* index_type);
auto lyra_rt_assocarray_eq(const void* lhs, const void* rhs, void* out)
    -> void*;
auto lyra_rt_assocarray_ne(const void* lhs, const void* rhs, void* out)
    -> void*;
auto lyra_rt_assocarray_case_equal(const void* lhs, const void* rhs, void* out)
    -> void*;
auto lyra_rt_assocarray_bitstream_width(const void* array, void* out) -> void*;
// LRM 20.7 `$low` / `$high` over an associative dimension: the smallest and
// largest index the array holds, or `unallocated` where it holds none. That
// answer is an index, so it crosses erased for the same reason a probe does.
auto lyra_rt_assocarray_assoc_min_index(
    const void* array, const void* unallocated, const void* unallocated_type,
    void* out) -> void*;
auto lyra_rt_assocarray_assoc_max_index(
    const void* array, const void* unallocated, const void* unallocated_type,
    void* out) -> void*;
// LRM 7.9.4 -- 7.9.7 traversal. Each completes with the SV int it answers with
// and the index it visited, which is the probe unchanged when there is no such
// index; the probe crosses erased because an index states its own
// representation.
auto lyra_rt_assocarray_assoc_first(
    const void* array, const void* probe, const void* probe_type, void* out)
    -> void*;
auto lyra_rt_assocarray_assoc_last(
    const void* array, const void* probe, const void* probe_type, void* out)
    -> void*;
auto lyra_rt_assocarray_assoc_next(
    const void* array, const void* probe, const void* probe_type, void* out)
    -> void*;
auto lyra_rt_assocarray_assoc_prev(
    const void* array, const void* probe, const void* probe_type, void* out)
    -> void*;
auto lyra_rt_assocarray_count_bits(
    const void* array, const void* control_bits, void* out) -> void*;
auto lyra_rt_assocarray_cell_get(void* cell) -> const void*;
void lyra_rt_assocarray_cell_initialize(
    void* cell, const void* prototype) noexcept;
void lyra_rt_assocarray_cell_set(void* cell, const void* value);
void lyra_rt_assocarray_cell_arm_sampling(void* cell);
auto lyra_rt_assocarray_cell_sampled_load(void* cell, void* out) -> void*;
auto lyra_rt_assocarray_value_cell_alloc() noexcept -> void*;
void lyra_rt_assocarray_value_cell_store(
    void* cell, const void* value) noexcept;
auto lyra_rt_assocarray_value_cell_load(void* cell) noexcept -> void*;

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
    const void* receiver, void* body, const void* prototype,
    const void* prototype_type, void* out) -> void*;
auto lyra_rt_unpackedarray_product(
    const void* receiver, void* body, const void* prototype,
    const void* prototype_type, void* out) -> void*;
auto lyra_rt_unpackedarray_and(
    const void* receiver, void* body, const void* prototype,
    const void* prototype_type, void* out) -> void*;
auto lyra_rt_unpackedarray_or(
    const void* receiver, void* body, const void* prototype,
    const void* prototype_type, void* out) -> void*;
auto lyra_rt_unpackedarray_xor(
    const void* receiver, void* body, const void* prototype,
    const void* prototype_type, void* out) -> void*;
auto lyra_rt_unpackedarray_find(
    const void* receiver, void* body, const void* prototype,
    const void* prototype_type, void* out) -> void*;
auto lyra_rt_unpackedarray_find_index(
    const void* receiver, void* body, const void* prototype,
    const void* prototype_type, void* out) -> void*;
auto lyra_rt_unpackedarray_find_first(
    const void* receiver, void* body, const void* prototype,
    const void* prototype_type, void* out) -> void*;
auto lyra_rt_unpackedarray_find_first_index(
    const void* receiver, void* body, const void* prototype,
    const void* prototype_type, void* out) -> void*;
auto lyra_rt_unpackedarray_find_last(
    const void* receiver, void* body, const void* prototype,
    const void* prototype_type, void* out) -> void*;
auto lyra_rt_unpackedarray_find_last_index(
    const void* receiver, void* body, const void* prototype,
    const void* prototype_type, void* out) -> void*;
auto lyra_rt_unpackedarray_min(
    const void* receiver, void* body, const void* prototype,
    const void* prototype_type, void* out) -> void*;
auto lyra_rt_unpackedarray_max(
    const void* receiver, void* body, const void* prototype,
    const void* prototype_type, void* out) -> void*;
auto lyra_rt_unpackedarray_unique(
    const void* receiver, void* body, const void* prototype,
    const void* prototype_type, void* out) -> void*;
auto lyra_rt_unpackedarray_unique_index(
    const void* receiver, void* body, const void* prototype,
    const void* prototype_type, void* out) -> void*;
auto lyra_rt_unpackedarray_map(
    const void* receiver, void* body, const void* prototype,
    const void* prototype_type, void* out) -> void*;
auto lyra_rt_dynarray_sum(
    const void* receiver, void* body, const void* prototype,
    const void* prototype_type, void* out) -> void*;
auto lyra_rt_dynarray_product(
    const void* receiver, void* body, const void* prototype,
    const void* prototype_type, void* out) -> void*;
auto lyra_rt_dynarray_and(
    const void* receiver, void* body, const void* prototype,
    const void* prototype_type, void* out) -> void*;
auto lyra_rt_dynarray_or(
    const void* receiver, void* body, const void* prototype,
    const void* prototype_type, void* out) -> void*;
auto lyra_rt_dynarray_xor(
    const void* receiver, void* body, const void* prototype,
    const void* prototype_type, void* out) -> void*;
auto lyra_rt_dynarray_find(
    const void* receiver, void* body, const void* prototype,
    const void* prototype_type, void* out) -> void*;
auto lyra_rt_dynarray_find_index(
    const void* receiver, void* body, const void* prototype,
    const void* prototype_type, void* out) -> void*;
auto lyra_rt_dynarray_find_first(
    const void* receiver, void* body, const void* prototype,
    const void* prototype_type, void* out) -> void*;
auto lyra_rt_dynarray_find_first_index(
    const void* receiver, void* body, const void* prototype,
    const void* prototype_type, void* out) -> void*;
auto lyra_rt_dynarray_find_last(
    const void* receiver, void* body, const void* prototype,
    const void* prototype_type, void* out) -> void*;
auto lyra_rt_dynarray_find_last_index(
    const void* receiver, void* body, const void* prototype,
    const void* prototype_type, void* out) -> void*;
auto lyra_rt_dynarray_min(
    const void* receiver, void* body, const void* prototype,
    const void* prototype_type, void* out) -> void*;
auto lyra_rt_dynarray_max(
    const void* receiver, void* body, const void* prototype,
    const void* prototype_type, void* out) -> void*;
auto lyra_rt_dynarray_unique(
    const void* receiver, void* body, const void* prototype,
    const void* prototype_type, void* out) -> void*;
auto lyra_rt_dynarray_unique_index(
    const void* receiver, void* body, const void* prototype,
    const void* prototype_type, void* out) -> void*;
auto lyra_rt_dynarray_map(
    const void* receiver, void* body, const void* prototype,
    const void* prototype_type, void* out) -> void*;
auto lyra_rt_queue_sum(
    const void* receiver, void* body, const void* prototype,
    const void* prototype_type, void* out) -> void*;
auto lyra_rt_queue_product(
    const void* receiver, void* body, const void* prototype,
    const void* prototype_type, void* out) -> void*;
auto lyra_rt_queue_and(
    const void* receiver, void* body, const void* prototype,
    const void* prototype_type, void* out) -> void*;
auto lyra_rt_queue_or(
    const void* receiver, void* body, const void* prototype,
    const void* prototype_type, void* out) -> void*;
auto lyra_rt_queue_xor(
    const void* receiver, void* body, const void* prototype,
    const void* prototype_type, void* out) -> void*;
auto lyra_rt_queue_find(
    const void* receiver, void* body, const void* prototype,
    const void* prototype_type, void* out) -> void*;
auto lyra_rt_queue_find_index(
    const void* receiver, void* body, const void* prototype,
    const void* prototype_type, void* out) -> void*;
auto lyra_rt_queue_find_first(
    const void* receiver, void* body, const void* prototype,
    const void* prototype_type, void* out) -> void*;
auto lyra_rt_queue_find_first_index(
    const void* receiver, void* body, const void* prototype,
    const void* prototype_type, void* out) -> void*;
auto lyra_rt_queue_find_last(
    const void* receiver, void* body, const void* prototype,
    const void* prototype_type, void* out) -> void*;
auto lyra_rt_queue_find_last_index(
    const void* receiver, void* body, const void* prototype,
    const void* prototype_type, void* out) -> void*;
auto lyra_rt_queue_min(
    const void* receiver, void* body, const void* prototype,
    const void* prototype_type, void* out) -> void*;
auto lyra_rt_queue_max(
    const void* receiver, void* body, const void* prototype,
    const void* prototype_type, void* out) -> void*;
auto lyra_rt_queue_unique(
    const void* receiver, void* body, const void* prototype,
    const void* prototype_type, void* out) -> void*;
auto lyra_rt_queue_unique_index(
    const void* receiver, void* body, const void* prototype,
    const void* prototype_type, void* out) -> void*;
auto lyra_rt_queue_map(
    const void* receiver, void* body, const void* prototype,
    const void* prototype_type, void* out) -> void*;
auto lyra_rt_assocarray_sum(
    const void* receiver, void* body, const void* prototype,
    const void* prototype_type, void* out) -> void*;
auto lyra_rt_assocarray_product(
    const void* receiver, void* body, const void* prototype,
    const void* prototype_type, void* out) -> void*;
auto lyra_rt_assocarray_and(
    const void* receiver, void* body, const void* prototype,
    const void* prototype_type, void* out) -> void*;
auto lyra_rt_assocarray_or(
    const void* receiver, void* body, const void* prototype,
    const void* prototype_type, void* out) -> void*;
auto lyra_rt_assocarray_xor(
    const void* receiver, void* body, const void* prototype,
    const void* prototype_type, void* out) -> void*;
auto lyra_rt_assocarray_find(
    const void* receiver, void* body, const void* prototype,
    const void* prototype_type, void* out) -> void*;
auto lyra_rt_assocarray_find_index(
    const void* receiver, void* body, const void* prototype,
    const void* prototype_type, void* out) -> void*;
auto lyra_rt_assocarray_find_first(
    const void* receiver, void* body, const void* prototype,
    const void* prototype_type, void* out) -> void*;
auto lyra_rt_assocarray_find_first_index(
    const void* receiver, void* body, const void* prototype,
    const void* prototype_type, void* out) -> void*;
auto lyra_rt_assocarray_find_last(
    const void* receiver, void* body, const void* prototype,
    const void* prototype_type, void* out) -> void*;
auto lyra_rt_assocarray_find_last_index(
    const void* receiver, void* body, const void* prototype,
    const void* prototype_type, void* out) -> void*;
auto lyra_rt_assocarray_min(
    const void* receiver, void* body, const void* prototype,
    const void* prototype_type, void* out) -> void*;
auto lyra_rt_assocarray_max(
    const void* receiver, void* body, const void* prototype,
    const void* prototype_type, void* out) -> void*;
auto lyra_rt_assocarray_unique(
    const void* receiver, void* body, const void* prototype,
    const void* prototype_type, void* out) -> void*;
auto lyra_rt_assocarray_unique_index(
    const void* receiver, void* body, const void* prototype,
    const void* prototype_type, void* out) -> void*;
auto lyra_rt_assocarray_map(
    const void* receiver, void* body, const void* prototype,
    const void* prototype_type, void* out) -> void*;
void lyra_rt_unpackedarray_sort(void* receiver, void* body);
void lyra_rt_unpackedarray_rsort(void* receiver, void* body);
void lyra_rt_dynarray_sort(void* receiver, void* body);
void lyra_rt_dynarray_rsort(void* receiver, void* body);
void lyra_rt_queue_sort(void* receiver, void* body);
void lyra_rt_queue_rsort(void* receiver, void* body);
void lyra_rt_unpackedarray_reverse(void* receiver);
void lyra_rt_dynarray_reverse(void* receiver);
void lyra_rt_queue_reverse(void* receiver);

// LRM 21.4 / 21.5 memory load and dump. The memory names the entry, since what
// an address means is its own: an unpacked memory reads the declared bounds of
// every dimension, which ride as a sequence of packed values with the addressed
// one first; a dynamic array or queue is the dense space its current size
// spans; and an associative memory is addressed by key, so a load takes a key
// prototype to build each key at the width an ordinary access uses. Running
// upward from an address and running within a window are two requests, so each
// is its own entry. A load answers through its completion, because a word the
// file does not address keeps what it held.
auto lyra_rt_unpackedarray_read_mem(
    void* runtime, const void* memory, const void* name, LyraSpan dims,
    const void* base, const void* start, void* out) -> void*;
auto lyra_rt_unpackedarray_read_mem_within(
    void* runtime, const void* memory, const void* name, LyraSpan dims,
    const void* base, const void* start, const void* finish, void* out)
    -> void*;
void lyra_rt_unpackedarray_write_mem(
    void* runtime, const void* memory, const void* name, LyraSpan dims,
    const void* base, const void* start);
void lyra_rt_unpackedarray_write_mem_within(
    void* runtime, const void* memory, const void* name, LyraSpan dims,
    const void* base, const void* start, const void* finish);
auto lyra_rt_dynarray_read_mem(
    void* runtime, const void* memory, const void* name, const void* base,
    const void* start, void* out) -> void*;
auto lyra_rt_dynarray_read_mem_within(
    void* runtime, const void* memory, const void* name, const void* base,
    const void* start, const void* finish, void* out) -> void*;
void lyra_rt_dynarray_write_mem(
    void* runtime, const void* memory, const void* name, const void* base,
    const void* start);
void lyra_rt_dynarray_write_mem_within(
    void* runtime, const void* memory, const void* name, const void* base,
    const void* start, const void* finish);
auto lyra_rt_queue_read_mem(
    void* runtime, const void* memory, const void* name, const void* base,
    const void* start, void* out) -> void*;
auto lyra_rt_queue_read_mem_within(
    void* runtime, const void* memory, const void* name, const void* base,
    const void* start, const void* finish, void* out) -> void*;
void lyra_rt_queue_write_mem(
    void* runtime, const void* memory, const void* name, const void* base,
    const void* start);
void lyra_rt_queue_write_mem_within(
    void* runtime, const void* memory, const void* name, const void* base,
    const void* start, const void* finish);
auto lyra_rt_assocarray_read_mem(
    void* runtime, const void* memory, const void* name,
    const void* key_prototype, const void* base, const void* start, void* out)
    -> void*;
auto lyra_rt_assocarray_read_mem_within(
    void* runtime, const void* memory, const void* name,
    const void* key_prototype, const void* base, const void* start,
    const void* finish, void* out) -> void*;
void lyra_rt_assocarray_write_mem(
    void* runtime, const void* memory, const void* name, const void* base,
    const void* start);
void lyra_rt_assocarray_write_mem_within(
    void* runtime, const void* memory, const void* name, const void* base,
    const void* start, const void* finish);

auto lyra_rt_make_packed_range(std::int64_t left, std::int64_t right) -> const
    void*;
auto lyra_rt_make_unpacked_range(std::int64_t left, std::int64_t right) -> const
    void*;
auto lyra_rt_make_packed_type(LyraSpan dims, bool is_signed, bool is_four_state)
    -> const void*;
auto lyra_rt_make_enumeration(const void* base, LyraSpan planes, LyraSpan names)
    -> const void*;

// A packed constant crosses as its own word planes so that no part of its value
// is lost at the boundary: the value plane holds every word of the constant,
// and the unknown plane the X / Z mask a 4-state constant carries (empty when
// it carries none, and always empty for a 2-state one). Its type keeps a
// multi-dimensional value's shape into element and slice access, and whether
// the planes span the width the type describes is checked here, where the
// width is a concrete size.
auto lyra_rt_packed_from_words(
    LyraSpan value_words, LyraSpan unknown_words, const void* type, void* out)
    -> void*;

// LRM 21.3.3 / 5.9: text conformed to a destination's declared shape. An
// integral destination takes it right-justified and an unpacked array of bytes
// left-justified, which is why only the array form carries an element count.
auto lyra_rt_packed_from_string(const void* text, const void* type, void* out)
    -> void*;
auto lyra_rt_unpackedarray_from_string(
    const void* text, const void* element_type, const void* count, void* out)
    -> void*;

// LRM 20.6.2 `$bits` over the domains whose value is a bit stream: how many
// bits the value currently holds, which for an aggregate is its parts' streams
// laid end to end. A packed value answers from its own shape and needs no entry
// here.
auto lyra_rt_string_bitstream_width(const void* value, void* out) -> void*;
auto lyra_rt_dynarray_bitstream_width(const void* value, void* out) -> void*;
auto lyra_rt_unpackedarray_bitstream_width(const void* value, void* out)
    -> void*;

// LRM 6.24.3: the bits a value makes, one entry per domain a fixed-size stream
// is built from; a domain whose width only the running program fixes has no
// entry, because no stream over one is nameable. A value read back from them
// at the shape a prototype states is one entry for every type: the prototype
// crosses with its type, and the stream is read back by that type.
auto lyra_rt_packed_to_bitstream(const void* value, void* out) -> void*;
auto lyra_rt_unpackedarray_to_bitstream(const void* value, void* out) -> void*;
auto lyra_rt_from_bitstream(
    const void* bits, const void* prototype, const void* prototype_type,
    void* out) -> void*;

// LRM 11.4.14.2: a vector's `block`-wide blocks in reversed order, the bits
// inside each block left where they are.
auto lyra_rt_packed_reverse_blocks(
    const void* value, std::int64_t block, void* out) -> void*;

// LRM 20.9 `$countbits` over the domains whose value is a bit stream. An
// aggregate reduces over its parts, so each of these is the same fold seen at a
// different element type.
auto lyra_rt_string_count_bits(
    const void* value, const void* control_bits, void* out) -> void*;
auto lyra_rt_dynarray_count_bits(
    const void* value, const void* control_bits, void* out) -> void*;
auto lyra_rt_unpackedarray_count_bits(
    const void* value, const void* control_bits, void* out) -> void*;

// The whole-value operations of each domain a structure's own functions apply
// to its members (LRM 7.2): a member of any domain is asked them, so each has
// an entry here even where no program asks it of a value of that domain alone.
// Where a domain the language admits has no such operation on this backend
// yet, its entry answers as an operation not carried out; where the language
// gives the domain none, there is no entry and no structure holding one has
// the operation.
auto lyra_rt_packed_bit_identical(const void* lhs, const void* rhs) -> bool;
auto lyra_rt_string_bit_identical(const void* lhs, const void* rhs) -> bool;
auto lyra_rt_real_bit_identical(const void* lhs, const void* rhs) -> bool;
auto lyra_rt_shortreal_bit_identical(const void* lhs, const void* rhs) -> bool;
auto lyra_rt_chandle_bit_identical(const void* lhs, const void* rhs) -> bool;
auto lyra_rt_union_bit_identical(const void* lhs, const void* rhs) -> bool;
auto lyra_rt_tagged_union_bit_identical(const void* lhs, const void* rhs)
    -> bool;
auto lyra_rt_dynarray_bit_identical(const void* lhs, const void* rhs) -> bool;
auto lyra_rt_unpackedarray_bit_identical(const void* lhs, const void* rhs)
    -> bool;
auto lyra_rt_queue_bit_identical(const void* lhs, const void* rhs) -> bool;
auto lyra_rt_assocarray_bit_identical(const void* lhs, const void* rhs) -> bool;
auto lyra_rt_managedref_bit_identical(const void* lhs, const void* rhs) -> bool;
auto lyra_rt_packed_has_unknown(const void* value) -> bool;
auto lyra_rt_string_has_unknown(const void* value) -> bool;
auto lyra_rt_real_has_unknown(const void* value) -> bool;
auto lyra_rt_shortreal_has_unknown(const void* value) -> bool;
auto lyra_rt_chandle_has_unknown(const void* value) -> bool;
auto lyra_rt_union_has_unknown(const void* value) -> bool;
auto lyra_rt_tagged_union_has_unknown(const void* value) -> bool;
auto lyra_rt_dynarray_has_unknown(const void* value) -> bool;
auto lyra_rt_unpackedarray_has_unknown(const void* value) -> bool;
auto lyra_rt_queue_has_unknown(const void* value) -> bool;
auto lyra_rt_assocarray_has_unknown(const void* value) -> bool;
auto lyra_rt_managedref_has_unknown(const void* value) -> bool;
auto lyra_rt_packed_bitstream_width(const void* value, void* out) -> void*;
auto lyra_rt_union_bitstream_width(const void* value, void* out) -> void*;
auto lyra_rt_tagged_union_bitstream_width(const void* value, void* out)
    -> void*;
auto lyra_rt_managedref_bitstream_width(const void* value, void* out) -> void*;
auto lyra_rt_union_count_bits(
    const void* value, const void* control_bits, void* out) -> void*;
auto lyra_rt_tagged_union_count_bits(
    const void* value, const void* control_bits, void* out) -> void*;
auto lyra_rt_managedref_count_bits(
    const void* value, const void* control_bits, void* out) -> void*;
auto lyra_rt_string_to_bitstream(const void* value, void* out) -> void*;
auto lyra_rt_union_to_bitstream(const void* value, void* out) -> void*;
auto lyra_rt_tagged_union_to_bitstream(const void* value, void* out) -> void*;
auto lyra_rt_dynarray_to_bitstream(const void* value, void* out) -> void*;
auto lyra_rt_queue_to_bitstream(const void* value, void* out) -> void*;
auto lyra_rt_assocarray_to_bitstream(const void* value, void* out) -> void*;
auto lyra_rt_managedref_to_bitstream(const void* value, void* out) -> void*;
auto lyra_rt_packed_resolve_tri_state(
    const void* lhs, const void* rhs, void* out) -> void*;
auto lyra_rt_union_resolve_tri_state(
    const void* lhs, const void* rhs, void* out) -> void*;
auto lyra_rt_unpackedarray_resolve_tri_state(
    const void* lhs, const void* rhs, void* out) -> void*;
auto lyra_rt_packed_resolve_wired_and(
    const void* lhs, const void* rhs, void* out) -> void*;
auto lyra_rt_union_resolve_wired_and(
    const void* lhs, const void* rhs, void* out) -> void*;
auto lyra_rt_unpackedarray_resolve_wired_and(
    const void* lhs, const void* rhs, void* out) -> void*;
auto lyra_rt_packed_resolve_wired_or(
    const void* lhs, const void* rhs, void* out) -> void*;
auto lyra_rt_union_resolve_wired_or(const void* lhs, const void* rhs, void* out)
    -> void*;
auto lyra_rt_unpackedarray_resolve_wired_or(
    const void* lhs, const void* rhs, void* out) -> void*;
auto lyra_rt_packed_dominating(
    const void* stronger, const void* weaker, void* out) -> void*;
auto lyra_rt_union_dominating(
    const void* stronger, const void* weaker, void* out) -> void*;
auto lyra_rt_unpackedarray_dominating(
    const void* stronger, const void* weaker, void* out) -> void*;
auto lyra_rt_packed_filled_like(
    const void* prototype, const void* fill, void* out) -> void*;
auto lyra_rt_union_filled_like(
    const void* prototype, const void* fill, void* out) -> void*;
auto lyra_rt_unpackedarray_filled_like(
    const void* prototype, const void* fill, void* out) -> void*;

// Builds one conversion's format specification, and the print item that pairs a
// value with it. Each field arrives as a packed value, as the value model
// routes every compile-time scalar.
auto lyra_rt_make_format_spec(
    const void* kind, const void* width, const void* precision,
    const void* zero_pad, const void* left_align, const void* timeunit_power,
    void* out) -> void*;
auto lyra_rt_packed_make_print_value_item(
    const void* value, const void* spec, void* out) -> void*;
auto lyra_rt_string_make_print_value_item(
    const void* value, const void* spec, void* out) -> void*;
auto lyra_rt_chandle_make_print_value_item(
    const void* value, const void* spec, void* out) -> void*;
auto lyra_rt_managedref_make_print_value_item(
    const void* value, const void* spec, void* out) -> void*;

// A format performed at run time (LRM 21.3.3), where the format string is not a
// literal and so no print item could be built for it at compile time: the text
// is parsed against the arguments as it is rendered. Each argument borrows the
// value it formats, which holds because both belong to the full-expression that
// performs the format. The hierarchical name a `%m` renders and the
// time scale a `%t` is read against are facts of the call site, so they arrive
// as operands rather than being reached from here.
auto lyra_rt_format_runtime(
    const void* format, LyraSpan args, const void* scope_path,
    const void* time_format, const void* timeunit_power, void* out) -> void*;
auto lyra_rt_packed_make_format_arg(const void* value, void* out) -> void*;
auto lyra_rt_string_make_format_arg(const void* value, void* out) -> void*;
// The same operand, carrying the text LRM 21.2.1.6 renders it as -- composed
// where the type that decides it was still in hand, because a format string
// the program computes reaches no directive until it is parsed.
auto lyra_rt_make_patterned_format_arg(
    const void* value, const void* pattern, void* out) -> void*;
// An operand that reads only as that text, there being no other conversion the
// language defines for what it stands for.
auto lyra_rt_make_rendered_format_arg(const void* pattern, void* out) -> void*;
auto lyra_rt_chandle_make_format_arg(const void* value, void* out) -> void*;
auto lyra_rt_managedref_make_format_arg(const void* value, void* out) -> void*;

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
auto lyra_rt_make_dpi_bit_buffer(const void* sv, void* out) -> void*;
auto lyra_rt_make_dpi_logic_buffer(const void* sv, void* out) -> void*;
auto lyra_rt_dpi_bit_buffer_data(void* buffer) -> void*;
auto lyra_rt_dpi_logic_buffer_data(void* buffer) -> void*;
auto lyra_rt_read_canonical_bit_vec(
    const void* src, const void* type, void* out) -> void*;
auto lyra_rt_read_canonical_logic_vec(
    const void* src, const void* type, void* out) -> void*;
// The other direction, where the buffer is the foreign side's and an SV value
// is written out into it: the argument an exported subroutine hands back
// through a pointer its caller owns (LRM 35.5.1.2).
void lyra_rt_write_canonical_bit_vec(void* dst, const void* sv);
void lyra_rt_write_canonical_logic_vec(void* dst, const void* sv);

// A 1-bit 4-state value's `svLogic` scalar encoding (Annex H.10.1.1), which
// crosses as the machine byte the C side declares rather than as a handle.
auto lyra_rt_to_sv_logic(const void* sv) -> std::uint8_t;
auto lyra_rt_from_sv_logic(std::uint8_t encoded, const void* type, void* out)
    -> void*;

// The open-array image (LRM 35.5.6.1, Annex H.12). The value it images crosses
// erased, because an image is element-type-independent and nothing on this side
// could read that representation off anything else; `bounds` is the declared
// `(left, right)` pair of each unpacked dimension, outermost first,
// `element_type` is what the actual's declaration says one element is, and
// `addressable_elements` says an individual value of the element type crosses
// in the same canonical form the image holds it in (Annex H.12.4). The handle
// is what the foreign side receives in place of the actual, and the value entry
// rebuilds one SV value shaped like the prototype a write-back direction hands
// it.
auto lyra_rt_make_dpi_open_array(
    const void* sv, const void* sv_type, LyraSpan bounds,
    const void* element_type, bool addressable_elements, void* out) -> void*;
auto lyra_rt_dpi_open_array_handle(void* image) -> void*;
auto lyra_rt_dpi_open_array_value(
    const void* image, const void* prototype, const void* prototype_type,
    void* out) -> void*;

// A write into the storage a wrapper stands for (LRM 11.5.1), opened in storage
// the writing body gives. The body designates the wrapper's contents within
// it, takes its steps from there, writes the parts it writes where they lie,
// and ends it once the write is over, which is when the wrapper learns what
// the write did.
auto lyra_rt_packed_cell_open_for_write(void* cell, void* out) -> void*;
auto lyra_rt_string_cell_open_for_write(void* cell, void* out) -> void*;
auto lyra_rt_real_cell_open_for_write(void* cell, void* out) -> void*;
auto lyra_rt_shortreal_cell_open_for_write(void* cell, void* out) -> void*;
auto lyra_rt_chandle_cell_open_for_write(void* cell, void* out) -> void*;
auto lyra_rt_managedref_cell_open_for_write(void* cell, void* out) -> void*;
auto lyra_rt_tuple_cell_open_for_write(void* cell, void* out) -> void*;
auto lyra_rt_union_cell_open_for_write(void* cell, void* out) -> void*;
auto lyra_rt_tagged_union_cell_open_for_write(void* cell, void* out) -> void*;
auto lyra_rt_dynarray_cell_open_for_write(void* cell, void* out) -> void*;
auto lyra_rt_unpackedarray_cell_open_for_write(void* cell, void* out) -> void*;
auto lyra_rt_queue_cell_open_for_write(void* cell, void* out) -> void*;
auto lyra_rt_assocarray_cell_open_for_write(void* cell, void* out) -> void*;
auto lyra_rt_packed_ref_open_for_write(void* reference, void* out) -> void*;
auto lyra_rt_string_ref_open_for_write(void* reference, void* out) -> void*;
auto lyra_rt_real_ref_open_for_write(void* reference, void* out) -> void*;
auto lyra_rt_shortreal_ref_open_for_write(void* reference, void* out) -> void*;
auto lyra_rt_chandle_ref_open_for_write(void* reference, void* out) -> void*;
auto lyra_rt_managedref_ref_open_for_write(void* reference, void* out) -> void*;
auto lyra_rt_tuple_ref_open_for_write(void* reference, void* out) -> void*;
auto lyra_rt_union_ref_open_for_write(void* reference, void* out) -> void*;
auto lyra_rt_tagged_union_ref_open_for_write(void* reference, void* out)
    -> void*;
auto lyra_rt_dynarray_ref_open_for_write(void* reference, void* out) -> void*;
auto lyra_rt_unpackedarray_ref_open_for_write(void* reference, void* out)
    -> void*;
auto lyra_rt_queue_ref_open_for_write(void* reference, void* out) -> void*;
auto lyra_rt_assocarray_ref_open_for_write(void* reference, void* out) -> void*;
auto lyra_rt_packed_driver_open_for_write(void* driver, void* out) -> void*;
auto lyra_rt_tuple_driver_open_for_write(void* driver, void* out) -> void*;
auto lyra_rt_union_driver_open_for_write(void* driver, void* out) -> void*;
auto lyra_rt_unpackedarray_driver_open_for_write(void* driver, void* out)
    -> void*;
// The whole of what a write in progress was opened on, designated within it and
// built in `out`.
auto lyra_rt_designate_whole(void* write, void* out) -> void*;
// A step within a write in progress, taken on a place designated within it and
// answering with the part it reaches, designated within the same write, built
// in `out`. An element step tells the write what forming the element did (LRM
// 7.8.7, 7.10.1, 7.4.6); a component is formed by nothing.
auto lyra_rt_dynarray_designate_element(
    const void* designation, const void* index, void* out) -> void*;
auto lyra_rt_unpackedarray_designate_element(
    const void* designation, const void* position, void* out) -> void*;
auto lyra_rt_queue_designate_element(
    const void* designation, const void* index, void* out) -> void*;
auto lyra_rt_assocarray_designate_element(
    const void* designation, const void* index, const void* index_type,
    void* out) -> void*;
auto lyra_rt_tuple_designate_component(
    const void* designation, std::int64_t index, void* out) -> void*;
// A slice of what a write designates, written within it (LRM 7.6), telling the
// write whether an element moved.
void lyra_rt_dynarray_assign_slice(
    const void* designation, const void* start, std::int64_t count,
    const void* replacement);
void lyra_rt_unpackedarray_assign_slice(
    const void* designation, const void* start, std::int64_t count,
    const void* replacement);
// Bits of a packed value a write designates, written where they lie, telling
// the write which bits it reached and what they held (LRM 11.5.1); and those
// bits as they stand, which an assignment operator combines before the write
// (LRM 11.4.1), built in `out`.
void lyra_rt_packed_assign_slice(
    const void* designation, const void* start, std::int64_t count,
    const void* replacement);
auto lyra_rt_packed_read_slice(
    const void* designation, const void* start, std::int64_t count, void* out)
    -> void*;
// Where a write in progress lands: the part a designation names, whose value
// from before the write the write keeps where anything will ask whether the
// write changed what it was opened on (LRM 4.3). Answers with where the part
// lies, which is what is then written.
auto lyra_rt_packed_land(const void* designation) noexcept -> void*;
auto lyra_rt_string_land(const void* designation) noexcept -> void*;
auto lyra_rt_real_land(const void* designation) noexcept -> void*;
auto lyra_rt_shortreal_land(const void* designation) noexcept -> void*;
auto lyra_rt_chandle_land(const void* designation) noexcept -> void*;
auto lyra_rt_empty_land(const void* designation) noexcept -> void*;
auto lyra_rt_tuple_land(const void* designation) noexcept -> void*;
auto lyra_rt_union_land(const void* designation) noexcept -> void*;
auto lyra_rt_tagged_union_land(const void* designation) noexcept -> void*;
auto lyra_rt_dynarray_land(const void* designation) noexcept -> void*;
auto lyra_rt_unpackedarray_land(const void* designation) noexcept -> void*;
auto lyra_rt_queue_land(const void* designation) noexcept -> void*;
auto lyra_rt_assocarray_land(const void* designation) noexcept -> void*;
auto lyra_rt_managedref_land(const void* designation) noexcept -> void*;

// A value written into storage that already holds one of its domain -- an
// element, a member, the contents a write opened -- which takes it where it
// lies rather than being replaced by a new object (LRM 7.6).
void lyra_rt_packed_assign(void* storage, const void* value);
void lyra_rt_string_assign(void* storage, const void* value);
void lyra_rt_real_assign(void* storage, const void* value);
void lyra_rt_shortreal_assign(void* storage, const void* value);
void lyra_rt_chandle_assign(void* storage, const void* value);
void lyra_rt_empty_assign(void* storage, const void* value);
void lyra_rt_union_assign(void* storage, const void* value);
void lyra_rt_tagged_union_assign(void* storage, const void* value);
void lyra_rt_dynarray_assign(void* storage, const void* value);
void lyra_rt_unpackedarray_assign(void* storage, const void* value);
void lyra_rt_queue_assign(void* storage, const void* value);
void lyra_rt_assocarray_assign(void* storage, const void* value);
void lyra_rt_managedref_assign(void* storage, const void* value);
void lyra_rt_reference_assign(void* storage, const void* value);

// Ending an object a generated body built in its own storage, where ending one
// has anything to do; an object whose storage going away is the whole of its
// end has no entry. And the two ways a value reaches further storage: a copy of
// one the body only reads, and a move of one it owns into storage that takes it
// over -- a slot of its own, or what its caller gave for its answer. What a
// move leaves behind is still an object, which the body then ends.
void lyra_rt_packed_destroy(void* object);
void lyra_rt_string_destroy(void* object);
void lyra_rt_union_destroy(void* object);
void lyra_rt_tagged_union_destroy(void* object);
void lyra_rt_dynarray_destroy(void* object);
void lyra_rt_unpackedarray_destroy(void* object);
void lyra_rt_queue_destroy(void* object);
void lyra_rt_assocarray_destroy(void* object);
void lyra_rt_managedref_destroy(void* object);
void lyra_rt_closure_destroy(void* object);
void lyra_rt_hierarchy_segment_destroy(void* object);
void lyra_rt_trigger_destroy(void* object);
void lyra_rt_observation_destroy(void* object);
void lyra_rt_read_report_destroy(void* object);
void lyra_rt_wait_destroy(void* object);
void lyra_rt_dpi_bit_buffer_destroy(void* object);
void lyra_rt_dpi_logic_buffer_destroy(void* object);
void lyra_rt_dpi_open_array_destroy(void* object);
void lyra_rt_channel_cancellation_destroy(void* object);
void lyra_rt_shared_pointer_destroy(void* object);
void lyra_rt_open_write_destroy(void* object);
void lyra_rt_object_write_destroy(void* object);
auto lyra_rt_packed_copy(const void* value, void* out) -> void*;
auto lyra_rt_string_copy(const void* value, void* out) -> void*;
auto lyra_rt_real_copy(const void* value, void* out) -> void*;
auto lyra_rt_shortreal_copy(const void* value, void* out) -> void*;
auto lyra_rt_chandle_copy(const void* value, void* out) -> void*;
auto lyra_rt_empty_copy(const void* value, void* out) -> void*;
auto lyra_rt_union_copy(const void* value, void* out) -> void*;
auto lyra_rt_tagged_union_copy(const void* value, void* out) -> void*;
auto lyra_rt_dynarray_copy(const void* value, void* out) -> void*;
auto lyra_rt_unpackedarray_copy(const void* value, void* out) -> void*;
auto lyra_rt_queue_copy(const void* value, void* out) -> void*;
auto lyra_rt_assocarray_copy(const void* value, void* out) -> void*;
auto lyra_rt_managedref_copy(const void* value, void* out) -> void*;
auto lyra_rt_shared_pointer_copy(const void* value, void* out) -> void*;
auto lyra_rt_print_item_copy(const void* value, void* out) -> void*;
auto lyra_rt_format_spec_copy(const void* value, void* out) -> void*;
auto lyra_rt_format_arg_copy(const void* value, void* out) -> void*;
auto lyra_rt_hierarchy_segment_copy(const void* value, void* out) -> void*;
auto lyra_rt_trigger_copy(const void* value, void* out) -> void*;
auto lyra_rt_observation_copy(const void* value, void* out) -> void*;
auto lyra_rt_dpi_bit_buffer_copy(const void* value, void* out) -> void*;
auto lyra_rt_dpi_logic_buffer_copy(const void* value, void* out) -> void*;
auto lyra_rt_dpi_open_array_copy(const void* value, void* out) -> void*;
auto lyra_rt_channel_cancellation_copy(const void* value, void* out) -> void*;
auto lyra_rt_reference_copy(const void* value, void* out) -> void*;
auto lyra_rt_packed_move(void* value, void* out) -> void*;
auto lyra_rt_string_move(void* value, void* out) -> void*;
auto lyra_rt_real_move(void* value, void* out) -> void*;
auto lyra_rt_shortreal_move(void* value, void* out) -> void*;
auto lyra_rt_chandle_move(void* value, void* out) -> void*;
auto lyra_rt_empty_move(void* value, void* out) -> void*;
auto lyra_rt_union_move(void* value, void* out) -> void*;
auto lyra_rt_tagged_union_move(void* value, void* out) -> void*;
auto lyra_rt_dynarray_move(void* value, void* out) -> void*;
auto lyra_rt_unpackedarray_move(void* value, void* out) -> void*;
auto lyra_rt_queue_move(void* value, void* out) -> void*;
auto lyra_rt_assocarray_move(void* value, void* out) -> void*;
auto lyra_rt_managedref_move(void* value, void* out) -> void*;
auto lyra_rt_closure_move(void* value, void* out) -> void*;
auto lyra_rt_shared_pointer_move(void* value, void* out) -> void*;
auto lyra_rt_print_item_move(void* value, void* out) -> void*;
auto lyra_rt_format_spec_move(void* value, void* out) -> void*;
auto lyra_rt_format_arg_move(void* value, void* out) -> void*;
auto lyra_rt_hierarchy_segment_move(void* value, void* out) -> void*;
auto lyra_rt_trigger_move(void* value, void* out) -> void*;
auto lyra_rt_observation_move(void* value, void* out) -> void*;
auto lyra_rt_read_report_move(void* value, void* out) -> void*;
auto lyra_rt_wait_move(void* value, void* out) -> void*;
auto lyra_rt_dpi_bit_buffer_move(void* value, void* out) -> void*;
auto lyra_rt_dpi_logic_buffer_move(void* value, void* out) -> void*;
auto lyra_rt_dpi_open_array_move(void* value, void* out) -> void*;
auto lyra_rt_channel_cancellation_move(void* value, void* out) -> void*;
auto lyra_rt_reference_move(void* value, void* out) -> void*;

// One member's storage, built in place where its owner was laid out and ended
// there: `lyra_rt_<domain>_<kind>_construct` / `_destroy` for storage over a
// value domain, `lyra_rt_<kind>_construct` / `_destroy` for storage of a kind
// naming no domain. A value held inline is built by copying the value in, so
// it has no entry here; a counted hold on a cell and a channel's cancellation
// view end through their own entries, and a borrowed handle and a reference
// have nothing to end.
void lyra_rt_borrowed_handle_construct(void* storage);
void lyra_rt_reference_construct(void* storage);
void lyra_rt_packed_cell_construct(void* storage);
void lyra_rt_string_cell_construct(void* storage);
void lyra_rt_real_cell_construct(void* storage);
void lyra_rt_shortreal_cell_construct(void* storage);
void lyra_rt_chandle_cell_construct(void* storage);
void lyra_rt_tuple_cell_construct(void* storage);
void lyra_rt_union_cell_construct(void* storage);
void lyra_rt_tagged_union_cell_construct(void* storage);
void lyra_rt_dynarray_cell_construct(void* storage);
void lyra_rt_unpackedarray_cell_construct(void* storage);
void lyra_rt_queue_cell_construct(void* storage);
void lyra_rt_assocarray_cell_construct(void* storage);
void lyra_rt_managedref_cell_construct(void* storage);
void lyra_rt_packed_value_cell_construct(void* storage);
void lyra_rt_string_value_cell_construct(void* storage);
void lyra_rt_real_value_cell_construct(void* storage);
void lyra_rt_shortreal_value_cell_construct(void* storage);
void lyra_rt_chandle_value_cell_construct(void* storage);
void lyra_rt_tuple_value_cell_construct(void* storage);
void lyra_rt_union_value_cell_construct(void* storage);
void lyra_rt_tagged_union_value_cell_construct(void* storage);
void lyra_rt_dynarray_value_cell_construct(void* storage);
void lyra_rt_unpackedarray_value_cell_construct(void* storage);
void lyra_rt_queue_value_cell_construct(void* storage);
void lyra_rt_assocarray_value_cell_construct(void* storage);
void lyra_rt_managedref_value_cell_construct(void* storage);
void lyra_rt_packed_net_construct(void* storage);
void lyra_rt_tuple_net_construct(void* storage);
void lyra_rt_union_net_construct(void* storage);
void lyra_rt_unpackedarray_net_construct(void* storage);
void lyra_rt_packed_sampled_history_construct(void* storage);
void lyra_rt_string_sampled_history_construct(void* storage);
void lyra_rt_real_sampled_history_construct(void* storage);
void lyra_rt_shortreal_sampled_history_construct(void* storage);
void lyra_rt_tuple_sampled_history_construct(void* storage);
void lyra_rt_union_sampled_history_construct(void* storage);
void lyra_rt_tagged_union_sampled_history_construct(void* storage);
void lyra_rt_dynarray_sampled_history_construct(void* storage);
void lyra_rt_unpackedarray_sampled_history_construct(void* storage);
void lyra_rt_queue_sampled_history_construct(void* storage);
void lyra_rt_assocarray_sampled_history_construct(void* storage);
void lyra_rt_managedref_sampled_history_construct(void* storage);
void lyra_rt_named_event_construct(void* storage);
void lyra_rt_cancellation_target_construct(void* storage);
void lyra_rt_evaluation_attempts_construct(void* storage);
void lyra_rt_channel_cancellation_construct(void* storage);
void lyra_rt_shared_pointer_construct(void* storage);
void lyra_rt_packed_cell_destroy(void* storage);
void lyra_rt_string_cell_destroy(void* storage);
void lyra_rt_real_cell_destroy(void* storage);
void lyra_rt_shortreal_cell_destroy(void* storage);
void lyra_rt_chandle_cell_destroy(void* storage);
void lyra_rt_tuple_cell_destroy(void* storage);
void lyra_rt_union_cell_destroy(void* storage);
void lyra_rt_tagged_union_cell_destroy(void* storage);
void lyra_rt_dynarray_cell_destroy(void* storage);
void lyra_rt_unpackedarray_cell_destroy(void* storage);
void lyra_rt_queue_cell_destroy(void* storage);
void lyra_rt_assocarray_cell_destroy(void* storage);
void lyra_rt_managedref_cell_destroy(void* storage);
void lyra_rt_packed_value_cell_destroy(void* storage);
void lyra_rt_string_value_cell_destroy(void* storage);
void lyra_rt_tuple_value_cell_destroy(void* storage);
void lyra_rt_union_value_cell_destroy(void* storage);
void lyra_rt_tagged_union_value_cell_destroy(void* storage);
void lyra_rt_dynarray_value_cell_destroy(void* storage);
void lyra_rt_unpackedarray_value_cell_destroy(void* storage);
void lyra_rt_queue_value_cell_destroy(void* storage);
void lyra_rt_assocarray_value_cell_destroy(void* storage);
void lyra_rt_managedref_value_cell_destroy(void* storage);
void lyra_rt_packed_net_destroy(void* storage);
void lyra_rt_tuple_net_destroy(void* storage);
void lyra_rt_union_net_destroy(void* storage);
void lyra_rt_unpackedarray_net_destroy(void* storage);
void lyra_rt_packed_sampled_history_destroy(void* storage);
void lyra_rt_string_sampled_history_destroy(void* storage);
void lyra_rt_real_sampled_history_destroy(void* storage);
void lyra_rt_shortreal_sampled_history_destroy(void* storage);
void lyra_rt_tuple_sampled_history_destroy(void* storage);
void lyra_rt_union_sampled_history_destroy(void* storage);
void lyra_rt_tagged_union_sampled_history_destroy(void* storage);
void lyra_rt_dynarray_sampled_history_destroy(void* storage);
void lyra_rt_unpackedarray_sampled_history_destroy(void* storage);
void lyra_rt_queue_sampled_history_destroy(void* storage);
void lyra_rt_assocarray_sampled_history_destroy(void* storage);
void lyra_rt_managedref_sampled_history_destroy(void* storage);
void lyra_rt_named_event_destroy(void* storage);
void lyra_rt_cancellation_target_destroy(void* storage);
void lyra_rt_evaluation_attempts_destroy(void* storage);
}
