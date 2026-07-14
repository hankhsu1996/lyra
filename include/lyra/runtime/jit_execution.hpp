#pragma once

#include <cstdint>

// The execution-strategy-neutral ABI the generated module calls. Every runtime
// value crosses as an opaque pointer; the runtime owns its type and lifetime.
// Definitions wrap the runtime; a host resolves these symbols when it loads a
// generated module (JIT-compiled, AOT-linked, or interpreted).
extern "C" {

struct LyraSpan {
  void* data;
  std::uint64_t count;
};

auto lyra_rt_services(void* self) -> void*;
auto lyra_rt_files(void* services) -> void*;
auto lyra_rt_time_format(void* services) -> const void*;
auto lyra_rt_make_string(void* cstr) -> void*;
auto lyra_rt_make_print_literal_item(void* string_value) -> void*;
auto lyra_rt_format(LyraSpan items, const void* time_format) -> void*;
auto lyra_rt_packed_const(
    std::int64_t value, std::int32_t width, bool is_signed, bool is_four_state)
    -> void*;
void lyra_rt_writeln(void* files, void* descriptor, void* text);
void lyra_rt_write(void* files, void* descriptor, void* text);
// Wraps a generated process body in a runtime-owned coroutine. `ramp` starts
// the body's own coroutine: called with the receiver it reaches its members
// through, it runs to the body's first suspension and yields that coroutine's
// handle. The runtime owns the coroutine the engine schedules and drives the
// generated one through its handle; the generated body never owns the
// scheduler's coroutine.
auto lyra_rt_make_coroutine(void* (*ramp)(void* env), void* env) -> void*;
void lyra_rt_register_initial(void* self, void* coroutine);
void lyra_rt_register_final(void* self, void* coroutine);

// Registers the running process to wake after `ticks` steps of its scope's
// precision (`precision_power`), the registration a delay's suspend edge is
// preceded by (LRM 9.4.1). A zero delay re-enqueues on the current slot's
// inactive region; a positive one scales to the engine's global tick. The
// counts cross as opaque packed values, like every scalar. The wakeup source is
// the running process itself, read from the runtime; no token crosses the
// boundary.
void lyra_rt_delay(
    void* services, const void* ticks, const void* precision_power);

// Builds a scope's structural identity from its base label and per-dimension
// indices (a span of 32-bit index values, empty for a scalar). The segment is
// a transient runtime value owned by the current call scope.
auto lyra_rt_make_segment(void* label, LyraSpan indices) -> void*;

// Allocates a generic instance of `definition`, runs its construct entry to
// build its subtree, and returns the owning handle to the caller (which hands
// it to `lyra_rt_add_owned_child`). `definition` is an opaque cross-unit
// reference the generated code never inspects.
auto lyra_rt_make_unit(
    const void* definition, void* parent, void* segment, void* services)
    -> void*;

// Attaches a freshly built child to its parent, transferring ownership into the
// runtime tree; returns the child as a borrowed scope handle.
auto lyra_rt_add_owned_child(void* parent, void* child) -> void*;

// The address of a generic instance's member storage, by class-local index.
auto lyra_rt_member_addr(void* self, std::uint32_t index) -> void*;

// Publishes a member cell under its source-level name for by-name navigation.
void lyra_rt_register_signal(void* self, const void* name, void* cell);

// Observable storage cell operations, reached through the cell's address. The
// entry names the cell's value domain; the runtime never inspects a type tag.
auto lyra_rt_cell_packed_get(void* cell) -> const void*;
void lyra_rt_cell_packed_initialize(void* cell, const void* prototype);
void lyra_rt_cell_packed_set(void* cell, void* services, const void* value);
auto lyra_rt_cell_string_get(void* cell) -> const void*;
void lyra_rt_cell_string_initialize(void* cell, const void* prototype);
void lyra_rt_cell_string_set(void* cell, void* services, const void* value);

// One entry per operator per value domain: the generated module names the entry
// it means, so no operator code crosses the boundary. Each is the library peer
// of the C++ operator a native target would emit. The result is a transient
// value owned by the current call scope.
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
auto lyra_rt_packed_convert_from(const void* src, const void* prototype)
    -> void*;
auto lyra_rt_packed_from_bool(bool value) -> void*;
auto lyra_rt_packed_to_int64(const void* value) -> std::int64_t;
auto lyra_rt_packed_is_unknown(const void* value) -> void*;
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
auto lyra_rt_packed_reduction_and(const void* value) -> void*;
auto lyra_rt_packed_reduction_or(const void* value) -> void*;
auto lyra_rt_packed_reduction_xor(const void* value) -> void*;
auto lyra_rt_packed_reduction_nand(const void* value) -> void*;
auto lyra_rt_packed_reduction_nor(const void* value) -> void*;
auto lyra_rt_packed_reduction_xnor(const void* value) -> void*;

auto lyra_rt_string_from_packed_array(const void* bits) -> void*;
auto lyra_rt_string_len(const void* value) -> void*;
auto lyra_rt_string_getc(const void* value, const void* index) -> void*;
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

auto lyra_rt_string_add(const void* lhs, const void* rhs) -> void*;
auto lyra_rt_string_eq(const void* lhs, const void* rhs) -> void*;
auto lyra_rt_string_ne(const void* lhs, const void* rhs) -> void*;
auto lyra_rt_string_lt(const void* lhs, const void* rhs) -> void*;
auto lyra_rt_string_le(const void* lhs, const void* rhs) -> void*;
auto lyra_rt_string_gt(const void* lhs, const void* rhs) -> void*;
auto lyra_rt_string_ge(const void* lhs, const void* rhs) -> void*;

// Builds one conversion's format specification, and the print item that pairs a
// value with it. Each field arrives as a packed value, as the value model
// routes every compile-time scalar.
auto lyra_rt_make_format_spec_of_kind(const void* kind) -> void*;
auto lyra_rt_make_format_spec(
    const void* kind, const void* width, const void* precision,
    const void* zero_pad, const void* left_align, const void* timeunit_power)
    -> void*;
auto lyra_rt_make_print_value_item_packed(const void* value, const void* spec)
    -> void*;
auto lyra_rt_make_print_value_item_string(const void* value, const void* spec)
    -> void*;
}
