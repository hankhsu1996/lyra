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
auto lyra_rt_make_coroutine(void (*entry)(void*), void* env) -> void*;
void lyra_rt_register_initial(void* self, void* coroutine);
void lyra_rt_register_final(void* self, void* coroutine);
}
