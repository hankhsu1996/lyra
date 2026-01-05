#pragma once

// Platform requirements check for Lyra-generated code.
// This header is automatically included by sdk.hpp.

#include <version>

#if !defined(__cpp_lib_print) || !defined(__cpp_lib_expected) || \
    !defined(__cpp_impl_coroutine)
#error \
    "Lyra requires a C++23 compiler with coroutines, <print>, and <expected> support. Please upgrade your compiler."
#endif
