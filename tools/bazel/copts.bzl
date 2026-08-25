"""Compiler flags applied to Lyra's own C++ targets.

Scoped per-target rather than set build-wide so that every dependency keeps
its own warning policy.
"""

# -Wall / -Wextra are the baseline. They live here rather than build-wide so a
# dependency is never compiled under Lyra's warning policy, which is what makes
# it safe to keep the set strict: no suppression is ever owed to third-party
# code.
#
# -Wswitch catches a no-default switch that omits an enumerator;
# -Wreturn-type catches falling off the end of a non-void function.
#
# -Wno-unused-command-line-argument: the header-parse action is handed both
# -fsyntax-only and -c, so the driver reports -c as unused once per header.
# The flags come from the toolchain's generated command line, not from any
# source here, so the diagnostic can only ever be noise.
LYRA_COPTS = [
    "-Wall",
    "-Wextra",
    "-Werror=switch",
    "-Werror=return-type",
    "-Wno-unused-command-line-argument",
]
