# SystemVerilog Standard Interface Headers

Vendored normative header files from the IEEE Std 1800-2023 SystemVerilog
standard, pinned as Lyra-owned third-party artifacts.

## Contents

| File                  | Source                      | Purpose                                      |
| --------------------- | --------------------------- | -------------------------------------------- |
| `svdpi.h`             | IEEE Std 1800-2023, Annex I | DPI-C foreign function interface             |
| `vpi_user.h`          | IEEE Std 1800-2023, Annex K | VPI routines, objects and properties         |
| `vpi_compatibility.h` | IEEE Std 1800-2023, Annex L | VPI compatibility modes, included by `vpi_user.h` |
| `sv_vpi_user.h`       | IEEE Std 1800-2023, Annex M | VPI extensions for SystemVerilog             |

## Provenance

Vendored from the IEEE 1800-2023 standard (approved 6 December 2023,
published 28 February 2024) and kept identical but for the corrections recorded below. These headers
define the standard C ABI contract between SystemVerilog simulators and user
DPI and VPI code.

`svdpi.h`, `vpi_user.h`, `vpi_compatibility.h` and `sv_vpi_user.h` were taken
from the source code listings of Annexes I, K, L and M of the standard's PDF, and checked
token by token against those listings. Four changes were made, each where the
PDF's typesetting broke a line or a character, or the listing left out what its
own clause says:

- In `vpi_compatibility.h`, the `#error` message the PDF breaks across two lines
  is one line, as a string literal has to be.
- In `vpi_compatibility.h`, each test that no second mode symbol is defined runs
  over three lines in the PDF, and the second and third carry on the first with
  a backslash; without one they were text inside the group, so the test read
  two symbols of five and defining two modes compiled, where LRM 36.12.2.1 says
  it is an error.
- In `vpi_user.h` and `sv_vpi_user.h`, the comment on `vpiIfElse` and on
  `vpiIfElseOp` writes "if-else" with a hyphen where the PDF typesets an en
  dash, since the repository holds ASCII text alone.
- In `vpi_compatibility.h`, the 1800-2017 and 1800-2023 modes define
  `VPI_COMPATIBILITY_VERSION_1800v2012` as `1`, the value 36.12.2.1 gives every
  mode symbol, rather than as nothing. The listing defines it empty, which
  leaves the `#elif` that tests it with no expression, so an application
  selecting either mode could not compile.

## Patch Policy

Do not modify these headers for style, modernization, or Lyra-internal
conventions. They are standard ABI contracts, not internal code, and the
`.clang-format` beside them turns the formatter off so that no repository-wide
pass rewrites them.

Permitted changes (must be individually auditable):

- Minimal portability fixes required by the Lyra build environment
- Corrections that make a listing compile as the header it states, each recorded
  above
- Whitespace normalization (no semantic change)
- A typeset character in a comment written in ASCII, each recorded above

Prohibited changes:

- Typedef redesign or renaming
- Struct field renaming
- Function declaration style changes
- Adding Lyra-specific extensions
- Any change that alters the ABI surface

## Boundary

Every header is shipped beside a program's generated DPI-C header, so a user's
foreign sources compile against the standard declarations. The runtime defines
the DPI and VPI routines it has against `svdpi.h` and `vpi_user.h` themselves,
so the compiler holds each definition to the standard's prototype. The runtime
also holds the canonical representation of a packed value in `svdpi.h`'s own
types, so `svdpi.h` ships among the runtime's headers too.
