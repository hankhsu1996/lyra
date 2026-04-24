# SystemVerilog Standard Interface Headers

Vendored normative header files from the IEEE Std 1800-2023 SystemVerilog
standard, pinned as Lyra-owned third-party artifacts.

## Contents

| File      | Source                      | Purpose                          |
| --------- | --------------------------- | -------------------------------- |
| `svdpi.h` | IEEE Std 1800-2023, Annex I | DPI-C foreign function interface |

## Provenance

Vendored from the IEEE 1800-2023 standard (approved 6 December 2023,
published 28 February 2024) and kept semantically identical. These headers
define the standard C ABI contract between SystemVerilog simulators and user
DPI code.

## Patch Policy

Do not modify these headers for style, modernization, or Lyra-internal
conventions. They are standard ABI contracts, not internal code.

Permitted changes (must be individually auditable):

- Minimal portability fixes required by the Lyra build environment
- Whitespace normalization (no semantic change)

Prohibited changes:

- Typedef redesign or renaming
- Struct field renaming
- Function declaration style changes
- Adding Lyra-specific extensions
- Any change that alters the ABI surface

## Boundary Isolation

`svdpi.h` must only be included from DPI boundary code and external companion
test sources. The standard C ABI surface is frozen at that boundary; internal
Lyra code uses Lyra-owned types and helpers.

Examples:

- `src/lyra/runtime/svdpi_runtime.cpp`
- DPI companion C test sources
