# A part is named by how it is selected

Date: 2026-09-27 Status: accepted. Revises the `Get` / `GetRef` spelling in
[compound-assignment-write-location](compound-assignment-write-location.md) and
[unpacked-union-representation](unpacked-union-representation.md).

## Context

A value's part is selected one of three ways (LRM 7.2, 7.3, 7.4.5, 7.4.6, 7.8, 7.10): an element, by
a coordinate the program computes; a component, by a declaration-order position; a slice, by a run
of consecutive elements. Each is reached by the same four operations: reading its value, reaching it
for a write in place, reaching it within a write in progress, and building a whole with it replaced.

Element and slice were spelled one way through every layer (`element` / `element_ref`, `Element` /
`ElementRef`, `with_element`). The component was spelled five ways: a `part` / `part_ref` entry
pair, whose read was published as `extract`; `Get` / `GetRef` on the typed C++ values; `Component` /
`ComponentRef` on the erased product but `Member` on the erased unions, with `SetActive` and
`SetMember` for the write; `update` for the functional write; a `Part` selector and a
`PartProjection` step below MIR. `Get` was also the whole-value read of every capability wrapper, so
one name meant two operations. And "part" was at the same time the general word for all three kinds.

## Decision

**"Part" is the general word; each kind of part is named for how it is selected, and every operation
on one is that name plus a fixed suffix, in every layer.**

| Operation                 | Element                  | Component                          | Slice                |
| ------------------------- | ------------------------ | ---------------------------------- | -------------------- |
| Read the value            | `element`                | `component`                        | `slice`              |
| Reach it for a write      | `element_ref`            | `component_ref`                    | `slice_ref`          |
| Reach it within a write   | `designate_element`      | `designate_component`              | `designate_slice`    |
| Refer to it by reference  | `refer_element`          | `refer_component`                  | --                   |
| Build the whole, replaced | `with_element`           | `with_component`                   | `with_slice`         |
| C++ method, read / reach  | `Element` / `ElementRef` | `Component<I>` / `ComponentRef<I>` | `Slice` / `SliceRef` |

The selector below MIR, the place step, the library containers' methods and the runtime entries use
the same word. "Component" is the word the position type and the selection kind already used, and
"member" stays free for a member of a scope or a class, which is a different selection.

## Rejected

- **`Get` / `GetRef`, after `std::get<I>`.** The records it revises chose it because `std::get` is
  the standard accessor of the `std::tuple` and `std::variant` behind a struct and a union. That
  names the realization, not the operation, and the same records state the rule it breaks: each
  access family is a value / reference pair named `X` / `XRef`. `Get` is also every wrapper's
  whole-value read, so the name no longer says which of the two a call is.
- **`part` for the component.** "Part" is what all three kinds are; using it for one of them makes
  "a part" ambiguous in every sentence and identifier that means all three.
- **`member`.** A member is selected by name out of a scope or a class, and the LIR place step that
  reaches one is already called that.
