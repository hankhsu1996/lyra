#pragma once

#include <cstdint>
#include <optional>
#include <span>
#include <string_view>

namespace lyra::support {

// A class the runtime library defines whose objects generated code reaches but
// never lays out: the scope every object of the design hierarchy is (LRM 23.3),
// the part every object the program builds starts with (LRM 8.3), and the
// object a process is (LRM 9.7). It belongs to no compilation unit, so which
// one it is is the whole of its identity, and how a target spells it is that
// target's own answer. Where a class of the source extends one, generated code
// places the class's own storage after it and nothing inside it.
//
// Two layers name it -- a type of each stands for a value of one -- so it lives
// beside them rather than in either.
enum class RuntimeClass : std::uint8_t {
  kScope,
  kObject,
  kProcess,
};

// What a dump calls it.
auto RuntimeClassName(RuntimeClass klass) -> std::string_view;

// A virtual function one of those classes declares for a class extending it to
// override: the three phases the runtime drives every scope through, in the
// order it enters them -- every route and alias is bound, then every cell takes
// the value its declaration gives it (LRM 10.5), then every process is created
// (LRM 9.2). Listed in the order the class declares them, which is the order
// they take in its table.
enum class LibraryVirtual : std::uint8_t {
  kScopeResolve,
  kScopeInitialize,
  kScopeCreateProcesses,
};

// The class declaring it.
auto DeclaringClassOf(LibraryVirtual function) -> RuntimeClass;

// The virtual functions `klass` declares itself, in the order it declares
// them, and the class it extends where it extends one: the scope extends the
// part every object starts with.
auto VirtualsDeclaredBy(RuntimeClass klass) -> std::span<const LibraryVirtual>;
auto LibraryBaseOf(RuntimeClass klass) -> std::optional<RuntimeClass>;

// Its position among the virtual functions its class introduces.
auto OrdinalOf(LibraryVirtual function) -> std::uint32_t;

// Its name in the library's C++, which an override spells too. Every one starts
// with `sv_`, which a name the source writes never does once it reaches C++,
// so no subroutine of a design can override one by having its name.
auto LibraryVirtualName(LibraryVirtual function) -> std::string_view;

}  // namespace lyra::support
