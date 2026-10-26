#include "lyra/runtime/construction_table.hpp"

#include <cstddef>
#include <cstdint>

#include "lyra/runtime/scope_info.hpp"

namespace lyra::runtime {

namespace {

auto BlockAt(
    const ConstructionTable* table, std::int64_t generate, std::int64_t place)
    -> const Instantiation& {
  return table->generates.Entries()[static_cast<std::size_t>(generate)]
      .places.Entries()[static_cast<std::size_t>(place)];
}

}  // namespace

auto InstantiationEntry(const ConstructionTable* table, std::int64_t at)
    -> ErasedEntry {
  return table->instantiations.Entries()[static_cast<std::size_t>(at)].entry;
}

auto InstantiationTable(const ConstructionTable* table, std::int64_t at)
    -> const ConstructionTable* {
  return table->instantiations.Entries()[static_cast<std::size_t>(at)].table;
}

auto BlockEntry(
    const ConstructionTable* table, std::int64_t generate, std::int64_t place)
    -> ErasedEntry {
  return BlockAt(table, generate, place).entry;
}

auto BlockTable(
    const ConstructionTable* table, std::int64_t generate, std::int64_t place)
    -> const ConstructionTable* {
  return BlockAt(table, generate, place).table;
}

}  // namespace lyra::runtime
