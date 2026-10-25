#include "lyra/runtime/construction_table.hpp"

#include <cstddef>
#include <cstdint>

#include "lyra/runtime/scope_info.hpp"

namespace lyra::runtime {

auto InstantiationEntry(const ConstructionTable* table, std::int64_t at)
    -> ErasedEntry {
  return table->instantiations.Entries()[static_cast<std::size_t>(at)].entry;
}

auto InstantiationTable(const ConstructionTable* table, std::int64_t at)
    -> const ConstructionTable* {
  return table->instantiations.Entries()[static_cast<std::size_t>(at)].table;
}

auto BlockTable(const ConstructionTable* table, std::int64_t at)
    -> const ConstructionTable* {
  return table->blocks.Entries()[static_cast<std::size_t>(at)];
}

}  // namespace lyra::runtime
