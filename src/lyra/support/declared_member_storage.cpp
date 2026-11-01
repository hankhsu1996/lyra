#include "lyra/support/declared_member_storage.hpp"

#include <string_view>
#include <variant>

#include "lyra/base/overloaded.hpp"
#include "lyra/support/library_decl.hpp"
#include "lyra/support/runtime_object.hpp"

namespace lyra::support {

auto MemberStorageName(const DeclaredMemberStorage& storage)
    -> std::string_view {
  using Name = std::string_view;
  return std::visit(
      Overloaded{
          [](const ObservableCellMember&) -> Name { return "cell"; },
          [](const ResolvedNetMember&) -> Name { return "net"; },
          [](const SampledHistoryMember&) -> Name { return "sampled_history"; },
          [](const ValueCellMember&) -> Name { return "value_cell"; },
          [](const InlineValueMember&) -> Name { return "inline_value"; },
          [](const InlineBytesMember&) -> Name { return "inline_bytes"; },
          [](const BorrowedHandleMember&) -> Name { return "borrowed_handle"; },
          [](const LibraryDeclMember& m) -> Name {
            return OwnObjectEntries(m.decl);
          },
          [](const LibraryObjectMember& m) -> Name {
            return RuntimeObjectName(m.object);
          }},
      storage);
}

}  // namespace lyra::support
