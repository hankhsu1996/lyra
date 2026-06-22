#include "lyra/support/builtin_fn.hpp"

#include "lyra/base/internal_error.hpp"

namespace lyra::support {

auto IsMutatingBuiltinFn(BuiltinFn id) -> bool {
  switch (id) {
    case BuiltinFn::kPutc:
    case BuiltinFn::kItoa:
    case BuiltinFn::kHextoa:
    case BuiltinFn::kOcttoa:
    case BuiltinFn::kBintoa:
    case BuiltinFn::kRealtoa:
    case BuiltinFn::kDelete:
    case BuiltinFn::kReverse:
    case BuiltinFn::kSort:
    case BuiltinFn::kRsort:
    case BuiltinFn::kInsert:
    case BuiltinFn::kPopFront:
    case BuiltinFn::kPopBack:
    case BuiltinFn::kPushFront:
    case BuiltinFn::kPushBack:
      return true;
    default:
      return false;
  }
}

auto IsContainerAccessFn(BuiltinFn id) -> bool {
  switch (id) {
    case BuiltinFn::kElement:
    case BuiltinFn::kElementRef:
    case BuiltinFn::kSlice:
    case BuiltinFn::kSliceRef:
      return true;
    default:
      return false;
  }
}

auto ArrayMethodTakesClosure(BuiltinFn id) -> bool {
  switch (id) {
    case BuiltinFn::kSort:
    case BuiltinFn::kRsort:
    case BuiltinFn::kSum:
    case BuiltinFn::kProduct:
    case BuiltinFn::kAnd:
    case BuiltinFn::kOr:
    case BuiltinFn::kXor:
    case BuiltinFn::kFind:
    case BuiltinFn::kFindIndex:
    case BuiltinFn::kFindFirst:
    case BuiltinFn::kFindFirstIndex:
    case BuiltinFn::kFindLast:
    case BuiltinFn::kFindLastIndex:
    case BuiltinFn::kMin:
    case BuiltinFn::kMax:
    case BuiltinFn::kUnique:
    case BuiltinFn::kUniqueIndex:
    case BuiltinFn::kMap:
      return true;
    default:
      return false;
  }
}

auto IsAssociativeTraversalFn(BuiltinFn id) -> bool {
  switch (id) {
    case BuiltinFn::kAssocFirst:
    case BuiltinFn::kAssocLast:
    case BuiltinFn::kAssocNext:
    case BuiltinFn::kAssocPrev:
      return true;
    default:
      return false;
  }
}

auto BuiltinFnName(BuiltinFn id) -> std::string_view {
  switch (id) {
    case BuiltinFn::kElement:
      return "element";
    case BuiltinFn::kElementRef:
      return "element_ref";
    case BuiltinFn::kSlice:
      return "slice";
    case BuiltinFn::kSliceRef:
      return "slice_ref";
    case BuiltinFn::kSize:
      return "size";
    case BuiltinFn::kLen:
      return "len";
    case BuiltinFn::kToOwned:
      return "to_owned";
    case BuiltinFn::kDelete:
      return "delete";
    case BuiltinFn::kReverse:
      return "reverse";
    case BuiltinFn::kSort:
      return "sort";
    case BuiltinFn::kRsort:
      return "rsort";
    case BuiltinFn::kSum:
      return "sum";
    case BuiltinFn::kProduct:
      return "product";
    case BuiltinFn::kAnd:
      return "and";
    case BuiltinFn::kOr:
      return "or";
    case BuiltinFn::kXor:
      return "xor";
    case BuiltinFn::kFind:
      return "find";
    case BuiltinFn::kFindIndex:
      return "find_index";
    case BuiltinFn::kFindFirst:
      return "find_first";
    case BuiltinFn::kFindFirstIndex:
      return "find_first_index";
    case BuiltinFn::kFindLast:
      return "find_last";
    case BuiltinFn::kFindLastIndex:
      return "find_last_index";
    case BuiltinFn::kMin:
      return "min";
    case BuiltinFn::kMax:
      return "max";
    case BuiltinFn::kUnique:
      return "unique";
    case BuiltinFn::kUniqueIndex:
      return "unique_index";
    case BuiltinFn::kMap:
      return "map";
    case BuiltinFn::kInsert:
      return "insert";
    case BuiltinFn::kPopFront:
      return "pop_front";
    case BuiltinFn::kPopBack:
      return "pop_back";
    case BuiltinFn::kPushFront:
      return "push_front";
    case BuiltinFn::kPushBack:
      return "push_back";
    case BuiltinFn::kExists:
      return "exists";
    case BuiltinFn::kAssocFirst:
      return "assoc_first";
    case BuiltinFn::kAssocLast:
      return "assoc_last";
    case BuiltinFn::kAssocNext:
      return "assoc_next";
    case BuiltinFn::kAssocPrev:
      return "assoc_prev";
    case BuiltinFn::kGetc:
      return "getc";
    case BuiltinFn::kPutc:
      return "putc";
    case BuiltinFn::kToupper:
      return "toupper";
    case BuiltinFn::kTolower:
      return "tolower";
    case BuiltinFn::kCompare:
      return "compare";
    case BuiltinFn::kIcompare:
      return "icompare";
    case BuiltinFn::kSubstr:
      return "substr";
    case BuiltinFn::kAtoi:
      return "atoi";
    case BuiltinFn::kAtohex:
      return "atohex";
    case BuiltinFn::kAtooct:
      return "atooct";
    case BuiltinFn::kAtobin:
      return "atobin";
    case BuiltinFn::kAtoreal:
      return "atoreal";
    case BuiltinFn::kItoa:
      return "itoa";
    case BuiltinFn::kHextoa:
      return "hextoa";
    case BuiltinFn::kOcttoa:
      return "octtoa";
    case BuiltinFn::kBintoa:
      return "bintoa";
    case BuiltinFn::kRealtoa:
      return "realtoa";
    case BuiltinFn::kTrigger:
      return "trigger";
    case BuiltinFn::kAwait:
      return "await";
    case BuiltinFn::kTriggered:
      return "triggered";
    case BuiltinFn::kEnumFirst:
      return "enum_first";
    case BuiltinFn::kEnumLast:
      return "enum_last";
    case BuiltinFn::kEnumNum:
      return "enum_num";
    case BuiltinFn::kEnumName:
      return "enum_name";
    case BuiltinFn::kEnumNext:
      return "enum_next";
    case BuiltinFn::kEnumPrev:
      return "enum_prev";
    case BuiltinFn::kIsUnknown:
      return "is_unknown";
    case BuiltinFn::kGet:
      return "get";
    case BuiltinFn::kSet:
      return "set";
    case BuiltinFn::kMutate:
      return "mutate";
    case BuiltinFn::kServices:
      return "services";
  }
  throw InternalError("BuiltinFnName: unknown BuiltinFn");
}

}  // namespace lyra::support
