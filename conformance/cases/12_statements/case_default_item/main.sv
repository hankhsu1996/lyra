// The default item's statement executes when every case item comparison fails,
// and the default item is itself ignored during the linear search, so an item
// written after it is still compared and still selected when it matches
// (LRM 12.5). A case whose only item is default always executes it, and casez
// is used in the same way as a plain case (LRM 12.5.1).
module Top;
  int sel;
  int all_failed;
  int item_after_default;
  int only_item;
  int wildcard_only_item;

  initial begin
    sel = 5;
    all_failed = 0;
    case (sel)
      0: all_failed = 1;
      1: all_failed = 2;
      default: all_failed = 100;
    endcase

    sel = 1;
    item_after_default = 0;
    case (sel)
      0: item_after_default = 1;
      default: item_after_default = 100;
      1: item_after_default = 2;
    endcase

    sel = 5;
    only_item = 0;
    case (sel)
      default: only_item = 42;
    endcase

    sel = 5;
    wildcard_only_item = 0;
    casez (sel)
      default: wildcard_only_item = 7;
    endcase
  end

  final begin
    if (all_failed !== 100)
      $fatal(1, "all_failed was %0d, expected 100", all_failed);
    if (item_after_default !== 2)
      $fatal(1, "item_after_default was %0d, expected 2", item_after_default);
    if (only_item !== 42)
      $fatal(1, "only_item was %0d, expected 42", only_item);
    if (wildcard_only_item !== 7)
      $fatal(1, "wildcard_only_item was %0d, expected 7", wildcard_only_item);
    $display("All checks passed");
  end
endmodule
