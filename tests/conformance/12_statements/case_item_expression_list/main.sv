// A case item may name several expressions separated by commas, and the item is
// selected when the case expression matches any one of them (LRM 12.5).
module Top;
  int sel;
  int list_head;
  int list_tail;
  int outside_list;

  initial begin
    sel = 1;
    list_head = 0;
    case (sel)
      0:    list_head = 1;
      1, 2: list_head = 12;
      3:    list_head = 3;
      default: list_head = 99;
    endcase

    sel = 2;
    list_tail = 0;
    case (sel)
      0:    list_tail = 1;
      1, 2: list_tail = 12;
      3:    list_tail = 3;
      default: list_tail = 99;
    endcase

    sel = 3;
    outside_list = 0;
    case (sel)
      0:    outside_list = 1;
      1, 2: outside_list = 12;
      default: outside_list = 99;
    endcase
  end

  final begin
    if (list_head !== 12)
      $fatal(1, "list_head was %0d, expected 12", list_head);
    if (list_tail !== 12)
      $fatal(1, "list_tail was %0d, expected 12", list_tail);
    if (outside_list !== 99)
      $fatal(1, "outside_list was %0d, expected 99", outside_list);
    $display("All checks passed");
  end
endmodule
