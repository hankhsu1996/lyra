// A case item may name several expressions separated by commas, and the item is
// selected when the case expression matches any one of them. The item
// expressions are evaluated and compared in the order they are written, and
// the search ends at the first that matches, so an expression after it in the
// same list is never evaluated (LRM 12.5) -- nor anything evaluating it would
// take, such as the handle a property is written through (LRM 8.4). That holds
// for casez and for a case inside, which match differently and search the same
// way (LRM 12.5.1, 12.5.4).
module Top;
  class Holder;
    int count;
  endclass

  int calls;
  Holder kept;

  function automatic int counted(int v);
    calls = calls + 1;
    return v;
  endfunction

  function automatic Holder counted_holder();
    calls = calls + 1;
    return kept;
  endfunction

  int sel;
  int list_head;
  int list_tail;
  int outside_list;
  int case_stops = -1;
  int case_continues = -1;
  int casez_stops = -1;
  int case_inside_stops = -1;
  int write_after_match = -1;

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

    sel = 2;
    calls = 0;
    case (sel)
      counted(2), counted(3): ;
      default: calls = calls + 100;
    endcase
    case_stops = calls;

    calls = 0;
    case (sel)
      counted(1), counted(2), counted(3): ;
      default: calls = calls + 100;
    endcase
    case_continues = calls;

    calls = 0;
    casez (sel)
      counted(2), counted(3): ;
      default: calls = calls + 100;
    endcase
    casez_stops = calls;

    calls = 0;
    case (sel) inside
      counted(2), counted(3): ;
      default: calls = calls + 100;
    endcase
    case_inside_stops = calls;

    kept = new;
    calls = 0;
    case (sel)
      counted(2), (counted_holder().count = 2): ;
      default: calls = calls + 100;
    endcase
    write_after_match = calls;
  end

  final begin
    if (list_head !== 12)
      $fatal(1, "list_head was %0d, expected 12", list_head);
    if (list_tail !== 12)
      $fatal(1, "list_tail was %0d, expected 12", list_tail);
    if (outside_list !== 99)
      $fatal(1, "outside_list was %0d, expected 99", outside_list);
    if (case_stops !== 1)
      $fatal(1, "a case list evaluated %0d expressions to reach its first, expected 1",
             case_stops);
    if (case_continues !== 2)
      $fatal(1, "a case list evaluated %0d expressions to reach its second, expected 2",
             case_continues);
    if (casez_stops !== 1)
      $fatal(1, "a casez list evaluated %0d expressions to reach its first, expected 1",
             casez_stops);
    if (case_inside_stops !== 1)
      $fatal(1, "a case inside list evaluated %0d expressions to reach its first, expected 1",
             case_inside_stops);
    if (write_after_match !== 1 || kept.count !== 0)
      $fatal(1, "a write after the matching expression ran %0d calls and left %0d, expected 1 and 0",
             write_after_match, kept.count);
    $display("All checks passed");
  end
endmodule
