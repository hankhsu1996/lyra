// A constant expression may stand as the case expression, and its value is what
// the case item expressions are compared against (LRM 12.5.2). The item
// expressions need not be constant themselves (LRM 12.5), so selecting on the
// constant 1 executes the item holding the first condition that is true.
module Top;
  int encode;
  int sel;
  int single_condition;
  int first_true_wins;
  int list_head;
  int list_tail;
  int none_true;

  initial begin
    encode = 2;
    single_condition = 0;
    case (1)
      encode == 1: single_condition = 1;
      encode == 2: single_condition = 2;
      encode == 3: single_condition = 3;
      default: single_condition = 99;
    endcase

    // Three of the conditions hold at once, and the one that runs is the
    // first of them, which is what makes this form a priority encoder.
    encode = 6;
    first_true_wins = 0;
    case (1)
      encode > 9: first_true_wins = 1;
      encode > 5: first_true_wins = 2;
      encode > 3: first_true_wins = 3;
      encode > 1: first_true_wins = 4;
      default: first_true_wins = 99;
    endcase

    sel = 1;
    list_head = 0;
    case (1)
      sel == 0, sel == 2: list_head = 11;
      sel == 1, sel == 3: list_head = 22;
      default: list_head = 99;
    endcase

    sel = 3;
    list_tail = 0;
    case (1)
      sel == 0, sel == 2: list_tail = 11;
      sel == 1, sel == 3: list_tail = 22;
      default: list_tail = 99;
    endcase

    sel = 7;
    none_true = 0;
    case (1)
      sel == 0, sel == 2: none_true = 11;
      sel == 1, sel == 3: none_true = 22;
      default: none_true = 99;
    endcase
  end

  final begin
    if (single_condition !== 2)
      $fatal(1, "single_condition was %0d, expected 2", single_condition);
    if (first_true_wins !== 2)
      $fatal(1, "first_true_wins was %0d, expected 2", first_true_wins);
    if (list_head !== 22)
      $fatal(1, "list_head was %0d, expected 22", list_head);
    if (list_tail !== 22)
      $fatal(1, "list_tail was %0d, expected 22", list_tail);
    if (none_true !== 99)
      $fatal(1, "none_true was %0d, expected 99", none_true);
    $display("All checks passed");
  end
endmodule
