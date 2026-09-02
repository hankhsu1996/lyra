// @reports-nothing:
//
// A default is one of the two forms a case_item takes, and it is the item
// selected when no other one matches. A qualified case carrying one therefore
// always selects a case_item, so the no-match violation a unique-case or
// priority-case reports has no execution that reaches it, and the advice that
// specifying unique or priority removes the need to code a default is advice
// only because the two answer for the same values (LRM 12.5.3, Syntax 12-3).
module Top;
  int unique_taken;
  int unique0_taken;
  int priority_taken;

  initial begin
    bit [2:0] value;
    value = 3'd5;

    unique_taken = 0;
    unique case (value)
      3'd0, 3'd1: unique_taken = 1;
      3'd2: unique_taken = 2;
      default: unique_taken = 7;
    endcase

    unique0_taken = 0;
    unique0 case (value)
      3'd0, 3'd1: unique0_taken = 1;
      3'd2: unique0_taken = 2;
      default: unique0_taken = 7;
    endcase

    priority_taken = 0;
    priority case (value)
      3'd0, 3'd1: priority_taken = 1;
      3'd2: priority_taken = 2;
      default: priority_taken = 7;
    endcase
  end

  final begin
    if (unique_taken !== 7)
      $fatal(1, "unique_taken was %0d, expected 7", unique_taken);
    if (unique0_taken !== 7)
      $fatal(1, "unique0_taken was %0d, expected 7", unique0_taken);
    if (priority_taken !== 7)
      $fatal(1, "priority_taken was %0d, expected 7", priority_taken);
    $display("All checks passed");
  end
endmodule
