// @reports: violation
//
// With no default to select, a unique-case and a priority-case each assert
// that some case_item matches, so an execution where none does is a violation
// and is reported. No item's statement runs either way (LRM 12.5.3).
module Top;
  int unique_taken;
  int priority_taken;

  initial begin
    bit [2:0] value;
    value = 3'd5;

    unique_taken = 7;
    unique case (value)
      3'd0, 3'd1: unique_taken = 1;
      3'd2: unique_taken = 2;
    endcase

    priority_taken = 7;
    priority case (value)
      3'd0, 3'd1: priority_taken = 1;
      3'd2: priority_taken = 2;
    endcase
  end

  final begin
    if (unique_taken !== 7)
      $fatal(1, "unique_taken was %0d, expected 7", unique_taken);
    if (priority_taken !== 7)
      $fatal(1, "priority_taken was %0d, expected 7", priority_taken);
    $display("All checks passed");
  end
endmodule
