// A case item's statement may be null, so selecting that item executes nothing
// and control resumes with the statement following endcase (LRM 12.5).
module Top;
  int sel;
  int result;

  initial begin
    // The default item stands so that selecting the null item is told apart
    // from selecting no item at all.
    sel = 2;
    result = 0;
    case (sel)
      1: result = 10;
      2: ;
      3: result = 30;
      default: result = 40;
    endcase
    result = result + 1;
  end

  final begin
    if (result !== 1) $fatal(1, "result was %0d, expected 1", result);
    $display("All checks passed");
  end
endmodule
