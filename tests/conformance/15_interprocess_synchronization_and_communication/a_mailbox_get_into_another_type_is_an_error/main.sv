// @reports: error
//
// A mailbox of no stated type keeps the type of each message put into it, and
// a get into a variable whose type is not equivalent to the message's is a
// run-time error (LRM 15.4.5, 15.4.9). What becomes of the message and of the
// variable the standard does not say, so nothing is read after the error.
module Top;
  mailbox box;
  string wrong = "unchanged";

  initial begin
    box = new;
    box.put(42);
    $display("Ready for the report");
    box.get(wrong);
    $display("All checks passed");
  end
endmodule
