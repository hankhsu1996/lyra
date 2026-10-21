// @reports: of a type not equivalent to the variable asking for it
//
// A get of a message from a mailbox of no stated type, into a variable whose
// type is not equivalent to the message's, is a run-time error (LRM 15.4.5);
// the message is not received, so it stays, and a later peek and get into a
// variable of its own type receive it.
module Top;
  mailbox box;
  string wrong = "unchanged";
  int peeked = 0;
  int got = 0;
  int left_after_mismatch = -1;

  initial begin
    box = new;
    box.put(42);
    box.get(wrong);
    left_after_mismatch = box.num();
    box.peek(peeked);
    box.get(got);
  end

  final begin
    if (wrong != "unchanged") $fatal(1, "a mismatched get wrote '%s'", wrong);
    if (left_after_mismatch !== 1)
      $fatal(1, "%0d messages were left after a mismatch, expected 1",
             left_after_mismatch);
    if (peeked !== 42 || got !== 42)
      $fatal(1, "peek got %0d and get got %0d, expected 42", peeked, got);
    if (box.num() !== 0) $fatal(1, "%0d messages were left", box.num());
    $display("All checks passed");
  end
endmodule
