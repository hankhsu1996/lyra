// The case item expressions are compared in the exact order in which they
// appear, and the first item that matches executes its statement and ends the
// search, so a later item that also matches has no effect (LRM 12.5). casez and
// casex are used in the same way, so an earlier do-not-care item is taken ahead
// of a later exact one (LRM 12.5.1).
module Top;
  int sel;
  logic [3:0] code;
  int duplicate_label;
  int wildcard_first;

  initial begin
    sel = 2;
    duplicate_label = 0;
    case (sel)
      2: duplicate_label = 1;
      2: duplicate_label = 2;
      2: duplicate_label = 3;
    endcase

    code = 4'b0101;
    wildcard_first = 0;
    casez (code)
      4'b01??: wildcard_first = 1;
      4'b0101: wildcard_first = 2;
    endcase
  end

  final begin
    if (duplicate_label !== 1)
      $fatal(1, "duplicate_label was %0d, expected 1", duplicate_label);
    if (wildcard_first !== 1)
      $fatal(1, "wildcard_first was %0d, expected 1", wildcard_first);
    $display("All checks passed");
  end
endmodule
