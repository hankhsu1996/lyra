module Top;
  int sel;
  int matched;
  int unmatched;
  initial begin
    matched = 0;
    unmatched = 99;

    sel = 2;
    case (1)
      sel == 1: matched = 5;
      sel == 2: matched = 7;
      sel == 3: matched = 9;
    endcase

    sel = 99;
    case (1)
      sel == 1: unmatched = 1;
      sel == 2: unmatched = 2;
    endcase
  end
endmodule
