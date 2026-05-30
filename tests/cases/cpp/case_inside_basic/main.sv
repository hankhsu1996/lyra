module Top;
  int val;
  int matched;
  int multi;

  initial begin
    val = 2;
    matched = 0;
    case (val) inside
      1: matched = 1;
      2: matched = 2;
      3: matched = 3;
    endcase

    multi = 0;
    val = 3;
    case (val) inside
      1, 2, 3: multi = 23;
      4, 5:    multi = 45;
    endcase
  end
endmodule
