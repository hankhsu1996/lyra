module Top;
  logic [3:0] a [3];
  logic [3:0] b [3];
  logic [3:0] c [3];
  logic case_ne_same, case_ne_diff;
  initial begin
    a[0] = 4'b1010; b[0] = 4'b1010; c[0] = 4'b1010;
    a[1] = 4'b10x0; b[1] = 4'b10x0; c[1] = 4'b1010;
    a[2] = 4'b1111; b[2] = 4'b1111; c[2] = 4'b1111;
    case_ne_same = (a !== b);  // X positions match -> 0
    case_ne_diff = (a !== c);  // X vs 0 mismatch -> 1
  end
endmodule
