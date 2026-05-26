module Top;
  logic and_tt, and_tf, and_ff;
  logic or_tt, or_tf, or_ff;
  logic not_t, not_f;
  initial begin
    logic [3:0] a;
    logic [3:0] b;
    a = 4'b1010; b = 4'b0011;
    and_tt = a && b;
    a = 4'b1010; b = 4'b0000;
    and_tf = a && b;
    a = 4'b0000; b = 4'b0000;
    and_ff = a && b;
    a = 4'b1010; b = 4'b0011;
    or_tt = a || b;
    a = 4'b1010; b = 4'b0000;
    or_tf = a || b;
    a = 4'b0000; b = 4'b0000;
    or_ff = a || b;
    a = 4'b1010;
    not_t = !a;
    a = 4'b0000;
    not_f = !a;
  end
endmodule
