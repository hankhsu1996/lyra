module Top;
  logic [3:0] a;
  logic y;
  initial begin
    y = +a;
    y = -a;
    y = ~a;
    y = !a;
    y = &a;
    y = |a;
    y = ^a;
    y = ~&a;
    y = ~|a;
    y = ~^a;
  end
endmodule
