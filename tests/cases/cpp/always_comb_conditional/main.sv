module Top;
  int a, b, out;
  bit sel;

  initial begin
    a = 3;
    b = 7;
    sel = 1;
  end

  always_comb begin
    if (sel)
      out = b;
    else
      out = a;
  end
endmodule
