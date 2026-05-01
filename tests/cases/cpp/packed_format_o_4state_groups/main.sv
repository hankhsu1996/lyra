module Top;
  logic [5:0] v;

  initial begin
    v = 6'bxxx101; $display("%o", v);
    v = 6'bzzz101; $display("%o", v);
    v = 6'bx0z101; $display("%o", v);
    v = 6'bz00z01; $display("%o", v);
    v = 6'b1z0001; $display("%o", v);
  end
endmodule
