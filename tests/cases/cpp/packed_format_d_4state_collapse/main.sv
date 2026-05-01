module Top;
  logic [7:0] v;

  initial begin
    v = 8'bxxxxxxxx; $display("%d", v);
    v = 8'bzzzzzzzz; $display("%d", v);
    v = 8'bxxxx0101; $display("%d", v);
    v = 8'bzzzz0101; $display("%d", v);
    v = 8'bx01z0101; $display("%d", v);
  end
endmodule
