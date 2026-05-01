module Top;
  logic [7:0] v;

  initial begin
    v = 8'bxxxx0101; $display("%h", v);
    v = 8'bzzzz0101; $display("%h", v);
    v = 8'bx01z0101; $display("%h", v);
    v = 8'b1z01z011; $display("%h", v);
    v = 8'bz0010001; $display("%h", v);
  end
endmodule
