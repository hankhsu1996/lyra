module Child;
  int x;
  initial begin
    x = 1;
    #1 x = 7;
  end
endmodule

module Top;
  Child c();
  int r;
  always_comb r = c.x;
  initial begin
    #2;
    $display("r=%0d", r);
  end
endmodule
