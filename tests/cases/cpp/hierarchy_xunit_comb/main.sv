module Child;
  int x;
  initial begin
    x = 1;
    #1 x = 7;
  end
endmodule

module Top;
  Child a();
  Child b();
  int r;
  always_comb r = a.x + b.x;
  initial begin
    #2 $display("r=%0d", r);
    a.x = 100;
    #1 $display("r=%0d", r);
    b.x = 200;
    #1 $display("r=%0d", r);
  end
endmodule
