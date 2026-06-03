module Child;
  int x;
  initial x = 42;
endmodule

module Top;
  Child c();
  int r;
  initial begin
    #1;
    r = c.x;
    $display("r=%0d", r);
  end
endmodule
