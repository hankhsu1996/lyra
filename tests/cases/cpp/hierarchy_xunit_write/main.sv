module Child;
  int x;
endmodule

module Top;
  Child c();
  int r;
  initial begin
    c.x = 99;
    #1;
    r = c.x;
    $display("r=%0d", r);
  end
endmodule
