module Child;
  int x;
  initial begin
    x = 0;
    #1 x = x + 1;
    #1 x = x + 1;
    $display("x=%0d", x);
  end
endmodule

module Top;
  Child c[3]();
endmodule
