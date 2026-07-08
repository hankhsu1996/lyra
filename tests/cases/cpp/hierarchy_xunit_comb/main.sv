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
  int r_assign;
  always_comb r = a.x + b.x;
  // A continuous assignment reading the same cross-instance sources takes
  // the same reactive path as `always_comb` -- both re-evaluate when the
  // source's observable cell fires, regardless of what wrote it.
  assign r_assign = a.x + b.x;
  initial begin
    #2 $display("r=%0d r_assign=%0d", r, r_assign);
    a.x = 100;
    #1 $display("r=%0d r_assign=%0d", r, r_assign);
    b.x = 200;
    #1 $display("r=%0d r_assign=%0d", r, r_assign);
  end
endmodule
