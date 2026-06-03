module Child;
  int a;
  initial begin
    a = 5;
    unique if (a > 0) ;
    else if (a > 3) ;
    else if (a > 4) ;
  end
endmodule

module Top;
  Child c();
endmodule
