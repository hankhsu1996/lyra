module Top;
  typedef union packed {
    logic [15:0] view1;
    logic [15:0] view2;
  } my_union_t;
  my_union_t u;
  int result;
  initial begin
    u.view1 = 16'h1234;
    u.view2 = 16'h5678;
    result = u.view1;
  end
endmodule
