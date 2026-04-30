module Top;
  function automatic int add(int a, int b);
    return a + b;
  endfunction
  int x;
  if (1) begin : g
    initial x = add(1, 2);
  end
endmodule
