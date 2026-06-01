module Top;
  int result;

  function int add(int a, int b);
    return a + b;
  endfunction

  initial begin
    result = add(17, 25);
  end
endmodule
