module Top;
  int result;

  function int square(int x);
    return x * x;
  endfunction

  initial begin
    result = square(3) + square(4);
  end
endmodule
