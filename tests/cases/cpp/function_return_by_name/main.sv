module Top;
  int result;

  function int square(input int n);
    square = n * n;
  endfunction

  initial begin
    result = square(6);
  end
endmodule
