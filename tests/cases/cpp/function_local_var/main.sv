module Top;
  int result;

  function int compute(int x);
    int temp;
    temp = x + 10;
    return temp * 2;
  endfunction

  initial begin
    result = compute(11);
  end
endmodule
