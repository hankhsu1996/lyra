module Top;
  int result;

  function int nothing(input int n);
  endfunction

  initial begin
    result = nothing(7);
  end
endmodule
