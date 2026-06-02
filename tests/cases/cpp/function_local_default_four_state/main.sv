module Top;
  logic [7:0] result;

  function logic [7:0] probe;
    logic [7:0] y;
    return y;
  endfunction

  initial begin
    result = probe();
  end
endmodule
