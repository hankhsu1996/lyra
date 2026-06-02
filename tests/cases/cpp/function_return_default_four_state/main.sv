module Top;
  logic [7:0] result;

  function logic [7:0] unwritten(input logic [7:0] n);
  endfunction

  initial begin
    result = unwritten(8'hAA);
  end
endmodule
