module Top;
  logic [7:0] r;

  function void f(output logic [7:0] v);
    if (v === 8'hxx) v = 8'hAA;
    else v = 8'h55;
  endfunction

  initial begin
    f(r);
  end
endmodule
