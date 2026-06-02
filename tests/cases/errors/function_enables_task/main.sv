module Top;
  logic [7:0] r;

  task set_one(output logic [7:0] v);
    v = 8'd1;
  endtask

  function logic [7:0] bad();
    logic [7:0] tmp;
    set_one(tmp);
    return tmp;
  endfunction

  initial begin
    r = bad();
  end
endmodule
