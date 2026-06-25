module Test;
  function automatic int add(int a, int b);
    return a + b;
  endfunction

  if (1) begin : g
    initial begin
      int r;
      r = add(40, 2);
      $display("r=%0d", r);
    end
  end
endmodule
