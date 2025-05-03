module Test;
  int a, b, sel, c;
  initial begin
    a = 1;
    b = 2;
    sel = 0;
    #5 sel = 1;
    #5 sel = 0;
    #5 sel = 1;
    #5 sel = 0;

    $finish();
  end

  always_comb begin
    if (sel == 0) begin
      c = a;
    end else begin
      c = b;
    end
  end

endmodule
