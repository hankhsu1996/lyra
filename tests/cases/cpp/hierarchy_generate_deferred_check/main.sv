module Top;
  if (1) begin : g
    int a;
    initial begin
      a = 5;
      unique if (a > 0) ;
      else if (a > 3) ;
      else if (a > 4) ;
    end
  end
endmodule
