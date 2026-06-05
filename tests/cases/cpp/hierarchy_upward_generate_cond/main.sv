module Reader;
  int p, q;
  always_comb p = Top.bp.v;   // bp resolves to its then arm, bq to its else arm
  always_comb q = Top.bq.v;
endmodule

module Top;
  // Both alternatives of one conditional generate may share a name (LRM 27.5);
  // a reference binds whichever was instantiated, independent of the condition.
  generate
    if (1) begin : bp
      int v = 5;
    end
    else begin : bp
      int v = 9;
    end
    if (0) begin : bq
      int v = 6;
    end
    else begin : bq
      int v = 8;
    end
  endgenerate
  Reader rd();
  initial begin
    #1;
    $display("%0d %0d", rd.p, rd.q);
  end
endmodule
