    module Test;
      bit b0, b1;
      int zero, nonzero;
      bit r1, r2, r3, r4;
      initial begin
        b0 = 0;
        b1 = 1;
        zero = 0;
        nonzero = 42;

        // Logical NOT with different types
        r1 = !b0;      // !0 = 1
        r2 = !b1;      // !1 = 0
        r3 = !zero;    // !0 = 1
        r4 = !nonzero; // !42 = 0
      end
    endmodule
