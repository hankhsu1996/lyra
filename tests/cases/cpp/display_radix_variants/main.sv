module Top;
  initial begin
    // Same value through the four variants -- LRM 21.2.1.1 default radix.
    $display(8'hAB);
    $displayb(8'hAB);
    $displayh(8'hAB);
    $displayo(8'hAB);
    // Explicit %X directives override the variant's default radix.
    $displayb("decimal via %%d in $displayb: %d", 8'hAB);
    $displayh("binary via %%b in $displayh: %b", 4'b1010);
    // 4-state operands respect the per-radix X/Z rules (LRM 21.2.1.3).
    $displayb(4'b10xz);
    $displayh(8'b0000_xxxx);
    $displayh(8'b0011_zzzz);
  end
endmodule
