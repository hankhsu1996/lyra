// Partial X collapses to lowercase x for %c (any X bit dominates).
module Top;
  logic [7:0] c;
  initial begin
    c = 8'b0000_xxxx;
    $display("%c", c);
  end
endmodule
