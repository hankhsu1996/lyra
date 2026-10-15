// A class specialization named by a value a loop's index or a parameter gives,
// and read nowhere else (LRM 8.25). Each block and each instance reaches the
// specialization its own value names, so each answers with that value.
class C #(parameter int K = 0);
  static function int k();
    return K;
  endfunction
endclass

module Use #(parameter int B = 0) (output int v);
  initial v = C#(B)::k();
endmodule

module Top;
  int v[4];
  int v5, v7;

  for (genvar b = 0; b < 4; b++) begin : g
    initial v[b] = C#(b)::k();
  end

  Use #(.B(5)) u5 (.v(v5));
  Use #(.B(7)) u7 (.v(v7));

  initial begin
    #1;
    if (v[0] !== 0 || v[1] !== 1 || v[2] !== 2 || v[3] !== 3)
      $fatal(1, "blocks answered %0d %0d %0d %0d", v[0], v[1], v[2], v[3]);
    if (v5 !== 5 || v7 !== 7) $fatal(1, "instances answered %0d and %0d", v5, v7);
    $display("All checks passed");
  end
endmodule
