// A variable declared with the type of an indexed part-select whose position
// a loop index, or a parameter, gives (LRM 6.23). Each block and each instance
// declares it with the type its own select has, so a bit selected at the
// position the select started from is the select's lowest bit there.
module Take #(parameter int B = 0) (input logic [15:0] a, output logic y);
  always_comb begin
    var type(a[B*8+:8]) t;
    t = a[B*8+:8];
    y = t[B*8];
  end
endmodule

module Top;
  logic [15:0] a;
  logic v[2];
  logic y0, y1;

  for (genvar b = 0; b < 2; b++) begin : g
    always_comb begin
      var type(a[b*8+:8]) t;
      t = a[b*8+:8];
      v[b] = t[b*8];
    end
  end

  Take #(.B(0)) k0 (.a(a), .y(y0));
  Take #(.B(1)) k1 (.a(a), .y(y1));

  initial begin
    a = 16'h0100;
    #1;
    if (v[0] !== 1'b0 || v[1] !== 1'b1) $fatal(1, "blocks read %b and %b", v[0], v[1]);
    if (y0 !== 1'b0 || y1 !== 1'b1) $fatal(1, "instances read %b and %b", y0, y1);
    a = 16'h0001;
    #1;
    if (v[0] !== 1'b1 || v[1] !== 1'b0)
      $fatal(1, "after a change blocks read %b and %b", v[0], v[1]);
    if (y0 !== 1'b1 || y1 !== 1'b0)
      $fatal(1, "after a change instances read %b and %b", y0, y1);
    $display("All checks passed");
  end
endmodule
