// A generate loop whose blocks each declare a vector as wide as a constant
// table says at the block's index (LRM 27.4, 6.20.2). The table holds three
// widths however long it is, so the blocks are three texts' worth of work at
// any N. N is how many blocks the loop counts out.
module Top #(parameter int N = 4);
  logic [7:0] a;
  logic [N-1:0] y;
  typedef logic [7:0] width_t;

  function automatic width_t [N-1:0] widths();
    for (int k = 0; k < N; k += 1) begin
      widths[k] = 8'(1 << (k % 3));
    end
  endfunction

  localparam width_t [N-1:0] kWidths = widths();

  for (genvar i = 0; i < N; i += 1) begin : g
    localparam int kWidth = kWidths[i];
    logic [kWidth-1:0] part;
    assign part = a[kWidth-1:0];
    assign y[i] = ^part;
  end
endmodule
