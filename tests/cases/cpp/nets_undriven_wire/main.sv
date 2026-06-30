// An undriven net reads its net type's undriven value (LRM 6.6.1): a `wire`
// with no driver is high-impedance at its full width. The net is a readable,
// well-typed observable from construction, so reading it before (or without)
// any driver yields `z`, never an uninitialized cell. The output port here
// attaches no driver because the child never drives `o`.
module Source(output wire [7:0] o);
endmodule

module Top;
  wire [7:0] w;
  Source u(.o(w));
  initial begin
    #1;
    $display("undriven=%b", w);
  end
endmodule
