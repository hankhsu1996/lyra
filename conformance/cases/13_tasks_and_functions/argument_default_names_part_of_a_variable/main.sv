// A formal's default names what a call omitting the argument binds, in the
// scope containing the declaration (LRM 13.5.3), so an `output` formal's
// default may name part of a variable of that scope, and the call writes back
// into that part alone.
module Child;
  logic [31:0] bits = '0;
  task automatic put_byte(output logic [7:0] b = bits[15:8]);
    b = 8'hab;
  endtask
endmodule

module Top;
  Child c ();

  initial begin
    c.put_byte();
    if (c.bits !== 32'h0000ab00)
      $fatal(1, "a part-select default left %h, expected 0000ab00", c.bits);
    $display("All checks passed");
  end
endmodule
