`timescale 1ns / 1ps
module Top;
  import "DPI-C" context function int scope_ends_with(input string suffix);
  import "DPI-C" context function int setscope_roundtrip();
  import "DPI-C" context function int userdata_roundtrip();
  import "DPI-C" context function int check_time();

  initial begin
    $display("observe=%0d", scope_ends_with("Top"));
    $display("setscope=%0d", setscope_roundtrip());
    $display("userdata=%0d", userdata_roundtrip());
    #5;
    $display("time=%0d", check_time());
  end

  // A context import observes the scope of its declaration, not of its call
  // site: called from the generate block, `scope_ends_with` still declared at
  // module scope observes `Top`, not `Top.g`.
  if (1) begin : g
    initial begin
      #6;
      $display("nested=%0d", scope_ends_with("Top"));
    end
  end
endmodule
