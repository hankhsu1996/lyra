// Exporting a DPI-C task is not yet supported: only functions are lowered so
// far (LRM 35.5.2). The declaration is well-formed, so slang accepts it and the
// unsupported-construct diagnostic comes from lowering, not the frontend.
module Top;
  export "DPI-C" task do_work;

  task do_work(input int x);
    $display("x=%0d", x);
  endtask
endmodule
