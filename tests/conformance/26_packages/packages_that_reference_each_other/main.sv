// Two packages may each reference the other's declarations by resolved name
// (LRM 26.2, 26.3). What the standard fixes about their variables is a barrier
// and not an order: a package's variable declaration assignments occur before
// any initial or always procedure is started (LRM 26.2), and nothing orders one
// package's against another's. So every value checked here is fixed by the
// barrier alone -- each package initializes its own variable from a constant,
// and the references that cross sit in function bodies a procedure calls after
// every package is initialized.
package base_pkg;
  int scale = 5;

  // Reaches into the package that reaches back into this one. A subroutine runs
  // when it is called rather than while the package is brought up, so what it
  // reads is fixed by the barrier.
  function automatic int scaled_offset();
    return offset_pkg::offset * scale;
  endfunction
endpackage

package offset_pkg;
  int offset = 6;

  function automatic int doubled_scale();
    return base_pkg::scale * 2;
  endfunction
endpackage

module Top;
  int scaled_read;
  int doubled_read;

  initial begin
    scaled_read = base_pkg::scaled_offset();
    doubled_read = offset_pkg::doubled_scale();
  end

  final begin
    if (scaled_read !== 30)
      $fatal(1, "scaled_read was %0d, expected 30", scaled_read);
    if (doubled_read !== 10)
      $fatal(1, "doubled_read was %0d, expected 10", doubled_read);
    $display("All checks passed");
  end
endmodule
