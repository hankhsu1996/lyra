// Two packages may each reference the other's declarations by resolved name
// (LRM 26.2, 26.3). Nothing in the language orders the packages against each
// other, so a case may only rely on what the standard fixes: every package's
// variable declaration assignments happen before any initial or always
// procedure starts (LRM 26.2). This pair therefore references in both
// directions while the values stay ordered -- only one of the two initializers
// reads the other package, and the reference back sits in a function body that
// no initializer calls, so what each variable holds is what the standard
// requires rather than a choice a tool made.
package base_pkg;
  int scale = 5;

  // Reaches back into the package that reads this one. A subroutine runs when
  // it is called rather than while the package is brought up, so this reference
  // orders nothing.
  function automatic int scaled_offset();
    return offset_pkg::offset * scale;
  endfunction
endpackage

package offset_pkg;
  int offset = base_pkg::scale + 1;
endpackage

module Top;
  int offset_read;
  int scaled_read;

  initial begin
    offset_read = offset_pkg::offset;
    scaled_read = base_pkg::scaled_offset();
  end

  final begin
    if (offset_read !== 6)
      $fatal(1, "offset_read was %0d, expected 6", offset_read);
    if (scaled_read !== 30)
      $fatal(1, "scaled_read was %0d, expected 30", scaled_read);
    $display("All checks passed");
  end
endmodule
