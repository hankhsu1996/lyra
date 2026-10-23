// A `with` filter keeps the values of a bin's range list for which its
// expression holds, `item` standing for each, and may call a function; a bin
// naming its coverpoint stands for all its values (LRM 19.5.1.1).
module Top;
  bit [7:0] x;

  function automatic bit odd(bit [7:0] value);
    return value[0];
  endfunction

  covergroup cg;
    a: coverpoint x {
      bins mod3[] = {[0:15]} with (item % 3 == 0);
      bins odds[] = a with (odd(item) && item < 8);
    }
  endgroup

  int covered = -1, total = -1;

  initial begin
    static cg g = new;
    real ignored;
    x = 3; g.sample();
    x = 5; g.sample();
    x = 4; g.sample();
    ignored = g.get_inst_coverage(covered, total);
  end

  final begin
    // mod3[0, 3, ..., 15] and odds[1, 3, 5, 7]; 3 hits two bins, 5 one.
    if (covered !== 3 || total !== 10)
      $fatal(1, "%0d of %0d bins covered, expected 3 of 10", covered, total);
    $display("All checks passed");
  end
endmodule
