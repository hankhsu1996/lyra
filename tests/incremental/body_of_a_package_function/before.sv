// @remakes: pkg
//
// One function of a package answers another value, on the line it was written
// on, and what it takes and returns stays as it was. A caller is compiled
// against what a function declares, so neither module calling into the package
// means anything else.

package pkg;
  function automatic int first();
    return 1;
  endfunction
  function automatic int second();
    return 2;
  endfunction
endpackage

module CallsFirst;
  int seen;
  initial seen = pkg::first();
endmodule

module CallsSecond;
  int seen;
  initial seen = pkg::second();
endmodule

module Top;
  CallsFirst a ();
  CallsSecond b ();
endmodule
