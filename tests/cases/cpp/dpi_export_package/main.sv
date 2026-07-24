`timescale 1ns / 1ps
// LRM 35.7 package-scoped export: a package function has no instance and no
// receiver, so its foreign-linkage wrapper calls the package's free function
// directly and threads the run's services, unlike a module export that recovers a
// calling instance. The context import `call_pkg` reaches the exported `triple`
// through the import -> export chain; a result of 21 (7 tripled) proves the
// receiver-less wrapper ran.
package pkg;
  export "DPI-C" function triple;
  function int triple(input int x);
    return x * 3;
  endfunction
endpackage

module Top;
  import "DPI-C" context function int call_pkg(input int x);
  initial $display("pkg=%0d", call_pkg(7));
endmodule
