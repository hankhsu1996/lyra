// Writing a package variable from a package subroutine is not yet supported: the
// write fires subscribers, which needs the runtime services, and a receiver-less
// callable (LRM 26.3) has no `self` to reach them through. Reading a package
// variable from a subroutine works; writing awaits threading the services into a
// receiver-less callable. The declaration is well-formed, so the diagnostic
// comes from lowering, not the frontend.
package pkg;
  int cnt = 0;

  function automatic void bump();
    cnt = cnt + 1;
  endfunction
endpackage

module Top;
  initial pkg::bump();
endmodule
