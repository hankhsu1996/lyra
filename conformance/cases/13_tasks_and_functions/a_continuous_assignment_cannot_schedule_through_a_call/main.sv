// @reports: error
//
// A function may hold a nonblocking assignment, but scheduling what cannot
// become active until the function returns is allowed only on a thread an
// initial or always procedure created, or a fork of one, and a tool issues an
// error otherwise (LRM 13.4.4). A continuous assignment is neither, so the
// assignment it makes through the call is refused once the call reaches it.
// While `go` is 0 the call schedules nothing, which is no error.
module Top;
  bit go;
  int x, y;

  function int schedule_when(bit when);
    if (when) x <= 1;
    return 0;
  endfunction

  assign y = schedule_when(go);

  initial begin
    #1;
    $display("Ready for the report");
    go = 1;
    #1;
    $display("All checks passed");
  end
endmodule
