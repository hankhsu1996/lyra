// `vpi_mcd_close` closes the files a descriptor names and answers the part it
// did not close (LRM 38.24), and descriptor 1 is one it cannot close. The
// file descriptors of standard input, output and error are open before the
// design runs (LRM 21.3.1), and whether a close may end them is the tool's.
// Lyra leaves every pre-opened file open, so closing standard output's file
// descriptor answers it unclosed and reports an error (LRM 38.2), and the
// design goes on writing to it. A descriptor naming nothing closes nothing,
// which is no failure.
module Top;
  import "DPI-C" context function int close_and_check(int descriptor);

  int stdout_closed = -1;
  int nothing_closed = -1;

  initial begin
    stdout_closed = close_and_check(32'h8000_0001);
    nothing_closed = close_and_check(0);
    $fdisplay(32'h8000_0001, "still open");
  end

  final begin
    if (stdout_closed !== 1)
      $fatal(1, "closing standard output's descriptor was not refused");
    if (nothing_closed !== 0)
      $fatal(1, "closing a descriptor naming nothing was refused");
    $display("All checks passed");
  end
endmodule
