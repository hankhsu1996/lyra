// $dumplimit bounds the file at a number of bytes: once it reaches them the
// dump stops and a comment says the limit was reached, so no later change is
// recorded however long the simulation goes on (LRM 21.7.1.5). How much of the
// bound the definitions take is the tool's, so no expected dump can say where
// it stops; the bound here is far above any header and far below what the run
// would write without it, and the file is read back one token at a time, which
// is what a free-format dump allows (LRM 21.7.2, 21.3.4.3).
`timescale 1ns / 1ns
module Top;
  logic [7:0] count = 0;

  initial begin
    int fd;
    string token;
    bit defined;
    bit commented;
    int changes_after_comment;

    $dumpfile("limited.vcd");
    $dumplimit(4000);
    $dumpvars;
    repeat (2000) #1 count = count + 1;
    $dumpflush;

    fd = $fopen("limited.vcd", "r");
    while ($fscanf(fd, "%s", token) == 1) begin
      if (token == "$enddefinitions") defined = 1;
      else if (defined && token == "$comment") commented = 1;
      else if (commented && token[0] == "#") changes_after_comment++;
    end
    $fclose(fd);

    if (!commented)
      $fatal(1, "no comment followed the definitions to say the limit was hit");
    if (changes_after_comment != 0)
      $fatal(1, "%0d times were recorded after the limit was hit",
             changes_after_comment);
    $display("All checks passed");
  end
endmodule
