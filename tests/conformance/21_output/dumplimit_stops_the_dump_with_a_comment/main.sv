// $dumplimit bounds the file at a number of bytes: once it reaches them the
// dump stops and a comment says the limit was reached, so no later change is
// recorded however long the simulation goes on (LRM 21.7.1.5).
module Top;
  logic [7:0] count = 0;

  int last_time;
  bit commented;
  int bytes;

  initial begin
    int fd;
    string line;
    bit defined;

    $dumpfile("limited.vcd");
    $dumplimit(400);
    $dumpvars;
    repeat (200) #1 count = count + 1;
    $dumpflush;

    last_time = -1;
    fd = $fopen("limited.vcd", "r");
    while ($fgets(line, fd) > 0) begin
      bytes += line.len();
      if (line.substr(0, 14) == "$enddefinitions") defined = 1;
      else if (defined && line.substr(0, 7) == "$comment") commented = 1;
      else if (line.len() > 1 && line[0] == "#")
        last_time = line.substr(1, line.len() - 1).atoi();
    end
    $fclose(fd);
  end

  final begin
    if (!commented)
      $fatal(1, "no comment followed the definitions to say the limit was hit");
    if (last_time < 0 || last_time >= 200)
      $fatal(1, "changes were recorded up to time %0d, expected fewer",
             last_time);
    if (bytes > 1000)
      $fatal(1, "the file held %0d bytes, expected it to stop near 400", bytes);
    $display("All checks passed");
  end
endmodule
