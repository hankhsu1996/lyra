// `vpi_printf` writes to the tool's output channel and answers how many
// characters it wrote (LRM 38.30), and `vpi_vprintf` does the same from a list
// (38.41). That channel is descriptor 1 (38.24), so `vpi_mcd_printf` names it
// beside a file's channel to write both at once (38.28), and `vpi_flush` and
// `vpi_mcd_flush` of it answer 0 for success (38.5, 38.25). What reached the
// file is read back once it is closed.
module Top;
  import "DPI-C" context function int print(string text);
  import "DPI-C" context function int print_from_a_list(string text);
  import "DPI-C" context function int print_to(int mcd, string text);
  import "DPI-C" context function int flush_tool_output();
  import "DPI-C" context function int flush_channel(int mcd);

  string file_name = "tool_output_beside.txt";

  int printed = -1;
  int listed = -1;
  int both = -1;
  int flushed = -1;
  int channel_flushed = -1;
  int file = 0;

  function automatic string contents(string name);
    int fd = $fopen(name, "r");
    string all = "";
    string line;
    if (fd == 0) return "<not readable>";
    while ($fgets(line, fd) != 0) all = {all, line};
    $fclose(fd);
    return all;
  endfunction

  initial begin
    file = $fopen(file_name);
    printed = print("printed by vpi_printf\n");
    listed = print_from_a_list("printed by vpi_vprintf\n");
    both = print_to(1 | file, "to the tool and a file\n");
    flushed = flush_tool_output();
    channel_flushed = flush_channel(1);
    $fclose(file);
  end

  final begin
    if (printed !== 22)
      $fatal(1, "vpi_printf answered %0d characters, expected 22", printed);
    if (listed !== 23)
      $fatal(1, "vpi_vprintf answered %0d characters, expected 23", listed);
    if (both !== 23)
      $fatal(1, "vpi_mcd_printf answered %0d characters, expected 23", both);
    if (flushed !== 0)
      $fatal(1, "vpi_flush answered %0d, expected 0", flushed);
    if (channel_flushed !== 0)
      $fatal(1, "vpi_mcd_flush(1) answered %0d, expected 0", channel_flushed);
    if (contents(file_name) != "to the tool and a file\n")
      $fatal(1, "the file held '%s'", contents(file_name));
    $display("All checks passed");
  end
endmodule
