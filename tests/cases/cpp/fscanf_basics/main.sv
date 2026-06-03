module Top;
  // LRM 21.3.4.3: $fscanf shares Table 21-7 with $sscanf. This case proves
  // the file source actually works end-to-end across the conversion-spec
  // set; the scanner core itself is already exercised by sscanf cases.
  // The test is self-contained: write input via $fopen("w")+$fwrite, then
  // read it back via $fopen("r")+$fscanf (same pattern as fread_int).
  int    fd, code;
  int    a;
  logic [15:0] b;
  string s;
  initial begin
    fd = $fopen("scan_input.txt", "w");
    $fdisplay(fd, "42 dead hello");
    $fclose(fd);

    fd = $fopen("scan_input.txt", "r");
    code = $fscanf(fd, "%d %h %s", a, b, s);
    $fclose(fd);
  end
endmodule
