module Top;
  // LRM 21.3.4.3: $fscanf shares Table 21-7 with $sscanf. Cover the
  // `%[*][digits]spec` modifier set through the file source so file-side
  // pushback / EOF semantics are also exercised under the new parser.
  int fd;
  int w_d, w_h, w_b;
  string w_s;
  int count_w;
  int sup_a, sup_b;
  int count_sup;
  initial begin
    fd = $fopen("scan_input.txt", "w");
    $fdisplay(fd, "123 abc 1111 hello");
    $fdisplay(fd, "10 20 30");
    $fclose(fd);

    fd = $fopen("scan_input.txt", "r");
    count_w = $fscanf(fd, "%3d %3h %4b %5s", w_d, w_h, w_b, w_s);
    count_sup = $fscanf(fd, "%d %*d %d", sup_a, sup_b);
    $fclose(fd);
  end
endmodule
