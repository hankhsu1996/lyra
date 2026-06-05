module Top;
  // LRM 21.3.4.3(c): conversion spec is `%` `[*]` `[max field width]` code.
  // Max field width caps the input chars consumed by one conversion;
  // assignment suppression (`%*spec`) parses but discards.
  int w_d, w_h, w_b;
  string w_s;
  int count_w;
  int sup_a, sup_b;
  int count_sup;
  int mix_a, mix_b;
  int count_mix;
  initial begin
    count_w = $sscanf(
        "123 abc 1111 hello world", "%3d %3h %4b %5s", w_d, w_h, w_b, w_s);
    count_sup = $sscanf("10 20 30", "%d %*d %d", sup_a, sup_b);
    count_mix = $sscanf("999 88 7", "%3d %*d %d", mix_a, mix_b);
  end
endmodule
