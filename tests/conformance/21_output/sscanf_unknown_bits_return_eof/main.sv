// When the control string or the source given to $sscanf holds unknown bits,
// the function returns EOF and assigns nothing, whichever of the two carries
// them. A source or control string that is four-state but free of them parses
// as any other does (LRM 21.3.4.3).
module Top;
  logic [15:0] source_with_x;
  logic [15:0] source_with_z;
  logic [39:0] format_with_x;
  logic [15:0] clean_source;

  int from_x_source;
  int from_z_source;
  int from_x_format;
  int from_clean_source;

  int after_x_source;
  int after_z_source;
  int after_x_format;
  int trailing_after_x_format;
  int from_clean;

  initial begin
    source_with_x = 16'b00110001_xxxx0010;
    source_with_z = 16'b00110010_zzzz0011;
    format_with_x = 40'h2564_20xx_64;
    clean_source = 16'h3530;

    after_x_source = 999;
    after_z_source = 999;
    after_x_format = 999;
    trailing_after_x_format = 999;

    from_x_source = $sscanf(source_with_x, "%d", after_x_source);
    from_z_source = $sscanf(source_with_z, "%d", after_z_source);
    from_x_format = $sscanf("42 99", format_with_x, after_x_format,
                            trailing_after_x_format);
    from_clean_source = $sscanf(clean_source, "%d", from_clean);
  end

  final begin
    if (from_x_source !== -1)
      $fatal(1, "a source holding x bits returned %0d, expected -1",
             from_x_source);
    if (after_x_source !== 999)
      $fatal(1, "a source holding x bits assigned %0d, expected nothing",
             after_x_source);

    if (from_z_source !== -1)
      $fatal(1, "a source holding z bits returned %0d, expected -1",
             from_z_source);
    if (after_z_source !== 999)
      $fatal(1, "a source holding z bits assigned %0d, expected nothing",
             after_z_source);

    if (from_x_format !== -1)
      $fatal(1, "a control string holding x bits returned %0d, expected -1",
             from_x_format);
    if (after_x_format !== 999 || trailing_after_x_format !== 999)
      $fatal(1, "a control string holding x bits assigned %0d and %0d",
             after_x_format, trailing_after_x_format);

    if (from_clean_source !== 1)
      $fatal(1, "a clean four-state source returned %0d, expected 1",
             from_clean_source);
    if (from_clean !== 50)
      $fatal(1, "the value from a clean four-state source was %0d",
             from_clean);
    $display("All checks passed");
  end
endmodule
