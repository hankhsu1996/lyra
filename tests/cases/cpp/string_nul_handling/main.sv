module Top;
  string embedded;
  string leading;
  string empty;
  int len_embedded;
  int len_leading;
  int len_empty;

  initial begin
    // LRM 6.16: building a string value strips every NUL. The embedded NUL is
    // removed, not a truncation point: "hello\0world" -> "helloworld" (len 10).
    embedded = "hello\0world";
    len_embedded = embedded.len();
    // LRM 6.16 integral cast: 16'h0041 is bytes 0x00, 0x41; the leading NUL
    // strips, leaving "A" (len 1).
    leading = string'(16'h0041);
    len_leading = leading.len();
    // LRM 11.10.3: "" is the packed 8'h00, whose one NUL strips to the empty
    // string (len 0).
    empty = "";
    len_empty = empty.len();
  end
endmodule
