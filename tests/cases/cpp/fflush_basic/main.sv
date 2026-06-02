module Top;
  int writer;
  int reader;
  int byte_after_flush;
  initial begin
    writer = $fopen("scratch.txt", "w");
    $fwrite(writer, "z");
    // LRM 21.3.6: $fflush forces buffered output to disk. After this call
    // a second descriptor opened for reading must see the byte without the
    // writer having closed.
    $fflush(writer);

    reader = $fopen("scratch.txt", "r");
    byte_after_flush = $fgetc(reader);
    $fclose(reader);
    $fclose(writer);
  end
endmodule
