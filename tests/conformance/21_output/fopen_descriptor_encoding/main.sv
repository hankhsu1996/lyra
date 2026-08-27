// $fopen called without a type returns a multichannel descriptor: a 32-bit
// value with a single bit set, whose bit 31 is reserved and always cleared and
// whose bit 0 stands for the standard output rather than any opened file.
// Called with a type it returns a file descriptor instead, whose bit 31 is
// reserved and always set (LRM 21.3.1).
module Top;
  int channel;
  int write_descriptor;
  int read_descriptor;
  int channel_bits_set;

  initial begin
    channel = $fopen("channel.txt");
    channel_bits_set = $countones(channel);
    $fclose(channel);

    write_descriptor = $fopen("descriptor.txt", "w");
    $fdisplay(write_descriptor, "data");
    $fclose(write_descriptor);

    read_descriptor = $fopen("descriptor.txt", "r");
    $fclose(read_descriptor);
  end

  final begin
    if (channel_bits_set !== 1)
      $fatal(1, "the multichannel descriptor %h had %0d bits set, expected 1",
             channel, channel_bits_set);
    if (channel[31] !== 1'b0)
      $fatal(1, "bit 31 of the multichannel descriptor %h was set", channel);
    if (channel[0] !== 1'b0)
      $fatal(1, "the multichannel descriptor %h took the standard output bit",
             channel);
    if (write_descriptor[31] !== 1'b1)
      $fatal(1, "bit 31 of the write file descriptor %h was clear",
             write_descriptor);
    if (read_descriptor[31] !== 1'b1)
      $fatal(1, "bit 31 of the read file descriptor %h was clear",
             read_descriptor);
    $display("All checks passed");
  end
endmodule
