// A packed structure with a 4-state member is a 4-state vector as a whole,
// and a 2-state member of it converts only where that member is read or
// written: read, its x and z bits are 0; written, the bits written become
// known. A select of the member stores the bits it names and no others, so
// the member's remaining bits keep what the vector holds, x included. That
// holds as well of a structure reached as a member of an unpacked union (LRM
// 7.2.1, 7.3, 11.5.1).
module Top;
  typedef struct packed {
    logic [7:0] hi;
    bit [3:0] two;
    logic [3:0] lo;
  } record_t;

  typedef union {
    record_t r;
    logic [15:0] flat;
  } either_t;

  record_t held;
  either_t viewed;
  logic [15:0] viewed_member_bit_written;
  logic [15:0] viewed_member_part_written;
  logic [15:0] member_written;
  logic [15:0] member_bit_written;
  logic [15:0] member_part_written;
  logic [15:0] member_bit_written_by_variable;
  bit [3:0] member_read;
  logic [15:0] local_member_written;
  logic [15:0] local_member_bit_written;
  logic [15:0] local_member_part_written;
  logic [15:0] local_member_bit_written_by_variable;
  bit [3:0] local_member_read;

  function automatic logic [15:0] write_member();
    record_t r;
    r = 'x;
    r.two = 4'b1010;
    return r;
  endfunction

  function automatic logic [15:0] write_member_bit();
    record_t r;
    r = 'x;
    r.two[1] = 1'b1;
    return r;
  endfunction

  function automatic logic [15:0] write_member_part();
    record_t r;
    r = 'x;
    r.two[2:1] = 2'b01;
    return r;
  endfunction

  function automatic logic [15:0] write_member_bit_by_variable(int i);
    record_t r;
    r = 'x;
    r.two[i] = 1'b1;
    return r;
  endfunction

  function automatic bit [3:0] read_member_after_bit_write();
    record_t r;
    r = 'x;
    r.two[1] = 1'b1;
    return r.two;
  endfunction

  initial begin
    int at;

    member_read = 4'b1111;
    local_member_read = 4'b1111;

    held = 'x;
    held.two = 4'b1010;
    member_written = held;

    held = 'x;
    held.two[1] = 1'b1;
    member_bit_written = held;
    member_read = held.two;

    held = 'x;
    held.two[2:1] = 2'b01;
    member_part_written = held;

    at = 3;
    held = 'x;
    held.two[at] = 1'b1;
    member_bit_written_by_variable = held;

    viewed.r = 'x;
    viewed.r.two[1] = 1'b1;
    viewed_member_bit_written = viewed.r;

    viewed.r = 'x;
    viewed.r.two[2:1] = 2'b01;
    viewed_member_part_written = viewed.r;

    local_member_written = write_member();
    local_member_bit_written = write_member_bit();
    local_member_part_written = write_member_part();
    local_member_bit_written_by_variable = write_member_bit_by_variable(3);
    local_member_read = read_member_after_bit_write();
  end

  final begin
    if (member_written !== 16'bxxxxxxxx_1010_xxxx)
      $fatal(1, "writing the member gave %b, expected xxxxxxxx1010xxxx",
             member_written);
    if (member_bit_written !== 16'bxxxxxxxx_xx1x_xxxx)
      $fatal(1, "one bit of the member written: %b, expected xxxxxxxxxx1xxxxx",
             member_bit_written);
    if (member_read !== 4'b0010)
      $fatal(1, "the member read back %b, expected 0010", member_read);
    if (member_part_written !== 16'bxxxxxxxx_x01x_xxxx)
      $fatal(1, "two bits of the member written: %b, expected xxxxxxxxx01xxxxx",
             member_part_written);
    if (member_bit_written_by_variable !== 16'bxxxxxxxx_1xxx_xxxx)
      $fatal(1, "the member's bit 3 written: %b, expected xxxxxxxx1xxxxxxx",
             member_bit_written_by_variable);

    if (viewed_member_bit_written !== 16'bxxxxxxxx_xx1x_xxxx)
      $fatal(1, "one bit of a union member's member: %b, expected %s",
             viewed_member_bit_written, "xxxxxxxxxx1xxxxx");
    if (viewed_member_part_written !== 16'bxxxxxxxx_x01x_xxxx)
      $fatal(1, "two bits of a union member's member: %b, expected %s",
             viewed_member_part_written, "xxxxxxxxx01xxxxx");

    if (local_member_written !== 16'bxxxxxxxx_1010_xxxx)
      $fatal(1, "writing a local's member gave %b, expected xxxxxxxx1010xxxx",
             local_member_written);
    if (local_member_bit_written !== 16'bxxxxxxxx_xx1x_xxxx)
      $fatal(1, "one bit of a local's member: %b, expected xxxxxxxxxx1xxxxx",
             local_member_bit_written);
    if (local_member_read !== 4'b0010)
      $fatal(1, "a local's member read back %b, expected 0010",
             local_member_read);
    if (local_member_part_written !== 16'bxxxxxxxx_x01x_xxxx)
      $fatal(1, "two bits of a local's member: %b, expected xxxxxxxxx01xxxxx",
             local_member_part_written);
    if (local_member_bit_written_by_variable !== 16'bxxxxxxxx_1xxx_xxxx)
      $fatal(1, "bit 3 of a local's member: %b, expected xxxxxxxx1xxxxxxx",
             local_member_bit_written_by_variable);
    $display("All checks passed");
  end
endmodule
