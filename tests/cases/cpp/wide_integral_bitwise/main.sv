module Top;
  bit [127:0] and_low;
  bit [127:0] or_low;
  bit [127:0] xor_low;
  bit [127:0] not_zero;
  bit [127:0] and_cross_word;
  initial begin
    bit [127:0] a;
    bit [127:0] b;

    a = 128'd255;
    b = 128'd15;
    and_low = a & b;

    a = 128'd240;
    b = 128'd15;
    or_low = a | b;

    a = 128'd255;
    b = 128'd15;
    xor_low = a ^ b;

    a = 128'd0;
    not_zero = ~a;

    a = 128'hFFFFFFFF_00000000_00000000_00000000;
    b = 128'h0F0F0F0F_0F0F0F0F_0F0F0F0F_0F0F0F0F;
    and_cross_word = a & b;
  end
endmodule
