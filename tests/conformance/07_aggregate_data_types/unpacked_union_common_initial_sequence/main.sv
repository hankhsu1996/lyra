// When the members of an unpacked union are unpacked structures sharing a
// common initial sequence -- corresponding leading members of equivalent
// types -- and the union currently holds one of them, the common initial part
// may be read through any of the others, and reads what was written there
// (LRM 7.3).
module Top;
  typedef struct {
    int kind;
    logic [7:0] code;
    int count;
  } counted_t;

  typedef struct {
    int kind;
    logic [7:0] code;
    shortreal ratio;
  } measured_t;

  typedef union {
    counted_t counted;
    measured_t measured;
  } record_t;

  record_t record;

  int read_kind = -1;
  logic [7:0] read_code = 8'h00;
  int read_kind_back = -1;

  initial begin
    record.counted.kind = 3;
    record.counted.code = 8'hA5;
    record.counted.count = 9;
    read_kind = record.measured.kind;
    read_code = record.measured.code;

    record.measured = '{kind: 6, code: 8'h3C, ratio: 0.5};
    read_kind_back = record.counted.kind;
  end

  final begin
    if (read_kind !== 3)
      $fatal(1, "read_kind was %0d, expected 3", read_kind);
    if (read_code !== 8'hA5)
      $fatal(1, "read_code was %0h, expected a5", read_code);
    if (read_kind_back !== 6)
      $fatal(1, "read_kind_back was %0d, expected 6", read_kind_back);
    $display("All checks passed");
  end
endmodule
