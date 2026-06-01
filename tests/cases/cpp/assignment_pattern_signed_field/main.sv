module Top;
  typedef struct packed {
    logic signed [7:0] s_field;
  } wrapper_t;
  wrapper_t w;
  int raw_us;
  int via_signed;
  initial begin
    // Pattern element is bound under the field's signed context; the
    // emitted concat must preserve signedness on the field read so the
    // sign-extending int conversion sees -1, not 0xFF.
    w = '{s_field: 8'hFF};
    raw_us = w;
    via_signed = w.s_field;
  end
endmodule
