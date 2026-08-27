// When every case item comparison fails and no default item is given, none of
// the case item statements executes, so every variable an item would have
// written keeps the value it already held (LRM 12.5). The do-not-care forms,
// the set membership form, and a constant case expression are used in the same
// way as a plain case, so the rule reaches all of them (LRM 12.5.1, LRM 12.5.2,
// LRM 12.5.4).
module Top;
  typedef enum {ALPHA, BETA, GAMMA} Tag;

  int sel;
  Tag tag;
  string word;
  logic [3:0] code;

  int after_plain;
  int after_enum;
  int after_string;
  int after_casez;
  int after_inside;
  int after_constant;

  initial begin
    after_plain = 7;
    sel = 5;
    case (sel)
      0: after_plain = 1;
      1: after_plain = 2;
    endcase

    after_enum = 7;
    tag = GAMMA;
    case (tag)
      ALPHA: after_enum = 1;
      BETA:  after_enum = 2;
    endcase

    after_string = 7;
    word = "nope";
    case (word)
      "a": after_string = 1;
      "b": after_string = 2;
    endcase

    after_casez = 7;
    code = 4'b1111;
    casez (code)
      4'b000?: after_casez = 1;
      4'b001?: after_casez = 2;
    endcase

    after_inside = 7;
    sel = 100;
    case (sel) inside
      [1:3]: after_inside = 1;
      [4:6]: after_inside = 2;
    endcase

    after_constant = 7;
    sel = 99;
    case (1)
      sel == 1: after_constant = 1;
      sel == 2: after_constant = 2;
    endcase
  end

  final begin
    if (after_plain !== 7)
      $fatal(1, "after_plain was %0d, expected 7", after_plain);
    if (after_enum !== 7)
      $fatal(1, "after_enum was %0d, expected 7", after_enum);
    if (after_string !== 7)
      $fatal(1, "after_string was %0d, expected 7", after_string);
    if (after_casez !== 7)
      $fatal(1, "after_casez was %0d, expected 7", after_casez);
    if (after_inside !== 7)
      $fatal(1, "after_inside was %0d, expected 7", after_inside);
    if (after_constant !== 7)
      $fatal(1, "after_constant was %0d, expected 7", after_constant);
    $display("All checks passed");
  end
endmodule
