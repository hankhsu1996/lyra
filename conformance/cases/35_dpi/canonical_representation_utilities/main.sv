// The C layer gives foreign code utilities for the canonical representation
// of a packed value, numbered from 0 at its least significant bit (LRM Annex
// H.11.5): a bit-select is read and written as an svBit or svLogic scalar, and
// a part-select of up to 32 bits is read into the low bits of a destination
// group, leaving the group's bits above the selection as they were, and
// written from the low bits of a source group, leaving the bits around the
// selection as they were. A selection may sit inside one group or cross into
// the next. A 4-state selection carries both planes, so 0, 1, z and x each
// come back as they went (Annex H.10.1.1, H.10.1.2). svDpiVersion names the
// canonical representation the tool passes packed values in, which for the
// "DPI-C" argument passing of IEEE Std 1800 is "1800-2005" (Annex H.10.1.3,
// H.14.1).
module Top;
  import "DPI-C" function bit get_bit(input bit [95:0] v, input int i);
  import "DPI-C" function logic get_logic(input logic [95:0] v, input int i);
  import "DPI-C" function void put_bit(
      inout bit [95:0] v, input int i, input bit s);
  import "DPI-C" function void put_logic(
      inout logic [95:0] v, input int i, input logic s);
  import "DPI-C" function void get_part_bit(
      inout bit [31:0] d, input bit [95:0] v, input int i, input int w);
  import "DPI-C" function void get_part_logic(
      inout logic [31:0] d, input logic [95:0] v, input int i, input int w);
  import "DPI-C" function void put_part_bit(
      inout bit [95:0] d, input bit [31:0] s, input int i, input int w);
  import "DPI-C" function void put_part_logic(
      inout logic [95:0] d, input logic [31:0] s, input int i, input int w);
  import "DPI-C" function string dpi_version();

  // A bit of every kind at every place a select can start: the first and last
  // bit of each group, and a bit inside one.
  localparam int kBits[8] = '{0, 5, 31, 32, 47, 63, 64, 95};
  // Where a part-select starts and how wide it is: one bit; inside one group;
  // a whole group starting on its boundary; across into the next group; a
  // whole group's width starting inside one; the last bits of the vector.
  localparam int kStarts[7] = '{0, 4, 32, 28, 16, 62, 88};
  localparam int kWidths[7] = '{1, 8, 32, 8, 32, 4, 8};

  // The four values a 4-state bit takes, cycled through by a position.
  function automatic logic four_state(int n);
    case (n % 4)
      0: return 1'b0;
      1: return 1'b1;
      2: return 1'bz;
      default: return 1'bx;
    endcase
  endfunction

  // Two vectors whose bits differ at every place a select reads, so a select
  // reading the wrong place reads a different bit.
  function automatic bit [95:0] bit_pattern();
    bit [95:0] v;
    for (int b = 0; b < 96; b++) v[b] = ((b * 5 + b / 7) % 3) == 0;
    return v;
  endfunction
  function automatic logic [95:0] logic_pattern();
    logic [95:0] v;
    for (int b = 0; b < 96; b++) v[b] = four_state(b * 3 + b / 5);
    return v;
  endfunction

  initial begin
    bit [95:0] bv;
    logic [95:0] lv;
    string version;

    bv = bit_pattern();
    lv = logic_pattern();

    foreach (kBits[k]) begin
      int i;
      i = kBits[k];
      if (get_bit(bv, i) !== bv[i])
        $fatal(1, "svGetBitselBit at %0d read %b, expected %b", i,
               get_bit(bv, i), bv[i]);
      if (get_logic(lv, i) !== lv[i])
        $fatal(1, "svGetBitselLogic at %0d read %b, expected %b", i,
               get_logic(lv, i), lv[i]);
    end

    foreach (kBits[k]) begin
      int i;
      bit [95:0] bd;
      bit [95:0] bexpected;
      i = kBits[k];
      bd = bv;
      bexpected = bv;
      bexpected[i] = !bv[i];
      put_bit(bd, i, !bv[i]);
      if (bd !== bexpected)
        $fatal(1, "svPutBitselBit at %0d left %h, expected %h", i, bd,
               bexpected);
      for (int n = 0; n < 4; n++) begin
        logic [95:0] ld;
        logic [95:0] lexpected;
        ld = lv;
        lexpected = lv;
        lexpected[i] = four_state(n);
        put_logic(ld, i, four_state(n));
        if (ld !== lexpected)
          $fatal(1, "svPutBitselLogic of %b at %0d left %h, expected %h",
                 four_state(n), i, ld, lexpected);
      end
    end

    foreach (kStarts[k]) begin
      int i;
      int w;
      bit [31:0] bd;
      bit [31:0] bexpected;
      logic [31:0] ld;
      logic [31:0] lexpected;
      i = kStarts[k];
      w = kWidths[k];
      // The destination holds bits the selection could not give it above the
      // width, so a get that writes them shows.
      bd = 32'hA5C3_96E1;
      ld = 32'hzx10_xz01;
      bexpected = bd;
      lexpected = ld;
      for (int b = 0; b < w; b++) begin
        bexpected[b] = bv[i+b];
        lexpected[b] = lv[i+b];
      end
      get_part_bit(bd, bv, i, w);
      if (bd !== bexpected)
        $fatal(1, "svGetPartselBit of %0d bits at %0d gave %h, expected %h",
               w, i, bd, bexpected);
      get_part_logic(ld, lv, i, w);
      if (ld !== lexpected)
        $fatal(1, "svGetPartselLogic of %0d bits at %0d gave %h, expected %h",
               w, i, ld, lexpected);
    end

    foreach (kStarts[k]) begin
      int i;
      int w;
      bit [31:0] bs;
      logic [31:0] ls;
      bit [95:0] bd;
      bit [95:0] bexpected;
      logic [95:0] ld;
      logic [95:0] lexpected;
      i = kStarts[k];
      w = kWidths[k];
      // The source differs from the destination at every bit it writes, and
      // its bits above the width differ from the destination's above the
      // selection, so a put that writes too many shows.
      for (int b = 0; b < 32; b++) bs[b] = !bv[(i+b)%96];
      for (int b = 0; b < 32; b++) ls[b] = four_state(b + 1);
      for (int b = 0; b < 32; b++)
        if (ls[b] === lv[(i+b)%96]) ls[b] = four_state(b + 2);
      bd = bv;
      ld = lv;
      bexpected = bv;
      lexpected = lv;
      for (int b = 0; b < w; b++) begin
        bexpected[i+b] = bs[b];
        lexpected[i+b] = ls[b];
      end
      put_part_bit(bd, bs, i, w);
      if (bd !== bexpected)
        $fatal(1, "svPutPartselBit of %0d bits at %0d left %h, expected %h",
               w, i, bd, bexpected);
      put_part_logic(ld, ls, i, w);
      if (ld !== lexpected)
        $fatal(1, "svPutPartselLogic of %0d bits at %0d left %h, expected %h",
               w, i, ld, lexpected);
    end

    version = "unset";
    version = dpi_version();
    if (version != "1800-2005")
      $fatal(1, "svDpiVersion answered '%s', expected '1800-2005'", version);

    $display("All checks passed");
  end
endmodule
