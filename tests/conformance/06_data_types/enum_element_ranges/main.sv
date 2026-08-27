// A name written with a range in an enumeration declaration stands for a
// sequence of names: name[N] declares name0 through nameN-1, and name[N:M]
// declares nameN through nameM, counting up or down as the two bounds require.
// Each name so declared takes the next consecutive value, so a range following
// an explicit value continues from it (LRM 6.19.2).
module Top;
  typedef enum {ca[5]} count_t;
  typedef enum {up[6:8]} up_t;
  typedef enum {dn[3:1]} down_t;
  typedef enum {add = 10, sb[5], jp[6:8]} mixed_t;

  count_t c;
  up_t u;
  down_t w;
  mixed_t m;

  int c0 = -1;
  int c1;
  int c2;
  int c3;
  int c4;
  int u6 = -1;
  int u7;
  int u8;
  int w3 = -1;
  int w2;
  int w1;
  int m_add;
  int m_sb0;
  int m_sb4;
  int m_jp6;
  int m_jp8;

  initial begin
    c = ca0;
    c0 = c;
    c = ca1;
    c1 = c;
    c = ca2;
    c2 = c;
    c = ca3;
    c3 = c;
    c = ca4;
    c4 = c;

    u = up6;
    u6 = u;
    u = up7;
    u7 = u;
    u = up8;
    u8 = u;

    w = dn3;
    w3 = w;
    w = dn2;
    w2 = w;
    w = dn1;
    w1 = w;

    m = add;
    m_add = m;
    m = sb0;
    m_sb0 = m;
    m = sb4;
    m_sb4 = m;
    m = jp6;
    m_jp6 = m;
    m = jp8;
    m_jp8 = m;
  end

  final begin
    if (c0 !== 0) $fatal(1, "c0 was %0d, expected 0", c0);
    if (c1 !== 1) $fatal(1, "c1 was %0d, expected 1", c1);
    if (c2 !== 2) $fatal(1, "c2 was %0d, expected 2", c2);
    if (c3 !== 3) $fatal(1, "c3 was %0d, expected 3", c3);
    if (c4 !== 4) $fatal(1, "c4 was %0d, expected 4", c4);
    if (u6 !== 0) $fatal(1, "u6 was %0d, expected 0", u6);
    if (u7 !== 1) $fatal(1, "u7 was %0d, expected 1", u7);
    if (u8 !== 2) $fatal(1, "u8 was %0d, expected 2", u8);
    if (w3 !== 0) $fatal(1, "w3 was %0d, expected 0", w3);
    if (w2 !== 1) $fatal(1, "w2 was %0d, expected 1", w2);
    if (w1 !== 2) $fatal(1, "w1 was %0d, expected 2", w1);
    if (m_add !== 10) $fatal(1, "m_add was %0d, expected 10", m_add);
    if (m_sb0 !== 11) $fatal(1, "m_sb0 was %0d, expected 11", m_sb0);
    if (m_sb4 !== 15) $fatal(1, "m_sb4 was %0d, expected 15", m_sb4);
    if (m_jp6 !== 16) $fatal(1, "m_jp6 was %0d, expected 16", m_jp6);
    if (m_jp8 !== 18) $fatal(1, "m_jp8 was %0d, expected 18", m_jp8);
    $display("All checks passed");
  end
endmodule
