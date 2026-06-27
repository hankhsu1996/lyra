module Top;
  // LRM 7.2.1 / 7.4: a packed array whose element is a packed aggregate (struct
  // or enum) is itself a flat vector. Element selects yield the element type, so
  // a member access through `pas[i].field` and an enum element read both work.
  typedef struct packed {
    logic [3:0] a;
    logic [3:0] b;
  } byte_t;

  typedef enum logic [1:0] {
    RED,
    GREEN,
    BLUE
  } color_t;

  typedef union packed {
    logic [7:0] full;
    logic [1:0][3:0] halves;
  } u_t;

  logic [15:0] pas_full;
  logic [3:0] field_a;
  logic [7:0] elem1;
  logic [7:0] pae_full;
  logic [1:0] pae0;
  logic [15:0] pau_full;
  logic [7:0] umem;
  logic [31:0] pn_full;
  logic [7:0] pn_elem;
  logic [3:0] pn_field;

  initial begin
    byte_t [1:0] pas;
    color_t [3:0] pae;
    u_t [1:0] pau;
    byte_t [1:0][1:0] pn;

    pas[0].a = 4'h1;
    pas[0].b = 4'h2;
    pas[1] = 8'hAB;
    pas_full = pas;
    field_a = pas[0].a;
    elem1 = pas[1];

    pae[0] = GREEN;
    pae[1] = BLUE;
    pae[2] = RED;
    pae[3] = GREEN;
    pae_full = pae;
    pae0 = pae[0];

    // Packed array whose element is a packed union.
    pau[0] = 8'hCD;
    pau[1] = 8'hAB;
    pau_full = pau;
    umem = pau[0].full;

    // Nested packed array of a packed struct: a chained element select
    // (`pn[1][0]`) yields the struct, and a field reaches through it.
    pn[0][0] = 8'h11;
    pn[0][1] = 8'h22;
    pn[1][0] = 8'h33;
    pn[1][1] = 8'h44;
    pn_full = pn;
    pn_elem = pn[1][0];
    pn_field = pn[1][1].a;
  end
endmodule
