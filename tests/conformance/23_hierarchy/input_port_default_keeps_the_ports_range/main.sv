// A port's default is evaluated in the scope of the module that declares it and
// takes the port's type as any value assigned to the port does (LRM 23.2.2.4),
// so a port declared with a range that does not start at zero holds the default
// under that range: a select into the port reads the bit its own index names,
// whatever range the default's expression was written with.
module Child #(parameter logic [7:0] P = 8'hA5) (
    input logic [7:4] whole = 4'b1010,
    input logic [7:4] part = P[7:4],
    input logic [11:8] sum = P[7:4] + P[3:0],
    output logic [3:0] got_whole,
    output logic [3:0] got_part,
    output logic [3:0] got_sum
);
  assign got_whole = {whole[7], whole[6], whole[5], whole[4]};
  assign got_part = {part[7], part[6:5], part[4]};
  assign got_sum = {sum[11:10], sum[9], sum[8]};
endmodule

module Top;
  logic [3:0] whole[1:2];
  logic [3:0] part[1:2];
  logic [3:0] sum[1:2];

  Child first (.got_whole(whole[1]), .got_part(part[1]), .got_sum(sum[1]));
  Child #(.P(8'h3C)) second (
      .got_whole(whole[2]), .got_part(part[2]), .got_sum(sum[2]));

  final begin
    if (whole[1] !== 4'b1010) $fatal(1, "first.whole read %b", whole[1]);
    if (whole[2] !== 4'b1010) $fatal(1, "second.whole read %b", whole[2]);
    if (part[1] !== 4'hA) $fatal(1, "first.part read %h", part[1]);
    if (part[2] !== 4'h3) $fatal(1, "second.part read %h", part[2]);
    // 4'hA + 4'h5 and 4'h3 + 4'hC, each kept to four bits.
    if (sum[1] !== 4'hF) $fatal(1, "first.sum read %h", sum[1]);
    if (sum[2] !== 4'hF) $fatal(1, "second.sum read %h", sum[2]);
    $display("All checks passed");
  end
endmodule
