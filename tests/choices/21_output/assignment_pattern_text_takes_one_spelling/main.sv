// %p prints an aggregate as a legal assignment pattern, but the standard leaves
// its white space to the tool, lets it label an array's indices or gather
// elements under a default, lets it stop at 1024 characters, and leaves a
// non-null handle's text to it as well (LRM 21.2.1.6). Lyra writes one spelling
// throughout, and this case holds it to that: one space after each comma and
// none around a colon, every element written out with no default, an unpacked
// array's elements with no index labels, a struct's members by name in
// declaration order however many there are, a value with no padding, and a
// handle's text standing for the object it names, alike for two handles to one
// object and different for two objects.
module Top;
  typedef enum {ON, OFF} switch_e;
  typedef enum {IDLE, RUN, STOP} state_t;

  typedef struct {
    switch_e sw;
    int count;
  } setting_t;

  typedef struct packed {
    switch_e sw;
    bit [3:0] pad;
  } packed_setting_t;

  typedef struct {
    int count;
    state_t state;
  } entry_t;

  typedef struct packed {
    bit f0, f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, f14, f15, f16, f17, f18, f19, f20, f21, f22, f23, f24, f25, f26, f27, f28, f29, f30, f31, f32, f33, f34, f35, f36, f37, f38, f39, f40, f41, f42, f43, f44, f45, f46, f47, f48, f49, f50, f51, f52, f53, f54, f55, f56, f57, f58, f59, f60, f61, f62, f63, f64, f65, f66, f67, f68, f69, f70, f71, f72, f73, f74, f75, f76, f77, f78, f79, f80, f81, f82, f83, f84, f85, f86, f87, f88, f89, f90, f91, f92, f93, f94, f95, f96, f97, f98, f99, f100, f101, f102, f103, f104, f105, f106, f107, f108, f109, f110, f111, f112, f113, f114, f115, f116, f117, f118, f119, f120, f121, f122, f123, f124, f125, f126, f127, f128, f129, f130, f131, f132, f133, f134, f135, f136, f137, f138, f139, f140, f141, f142, f143, f144, f145, f146, f147, f148, f149, f150, f151, f152, f153, f154, f155, f156, f157, f158, f159, f160, f161, f162, f163, f164, f165, f166, f167, f168, f169, f170, f171, f172, f173, f174, f175, f176, f177, f178, f179, f180, f181, f182, f183, f184, f185, f186, f187, f188, f189, f190, f191, f192, f193, f194, f195, f196, f197, f198, f199, f200, f201, f202, f203, f204, f205, f206, f207, f208, f209, f210, f211, f212, f213, f214, f215, f216, f217, f218, f219, f220, f221, f222, f223, f224, f225, f226, f227, f228, f229, f230, f231, f232, f233, f234, f235, f236, f237, f238, f239, f240, f241, f242, f243, f244, f245, f246, f247, f248, f249, f250, f251, f252, f253, f254, f255, f256, f257, f258, f259, f260, f261, f262, f263, f264, f265, f266, f267, f268, f269, f270, f271, f272, f273, f274, f275, f276, f277, f278, f279, f280, f281, f282, f283, f284, f285, f286, f287, f288, f289, f290, f291, f292, f293, f294, f295, f296, f297, f298, f299;
  } wide_t;

  class Cell;
    int v;
  endclass

  switch_e settings [2];
  switch_e outside [1];
  setting_t nested;
  packed_setting_t packed_nested;
  entry_t entry;
  wide_t wide;
  switch_e undeclared;
  Cell made;
  Cell alias_of_made;
  Cell other;
  string pattern;

  string settings_text, outside_text, nested_text, packed_nested_text;
  string entry_text, wide_text, undeclared_text, value_text;
  string made_text, alias_text, other_text, computed_text;

  initial begin
    settings[0] = ON;
    settings[1] = OFF;
    outside[0] = switch_e'(7);
    nested = '{sw: OFF, count: 3};
    packed_nested = '{sw: ON, pad: 4'd5};
    entry = '{count: 4, state: IDLE};
    wide = '{default: 1'b1};
    undeclared = switch_e'(7);

    settings_text = $sformatf("%p", settings);
    outside_text = $sformatf("%p", outside);
    nested_text = $sformatf("%p", nested);
    packed_nested_text = $sformatf("%p", packed_nested);
    entry_text = $sformatf("%p", entry);
    wide_text = $sformatf("%p", wide);
    undeclared_text = $sformatf("%p", undeclared);
    value_text = $sformatf("%p", 8'hAA);

    made = new;
    other = new;
    alias_of_made = made;
    made_text = $sformatf("%p", made);
    alias_text = $sformatf("%p", alias_of_made);
    other_text = $sformatf("%p", other);
    pattern = "%p";
    computed_text = $sformatf(pattern, made);
  end

  final begin
    if (settings_text != "'{ON, OFF}")
      $fatal(1, "settings_text was '%s'", settings_text);
    if (outside_text != "'{7}")
      $fatal(1, "outside_text was '%s'", outside_text);
    if (nested_text != "'{sw:OFF, count:3}")
      $fatal(1, "nested_text was '%s'", nested_text);
    if (packed_nested_text != "'{sw:ON, pad:5}")
      $fatal(1, "packed_nested_text was '%s'", packed_nested_text);
    if (entry_text != "'{count:4, state:IDLE}")
      $fatal(1, "entry_text was '%s'", entry_text);
    if (wide_text.substr(0, 4) != "'{f0:")
      $fatal(1, "wide_text began with '%s'", wide_text.substr(0, 4));
    if (wide_text.substr(wide_text.len() - 9, wide_text.len() - 1) != ", f299:1}")
      $fatal(1, "wide_text ended with '%s'",
             wide_text.substr(wide_text.len() - 9, wide_text.len() - 1));
    if (undeclared_text != "7")
      $fatal(1, "undeclared_text was '%s'", undeclared_text);
    if (value_text != "170")
      $fatal(1, "value_text was '%s'", value_text);
    if (made_text == "null")
      $fatal(1, "a handle naming an object printed as null");
    if (made_text != alias_text)
      $fatal(1, "one object printed as %s and as %s", made_text, alias_text);
    if (made_text == other_text)
      $fatal(1, "two objects both printed as %s", made_text);
    if (computed_text != made_text)
      $fatal(1, "a computed format gave %s where a literal one gave %s",
             computed_text, made_text);
    $display("All checks passed");
  end
endmodule
