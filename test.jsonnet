local a = function(x=2, y, z=0) x + z + y;
local b = a(y=0);
local c = a(1, 2, 3);
//local z = a(x=1, 2, z=3);
local a = {
  [std.trace(k, k)]:
  std.trace(k, k)
  for k in ["a", "b", "c"]
};
local d = {
  [std.trace("asdf", "asdf")]:
  std.trace("asdf", "asdf"),
  d: 0,
};
local dd = {
  [std.trace("asdf", "asdf")]:
  std.trace("asdf", "asdf"),
  c: 0,
  d: 0,
  //z: asdf,
  assert self.d != 0 : "DLKJFLKSJ",
};
[b, c, std.thisFile, a.a, d.d, dd.c]
