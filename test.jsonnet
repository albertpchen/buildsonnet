local a = function(x=2, y, z=0, x) x + z + y;
local mut = 0;
local b = a(x=0, y=mut);
local mut = 1234;
local c = a(1, 2, 3, 0);
//local z = a(x=1, 2, z=3);
/*
local a = {
  [std.trace(k, k)]:
  std.trace(k, k)
  for k in ["a", "b", "c"]
};
*/
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
//[b, c, std.thisFile, std.trace("LKJ" + b, b), d.d, dd.c]
[b, c]
