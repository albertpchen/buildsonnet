local a = function(x=2, y, z=0, x) x + z + y;
local zzz = 0; //c + 1;
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
  [0]: "DLFKJ",
  //z: asdf,
  // assert self.d != 0 : "DLKJFLKSJ",
};
// [b, c, std.thisFile, std.trace("LKJ" + b, b), d.d, dd.c, dd.asdf]
//[b, c]
local child = {
  c: 2,
  //[std.trace("inner ran", "asdf")]: 0
  z: super.z + {
    aa: 1234,
    c: $.c,
  }
};

{
  c: 1,
  d: 2,
  local z = a,
  zzzz: z,
  local a = self.c,
  a: a,
  local b = a + $.d,
  b: b,
  e: b + 9,
  s: "SDLKFJ",
  z: {
    e: "LKJKLJ",
    c: $.c,
    //[self.e]: 0,
    [$.s]: 0,
  },
  //[std.trace("outer ran", "asdf")]: 0
  zz: { z: {} } + child,
  zzz: zzz,
  arr: [0, 1, 2, 3, 4, 5, 6, 7,],
  slice: self.arr[0:4:2],
} + child
