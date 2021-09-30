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
  z +: {
    aa: 1234,
    c: $.c,
  }
};

local aaaaaa(arr) = arr[0:4:2];
{
  c: 1,
  d: 2,
  local z = a,
  zzzz: z,
  local a = self.c,
  local c = b,
  a: a,
  local b = a + $.d,
  b: c,
  e: b + 9,
  s: "SDLKFJ",
  z: {
    e: "LKJKLJ",
    c: $.c,
    //[self.e]: 0,
    [$.s]: 0,
  },
  //[std.trace("outer ran", "asdf")]: 0
  zz: {} + { z: {} } + child,
  zzchild: (self.zz + child) {
    c: "zzchild",
  },
  zzz: zzz,
  // fff: if 0 then 0 else 1,
  arr: [i for i in [0, 1, 2, 3, 4, 5, 6, 7,] if true],
  slice: aaaaaa(self.arr),
  objComp: {
    local a = 0,
    local b = c,
    local c = a + 3,
    local d = "s",
    [std.toString(i)]: b for i in ["a", "b", "c"]
  },
  objObj: std.toString(self.objComp) + 1234,
  objTpe: std.type(self.objComp),
  bad: std.length(aaaaaa),
  // asdf: std.get({ a:: 0}, "a", inc_hidden=false),
  // cs: ["asdfasdf"] + std.scala.cs("org.tpolecat", "doobie-core_2.12", "0.6.0"),
  // css: std.scala.cs("org.typelevel", "cats-core_2.11", "0.6.0"),
  cs: std.scala.cs([
    { org: "org.typelevel", name:"cats-core_2.11", version:"0.6.0"},
  ]),
  p1: [
    std.trace("P1: 1", 0),
    std.trace("P1: 2", 0),
    std.trace("P1: 3", 0),
    std.trace("P1: 4", 0),
    std.trace("P1: 5", 0),
    std.trace("P1: 6", 0),
    std.trace("P1: 7", 0),
  ],
  p2: [
    std.trace("P2: 1", 0),
    std.trace("P2: 2", 0),
    std.trace("P2: 3", 0),
    std.trace("P2: 4", 0),
    std.trace("P2: 5", 0),
    std.trace("P2: 6", 0),
    std.trace("P2: 7", 0),
  ],
  //file: std.thisFile
  jjjobb: std.runJob({
    cmdline: ["env"],
    inputFiles: [],
    envVars: { "asdf": "DLKFJLKJ" },
  }).stdout
} + child
