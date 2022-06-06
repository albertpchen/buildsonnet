{
  'cats-effect': std.scala.Dep("org.typelevel", "cats-effect", "3.3.8"),
  'cats-parse': std.scala.Dep("org.typelevel", "cats-parse", "0.3.7"),

  log4cats(name): std.scala.Dep("org.typelevel", "log4cats-" + name, "2.3.1"),
  'log4cats-core': self.log4cats('core'),
  'log4cats-slf4j': self.log4cats('slf4j'),
  logback: std.java.Dep("ch.qos.logback", "logback-classic", "1.2.11"),

  fs2(name): std.scala.Dep("co.fs2", "fs2-" + name, "3.2.7"),
  'fs2-core': self.fs2('core'),
  'fs2-io': self.fs2('io'),

  weaver(name): std.scala.Dep("com.disneystreaming", "weaver-" + name, "0.7.11"),
  'weaver-cats': self.weaver('cats'),
  'weaver-scalacheck': self.weaver('scalacheck'),

  jsoniter(name): std.scala.Dep("com.github.plokhotnyuk.jsoniter-scala", "jsoniter-scala-" + name, "2.13.8"),
  'jsoniter-core': self.jsoniter('core'),
  'jsoniter-macros': self.jsoniter('macros'),

  'decline-effect': std.scala.Dep("com.monovore", "decline-effect", "2.2.0"),

  doobie(name): std.scala.Dep("org.tpolecat", "doobie-" + name, "1.0.0-RC2"),
  'doobie-core': self.doobie('core'),
  'doobie-hikari': self.doobie('hikari'),

  'sqlite-jdbc': std.java.Dep("org.xerial", "sqlite-jdbc", "3.36.0.3"),

  coursierDep(name): std.scala.Dep("io.get-coursier", "coursier" + name, "2.0.16", crossVersion="for3Use2_13"),
  'coursier': self.coursierDep(""),
  'coursier-cache': self.coursierDep("-cache"),
  'coursier-jvm': self.coursierDep("-jvm"),

  'ipcsocket': std.java.Dep("org.scala-sbt.ipcsocket", "ipcsocket", "1.4.0",),

  bloop(name): std.scala.Dep("ch.epfl.scala", "bloop" + name, "1.5.0", crossVersion='for3Use2_13'),
  'bloop-launcher': self.bloop('-launcher'),
  'bloop-config': self.bloop('-config'),
  'bloopgun': self.bloop('gun'),
}
