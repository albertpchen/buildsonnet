package a

import weaver.SimpleIOSuite

object SimpleTest extends SimpleIOSuite:
  test("hello world") {
    HelloWorld.run.as(expect(true))
  }
