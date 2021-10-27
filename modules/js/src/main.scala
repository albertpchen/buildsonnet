package tutorial.webapp

import org.scalajs.dom
import org.scalajs.dom.document

@main
def hello(): Unit =
  val parNode = document.createElement("p")
  parNode.textContent = "SDLFKJ"


  println("Hello world!")
