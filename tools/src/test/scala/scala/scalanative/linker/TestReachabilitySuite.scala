package scala.scalanative.linker

import org.scalatest._
import scalanative.nir.{Sig, Type, Global}

class TestReachabilitySuite extends ReachabilitySuite {
  val TestMain     = g("Test$", Sig.Method("main", Seq(Type.Unit)))
  test("playing around") {
    val entry = TestMain
    val sources = List(
      """
        class Parent
        class Child extends Parent

        object Test {
          def main: Unit = {
            new Parent
          }
        }
      """
    )

    link(Seq(entry), sources) { res =>
      println(res)
    }
  }
}
