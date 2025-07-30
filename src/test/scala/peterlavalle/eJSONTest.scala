package peterlavalle

import org.json.{JSONArray, JSONObject}
import peterlavalle.eJSON.*

import scala.util.{Failure, Success, Try}

class eJSONTest extends munit.FunSuite {

  test("do the thing - but - with the optionals") {
    import eJSON.*

    case class FooBar(i: Int, s: String)

    val foobar: E[FooBar] =
      for {
        i <- field[Int]
        s <- field[String]
      } yield {
        FooBar(i, s)
      }

    assertEquals(
      foobar(
        new JSONObject()
          .put("i", "7")
          .put("s", -9)
      ).get,
      FooBar(7, "-9")
    )
  }

  test("test one hot") {
    case class OneHo(i: Int)
    import eJSON.*

    val goo: E[OneHo] =
      "foo" / {
        for {
          g <- field[Int]
        } yield {
          OneHo(g)
        }
      }

    "{foo:{g:9}}" match
      case goo(OneHo(q)) =>
        assertEquals(q, 9)

    assertEquals(
      goo.unapply("{bar:{g:19}}"),
      None
    )

    assertEquals(
      goo.unapply("{foo:{G:9}}"),
      None
    )
  }

  test("test two one hots") {
    import eJSON.*

    case class Box(i: Int, s: Int)

    val foo = "foo" / {
      for {
        g <- field[Int]
      } yield {
        Box(g, 1)
      }
    }

    val bar = "bar" / {
      for {
        g <- field[Int]
      } yield {
        Box(g, 2)
      }
    }

    def go(src: String) =
      src match
        case foo(v) =>
          v
        case bar(v) =>
          v
        case _ =>
          null

    assertEquals(
      go("{foo:{G:9}}"),
      null
    )
    assertEquals(
      go("{bar:{g:19}}"),
      Box(19, 2)
    )
    assertEquals(
      go("{foo:{g:'-179'}}"),
      Box(-179, 1)
    )
  }

  test("test one-hot alt-merge") {
    import eJSON.*

    case class Box(i: Int, s: Int)

    val foo = "foo" / {
      for {
        g <- field[Int]
      } yield {
        Box(g, 1)
      }
    }

    val bar = "bar" / {
      for {
        g <- field[Int]
      } yield {
        Box(g, 2)
      }
    }

    val goo = foo | bar

    def go(src: String) =
      src match
        case goo(v) =>
          v
        case _ =>
          null

    assertEquals(
      go("{foo:{G:9}}"),
      null
    )
    assertEquals(
      go("{bar:{g:19}}"),
      Box(19, 2)
    )
    assertEquals(
      go("{foo:{g:'-179'}}"),
      Box(-179, 1)
    )
  }

  test("test toListOf on floats") {
    val src = "[1, '3.4', -5.0]"
    val actual: List[Float] = JSONArray(src).toListOf[Float].get

    assertEquals(
      actual,
      List(1.0f, 3.4f, -5.0f)
    )
  }

  test("test simple") {

    class Foo(val a: Int)

    val f: E[Foo] =
      for {
        a <- field[Int]
      } yield {
        Foo(a)
      }

    val r: Try[Foo] =
      f(JSONObject().put("a", 12))

    assertEquals(r.get.a, 12)
  }

  test("test foo|bar") {

    class Foo(val a: Int)
    class Bar(val c: Float)

    val p: E[Foo | Bar] =
      val f: E[Foo] =
        for {
          a <- field[Int]
        } yield {
          Foo(a)
        }
      val b: E[Bar] =
        for {
          boo <- field[Float]
        } yield {
          Bar(boo * -1)
        }

      f | b

    p(JSONObject().put("a", 13)) match
      case Failure(exception)  => throw exception
      case Success(value: Foo) =>
        assert(value.a == 13)
      case what =>
        fail(what.toString)

    p(JSONObject().put("boo", 1.2f)) match
      case Failure(exception)  => throw exception
      case Success(value: Bar) =>
        assert(value.c == -1.2f)
      case what =>
        fail(what.toString)
  }

  test("test map") {
    case class Foo(i: Int)
    case class Bar(s: String)

    val f =
      for {
        i <- field[Int]
      } yield {
        Foo(i)
      }
    val b =
      f ! { (f) =>
        Bar(f.toString)
      }

    assertEquals(
      b(JSONObject().put("i", 27)),
      Success(
        Bar("Foo(27)")
      )
    )

  }
}
