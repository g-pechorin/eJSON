package peterlavalle

import org.json.{JSONArray, JSONObject}
import peterlavalle.eJSON.*

class eJSONTest extends munit.FunSuite {

	test("do the thing - but - with the optionals") {
		import eJSON.*

		case class FooBar(i: Int, s: String)

		val foobar: oEntity[FooBar] =
			for {
				i <- field[Int]
				s <- field[String]
			} yield {
				FooBar(i, s)
			}

		assertEquals(
			foobar.decode(
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

		val goo: TUn[OneHo] =
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
	test("tes asOf on floats") {
		val src = "[1, '3.4', -5.0]"
		val actual: List[Float] = JSONArray(src).asOf[Float].get

		assertEquals(
			actual,
			List(1.0f, 3.4f, -5.0f)
		)
	}
}
