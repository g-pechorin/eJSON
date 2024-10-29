package peterlavalle

import org.json.JSONObject

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
}
