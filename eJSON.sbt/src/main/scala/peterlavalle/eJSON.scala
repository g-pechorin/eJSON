package peterlavalle

import org.json.{JSONArray, JSONObject}

import java.io.File

object eJSON {
	given Field[String] =
		field {
			(o, k) =>
				o.get(k).toString
		}

	extension [W](seq: Iterable[W])
		def toJSONArray(set: (JSONArray, W) => Unit): JSONArray =
			val json = JSONArray()
			seq.foreach((item: W) => set(json, item))
			json

	extension (a: JSONArray)

		def asStrings: Seq[String] =
			(0 until a.length())
				.to(LazyList)
				.map(a.getString)

		def asObjects: Seq[JSONObject] =
			(0 until a.length())
				.to(LazyList)
				.map(a.getJSONObject)

	given Field[Int] =
		field {
			(o, k) =>
				o.get(k) match
					case i: Int =>
						i
					case s: String =>
						s.toInt
		}

	def field[Q](get: (JSONObject, String) => Q): Field[Q] =
		(json: JSONObject, key: String) =>
			try
				Re(get(json, key))
			catch
				case e: Throwable =>
					Re ! (e)

	given Field[File] = field((o: JSONObject, k: String) => File(o.getString(k)).getAbsoluteFile)

	def field[I: Field]: field0[I] = field0[I]()

	extension (s: String)
		def /[Q](f: oEntity[Q]): TUn[Q] =
			(json: JSONObject) =>
				val keys = json.keySet()
				if (1 != keys.size())
					None
				else {
					val key = keys.iterator().next()
					if (key != s)
						None
					else
						json.optJSONObject(key) match
							case null =>
								None
							case json =>
								f.unapply(json)
				}

	trait oEntity[Q] extends Field[Q] with TUn[Q] {
		override def unapply(o: JSONObject): Option[Q] = Re.unapply(decode(o))

		def decode(o: JSONObject): Re[Q]

		override def onObject(json: JSONObject, key: String): Re[Q] =
			decode(json.getJSONObject(key))
	}

	trait Field[Q] {
		def onArray(json: JSONArray, i: Int): Re[Q] =

			if (i < 0 || json.length() <= i)
				Re ! IndexOutOfBoundsException(s"index $i is OOB in array $json")
			else
				val n = getClass.getSimpleName
				onObject(
					new JSONObject().put(n, json.get(i)),
					n
				)

		def onObject(json: JSONObject, key: String): Re[Q]
	}

	final class field0[I: Field]():
		inline def flatMap[O](inline func: I => oEntity[O]): oEntity[O] =
			${ field1.code('{ bind(func) }, '{ func }) }

		private def bind[O](get: I => oEntity[O]): String => oEntity[O] =
			(k: String) =>
				(o: JSONObject) =>
					if (!o.has(k))
						Re ! KeyMissing(s"key $k is not in $o")
					else
						summon[Field[I]]
							.onObject(o, k)
							.map(get)
							.flatMap(_.decode(o))

		inline def map[O](inline func: I => O): oEntity[O] =
			${ field1.code('{ pure(func) }, '{ func }) }

		private def pure[O](get: I => O): String => oEntity[O] =
			(k: String) =>
				(o: JSONObject) =>
					if (!o.has(k))
						Re ! IndexOutOfBoundsException(s"key $k is not in $o")
					else
						summon[Field[I]]
							.onObject(o, k)
							.map(get)

	private case class KeyMissing(message: String) extends Exception(message)

	private object field1:

		import scala.quoted.*

		def code[I: Type, O: Type]
		(
			q: Expr[String => oEntity[O]],
			f: Expr[I => Any]
		)(using quotes: Quotes): Expr[oEntity[O]] =
			import quotes.reflect.*

			// Inspect the lambda to extract parameter name
			f.asTerm match {

				case Inlined(_, List(), Block(List(DefDef(_, List(List(ValDef(name: String, _, _))), _, _)), _)) =>
					// pass the name into the whatnot
					val key: Expr[String] = Expr(name)
					'{ $q($key) }

				case _ =>
					quotes.reflect.report.error(s"pure expected a for{}yield comprehension")
					'{ ??? }
			}

}
