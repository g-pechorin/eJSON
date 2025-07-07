package peterlavalle

import org.json.{JSONArray, JSONObject, JSONTokener}

import java.io.File
import scala.math.BigDecimal.javaBigDecimal2bigDecimal
import scala.util.{Failure, Try}

object eJSON {
	given F[String] =
		field {
			(o, k) =>
				o.get(k).toString
		}

	given F[Int] =
		field {
			(o: JSONObject, k: String) =>
				o.get(k) match
					case i: Int =>
						i
					case s: String =>
						s.toInt
		}

	given F[Float] =
		field {
			(o, k) =>
				o.get(k) match
					case i: Int =>
						i.toFloat
					case s: String =>
						s.toFloat
					case f: Float =>
						f
					case d: Double => d.toFloat
					case b: java.math.BigDecimal =>
						b.toFloat
		}

	extension [W](seq: Iterable[W])
		def toJSONArray(set: (JSONArray, W) => Unit): JSONArray =
			val json = JSONArray()
			seq.foreach((item: W) => set(json, item))
			json

	extension (a: JSONArray)
		def toListOf[E: F]: Try[List[E]] =
			(0 until a.length())
				.foldLeft(Try(List[E]())) {
					case (left, index) =>
						left.flatMap {
							left =>
								summon[F[E]].onArray(a, index).map(left :+ _)
						}
				}
		def asStrings: Seq[String] = a.toListOf[String].get

		def asObjects: Seq[JSONObject] =
			(0 until a.length())
				.to(LazyList)
				.map(a.getJSONObject)

	given F[File] = field((o: JSONObject, k: String) => File(o.getString(k)).getAbsoluteFile)

	def field[Q](get: (JSONObject, String) => Q): F[Q] =
		(json: JSONObject, key: String) =>
			try
				Try(get(json, key))
			catch
				case e: Throwable =>
					Failure(e)

	def field[I: F]: field0[I] = field0[I]()

	trait U[T]:
		def |[V >: T, E <: T](them: U[E]): U[V] =
			val base = this
			(json: JSONObject) =>
				base.unapply(json).orElse(them.unapply(json))

		def ![O](f: T => O): U[O] =
			val b = this
			(json: JSONObject) =>
				b.unapply(json).map(f)

		def ?[O](f: T => Option[O]): U[O] =
			val b = this
			(json: JSONObject) =>
				b.unapply(json).flatMap(f)

		def unapply(json: JSONObject): Option[T]

		def unapply(src: String): Option[T] =
			unapply(JSONObject(JSONTokener(src)))

	trait E[Q] extends F[Q] with U[Q] {
		override def unapply(o: JSONObject): Option[Q] = decode(o).toOption

		def decode(o: JSONObject): Try[Q]

		override def onObject(json: JSONObject, key: String): Try[Q] =
			decode(json.getJSONObject(key))
	}

	extension (s: String)
		def /[Q](f: E[Q]): U[Q] =
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

	trait F[Q] {
		def onArray(json: JSONArray, i: Int): Try[Q] =

			if (i < 0 || json.length() <= i)
				Failure(
					IndexOutOfBoundsException(s"index $i is OOB in array $json")
				)
			else
				val n = getClass.getSimpleName
				onObject(
					new JSONObject().put(n, json.get(i)),
					n
				)

		def onObject(json: JSONObject, key: String): Try[Q]
	}

	final class field0[I: F]():
		inline def flatMap[O](inline func: I => E[O]): E[O] =
			${ field1.code('{ bind(func) }, '{ func }) }

		private def bind[O](get: I => E[O]): String => E[O] =
			(k: String) =>
				(o: JSONObject) =>
					if (!o.has(k))
						Failure(KeyMissing(s"key $k is not in $o"))
					else
						summon[F[I]]
							.onObject(o, k)
							.map(get)
							.flatMap(_.decode(o))

		inline def map[O](inline func: I => O): E[O] =
			${ field1.code('{ pure(func) }, '{ func }) }

		private def pure[O](get: I => O): String => E[O] =
			(k: String) =>
				(o: JSONObject) =>
					if (!o.has(k))
						Failure(
							IndexOutOfBoundsException(s"key $k is not in $o")
						)
					else
						summon[F[I]]
							.onObject(o, k)
							.map(get)

	private case class KeyMissing(message: String) extends Exception(message)

	private object field1:

		import scala.quoted.*

		def code[I: Type, O: Type]
		(
			q: Expr[String => E[O]],
			f: Expr[I => Any]
		)(using quotes: Quotes): Expr[E[O]] =
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

	given [T: F]: F[List[T]] with {
		override def onObject(json: JSONObject, key: String): Try[List[T]] =
			json.getJSONArray(key).toListOf[T]
	}

}
