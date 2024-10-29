package peterlavalle

import org.json.{JSONObject, JSONTokener}


trait TUn[T]:
	def |[V >: T, E <: T](them: TUn[E]): TUn[V] =
		val base = this
		(json: JSONObject) =>
			base.unapply(json).orElse(them.unapply(json))

	def ![O](f: T => O): TUn[O] =
		val b = this
		(json: JSONObject) =>
			b.unapply(json).map(f)

	def ?[O](f: T => Option[O]): TUn[O] =
		val b = this
		(json: JSONObject) =>
			b.unapply(json).flatMap(f)

	def unapply(json: JSONObject): Option[T]

	def unapply(src: String): Option[T] =
		unapply(JSONObject(JSONTokener(src)))
