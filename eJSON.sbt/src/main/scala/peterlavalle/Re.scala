package peterlavalle

sealed trait Re[T]:
	def map[O](f: T => O): Re[O]

	def flatMap[O](f: T => Re[O]): Re[O]

	def get: T

object Re {
	def unapply[Q](i: Re[Q]): Option[Q] =
		???

	def ![Q](t: Throwable): Re[Q] = L(t)

	def apply[E](v: E): Re[E] = R(v)

	private class R[Q](val get: Q) extends Re[Q]:
		override def map[O](f: Q => O): Re[O] = R(f(get))

		override def flatMap[O](f: Q => Re[O]): Re[O] = f(get)


	private class L[Q](e: Throwable) extends Re[Q]:
		override def map[O](f: Q => O): Re[O] = L(e)

		override def flatMap[O](f: Q => Re[O]): Re[O] = L(e)

		override def get: Q = throw e
}