package peterlavalle

import org.json.{JSONArray, JSONObject, JSONTokener}

import java.io.File
import scala.math.BigDecimal.javaBigDecimal2bigDecimal
import scala.util.{Failure, Try}

object eJSON {
  given F[String] =
    field.pure { (o, k) =>
      o.get(k).toString
    }

  given F[Int] =
    field.pure { (o: JSONObject, k: String) =>
      o.get(k) match
        case i: Int =>
          i
        case s: String =>
          s.toInt
    }

  given F[Float] =
    field.pure { (o, k) =>
      o.get(k) match
        case i: Int =>
          i.toFloat
        case s: String =>
          s.toFloat
        case f: Float =>
          f
        case d: Double               => d.toFloat
        case b: java.math.BigDecimal =>
          b.toFloat
    }

  given F[File] = field.pure((o: JSONObject, k: String) =>
    File(o.getString(k)).getAbsoluteFile
  )

  given F[JSONObject] = field.pure(_ getJSONObject _)

  extension [W](seq: Iterable[W])
    def toJSONArray(set: (JSONArray, W) => Unit): JSONArray =
      val json = JSONArray()
      seq.foreach((item: W) => set(json, item))
      json

  extension (a: JSONArray)
    def toListOf[E: F]: Try[List[E]] =
      (0 until a.length())
        .foldLeft(Try(List[E]())) { case (left, index) =>
          left.flatMap { left =>
            summon[F[E]].onArray(a, index).map(left :+ _)
          }
        }
    def asStrings: Seq[String] = a.toListOf[String].get
    def asObjects: Seq[JSONObject] =
      (0 until a.length())
        .to(LazyList)
        .map(a.getJSONObject)

  def field[I: F]: field0[I] = field0[I]()

  def flag(s: String): field0[Unit] =
    given F[Unit] =
      field.bind { (j: JSONObject, k: String) =>
        val v = j.getString(k)
        if (v != s)
          Failure(
            Exception(
              s"its not a match - `$s` != `$v`"
            )
          )
        else
          Try(())
      }

    field0[Unit]()

  def array[I: F]: field0[List[I]] =
    type Q = List[I]

    field[Q]

  def setOf[I: F]: field0[Set[I]] =
    sys.error("wrap the array one")

  trait E[Q] extends F[Q] {

    def ![V](f: Q => V): E[V] =
      apply(_: JSONObject)
        .map(f)

    def |[V >: Q, Z <: V](them: E[Z]): E[V] =
      (json: JSONObject) => apply(json).orElse(them(json))

    def unapply(src: String): Option[Q] = unapply(JSONObject(JSONTokener(src)))

    def unapply(o: JSONObject): Option[Q] = apply(o).toOption

    def apply(o: JSONObject): Try[Q]

    override def onObject(json: JSONObject, key: String): Try[Q] =
      apply(json.getJSONObject(key))
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

  extension (s: String)
    def /[Q](f: E[Q]): E[Q] =

      (json: JSONObject) =>
        val keys = json.keySet()
        val value =
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

        Try {
          value.get
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
              .flatMap(_.apply(o))

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

  object field {
    def read[Q](key: String => Boolean)(value: String => Q): F[Q] =
      pure { (json: JSONObject, name: String) =>
        require(
          json.has(name)
        )
        val text = json.getString(name)
        require(
          key(text)
        )
        value(text)
      }

    def pure[Q](get: (JSONObject, String) => Q): F[Q] =
      (json: JSONObject, key: String) =>
        try Try(get(json, key))
        catch
          case e: Throwable =>
            Failure(e)

    def bind[Q](get: (JSONObject, String) => Try[Q]): F[Q] =
      (json: JSONObject, key: String) => get(json, key)
  }

  private object field1:

    import scala.quoted.*

    def code[I: Type, O: Type](
        q: Expr[String => E[O]],
        f: Expr[I => Any]
    )(using quotes: Quotes): Expr[E[O]] =
      import quotes.reflect.*

      // Inspect the lambda to extract parameter name
      f.asTerm match {

        case Inlined(
              _,
              List(),
              Block(
                List(DefDef(_, List(List(ValDef(name: String, _, _))), _, _)),
                _
              )
            ) =>
          // pass the name into the whatnot
          val key: Expr[String] = Expr(name)
          '{ $q($key) }

        case _ =>
          quotes.reflect.report.error(
            s"pure expected a for{}yield comprehension"
          )
          '{ ??? }
      }

  given [T: F]: F[List[T]] with {
    override def onObject(json: JSONObject, key: String): Try[List[T]] =
      json.getJSONArray(key).toListOf[T]
  }

}
