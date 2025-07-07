

[![](https://jitpack.io/v/g-pechorin/eJSON.svg)](https://jitpack.io/#g-pechorin/eJSON)

[public repo https://github.com/g-pechorin/eJSON](https://github.com/g-pechorin/eJSON)


extends JSON with a PEG-like system

```scala 3
import eJSON.*
case class FooBar(i: Int, s: String)

val foobar: E[FooBar] =
	for {
		i <- field[Int]
		s <- field[String]
	} yield {
		FooBar(i, s)
	}
```


