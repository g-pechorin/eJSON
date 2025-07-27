

[![](https://jitpack.io/v/g-pechorin/eJSON.svg)](https://jitpack.io/#g-pechorin/eJSON)

[public repo https://github.com/g-pechorin/eJSON](https://github.com/g-pechorin/eJSON)

extends [org.JSON](https://mvnrepository.com/artifact/org.json/json) with a PEG-like system (not shown) and a do notation (`for{}yield` - shown below) to parse values from JSON.

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


