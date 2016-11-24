---
layout: docs
title:  "Optional"
section: "optics"
scaladoc: "http://julien-truffaut.github.io/Monocle/api/#monocle.POptional"
pageSource: "https://raw.githubusercontent.com/julien-truffaut/Monocle/master/docs/src/main/tut/optional.md"
---
# Optional

An `Optional` is an Optic used to zoom inside a `Product`, e.g. `case class`, `Tuple`, `HList` or even `Map`.
Unlike the `Lens`, the element that the `Optional` focus on may not exist.

`Optionals` have two type parameters generally called `S` and `A`: `Optional[S, A]` where `S` represents the `Product` and `A` an optional element inside of `S`.

Let's take a simple list with integers.

We can create an `Optional[List[Int], Int]` which zoom from a `List[Int]` to its potential head by supplying a pair of functions:

*   `getOption: List[Int] => Option[Int]`
*   `set: Int => List[Int] => List[Int]`

```scala
import monocle.Optional
val head = Optional[List[Int], Int] {
  case Nil => None
  case x :: xs => Some(x)
}{ a => {
   case Nil => Nil
   case x :: xs => a :: xs
  }
}
```

Once we have an `Optional`, we can use the supplied `nonEmpty` function to know if it matches:

```scala
scala> val xs = List(1, 2, 3)
xs: List[Int] = List(1, 2, 3)

scala> val ys = List.empty[Int]
ys: List[Int] = Nil

scala> head.nonEmpty(xs)
res0: Boolean = true

scala> head.nonEmpty(ys)
res1: Boolean = false
```

We can use the supplied `getOrModify` function to retrieve the target if it matches, or the original value:

```scala
scala> val xs = List(1, 2, 3)
xs: List[Int] = List(1, 2, 3)

scala> val ys = List.empty[Int]
ys: List[Int] = Nil

scala> head.getOrModify(xs)
res0: scalaz.\/[List[Int],Int] = -\/(List())

scala> head.nonEmpty(ys)
res1: scalaz.\/[List[Int],Int] = \/-(1)
```

The function `getOrModify` is mostly use for polymorphic optics.
If you use monomorphic optics, use function `getOption`

We can use the supplied `getOption` and `set` functions:

```scala
scala> val xs = List(1, 2, 3)
xs: List[Int] = List(1, 2, 3)

scala> head.getOption(xs)
res0: Option[Int] = Some(1)

scala> head.set(5)(xs)
res1: List[Int] = List(5, 2, 3)
```

If we use the `Optional` on an empty list:

```scala
scala> val xs = List.empty[Int]
xs: List[Int] = Nil

scala> head.getOption(xs)
res0: Option[Int] = None

scala> head.set(5)(xs)
res1: List[Int] = Nil
```

We can also `modify` the target of `Optional` with a function:

```scala
scala> head.modify(_ + 1)(xs)
res2: List[Int] = List(2, 2, 3)
```

## Laws

```scala
class OptionalLaws[S, A](optional: Optional[S, A]) {

  def getOptionSet(s: S): IsEq[S] =
    optional.getOrModify(s).fold(identity, optional.set(_)(s)) <==> s

  def setGetOption(s: S, a: A): IsEq[Option[A]] =
    optional.getOption(optional.set(a)(s)) <==> optional.getOption(s).map(_ => a)

}
```

An `Optional` must satisfies all properties defined in `OptionalLaws` in `core` module.
You can check the validity of your own `Optional` using `OptionalTests` in `law` module.

`getOptionSet` states that if you `getOrModify` a value `A` from `S` and then `set` it back in, the result is an object identical to the original one.

`setGetOption` states that if you `set` a value, you always `getOption` the same value back.