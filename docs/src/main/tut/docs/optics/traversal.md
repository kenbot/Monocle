---
layout: docs
title:  "Traversal"
section: "optics"
source: "core/src/main/scala/monocle/PTraversal.scala"
scaladoc: "#monocle.Traversal"
---
# Traversal

A `Traversal` is the generalisation of an `Optional` to several targets. In other word, a `Traversal` allows 
to focus from a type `S` into 0 to n values of type `A`.

The most common example of a `Traversal` would be to focus into all elements inside of a container (e.g. `List`, `Vector`, `Option`). 
To do this we will use the relation between the typeclass `scalaz.Traverse` and `Traversal`:

```tut:silent
import monocle.Traversal
import scalaz.std.list._   // to get the Traverse instance for List

val xs = List(1,2,3,4,5)
```

```tut:book
val eachL = Traversal.fromTraverse[List, Int]
eachL.set(0)(xs)
eachL.modify(_ + 1)(xs)
```

A `Traversal` is also a `Fold`, so we have access to a few interesting methods to query our data:

```tut:book
eachL.getAll(xs)
eachL.headOption(xs)
eachL.find(_ > 3)(xs)
eachL.all(_ % 2 == 0)(xs)
```

`Traversal` also offers smart constructors to build a `Traversal` for a fix number of target (currently 2 to 6 targets):

```scala
case class Point(id: String, x: Int, y: Int)

val points = Traversal.apply2[Point, Int](_.x, _.y)((x, y, p) => p.copy(x = x, y = y))
```

```tut:invisible
import monocle.example.TraversalExample._ // because of SI-7139
```

```tut:book
points.set(5)(Point("bottom-left",0,0))
```

Finally, if you want to build something more custom you will have to implement a `Traversal` manually.
A `Traversal` is defined by a single method `modifyF` which corresponds to the Van Laarhoven representation.


For example, let's write a `Traversal` for `Map` that will focus into all values where the key satisfies a certain predicate:

```tut:silent
import monocle.Traversal
import scalaz.Applicative
import scalaz.std.map._
import scalaz.syntax.traverse._
import scalaz.syntax.applicative._

def filterKey[K, V](predicate: K => Boolean): Traversal[Map[K, V], V] =
    new Traversal[Map[K, V], V]{
      def modifyF[F[_]: Applicative](f: V => F[V])(s: Map[K, V]): F[Map[K, V]] =
        s.map{ case (k, v) =>
          k -> (if(predicate(k)) f(v) else v.pure[F])
        }.sequenceU
    }
    
val m = Map(1 -> "one", 2 -> "two", 3 -> "three", 4 -> "Four")
```

```tut:book
val filterEven = filterKey[Int, String](_ % 2 == 0)

filterEven.modify(_.toUpperCase)(m)
```

## Laws

A `Traversal` must satisfies all properties defined in `TraversalLaws` from the `core` module.
You can check the validity of your own `Traversal` using `TraversalTests` from the `law` module.


In particular, a `Traversal` must respect the `modifyGetAll` law which check that you can modify all elements targeted by a `Traversal`

```tut:silent
def modifyGetAll[S, A](t: Traversal[S, A], s: S, f: A => A): Boolean =
    t.getAll(t.modify(f)(s)) == t.getAll(s).map(f)
```

Another important law is `composeModify` also known as `fusion` law:
 
```tut:silent
def composeModify[S, A](t: Traversal[S, A], s: S, f: A => A, g: A => A): Boolean =
    t.modify(g)(t.modify(f)(s)) == t.modify(g compose f)(s)
```