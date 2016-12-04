package monocle.law

object PIsoLaws {


  import scalaz._, Scalaz._

  trait VanLarIso[S,T,A,B] {
    self =>

    def apply[P[_,_]: Profunctor](p: P[A,B]): P[S,T]

    final def compose[X,Y](other: VanLarIso[A,B,X,Y]) = new VanLarIso[S,T,X,Y] {
      def apply[P[_,_]: Profunctor](p: P[X,Y]): P[S,T] = self(other(p))
    }
  }

  implicit def vanLarIsoProfunctor[A,B] = new Profunctor[({type f[s,t] = VanLarIso[s,t,A,B]})#f] {
    override def mapfst[S, T, C](vli: VanLarIso[S,T,A,B])(f: C => S) = new VanLarIso[C,T,A,B] {
      def apply[P[_,_]: Profunctor](pab: P[A,B]): P[C,T] = vli(pab).mapfst(f)
    }

    override def mapsnd[S, T, C](vli: VanLarIso[S,T,A,B])(f: T => C) = new VanLarIso[S,C,A,B] {
      def apply[P[_,_]: Profunctor](pab: P[A,B]): P[S,C] = vli(pab).mapsnd(f)
    }
  }

  implicit def pIsoProfunctor[A,B] = new Profunctor[({type f[s,t] = PIso[s,t,A,B]})#f] {
    override def mapfst[S, T, C](pi: PIso[S,T,A,B])(f: C => S) =
      PIso[C,T,A,B](c => pi.get(f(c)))(pi.reverseGet)

    override def mapsnd[S, T, C](pi: PIso[S,T,A,B])(f: T => C): PIso[S,C,A,B] =
      PIso[S,C,A,B](pi.get)(b => f(pi.reverseGet(b)))
  }

  def vanLarify[S,T,A,B](pIso: PIso[S,T,A,B]): VanLarIso[S,T,A,B] =
    new VanLarIso[S,T,A,B] {
      def apply[P[_,_]: Profunctor](pab: P[A,B]): P[S,T] =
        pab.dimap(pIso.get, pIso.reverseGet)
    }

  def isoLaw[S: Arbitrary,T,A,B](p: PIso[S,T,A,B]) = prop { (s: S) =>
    // f . from f ≡ id
    // from f . f ≡ id
    val f: VanLarIso[S,T,A,B] = vanLarify(p)
    val fromF: VanLarIso[B,A,T,S] = vanLarify(p.reverse)
    
    ???
  }




}

