package monocle

sealed trait OpticCompose[Opt1[_,_,_,_], Opt2[_,_,_,_]] {
  type Result[_,_,_,_]

  def compose[S,T,A,B,C,D](o1: Opt1[S,T,A,B], o2: Opt2[A,B,C,D]): Result[S,T,C,D]
}


/*  Composition matrix of Folds, Getters, Setters, Traversals, Optionals, Prisms, Lenses, Isos
 *  Cell = row.compose(column)
 *
 *    FGSTOPLI
 *   +--------
 *  F|ffffffff
 *  G|fg-fffgg
 *  S|--ssssss
 *  T|ffsttttt
 *  O|ffstoooo
 *  P|ffstopop
 *  L|fgstooll
 *  I|fgstopli
 *
 */
trait OpticComposeInstances {

  type PFold[S,T,A,B] = Fold[S,A]
  type PGetter[S,T,A,B] = Getter[S,A]

  implicit val tf = new OpticCompose[PTraversal, PFold] {
    override type Result[S,T,A,B] = PFold[S,T,A,B] 
    override def compose[S,T,A,B,C,D](o1: PTraversal[S,T,A,B], o2: PFold[A,B,C,D]): Result[S, T, C, D] = o1 composeFold o2
  }

  implicit val tg = new OpticCompose[PTraversal, PGetter] {
    override type Result[S,T,A,B] = PFold[S,T,A,B] 
    override def compose[S,T,A,B,C,D](o1: PTraversal[S,T,A,B], o2: PGetter[A,B,C,D]): Result[S, T, C, D] = o1 composeGetter o2
  }

  implicit val ts = new OpticCompose[PTraversal, PSetter] {
    override type Result[S,T,A,B] = PSetter[S,T,A,B] 
    override def compose[S,T,A,B,C,D](o1: PTraversal[S,T,A,B], o2: PSetter[A,B,C,D]): Result[S, T, C, D] = o1 composeSetter o2
  }

  implicit val tt = new OpticCompose[PTraversal, PTraversal] {
    override type Result[S,T,A,B] = PTraversal[S,T,A,B] 
    override def compose[S,T,A,B,C,D](o1: PTraversal[S,T,A,B], o2: PTraversal[A,B,C,D]): Result[S, T, C, D] = o1 composeTraversal o2
  }

  implicit val to = new OpticCompose[PTraversal, POptional] {
    override type Result[S,T,A,B] = PTraversal[S,T,A,B] 
    override def compose[S,T,A,B,C,D](o1: PTraversal[S,T,A,B], o2: POptional[A,B,C,D]): Result[S, T, C, D] = o1 composeOptional o2
  }

  implicit val tp = new OpticCompose[PTraversal, PPrism] {
    override type Result[S,T,A,B] = PTraversal[S,T,A,B] 
    override def compose[S,T,A,B,C,D](o1: PTraversal[S,T,A,B], o2: PPrism[A,B,C,D]): Result[S, T, C, D] = o1 composePrism o2
  }

  implicit val tl = new OpticCompose[PTraversal, PLens] {
    override type Result[S,T,A,B] = PTraversal[S,T,A,B] 
    override def compose[S,T,A,B,C,D](o1: PTraversal[S,T,A,B], o2: PLens[A,B,C,D]): Result[S, T, C, D] = o1 composeLens o2
  }

  implicit val ti = new OpticCompose[PTraversal, PIso] {
    override type Result[S,T,A,B] = PTraversal[S,T,A,B] 
    override def compose[S,T,A,B,C,D](o1: PTraversal[S,T,A,B], o2: PIso[A,B,C,D]): Result[S, T, C, D] = o1 composeIso o2
  }

  implicit val lf = new OpticCompose[PLens, PFold] {
    override type Result[S,T,A,B] = PFold[S,T,A,B] 
    override def compose[S,T,A,B,C,D](o1: PLens[S,T,A,B], o2: PFold[A,B,C,D]): Result[S, T, C, D] = o1 composeFold o2
  }

  implicit val lg = new OpticCompose[PLens, PGetter] {
    override type Result[S,T,A,B] = PGetter[S,T,A,B] 
    override def compose[S,T,A,B,C,D](o1: PLens[S,T,A,B], o2: PGetter[A,B,C,D]): Result[S, T, C, D] = o1 composeGetter o2
  }

  implicit val ls = new OpticCompose[PLens, PSetter] {
    override type Result[S,T,A,B] = PSetter[S,T,A,B] 
    override def compose[S,T,A,B,C,D](o1: PLens[S,T,A,B], o2: PSetter[A,B,C,D]): Result[S, T, C, D] = o1 composeSetter o2
  }

  implicit val lt = new OpticCompose[PLens, PTraversal] {
    override type Result[S,T,A,B] = PTraversal[S,T,A,B]
    override def compose[S,T,A,B,C,D](o1: PLens[S,T,A,B], o2: PTraversal[A, B, C, D]): Result[S, T, C, D] = o1 composeTraversal o2
  }

  implicit val lo = new OpticCompose[PLens, POptional] {
    override type Result[S,T,A,B] = POptional[S,T,A,B] 
    override def compose[S,T,A,B,C,D](o1: PLens[S,T,A,B], o2: POptional[A,B,C,D]): Result[S, T, C, D] = o1 composeOptional o2
  }

  implicit val lp = new OpticCompose[PLens, PPrism] {
    override type Result[S,T,A,B] = POptional[S,T,A,B] 
    override def compose[S,T,A,B,C,D](o1: PLens[S,T,A,B], o2: PPrism[A,B,C,D]): Result[S, T, C, D] = o1 composePrism o2
  }

  implicit val ll = new OpticCompose[PLens, PLens] {
    override type Result[S,T,A,B] = PLens[S,T,A,B] 
    override def compose[S,T,A,B,C,D](o1: PLens[S,T,A,B], o2: PLens[A,B,C,D]): Result[S, T, C, D] = o1 composeLens o2
  }

  implicit val li = new OpticCompose[PLens, PIso] {
    override type Result[S,T,A,B] = PLens[S,T,A,B]
    override def compose[S,T,A,B,C,D](o1: PLens[S,T,A,B], o2: PIso[A, B, C, D]): Result[S, T, C, D] = o1 composeIso o2
  }
}

object OpticCompose extends OpticComposeInstances 
