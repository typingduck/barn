package barn

object Utils {

  def tap[A](a: A)(f: A => Unit) : A = {f(a); a}

  object -> {
    def unapply[A, B](pair: (A, B)): Option[(A, B)] =
      Some(pair)
  }
}
