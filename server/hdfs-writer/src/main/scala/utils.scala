package barn


object Utils {

  def tap[A](a: A)(f: A => Unit) : A = {f(a); a}

}
