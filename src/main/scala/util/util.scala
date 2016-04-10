package util

object Functional {
  def nApply[A] (e: A, f:A=>A, n:Int) : A =
    if (n <= 0) e
    else nApply(f(e), f, (n-1))

  def nList[A] (g: ()=>A, n: Int) : List[A] =
    nApply(List[A](), (l:List[A]) => g() :: l, n)
}
